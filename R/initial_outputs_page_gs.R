initial_outputs_gs_ui <- function(id, map_outputs, label = "IO") {
	ns <- NS(id)
	tabItem(tabName = "initial_outputs",
					fluidRow(
					  column(6,
					         box(title = p("Growth Stage"), solidHeader = TRUE, status = "primary", width = 12,
					             fluidRow(column(12, htmlOutput(ns("growth_stage_name")))),
					             fluidRow(column(12, img(src="www/growth_stages_linear.png", class="img-responsive"))),
					             fluidRow(column(12, class = 'slider-container', shinyWidgets::noUiSliderInput(inputId = ns("growth_stage_user_input"), label = NULL, color = "#005fae", min = 0, max = 14, value = 0, step = 0.1))),
					         )
					  ),
						column(6,
									 box(title = p("Seasonal Growth and Precipitation", actionLink(ns("gdd_info"), label = "", icon = icon("info-circle"), class = "btn-info")),
									 		solidHeader = TRUE,
									 		status = "primary",
									 		width = 12,
									 		fluidRow(
									 		  column(12, htmlOutput(ns("reactive_growth_stage"))
									 		         )
									 		),
									 		fluidRow(
									 			column(12,
									 						 valueBoxOutput(ns("rainfall"), width = 6),
									 						 valueBoxOutput(ns("nuptake"), width = 6)
									 			)
									 		),
									 		fluidRow(
									 			column(12,
									 			       conditionalPanel(
									 			         condition = "input.which_plot == 'Growth Stage Estimate'", ns = ns,
									 			         plotly::plotlyOutput(ns("gdd_plotly")) %>% shinycssloaders::withSpinner(type = 6, color="#005fae")
									 			       ),
									 						 conditionalPanel(
									 						 	condition = "input.which_plot == 'N uptake/Precip. (%)'", ns = ns,
									 						 	plotly::plotlyOutput(ns("nuptake_plotly")) %>% shinycssloaders::withSpinner(type = 6, color="#005fae")
									 						 ),
									 						 conditionalPanel(
									 						 	condition = "input.which_plot == 'Seasonal Water (in.)'", ns = ns,
									 						 	plotly::plotlyOutput(ns("total_water_plotly")) %>% shinycssloaders::withSpinner(type = 6, color="#005fae")
									 						 )
									 			)
									 		),
									 		fluidRow(
									 			column(12,
									 						   uiOutput(outputId = ns("which_plot"))
									 			)
									 		),
									 		fluidRow(
									 			column(12,
									 						 downloadButton(ns("downloadCSV"), "Download CSV")
									 			)
									 		)
									 ),
									 shinyBS::bsButton(ns("back_to_location"), label = "Back", block = TRUE, style="default", size = "lg")
									 ))
	)
}


initial_outputs_gs_server <- function(id,
																	 parent,
																	 map_outputs,
																	 growth_stage_option,
																	 prelim_weather_data,
																	 irrigation_data,
																	 con){
	moduleServer(
		id,
		function(input, output, session){

			ns <- session$ns
			
			max_prism_date <- DBI::dbGetQuery(con, "SELECT DISTINCT(date) FROM grain.prism WHERE quality != 'forecast' ORDER BY date DESC LIMIT 1;")

			observe({
			 
				if(nrow(req(prelim_weather_data())) > 0){

				if(req(map_outputs()$nuptakemod) == FALSE){
				  shinyWidgets::updateNoUiSliderInput(session = parent, inputId = ns("growth_stage_user_input"), value = reverseValue(growth_stage_option()))
				} else {
				  
				  present_prism_gdd <- weather_data()[weather_data()$time == "present" & weather_data()$quality == "prism" & weather_data()$measurement == "gdd_cumsum", ]
				  
				  val <- gdd_to_feekes((present_prism_gdd[present_prism_gdd$amount == max(present_prism_gdd$amount), ]$amount)*(present_prism_gdd[present_prism_gdd$amount == max(present_prism_gdd$amount), ]$correction))
				  
#					val <- round(gdd_to_feekes(max(isolate(prelim_weather_data())[isolate(prelim_weather_data())$quality != #"forecast" & isolate(prelim_weather_data())$time == "present" & isolate(prelim_weather_data())$measurement == #"gdd_cumsum", "amount"], na.rm = TRUE)), 1)
					
					shinyWidgets::updateNoUiSliderInput(parent, inputId = ns("growth_stage_user_input"), value = reverseValue(val))
				}

				}
			})
			
			observeEvent(input$gdd_info, {
			  showModal(
			    modalDialog(title = "Climate measurements",
			                p("Climate data for each location was obtained from the PRISM Climate Group (PRISM Climate Group, 2021). Cumulative precipitation and growing degree-days from sowing are estimated for each location and compared to 10-year means. Degree-days were estimated using the corrected single triangle method. Temperature thresholds of 87°F (30°C) and 44°F (7°C) were used.")
			    )
			  )
			})
			
			output$which_plot <- renderUI({
			  if(map_outputs()$nuptakemod == TRUE){
			  radioButtons(session$ns("which_plot"),label = "",
			               choices = c("Growth Stage Estimate", "N uptake/Precip. (%)", "Seasonal Water (in.)"),
			               selected = "Growth Stage Estimate", inline = TRUE)
			  } else {
			    radioButtons(session$ns("which_plot"),label = "",
			                 choices = c("Seasonal Water (in.)"),
			                 selected = "Seasonal Water (in.)", inline = TRUE)
			  }
			})

			daterange_full <- reactive({

				dates <- prelim_weather_data() %>%
					filter(time == "present") %>%
					ungroup() %>%
					select(date)

				range(dates$date)
			})

			min_date <- reactive({
				min(prelim_weather_data()[prelim_weather_data()$time == "present" & prelim_weather_data()$quality == "prism" & prelim_weather_data()$measurement == "gdd_cumsum", ]$date, na.rm = TRUE)
			})

			max_date <- reactive({
				max(prelim_weather_data()[prelim_weather_data()$time == "present" & prelim_weather_data()$quality == "prism" & prelim_weather_data()$measurement == "gdd_cumsum", ]$date, na.rm = TRUE)
			})

			weather_data <- reactive({

				if (req(input$growth_stage_user_input) > 0){

					if(map_outputs()$nuptakemod == TRUE){

						# reactive to changes in user growth stage input
						mx_date <- max(prelim_weather_data()[prelim_weather_data()$time == "present" & prelim_weather_data()$quality == "prism" & prelim_weather_data()$measurement == "gdd_cumsum", ]$date, na.rm = TRUE)
						
						mx <- max(prelim_weather_data()[prelim_weather_data()$time == "present" & prelim_weather_data()$quality == "prism" & prelim_weather_data()$measurement == "gdd_cumsum", ]$amount, na.rm = TRUE)
						
						mx_historical <- prelim_weather_data()[prelim_weather_data()$time == "historical" & prelim_weather_data()$quality == "historical" & prelim_weather_data()$measurement == "gdd_cumsum" & prelim_weather_data()$date == mx_date,]$amount


						if(round(gdd_to_feekes(max(isolate(prelim_weather_data())[isolate(prelim_weather_data())$quality != "forecast" & isolate(prelim_weather_data())$time == "present" & isolate(prelim_weather_data())$measurement == "gdd_cumsum", "amount"], na.rm = TRUE)), 1) == input$growth_stage_user_input){
							
							adjustment_factor <- 1
							
						feekes <- prelim_weather_data() %>% 
								filter(measurement == "gdd_cumsum") %>% 
								mutate(measurement = "feekes")
							
							feekes$amount <- apply(as.data.frame(feekes$amount)*adjustment_factor, 1, gdd_to_feekes)
							
							wd <- prelim_weather_data() %>% 
								bind_rows(feekes)
						
							
						} else {

							adjustment_factor <- feekes_to_gdd(input$growth_stage_user_input)/mx
							adjustment_factor_historical <- feekes_to_gdd(input$growth_stage_user_input)/mx_historical
							
							
							wd <- prelim_weather_data() %>% 
								filter(!(quality == "forecast"))
							
#							if(gdd_to_nuptake(mx) < 100){
								
								wd_temp <- wd %>%
									tidyr::spread(key = measurement, value = amount) %>%
									mutate(nuptake_perc = if_else(time == "present", gdd_to_nuptake(gdd_cumsum*adjustment_factor),
										gdd_to_nuptake(gdd_cumsum*adjustment_factor_historical)),
										nuptake_perc = if_else(nuptake_perc > 100, 100, nuptake_perc))
								
								wd_temp_present <- wd_temp %>% 
									filter(time == "present")
								
								wd_temp_present$feekes <- apply(as.data.frame(wd_temp_present$gdd_cumsum)*adjustment_factor, 1, gdd_to_feekes)
								
								wd_temp_historical <- wd_temp %>% 
									filter(time == "historical")
								
								wd_temp_historical$feekes <- apply(as.data.frame(wd_temp_historical$gdd_cumsum)*adjustment_factor_historical, 1, gdd_to_feekes)
								
								wd <- wd_temp_present %>% 
									bind_rows(wd_temp_historical) %>% 
									tidyr::gather(key = "measurement", value = "amount", -date, -month, -day, -time, -quality) %>%
									tidyr::drop_na(amount)
								
#							} else {
#								
#								wd <- wd %>%
#									filter(measurement == "nuptake_perc",
#												 time == "present",
#												 amount <= gdd_to_nuptake(feekes_to_gdd(input$growth_stage_user_input))) %>%
#									tidyr::spread(key = measurement, value = amount) 
#								
#								#mx_date_nup <- max(wd$date, na.rm = TRUE)
#								#days_adjust <- as.numeric(mx_date - min_date())/as.numeric(mx_date_nup - min_date())
#								
#								wd_temp <- wd %>%
#									mutate(num_days = date - min_date(),
#												 new_days = num_days*days_adjust,
#												 date = min_date() + new_days)
#								
#								
#								wd_temp$feekes <- apply(as.data.frame(wd_temp$gdd_cumsum*adjustment_factor), 1, gdd_to_feekes)
#								
#								print("isolate bug")
#								
#								wd <- wd_temp_present %>%
#									bind_rows(wd_temp_historical) %>% 
#									gather(key = "measurement", value = "amount", -date, -month, -day, -time, -quality) %>%
#									bind_rows(prelim_weather_data() %>%
#															filter(!(measurement == "nuptake_perc" &
#																			 	time == "present"))) %>%
#									drop_na(amount)
#								
#							}
						}

					} else {

												# reactive to changes in user growth stage input
						mx <- prelim_weather_data()[prelim_weather_data()$time == "present" & prelim_weather_data()$measurement == "gdd_cumsum" & prelim_weather_data()$date == daterange_full()[2], ]$amount

						adjustment_factor <- if_else(is.null(input$growth_stage_user_input), 1, feekes_to_gdd(input$growth_stage_user_input)/mx) 

						bd <- prelim_weather_data()[!(prelim_weather_data()$time == "present" & prelim_weather_data()$measurement == "gdd_cumsum" & prelim_weather_data()$date > daterange_full()[2]), ]
						
						wd_temp <- bd %>%
							tidyr::spread(key = measurement, value = amount) %>%
							mutate(
								nuptake_perc = if_else(time == "present", gdd_to_nuptake(gdd_cumsum*adjustment_factor), nuptake_perc),
								nuptake_perc = if_else(nuptake_perc > 100, 100, nuptake_perc))
						
						wd_temp$feekes <- apply(as.data.frame(wd_temp$gdd_cumsum)*adjustment_factor, 1, gdd_to_feekes)
						
						wd <- wd_temp %>% 
							tidyr::gather(key = "measurement", value = "amount", -date, -month, -day, -time, -quality) %>%
							tidyr::drop_na(amount)

					}


				} else {
					
					
					feekes <- prelim_weather_data() %>% 
						filter(measurement == "gdd_cumsum") %>% 
						mutate(measurement = "feekes")
					
					feekes$amount <- apply(as.data.frame(feekes$amount), 1, gdd_to_feekes)
					
					wd <- prelim_weather_data() %>% 
						bind_rows(feekes) %>% 
						tidyr::drop_na(amount)
				}

				return(wd)
			}) # end weather_data reactive block
			

				irrigation <- reactive({
					irrigation_data() %>%
					mutate(date = as.Date(date),
								 month = lubridate::month(date),
								 day = lubridate::day(date),
								 time = "present",
								 measurement = "irr",
								 amount = amount,
								 quality = "irrigation")
							 })

				total_water <- reactive({
					if (nrow(irrigation_data()) > 0){
						weather_data() %>%
						filter(measurement == "ppt",
									 date <= max_prism_date$date) %>%
						mutate(amount = amount/25.4) %>%
						bind_rows(irrigation()) %>%
						group_by(time) %>%
						arrange(date) %>%
						mutate(water_cumsum = cumsum(amount))
					} else {
						weather_data() %>%
							filter(measurement == "ppt",
										 date <= max_prism_date$date) %>%
							mutate(amount = amount/25.4) %>%
							group_by(time) %>%
							arrange(date) %>%
							mutate(water_cumsum = cumsum(amount))
					}
				})

				present_nup <- reactive({
					weather_data() %>%
					filter(measurement == "nuptake_perc",
								 time == "present",
								 quality != "forecast")
							 })

				historical_nup <- reactive({
					weather_data() %>%
					filter(measurement == "nuptake_perc",
								 time == "historical")
							 })

				present_sum <- reactive({
					round(gdd_to_nuptake(feekes_to_gdd(input$growth_stage_user_input)), 0)
				})

				historical_sum <- reactive({
					month <- as.numeric(substring(present_nup()[present_nup()$date == max(present_nup()$date),]$date, 6, 7))
					day <- as.numeric(substring(present_nup()[present_nup()$date == max(present_nup()$date),]$date, 9, 10))
					hist.sum <- round(historical_nup()[historical_nup()$month == month & historical_nup()$day == day,]$amount, 0)
					return(hist.sum)
				})

			# total water value to be used in value box (excludes any forecast data)
			tw <- reactive({
				if(nrow(total_water()) > 0) {
					total_water() %>%
						filter(time == "present",
									 quality != "forecast") %>%
						ungroup() %>%
						select(water_cumsum) %>%
						max() %>%
						as.numeric() %>%
						round(1)
					} else {
						NA
					}
			})

			hw <- reactive({
				if(nrow(total_water()) > 0) {
					total_water() %>%
						filter(time == "historical") %>%
						ungroup() %>%
						filter(date == daterange_present()[2]) %>%
						select(water_cumsum) %>%
						as.numeric() %>%
						round(1)
					} else {
						NA
					}
			})

			irr <- reactive({
					if(nrow(total_water()) > 0) {
						tw_irr <- total_water() %>%
							filter(measurement == "irr")

							if(nrow(tw_irr) > 0) {
								tw_irr %>%
								ungroup() %>%
								select(amount) %>%
								sum() %>%
								as.numeric() %>%
								round(1)
							} else {
								0
							}
					} else {
						NA
					}
			})

			daterange_present <- reactive({
				if(nrow(total_water())> 0) {
					dates <- total_water() %>%
						filter(time == "present", quality != "forecast") %>%
						ungroup() %>%
						select(date)

					range(dates$date)
				} else {
					c("", "")
				}
			})
			
			

			observe({

			output$total_water_plotly <- plotly::renderPlotly(total_water_plot(weather_data = weather_data(),
																																 total_water = weather_data(),
																																 irrigation = irrigation(),
																																 present_data = prelim_weather_data(),
																																 lat = map_outputs()$lat,
																																 long = map_outputs()$lon,
																																 con = con))

			output$nuptake_plotly <- plotly::renderPlotly(graph_nuptake_plotly(weather_data = weather_data(),
																																 lat = map_outputs()$lat,
																																 long = map_outputs()$lon,
																																 nuptake_mod = map_outputs()$nuptakemod,
																																 con = con))
			
			output$gdd_plotly <- plotly::renderPlotly(graph_gdd_plotly(weather_data = weather_data(),
			                                                                   lat = map_outputs()$lat,
			                                                                   long = map_outputs()$lon,
			                                                                   nuptake_mod = map_outputs()$nuptakemod,
			                                                           con = con))





			output$rainfall <- renderValueBox({

				if ("irr" %in% unique(total_water()$measurement)) {

					valueBox(value = tags$p(paste(tw(), " in (", tw() - irr(), "\" precip. + ", irr(), "\" irrig.)", sep = ""), style = "font-size: 50%"),
									 subtitle = paste0("Total water (", ifelse(substr(min_date(), 6, 6) == 0,
									 																					stringr::str_replace(substr(daterange_present()[1], 7, 10), "-", "/"),
									 																					stringr::str_replace(substr(daterange_present()[1], 6, 10), "-", "/")), "-",
									 									ifelse(substr(daterange_present()[2], 6, 6) == 0,
									 												 stringr::str_replace(substr(daterange_present()[2], 7, 10), "-", "/"),
									 												 stringr::str_replace(substr(daterange_present()[2], 6, 10), "-", "/")), ")"), color = "blue"
					)
				} else {
					# water_output <- c(tw,  round(tw-hw, 1))
					valueBox(value = tags$p(paste(tw(), " in (", ifelse(tw()-hw() >= 0, "+", ""),
																				round(tw()-hw(), 1), " in of average)", sep = ""), style = "font-size: 50%"),
									 subtitle = paste0("Cumulative precipitation (",
									 									ifelse(substr(daterange_present()[1], 6, 6) == 0,
									 												 stringr::str_replace(substr(daterange_present()[1], 7, 10), "-", "/"),
									 												 stringr::str_replace(substr(daterange_present()[1], 6, 10), "-", "/")),
									 									"-", ifelse(substr(daterange_present()[2], 6, 6) == 0,
									 															stringr::str_replace(substr(daterange_present()[2], 7, 10), "-", "/"),
									 															stringr::str_replace(substr(daterange_present()[2], 6, 10), "-", "/")), ")"), color = "blue"
					)

				}

			})

			output$nuptake <- renderValueBox({

				if(map_outputs()$nuptakemod == TRUE){

					# nuptake_output <- c(present_sum, historical_sum)
					valueBox(value = tags$p(paste0(as.character(present_sum()), "%",
																				 if_else(abs(present_sum()-historical_sum()) >= 1,
																				 				paste0(" (",if_else(present_sum()-historical_sum() > 0, "+", ""),
																				 							 round(present_sum()-historical_sum(), 1), "% of average)"), "")), style = "font-size: 50%"),
									 paste0("N Uptake (", ifelse(substr(daterange_present()[1], 6, 6) == 0,
									 														stringr::str_replace(substr(daterange_present()[1], 7, 10), "-", "/"),
									 														stringr::str_replace(substr(daterange_present()[1], 6, 10), "-", "/")), "-",
									 			 ifelse(substr(daterange_present()[2], 6, 6) == 0,
									 			 			 stringr::str_replace(substr(daterange_present()[2], 7, 10), "-", "/"),
									 			 			 stringr::str_replace(substr(daterange_present()[2], 6, 10), "-", "/")), ")"), color = "red")
				} else {
					# nuptake_output <- round(gdd_to_nuptake(feekes_to_gdd(input$growth_stage_user_input)), 0)
					valueBox(value = tags$p(paste0(as.character(round(gdd_to_nuptake(feekes_to_gdd(input$growth_stage_user_input)), 0)), "%"), style = "font-size: 50%"),
									 paste0("N Uptake (", ifelse(substr(daterange_present()[1], 6, 6) == 0,
									 														stringr::str_replace(substr(daterange_present()[1], 7, 10), "-", "/"),
									 														stringr::str_replace(substr(daterange_present()[1], 6, 10), "-", "/")), "-",
									 			 ifelse(substr(daterange_present()[2], 6, 6) == 0,
									 			 			 stringr::str_replace(substr(daterange_present()[2], 7, 10), "-", "/"),
									 			 			 stringr::str_replace(substr(daterange_present()[2], 6, 10), "-", "/")), ")"), color = "red"
					)

				}

			})
			
			output$downloadCSV <- downloadHandler(

				filename = function() {
					paste0("weather_data_", "lat_", map_outputs()$lat, "_long_", map_outputs()$lon, "_", daterange_full()[1], "_", daterange_full()[2], ".csv")
				},
				content = function(file) { write.csv(x = prelim_weather_data() %>%
				                                       select(-correction) %>% 
				                                       filter(measurement == 'ppt' | 
				                                                measurement == 'tmax' | 
				                                                measurement == 'tmin' | 
				                                                measurement == 'gdd') %>%
				                                       mutate(key = if_else(measurement == "ppt",
				                                                            paste(measurement, time, "in", sep = "_"),
				                                                            if_else(measurement == "tmax" | measurement == "tmin",
				                                                                    paste(measurement, time, "F", sep = "_"), 
				                                                                    paste(measurement, time, sep = "_"))),
				                                              data_type = if_else(quality == "prism", "current", quality),
				                                              amount = if_else(measurement == "ppt", amount/25.4, amount)) %>%
				                                       select(-measurement, - time, -quality, -data_type) %>%
				                                       tidyr::spread(key = key, value = amount),
																						 file, row.names = FALSE) }
			)

			output$growth_stage_name <- renderUI({
			  
			  present_prism_gdd <- weather_data()[weather_data()$time == "present" & weather_data()$quality == "prism" & weather_data()$measurement == "gdd_cumsum", ]
			  
			  
			  if(req(map_outputs()$nuptakemod) == FALSE){
			   feekes_current <- reverseValue(growth_stage_option())
			  } else {
			    
			    feekes_current <- gdd_to_feekes((present_prism_gdd[present_prism_gdd$amount == max(present_prism_gdd$amount), ]$amount)*(present_prism_gdd[present_prism_gdd$amount == max(present_prism_gdd$amount), ]$correction))
			    
			  }
				
				forecast_gdd <- weather_data()[weather_data()$time == "present" & weather_data()$quality == "forecast" & weather_data()$measurement == "gdd_cumsum", ]
				forecast_gdd <-max(forecast_gdd[forecast_gdd$amount == max(forecast_gdd$amount), ]$amount)
				
				max_dates <- weather_data() %>% 
				  group_by(quality) %>% 
				  summarize(max(date)) %>% 
				  tidyr::pivot_wider(names_from = quality, values_from = `max(date)`)
				
				if("forecast" %in% names(max_dates)){
				  
				  time_diff <- max_dates %>% 
				    mutate(diff = forecast - prism) %>% 
				    pull(diff) %>% 
				    as.numeric()
				  
				  feekes_forecast <- gdd_to_feekes(forecast_gdd)
				  
				  HTML(paste("<h5>Your <a href = 'https://anrcatalog.ucanr.edu/pdf/8165.pdf' target='_blank'>growth stage</a> is estimated to be <strong>",
				             growth_stage_estimate(feekes_current),
				             ".</strong> Your growth stage forecasted to be <strong>",  growth_stage_estimate(feekes_forecast), "</strong> in ",  time_diff , " days (", ifelse(substr(max_dates$forecast, 6, 6) == 0,
				                                                                                                                                                                stringr::str_replace(substr(max_dates$forecast, 7, 10), "-", "/"),
				                                                                                                                                                                stringr::str_replace(substr(max_dates$forecast, 6, 10), "-", "/")), "). The growth stage is the most advanced growth stage that 50% of plants in the field have reached. Make adjustments to the estimated crop growth stage below if you want to change the seasonal N uptake estimate.</h5>", sep = ""))
				  
				} else {
				  
				  HTML(paste("<h5>Your <a href = 'https://anrcatalog.ucanr.edu/pdf/8165.pdf' target='_blank'>growth stage</a> is estimated to be <strong>",
				             growth_stage_estimate(feekes_current),
				             ".</strong> The growth stage is the most advanced growth stage that 50% of plants in the field have reached. Make adjustments to the estimated crop growth stage below if you want to change the seasonal N uptake estimate.</h5><br></br>", sep = ""))
				}
				
			})
			
			output$reactive_growth_stage <- renderUI({
			  if(map_outputs()$nuptakemod == TRUE){
			  HTML(paste("<h5>The graphs show the relationship between time, crop growth stage, N uptake, and seasonal water. Estimates of growth stage and N uptake reflect relationships between time and <a href = 'http://ipm.ucanr.edu/WEATHER/ddconcepts.html' target='_blank'>growing degree days</a>.", "<br></br>", "<strong>At " , growth_stage_estimate(input$growth_stage_user_input)," N uptake is estimated to be ", round(gdd_to_nuptake(feekes_to_gdd(input$growth_stage_user_input)), 0), "% of seasonal total.</strong></h5>"))
			  } else {
			    HTML(paste("<h5>The graph shows the relationship between time and seasonal water.", "<br></br>", "<strong>At " , growth_stage_estimate(input$growth_stage_user_input)," N uptake is estimated to be ", round(gdd_to_nuptake(feekes_to_gdd(input$growth_stage_user_input)), 0), "% of seasonal total.</strong></h5>"))
			  }
			})


	}) # end of observe

	nuptake_output <- reactive({
		if(map_outputs()$nuptakemod == TRUE){
			c(present_sum(), historical_sum())
		} else {
			round(gdd_to_nuptake(feekes_to_gdd(input$growth_stage_user_input)), 0)
		}
	})

	water_output <- reactive({
		if(map_outputs()$nuptakemod == TRUE){
			c(tw(), tw() - irr(), irr())
		} else {
			c(tw(),  round(tw()-hw(), 1))
		}
	})
	
	observeEvent(input$back_to_location, {
	  print("going back")
		updateTabItems(parent, "tabs", "location")
	})

			return(list(growth_stage = reactive({input$growth_stage_user_input}),
									weather_data = weather_data,
									#nuptake_output = nuptake_output,
									min_date = min_date, max_date = max_date,
									water_output = water_output))

		}
)}
