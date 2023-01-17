location_page_ui <- function(id, label = "Location") {
	ns <- NS(id)
	
	shinydashboard::tabItem(tabName = "location",
					fluidRow(
						column(6,
									 box(title = p("Location"), solidHeader = TRUE, status = "primary", width = 12,
									 		p("Click or move the marker to the location of interest. For full functionality,
											choose a field within the wheat-growing region of CA (non-shaded region)."),
									 		map_mod_ui(ns("map_mod"))),
									 box(title = p("Period of interest"), solidHeader = TRUE, status = "primary", width = 12,
									 		HTML(paste0("Choose dates below that begin with the planting date and span the period of interest. The date
range is limited to one ", actionLink(ns("actionlink"), "wheat growing season"), ". A 10-day forecast is available for locations in the Central Valley and desert regions.")),
									 		uiOutput(outputId = ns("daterange")))),
						column(6,
									 conditionalPanel(condition = "output.nuptake_panel", ns = ns,
									 								 box(title = p("Growth Stage"), solidHeader = TRUE, status = "primary", width = 12,
									 								 		HTML(paste0("Choose the ",
									 								 								"<a href = 'https://anrcatalog.ucanr.edu/pdf/8165.pdf' target='_blank'>growth stage</a>",
									 								 								" of the plants for the current date or if looking historically, the latest date in your chosen date range.
										 								 								 The growth stage should be the most advanced growth stage that 50% of plants in the field have reached.")),
									 								 		fluidRow(column(12, img(src="www/growth_stages_linear.png", class="img-responsive"))),
									 								 		fluidRow(column(12, class = 'slider-container', shinyWidgets::noUiSliderInput(inputId = ns("growth_stage_user_input_1"), label = NULL, color = "#005fae", min = 0, max = 14, value = 0, step = 0.1))),
									 								 		br()
									 								 )),
									 conditionalPanel(condition = "!output.nuptake_panel",
									 								 box(title = p("Crop Type"), solidHeader = TRUE, status = "primary", width = 12,
									 		p("Choose the crop type or variety (if known) for the field of interest."),
									 		uiOutput(outputId = ns("variety")))),
									 box(title = p("Irrigation"), solidHeader = TRUE, status = "primary", width = 12,
									 		checkboxInput(ns("moisture"), "This field was planted into moisture or irrigated up.", value = FALSE),
									 		checkboxInput(ns("irrigation"), "This field was irrigated.", value = FALSE),
									 		conditionalPanel(condition = "input.irrigation == 1", ns = ns,
									 		                 shinyWidgets::noUiSliderInput(inputId = ns("numIrr"),
									 										 								label = "Number of Irrigations",
									 										 								value = 1,
									 										 								min = 1,
									 										 								max = 12,
									 										 								step = 1,
									 										 								color = "#005fae",
									 										 								tooltips = TRUE,
									 										 								pips = list(mode = 'values',
									 										 														values = 1:12, format = shinyWidgets::wNumbFormat(decimals = 0)),
									 										 								format = shinyWidgets::wNumbFormat(decimals = 0))),
									 		conditionalPanel(condition = "input.irrigation == 1", ns = ns,
									 										 column(6, uiOutput(ns("dateGroup"))),
									 										 column(6, uiOutput(ns("inputGroup")))
									 		),
									 		br()#,
									 		#p("If you are satisfied with the information you entered and are ready to gather your site-specific data, click 'Next' below."),

						),
						shinyBS::bsButton(ns("switchtab"), label = "Next", block = TRUE, style="default", size = "lg")
						)
					)
	)
}

location_page_server <- function(id, parent, con, api_key){
	moduleServer(
		id,
		function(input, output, session){

			ns <- session$ns
			
			max_forecast_date <- DBI::dbGetQuery(con, "SELECT DISTINCT(date) FROM grain.prism WHERE quality = 'forecast' ORDER BY date DESC LIMIT 1;")
			
			if (length(max_forecast_date$date) == 0){
			  max_forecast_date <- data.frame(date = as.Date("2009-01-01"))
			}
			
			max_prism_date <- DBI::dbGetQuery(con, "SELECT DISTINCT(date) FROM grain.prism WHERE quality != 'forecast' ORDER BY date DESC LIMIT 1;")
			
			variety_list <- readr::read_csv("data/rel_gdd_crop_type.csv") 
			
			output$daterange <- renderUI({
			  dateRangeInput(session$ns('daterange'), label = "", format = 'mm/dd/yyyy',
			                 start = if_else(as.Date(paste0(lubridate::year(Sys.Date()),
			                                                "-11-15")) <= Sys.Date(),
			                                 as.Date(paste0(lubridate::year(Sys.Date()), "-11-15")),
			                                 if_else(as.Date(paste0(lubridate::year(Sys.Date()),
			                                                        "-10-01")) <= Sys.Date(),
			                                         as.Date(paste0(lubridate::year(Sys.Date()),
			                                                        "-10-01")), as.Date(paste0(lubridate::year(Sys.Date())-1, "-11-15")))),
			                 end = if_else(
			                   as.Date(paste0(lubridate::year(Sys.Date()), "-06-30")) >= Sys.Date(),
			                   max_forecast_date$date,
			                   if_else(as.Date(paste0(lubridate::year(Sys.Date()),
			                                          "-10-01")) <= Sys.Date(),
			                           max_forecast_date$date,	as.Date(paste0(lubridate::year(Sys.Date()),"-06-30")))),
			                 min = as.Date("2009-01-01"),
			                 max = max(max_forecast_date$date, max_prism_date$date)
			  )
			})
			
			
			
			map_outputs <- map_mod_server("map_mod",
			                              api_key = api_key,
																		shapefile_path = "inst/extdata/ca_wheat_regions.shp",
																		region_behavior = region_behavior_nmanagement,
																		default_lat = 38.533867,
																		default_lon = -121.771598,
																		scope_id = id)
			
			observe({
			  if (as.logical(req(map_outputs$nuptakemod)) == TRUE){
			    updateDateRangeInput(session = session, inputId = 'daterange', label = "",
			                         start = req(input$daterange[1]),
			                         end = if_else(req(input$daterange[2]) == if_else(as.Date(paste0(lubridate::year(Sys.Date()), "-08-31")) >= max_prism_date$date,
			                                                                          max_prism_date$date,
			                                                                          as.Date(paste0(lubridate::year(Sys.Date()),"-08-31"))),
			                                       if_else(as.Date(paste0(lubridate::year(Sys.Date()), "-06-30")) >= max_prism_date$date,
			                                               max_forecast_date$date,
			                                               as.Date(paste0(lubridate::year(Sys.Date()),"-06-30"))),
			                                       req(input$daterange[2])),
			                         min = as.Date("2009-01-01"), max = max(max_forecast_date$date, max_prism_date$date))
			  } else {
			    updateDateRangeInput(session = session, inputId = 'daterange', label = "",
			                         start = if_else(req(input$daterange[1]) > max_prism_date$date, max_prism_date$date, req(input$daterange[1])),
			                         end = if_else(req(input$daterange[2]) == if_else(as.Date(paste0(lubridate::year(Sys.Date()), "-06-30")) >= max_prism_date$date,
			                                                                          max_forecast_date$date,
			                                                                          as.Date(paste0(lubridate::year(Sys.Date()),"-06-30"))),
			                                       if_else(as.Date(paste0(lubridate::year(Sys.Date()), "-08-31")) >= max_prism_date$date,
			                                               max_prism_date$date,
			                                               as.Date(paste0(lubridate::year(Sys.Date()),"-08-31"))),
			                                       if_else(req(input$daterange[2]) > max_prism_date$date, max_prism_date$date, req(input$daterange[2]))),
			                         min = as.Date("2009-01-01"), max = max_prism_date$date)
			  }
			  
			  output$nuptake_panel = reactive({
			    !as.logical(req(map_outputs$nuptakemod))
			  })
			  
			  outputOptions(output, "nuptake_panel", suspendWhenHidden = FALSE)
			  
			})	


			observeEvent(input$actionlink, {
				showModal(
					modalDialog(
						title = "What is a wheat growing season?",
						imageOutput(ns("growingSeasonImage")),
						HTML("Jackson, L. and Williams, J. Growth and Development of Small Grains. Publication 8165 in <a href = 'https://anrcatalog.ucanr.edu/pdf/8208.pdf' target='_blank'>UC Small Grain Production Manual</a>.")
					)
				)
			})
			
			output$variety <- renderUI({
			  selectInput(inputId = session$ns("variety"), label = "", choices = unique(variety_list$crop_sub_type), selected = "COMMON")
			})

			output$growingSeasonImage <- renderImage({
				filename <- "inst/extdata/testing_tooltip.PNG"

				# Return a list containing the filename and alt text
				list(src = filename,
						 alt = paste("Image number"))

			},
			deleteFile = FALSE)

			irrigation_memory <- clear_irrigation_memory()
			
			observeEvent(input$irrigation | input$numIrr, {
			  if(input$numIrr > 0) {
			    if(!is.null(input$daterange)){
			      irrigation_memory <- memoize_irrigation(input, irrigation_memory)
			      output$inputGroup = build_memoized_irrigation_amounts(input$numIrr, irrigation_memory, session = session)
			      output$dateGroup =
			        build_memoized_irrigation_dates(isolate(input$numIrr),
			                                        req(input$daterange[1]),
			                                        req(input$daterange[2]),
			                                        irrigation_memory, session = session)
			    }
			  }
			})
			

			irrigation <- reactiveVal(data.frame(date = as.Date(character()), amount = numeric()))
			bind_data <- reactiveVal(data.frame(date = as.Date(character()), month = numeric(),
																					day = numeric(), quality = character(),
																					time = character(), measurement = character(),
																					amount = numeric()))

			observeEvent(input$switchtab, {

				if (input$irrigation == 1){
					for (i in seq(1, input$numIrr)){

						if(length(input[[paste0("Date", i)]]) == 0){
							date <- as.Date("1970-01-01")
						} else {
							date <- as.Date(input[[paste0("Date", i)]], origin = "1970-01-01")
						}

						amount <- ifelse(length(input[[paste0("Irrigation", i)]]) == 0,
														 0,
														 input[[paste0("Irrigation", i)]])

						#irrigation(bind_rows(irrigation(), data.frame(date = date, amount = amount)))
						irrigation(data.frame(date = date, amount = amount))
					}

				}

				irrigation_check <- !any(irrigation()$amount < 0 | irrigation()$amount > 12)
				if(irrigation_check == FALSE){
					showNotification("Error: Irrigation amounts out of bounds!", id = "irrigation_error")
				}
				
				date_check <- check_dates(daterange = input$daterange,
																	irrigation_input = input$irrigation,
																	irrigation = irrigation(),
																	region = map_outputs$region,
																	max_prism_date = max_prism_date)
				
				
				
				if(date_check == TRUE & irrigation_check == TRUE){
				  
				  shinyBS::updateButton(parent, inputId = "switchtab", label = "Next", block = TRUE, style="default", size = "lg", disabled = TRUE)
				  
				  updateTabItems(parent, "tabs", "initial_outputs")
				  
				  withProgress(message = "Gathering current and historical season data...", value = 0, min = 0, max = 100, {
				    
				    historical_data <- prism_historical_season_all(con = con,
				                                                   lat = map_outputs$lat,
				                                                   long = map_outputs$lon,
				                                                   current_start_date = input$daterange[1],
				                                                   end_date = if_else((map_outputs$region == "IR" |  is.na(map_outputs$region)) & lubridate::month(input$daterange[2]) > 9 & lubridate::year(input$daterange[1]) != lubridate::year(input$daterange[2]),
				                                                                      input$daterange[2],
				                                                                      if_else((map_outputs$region == "IR" |  is.na(map_outputs$region)) & lubridate::year(input$daterange[1]) != lubridate::year(input$daterange[2]),
				                                                                              as.Date(paste0(lubridate::year(input$daterange[2]), "-08-31")),
				                                                                              if_else((map_outputs$region == "IR" |  is.na(map_outputs$region)) & lubridate::year(input$daterange[1]) == lubridate::year(input$daterange[2]) & lubridate::month(input$daterange[1]) >= 10,
				                                                                                      as.Date(paste0(lubridate::year(input$daterange[2]) + 1, "-08-31")),
				                                                                                      if_else((map_outputs$region == "IR" |  is.na(map_outputs$region)) & lubridate::year(input$daterange[1]) == lubridate::year(input$daterange[2]) & lubridate::month(input$daterange[1]) < 10,
				                                                                                              as.Date(paste0(lubridate::year(input$daterange[2]), "-08-31")),
				                                                                                              if_else(map_outputs$region != "IR" & lubridate::month(input$daterange[2]) > 7 & lubridate::year(input$daterange[1]) != lubridate::year(input$daterange[2]),
				                                                                                                      input$daterange[2],
				                                                                                                      if_else(map_outputs$region != "IR" & lubridate::year(input$daterange[1]) != lubridate::year(input$daterange[2]),
				                                                                                                              as.Date(paste0(lubridate::year(input$daterange[2]), "-06-30")),
				                                                                                                              if_else(map_outputs$region != "IR" & lubridate::year(input$daterange[1]) == lubridate::year(input$daterange[2]) & lubridate::month(input$daterange[1]) >= 10,
				                                                                                                                      as.Date(paste0(lubridate::year(input$daterange[2]) + 1, "-06-30")),
				                                                                                                                      as.Date(paste0(lubridate::year(input$daterange[2]), "-06-30"))
				                                                                                                              )
				                                                                                                      )
				                                                                                              )
				                                                                                      )
				                                                                              )
				                                                                      )
				                                                   )
				    )
				    
				    incProgress(10)
				    
				    present_data <- prism_date_range_all(con = con, lat = map_outputs$lat, long = map_outputs$lon, from_date = input$daterange[1], to_date = input$daterange[2])
				    
				    # remove forecast data that overlaps with PRISM data and NA's
				    removes <- present_data %>%
				      group_by(date) %>%
				      filter(n()>1) %>%
				      filter(quality == "forecast")
				    
				    present_data <- anti_join(present_data, removes) %>% # recalculate cumulative sums in case there was a failed data point in PRISM or forecast
				      mutate(precip_cumsum = cumsum(ppt),
				             gdd_cumsum = cumsum(gdd),
				             nuptake_perc = gdd_to_nuptake(gdd_cumsum),
				             rel_precip_cumsum = precip_cumsum/max(historical_data$precip_cumsum)*100) #%>% 
				      #rowwise() %>% 
				    
				    
				    lowset <- present_data[present_data$gdd_cumsum <= 1125,]
				    variableset <-present_data[present_data$gdd_cumsum > 1125,]
				    
				    lowset$correction <- (variety_list %>% 
				                            filter(group == 1000) %>% 
				                            filter(crop_sub_type == input$variety))$relative_gs
				    
				    variableset$correction <- apply(as.matrix(variableset$gdd_cumsum), 1, FUN = gs_correction, crop_type = input$variety, gd_rel_gdds_input = variety_list)
				    
				    present_data <- bind_rows(lowset, variableset) %>% 
				      mutate(nuptake_perc = gdd_to_nuptake(gdd_cumsum*correction)) %>%
				      tidyr::drop_na()
				    
				    shinyBS::updateButton(session, inputId = "switchtab", label = "Next", block = TRUE, style="default", size = "lg", disabled = FALSE)
				  }) # end of progress bar tracking
				  
				  
				  if (input$moisture == 0) {
				    if(input$irrigation == 1){
				      
				      first_water <- min(c(min(irrigation()[irrigation()$amount > 0, "date"]),
				                           min(as.data.frame(present_data)[as.data.frame(present_data)$ppt > 0, "date"])))
				      present_data <- present_data %>%
				        mutate(gdd_temp = if_else(date < first_water, 0, gdd),
				               gdd_cumsum = cumsum(gdd_temp))# %>% 
				        #rowwise() %>% 
				      
				      
				      lowset <- present_data[present_data$gdd_cumsum <= 1125,]
				      variableset <-present_data[present_data$gdd_cumsum > 1125,]
				      
				      lowset$correction <- (variety_list %>% 
				                              filter(group == 1000) %>% 
				                              filter(crop_sub_type == input$variety))$relative_gs
				      
				      variableset$correction <- apply(as.matrix(variableset$gdd_cumsum), 1, FUN = gs_correction, crop_type = input$variety, gd_rel_gdds_input = variety_list)
				      
				      present_data <- bind_rows(lowset, variableset) %>% 
				        mutate(nuptake_perc = gdd_to_nuptake(gdd_cumsum*correction)) #%>%
				        #select(-gdd_temp)
				      
				      # moving the beginning of the historical N uptake to the current season's first irrigation
				      historical_data <- historical_data %>%
				        mutate(gdd_temp = if_else(pseudo_date < first_water, 0, gdd),
				               gdd_cumsum = cumsum(gdd_temp)) #%>% 
				        #rowwise() %>% 
				      
				      
				      lowset <- present_data[present_data$gdd_cumsum <= 1125,]
				      variableset <-present_data[present_data$gdd_cumsum > 1125,]
				      
				      lowset$correction <- (variety_list %>% 
				                              filter(group == 1000) %>% 
				                              filter(crop_sub_type == input$variety))$relative_gs
				      
				      variableset$correction <- apply(as.matrix(variableset$gdd_cumsum), 1, FUN = gs_correction, crop_type = input$variety, gd_rel_gdds_input = variety_list)
				      
				      present_data <- bind_rows(lowset, variableset) %>% 
				        mutate(nuptake_perc = gdd_to_nuptake(gdd_cumsum*correction)) %>%
				        select(-gdd_temp)
				      
				    } else{
				      
				      first_ppt <- present_data %>% 
				        filter(ppt > 0) %>% 
				        tidyr::drop_na() %>% 
				        pull(date) %>% 
				        min()
				      
				      present_data <- present_data %>%
				        mutate(gdd_temp = if_else(date < first_ppt, 0, gdd),
				               gdd_cumsum = cumsum(gdd_temp))# %>% 
				        #rowwise() %>% 
				      
				      
				      lowset <- present_data[present_data$gdd_cumsum <= 1125,]
				      variableset <-present_data[present_data$gdd_cumsum > 1125,]
				      
				      lowset$correction <- (variety_list %>% 
				                              filter(group == 1000) %>% 
				                              filter(crop_sub_type == input$variety))$relative_gs
				      
				      variableset$correction <- apply(as.matrix(variableset$gdd_cumsum), 1, FUN = gs_correction, crop_type = input$variety, gd_rel_gdds_input = variety_list)
				      
				      present_data <- bind_rows(lowset, variableset) %>% 
				        mutate(nuptake_perc = gdd_to_nuptake(gdd_cumsum*correction)) %>%
				        tidyr::drop_na() #%>% 
				        #select(-gdd_temp)
				      
				      # moving the beginning of the historical N uptake to the current season's first rainfall
				      historical_data <- historical_data %>%
				        mutate(gdd_temp = if_else(pseudo_date < first_ppt, 0, gdd),
				               gdd_cumsum = cumsum(gdd_temp))# %>% 
				        #rowwise() %>% 
				      
				      
				      lowset <- present_data[present_data$gdd_cumsum <= 1125,]
				      variableset <-present_data[present_data$gdd_cumsum > 1125,]
				      
				      lowset$correction <- (variety_list %>% 
				                              filter(group == 1000) %>% 
				                              filter(crop_sub_type == input$variety))$relative_gs
				      
				      variableset$correction <- apply(as.matrix(variableset$gdd_cumsum), 1, FUN = gs_correction, crop_type = input$variety, gd_rel_gdds_input = variety_list)
				      
				      present_data <- bind_rows(lowset, variableset) %>% 
				        mutate(nuptake_perc = gdd_to_nuptake(gdd_cumsum*correction)) %>%
				        tidyr::drop_na() %>% 
				        select(-gdd_temp)
				      
				    }
				    
				  }
				  
				  historical_data_long <- historical_data %>%
				    mutate(time = "historical",
				           quality = "historical",
				           date = pseudo_date) %>%
				    select(-pseudo_date) %>%
				    tidyr::gather(measurement, amount, -date, -month, -day, -time, -quality)
				  
				  present_data_long <- present_data %>%
				    mutate(time = "present",
				           quality = if_else(quality == "forecast", "forecast", "prism")) %>%
				    tidyr::gather(measurement, amount, -date, -month, -day, -time, -quality)
				  
				  bind_data(data.frame(date = as.Date(character()), month = numeric(),
				                       day = numeric(), quality = character(),
				                       time = character(), measurement = character(),
				                       amount = numeric()))
				  bind_data(bind_rows(bind_data(), present_data_long, historical_data_long))
				  
				}
				
				shinyjs::runjs(autoscroll_to_anchor)

			})


			return(list(map_outputs = reactive({map_outputs}),
									growth_stage_option = reactive({input$growth_stage_user_input_1}),
									prelim_weather_data = bind_data,
									irrigation = irrigation))

		}
	)
}
