graph_gdd_plotly <- function(weather_data, lat, long, nuptake_mod, con) {
  
  print("GDD GRAPH")
  print(names(weather_data))

	county <- DBI::dbGetQuery(con, paste0("SELECT namelsad FROM grain.ca_counties WHERE ST_Contains(geom, ST_GeomFromText('POINT(", long, " ", lat, ")',4326));"))$namelsad
	plot_title <- paste0(c(""),
		substr(as.character(min(weather_data[weather_data$time == "present", "date"])), 6,10), " to ",
		substr(as.character(max(weather_data[weather_data$time == "present", "date"])), 6,10), " (", county, "; ", as.character(round(lat, 2)), ", ", as.character(round(long, 2)), ")")

	named_colors <- c(gdd_cumsum.forecast = 'gold',gdd_cumsum.historical = "mediumorchid", gdd_cumsum.present = "mediumorchid")
	
	named_lines <- c(gdd_cumsum.forecast = 'solid', gdd_cumsum.historical = "dash", gdd_cumsum.present = "solid")
	
	named_labels <- c(gdd_cumsum.forecast = 'Cumulative GDD Forecast', gdd_cumsum.historical = "Cumulative GDD (10-yr avg)", gdd_cumsum.present = "Cumulative GDD (current season)")
	
	named_titles <- c(gdd_cumsum.forecast = 'Cumulative GDD: ', gdd_cumsum.historical = "Cumulative GDD: ", gdd_cumsum.present = "Cumulative GDD: ")
	
	print(unique(weather_data$measurement))
	
	df <- data.frame(growth_stage = c("emergence", "early-tillering", "mid-tillering", "late-tillering", 
	                                  "1-node visible", "2-nodes visible", "last leaf visible", "last leaf fully formed", 
	                                  "early boot", "late-boot/early-heading", "heading", "flowering", "milk", "dough", "kernel hard", "maturity"),
	                 gdd = c(0, 350, 425, 500, 515, 530, 620, 680, 740, 800, 893, 986, 1100, 1300, 1550, 1650),
	                 feekes = c(0, 3,4,5,6,7,8, 9,10, 10.1, 10.3, 10.5, 11.1, 11.2, 11.3, 11.4))
	df$nup <- gdd_to_nuptake(df$gdd)
	
	gs_ranges <-	weather_data %>% 
	  filter(time == "historical", 
	         measurement == "feekes" | measurement == "gdd_cumsum") %>% 
	  tidyr::pivot_wider(names_from = measurement, values_from = amount) %>% 
	  mutate(growth_stage = if_else(feekes >= 3 & feekes < 6, "tillering",
	                                if_else(feekes >= 10.1 & feekes < 10.5, "heading",
	                                        if_else(feekes == 11.4, "maturity", "NA")))) %>% 
	  filter(growth_stage != "NA") %>% 
	  group_by(growth_stage) %>% 
	  summarize(min = min(gdd_cumsum),
	            max = max(gdd_cumsum))
	
	print(gs_ranges)
	
	gs_ranges <- gs_ranges %>% 
	  mutate(max = if_else(growth_stage == "maturity", 
	                       gs_ranges$min[2] + (gs_ranges$max[1] - gs_ranges$min[1]),
	                       max)) %>% 
	  mutate(average = (min + max)/2) %>% 
	  right_join(data.frame(growth_stage = c("heading", "maturity", "tillering"))) %>% 
	  mutate(growth_stage = factor(growth_stage, levels = c("heading", "maturity", "tillering"))) %>% 
	  arrange(growth_stage)
	
	print(gs_ranges)
	                                                               
	
	visibility <- c(gdd_cumsum.forecast = T, gdd_cumsum.historical = F, gdd_cumsum.present = T)

	# only care about certain measurements
	weather_data <- weather_data %>%
		filter(measurement %in%
			unlist(lapply(strsplit(gdd_lines$var, "\\."), function(x) { x[1]}))
		)

	fig <- plotly::plot_ly(colors = named_colors, linetypes = named_lines)

		data <- weather_data %>% filter(quality != 'forecast') %>%
					mutate(plot_group = interaction(measurement, time))

		data <- rbind(data, weather_data %>% filter(quality == "forecast") %>%
					mutate(plot_group = interaction(measurement, quality)))

		# add the latest precip cumsum point as a forecast point so the lines match up
		# only do this if there is forecast data, otherwise we introduce fake forecast
		# data, say when looking at previous years

		# the above was showing forecast data on the graph for data that was from prism - changed to still make
		# the lines match but connect using duplicate prism data point

		if(data %>% filter(plot_group == 'rel_precip_cumsum.forecast') %>% nrow() > 0) {
			data <- rbind(data,
								weather_data %>%
								filter(measurement == 'rel_precip_cumsum', time == 'present', quality == 'forecast') %>%
								arrange(date) %>%
								head(1) %>%
								mutate(quality = 'prism', plot_group = 'rel_precip_cumsum.present')
							)

				data <- data %>% arrange(date)
			}

		# same for the nuptake forecast
		if(data %>% filter(plot_group == 'gdd_cumsum.forecast') %>% nrow() > 0) {
			data <- rbind(data,
								weather_data %>%
								filter(measurement == 'gdd_cumsum', time == 'present', quality == 'forecast') %>%
								arrange(date) %>%
								head(1) %>%
								mutate(quality = 'forecast', plot_group = 'gdd_cumsum.present')
							)

				data <- data %>% arrange(date)
			}


		# }
	

	# turn on historical nuptake % if present is less than 5%
	if(data %>%
			filter(plot_group == 'gdd_cumsum.present') %>%
			select(amount) %>%
			max() < 5 |

			data %>%
				filter(plot_group == 'rel_precip_cumsum.present') %>%
				select(amount) %>%
				max() < 5

		) {

		visibility$gdd_cumsum.historical = T
		visibility$rel_precip_cumsum.historical = T
	}

	data <- data %>%
		mutate(label = named_labels[as.character(plot_group)],
					title = named_titles[as.character(plot_group)],
					visible = visibility[as.character(plot_group)])

		fig <- fig %>%
		  plotly::add_trace(
			data = data %>%
			  filter(visible == T),
			name = ~label,
			x = ~date,
			y = ~amount,
			color = ~plot_group,
			linetype = ~plot_group,
			type = 'scatter',
			mode = 'lines',
			hoverinfo = 'text',
			text = ~paste0(label, "<br>", "Date: ", date, "<br>", title, round(amount, 1), " GDD")
		)

		fig <- fig %>%
		  plotly::add_trace(data = data %>%
		                      filter(visible == F),
		                    name = ~label,
		                    x = ~date,
		                    y = ~amount,
		                    color = ~plot_group,
			linetype = ~plot_group,
			type = 'scatter', mode = 'lines',
			hoverinfo = 'text',
			text = ~paste0(label, "<br>", "Date: ", date, "<br>", title, round(amount, 1), "%"),
			visible = "legendonly"
		)


	tick_font = list(size = 14)
	title_font = list(size = 14)
	
	fig <- fig %>% plotly::layout(title = plot_title,
	                              font = list(size = 11),
	                              shapes = list(hline(gs_ranges$min[2]),
	                                            hline(gs_ranges$max[2]), hline(gs_ranges$min[1]), hline(gs_ranges$max[1]), hline(gs_ranges$min[3]), hline(gs_ranges$max[3])),
	                              xaxis = list(title = F, tickfont = tick_font, titlefont = title_font), 
	                              yaxis = list(range = c(min(data$amount), 2000), title = "Cumulative GDD", tickfont = tick_font, titlefont = title_font),
												margin = list(l = 50, r = 50, b = 0, t = 25),
												showlegend = T, legend = list(orientation = 'h', y = -0.25)) %>% 
	  plotly::add_text(showlegend = FALSE, x = c(min(data$date) + ((max(data$date) - min(data$date))/5),min(data$date) + ((max(data$date) - min(data$date))/5), min(data$date) + ((max(data$date) - min(data$date))/5)), y = c(gs_ranges$average[3],gs_ranges$average[1], gs_ranges$average[2]),
	           text = c("tillering","heading", "maturity"))
	
	return(fig)

}
