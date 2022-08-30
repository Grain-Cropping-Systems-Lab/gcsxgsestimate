graph_gdd_plotly <- function(weather_data, lat, long, nuptake_mod, con) {

	county <- DBI::dbGetQuery(con, paste0("SELECT namelsad FROM grain.ca_counties WHERE ST_Contains(geom, ST_GeomFromText('POINT(", long, " ", lat, ")',4326));"))$namelsad
	plot_title <- paste0(c(""),
		substr(as.character(min(weather_data[weather_data$time == "present", "date"])), 6,10), " to ",
		substr(as.character(max(weather_data[weather_data$time == "present", "date"])), 6,10), " (", county, "; ", as.character(round(lat, 2)), ", ", as.character(round(long, 2)), ")")

	named_colors <- c(gdd_cumsum.forecast = 'gold',gdd_cumsum.historical = "orangered", gdd_cumsum.present = "orangered")
	
	named_lines <- c(gdd_cumsum.forecast = 'solid', gdd_cumsum.historical = "dash", gdd_cumsum.present = "solid")
	
	named_labels <- c(gdd_cumsum.forecast = 'Cumulative GDD Forecast', gdd_cumsum.historical = "Cumulative GDD (10-yr avg)", gdd_cumsum.present = "Cumulative GDD (current season)")
	
	named_titles <- c(gdd_cumsum.forecast = 'Cumulative GDD: ', gdd_cumsum.historical = "Cumulative GDD: ", gdd_cumsum.present = "Cumulative GDD: ")
	
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

	fig <- fig %>% plotly::layout(title = plot_title, font = list(size = 11),
												xaxis = list(title = F, tickfont = tick_font, titlefont = title_font),
												yaxis = list(range = range(data$amount) * c(1, 1.15), title = "Cumulative GDD", tickfont = tick_font, titlefont = title_font),
												margin = list(l = 50, r = 50, b = 0, t = 25),
												showlegend = T, legend = list(orientation = 'h', y = -0.25))
	return(fig)

}
