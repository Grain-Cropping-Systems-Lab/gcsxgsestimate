graph_nuptake_plotly <- function(weather_data, lat, long, nuptake_mod) {

	county <- DBI::dbGetQuery(con, paste0("SELECT namelsad FROM grain.ca_counties WHERE ST_Contains(geom, ST_GeomFromText('POINT(", long, " ", lat, ")',4326));"))$namelsad
	plot_title <- paste0(c(""),
		substr(as.character(min(weather_data[weather_data$time == "present", "date"])), 6,10), " to ",
		substr(as.character(max(weather_data[weather_data$time == "present", "date"])), 6,10), " (", county, "; ", as.character(round(lat, 2)), ", ", as.character(round(long, 2)), ")")

	named_colors <- c(nuptake_perc.forecast = 'gold', rel_precip_cumsum.forecast = "gold",nuptake_perc.historical = "orangered", nuptake_perc.present = "orangered", rel_precip_cumsum.historical = "dodgerblue", rel_precip_cumsum.present = "dodgerblue")
	named_lines <- c(nuptake_perc.forecast = 'solid', rel_precip_cumsum.forecast = "solid",nuptake_perc.historical = "dash", nuptake_perc.present = "solid", rel_precip_cumsum.historical = "dash", rel_precip_cumsum.present = "solid")
	named_labels <- c(nuptake_perc.forecast = 'N uptake Forecast', rel_precip_cumsum.forecast = "Precipitation Forecast", nuptake_perc.historical = "N uptake (10-yr avg)", nuptake_perc.present = "N uptake (current season)", rel_precip_cumsum.historical = "Precipitation (10-yr avg)", rel_precip_cumsum.present = "Precipitation (current season)")
	named_titles <- c(nuptake_perc.forecast = '% of total N uptake: ', rel_precip_cumsum.forecast = "% of total precipitation: ", nuptake_perc.historical = "% of total N uptake: ", nuptake_perc.present = "% of total N uptake: ", rel_precip_cumsum.historical = "% of total precipitation: ", rel_precip_cumsum.present = "% of total precipitation: ")
	visibility <- c(nuptake_perc.forecast = T, rel_precip_cumsum.forecast = T, nuptake_perc.historical = F, nuptake_perc.present = T, rel_precip_cumsum.historical = F, rel_precip_cumsum.present = T)

	# only care about certain measurements
	weather_data <- weather_data %>%
		filter(measurement %in%
			unlist(lapply(strsplit(nuptake_lines$var, "\\."), function(x) { x[1]}))
		)

	fig <- plotly::plot_ly(colors = named_colors, linetypes = named_lines)

	if(nuptake_mod == TRUE) {
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
		if(data %>% filter(plot_group == 'nuptake_perc.forecast') %>% nrow() > 0) {
			data <- rbind(data,
								weather_data %>%
								filter(measurement == 'nuptake_perc', time == 'present', quality == 'forecast') %>%
								arrange(date) %>%
								head(1) %>%
								mutate(quality = 'forecast', plot_group = 'nuptake_perc.present')
							)

				data <- data %>% arrange(date)
			}


		# }
	} else {
			data <- weather_data %>%
				filter(measurement == "rel_precip_cumsum" | measurement == "nuptake_perc") %>%
				mutate(plot_group = interaction(measurement, time)) %>%
				filter(plot_group != "nuptake_perc.historical")
	}

	# turn on historical nuptake % if present is less than 5%
	if(data %>%
			filter(plot_group == 'nuptake_perc.present') %>%
			select(amount) %>%
			max() < 5 |

			data %>%
				filter(plot_group == 'rel_precip_cumsum.present') %>%
				select(amount) %>%
				max() < 5

		) {

		visibility$nuptake_perc.historical = T
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
			text = ~paste0(label, "<br>", "Date: ", date, "<br>", title, round(amount, 1), "%")
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
												yaxis = list(range = range(data$amount) * c(1, 1.15), title = "% of Total", tickfont = tick_font, titlefont = title_font),
												margin = list(l = 50, r = 50, b = 0, t = 25),
												showlegend = T, legend = list(orientation = 'h', y = -0.25))
	return(fig)

}
