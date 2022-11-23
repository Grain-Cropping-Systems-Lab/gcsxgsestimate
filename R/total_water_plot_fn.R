total_water_plot <- function(weather_data, total_water, irrigation, present_data, lat, long, ranges, lines, con){

		irrigation <- irrigation %>%
			mutate(date = as.Date(date),
						 month = lubridate::month(date),
						 day = lubridate::day(date),
						 time = "present",
						 measurement = "irr",
						 amount = amount,
						 quality = "irrigation")

		total_water <- weather_data %>%
			filter(measurement == "ppt",
						 date <= max(present_data$date)) %>%
			mutate(amount = amount/25.4) %>%
			bind_rows(irrigation) %>%
			group_by(time) %>%
			arrange(date) %>%
			mutate(water_cumsum = cumsum(amount))
		
	county <- DBI::dbGetQuery(con, paste0("SELECT namelsad FROM grain.ca_counties WHERE ST_Contains(geom, ST_GeomFromText('POINT(", long, " ", lat, ")',4326));"))$namelsad
	plot_title <- paste0(c(""),
											 substr(as.character(min(total_water[total_water$time == "present", "date"]$date)), 6,10), " to ",
											 substr(as.character(max(total_water[total_water$time == "present", "date"]$date)), 6,10), " (", county, "; ", as.character(round(lat, 2)), ", ", as.character(round(long, 2)), ")")

	twd <- total_water %>%
					mutate(plot_group = interaction(measurement, quality))

	named_labels = c(ppt.prism = "Current Precipitation", irr.irrigation = "Irrigation Water", ppt.forecast = "Forecast Precipitation", ppt.historical = "Historical Precipitation")
	named_colors = c(ppt.prism = "darkblue", irr.irrigation = "palegreen", ppt.forecast = "gold", ppt.historical = "lightblue")
	named_lines = c(ppt.prism = "solid", irr.irrigation = "solid", ppt.forecast = "solid", ppt.historical = "dash")

	yaxis_label <- "Cumulative Precipitation (in)"

	if(nrow(irrigation) > 0) {
		yaxis_label <- "Cumulative Total Water (in)"
		named_labels['ppt.prism'] <- "Current Total Water"
	}

	fig <- plotly::plot_ly(
		hoverinfo = 'text', type = 'scatter',
		colors = named_colors, linetypes = named_lines
	)

	# build a list of water lines
	# irrigation_lines <- apply(as.data.frame(twd %>% filter(measurement == "irr")), 1, function(row) {
	# 	list(
	# 		type = "line",
	# 		line = list(color = "red", width = 3),
	# 		xref = "x", yref = "y",
	# 		x0 = row['date'],
	# 		x1 = as.Date(row['date']) + 1,
	# 		y0 = as.numeric(row['water_cumsum']) - as.numeric(row['amount']),
	# 		y1 = as.numeric(row['water_cumsum'])
	# 	)
	# })

	# add the latest precip cumsum point as a forecast point so the lines match up
	if(twd %>% filter(quality == 'forecast') %>% nrow() > 0) {
		twd <- rbind(twd,
								twd %>%
								filter(measurement == 'ppt', time == 'present', quality != 'forecast') %>%
								arrange(date) %>%
								tail(1) %>%
								mutate(quality = 'forecast', plot_group = 'ppt.forecast')
							)
		}

	twd <- twd %>%
			arrange(date) %>%
			mutate(label = named_labels[as.character(plot_group)])


	irr_lines <- twd %>%
		filter(measurement == "irr") %>%
		mutate(marker = water_cumsum - amount, enddate = date + 1)

	# fixes the rainy day water bug by plotting the endpoint of the irrigation lines
	# using the cumsum from the following day, renamed endpoint
	irr_lines$endpoint <-
		(twd %>% filter(plot_group == 'ppt.prism', date %in% irr_lines$enddate))$water_cumsum

	fig <- fig %>%
	  plotly::add_trace(data = twd %>%
	                      filter(measurement != "irr"),
		x = ~date, y = ~water_cumsum, type = 'scatter', mode = "lines",
		color = ~plot_group, linetype = ~plot_group, name = ~label,
		text = ~paste(label, "<br>", "Date: ", date, "<br>", "Inches: ", round(water_cumsum, 1))
		)

	fig <- fig %>% plotly::add_segments(data = irr_lines,
		x = ~date, y = ~marker, xend = ~enddate, yend = ~endpoint,
		color = ~plot_group, name = ~label, line = list(width = 3),
		text = ~paste(label, "<br>", "Date: ", date, "<br>", "Inches: ", round(amount, 1)) )

	tick_font = list(size = 14)
	title_font = list(size = 14)

	fig <- fig %>% plotly::layout(title = plot_title, font = list(size = 11),
												xaxis = list(title = F, tickfont = tick_font, titlefont = title_font),
												yaxis = list(title = yaxis_label, tickfont = tick_font, titlefont = title_font),
												showlegend = T, legend = list(orientation = 'h', y = -0.25))
												# shapes = irrigation_lines)

	return(fig)
}
