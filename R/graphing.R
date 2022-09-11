water_lines <- data.frame(
	var = c("ppt.prism", "irr.irrigation", "ppt.forecast", "ppt.historical"),
	label = c("Current Precipitation", "Irrigation Water", "Forecast Precipitation", "Historical Precipitation"),
	color = c("darkblue", "palegreen", "gold", "lightblue"),
	style = c("1111", "solid", "solid", "dash"),
	check = c("check_current_ppt", "check_irrigation_water", "check_forecast_ppt", "check_hist_ppt"),
	render = c(T, T, T, T),
	stringsAsFactors=FALSE
)

rownames(water_lines) <- water_lines$var

nuptake_lines <- data.frame(
	var = c("forecast", "nuptake_perc.historical", "nuptake_perc.present", "rel_precip_cumsum.historical", "rel_precip_cumsum.present"),
	label = c("Forecast", "N uptake (10-yr avg)", "N uptake (current season)", "Precipitation (10-yr avg)", "Precipitation (current season)"),
	color = c("gold", "orangered", "orangered", "dodgerblue", "dodgerblue"),
	style = c("1111", "dash", "solid", "dash", "solid"),
	alpha = c(1, .4, 1, .4, 1),
	check = c("check_forecast", "check_n_uptake_10", "check_n_uptake_current", "check_ppt_10", "check_ppt_current"),
	render = c(T, F, T, F, T),

	stringsAsFactors=FALSE
)

rownames(nuptake_lines) <- nuptake_lines$var


gdd_lines <- data.frame(
  var = c("forecast", "gdd_cumsum.historical", "gdd_cumsum.present"),
  label = c("Forecast", "Cumulative GDD (10-yr avg)", "Cumulative GDD (current season)"),
  color = c("gold", "mediumorchid", "mediumorchid"),
  style = c("1111", "dash", "solid"),
  alpha = c(1, .4, 1),
  check = c("check_forecast", "check_gdd_10", "check_gdd_current"),
  render = c(T, F, T),
  
  stringsAsFactors=FALSE
)

rownames(gdd_lines) <- gdd_lines$var

create_checkboxes_from_data <- function(specs, data_raw, type, nuptake_mod) {

	if(nuptake_mod == F) {
		data <- data_raw %>%
			filter(measurement == "rel_precip_cumsum" | measurement == "nuptake_perc") %>%
			mutate(wcol = interaction(measurement, time)) %>%
			filter(wcol != "nuptake_perc.historical")
	} else {
		data <- data_raw
	}


	available_data <- c(unique(as.character(interaction(data$measurement, data[[type]]))), unique(data$quality))
	widgets <- renderUI({
		checkbox_list <- apply(specs, 1, function(row) {
			widget <-
				tags$div(class = "legend-boxes",
					tags$div(
						checkboxInput(row['check'],
							HTML(
								paste(tags$span(row['label'], class = ifelse(row['var'] %in% available_data, "", "greyed")),
									tags$div(class = paste(row['check'], "legend-box", collapse = " "), HTML("&nbsp;"))
								, collapse = " ")),
							value =
								ifelse( row['var'] == "nuptake_perc.historical" && max((data %>% filter(measurement == "nuptake_perc", time == "present"))$amount, na.rm = T) < 5,
								T, row['render'] == "TRUE" && row['var'] %in% available_data))
					)
				)
			if(row['var'] %in% available_data) {
				widget
			} else {
				shinyjs::disabled(widget)
			}
	 })
	 do.call(tagList, checkbox_list)
 })
}

checkbox_filter_nup <- function(data, input) {
	colname <- 'time'
	def_table <- nuptake_lines
	query <-
		paste(unlist(apply(def_table, 1, function(row) {
				if(input[[row['check']]] == "TRUE") {
						paste0(
							"(measurement == '",
							unlist(strsplit(row['var'], ".", fixed = T))[1],
							"' & ", colname,  " == '",
							unlist(strsplit(row['var'], ".", fixed = T))[2],
							"')"
						)
				}
		})), collapse = '|')
	data %>% filter(eval(parse(text = query)))
}

checkbox_filter_water <- function(data, input) {
	colname <- 'quality'
	def_table <- water_lines
	query <-
		paste(unlist(apply(def_table, 1, function(row) {
				if(input[[row['check']]] == "TRUE") {
						paste0(
							"(measurement == '",
							unlist(strsplit(row['var'], ".", fixed = T))[1],
							"' & ", colname,  " == '",
							unlist(strsplit(row['var'], ".", fixed = T))[2],
							"')"
						)
				}
		})), collapse = '|')
	data %>% filter(eval(parse(text = query)))
}
