prism_historical_season_all <- function(con, lat, long, current_start_date = "2019-10-01", end_date){
	#source("../functions/gdd_to_nuptake.R")
	# all months have the 1st but not all months have a 29th and 30th
	months <- lubridate::month(seq(as.Date(current_start_date) - lubridate::day(as.Date(current_start_date)) + 1,
																 as.Date(end_date) - lubridate::day(as.Date(end_date)) + 1, by = "month"))
	days_df <- data.frame(pseudo_date = seq(as.Date(current_start_date), as.Date(end_date), by = "day")) %>%
		mutate(month = lubridate::month(pseudo_date),
					 day = lubridate::day(pseudo_date))

	historical <- data.frame(month = numeric(), day = numeric(), measurement = character(), amount = numeric())

	for (i in 1:length(months)){
		#print(i)
		#print(months[i])
		historical <- rbind(historical, DBI::dbGetQuery(con, paste0(
			"WITH point as (
			SELECT (ST_WorldToRasterCoord(rast,", long, ", ", lat, ")).* from prism limit 1
		)
			SELECT EXTRACT(MONTH FROM date) AS month,
			EXTRACT(DAY FROM date) AS day,
			measurement,
			AVG(ST_Value(rast, point.columnx, point.rowy)) AS amount
			FROM
			prism, point
			WHERE NOT measurement = 'tmean'
			AND date BETWEEN (CURRENT_DATE - INTERVAL '11 year') AND (CURRENT_DATE - INTERVAL '1 year')
			AND EXTRACT(MONTH FROM date) IN (", months[i], ")
			GROUP BY
			measurement, month, day
			ORDER BY
			month, day, measurement;")))
		#print(dim(historical))
		incProgress(100/(length(months) + 2))
	}

	historical_output <- historical %>%
		tidyr::spread(key = measurement, value = amount) %>%
		mutate(tmax = (tmax* 9/5) + 32,
					 tmin = (tmin* 9/5) + 32,
					 gdd = ifelse(tmax<45, 0,
					 						 ifelse(tmin > 86, tmax - tmin,
					 						 			 ifelse(tmax<86,ifelse(tmin<45,
					 						 			 											(6*(tmax-45)^2)/
					 						 			 												(tmax-
					 						 			 												 	tmin)/12,
					 						 			 											((tmax+
					 						 			 													tmin-2*45)*6/12)),
					 						 			 			 ((6*(tmax+tmin-2*45)/12)-
					 						 			 			  	((6*(tmax-86)^2)/
					 						 			 			  	 	(tmax-tmin))/12)
					 						 			 )))) %>%
		inner_join(days_df) %>%
		arrange(pseudo_date) %>%
		mutate(precip_cumsum = cumsum(ppt),
					 rel_precip_cumsum =  precip_cumsum /max(precip_cumsum)*100,
					 gdd_cumsum = cumsum(gdd),
					 nuptake_perc = gdd_to_nuptake(gdd_cumsum))

	return(historical_output)

}
