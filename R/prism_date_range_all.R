prism_date_range_all <- function(con, lat, long, from_date, to_date){
	#source("../functions/gdd_to_nuptake.R")
	daily_data <- DBI::dbGetQuery(con, paste0(
		"WITH point as (
			SELECT (ST_WorldToRasterCoord(rast,", long, ",", lat, ")).* from prism limit 1)
			SELECT date,
			EXTRACT(MONTH FROM date) AS month,
			EXTRACT(DAY FROM date) AS day,
			measurement,
			quality,
			ST_Value(rast, point.columnx, point.rowy) AS amount
			FROM
			prism, point
			WHERE date BETWEEN CAST('", from_date, "' AS DATE) AND CAST('", to_date, "' AS DATE);"))

	daily_output <- daily_data %>%
	  tidyr::spread(key = measurement, value = amount) %>%
		arrange(date) %>%
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
					 						 			 ))),
					 precip_cumsum = cumsum(ppt),
					 gdd_cumsum = cumsum(gdd),
					 nuptake_perc = gdd_to_nuptake(gdd_cumsum))

	return(daily_output)

}






