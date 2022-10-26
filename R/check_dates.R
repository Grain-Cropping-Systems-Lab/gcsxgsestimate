check_dates <- function(daterange, irrigation_input, irrigation, region, max_prism_date){
	
	if (is.na(region)){
		region <- "undefined"
	}
	
	if (is.na(daterange[1]) | is.na(daterange[2])){
		showNotification("Error: the date range is not complete.")
		return(FALSE)
	}
	
	if (daterange[1] > max_prism_date$date){
		showNotification("Error: All data is forecast data - choose date range that includes past days.")
		return(FALSE)
	}
	
	if((irrigation_input == 1) & (any(irrigation$date == as.Date("1970-01-01")))){
		showNotification("Error: an irrigation is missing a date.")
		return(FALSE)
	}
	
	if (daterange[1] > daterange[2]){
		showNotification("Error: the entered start date occurs after the end date.")
		return(FALSE)
	}

	if ((irrigation_input == 1) & (any(irrigation$date > daterange[2]) | any(irrigation$date < daterange[1]))){
		showNotification("Error: the entered irrigation date is not in the selected date range!")
		return(FALSE)
	}

	if (region == "IR"){
		if (daterange[2] - daterange[1] > 365) {
			showNotification("Error: the entered date range is longer than the wheat growing season!")
			return(FALSE)
		}
		
		if(!any(c(10, 11, 12, 1, 2, 3, 4, 5) %in% lubridate::month(daterange[1]))){
			showNotification("Error: the start date is outside of a normal wheat growing season.")
			return(FALSE)
		}
		
	#	if(lubridate::month(daterange[2]) == 10 & lubridate::day(daterange[2]) > 15){
	#		showNotification("Error: the end date is outside of a normal wheat growing season.")
	#		return(FALSE)
	#	}
	
	} else {
		
		if (daterange[2] - daterange[1] > 275) {
			showNotification("Error: the entered date range is longer than the wheat growing season!")
			return(FALSE)
		}
		
		if(!any(c(10, 11, 12, 1, 2) %in% lubridate::month(daterange[1]))){
			showNotification("Error: the start date is outside of a normal wheat growing season.")
			return(FALSE)
		}
		
		if(lubridate::month(daterange[1]) == 2 & lubridate::day(daterange[1]) > 15){
			showNotification("Error: the start date is outside of a normal wheat growing season.")
			return(FALSE)
		}
		
	#	if(lubridate::month(daterange[2]) >= 7){
	#		showNotification("Error: the end date is outside of a normal wheat growing season.")
	#		return(FALSE)
	#	}
		
	}
	
	return(TRUE)
}