clear_irrigation_memory <- function() {
	data.frame(
		inputName = character(0),
		dateName = character(0),
		date = character(0),
		amount = numeric(0)
	)
}

# memorizes irrigation widgets to a data frame
memoize_irrigation <- function(input, memory) {

	irrigation_memory <- memory

	# only rebuild memory if we are increasing the number of slots
	if(req(input$numIrr) > nrow(irrigation_memory)) {
		# nuke existing data
		irrigation_memory <- clear_irrigation_memory()

		# save key:val to memoization df
		lapply(1:input$numIrr, function(i) {
			inputName <- paste("Irrigation", i, sep = "")
			dateName <- paste("Date", i, sep = "")

			value <- ifelse(length(input[[inputName]]) > 0, input[[inputName]], 0.0)
			date <- as.Date(ifelse(length(input[[dateName]]) > 0, input[[dateName]], input$daterange[1]), origin = "1970-01-01")

			new_row <- data.frame(inputName = inputName, dateName = dateName, date = date, amount = value)
			irrigation_memory <<- rbind(irrigation_memory, new_row)
		})
	}

	return(irrigation_memory)
}

# uses a memoization data frame to renderUI for irrigation widgets
build_memoized_irrigation_amounts <- function(number, memory, session) {
	renderUI({
		input_list <- lapply(1:number, function(i) {

			# for each dynamically generated input, give a different name
			inputName <- paste("Irrigation", i, sep = "")

			# grab value from data frame, or 0 as the default
			value <- memory[memory$inputName == inputName,]$amount
			inputValue <- ifelse(length(value) > 0, value, 0.0)

			if (i == 1){
				numericInput(session$ns(inputName), "Amount (in)", inputValue, min = 0, max = 20)
			} else {
				numericInput(session$ns(inputName), "", inputValue, min = 0, max = 20)
			}

		})
		do.call(tagList, input_list)
	})
}

build_memoized_irrigation_dates <- function(numIrr, dateMin, dateMax, memory, session) {

	renderUI({
		date_list <- lapply(1:numIrr, function(i) {

			# for each dynamically generated input, give a different name
			dateName <- paste("Date", i, sep = "")
			dateLabel <- ""

			# grab value from data frame
			date <- memory[memory$dateName == dateName,]$date
			dateValue <- ifelse(length(date) > 0, as.Date(date, origin = "1970-01-01"), as.Date(minDate, origin = "1970-01-01"))

			if(as.Date(dateMin, origin = "1970-01-01") > dateValue) {
				dateMin <- dateValue
			} else if(as.Date(dateMax, origin = "1970-01-01") < dateValue) {
				dateMax <- dateValue
			}

			isolate(if(i == 1){
				dateLabel <- "Date"
			})

			dateInput(session$ns(dateName), dateLabel,
					value = as.Date(dateValue, origin = "1970-01-01"),
					min = as.Date(dateMin, origin = "1970-01-01"),
					max = as.Date(dateMax, origin = "1970-01-01"),
					format = 'mm/dd/yyyy')

		})
		do.call(tagList, date_list)
	})
}
