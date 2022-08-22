#' browser_navigation
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

browser_navigation <- function(input, output, session){
	values <- reactiveValues(
		# variable to keep track of whether or not the tab switching is manual (by the
		# user) or automatic (restoring the app's state on initialization or prev/next buttons)
		autoNavigating = 0
	)

	runjs(autoscroll_to_anchor)

	# restore the Shiny app's state based on the URL
	restore <- function(qs) {
		print("restoring")
		data <- parseQueryString(qs)

		if (!is.null(data[['page']])) {
			# we're about to change tabs programatically, so don't trigger the
			# navigation function
			values$autoNavigating <<- values$autoNavigating + 1

			# change to the correct tab
			updateTabItems(session, "tabs", data[['page']])
		}
	}

	# when the user changes tabs, save the state in the URL
	observeEvent(input$tabs, {

		print(input$tabs)
		if (values$autoNavigating > 0) {
			values$autoNavigating <<- values$autoNavigating - 1
		}

		shinyjs::js$updateHistory(page = input$tabs)
		waiter::waiter_hide()

	})

	# when the user clicks prev/next buttons in the browser, restore the state
	observeEvent(input$navigatedTo, {

		restore(input$navigatedTo)

	})
}
