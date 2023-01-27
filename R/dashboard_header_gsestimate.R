dashboard_header_ui <- function(id, label = "header") {
	ns <- NS(id)
	shinydashboard::dashboardHeader(title = "The Crop Growth Stage and Forecasting Web-Tool", titleWidth = 600,
									tags$li(actionLink("comments",
																		 label = "",
																		 icon = icon("comments"),
																		 onclick ="window.open('https://forms.gle/SjL1kSNW9M4K5s2s9', '_blank')"),
													class = "dropdown"),
									tags$li(actionLink(ns("open_info_modal"),
																		 label = "",
																		 icon = icon("info-circle")),
													class = "dropdown"))
}

dashboard_header_server <- function(id, con){
	moduleServer(
		id,
		function(input, output, session){
			observeEvent(input$open_info_modal, {
			  max_prism_date <- DBI::dbGetQuery(con, "SELECT DISTINCT(date) FROM grain.prism WHERE quality != 'forecast' ORDER BY date DESC LIMIT 1;")
				showModal(
					modalDialog(title = "About The Crop Growth Stage and Forecasting Web-Tool",
											HTML("The Crop Growth Stage and Forecasting Web-Tool uses present and historical precipitation and temperature data from PRISM Climate Group."),
											HTML(paste0("PRISM Climate Group, Oregon State University, http://prism.oregonstate.edu, created ", format(max_prism_date$date, format = "%d %b %Y"), ". ", "Forecast data from MET Norway.")),
											#HTML("The Nitrogen Fertilizer Management Tool for California Wheat (Beta) was first published in April 2020. It was last updated on: 1/27/2021."),
											br(), br(),
											HTML("We would like to thank CDFA-Fertilizer Research and Education Program, NRCS-Conservation Innovation Grant, California Wheat Commission, California Crop Improvement Association, and the UC Davis, Library for their support of this project."),
											br(), br(),
											HTML("To cite this page, please use: Nelsen, T., Rosa, G., Merz, J. & Lundy, M. (2022, August 17). <i>The Crop Growth Stage and Forecasting Web-Tool.</i> Retrieved from https://smallgrains.ucanr.edu/General_Production_Guidelines/Crop_Growth_Stage_and_Forecasting_Tool/"),
											br(), br(),
											HTML("For questions about this content, please contact Mark Lundy, Assistant UC Cooperative Extension Specialist in Grain Cropping Systems (<a href = 'melundy@ucdavis.edu' target='_blank'>melundy@ucdavis.edu</a>) or one of the UCCE contacts listed at <a href = 'http://smallgrains.ucanr.edu/contact_us/' target='_blank'>http://smallgrains.ucanr.edu/contact_us/</a>")
					)
				)
			})
		}
	)
}
