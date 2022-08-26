#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {
  # Your application server logic
  browser_navigation(input, output, session)
  
  shinyjs::runjs(slider_js)
  
  # scroll to to whenever tabs are changed
  observeEvent(input$tabs, {
    shinyjs::runjs(autoscroll_to_anchor)
  })
  
  
  dashboard_header_server("header")
  
  location_outputs <- location_page_server("location", parent = session)
  initial_outputs <- initial_outputs_gs_server("initial_outputs",
                                               parent = session,
                                               map_outputs = location_outputs$map_outputs,
                                               growth_stage_option = location_outputs$growth_stage_option,
                                               prelim_weather_data = location_outputs$prelim_weather_data,
                                               irrigation_data = location_outputs$irrigation)
}
