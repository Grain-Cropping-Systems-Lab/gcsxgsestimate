#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @noRd

app_server <- function(input, output, session) {
  # Your application server logic
  
  db_creds <- httr::parse_url(golem::get_golem_options("DATABASE_URL"))

  api_key <- golem::get_golem_options("MAPS_API_KEY")
  
  start_time <- reactiveValues(time = Sys.time())
  
  observe({
    # Re-execute this reactive expression after 50000 milliseconds
    invalidateLater(50000)
    
    if((Sys.time() - start_time$time) < 120){
      print(Sys.time() - start_time$time)
      print("keep alive") 
    } else {
      stopApp()
    }
  })
  
  observeEvent(reactiveValuesToList(input), ignoreInit = TRUE,{
    start_time$time <- Sys.time()
    print("reset timer")
  })
  
  con <- DBI::dbConnect(
      RPostgres::Postgres(),
      dbname = db_creds$path,
      host = db_creds$hostname,
      port = db_creds$port,
      user = db_creds$username,
      password = db_creds$password,
      sslmode = "require",
      options = "-c search_path=grain,public,heroku_ext"
    )

  # switch off s2 geometries 
  sf::sf_use_s2(FALSE)
  
  browser_navigation(input, output, session)
  
  shinyjs::runjs(slider_js)
  
  # scroll to to whenever tabs are changed
  observeEvent(input$tabs, {
    shinyjs::runjs(autoscroll_to_anchor)
  })
  
  
  dashboard_header_server("header")
  
  location_outputs <- location_page_server("location",
                                           parent = session,
                                           con = con,
                                           api_key = api_key)
  initial_outputs <- initial_outputs_gs_server("initial_outputs",
                                               parent = session,
                                               map_outputs = location_outputs$map_outputs,
                                               growth_stage_option = location_outputs$growth_stage_option,
                                               prelim_weather_data = location_outputs$prelim_weather_data,
                                               irrigation_data = location_outputs$irrigation,
                                               con = con)
}
