#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @importFrom shinyjs useShinyjs extendShinyjs
#' @importFrom waiter waiter_show_on_load
#' @noRd

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shinydashboard::dashboardPage(
      dashboard_header_ui("header"),
      shinydashboard::dashboardSidebar(disable = TRUE,
                                       shinydashboard::sidebarMenu(id = "tabs",
                                                                   shinydashboard::menuItem("location", tabName = "location"),
                                                                   shinydashboard::menuItem("initial_outputs", tabName = "initial_outputs")
                       )
      ),
      shinydashboard::dashboardBody(
        shinyjs::useShinyjs(),
        waiter::use_waiter(), # include dependencies
        waiter::waiter_show_on_load(html = waiter::spin_loaders(8, color = '#005fae', style="width: 200px;
    height:200px;"),color = waiter::transparent(.5)),
        shinyjs::extendShinyjs("www/app-shinyjs.js", functions = c("updateHistory")),
        tags$head(
          tags$style("#shiny-modal img { max-width: 100%; }"),
          tags$link(rel = "stylesheet", type = "text/css", href = "css/slider.css"),
          tags$link(rel = "stylesheet", type = "text/css", href = "css/graph_legends.css"),
          tags$link(rel = "stylesheet", type = "text/css", href = "css/color-slider.css"),
          tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css"),
          tags$script(type="text/javascript", async = T, src=paste0("https://www.googletagmanager.com/gtag/js?id=", Sys.getenv("ANALYTICS_KEY"))),
          tags$script(
            paste0("
					 window.dataLayer = window.dataLayer || [];
						function gtag(){dataLayer.push(arguments);}
						gtag('js', new Date());
						gtag('config', '", Sys.getenv("ANALYTICS_KEY"), "');")
          ),
          tags$style(type="text/css", "#inline label{ display: table-cell; text-align: center; vertical-align: middle; padding-right: 20px;}
                #inline .form-group { display: table-row;}")
        ),
        tags$div(id = 'anchor'),
        shinydashboard::tabItems(
          location_page_ui("location", "Location"),
          initial_outputs_gs_ui("initial_outputs", "IO")
        )
      )
    )
    
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "gcsxgsestimate"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
