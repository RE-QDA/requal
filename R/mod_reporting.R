#' reporting UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_reporting_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    tabsetPanel(type = "tabs", id = "tabset",
                tabPanel("Instructions", textOutput(ns("report_instructions"))),
                tabPanel("Logs", dataTableOutput(ns("report_logs")))
    )
 
  )
}
    
#' reporting Server Functions
#'
#' @noRd 
mod_reporting_server <- function(id, project){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # instructions ------------
    output$report_instructions <- renderText(
      "Instructions for using this module..."
    )
    
    # logs ------------
    output$report_logs <- renderDataTable({
      load_logs_for_reporting(project()$project_db,
                              project()$active_project)
    })
 
  })
}
    
## To be copied in the UI
# mod_reporting_ui("reporting_ui_1")
    
## To be copied in the server
# mod_reporting_server("reporting_ui_1")
