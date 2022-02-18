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
    tabsetPanel(type = "tabs", id = ns("reporting_tabset"),
                tabPanel("Instructions", 
                         id = ns("instructions"),
                         value = "instructions",
                         textOutput(ns("report_instructions"))),
                tabPanel("Logs", 
                         id = ns("logs"),
                         value = "logs",
                         actionButton(ns("logs_refresh"),
                                      label = "",
                                      icon = icon("refresh")) %>% 
                           tagAppendAttributes(style = "float:right;"),
                         dataTableOutput(ns("report_logs")))
    )
 
  )
}
    
#' reporting Server Functions
#'
#' @noRd 
mod_reporting_server <- function(id, project){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # instructions ------------
    output$report_instructions <- renderText(
      "Instructions for using this module..."
    )
    
    logs_df <- eventReactive(input$reporting_tabset == "logs" | input$logs_refresh, {
     
      if(isTruthy(project()$active_project)){
        load_logs_for_reporting(project()$project_db,
                                project()$active_project) %>% 
          dplyr::arrange(dplyr::desc(created_at))
      }else{""}
    })
  
    # logs ------------
    output$report_logs <- renderDataTable({
      req(logs_df())
      logs_df() %>% 
        dplyr::mutate(payload = purrr::map_chr(.data$payload, parse_payload_json))
    })
    
})}
    
## To be copied in the UI
# mod_reporting_ui("reporting_ui_1")
    
## To be copied in the server
# mod_reporting_server("reporting_ui_1")
