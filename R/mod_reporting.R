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
    
    "Stats to be used in reports."
 
  )
}
    
#' reporting Server Functions
#'
#' @noRd 
mod_reporting_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_reporting_ui("reporting_ui_1")
    
## To be copied in the server
# mod_reporting_server("reporting_ui_1")
