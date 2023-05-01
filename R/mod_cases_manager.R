#' cases_manager UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cases_manager_ui <- function(id){
  ns <- NS(id)
  tagList(
 "cases"
  )
}
    
#' cases_manager Server Functions
#'
#' @noRd 
mod_cases_manager_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_cases_manager_ui("cases_manager_1")
    
## To be copied in the server
# mod_cases_manager_server("cases_manager_1")
