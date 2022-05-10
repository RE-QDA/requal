#' text_stats UI Function
#'
#' @description A shiny Module that calculates and displays corpus statistics.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_text_stats_ui <- function(id){
  ns <- NS(id)
  tagList(
 
      
      
      
  )
}
    
#' text_stats Server Functions
#'
#' @noRd 
mod_text_stats_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_text_stats_ui("text_stats_ui_1")
    
## To be copied in the server
# mod_text_stats_server("text_stats_ui_1")
