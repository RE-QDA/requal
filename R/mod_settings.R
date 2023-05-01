#' settings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_settings_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    "Project wide settings"
    
  )
}
    
#' settings Server Functions
#'
#' @noRd 
mod_settings_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_settings_ui("settings_ui_1")
    
## To be copied in the server
# mod_settings_server("settings_ui_1")
