#' attributes_manager UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_attributes_manager_ui <- function(id){
  ns <- NS(id)
  tagList(
 "attributes_manager"
  )
}
    
#' attributes_manager Server Functions
#'
#' @noRd 
mod_attributes_manager_server <- function(id, glob){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
