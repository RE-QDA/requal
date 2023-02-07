#' use_manager UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_use_manager_ui <- function(id){
  ns <- NS(id)
  tagList(
 "hello"
  )
}
    
#' use_manager Server Functions
#'
#' @noRd 
mod_use_manager_server <- function(id, glob){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


 
  })
}
  