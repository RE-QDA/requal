#' codebook UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_codebook_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    "Management of codes. Including creation, deletion, merges."
 
  )
}
    
#' codebook Server Functions
#'
#' @noRd 
mod_codebook_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_codebook_ui("codebook_ui_1")
    
## To be copied in the server
# mod_codebook_server("codebook_ui_1")
