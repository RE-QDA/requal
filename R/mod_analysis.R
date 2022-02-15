#' analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    selectizeInput(ns("code_filter"),
                   label = "Filter by code",
                   choices = ""),
                   
    htmlOutput(ns("segments"))
 
  )
}
    
#' analysis Server Functions
#'
#' @noRd 
mod_analysis_server <- function(id, project_observer, codebook, documents){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    observe({ 
      print(project_observer())
      print(codebook())
      print(documents())
      })
    
  })
}

