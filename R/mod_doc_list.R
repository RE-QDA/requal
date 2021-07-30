#' doc_list UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_doc_list_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    textOutput(ns("doc_list"))
 
  )
}
    
#' doc_list Server Functions
#'
#' @noRd 
mod_doc_list_server <- function(id, connection, project){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
      
    output$doc_list <- renderText({list_db_documents(connection, project)})
    
    

 
  })
}
    

