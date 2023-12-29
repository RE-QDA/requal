#' data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_data_ui <- function(id){
  ns <- NS(id)

  tabsetPanel(
      type = "tabs", id = ns("data_tabset"),
      tabPanel("Documents", 
               id = ns("data_documents"), 
               value = "data_documents", 
               mod_doc_manager_ui(ns("doc_manager_ui_1"))
  )
  
  )
}
    
#' data Server Functions
#'
#' @noRd 
mod_data_server <- function(id, glob){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

 # output: glob$documents
  mod_doc_manager_server("doc_manager_ui_1", glob)
 
  })
}

    

