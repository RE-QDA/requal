#' doc_manager UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_doc_manager_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    tabsetPanel(
      tabPanel("Project information"),
      tabPanel("Manage documents",
    textAreaInput(ns("doc"), label = NULL, placeholder = "Paste a new document content here"),
    actionButton(ns("doc_add"), label = "Add document")
    ),
    tabPanel("Settings")
    )
  )
}
    
#' doc_manager Server Functions
#'
#' @noRd 
mod_doc_manager_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_doc_manager_ui("doc_manager_ui_1")
    
## To be copied in the server
# mod_doc_manager_server("doc_manager_ui_1")
