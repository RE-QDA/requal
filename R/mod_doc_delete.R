#' doc_delete UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_doc_delete_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    selectizeInput(ns("doc_del"), label = "Select documents to remove", choices = NULL),
    actionButton(ns("doc_remove"), label = "Remove")
    
  )
}
    
#' doc_delete Server Functions
#'
#' @noRd 
mod_doc_delete_server <- function(id, connection, project){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    documents_list <- reactive({list_db_documents(connection, project)})
    
    print(documents_list())
    
    observe({
    updateSelectizeInput(session = session,
                         "doc_del",
                         label = "Select documents to remove",
                         choices = documents_list())
    })
 
  })
}
    
## To be copied in the UI
# mod_doc_delete_ui("doc_delete_ui_1")
    
## To be copied in the server
# mod_doc_delete_server("doc_delete_ui_1")
