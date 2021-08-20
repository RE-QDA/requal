#' doc_delete UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_doc_delete_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
          uiOutput(ns("doc_del_container")),
          actionButton(ns("doc_remove"), label = "Remove"))
  
}

#' doc_delete Server Functions
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param connection Database connection object.
#' @param project Active project.
#' @noRd
mod_doc_delete_server <- function(id, connection, project) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
   
    output$doc_del_container <- renderUI({
      tagList(
        
      selectInput(ns("doc_del"),
                  label = "Select documents to remove",
                  choices = list_db_documents(connection, project),
                  multiple = TRUE,
                  selected = NULL)
      )
    })
   
    observeEvent(input$doc_remove, {
      
      delete_db_documents(connection, 
                          project, 
                          delete_doc_id = input$doc_del)
      
      output$doc_del_container <- renderUI({
        tagList(
          
          selectInput(ns("doc_del"),
                      label = "Select documents to remove",
                      choices = list_db_documents(connection, project),
                      multiple = TRUE,
                      selected = NULL)
        )
      })
    })
  })
}

## To be copied in the UI
# mod_doc_delete_ui("doc_delete_ui_1")

## To be copied in the server
# mod_doc_delete_server("doc_delete_ui_1")
