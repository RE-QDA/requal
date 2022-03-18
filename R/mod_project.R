#' project UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_project_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    uiOutput(ns(
      "project_manager"
    ))
    
    
  )
  
  
}

#' project Server Functions
#'
#' @noRd
mod_project_server <- function(id, project, user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    

 
      output$project_manager <-  renderUI({
        if (!is.null(project()$active_project) & !is.null(project()$project_db) ) {
          tagList(tabsetPanel(
            tabPanel("Manage documents",
             mod_doc_manager_ui(ns("doc_manager_ui_1"))
             )
            )#,
            # tabPanel("Project information"),
            # tabPanel("Settings")
          )
          
          
        } else {
          
          "No active project."
          
        }
    
    })
      # update documents from embedded module
      
    documents_from_manager <- mod_doc_manager_server("doc_manager_ui_1", project)
    

    
  return(reactive(documents_from_manager()))
  
  })
}
