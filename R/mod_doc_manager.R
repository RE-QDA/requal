#' doc_manager UI Function
#'
#' @description This module accept document inputs and writes them to ReQual database.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_doc_manager_ui <- function(id){
  ns <- NS(id)

    fluidRow(
      column(width = 6,
             "dd",
            
             uiOutput(ns("project_active")),
             

      textOutput(ns("doc_list_text"))
      
    ),
      
      column(width = 6,
             
            box(title = "Document input",
                collapsible = TRUE,
                width = NULL,
               
               textInput(ns("doc_name"), label = "Document name", placeholder = "Short name"),
               textAreaInput(ns("doc_description"), label = "Document description", placeholder = "Description"),
               textAreaInput(ns("doc_text"), label = NULL, placeholder = "Paste the new document content here"),
               actionButton(ns("doc_add"), label = "Add document", class = "btn-success")
               
            ),
             
            box(title = "Delete documents",
                collapsible = TRUE,
                collapsed = TRUE,
                width = NULL,

        selectInput(ns("doc_delete_list"),
                    label = "Remove selected documents from project",
                    choices = "",
                    multiple = TRUE,
                    selected = NULL),
        
        actionButton(ns("doc_remove"), 
                     "Remove",
                     class = "btn-danger")
      
            )
      
    ))
    
  
}

#' doc_manager Server Functions
#'
#' @noRd 
mod_doc_manager_server <- function(id, project){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    doc_list <- reactiveVal()

    ## list documents initially ----
    observeEvent(project()$active_project, {
      
      doc_startup <- list_db_documents(project_db = project()$project_db,
                                 active_project = project()$active_project)
      doc_list(doc_startup)
    })
    

    ## render existing documents ----

output$doc_list_text <- renderText({

    req(project()$active_project)

    if (isTruthy(doc_list()))
    {
    
      doc_list()
    } else {
      
      "This project does not contain any documents yet."
      
    }
  })






 # observe documents actions ----
    
    # document input ----
    

observeEvent(input$doc_add, {
#
#   # this module processes document input
#   mod_doc_manager_server("doc_manager_ui_1",
#                          connection = project()$project_db,
#                          project = project()$active_project)
#   # this module displays active documents
#   mod_doc_list_server("doc_list_ui_1",
#                       connection = project()$project_db,
#                       project = project()$active_project)
#   # the delete module is called to update delete UI by adding new document
#   mod_doc_delete_server("doc_delete_ui_1",
#                         connection = project()$project_db,
#                         project = project()$active_project)
    # update reactive value containing project documents
    doc_list(list_db_documents(project_db = project()$project_db,
                               active_project = project()$active_project))

})

    # document removal ----
    observeEvent(input$doc_remove, {
      
      req(input$doc_delete_list)
      
      delete_db_documents(connection,
                          project,
                          delete_doc_id = input$doc_delete_list)
      
      # update reactive value containing project documents
      doc_list(list_db_documents(project_db = project()$project_db,
                                 active_project = project()$active_project))
      
    })
    


    return(reactive(doc_list()))
    
  })
}

