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
             br(),
             textOutput(ns("project_name")),
            
             uiOutput(ns("project_active")),
             

      tableOutput(ns("doc_list_table"))
      
    ),
      
      column(width = 6,
             
            box(title = "Document input",
                collapsible = TRUE,
                width = NULL,
               
               
                textInput(ns("doc_name"), label = "Document name", placeholder = "Short name") %>% 
                  tagAppendAttributes(class = "required"),
               textAreaInput(ns("doc_description"), label = "Document description", placeholder = "Description"),
                textAreaInput(ns("doc_text"), label = "Document content", placeholder = "Paste the new document content here") %>% 
                 tagAppendAttributes(class = "required"),
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
      
      output$project_name <- renderText({
        paste("Active project:", names(project()$active_project))
      })
      
      doc_startup <- list_db_documents(project_db = project()$project_db,
                                 active_project = project()$active_project)
      doc_list(doc_startup)
      
      output$doc_list_table <- make_doc_table(
        project()$project_db,
        project()$active_project,
        doc_list()
      )
      
      updateSelectInput(session = session,
                        "doc_delete_list", 
                        choices = doc_list())
      
    })
    

    ## render existing documents ----

output$doc_list_table <- make_doc_table(
  project()$project_db,
  project()$active_project,
  doc_list()
)






 # observe documents actions ----
    
    # document input ----
    

observeEvent(input$doc_add, {

  req(input$doc_name, input$doc_text)
  
  add_input_document(connection = project()$project_db,
                     project = project()$active_project,
                     doc_name = input$doc_name,
                     doc_text = input$doc_text,
                     doc_description = input$doc_description
  )
  
  output$doc_list_table <- make_doc_table(
    project()$project_db,
    project()$active_project,
    doc_list()
    )
  
    doc_list(list_db_documents(project_db = project()$project_db,
                               active_project = project()$active_project))
    
    updateSelectInput(session = session,
                      "doc_delete_list", 
                      choices = doc_list())

})

    # document removal ----
    observeEvent(input$doc_remove, {
      
 
      delete_db_documents(project()$project_db, 
                                      project()$active_project, 
                                      input$doc_delete_list)
      
      # update reactive value containing project documents
      doc_list(list_db_documents(project_db = project()$project_db,
                                 active_project = project()$active_project))
      
      output$doc_list_table <- make_doc_table(
        project()$project_db,
        project()$active_project,
        doc_list()
      )
      
      updateSelectInput(session = session,
                        "doc_delete_list",
                        choices = doc_list())
      
    })
    

    

    
    return(reactive(doc_list()))
    
  })
}

