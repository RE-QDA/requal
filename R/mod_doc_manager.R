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
  tagList(
    
    fluidRow(
      column(width = 7,
             
             uiOutput(ns("project_active")),
             uiOutput(ns(
               "project_manager"
             ))),
      textOutput(ns("doc_list")),
      
      column(
        width = 5,
        
        actionButton(ns("project_load_menu"), label = "Load existing project"),
        
        actionButton(ns("project_create_menu"), label = "Create new project"),
        
        uiOutput(ns("project_action"))
      )
    )
    
  )
}

#' doc_manager Server Functions
#'
#' @noRd 
mod_doc_manager_server <- function(id, project){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    documents <- reactiveValues()
    
    # observe({print(project$active_project)})
    # output$doc_list <- renderText({
    #   req(project$active_project)
    #   print(project$active_project)
    #   list_db_documents(project_db = project$project_db,
    #                     active_project = project$active_project)
    #   })
    # 
    
    
    
    # display active project details
    # 
    # 
    # observe({
    #   output$project_manager <-  renderUI({
    #     if (!is.null(project$active_project) & !is.null(project$project_db) ) {
    #       tagList(tabsetPanel(
    #         tabPanel("Manage documents",
    #                  fluidRow(
    #                    column(
    #                      width = 6,
    #                      mod_doc_manager_ui(ns("doc_manager_ui_1")),
    #                      mod_doc_delete_ui(ns("doc_delete_ui_1"))
    #                    ),
    #                    column(width = 6,
    #                           mod_doc_list_ui(ns(
    #                             "doc_list_ui_1"
    #                           )))
    #                  )),
    #         tabPanel("Project information"),
    #         tabPanel("Settings")
    #       ))
    #       
    #       
    #     }
    #   })
    # })
    # 
    # 
    # 
    # # observe documents actions
    # 
    # observeEvent(input[[paste("doc_manager_ui_1", "doc_add", sep = "-")]], {
    #   
    #   # this module processes document input
    #   mod_doc_manager_server("doc_manager_ui_1",
    #                          connection = project$project_db,
    #                          project = project$active_project)
    #   # this module displays active documents
    #   mod_doc_list_server("doc_list_ui_1",
    #                       connection = project$project_db,
    #                       project = project$active_project)
    #   # the delete module is called to update delete UI by adding new document
    #   mod_doc_delete_server("doc_delete_ui_1",
    #                         connection = project$project_db,
    #                         project = project$active_project)
    #   # update reactive value containing project documents
    #   documents$doc_list<- documents$doc_list+1
    #   
    # })
    # 
    # observeEvent(input[[paste("doc_delete_ui_1", "doc_remove", sep = "-")]], {
    #   # input$doc_remove defined in doc_delete module
    #   after_deletion <- mod_doc_delete_server("doc_delete_ui_1",
    #                                           connection = project$project_db,
    #                                           project = project$active_project)
    #   mod_doc_list_server("doc_list_ui_1",
    #                       connection = project$project_db,
    #                       project = project$active_project)
    #   # update reactive value containing project documents
    #   documents$doc_list<- documents$doc_list+1
    #   
    # })
    # 
    # ## list documents initially
    # observe({
    #   if (!is.null( project$active_project ) ) {
    #     mod_doc_list_server("doc_list_ui_1",
    #                         connection = project$project_db,
    #                         project = project$active_project)
    #     mod_doc_delete_server("doc_delete_ui_1",
    #                           connection = project$project_db,
    #                           project = project$active_project)
    #   }
    # })
    # 
    # documents$doc_list <- list_db_documents(project_db = project$project_db, 
    #                            active_project = project$active_project)
    
    return(documents)
    
  })
}

