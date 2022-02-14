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
  tagList(fluidRow(
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
  ))
  
  
}

#' project Server Functions
#'
#' @noRd
mod_project_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    








    output$doc_list <- renderText({
      req(active_project())
      list_db_documents(project_db = db_path(),
                                                     active_project = active_project())})

    # set up paths

    volumes <- c(Home = fs::path_home())















    # display active project details


    observe({
      output$project_manager <-  renderUI({
        if (!is.null(active_project()) & !is.null(db_path()) ) {
          tagList(tabsetPanel(
            tabPanel("Manage documents",
                     fluidRow(
                       column(
                         width = 6,
                         mod_doc_manager_ui(ns("doc_manager_ui_1")),
                         mod_doc_delete_ui(ns("doc_delete_ui_1"))
                       ),
                       column(width = 6,
                              mod_doc_list_ui(ns(
                                "doc_list_ui_1"
                              )))
                     )),
            tabPanel("Project information"),
            tabPanel("Settings")
          ))


        }
      })
    })



    # observe documents actions

    observeEvent(input[[paste("doc_manager_ui_1", "doc_add", sep = "-")]], {

      # this module processes document input
      mod_doc_manager_server("doc_manager_ui_1",
                             connection = db_path(),
                             project = active_project())
      # this module displays active documents
      mod_doc_list_server("doc_list_ui_1",
                          connection = db_path(),
                          project = active_project())
      # the delete module is called to update delete UI by adding new document
      mod_doc_delete_server("doc_delete_ui_1",
                            connection = db_path(),
                            project = active_project())
      # update reactive value containing project documents
      doc_list(doc_list()+1)

    })

    observeEvent(input[[paste("doc_delete_ui_1", "doc_remove", sep = "-")]], {
      # input$doc_remove defined in doc_delete module
      after_deletion <- mod_doc_delete_server("doc_delete_ui_1",
                            connection = db_path(),
                            project = active_project())
      mod_doc_list_server("doc_list_ui_1",
                          connection = db_path(),
                          project = active_project())
      # update reactive value containing project documents
      doc_list(doc_list()+1)

    })

    ## list documents initially
    observe({
      if (!is.null( active_project() ) ) {
        mod_doc_list_server("doc_list_ui_1",
                            connection = db_path(),
                            project = active_project())
        mod_doc_delete_server("doc_delete_ui_1",
                              connection = db_path(),
                              project = active_project())
      }
    })
    
  
  })
}
