#' launchpad_loader UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_launchpad_loader_ui <- function(id){
  ns <- NS(id)
  tagList(
      
      uiOutput(ns("db_path_selector")),
      
      selectInput(
        ns("project_selector_load"),
        "Select project",
        choices = NULL
      ),
      
      actionButton(
        ns("project_load"),
        label = "Load project",
        class = "btn-success"
      )
 
  )
}
    
#' launchpad_loader Server Functions
#'
#' @noRd 
mod_launchpad_loader_server <- function(id, pool){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # module reactive vals ----
    
    active_project <- reactiveVal(NULL)
    doc_list <- reactiveVal(NULL)
    
    # set active project from load ----
    
    output$project_active <- renderUI({
      if (is.null(active_project())) {
        "No active project."
      }
    })
    
    observe({
        updateSelectInput(session,
                          "project_selector_load",
                          choices = c("", read_project_db(pool, project_id = NULL)))
    })
    
    
    observeEvent(input$project_load, {
      
      req(input$project_selector_load)
      
      active_project(#isolate(
        read_project_db(pool, 
                        project_id = input$project_selector_load)
      )#)
      
    })
    
    # return active project details
    return(reactive(active_project()))
 
  })
}
