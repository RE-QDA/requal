#' launchpad_creator UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_launchpad_creator_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    h3("New project name"),
    textInput(
      ns("project_name"),
      label = NULL,
      placeholder = "The name of your project."
    ),
    
    h3("New project description"),
    
    textAreaInput(
      ns("project_description"),
      label = NULL,
      placeholder = "Brief description of your project"
    ),
    
    actionButton(
      ns("project_create"),
      label = "Create project",
      class = "btn-success"
    )
  )
 
  
}
    
#' launchpad_creator Server Functions
#'
#' @noRd 
mod_launchpad_creator_server <- function(id, pool){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    active_project <- reactiveVal(NULL)
    
    # set active project from create ----
    observeEvent(input$project_create, {
      req(input$project_name)
    
      # create project event - DB set up ----
      active_project(
        create_project_db(
          pool, 
          project_name = input$project_name,
          project_description = input$project_description
        ))
    })
    
    # return active project details ----
    return(reactive(active_project()))
    
  })
}
    

