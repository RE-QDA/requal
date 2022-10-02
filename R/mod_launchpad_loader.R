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
    

      
      h3("Project file"),
      
      div(span(textOutput(
        ns("project_path_load")
      ), class = "form-control overflow_barrier"), class = "form-group shiny-input-container"),
      
      shinyFiles::shinyFilesButton(
        ns("sel_file_load"),
        "File select",
        "Please select a project file",
        multiple = FALSE
      ),
      
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
mod_launchpad_loader_server <- function(id, glob, setup){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # module reactive vals ----

    db_path <- reactiveVal(NULL)
    active_project <- reactiveVal(NULL)
    doc_list <- reactiveVal(NULL)
    project <- reactiveValues()
    
    # file system prep ----
    volumes <- c(Home = fs::path_home(), get_volume_paths())
    
    
    shinyFiles::shinyFileChoose(
      input,
      "sel_file_load",
      roots = volumes,
      defaultRoot = "Home",
      session = session,
      restrictions = system.file(package = "base"),
      pattern = c('\\.requal')
    )
    
    
    project_file_load <-
      reactive({
        normalizePath(shinyFiles::parseFilePaths(volumes, input$sel_file_load)$datapath)
      })
    
    output$project_path_load <- renderText({
      if (is.integer(input$sel_file_load)) {
        "No project file has been selected."
      } else {
        project_file_load()
      }
    })
    
    # set active project from load ----
    
    output$project_active <- renderUI({
      if (is.null(active_project())) {
        "No active project."
      }
    })
    
    
    observeEvent(input$sel_file_load, {
      if(length(project_file_load() > 0)){

            glob$pool <- pool::dbPool(
              drv = switch(setup$mode,
                           "local" = RSQLite::SQLite(), 
                           "server" = RSQLite::SQLite()), #todo
              dbname = switch(setup$mode,
                              "local" = project_file_load(), 
                              "server" = project_file_load()) #todo
          )
          
          updateSelectInput(session,
                            "project_selector_load",
                            choices = read_project_db(glob$pool,
                                                      project_id = NULL))
      }
      
    })
    
    observeEvent(input$project_load, {
        active_project(isolate(
          read_project_db(pool = glob$pool,
                          project_id = input$project_selector_load)
        ))
        
        glob$active_project <- active_project()
        
    })
 
  })
}
