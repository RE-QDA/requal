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
    
    h3("New project folder"),
    
    div(span(textOutput(
      ns("project_path")
    ), class = "form-control"), class = "form-group shiny-input-container"),
    
    shinyFiles::shinyDirButton(
      ns("sel_directory"),
      "Folder select",
      "Please select a project folder"
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
mod_launchpad_creator_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    # module reactive vals ----
    
    db_path <- reactiveVal(NULL)
    active_project <- reactiveVal(NULL)
    doc_list <- reactiveVal(NULL)
    project <- reactiveValues()
    
   # file system prep -----
    volumes <- c(Home = fs::path_home(), get_volume_paths())
    
    
    shinyFiles::shinyDirChoose(
      input,
      "sel_directory",
      roots = volumes,
      defaultRoot = "Home",
      session = session,
      restrictions = system.file(package = "base"),
      allowDirCreate = TRUE
    )
 
    output$project_path <- renderText({
      if (is.integer(input$sel_directory)) {
        "No project directory has been selected."
      } else {
        project_directory()
      }
    })
    
    project_directory <-
      reactive({
        normalizePath(shinyFiles::parseDirPath(volumes, input$sel_directory))
      })
    
    # set active project from create ----
    
    observeEvent(input$project_create, {
      req(input$project_name, input$sel_directory)
      db_path(paste0(
        project_directory(),
        .Platform$file.sep,
        paste0(gsub(
          "[^a-zA-Z]+",
          "",
          iconv(input$project_name,
                to = "ASCII//TRANSLIT")
        ), ".requal")
      ))
      
      # create project event - DB set up ----
      
      active_project(
        create_project_db(
          project_directory = project_directory(),
          project_name = input$project_name,
          project_description = input$project_description
        ))
      
      # write active project details ----
      
      project$active_project <- active_project()
      project$project_db <- db_path()

    })
    
    # return active project details ----
    
    
    return(reactive(reactiveValuesToList(project)))
    
    
  })
}
    

