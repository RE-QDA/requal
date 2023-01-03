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
mod_launchpad_creator_server <- function(id, glob, setup){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # module reactive vals ----
    loc <- reactiveValues()
    loc$db_path <- NULL
    loc$active_project <- NULL
    loc$doc_list <- NULL
    
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
        loc$project_directory
      }
    })
    
    observeEvent(input$sel_directory, {
        loc$project_directory <- normalizePath(shinyFiles::parseDirPath(volumes, input$sel_directory))
      })
    
    # set active project from create ----
    observeEvent(input$project_create, {
      req(input$project_name, input$sel_directory)

      loc$db_path <- paste0(
        loc$project_directory,
        .Platform$file.sep,
        paste0(gsub(
          "[^a-zA-Z0-9]+",
          "",
          iconv(input$project_name,
                to = "ASCII//TRANSLIT")
        ), ".requal")
      )
      
      if(file.exists(loc$db_path)){
        shinyjs::addClass("project_name", "invalid")
        updateTextInput(
          session = session,
          inputId = "project_name", 
          value = paste("project", input$project_name, "already exists"), 
          placeholder = paste("project", input$project_name, "already exists"))
      }else{
        shinyjs::removeClass("project_name", "invalid")
        # create project event - DB set up ----
        mode <- golem::get_golem_options("mode")
        
        glob$pool <- pool::dbPool(
            drv = switch(setup$mode,
                         "local" = RSQLite::SQLite(), 
                         "server" = RSQLite::SQLite()), # todo
            dbname = switch(setup$mode,
                            "local" = loc$db_path, 
                            "server" = loc$db_path) #todo
        )
        
        loc$active_project <- create_project_db(
            pool = glob$pool,
            project_name = input$project_name,
            project_description = input$project_description
          )
        
        # write active project details ----
        glob$active_project <- loc$active_project
        
      }
      
    })
    
  })
}
    

