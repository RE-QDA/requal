#' project UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_project_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidRow(
      column(width = 6,
        
        uiOutput(ns("project_active"))
        
      ),
      column(width = 6,
        
        actionButton(ns("project_load_menu"), label = "Load existing project"),
        
        actionButton(ns("project_create_menu"), label = "Create new project"),
        
        uiOutput(ns("project_action"))
      )
    )
  )
                     

}

#' project Server Functions
#'
#' @noRd 
mod_project_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
# create project menu
    
     observeEvent(input$project_create_menu, {
      
       output$project_action <-  renderUI({
         
         box(closable = TRUE, width = NULL,
           
     h3("Project name"), 
     textInput(ns("project_name"), label = NULL, placeholder = "The name of your project."),
     
     h3("Project folder"), 
     
     div(span(textOutput(ns("project_path")), class = "form-control"), class = "form-group shiny-input-container"),
     
     shinyFiles::shinyDirButton(ns("sel_directory"), "Folder select", "Please select a project folder"),
     
     h3("Project description"), 
     
     textAreaInput(ns("project_description"), label = NULL, placeholder = "Brief description of your project"),
     
     actionButton(ns("project_create"), label = "Create project", class = "btn-success"))
         
       })
       
     })
   
# load project menu
     
     observeEvent(input$project_load_menu, {
       
       output$project_action <-  renderUI({
         
         box(closable = TRUE, width = NULL,
       
       h3("Project folder"),
       
       div(span(textOutput(ns("project_path_load")), class = "form-control"), class = "form-group shiny-input-container"),

       shinyFiles::shinyDirButton(ns("sel_directory_load"), "Folder select", "Please select a project folder"),
       
       selectInput(ns("project_selector_load"), "Select project", choices = NULL),

       actionButton(ns("project_load"), label = "Load project", class = "btn-success")
   ) })
       
       
     
   })
     
     
     
     # set up paths
     
     volumes <- c(Home = fs::path_home())
     
     shinyFiles::shinyDirChoose(
       input,
       "sel_directory",
       roots = volumes,
       session = session,
       restrictions = system.file(package = "base"),
       allowDirCreate = TRUE
     )
     
     shinyFiles::shinyDirChoose(
       input,
       "sel_directory_load",
       roots = volumes,
       session = session,
       restrictions = system.file(package = "base"),
       allowDirCreate = TRUE
     )
     
     project_directory <-
       reactive({
         normalizePath(shinyFiles::parseDirPath(volumes, input$sel_directory))
       })
     
     project_directory_load <-
       reactive({
         normalizePath(shinyFiles::parseDirPath(volumes, input$sel_directory_load))
       })
     
     output$project_path <- renderText({
       if (is.integer(input$sel_directory)) {
         "No project directory has been selected."
       } else {
         project_directory()
       }
     })
     
     output$project_path_load <- renderText({
       if (is.integer(input$sel_directory_load)) {
         "No project directory has been selected."
       } else {
         project_directory_load()
       }
     })
     
     
     # create project event
     
     observeEvent(input$project_create, {
       create_project_db(
         project_directory = project_directory(),
         project_name = input$project_name,
         project_description = input$project_description
       )
       
       
     })
     
     # set active project from load
     output$project_active <- renderUI({
       "No active project."
     })
     
     
     observe({
       updateSelectInput(session,
                         "project_selector_load",
                         choices = read_project_db(project_directory_load()))
     })
     
     observeEvent(input$project_load, {
       output$project_active <-
         reactive({
           isolate(read_project_db(project_directory_load()))
         })
       
     })
     
     # set active project from create
     
     observeEvent(input$project_create, {
       output$project_active <-
         reactive({
           isolate(read_project_db(project_directory()))
         })
       
     })
     
     
     
  })
}
