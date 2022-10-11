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
    loc <- reactiveValues()
    loc$db_path <- NULL
    loc$active_project <- NULL
    loc$doc_list <- NULL
    loc$project <- ""
    
    observeEvent(req(setup$mode == "local"), {
  
    
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
    
     observeEvent(input$sel_file_load, {
         loc$project_file_load <- normalizePath(shinyFiles::parseFilePaths(volumes, input$sel_file_load)$datapath)
         
         if(length(loc$project_file_load > 0)){
             
             glob$pool <- pool::dbPool(
                 drv = RSQLite::SQLite(), 
                 dbname = loc$project_file_load
             )
             
             reactive({ 
                 onStop(function(){
                 pool::poolClose(glob$pool) 
                     }
                 )
            })
             

             updateSelectInput(session,
                               "project_selector_load",
                               choices = read_project_db(glob$pool,
                                                         project_id = NULL))
         }
    })
  
    
    output$project_path_load <- renderText({
      if (is.integer(input$sel_file_load)) {
        "No project file has been selected."
      } else {
        loc$project_file_load
      }
    })
    
    # set active project from load ----
    
    output$project_active <- renderUI({
      if (is.null(loc$active_project)) {
        "No active project."
      }
    })
    
    
   
        

      
    
    observeEvent(input$project_load, {
        loc$active_project <- isolate(
          read_project_db(pool = glob$pool,
                          project_id = input$project_selector_load)
        )
        
        glob$active_project <- loc$active_project
        
    })
 
   
    })
    
    observeEvent(req(setup$mode == "server"), {
        
        pool <- pool::dbPool(
            drv = RPostgreSQL::PostgreSQL(),
            dbname = "requal",
            user = "radimhladik",
            password = "test"
        )
        
        
        onStop(reactive({function(){
            pool::poolClose(glob$pool) }})
        )
        
        
        updateSelectInput(session,
                          "project_selector_load",
                          choices = read_project_db(glob$pool,
                                                    project_id = NULL))
    
        
        
        
        })
  })
    }
