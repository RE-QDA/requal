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
    
    
    # create project menu
    
    observeEvent(input$project_create_menu, {
      output$project_action <-  renderUI({
        box(
          closable = TRUE,
          width = NULL,
          title = "Create project",
          solidHeader = TRUE,
          
          h3("Project name"),
          textInput(
            ns("project_name"),
            label = NULL,
            placeholder = "The name of your project."
          ),
          
          h3("Project folder"),
          
          div(span(textOutput(
            ns("project_path")
          ), class = "form-control"), class = "form-group shiny-input-container"),
          
          shinyFiles::shinyDirButton(
            ns("sel_directory"),
            "Folder select",
            "Please select a project folder"
          ),
          
          h3("Project description"),
          
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
        
      })
      
    })
    
    # load project menu
    
    observeEvent(input$project_load_menu, {
      output$project_action <-  renderUI({
        box(
          closable = TRUE,
          width = NULL,
          title = "Load project",
          solidHeader = TRUE,
          
          h3("Project file"),
          
          div(span(textOutput(
            ns("project_path_load")
          ), class = "form-control"), class = "form-group shiny-input-container"),
          
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
      })
      
      
      
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
    
    shinyFiles::shinyFileChoose(
      input,
      "sel_file_load",
      roots = volumes,
      session = session,
      restrictions = system.file(package = "base"),
      pattern = c('\\.requal')
    )
    
    project_directory <-
      reactive({
        normalizePath(shinyFiles::parseDirPath(volumes, input$sel_directory))
      })
    
    project_file_load <-
      reactive({
        normalizePath(shinyFiles::parseFilePaths(volumes, input$sel_file_load)$datapath)
      })
    
    output$project_path <- renderText({
      if (is.integer(input$sel_directory)) {
        "No project directory has been selected."
      } else {
        project_directory()
      }
    })
    
    output$project_path_load <- renderText({
      if (is.integer(input$sel_file_load)) {
        "No project file has been selected."
      } else {
        project_file_load()
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
    
    project_active <- reactiveVal("No active project.")
    output$project_active <- renderUI({
      project_active()
    })
    
    
    observe({
      updateSelectInput(session,
                        "project_selector_load",
                        choices = read_project_db(project_file_load(),
                                                  name = NULL))
    })
    
    db_path <- reactiveVal(NULL)
    
    observeEvent(input$project_load, {
      db_path(project_file_load())
      project_active(isolate(
        read_project_db(db_path(),
                        name = input$project_selector_load)
      ))
      
      
    })
    
    # set active project from create
    
    observeEvent(input$project_create, {
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
      
      project_active(isolate(read_project_db(db_path(),
                                             name = input$project_name)))
      
    })
    
    # display active project details
    
    
    observe({
      output$project_manager <-  renderUI({
        if (project_active() != "No active project.") {
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
    
    # manage documents
    
    observeEvent(input[[paste("doc_manager_ui_1", "doc_add", sep = "-")]], {
      mod_doc_manager_server("doc_manager_ui_1",
                             connection = db_path(),
                             project = project_active())
      mod_doc_list_server("doc_list_ui_1",
                          connection = db_path(),
                          project = project_active())
      mod_doc_delete_server("doc_delete_ui_1",
                            connection = db_path(),
                            project = project_active())
      
    })
    
    ## list documents
    
    observe({
      if (project_active() != "No active project.") {
        mod_doc_list_server("doc_list_ui_1",
                            connection = db_path(),
                            project = project_active())
        mod_doc_delete_server("doc_delete_ui_1",
                              connection = db_path(),
                              project = project_active())
      }
    })
    
  
    
  })
}
