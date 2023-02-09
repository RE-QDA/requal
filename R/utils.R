# Define project_name as global variable to pass R CMD Check without notes
utils::globalVariables(c("project_name", 
                         "project_id", 
                         "doc_id",
                         "code_name",
                         "code_id",
                         "segment_id",
                         "code_color",
                         "text",
                         "packageVersion",
                         "doc_name",
                         "doc_description",
                         "created_at",
                         "value",
                         "name",
                         "id",
                         "segment_end",
                         ".",
                         "segment_start",
                         "segment_text",
                         "memo_id", 
                         "coder1_name", 
                         "coder2_name", 
                         "user_name", 
                         "user_id", 
                         "coder1_id", 
                         "coder2_id", 
                         "coder2_id2", 
                         "coder1_id2", 
                         "coder1_name2", 
                         "coder2_name2", 
                         "memo_name", 
                         "n_char", 
                         "is_overlap", 
                         "total_overlap", 
                         "w_total_overlap", 
                         "n_char_coded", 
                         "n_coders",
                         "n_segments", 
                         "doc_text", 
                         "marked", 
                         "n", 
                         "segment_break", 
                         "max_intersect", 
                         "min_intersect", 
                         "position_type", 
                         "position_start", 
                         "tag_start", 
                         "tag_end", 
                         "credentials"
                         ))


set_dashboard_body <- function() {
    
    
    shinydashboard::dashboardBody(
        
        shinyjs::useShinyjs(),
        
        # css for control bar icon
        
        tags$head(tags$style(HTML('
#launchpad_icon{
  margin-right: 10px;
  color: white;
  animation-name: launchpad-animation;
  animation-duration: 1s;
  animation-iteration-count: 1;
}

@keyframes launchpad-animation {
  from {color: white;}
  to {color: red;}
}

  '))),

shinydashboard::tabItems(
    
    shinydashboard::tabItem("Project",
            mod_project_ui("mod_project_ui_1")),
    shinydashboard::tabItem("Codebook",
            mod_codebook_ui("codebook_ui_1")),
    shinydashboard::tabItem("Document",
            mod_document_code_ui("document_code_ui_1")),
    shinydashboard::tabItem("Analysis",
            mod_analysis_ui("analysis_ui_1")),
    shinydashboard::tabItem("Reporting",
            mod_reporting_ui("reporting_ui_1")),
    shinydashboard::tabItem("Settings",
            mod_settings_ui("settings_ui_1")),
    shinydashboard::tabItem("About",
            mod_about_ui("about_ui_1"))
)
    )
    
}

set_left_menu <- function() {
    
    
    shinydashboardPlus::dashboardSidebar(minified = TRUE, 
                     collapsed = FALSE,
                     shinydashboard::sidebarMenu(
                         shinydashboard::menuItem(
                             "Project",
                             tabName = "Project",
                             icon = icon("book")
                         ),
                         shinydashboard::menuItem(
                             "Codebook",
                             tabName = "Codebook",
                             icon = icon("code")
                         ),
                         shinydashboard::menuItem(
                             "Workdesk",
                             tabName = "Document",
                             icon = icon("marker")
                         ),
                         shinydashboard::menuItem(
                             "Analysis",
                             tabName = "Analysis",
                             icon = icon("microscope")
                         ),
                         shinydashboard::menuItem(
                             "Report",
                             tabName = "Reporting",
                             icon = icon("chart-bar")
                         ),
                         # menuItem(
                         #   "Settings",
                         #   tabName = "Settings"
                         # ),
                         shinydashboard::menuItem(
                             "About",
                             tabName = "About",
                             icon = icon("info")
                         )
                     ))
    
    
    
    
}

set_controlbar <- function() {
    
    
    shinydashboardPlus::dashboardControlbar(
        id = "control_bar",
        overlay = TRUE,
        collapsed = FALSE,
        width = 400,
        shinydashboardPlus::controlbarMenu(
            id = "launchpad",
            controlbarItem(
                id = "loader",
                title = "Load",
                icon = icon("spinner"),
                mod_launchpad_loader_ui("launchpad_loader_ui_1")
            ),
            shinydashboardPlus::controlbarItem(
                id = "creator",
                title = "Create",
                icon = icon("plus"),
                mod_launchpad_creator_ui("launchpad_creator_ui_1")
            )
        )
    )
}

# menu col and btn ---

menu_btn <- function(..., label, icon) {
  
  shinyWidgets::dropdown(
   ...,
    label = NULL,
    style = "material-circle",
    tooltip = shinyWidgets::tooltipOptions(
      placement = "left",
      title = label,
      html = FALSE
    ),
    size = "md", 
    width = "370px",
    icon = icon(icon, verify_fa = FALSE) %>% tagAppendAttributes(style = "color: #3c8dbc"), 
    right = TRUE
  ) %>% tagAppendAttributes(style = "padding-right: 5px; padding-top: 10px; top: 1vh; position: relative; min-width: 50%;")
}

menu_column <- function(width = 2, ...) {
    column(width = width,
           ...) %>% tagAppendAttributes(style = "text-align: right; valign: bottom; padding-right: 0px !important;")
  }


# File system: get_volume_paths  ----
get_volume_paths <- function() {
  
  sysinfo <- Sys.info()
  
  if (tolower(sysinfo["sysname"]) == "darwin") {
    
    volumes <- list.dirs(paste0(.Platform$file.sep, "Volumes"), recursive = FALSE)
    volumes_checked <- volumes[fs::file_access(volumes)]
    names(volumes_checked) <- volumes_checked
    volumes_checked
    
  } else if (tolower(sysinfo["sysname"]) == "linux") {
    
    volumes <- list.dirs(paste0(.Platform$file.sep, "media"), recursive = FALSE)
    volumes_checked <- volumes[fs::file_access(volumes)]
    names(volumes_checked) <- volumes_checked
    volumes_checked
    
  } else if (tolower(sysinfo["sysname"]) == "windows") {
    
    volumes_string <- system("wmic logicaldisk get caption", intern = TRUE)
    volumes <- unlist(stringr::str_extract_all(volumes_string, "[A-Z]\\:"))
    volumes_checked <- volumes[fs::file_access(volumes)]
    names(volumes_checked) <- volumes_checked
    volumes_checked
    
  } else {
    
    c(Volumes = fs::path_home())
  }
  
  
}

# loader UI

loader_UI_local <- function(ns){

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

loader_UI_server <- function(ns){

tagList(
  h3("Remote project"),
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

# creator UI

creator_UI_local <- function(ns) {
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

creator_UI_server <- function(ns) {
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

# warnings ------

warn_user <- function(warning) {
  
  showModal(modalDialog(title = "Warning",
                        warning))
}
  


    

