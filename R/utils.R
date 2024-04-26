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

shinyjs::hidden(shinydashboard::tabItems(
    
    shinydashboard::tabItem("Project",
            mod_project_ui("mod_project_ui_1")),
    shinydashboard::tabItem("Data",
            mod_data_ui("data_1")),
    shinydashboard::tabItem("Attributes",
            mod_attributes_ui("attributes_1")),
    shinydashboard::tabItem("Codebook",
            mod_codebook_ui("codebook_ui_1")),
    shinydashboard::tabItem("Annotate",
            mod_document_code_ui("document_code_ui_1")),
    shinydashboard::tabItem("Analyze",
            mod_analysis_ui("analysis_ui_1")),
    shinydashboard::tabItem("Report",
            mod_reporting_ui("reporting_ui_1"))
)
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
                             "Data",
                             tabName = "Data",
                             icon = icon("database")
                         ),
                        shinydashboard::menuItem(
                             "Attributes",
                             tabName = "Attributes",
                             icon = icon("table")
                         ),
                         shinydashboard::menuItem(
                             "Codebook",
                             tabName = "Codebook",
                             icon = icon("code")
                         ),
                         shinydashboard::menuItem(
                             "Annotate",
                             tabName = "Annotate",
                             icon = icon("marker")
                         ),
                         shinydashboard::menuItem(
                             "Analyze",
                             tabName = "Analyze",
                             icon = icon("microscope")
                         ),
                         shinydashboard::menuItem(
                             "Report",
                             tabName = "Report",
                             icon = icon("chart-bar")
                         ),
                         uiOutput("fab_button_ui"), 
                         id = "tab_menu"

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

menu_btn <- function(..., label, icon, inputId = NULL) {
  
  shinyWidgets::dropdown(
   ...,
    label = NULL,
    style = "material-circle",
    tooltip = shinyWidgets::tooltipOptions(
      placement = "right",
      title = label,
      html = FALSE
    ),
    size = "md", 
    width = "370px",
    icon = icon(icon, verify_fa = FALSE) %>% tagAppendAttributes(style = "color: #3c8dbc"), 
    right = FALSE,
    inputId = inputId
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
  
# send message to interactive or Shiny session
rql_message <- function(msg) {
  if (shiny::isRunning()){
    showNotification(msg)
    } else {
     message(msg)
    }
}
  
# check permission to modify permissions

check_modify_permission <- function(permission, msg) {
  if (permission != 1) warn_user(msg)
  req(permission == 1)
}

# filter data by view permissions

filter_view <- function(df, user_id, permission) {
  if (permission == 0) {
    df %>%
      dplyr::filter(user_id == !!user_id)
  } else if (permission == 1) {
     df
  }
}

# DT options

dt_options <- function() {
  list(
          paging = TRUE,
          searching = TRUE,
          fixedColumns = TRUE,
          autoWidth = TRUE,
          ordering = TRUE,
          dom = "lfrtpBi",
          buttons = c("csv")
        )
}

# Requal menu buttons 

rql_picker_UI <- function(inputId, label, choices = "", multiple = TRUE, none = "") { 

    if (multiple) {
    options <- list(
        `actions-box` = TRUE,
        `select-all-text` = "Select all",
        `deselect-all-text` = "Reset",
        `none-selected-text` = none
      )
    } else {
       options <- list(
        `none-selected-text` = none
      )
    }
    shinyWidgets::pickerInput(inputId, label,
      choices = choices, multiple = multiple,
      options = options
    )
} 


rql_button_UI <- function(inputId, label, class = NULL) {
     actionButton(inputId, label, class = class) %>% 
      tagAppendAttributes(style = "text-align: left;")
}

rql_logo <- function(){
  "data:image/png;base64,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"
}
