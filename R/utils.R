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
