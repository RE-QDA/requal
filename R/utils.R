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

# dummy function for satisfying checks (getting rid of Note on not used imports)
dummy <- function(){
  dbplyr::sql
  RPostgreSQL::dbConnect
}

set_dashboard_body <- function() {
    
    shinydashboard::dashboardBody( 
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




db_helper_column <- function(pool, table, column, action){
  
  check_colnames <- colnames(dplyr::tbl(pool, table))
 query <-  switch(action,
    "add" = glue::glue_sql("
        ALTER TABLE {`table`} 
        ADD COLUMN {`column`} INTEGER;
        ", .con = pool),
    "drop" = glue::glue_sql("
        ALTER TABLE {`table`}
        DROP COLUMN {`column`}
        ", .con = pool)
  )
    if (!column %in% check_colnames && action == "add") {
      res <- DBI::dbExecute(pool, query) 
      } else if (column %in% check_colnames && action == "drop"){
         res <- DBI::dbExecute(pool, query) 
      } else {
         NULL
      }
}

db_update_value <- function(pool, table, col_val, by_col_val){
  # col_val can be a list - list(c(col=1), c(col=2))
  query <- purrr::map(col_val, .f = function(x){ 
  glue::glue_sql("UPDATE {table}
                 SET {names(x)} = {x}
                 WHERE {names(by_col_val)} = {by_col_val}", .con = pool)})
      
      res <- purrr::map(query, ~tryCatch({DBI::dbExecute(pool, .x)}))
      
}


# format class HTML

format_class_id <- function(x, class_name = NULL)   {
class_string <- purrr::map_chr(unique(na.omit(x)), function(y) paste0(class_name, "_id_", y))
if (length(class_string) > 0) {
  class_string <- paste(class_name, paste(class_string, collapse = " "), collapse = " ") 
} else {
   class_string <- ""
}
class_string
}




