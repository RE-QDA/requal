# Define project_name as global variable to pass R CMD Check without notes
utils::globalVariables(c("project_name", 
                         "project_id", 
                         "doc_id",
                         "code_name",
                         "code_id",
                         "segment_id",
                         "code_color",
                         "text"))


set_dashboard_body <- function() {
    
    
    shinydashboard::dashboardBody(
        
        shinyjs::useShinyjs(),
        
        # css for control bar icon
        
        tags$head(tags$style(HTML('
#launchpad_icon{
  margin-right: 20px;
  color: white;
  animation-name: launchpad-animation;
  animation-duration: 2s;
  animation-iteration-count: 3;
}

@keyframes launchpad-animation {
  from {color: white;}
  to {color: red;}
}
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