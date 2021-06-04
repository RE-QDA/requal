#' dashboard_body UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_dashboard_body_ui <- function(id){
  ns <- NS(id)
  
  dashboardBody(
    
    tabItems(
      
    tabItem("Project",
            mod_project_ui("project_ui_1")),
    tabItem("Codebook",
            mod_codebook_ui("codebook_ui_1")),
    tabItem("Document",
            mod_document_code_ui("document_code_ui_1")),
    tabItem("Analysis",
            mod_analysis_ui("analysis_ui_1")),
    tabItem("Reporting",
            mod_reporting_ui("reporting_ui_1")),
    tabItem("Settings",
            mod_settings_ui("settings_ui_1")),
    tabItem("About",
            mod_about_ui("about_ui_1"))
    )
  )
  
}
    
#' dashboard_body Server Functions
#'
#' @noRd 
mod_dashboard_body_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    # module only generates UI, no server function needed
    
  })
}
    

    

