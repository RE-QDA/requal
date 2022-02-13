#' left_menu UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinydashboard sidebarMenu menuItem
mod_left_menu_ui <- function(id){
  ns <- NS(id)

    dashboardSidebar(minified = TRUE, 
                     collapsed = FALSE,
                     sidebarMenu(
                       menuItem(
                         "Project",
                         tabName = "Project",
                         icon = icon("book")
                       ),
                       menuItem(
                         "Codebook",
                         tabName = "Codebook",
                         icon = icon("code")
                       ),
                       menuItem(
                         "Workdesk",
                         tabName = "Document",
                         icon = icon("marker")
                       ),
                       menuItem(
                         "Analysis",
                         tabName = "Analysis",
                         icon = icon("microscope")
                       ),
                       menuItem(
                         "Report",
                         tabName = "Reporting",
                         icon = icon("chart-bar")
                       ),
                       # menuItem(
                       #   "Settings",
                       #   tabName = "Settings"
                       # ),
                       menuItem(
                         "About",
                         tabName = "About",
                         icon = icon("info")
                       )
                     ))
 

}
    
#' left_menu Server Functions
#'
#' @noRd 
mod_left_menu_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # module only generates UI, no server function needed
 
  })
}
