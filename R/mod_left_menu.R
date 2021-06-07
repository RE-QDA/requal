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
                         tabName = "Project"
                       ),
                       menuItem(
                         "Codebook",
                         tabName = "Codebook"
                       ),
                       menuItem(
                         "Document",
                         tabName = "Document"
                       ),
                       menuItem(
                         "Analysis",
                         tabName = "Analysis"
                       ),
                       menuItem(
                         "Reporting",
                         tabName = "Reporting"
                       ),
                       menuItem(
                         "Settings",
                         tabName = "Settings"
                       ),
                       menuItem(
                         "About",
                         tabName = "About"
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
