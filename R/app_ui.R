#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinydashboardPlus
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      options = list(sidebarExpandOnHover = TRUE),
      header = dashboardHeader(),
      sidebar = dashboardSidebar(minified = TRUE, 
                                 collapsed = TRUE,
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
                                 )),
      body = dashboardBody(
        
        h1("requal"),
        
        mod_document_code_ui("document_code_ui_1")
        
        ),
      controlbar = dashboardControlbar(
        overlay = FALSE,
        controlbarMenu(
          id = "codes_menu",
          controlbarItem(
            title = "Codes",
            lapply(paste0("button", 1:10), actionButton, label = "Code")
            ),
          controlbarItem(
            title = "Memos"
            )          
        )
        
      )
      )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'ReQual'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

