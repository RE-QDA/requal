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
    dashboardPage(title = "ReQual",
      options = list(sidebarExpandOnHover = FALSE),
      header = dashboardHeader(controlbarIcon = shiny::icon("file-code")),
      sidebar = mod_left_menu_ui("left_menu_ui_1"),
      body = mod_dashboard_body_ui("dashboard_body_ui_1"),
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

