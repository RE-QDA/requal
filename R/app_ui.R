#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboardPlus
#' @noRd

app_ui_setup <- function(request) {
    
    tagList(
        # Leave this function for adding external resources
        golem_add_external_resources(),
        # Your application UI logic
        dashboardPage(title = "ReQual",
                      options = list(sidebarExpandOnHover = FALSE),
                      header = dashboardHeader(title = tags$span(
                          tags$img(src="www/requal_logo.png", 
                                   height="70%", style = "margin-right: 20px"), "ReQual"),
                          tags$li(mod_user_ui("user_ui_1")) %>% 
                              tagAppendAttributes(class = "dropdown"),
                          controlbarIcon = icon("ellipsis-v", id = "launchpad_icon"),
                          dropdownBlock2(
                              id = "btn-memo",
                              badgeStatus = NULL,
                              icon = shiny::icon("sticky-note-o", verify_fa = FALSE),
                              mod_memo_ui("memo_ui_1")
                          ) %>% tagAppendAttributes(class = "memo") 
                      ),
                      sidebar = set_left_menu(),
                      body = set_dashboard_body(),
                      controlbar = set_controlbar()
        )
    )
}

if (get_golem_config("mode") == "local") {
    
 app_ui <- app_ui_setup
    
} else {
    # define some credentials
    credentials <- data.frame(
        user = c("admin", "requal"), # mandatory
        password = c("admin", "requal"), # mandatory
        start = c("2019-04-15"), # optinal (all others)
        expire = c(NA, "2022-12-31"),
        admin = c(FALSE, TRUE),
        comment = "Simple and secure authentification mechanism 
  for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
    )
    
    app_ui <- shinymanager::secure_app(app_ui_setup)
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
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
    
  )
}

