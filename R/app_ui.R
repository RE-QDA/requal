#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboardPlus
#' @noRd
app_ui <- function(request, mode = NULL, access = NULL) {
  tagList(
    shinyjs::useShinyjs(),
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      title = "requal",
      options = list(sidebarExpandOnHover = FALSE),
      header = dashboardHeader(
        title = tags$span(
          tags$img(
            src = "www/requal_logo.png",
            height = "70%", style = "margin-right: 20px"
          ), "requal"
        ),
        controlbarIcon = icon("ellipsis-v", id = "launchpad_icon"),
        dropdownBlock2(
          id = "btn-free_memo",
          badgeStatus = NULL,
          icon = shiny::icon("sticky-note", verify_fa = FALSE),
          mod_memo_ui("memo_ui_1")
        ) %>% tagAppendAttributes(class = "free_memo", style = "display:none;", title = "Free memo"),
        tags$li(mod_user_ui("user_ui_1")) %>%
          tagAppendAttributes(class = "dropdown"),
        tags$li(actionLink(
          inputId = ".shinymanager_logout",
          label = NULL,
          icon = icon("sign-out", verify_fa = FALSE),
          title = "Logout"
        )) %>%
          tagAppendAttributes(class = "dropdown")
      ),
      sidebar = set_left_menu(),
      body = set_dashboard_body(),
      controlbar = set_controlbar()
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
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "requal"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
