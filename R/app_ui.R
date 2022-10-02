#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboardPlus
#' @noRd

app_ui_setup <- function(request) {
<<<<<<< Updated upstream
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      title = "ReQual",
      options = list(sidebarExpandOnHover = FALSE),
      header = dashboardHeader(
        title = tags$span(
          tags$img(
            src = "www/requal_logo.png",
            height = "70%", style = "margin-right: 20px"
          ), "ReQual"
        ),
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
=======
    
    tagList(
        # Leave this function for adding external resources
        golem_add_external_resources(),
        # Your application UI logic
        dashboardPage(title = "ReQual",
                      options = list(sidebarExpandOnHover = FALSE),
                      header = dashboardHeader(title = tags$span(
                          tags$img(src="www/requal_logo.png", 
                                   height="70%", style = "margin-right: 20px"), "reQual"),
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
>>>>>>> Stashed changes
    )
  )
}

if (get_golem_config("mode") == "local") {
  app_ui <- app_ui_setup
} else {
<<<<<<< Updated upstream
  # define some credentials
  credentials <- data.frame(
    user = c("admin", "requal"), # mandatory
    password = c("admin", "requal"), # mandatory
    start = c("2019-04-15"), # optional (all others)
    expire = c(NA, "2022-12-31"),
    admin = c(FALSE, TRUE),
    comment = "Simple and secure authentification mechanism
  for single 'Shiny' applications.",
    stringsAsFactors = FALSE
  )

  app_ui <- shinymanager::secure_app(app_ui_setup)
=======
    print(get_golem_config("mode"))
    print(get_golem_config("access"))
    if (!file.exists(get_golem_config("access"))) {
    # define some credentials
    # Init DB using credentials data
    credentials <- data.frame(
        user = c("admin", "user"),
        password = c("admin", "user"),
        # password will automatically be hashed
        admin = c(TRUE, FALSE),
        stringsAsFactors = FALSE
    )
    
    # you can use keyring package to set database key
    keyring::key_set("requal-access-key", "requal")
    
    # Init the database
    shinymanager::create_db(
        credentials_data = credentials,
        sqlite_path = get_golem_config("access"), # will be created
        passphrase = keyring::key_get("requal-access-key", "requal")
        # passphrase = "passphrase_wihtout_keyring"
    ) 
    }
    app_ui <- shinymanager::secure_app(app_ui_setup, 
                                       tags_top = tags$img(src="www/requal_logo.png", 
                                                           height="30%", style = "margin-right: 20px"),
                                       enable_admin = TRUE, 
                                       fab_position = "bottom-left"
                                       )
>>>>>>> Stashed changes
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
      app_title = "ReQual"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
