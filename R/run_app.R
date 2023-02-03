#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(mode = "local",
                    dbhost = "localhost",
                    dbport = 5432,
                    dbname = NULL,
                    dbusername = NULL,
                    dbpassword = NULL,
                    credentials_path = NULL,
                    credentials_pass = NULL,
                    uiPattern = "/",
                    ...) {
  with_golem_options(
    app = shinyApp(
      ui = switch(mode,
          server = shinymanager::secure_app(app_ui,
                                   tags_top = tags$img(
                                       src = "www/requal_logo.png",
                                       height = "30%", style = "margin-right: 20px"
                                   ),
                                   enable_admin = TRUE,
                                   fab_position = "bottom-left"
          ),
          local = app_ui
          ),
      server = app_server,
      onStart = purrr::partial(eval, expr = make_globals, envir = globalenv()),
      uiPattern = uiPattern
    ),
    golem_opts = list(mode = mode,
                      dbhost = dbhost,
                      dbport = dbport, 
                      dbname = dbname,
                      dbusername = dbusername,
                      dbpassword = dbpassword,
                      credentials_path = credentials_path,
                      credentials_pass = credentials_pass)
  )
}

