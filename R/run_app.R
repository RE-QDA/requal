#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts. 
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options 
run_app <- function(
  onStart = NULL,
  options = list(), 
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
    
    g_opts <- list(...)
    
    if(!is.null(g_opts$db_path)){
        with_golem_options(
            app = shinyApp(
                ui = app_ui,
                server = app_server,
                onStart = onStart,
                options = options, 
                enableBookmarking = enableBookmarking, 
                uiPattern = uiPattern
            ), 
            golem_opts = g_opts
        )
    }else{
        path <- paste0(config_db_app(), "/requal.requal")
        with_golem_options(
            app = shinyApp(
                ui = app_ui,
                server = app_server,
                onStart = onStart,
                options = options, 
                enableBookmarking = enableBookmarking, 
                uiPattern = uiPattern
            ), 
            golem_opts = list(db_path = path)
        )
    }
    
}
