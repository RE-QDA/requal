#' project UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_project_ui <- function(id) {
  ns <- NS(id)
        tagList(
            tabsetPanel(
              id = "project_tabs",
              tabPanel(
                "Project info",
                fluidRow(class = "module_content",
                uiOutput(ns("project_manager"))
                ),
                value = "project_info_tab"
              ),
              tabPanel(
                "Manage users",
                mod_user_manager_ui("user_manager_1"),
                value = "user_manager_tab"
              ),
              tabPanel(
                "About",
                mod_about_ui("about_ui_1"),
                value = "about_tab"
              )
            )
        )
}

#' project Server Functions
#'
#' @noRd
mod_project_server <- function(id, glob) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$project_manager <- renderUI({
      if (!is.null(glob$active_project) & !is.null(glob$pool)) {
       tagList(
       br(),
       h4(names(glob$active_project))
       )
      } else {
        tagList(
          br(),
        "No active project."
        )
      }
})
    return(NULL)
  })
}
