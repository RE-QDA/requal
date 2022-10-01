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
    uiOutput(ns(
      "project_manager"
    ))
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
          tabsetPanel(
            tabPanel(
              "Manage documents",
              mod_doc_manager_ui("doc_manager_ui_1")
            )
          ) # ,
          # tabPanel("Project information"),
          # tabPanel("Settings")
        )
      } else {
        "No active project."
      }
    })

    return(NULL)
  })
}
