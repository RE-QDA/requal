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
        #if (glob$user$permissions_modify) {
          tagList(
            tabsetPanel(
              id = "project_tabs",
              tabPanel(
                "Manage documents",
                mod_doc_manager_ui("doc_manager_ui_1"),
                value = "doc_manager_tab"
              ),
              tabPanel(
                "Manage users",
                mod_use_manager_ui("use_manager_1"),
                value = "use_manager_tab"
              )
            )
          )
        # } else {
        #   tagList(
        #     tabsetPanel(
        #       id = "project_tabs",
        #       tabPanel(
        #         "Manage documents",
        #         mod_doc_manager_ui("doc_manager_ui_1"),
        #         value = "doc_manager_tab"
        #       )
        #     )
        #   )
        # }
      } else {
        "No active project."
      }
    })

    return(NULL)
  })
}
