#' launchpad_import UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_launchpad_import_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Import file"),
    fileInput(
      ns("import_file"),
      with_help(
        "Choose QDPX File",
        help_item = "qdpx_import",
        visible = TRUE
      ),
      accept = c(
        ".qdpx"
      )
    ),
    actionButton(
      ns("project_import"),
      label = "Import project",
      class = "btn-success"
    )
  )
}

#' launchpad_import Server Functions
#'
#' @noRd
mod_launchpad_import_server <- function(id, glob) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}
