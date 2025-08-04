#' codebook_import UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_codebook_import_ui <- function(id) {
  ns <- NS(id)
  tagList(
    "test"
  )
}

#' codebook_import Server Functions
#'
#' @noRd
mod_codebook_import_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_codebook_import_ui("codebook_import_1")

## To be copied in the server
# mod_codebook_import_server("codebook_import_1")
