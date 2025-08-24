utils::globalVariables(c(
  "category_name",
  "category_description",
  "code_description",
  "category_title"
))

#' download_csv UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_download_csv_ui <- function(id, type) {
  ns <- NS(id)
  tagList(
    downloadButton(ns(type), label = "CSV")
  )
}

# csv file download handling
handle_download <- function(filename, df) {
  downloadHandler(
    filename = function() {
      filename
    },
    content = function(file) {
      utils::write.csv(req(df), file, fileEncoding = "UTF-8", row.names = FALSE)
    }
  )
}

#' download_csv Server Functions
#'
#' @noRd
mod_download_csv_server <- function(id, glob) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(glob$segments_df, {
      output$download_analysis <- handle_download(
        paste0("requal_export-", format(Sys.time(), "%Y-%m-%d-%H%M%S"), ".csv"),
        glob$segments_df
      )
    })
  })
}
