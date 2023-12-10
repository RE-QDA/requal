#' download_csv UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_download_csv_ui <- function(id, type){
  ns <- NS(id)
  tagList(
    downloadButton(ns(type), label = "CSV")
  )
}

#' download_csv Server Functions
#'
#' @noRd 
mod_download_csv_server <- function(id, glob){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$download_analysis <- downloadHandler(
      
      filename = function() {
        paste0("requal_export-", format(Sys.time(), "%Y-%m-%d-%H%M%S"), ".csv")
      },
      content = function(file) {
        utils::write.csv(req(glob$segments_df), file, row.names = FALSE)
      }
    )
 
    output$download_codebook <- downloadHandler(
      
      filename = function() {
        paste0("requal_codebook_export-", format(Sys.time(), "%Y-%m-%d-%H%M%S"), ".csv")
      },
      content = function(file) {
        utils::write.csv(req(glob$codebook), file, row.names = FALSE)
      }
    )
  })
}
    

