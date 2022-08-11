#' download_handler UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_download_handler_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    downloadButton(ns("download"), label = "CSV")
    
  )
}
    
#' download_handler Server Functions
#'
#' @noRd 
mod_download_handler_server <- function(id, glob){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$download <- downloadHandler(
        
      filename = function() {
        paste0("requal_export-", format(Sys.time(), "%Y-%m-%d-%H%M%S"), ".csv")
      },
      content = function(file) {
        utils::write.csv(req(glob$segments_df), file)
      }
    )
 
  })
}
    

