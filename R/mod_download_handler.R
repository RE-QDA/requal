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
 
    downloadButton(ns("download"), label = "Download")
    
  )
}
    
#' download_handler Server Functions
#'
#' @noRd 
mod_download_handler_server <- function(id, df_out){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    output$download <- downloadHandler(
      
      filename = function() {
        paste0("requal_export-", format(Sys.time(), "%Y-%m-%d-%H%M%S"), ".csv")
      },
      content = function(file) {
        write.csv(req(df_out()), file)
      }
    )
 
  })
}
    

