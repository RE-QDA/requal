#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
    mod_project_server("project_ui_1")
    
    mod_document_code_server("document_code_ui_1")
    
}
