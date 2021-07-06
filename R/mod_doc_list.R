#' doc_list UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_doc_list_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    textOutput(ns("doc_list"))
 
  )
}
    
#' doc_list Server Functions
#'
#' @noRd 
mod_doc_list_server <- function(id, connection, project){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    

    con <- DBI::dbConnect(RSQLite::SQLite(),
                          connection)
    
    on.exit(DBI::dbDisconnect(con))

    if (DBI::dbExistsTable(con, "documents")) {
      
      project_id <- dplyr::tbl(con, "project") %>% 
        dplyr::filter(project_name == project) %>% 
        dplyr::pull(project_id) %>% 
        unique()
    
      doc_list <- dplyr::tbl(con, "documents") %>% 
        dplyr::filter(project == project_id) %>% 
        dplyr::pull(document_id)
      
    output$doc_list <- renderText({doc_list})
    
    }

 
  })
}
    
## To be copied in the UI
# mod_doc_list_ui("doc_list_ui_1")
    
## To be copied in the server
# mod_doc_list_server("doc_list_ui_1")
