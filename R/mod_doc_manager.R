#' doc_manager UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_doc_manager_ui <- function(id){
  ns <- NS(id)
  tagList(
    

    textAreaInput(ns("doc_text"), label = NULL, placeholder = "Paste a new document content here"),
    actionButton(ns("doc_add"), label = "Add document")

    )
}
    
#' doc_manager Server Functions
#'
#' @noRd 
mod_doc_manager_server <- function(id, connection, project){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    

    print(connection)
    print(project)
    con <- DBI::dbConnect(RSQLite::SQLite(),
                          connection)
    
    on.exit(DBI::dbDisconnect(con))
    
    project_id <- dplyr::tbl(con, "project") %>% 
      dplyr::filter(project_name == project) %>% 
      dplyr::pull(project_id) %>% 
      unique()
    
    text_df <- tibble::tibble(
      project = project_id,
      document_text = input$doc_text
    )
    
    DBI::dbWriteTable(con, "documents", text_df, 
                      append = TRUE)
    
  })
}
    
## To be copied in the UI
# mod_doc_manager_ui("doc_manager_ui_1")
    
## To be copied in the server
# mod_doc_manager_server("doc_manager_ui_1")
