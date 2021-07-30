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
    

    con <- DBI::dbConnect(RSQLite::SQLite(),
                          connection)
    
    on.exit(DBI::dbDisconnect(con))
    
    project_id <- dplyr::tbl(con, "projects") %>% 
      dplyr::filter(project_name == project) %>% 
      dplyr::pull(project_id) %>% 
      unique()
    
    text_df <- tibble::tibble(
      project_id = project_id,
      doc_text = input$doc_text
    )
    
    add_documents_record(con, project_id, text_df)
  })
}

