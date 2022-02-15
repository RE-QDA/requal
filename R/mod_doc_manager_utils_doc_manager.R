# delete documents from project
delete_db_documents <- function(project_db, 
                                active_project, 
                                delete_doc_id) {
    
    con <- DBI::dbConnect(RSQLite::SQLite(), project_db)
    on.exit(DBI::dbDisconnect(con))
    
    
    DBI::dbExecute(con,
                   "DELETE from documents
                   WHERE doc_id IN (?)",
                   params = list(delete_doc_id))
    log_delete_document_record(con, active_project, delete_doc_id)
}


# add input document ----
add_input_document <- function(connection, project, doc_name, doc_text, doc_description) {
    con <- DBI::dbConnect(RSQLite::SQLite(),
                          connection)
    
    on.exit(DBI::dbDisconnect(con))

        text_df <- tibble::tibble(
        project_id = project,
        doc_name = .env$doc_name,
        doc_description = .env$doc_description,
        doc_text = .env$doc_text
    )
    
    add_documents_record(con, project, text_df)
    
}

