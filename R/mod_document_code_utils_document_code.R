read_doc_db <- function(active_project, project_db) {
    
    if (isTruthy(active_project)) {
        
        con <- DBI::dbConnect(RSQLite::SQLite(),
                              project_db)
        on.exit(DBI::dbDisconnect(con))
        
        db_docs_df <- dplyr::tbl(con, "documents") %>%
            dplyr::filter(.data$project_id == as.integer(active_project)) %>%
            dplyr::select(doc_name, doc_id) %>%
            dplyr::collect() %>% 
            dplyr::mutate(doc_name = ifelse(is.na(doc_name), 
                                            as.character(doc_id), 
                                            doc_name))
        
        named_ids <- db_docs_df %>% 
            dplyr::select(dplyr::ends_with("_name")) %>% 
            dplyr::pull()
        ids <- db_docs_df %>% 
            dplyr::select(dplyr::ends_with("_id")) %>% 
            dplyr::pull()
        
        choices <- ids
        names(choices) <- named_ids
        
        return(choices)
        
    
        } else {""}
    
}

load_doc_db <- function(active_project, project_db, doc_id) {
    
    if (isTruthy(active_project)) {
        
        con <- DBI::dbConnect(RSQLite::SQLite(),
                              project_db)
        on.exit(DBI::dbDisconnect(con))
        
        doc_text <- dplyr::tbl(con, "documents") %>%
            dplyr::filter(.data$project_id == as.integer(active_project)) %>%
            dplyr::filter(.data$doc_id == as.integer(.env$doc_id)) %>% 
            dplyr::pull(doc_text) %>% 
            stringr::str_replace_all("\\n", "<br>")
            
        
        return(doc_text)
        
        
    } else {""}
    
}



# Write document segments to DB -------------------------------------------

write_segment_db <- function(
    active_project,
    project_db,
    doc_id,
    code_id,
    segment,
    startOff,
    endOff
) {
    
    con <- DBI::dbConnect(RSQLite::SQLite(),
                          project_db)
    on.exit(DBI::dbDisconnect(con))
    
    segment_df <- data.frame(project_id = active_project,
                             doc_id = doc_id,
                             code_id = code_id,
                             segment_start = startOff,
                             segment_end = endOff,
                             segment_text = segment
                             )
    
    res <- DBI::dbWriteTable(con, "segments", segment_df, append = TRUE)
    
    # if(res){
    #     log_add_document_record(con, project_id, document_df)    
    # }else{
    #     warning("document not added")
    # }
    
}

