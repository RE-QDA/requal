read_doc_db <- function(active_project, project_db) {
    
    if (isTruthy(active_project)) {
        
        con <- DBI::dbConnect(RSQLite::SQLite(),
                              project_db)
        on.exit(DBI::dbDisconnect(con))
        
        db_docs_df <- dplyr::tbl(con, "documents") %>%
            dplyr::filter(.data$project_id == as.integer(.env$project_id)) %>%
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