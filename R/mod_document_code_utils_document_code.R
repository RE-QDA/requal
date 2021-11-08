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

# load documents to display  -------------------------------------------

load_doc_db <- function(active_project, project_db, doc_id) {
    
    if (isTruthy(active_project)) {
        
        con <- DBI::dbConnect(RSQLite::SQLite(),
                              project_db)
        on.exit(DBI::dbDisconnect(con))
        
        doc_text <- dplyr::tbl(con, "documents") %>%
            dplyr::filter(.data$project_id == as.integer(active_project)) %>%
            dplyr::filter(.data$doc_id == as.integer(.env$doc_id)) %>% 
            dplyr::pull(doc_text)
            
            
        
        return(doc_text)
        
        
    } else {""}
    
}

# load segments for document display  -------------------------------------------

load_segments_db <- function(active_project, project_db, doc_id) {
    
    if (isTruthy(active_project)) {
        
        con <- DBI::dbConnect(RSQLite::SQLite(),
                              project_db)
        on.exit(DBI::dbDisconnect(con))
        
        segments <- dplyr::tbl(con, "segments") %>%
            dplyr::filter(.data$project_id == as.integer(active_project)) %>%
            dplyr::filter(.data$doc_id == as.integer(.env$doc_id)) %>% 
            dplyr::select(code_id,
                          segment_start,
                          segment_end) %>% 
            dplyr::collect()
        
        
        return(segments)
        
        
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

load_doc_to_display <- function(active_project, project_db, doc_selector){
    raw_text <- load_doc_db(active_project, project_db, doc_selector)
    raw_segments <- load_segments_db(active_project, project_db, doc_selector) 
    
    df <- strsplit(raw_text, "")[[1]] %>% tibble::enframe()
    
    df2 <- df %>% dplyr::left_join(raw_segments %>% 
                                       dplyr::select(code_id,segment_start), 
                                   by=c("name" = "segment_start")) %>% 
        dplyr::left_join(raw_segments %>% dplyr::select(code_end=code_id,segment_end), by=c("name" = "segment_end")) %>% 
        dplyr::mutate(code_end = ifelse(!is.na(code_end), "</mark>", ""),
                      code_id = ifelse(!is.na(code_id), paste0('<mark id="', code_id, '">'), ""))
    
    paste0(df2$code_id, df2$value, df2$code_end, collapse = "")
}


