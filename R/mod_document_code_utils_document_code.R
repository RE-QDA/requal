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

load_doc <- function(con, project_id, doc_id){
    dplyr::tbl(con, "documents") %>%
        dplyr::filter(.data$project_id == as.integer(.env$project_id)) %>%
        dplyr::filter(.data$doc_id == as.integer(.env$doc_id)) %>% 
        dplyr::pull(doc_text)
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

# Check overlap between already coded segments and newly coded segment
# using its start and end position
check_overlap <- function(coded_segments, startOff, endOff){
    if(!nrow(coded_segments)){
        return(data.frame())
    }
    
    overlapping_segments <- coded_segments %>%
        dplyr::rowwise() %>%
        dplyr::mutate(overlapping = dplyr::between(startOff, segment_start, segment_end) | 
                          dplyr::between(endOff, segment_start, segment_end)) %>%
        dplyr::filter(overlapping)
    
    if(!nrow(overlapping_segments)){
        return(data.frame())
    }
    
    overlapping_segments
}

get_segment_text <- function(con, project_id, doc_id, start, end){
    load_doc(con, project_id, doc_id) %>%
        substr(., start, end)
}

# Write document segments to DB -------------------------------------------

write_segment_db <- function(
    active_project,
    project_db,
    doc_id,
    code_id,
    # segment,
    startOff,
    endOff
) {
    
    con <- DBI::dbConnect(RSQLite::SQLite(),
                          project_db)
    on.exit(DBI::dbDisconnect(con))
    
    # Check overlap
    # - return segments with the same document ID and code ID from the DB
    coded_segments <- dplyr::tbl(con, "segments") %>%
        dplyr::filter(.data$project_id == as.integer(active_project)) %>%
        dplyr::filter(.data$doc_id == as.integer(.env$doc_id)) %>% 
        dplyr::filter(.data$code_id == as.integer(.env$code_id)) %>%
        dplyr::select(project_id, doc_id, code_id,
                      segment_start,
                      segment_end) %>% 
        dplyr::collect()
    # - check overlap
    overlap <- check_overlap(coded_segments, startOff, endOff)
    
    if(nrow(overlap)){
        # update existing record(s)
        segment_df <- overlap %>%
            dplyr::group_by(project_id, doc_id, code_id) %>%
            dplyr::summarise(segment_start = min(startOff, segment_start), 
                             segment_end = max(endOff, segment_end)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(segment_text = get_segment_text(con, 
                                                   project_id = active_project, 
                                                   doc_id, 
                                                   .$segment_start, 
                                                   .$segment_end))
        
            
        # delete_segments that are to be replaced by segment_df
        query <- glue::glue_sql("DELETE FROM segments 
                       WHERE project_id = {active_project} 
                       AND doc_id = {doc_id} 
                       AND code_id = {code_id} 
                       AND segment_start = {overlap$segment_start}
                       AND segment_end = {overlap$segment_end}", .con = con)
        
        purrr::walk(query, function(x) {DBI::dbExecute(con, x)})
        
        res <- DBI::dbWriteTable(con, "segments", segment_df, append = TRUE)
        
    }else{
        # in case of no overlap write in DB directly
        segment_df <- data.frame(project_id = active_project,
                                 doc_id = doc_id,
                                 code_id = code_id,
                                 segment_start = startOff,
                                 segment_end = endOff,
                                 segment_text = get_segment_text(con, 
                                                                 project_id = active_project, 
                                                                 doc_id, 
                                                                 startOff,
                                                                 endOff)
        )
        
        res <- DBI::dbWriteTable(con, "segments", segment_df, append = TRUE)
    }
    
    
    # TODO: logging
    
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


