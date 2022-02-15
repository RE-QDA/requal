# Load segments for analysis -------------------------------------------

load_segments_analysis <- function(project_db, 
                                  active_project, 
                                  selected_codes) {
    
    selected_codes <- segment_start <- segment_end <- NULL
    if (isTruthy(selected_codes)) {
        browser()
        
        con <- DBI::dbConnect(RSQLite::SQLite(),
                              project_db)
        on.exit(DBI::dbDisconnect(con))
        
        segments <- dplyr::tbl(con, "segments") %>%
            dplyr::filter(.data$project_id == as.integer(active_project)) %>%
            dplyr::filter(.data$doc_id %in% as.integer(.env$selected_codes)) %>%
            dplyr::select(code_id,
                          doc_id,
                          segment_id,
                          segment_text,
                          segment_start,
                          segment_end
                          ) %>%
            dplyr::collect()
        
        
        return(segments)
        
        
    } else {""}
    
}