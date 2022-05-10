load_all_segments_db <- function(active_project, project_db) {
    code_id <- segment_start <- segment_end <- NULL
    if (isTruthy(active_project)) {
        
        con <- DBI::dbConnect(RSQLite::SQLite(),
                              project_db)
        on.exit(DBI::dbDisconnect(con))
        
        segments <- dplyr::tbl(con, "segments") %>%
            dplyr::filter(.data$project_id == as.integer(active_project)) %>%
            dplyr::select(user_id, 
                          code_id,
                          doc_id, 
                          segment_start,
                          segment_end) %>%
            dplyr::collect()
        
        return(segments)
    } else {""}
}
