# Load segments for analysis -------------------------------------------

load_segments_analysis <- function(project_db, 
                                  active_project, 
                                  selected_codes) {
   
    if (isTruthy(selected_codes)) {
        
        
        con <- DBI::dbConnect(RSQLite::SQLite(),
                              project_db)
        on.exit(DBI::dbDisconnect(con))
        
        segments <- dplyr::tbl(con, "segments") %>%
            dplyr::filter(.data$project_id == as.integer(active_project)) %>%
            dplyr::filter(code_id  %in% !! as.integer(selected_codes)) %>%
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