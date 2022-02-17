# Load logs for reporting -------------------------------------------

load_logs_for_reporting <- function(project_db, 
                                    active_project) {
    
    con <- DBI::dbConnect(RSQLite::SQLite(),
                          project_db)
    on.exit(DBI::dbDisconnect(con))
    
    logs <- dplyr::tbl(con, "logs") %>%
        dplyr::filter(.data$project_id == as.integer(active_project)) %>%
        dplyr::select(user,
                      action, 
                      payload, 
                      created_at) %>%
        dplyr::collect()
    
    return(logs)
}