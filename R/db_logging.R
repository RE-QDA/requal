create_log_df <- function(project_id, action, payload){
    tibble::tibble(
        user = Sys.info()["user"], 
        project_id = project_id, 
        action = action, 
        payload = as.character(jsonlite::toJSON(payload)), 
        created_at = as.character(Sys.time(), usetz = TRUE)
    )
}

log_action <- function(con, action, project_id, data){
    UseMethod("log_action")
}

log_action.SQLiteConnection <- function(con, action, project_id, data){
    log_record_df <- create_log_df(project_id, 
                                   action = action, 
                                   payload = data)
    DBI::dbWriteTable(con, "logs", log_record_df, append = TRUE)
}

log_action.PqConnection <- function(con, action, project_id, project_df){
    log_record_df <- create_log_df(project_id, 
                                   action = action, 
                                   payload = project_df)
    DBI::dbWriteTable(con, "logs", log_record_df, append = TRUE)
}


log_create_project_record <- function(con, project_id, project_df){
    log_action(con, 
               project_id = project_id, 
               action = "Create project", 
               data = project_df)
}

log_add_document_record <- function(con, project_id, doc){
    log_action(con, 
               project_id = project_id, 
               action = "Add documents", 
               data = doc)
}

log_delete_document_record <- function(con, project_id, doc_ids){
    log_action(con, 
               project_id = project_id, 
               action = "Delete documents", 
               data = doc_ids)
}
