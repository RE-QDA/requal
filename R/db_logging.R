create_log_df <- function(project_id, action, payload, user_id){
    tibble::tibble(
        user_id = user_id, # Sys.info()["user"], # TODO: this needs to change
        project_id = project_id, 
        action = action, 
        payload = as.character(jsonlite::toJSON(payload)), 
        created_at = as.character(Sys.time(), usetz = TRUE)
    )
}

log_action <- function(con, action, project_id, data, user_id){
    UseMethod("log_action")
}

log_action.SQLiteConnection <- function(con, action, project_id, data, user_id){
    log_record_df <- create_log_df(project_id, 
                                   user_id = user_id, 
                                   action = action, 
                                   payload = data)
    DBI::dbWriteTable(con, "logs", log_record_df, append = TRUE)
}

log_action.PqConnection <- function(con, action, project_id, data, user_id){
    log_record_df <- create_log_df(project_id, 
                                   user_id = user_id, 
                                   action = action, 
                                   payload = data)
    DBI::dbWriteTable(con, "logs", log_record_df, append = TRUE)
}


log_create_project_record <- function(con, project_id, project_df, user_id){
    log_action(con, 
               user_id = user_id, 
               project_id = project_id, 
               action = "Create project", 
               data = project_df)
}

log_add_document_record <- function(con, project_id, doc, user_id){
    log_action(con, 
               user_id = user_id, 
               project_id = project_id, 
               action = "Add document", 
               data = doc)
}

log_delete_document_record <- function(con, project_id, doc_ids, user_id){
    log_action(con, 
               user_id = user_id, 
               project_id = project_id, 
               action = "Delete documents", 
               data = list(doc_id = doc_ids))
}

log_add_case_record <- function(con, project_id, case_df, user_id){
    log_action(con, 
               user_id = user_id, 
               project_id = project_id, 
               action = "Add case", 
               data = case_df)
}

log_add_case_doc_record <- function(con, project_id, case_doc_map_df, user_id){
    log_action(con, 
               user_id = user_id, 
               project_id = project_id, 
               action = "Add case document link", 
               data = case_doc_map_df)
}

log_add_code_record <- function(con, project_id, code, user_id){
    log_action(con, 
               user_id = user_id, 
               project_id = project_id, 
               action = "Add code", 
               data = code)
}

log_delete_code_record <- function(con, project_id, code_ids, user_id){
    log_action(con, 
               user_id = user_id, 
               project_id = project_id, 
               action = "Delete codes", 
               data = list(code_id = code_ids))
}

log_merge_code_record <- function(con, project_id, from, to, user_id){
    log_action(con, 
               user_id = user_id, 
               project_id = project_id, 
               action = "Merge codes", 
               data = list(merge_from = from, merge_to = to))
}

log_add_segment_record <- function(con, project_id, segment, user_id){
    log_action(con, 
               user_id = user_id, 
               project_id = project_id, 
               action = "Add segment", 
               data = segment)
}

log_delete_segment_record <- function(con, project_id, segment, user_id){
    log_action(con, 
               user_id = user_id, 
               project_id = project_id, 
               action = "Delete segment", 
               data = list(segment_id = segment))
}

log_add_category_record <- function(con, project_id, category, user_id){
    log_action(con, 
               user_id = user_id, 
               project_id = project_id, 
               action = "Add category", 
               data = category)
}

log_delete_category_record <- function(con, project_id, category, user_id){
    log_action(con, 
               user_id = user_id, 
               project_id = project_id, 
               action = "Delete category", 
               data = list(category_id = category, project_id = project_id))
}

log_add_category_code_record <- function(con, project_id, df, user_id){
    log_action(con, 
               user_id = user_id, 
               project_id = project_id, 
               action = "Add code to category", 
               data = df)
}

log_delete_category_code_record <- function(con, project_id, df, user_id){
    log_action(con, 
               user_id = user_id, 
               project_id = project_id, 
               action = "Delete code from category", 
               data = df)
}

log_add_memo_record <- function(con, project_id, df, user_id){
    log_action(con, 
               user_id = user_id,
               project_id = project_id, 
               action = "Add memo", 
               data = df)
}

log_update_memo_record <- function(con, project_id, df, user_id){
    log_action(con, 
               user_id = user_id,
               project_id = project_id, 
               action = "Update memo", 
               data = df)
}

log_delete_memo_record <- function(con, project_id, memo_id, user_id){
    log_action(con, 
               user_id = user_id,
               project_id, 
               action = "Delete memo", 
               data = list(memo_id = memo_id))
}
