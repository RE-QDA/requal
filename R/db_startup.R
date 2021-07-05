#' @export
create_db_schema <- function(con){
    UseMethod("create_db_schema")
}

CREATE_PROJECT_SQL <- "
CREATE TABLE projects 
(    project_id INTEGER PRIMARY KEY
,    project_name TEXT
,    project_description TEXT
,    created_at TEXT
)"

CREATE_LOG_SQL <- "
CREATE TABLE logs 
(   user TEXT
,   project_id INTEGER
,   action TEXT
,   payload TEXT
,   created_at TEXT
,   FOREIGN KEY(project_id) REFERENCES projects(project_id)     
)"

CREATE_LOG_SQL_POSTGRES <- "
CREATE TABLE if not exists logs (
--    user_id INTEGER
    user TEXT
,   project_id INTEGER
-- ,   action_type TEXT
,   action TEXT
,   payload JSON
,   created_at TEXT
-- ,   FOREIGN KEY(user_id) REFERENCES users(user_id)
,   FOREIGN KEY(project_id) REFERENCES projects(project_id)     
)"

CREATE_DOCUMENTS_SQL <- "
CREATE TABLE if not exists documents (
    doc_id INTEGER PRIMARY KEY
,   project_id INTEGER
,   doc_text TEXT
,   created_at TEXT
,   FOREIGN KEY(project_id) REFERENCES projects(project_id)     
)"

#' @export
create_db_schema.SQLiteConnection <- function(con){
    # TODO: Full DB structure
    DBI::dbExecute(con, CREATE_PROJECT_SQL)
    DBI::dbExecute(con, CREATE_LOG_SQL)
    DBI::dbExecute(con, CREATE_DOCUMENTS_SQL)
}

#' @export
create_db_schema.PqConnection <- function(con){
    # TODO: Full DB structure
    DBI::dbExecute(con, CREATE_PROJECT_SQL)
    DBI::dbExecute(con, CREATE_LOG_SQL_POSTGRES)
    DBI::dbExecute(con, CREATE_DOCUMENTS_SQL)
}

#' @export
log_create_project_record <- function(con, project_id, project_df){
    UseMethod("log_create_project_record")
}

#' @export
log_create_project_record.SQLiteConnection <- function(con, project_id, project_df){
    log_record_df <- tibble::tibble(
        user = Sys.info()["user"], 
        project_id = project_id, 
        action = "Create project", 
        payload = as.character(jsonlite::toJSON(project_df)),
        created_at = as.character(Sys.time())
    )
    DBI::dbWriteTable(con, "logs", log_record_df, append = TRUE)
}

#' @export
log_create_project_record.PqConnection <- function(con, project_id, project_df){
    log_record_df <- tibble::tibble(
        user = Sys.info()["user"], 
        project_id = project_id, 
        action = "Create project", 
        payload = jsonlite::toJSON(project_df),
        created_at = as.character(Sys.time())
    )
    DBI::dbWriteTable(con, "logs", log_record_df, append = TRUE)
}

#' @export
create_project_record <- function(con, project_df){
    DBI::dbWriteTable(con, "projects", project_df, append = TRUE)
    project_id <- dplyr::tbl(con, "projects") %>% 
        dplyr::filter(project_name == !!project_df$project_name) %>%
        dplyr::pull(project_id)
    
    log_create_project_record(con, project_id, project_df)
}