create_db_schema <- function(con){
    UseMethod("create_db_schema")
}

CREATE_PROJECT_SQL <- "
create table if not exists projects 
(    project_id INTEGER PRIMARY KEY
,    project_name TEXT
,    project_description TEXT
,    created_at TEXT
)"

CREATE_LOG_SQL <- "
CREATE TABLE if not exists logs (
--    user_id INTEGER
    user TEXT
,   project_id INTEGER
-- ,   action_type TEXT
,   action TEXT
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

create_db_schema.SQLiteConnection <- function(con){
    DBI::dbExecute(con, CREATE_PROJECT_SQL)
    DBI::dbExecute(con, CREATE_LOG_SQL)
    DBI::dbExecute(con, CREATE_DOCUMENTS_SQL)
}


create_db_schema.PqConnection <- function(con){
    # TODO:
    DBI::dbExecute(con, CREATE_PROJECT_SQL)
    DBI::dbExecute(con, CREATE_LOG_SQL)
    DBI::dbExecute(con, CREATE_DOCUMENTS_SQL)
}

log_create_project_record <- function(con, project_id, project_df){
    log_record_df <- tibble::tibble(
        user = Sys.info()["user"], 
        project_id = project_id, 
        action = "Create project", 
        created_at = as.character(Sys.time())
    )
    DBI::dbWriteTable(con, "logs", log_record_df, append = TRUE)
}

create_project_record <- function(con, project_df){
    DBI::dbWriteTable(con, "projects", project_df, append = TRUE)
    project_id <- dplyr::tbl(con, "projects") %>% 
        dplyr::filter(project_name == !!project_df$project_name) %>%
        dplyr::pull(project_id)
    
    log_create_project_record(con, project_id, project_df)
}