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

# TODO: users table & user_attributes
CREATE_LOG_SQL <- "
CREATE TABLE if not exists logs 
(   user TEXT
,   project_id INTEGER
,   action TEXT
,   payload JSON
,   created_at TEXT
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

CREATE_CODES_SQL <- "
CREATE TABLE if not exists codes (
    project_id INTEGER
,   code_id INTEGER PRIMARY KEY
,   code_name TEXT UNIQUE
)"

CREATE_SEGMENTS_SQL <- "
CREATE TABLE if not exists segments (
    project_id INTEGER
,   doc_id INTEGER
,   code_id INTEGER
,   segment_start INTEGER
,   segment_end INTEGER
,   segment_text TEXT
,   FOREIGN KEY(project_id) REFERENCES projects(project_id)     
,   FOREIGN KEY(doc_id) REFERENCES documents(doc_id)
,   FOREIGN KEY(code_id) REFERENCES codes(code_id)
)"

# TODO: memos
# TODO: memos_documents_map, memos_codes_map, memos_segments_map
# TODO: code_categories

create_db_schema.SQLiteConnection <- function(con){
    # TODO: Full DB structure
    DBI::dbExecute(con, CREATE_PROJECT_SQL)
    DBI::dbExecute(con, CREATE_LOG_SQL)
    DBI::dbExecute(con, CREATE_DOCUMENTS_SQL)
    DBI::dbExecute(con, CREATE_CODES_SQL)
    DBI::dbExecute(con, CREATE_SEGMENTS_SQL)
}

create_db_schema.PqConnection <- function(con){
    # TODO: Full DB structure
    # TODO: optimize sql for Postgres (e.g. created_at fields as date type)
    DBI::dbExecute(con, CREATE_PROJECT_SQL)
    DBI::dbExecute(con, CREATE_LOG_SQL)
    DBI::dbExecute(con, CREATE_DOCUMENTS_SQL)
    DBI::dbExecute(con, CREATE_CODES_SQL)
    DBI::dbExecute(con, CREATE_SEGMENTS_SQL)
}

create_log_df <- function(project_id, action, payload){
    tibble::tibble(
        user = Sys.info()["user"], 
        project_id = project_id, 
        action = action, 
        payload = as.character(jsonlite::toJSON(payload)), 
        created_at = as.character(Sys.time(), usetz = TRUE)
    )
}

log_create_project_record <- function(con, project_id, project_df){
    UseMethod("log_create_project_record")
}

log_create_project_record.SQLiteConnection <- function(con, project_id, project_df){
    log_record_df <- create_log_df(project_id, 
                                   action = "Create project", 
                                   payload = project_df)
    DBI::dbWriteTable(con, "logs", log_record_df, append = TRUE)
}

log_create_project_record.PqConnection <- function(con, project_id, project_df){
    log_record_df <- create_log_df(project_id, 
                                   action = "Create project", 
                                   payload = project_df)
    DBI::dbWriteTable(con, "logs", log_record_df, append = TRUE)
}

# Define project_name as global variable to pass R CMD Check without notes
utils::globalVariables(c("project_name"))

create_project_record <- function(con, project_df){
    DBI::dbWriteTable(con, "projects", project_df, append = TRUE)
    project_id <- dplyr::tbl(con, "projects") %>% 
        dplyr::filter(project_name == !!project_df$project_name) %>%
        dplyr::pull(project_id)
    
    log_create_project_record(con, project_id, project_df)
}