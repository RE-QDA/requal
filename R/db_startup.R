create_db_schema <- function(con){
    UseMethod("create_db_schema")
}

CREATE_PROJECT_SQL <- "
CREATE TABLE projects 
(    project_id INTEGER PRIMARY KEY AUTOINCREMENT
,    project_name TEXT
,    project_description TEXT
,    created_at TEXT DEFAULT CURRENT_TIMESTAMP
)"

CREATE_REQUAL_INFO_SQL <- "
CREATE TABLE if not exists requal_version (
    project_id INTEGER
,   version TEXT
,   FOREIGN KEY(project_id) REFERENCES projects(project_id)     
);
"

# TODO: user_attributes
CREATE_USERS_SQL <- "
CREATE TABLE if not exists users (
    user_id INTEGER PRIMARY KEY
,   user_name TEXT
-- ,   user_mail TEXT
,   created_at TEXT DEFAULT CURRENT_TIMESTAMP
);
"

CREATE_USER_PERMISSIONS_SQL <- "
CREATE TABLE if not exists user_permissions (
    user_id INTEGER
,   project_id INTEGER
,   can_code INTEGER
,   can_modify_codes INTEGER
,   can_delete_codes INTEGER
,   can_modify_documents INTEGER
,   can_delete_documents INTEGER
,   can_manage INTEGER
,   FOREIGN KEY(user_id) REFERENCES users(user_id)
,   FOREIGN KEY(project_id) REFERENCES projects(project_id)     
);
"

CREATE_LOG_SQL <- "
CREATE TABLE if not exists logs 
(   user TEXT
,   project_id INTEGER
,   action TEXT
,   payload JSON
,   created_at TEXT DEFAULT CURRENT_TIMESTAMP
,   FOREIGN KEY(project_id) REFERENCES projects(project_id)     
)"

CREATE_DOCUMENTS_SQL <- "
CREATE TABLE if not exists documents (
    doc_id INTEGER PRIMARY KEY AUTOINCREMENT
,   project_id INTEGER
,   doc_name TEXT
,   doc_description TEXT
,   doc_text TEXT
,   created_at TEXT DEFAULT CURRENT_TIMESTAMP
,   FOREIGN KEY(project_id) REFERENCES projects(project_id)     
)"

CREATE_CODES_SQL <- "
CREATE TABLE if not exists codes (
    project_id INTEGER
,   code_id INTEGER PRIMARY KEY AUTOINCREMENT
,   code_name TEXT UNIQUE
,   code_description TEXT
,   code_color TEXT
)"

CREATE_SEGMENTS_SQL <- "
CREATE TABLE if not exists segments (
    project_id INTEGER
,   doc_id INTEGER
,   code_id INTEGER
,   segment_id INTEGER PRIMARY KEY AUTOINCREMENT
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
    DBI::dbExecute(con, CREATE_REQUAL_INFO_SQL)
    DBI::dbExecute(con, CREATE_USERS_SQL)
    DBI::dbExecute(con, CREATE_USER_PERMISSIONS_SQL)
    DBI::dbExecute(con, CREATE_LOG_SQL)
    DBI::dbExecute(con, CREATE_DOCUMENTS_SQL)
    DBI::dbExecute(con, CREATE_CODES_SQL)
    DBI::dbExecute(con, CREATE_SEGMENTS_SQL)
}

create_db_schema.PqConnection <- function(con){
    # TODO: Full DB structure
    # TODO: optimize sql for Postgres (e.g. created_at fields as date type)
    DBI::dbExecute(con, CREATE_PROJECT_SQL)
    DBI::dbExecute(con, CREATE_REQUAL_INFO_SQL)
    DBI::dbExecute(con, CREATE_USERS_SQL)
    DBI::dbExecute(con, CREATE_USER_PERMISSIONS_SQL)
    DBI::dbExecute(con, CREATE_LOG_SQL)
    DBI::dbExecute(con, CREATE_DOCUMENTS_SQL)
    DBI::dbExecute(con, CREATE_CODES_SQL)
    DBI::dbExecute(con, CREATE_SEGMENTS_SQL)
}

create_default_user <- function(con, project_id){
    user_df <- tibble::tibble(
        user_name = Sys.info()["user"]
    )
    
    DBI::dbWriteTable(con, "users", user_df, append = TRUE)
    user_df_stored <- dplyr::tbl(con, "users") %>% 
        dplyr::filter(.data$user_name == !!user_df$user_name) %>% 
        dplyr::collect()
    
    user_permission_df <- tibble::tibble(
        user_id = user_df_stored$user_id, 
        project_id = project_id, 
        can_code = 1, 
        can_modify_codes = 1,
        can_delete_codes = 1,
        can_modify_documents = 1,
        can_delete_documents = 1,
        can_manage = 1
    )
    
    DBI::dbWriteTable(con, "user_permissions", user_permission_df, append = TRUE)
}

create_project_record <- function(con, project_df){
    res <- DBI::dbWriteTable(con, "projects", project_df, append = TRUE)
    project_id <- dplyr::tbl(con, "projects") %>% 
        dplyr::filter(project_name == !!project_df$project_name) %>%
        dplyr::pull(project_id)
    
    if(res){
        log_create_project_record(con, project_id, project_df)   
    }
    
    requal_version_df <- data.frame(
        project_id = project_id,
        version = as.character(packageVersion("requal"))
    )
    res_v <- DBI::dbWriteTable(con, "requal_version", requal_version_df, append = TRUE)
    
    create_default_user(con, project_id)
}

add_documents_record <- function(con, project_id, document_df){
    res <- DBI::dbWriteTable(con, "documents", document_df, append = TRUE)
    if(res){
        log_add_document_record(con, project_id, document_df)    
    }else{
        warning("document not added")
    }
}

add_codes_record <- function(con, project_id, codes_df){
    res <- DBI::dbWriteTable(con, "codes", codes_df, append = TRUE)
    if(res){
        log_add_code_record(con, project_id, codes_df)    
    }else{
        warning("code not added")
    }
}