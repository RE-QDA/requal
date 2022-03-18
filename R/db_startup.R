create_db_schema <- function(con){
    UseMethod("create_db_schema")
}

CREATE_PROJECT_SQL <- "
CREATE TABLE projects (    
     project_id INTEGER PRIMARY KEY AUTOINCREMENT
,    project_name TEXT
,    project_description TEXT
,    created_at TEXT DEFAULT CURRENT_TIMESTAMP
);
"

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
,   user_mail TEXT
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
);
"

CREATE_DOCUMENTS_SQL <- "
CREATE TABLE if not exists documents (
    doc_id INTEGER PRIMARY KEY AUTOINCREMENT
,   project_id INTEGER
,   doc_name TEXT
,   doc_description TEXT
,   doc_text TEXT
,   created_at TEXT DEFAULT CURRENT_TIMESTAMP
,   FOREIGN KEY(project_id) REFERENCES projects(project_id)     
);
"

CREATE_CODES_SQL <- "
CREATE TABLE if not exists codes (
    project_id INTEGER
,   code_id INTEGER PRIMARY KEY AUTOINCREMENT
,   code_name TEXT UNIQUE
,   code_description TEXT
,   code_color TEXT
,   FOREIGN KEY(project_id) REFERENCES projects(project_id)
);
"

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
);
"

CREATE_CATEGORIES_SQL <- "
CREATE TABLE if not exists categories (
    project_id INTEGER
,   category_id INTEGER PRIMARY KEY AUTOINCREMENT
,   category_name TEXT
,   category_description TEXT
,   FOREIGN KEY(project_id) REFERENCES projects(project_id)     
);
"

CREATE_CATEGORY_CODE_MAP_SQL <- "
CREATE TABLE if not exists category_code_map (
    project_id INTEGER
,   category_id INTEGER
,   code_id INTEGER
,   FOREIGN KEY(project_id) REFERENCES projects(project_id)     
,   FOREIGN KEY(category_id) REFERENCES categories(category_id)     
,   FOREIGN KEY(code_id) REFERENCES codes(codes_id)     
);
"

CREATE_CASES_SQL <- "
CREATE TABLE if not exists cases (
    project_id INTEGER
,   case_id INTEGER PRIMARY KEY AUTOINCREMENT
,   case_name TEXT
,   case_description TEXT
,   FOREIGN KEY(project_id) REFERENCES projects(project_id)
);
"

CREATE_CASE_DOC_MAP_SQL <- "
CREATE TABLE if not exists case_document_map (
    project_id INTEGER
,   case_id INTEGER
,   doc_id INTEGER
,   FOREIGN KEY(project_id) REFERENCES projects(project_id)
,   FOREIGN KEY(case_id) REFERENCES cases(case_id)
,   FOREIGN KEY(doc_id) REFERENCES documents(doc_id)
);
" 

# TODO: hierarchy between codes, cases, categories (code_code_map, case_case_map, category_category_map)

# memos
CREATE_MEMO_SQL <- "
CREATE TABLE if not exists memos (
    memo_id INTEGER PRIMARY KEY
,   text TEXT
)"

CREATE_MEMO_DOCUMENT_MAP_SQL <- "
CREATE TABLE if not exists memos_documents_map (
    memo_id INTEGER
,   doc_id INTEGER
,   FOREIGN KEY(doc_id) REFERENCES documents(doc_id)
,   FOREIGN KEY(memo_id) REFERENCES memos(memo_id)
)"

CREATE_MEMO_CODE_MAP_SQL <- "
CREATE TABLE if not exists memos_codes_map (
    memo_id INTEGER
,   code_id INTEGER
,   FOREIGN KEY(code_id) REFERENCES codes(code_id)
,   FOREIGN KEY(memo_id) REFERENCES memos(memo_id)
)"

# memos
CREATE_MEMO_SQL <- "
CREATE TABLE if not exists memos (
    memo_id INTEGER PRIMARY KEY
,   text TEXT
)"

CREATE_MEMO_DOCUMENT_MAP_SQL <- "
CREATE TABLE if not exists memos_documents_map (
    memo_id INTEGER
,   doc_id INTEGER
,   FOREIGN KEY(doc_id) REFERENCES documents(doc_id)
,   FOREIGN KEY(memo_id) REFERENCES memos(memo_id)
)"

CREATE_MEMO_CODE_MAP_SQL <- "
CREATE TABLE if not exists memos_codes_map (
    memo_id INTEGER
,   code_id INTEGER
,   FOREIGN KEY(code_id) REFERENCES codes(code_id)
,   FOREIGN KEY(memo_id) REFERENCES memos(memo_id)
)"

CREATE_MEMO_SEGMENT_MAP_SQL <- "
CREATE TABLE if not exists memos_segments_map (
    memo_id INTEGER
,   segment_id INTEGER
,   FOREIGN KEY(segment_id) REFERENCES segments(segment_id)
,   FOREIGN KEY(memo_id) REFERENCES memos(memo_id)
)"


create_db_schema.default <- function(con){
    # TODO: Full DB structure
    DBI::dbExecute(con, CREATE_PROJECT_SQL)
    DBI::dbExecute(con, CREATE_REQUAL_INFO_SQL)
    DBI::dbExecute(con, CREATE_USERS_SQL)
    DBI::dbExecute(con, CREATE_USER_PERMISSIONS_SQL)
    DBI::dbExecute(con, CREATE_LOG_SQL)
    DBI::dbExecute(con, CREATE_DOCUMENTS_SQL)
    DBI::dbExecute(con, CREATE_CODES_SQL)
    DBI::dbExecute(con, CREATE_CATEGORIES_SQL)
    DBI::dbExecute(con, CREATE_CATEGORY_CODE_MAP_SQL)
    DBI::dbExecute(con, CREATE_CASES_SQL)
    DBI::dbExecute(con, CREATE_CASE_DOC_MAP_SQL)
    DBI::dbExecute(con, CREATE_SEGMENTS_SQL)
    DBI::dbExecute(con, CREATE_MEMO_SQL)
    DBI::dbExecute(con, CREATE_MEMO_DOCUMENT_MAP_SQL)
    DBI::dbExecute(con, CREATE_MEMO_CODE_MAP_SQL)
    DBI::dbExecute(con, CREATE_MEMO_SEGMENT_MAP_SQL)
}

create_db_schema.SQLiteConnection <- function(con){
    # TODO: Full DB structure
    DBI::dbExecute(con, CREATE_PROJECT_SQL)
    DBI::dbExecute(con, CREATE_REQUAL_INFO_SQL)
    DBI::dbExecute(con, CREATE_USERS_SQL)
    DBI::dbExecute(con, CREATE_USER_PERMISSIONS_SQL)
    DBI::dbExecute(con, CREATE_LOG_SQL)
    DBI::dbExecute(con, CREATE_DOCUMENTS_SQL)
    DBI::dbExecute(con, CREATE_CODES_SQL)
    DBI::dbExecute(con, CREATE_CATEGORIES_SQL)
    DBI::dbExecute(con, CREATE_CATEGORY_CODE_MAP_SQL)
    DBI::dbExecute(con, CREATE_CASES_SQL)
    DBI::dbExecute(con, CREATE_CASE_DOC_MAP_SQL)
    DBI::dbExecute(con, CREATE_SEGMENTS_SQL)
    DBI::dbExecute(con, CREATE_MEMO_SQL)
    DBI::dbExecute(con, CREATE_MEMO_DOCUMENT_MAP_SQL)
    DBI::dbExecute(con, CREATE_MEMO_CODE_MAP_SQL)
    DBI::dbExecute(con, CREATE_MEMO_SEGMENT_MAP_SQL)
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
    DBI::dbExecute(con, CREATE_CATEGORIES_SQL)
    DBI::dbExecute(con, CREATE_CATEGORY_CODE_MAP_SQL)
    DBI::dbExecute(con, CREATE_CASES_SQL)
    DBI::dbExecute(con, CREATE_CASE_DOC_MAP_SQL)
    DBI::dbExecute(con, CREATE_SEGMENTS_SQL)
    DBI::dbExecute(con, CREATE_MEMO_SQL)
    DBI::dbExecute(con, CREATE_MEMO_DOCUMENT_MAP_SQL)
    DBI::dbExecute(con, CREATE_MEMO_CODE_MAP_SQL)
    DBI::dbExecute(con, CREATE_MEMO_SEGMENT_MAP_SQL)
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
        written_document_id <- dplyr::tbl(con, "documents") %>% 
            dplyr::filter(.data$doc_name == !!document_df$doc_name &
                          .data$doc_description == !!document_df$doc_description & 
                          .data$doc_text == !!document_df$doc_text &
                          .data$project_id == project_id) %>% 
            dplyr::pull(doc_id)
        log_add_document_record(con, project_id, document_df %>% 
                                    dplyr::mutate(doc_id = written_document_id))    
    }else{
        warning("document not added")
    }
}

add_codes_record <- function(con, project_id, codes_df){
    res <- DBI::dbWriteTable(con, "codes", codes_df, append = TRUE)
    if(res){
        written_code_id <- dplyr::tbl(con, "codes") %>% 
            dplyr::filter(.data$code_name == !!codes_df$code_name &
                          .data$code_description == !!codes_df$code_description &
                          .data$project_id == project_id) %>% 
            dplyr::pull(code_id)
        log_add_code_record(con, project_id, codes_df %>% 
                                dplyr::mutate(code_id = written_code_id))    
    }else{
        warning("code not added")
    }
}