
db_call <- c(

"attributes" =
"
CREATE TABLE if not exists attributes (
    attribute_id INTEGER PRIMARY KEY AUTOINCREMENT
,   attribute_name TEXT
,   attribute_object TEXT
,   attribute_type TEXT
,   user_id INTEGER
,   FOREIGN KEY(user_id) REFERENCES users(user_id)
);
"
",

"attributes_users_map" =
"
CREATE TABLE if not exists attributes_users_map (
    user_id INTEGER
,   attribute_id INTEGER
,   attribute_value_id INTEGER 
,   FOREIGN KEY(user_id) REFERENCES users(user_id)
,   FOREIGN KEY(attribute_id) REFERENCES attributes(attribute_id)
,   FOREIGN KEY(attribute_value_id) REFERENCES attribute_values(attribute_value_id)
);
",

"attribute_values" =
"
CREATE TABLE if not exists attribute_values (
    attribute_value_id INTEGER PRIMARY KEY AUTOINCREMENT
,   attribute_id INTEGER
,   value TEXT
,   FOREIGN KEY(attribute_id) REFERENCES attributes(attribute_id)
);
",

"cases" =
"
CREATE TABLE if not exists cases (
    project_id INTEGER
,   case_id INTEGER PRIMARY KEY AUTOINCREMENT
,   case_name TEXT
,   case_description TEXT
,   FOREIGN KEY(project_id) REFERENCES projects(project_id)
);
",


"cases_documents_map" =
"
CREATE TABLE if not exists cases_documents_map (
    project_id INTEGER
,   case_id INTEGER
,   doc_id INTEGER
,   FOREIGN KEY(project_id) REFERENCES projects(project_id)
,   FOREIGN KEY(case_id) REFERENCES cases(case_id)
,   FOREIGN KEY(doc_id) REFERENCES documents(doc_id)
);
",

"categories" =
"
CREATE TABLE if not exists categories (
    project_id INTEGER
,   category_id INTEGER PRIMARY KEY AUTOINCREMENT
,   category_name TEXT
,   category_description TEXT
,   FOREIGN KEY(project_id) REFERENCES projects(project_id)
);
",

"categories_codes_map" =
"
CREATE TABLE if not exists categories_codes_map (
    project_id INTEGER
,   category_id INTEGER
,   code_id INTEGER
,   FOREIGN KEY(project_id) REFERENCES projects(project_id)
,   FOREIGN KEY(category_id) REFERENCES categories(category_id)
,   FOREIGN KEY(code_id) REFERENCES codes(code_id)
);
",

"codes" =
"
CREATE TABLE if not exists codes (
    project_id INTEGER
,   code_id INTEGER PRIMARY KEY AUTOINCREMENT
,   code_name TEXT UNIQUE
,   code_description TEXT
,   code_color TEXT
,   FOREIGN KEY(project_id) REFERENCES projects(project_id)
);
",

"documents" =
"
CREATE TABLE if not exists documents (
    doc_id INTEGER PRIMARY KEY AUTOINCREMENT
,   project_id INTEGER
,   doc_name TEXT
,   doc_description TEXT
,   doc_text TEXT
,   created_at TEXT DEFAULT CURRENT_TIMESTAMP
,   FOREIGN KEY(project_id) REFERENCES projects(project_id)
);
",

"logs" =
"
CREATE TABLE if not exists logs
(   user_id INTEGER
,   project_id INTEGER
,   action TEXT
,   payload JSON
,   created_at TEXT DEFAULT CURRENT_TIMESTAMP
,   FOREIGN KEY(project_id) REFERENCES projects(project_id)
,   FOREIGN KEY(user_id) REFERENCES users(user_id)
);
",

"memos" =
"
CREATE TABLE if not exists memos (
    project_id INTEGER
,   memo_id INTEGER PRIMARY KEY
,   text TEXT
,   FOREIGN KEY(project_id) REFERENCES projects(project_id)
);
",

"memos_codes_map" =
"
CREATE TABLE if not exists memos_codes_map (
    memo_id INTEGER
    ,   code_id INTEGER
    ,   FOREIGN KEY(code_id) REFERENCES codes(code_id)
    ,   FOREIGN KEY(memo_id) REFERENCES memos(memo_id)
);
",

"memos_documents_map" =
"
CREATE TABLE if not exists memos_documents_map (
    memo_id INTEGER
    ,   doc_id INTEGER
    ,   memo_start INTEGER
    ,   memo_end INTEGER
    ,   FOREIGN KEY(doc_id) REFERENCES documents(doc_id)
    ,   FOREIGN KEY(memo_id) REFERENCES memos(memo_id)
);
",

"memos_segments_map" =
"
CREATE TABLE if not exists memos_segments_map (
    memo_id INTEGER
    ,   segment_id INTEGER
    ,   FOREIGN KEY(segment_id) REFERENCES segments(segment_id)
    ,   FOREIGN KEY(memo_id) REFERENCES memos(memo_id)
);
",

"projects" =
"
CREATE TABLE projects (
     project_id INTEGER PRIMARY KEY AUTOINCREMENT
,    project_name TEXT
,    project_description TEXT
,    created_at TEXT DEFAULT CURRENT_TIMESTAMP
);
",

"requal_version" = 
"
CREATE TABLE if not exists requal_version (
    project_id INTEGER
,   version TEXT
,   FOREIGN KEY(project_id) REFERENCES projects(project_id)
);
",

"segments" =
"
CREATE TABLE if not exists segments (
    project_id INTEGER
,   user_id INTEGER
,   doc_id INTEGER
,   code_id INTEGER
,   segment_id INTEGER PRIMARY KEY AUTOINCREMENT
,   segment_start INTEGER
,   segment_end INTEGER
,   segment_text TEXT
,   FOREIGN KEY(project_id) REFERENCES projects(project_id)
,   FOREIGN KEY(doc_id) REFERENCES documents(doc_id)
,   FOREIGN KEY(code_id) REFERENCES codes(code_id)
,   FOREIGN KEY(user_id) REFERENCES users(user_id)
);
",

"users" =
"
CREATE TABLE if not exists users (
    user_id INTEGER PRIMARY KEY
,   user_login TEXT UNIQUE
,   user_name TEXT
,   user_mail TEXT
,   created_at TEXT DEFAULT CURRENT_TIMESTAMP
);
",

"user_permissions" =
"
CREATE TABLE if not exists user_permissions (
    user_id INTEGER
,   project_id INTEGER
,   data_modify INTEGER            
,   data_other_modify INTEGER     
,   data_other_view INTEGER       
,   attributes_modify INTEGER      
,   attributes_other_modify INTEGER
,   attributes_other_view INTEGER  
,   codebook_modify INTEGER        
,   codebook_other_modify INTEGER  
,   codebook_other_view INTEGER    
,   annotation_modify INTEGER     
,   annotation_other_modify INTEGER
,   annotation_other_view INTEGER
,   analysis_other_view INTEGER    
,   report_other_view INTEGER      
,   permissions_modify INTEGER     
,   FOREIGN KEY(user_id) REFERENCES users(user_id)
,   FOREIGN KEY(project_id) REFERENCES projects(project_id)
);
"
)

db_call_df_unordered <- tibble::tibble(
  table = names(db_call),
  sql = db_call
)

# Arrange by priority as required by postgres
db_call_df_ordered <- tibble::tibble(
table = c(
"projects", 
"requal_version", 
"users", 
"user_permissions", 
"logs",
"documents", 
"codes", 
"categories", 
"categories_codes_map", 
"cases", 
"cases_documents_map", 
"segments", 
"memos", 
"attributes", 
"attribute_values"
))

db_call_df <- dplyr::full_join(
  db_call_df_ordered,
  db_call_df_unordered,
  by = "table"
)

)

db_call_df_unordered <- tibble::tibble(
  table = names(db_call),
  sql = db_call
)

# Arrange by priority as required by postgres
db_call_df_ordered <- tibble::tibble(
table = c(
"projects", 
"requal_version", 
"users", 
"user_permissions", 
"logs",
"documents", 
"codes", 
"categories", 
"categories_codes_map", 
"cases", 
"cases_documents_map", 
"segments", 
"memos", 
"attributes", 
"attribute_values"
))

db_call_df <- dplyr::full_join(
  db_call_df_ordered,
  db_call_df_unordered,
  by = "table"
)



create_db_schema <- function(pool) {

  db_postgres <- pool::dbGetInfo(pool)$pooledObjectClass != "SQLiteConnection"
  if (db_postgres) {

   psql <- db_call_df %>%
      dplyr::mutate(
        psql =
          stringr::str_replace(
            sql,
            "INTEGER PRIMARY KEY AUTOINCREMENT",
            "SERIAL PRIMARY KEY"
          )
      ) %>%
      dplyr::pull(psql)

   psql <- db_call_df %>%
      dplyr::mutate(
        psql =
          stringr::str_replace(
            sql,
            "INTEGER PRIMARY KEY AUTOINCREMENT",
            "SERIAL PRIMARY KEY"
          )
      ) %>%
      dplyr::pull(psql)

    purrr::walk(psql, ~DBI::dbExecute(pool, psql))
    purrr::walk(psql, ~DBI::dbExecute(pool, psql))
  } else {
    purrr::walk(db_call_df$sql, ~DBI::dbExecute(pool, .x))
    purrr::walk(db_call_df$sql, ~DBI::dbExecute(pool, .x))
  }
}

update_db_schema <- function(pool) {
  existing_tables <- pool::dbListTables(pool)
  existing_tables_no_sqlite <- existing_tables[!grepl("sqlite", existing_tables)]
  missing_tables <- setdiff(db_call_df$table, existing_tables_no_sqlite)
  if (length(missing_tables) > 0) {
    db_postgres <- pool::dbGetInfo(pool)$pooledObjectClass != "SQLiteConnection"
    if (db_postgres) {
      to_create_tables <- db_call_df %>%
        dplyr::filter(table %in% missing_tables) %>%
        dplyr::mutate(
          psql =
            stringr::str_replace(
              sql,
              "INTEGER PRIMARY KEY AUTOINCREMENT",
              "SERIAL PRIMARY KEY"
            )
        )
      purrr::walk(to_create_tables$psql, ~ DBI::dbExecute(pool, .x))
    } else {
    
      to_create_tables <- db_call_df %>%
        dplyr::filter(table %in% missing_tables)

      purrr::walk(to_create_tables$sql, ~ DBI::dbExecute(pool, .x))
    }
    message("Updated reQual schema.")
  } else {
    NULL
  }
}

update_db_schema <- function(pool) {
  existing_tables <- pool::dbListTables(pool)
  existing_tables_no_sqlite <- existing_tables[!grepl("sqlite", existing_tables)]
  missing_tables <- setdiff(db_call_df$table, existing_tables_no_sqlite)
  if (length(missing_tables) > 0) {
    db_postgres <- pool::dbGetInfo(pool)$pooledObjectClass != "SQLiteConnection"
    if (db_postgres) {
      to_create_tables <- db_call_df %>%
        dplyr::filter(table %in% missing_tables) %>%
        dplyr::mutate(
          psql =
            stringr::str_replace(
              sql,
              "INTEGER PRIMARY KEY AUTOINCREMENT",
              "SERIAL PRIMARY KEY"
            )
        )
      purrr::walk(to_create_tables$psql, ~ DBI::dbExecute(pool, .x))
    } else {
    
      to_create_tables <- db_call_df %>%
        dplyr::filter(table %in% missing_tables)

      purrr::walk(to_create_tables$sql, ~ DBI::dbExecute(pool, .x))
    }
    message("Updated reQual schema.")
  } else {
    NULL
  }
}

# Database functions ####

create_default_user <- function(pool, project_id, user_id) {


  if (golem::get_golem_options("mode") == "local") {
    user_df <- tibble::tibble(
      user_name = Sys.info()["user"]
    )
    DBI::dbWriteTable(pool, "users", user_df, append = TRUE, row.names = FALSE)

    user_df_stored <- dplyr::tbl(pool, "users") %>%
      dplyr::filter(.data$user_name == !!user_df$user_name) %>%
      dplyr::collect()
    user_permission_df <- tibble::tibble(
      user_id = user_df_stored$user_id,
      project_id = project_id,
      data_modify                  = 1,
      data_other_modify            = 1,
      data_other_view              = 1,
      attributes_modify            = 1,
      attributes_other_modify      = 1,
      attributes_other_view        = 1,
      codebook_modify              = 1,
      codebook_other_modify        = 1,
      codebook_other_view          = 1,
      annotation_modify            = 1,
      annotation_other_modify      = 1,
      annotation_other_view        = 1,
      analysis_other_view          = 1,
      report_other_view            = 1,
      permissions_modify           = 1
    )
  }else{
    user_permission_df <- tibble::tibble(
      user_id = user_id,
      project_id = project_id,
      data_modify                  = 1,
      data_other_modify            = 1,
      data_other_view              = 1,
      attributes_modify            = 1,
      attributes_other_modify      = 1,
      attributes_other_view        = 1,
      codebook_modify              = 1,
      codebook_other_modify        = 1,
      codebook_other_view          = 1,
      annotation_modify            = 1,
      annotation_other_modify      = 1,
      annotation_other_view        = 1,
      analysis_other_view          = 1,
      report_other_view            = 1,
      permissions_modify           = 1
    )
  }


  DBI::dbWriteTable(pool, "user_permissions", user_permission_df, append = TRUE, row.names = FALSE)
}

create_project_record <- function(pool, project_df, user_id) {

  res <- pool::dbWriteTable(pool, 
  "projects", 
  project_df, 
  append = TRUE,
  row.names = FALSE)

  
  project_id <- dplyr::tbl(pool, "projects") %>%
    dplyr::filter(project_name == !!project_df$project_name) %>%
    dplyr::pull(project_id)  

  # to delete later after we check for unique project names
  project_id <- max(project_id) 

  if (res) {
    log_create_project_record(pool, project_id, project_df, user_id)
  }

  requal_version_df <- data.frame(
    project_id = project_id,
    version = as.character(packageVersion("requal"))
  )
  res_v <- DBI::dbWriteTable(pool, "requal_version", requal_version_df, append = TRUE, row.names = FALSE)

  create_default_user(pool, project_id, user_id = user_id)
}

add_documents_record <- function(pool, project_id, document_df, user_id) {
  res <- DBI::dbWriteTable(pool, "documents", document_df, append = TRUE, row.names = FALSE)
  if (res) {
    written_document_id <- dplyr::tbl(pool, "documents") %>%
      dplyr::filter(.data$doc_name == !!document_df$doc_name &
        .data$doc_text == !!document_df$doc_text &
        .data$project_id == project_id) %>%
      dplyr::pull(doc_id)
    log_add_document_record(pool, project_id, document_df %>%
      dplyr::mutate(doc_id = written_document_id),
    user_id = user_id
    )
  } else {
    warning("document not added")
  }
}

add_cases_record <- function(pool, project_id, case_df, user_id) {
  res <- DBI::dbWriteTable(pool, "cases", case_df, append = TRUE, row.names = FALSE)
  if (res) {
    written_case_id <- dplyr::tbl(pool, "cases") %>%
      dplyr::filter(.data$case_name == !!case_df$case_name &
        .data$project_id == project_id) %>%
      dplyr::pull(.data$case_id)
    log_add_case_record(pool, project_id, case_df %>%
      dplyr::mutate(case_id = written_case_id),
    user_id = user_id
    )
  } else {
    warning("case not added")
  }
}

add_codes_record <- function(pool, project_id, codes_df, user_id) {
  res <- DBI::dbWriteTable(pool, "codes", codes_df, append = TRUE, row.names = FALSE)
  if (res) {
    written_code_id <- dplyr::tbl(pool, "codes") %>%
      dplyr::filter(.data$code_name == !!codes_df$code_name &
        .data$project_id == project_id) %>%
      dplyr::pull(code_id)
    log_add_code_record(pool, project_id, codes_df %>%
      dplyr::mutate(code_id = written_code_id),
    user_id = user_id
    )
  } else {
    warning("code not added")
  }
}

add_case_doc_record <- function(pool, project_id, case_doc_df, user_id) {
  res <- DBI::dbWriteTable(pool, "cases_documents_map", case_doc_df, append = TRUE, row.names = FALSE)
  if (res) {
    log_add_case_doc_record(pool, project_id, case_doc_df, user_id)
  } else {
    warning("code document map not added")
  }
}

# Globals ####

make_globals <- quote({

  if (golem::get_golem_options(which = "mode") == "server") {

    startup_con <- DBI::dbConnect(
      drv = RPostgreSQL::PostgreSQL(),
      host = golem::get_golem_options(which = "dbhost"),
      port = golem::get_golem_options(which = "dbport"),
      dbname = golem::get_golem_options(which = "dbname"),
      user = golem::get_golem_options(which = "dbusername"),
      password = golem::get_golem_options(which = "dbpassword")
    )

     existing_projects <- dplyr::tbl(startup_con, "projects") %>%
     dplyr::pull(project_id)
     names(existing_projects) <- dplyr::tbl(startup_con, "projects") %>%
     dplyr::pull(project_name)
    
    DBI::dbDisconnect(startup_con)
    

  }
})