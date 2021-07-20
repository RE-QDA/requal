create_project_db <- function(project_directory, 
                              project_name,
                              project_description) {
    
    con <- DBI::dbConnect(RSQLite::SQLite(),
                          fs::path_join(c(
                              project_directory,
                              paste0(gsub(
                                  "[^a-zA-Z]+",
                                  "",
                                  iconv(project_name, 
                                        to = "ASCII//TRANSLIT")
                              ), ".requal")
                          )))
    on.exit(DBI::dbDisconnect(con))
    
    project_df <- tibble::tibble(project_name, 
                                 project_description,
                                 created_at = as.character(Sys.time()))
    

    if (!DBI::dbExistsTable(con, "projects")) {
        # https://github.com/r-dbi/RSQLite/issues/59
        create_db_schema(con)
    }

    create_project_record(con, project_df)
}


read_project_db <- function(project_file, name) {
    
    db_file <- project_file
    
    if (length(db_file) >= 1) {
        
        db_file <- db_file[1]
        
    } else {
        
        db_file <- NULL
        
    } 
    
    if (!is.null(db_file)) {
        
        con <- DBI::dbConnect(RSQLite::SQLite(),
                              db_file
        )
        on.exit(DBI::dbDisconnect(con))
        
        if (!is.null(name)) {
            
            active_project_name <- dplyr::tbl(con, "projects") %>% 
                dplyr::filter(project_name == name) %>% 
                dplyr::pull(project_name)
            
            return(active_project_name)
            
        } else {
            
            active_project_name <- dplyr::tbl(con, "projects") %>% 
                dplyr::pull(project_name)
            
            return(active_project_name)
            
        }
        
        
    } else {
        
        return("No active project.")
    }
}

# list projects


list_db_projects <- function(project_db) {
    
    
    con <- DBI::dbConnect(RSQLite::SQLite(),
                          project_db
    )
    on.exit(DBI::dbDisconnect(con))
    
    
    project_name <- dplyr::tbl(con, "projects") %>% 
        dplyr::pull(project_name)
    
    return(project_name)
}

get_project_id <- function(con, project){
    dplyr::tbl(con, "projects") %>%
        dplyr::filter(project_name == project) %>%
        dplyr::pull(project_id) %>%
        unique
}

# list_db_documents

list_db_documents <- function(project_db, project) {
    
    con <- DBI::dbConnect(RSQLite::SQLite(),
                          project_db
                          )
    on.exit(DBI::dbDisconnect(con))
    
    project_filter <- get_project_id(con, project)
    
    project_documents <- dplyr::tbl(con, "documents") %>% 
        dplyr::filter(project_id == project_filter) %>% 
        dplyr::pull(doc_id)
    
    return(project_documents)
    
}

# delete documents from project
delete_db_documents <- function(project_db, project_name, delete_doc_id) {
    con <- DBI::dbConnect(RSQLite::SQLite(), project_db)
    on.exit(DBI::dbDisconnect(con))
    
    delete_doc_id <- as.integer(delete_doc_id)
    
    project_id <- get_project_id(con, project_name)
    
    DBI::dbExecute(con,
                   "DELETE from documents
                   WHERE doc_id IN (?)",
                   params = list(delete_doc_id))
    log_delete_document_record(con, project_id, delete_doc_id)
}

