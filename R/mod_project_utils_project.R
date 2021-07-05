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
                                 project_created = as.character(Sys.time()))
    
    if (!DBI::dbExistsTable(con, "project")) {
        
        DBI::dbExecute(con, "PRAGMA foreign_keys=ON")
        
    
       # https://github.com/r-dbi/RSQLite/issues/59
       DBI::dbExecute(con, 
                 "CREATE TABLE project
(  project_id INTEGER PRIMARY KEY, -- Autoincrement
   project_name TEXT,
   project_description TEXT,
   project_created TEXT
)" 
       )
        
        DBI::dbExecute(con,
                "CREATE TABLE IF NOT EXISTS documents
       (   project INTEGER,
           document_id INTEGER PRIMARY KEY, -- Autoincrement
           document_text TEXT,
           FOREIGN KEY(project) REFERENCES project(project_id)
       )"
       )

       
    }
       
       DBI::dbWriteTable(con, "project", project_df, 
                         append = TRUE)

       
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
        
    active_project_name <- dplyr::tbl(con, "project") %>% 
        dplyr::filter(project_name == name) %>% 
        dplyr::pull(project_name)
    
    return(active_project_name)
    
    } else {
        
        active_project_name <- dplyr::tbl(con, "project") %>% 
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
        
        
        project_name <- dplyr::tbl(con, "project") %>% 
            dplyr::pull(project_name)
        
        return(project_name)
    }

