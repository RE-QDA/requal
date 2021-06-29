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
                              ), ".sqlite")
                          )))
    on.exit(DBI::dbDisconnect(con))
    
    project_df <- tibble::tibble(project_name, 
                                 project_description,
                                 project_created = as.character(Sys.time()))
    
       # https://github.com/r-dbi/RSQLite/issues/59
       DBI::dbExecute(con,
                 "CREATE TABLE project
(  project_id INTEGER PRIMARY KEY, -- Autoincrement
   project_name TEXT,
   project_description TEXT,
   project_created TEXT
)" 
       )
       
       DBI::dbWriteTable(con, "project", project_df, 
                         append = TRUE)
    
    
   }
    

read_project_db <- function(project_directory) {
    
    db_file <- list.files(path = project_directory, 
                          pattern = ".sqlite$",
                          full.names = TRUE)
    
    if (length(db_file) >= 1) {
        
        db_file <- db_file[1]
        
    } else {
        
       return("No active project in the folder.")
        
    } 
    
    con <- DBI::dbConnect(RSQLite::SQLite(),
                          db_file
                         )
    on.exit(DBI::dbDisconnect(con))
    
    
    project_name <- dplyr::tbl(con, "project") %>% 
        dplyr::pull(project_name)
    
    return(project_name)
    
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

