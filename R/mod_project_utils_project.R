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
    
    active_project_df <- dplyr::tbl(con, "projects") %>% 
        dplyr::select(project_id, project_name) %>% 
        dplyr::collect() %>% 
        dplyr::slice_max(project_id, n = 1)
    
    active_project <- active_project_df %>% 
        dplyr::pull(project_id)
    
    names(active_project) <- active_project_df %>% 
                                   dplyr::pull(project_name)
    
    return(active_project)
}


read_project_db_util <- function(db_file,
                                 table = "projects"
                                 ) {
    con <- DBI::dbConnect(RSQLite::SQLite(),
                          db_file
    )
    on.exit(DBI::dbDisconnect(con))

        active_project_df <- dplyr::tbl(con, table) %>% 
        dplyr::select(project_id, project_name) %>% 
        dplyr::collect()
                                 
    active_project <- active_project_df %>% 
        dplyr::pull(project_id)
                                 
    names(active_project) <- active_project_df %>% 
                 dplyr::pull(project_name)
    
    return(active_project)

}

read_project_db <- function(db_file, project_id) {
    
    
    if (!is.null(db_file)) {
        
   
        
        if (!is.null(project_id)) { # filter by id
            
            project_id <- as.integer(project_id)
            
            active_project <- read_project_db_util(db_file)
            active_project <- active_project[active_project == project_id]
            
            return(active_project)
            
        } else  { # no filter
            
            active_project <- read_project_db_util(db_file)
            
            return(active_project)
        }
        
        
    } else {
        
        return("No active project.")
    }
}

# list projects

#' @importFrom rlang .env
list_db_projects <- function(project_db) {

    
    con <- DBI::dbConnect(RSQLite::SQLite(),
                          project_db
    )
    on.exit(DBI::dbDisconnect(con))
    
    
    project_name <-  read_project_db(.env$db_file)
    
    return(project_name)
}


# list_db_documents

list_db_documents <- function(project_db, active_project) {

    doc_id <- doc_name <- NULL
    
    con <- DBI::dbConnect(RSQLite::SQLite(),
                          project_db
                          )
    on.exit(DBI::dbDisconnect(con))

    active_project <- as.integer(active_project)

    project_documents <- dplyr::tbl(con, "documents") %>%
        dplyr::filter(.data$project_id == .env$active_project) %>%
        dplyr::select(.data$doc_id, .data$doc_name) %>%
        dplyr::collect() %>%
        dplyr::mutate(doc_name = ifelse(is.na(doc_name), "", doc_name))
        
       
    documents_list <- project_documents$doc_id
    names(documents_list) <- project_documents$doc_name
    
    return(documents_list)
    
}

list_db_document_table <- function(project_db, active_project) {
    

    con <- DBI::dbConnect(RSQLite::SQLite(),
                          project_db
    )
    on.exit(DBI::dbDisconnect(con))
    
    active_project <- as.integer(active_project)
    
    documents_table <- dplyr::tbl(con, "documents") %>%
        dplyr::filter(.data$project_id == .env$active_project) %>%
        dplyr::collect() 


    
    return(documents_table)
    
}


