# read user from DB ------

read_user_db <- function(project_db,
                         active_project,
                         user_id) {

    con <- DBI::dbConnect(RSQLite::SQLite(),
                          project_db)
    on.exit(DBI::dbDisconnect(con))
    
    dplyr::tbl(con, "users") %>% 
        dplyr::filter(.data$user_id == !!user_id) %>% 
        dplyr::inner_join(dplyr::tbl(con, "user_permissions"),
                          by = "user_id") %>% 
        dplyr::collect()
    
}


# update user details ----


update_user_db <- function(project_db,
               user_id = 1,
               user_name,
               user_email) {
    
    con <- DBI::dbConnect(RSQLite::SQLite(),
                          project_db)
    on.exit(DBI::dbDisconnect(con))
    
    update_user_sql <- glue::glue_sql("UPDATE users
                 SET user_name = {user_name}, user_mail = {user_email}
                 WHERE user_id = {user_id}", .con = con)
    
    res <- DBI::dbSendStatement(con, update_user_sql)
    DBI::dbClearResult(res)
    
    
}