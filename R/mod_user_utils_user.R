# read user from DB ------

read_user_db <- function(user_id) {

    dplyr::tbl(pool, "users") %>% 
        dplyr::filter(.data$user_id == !!user_id) %>% 
        dplyr::inner_join(dplyr::tbl(pool, "user_permissions"),
                          by = "user_id") %>% 
        dplyr::collect()
    
}


# update user details ----


update_user_db <- function(user_id = 1,
               user_name,
               user_email) {
    
    update_user_sql <- glue::glue_sql("UPDATE users
                 SET user_name = {user_name}, user_mail = {user_email}
                 WHERE user_id = {user_id}", .con = pool)
    
    res <- DBI::dbSendStatement(pool, update_user_sql)
    DBI::dbClearResult(res)
    
}