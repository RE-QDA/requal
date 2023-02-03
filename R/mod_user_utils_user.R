# read user from DB ------
read_user_db <- function(pool, user_id, active_project) {
    dplyr::tbl(pool, "users") %>% 
        dplyr::filter(.data$user_id == !!user_id) %>% 
        dplyr::left_join(., dplyr::tbl(pool, "user_permissions") %>% 
                              dplyr::filter(.data$project_id == as.integer(active_project)),
                          by = c("user_id")) %>% 
        dplyr::collect()
}


# update user details ----
update_user_db <- function(pool, 
                           user_id = 1,
                           user_name,
                           user_email) {
    update_user_sql <- glue::glue_sql("UPDATE users
                 SET user_name = {user_name}, user_mail = {user_email}
                 WHERE user_id = {user_id}", .con = pool)
    
    DBI::dbExecute(pool, update_user_sql)
}