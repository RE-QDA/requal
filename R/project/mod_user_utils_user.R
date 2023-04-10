# read user from DB ------
read_user_db <- function(pool, user_id, active_project) {
    dplyr::tbl(pool, "users") %>% 
        dplyr::filter(.data$user_id == !!user_id) %>% 
        dplyr::left_join(., dplyr::tbl(pool, "user_permissions") %>% 
                             dplyr::filter(.data$project_id == as.integer(active_project)), 
                         by = c("user_id"), 
                         suffix = c(".x", ".y")) %>% 
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

generate_user_attribute_select_ui <- function(id, attribute, values, user_value = ""){
    selectInput(id, label = attribute, choices = c("", values), 
                selected = user_value)
}

get_user_attributes_from_modal <- function(input, user_attributes){
   
    purrr::map_df(user_attributes, function(x) {
        tibble::tibble(
            attribute_name = x, 
            attribute_value = input[[paste0("user_attribute_", x)]]
        )
    })
}

update_user_attributes <- function(pool, project_id, user_id, user_attributes_df){
    project_id <- local(project_id)
    
    attribute_ids <- dplyr::tbl(pool, "attributes") %>% 
        dplyr::select(attribute_id, attribute_name) %>% 
        dplyr::collect()
    attribute_values <- dplyr::tbl(pool, "attribute_values") %>% 
        dplyr::select(attribute_id, attribute_value_id, value) %>% 
        dplyr::collect()
    
    user_attributes_df <- user_attributes_df %>% 
        dplyr::left_join(., attribute_ids, by = "attribute_name") %>% 
        dplyr::left_join(., attribute_values, by = c("attribute_id", "attribute_value"="value")) %>% 
        dplyr::mutate(user_id = user_id, 
                      project_id = project_id)
    
    purrr::walk(seq_len(nrow(user_attributes_df)), function(x) {
        # Check if attribute has value
        existing_attr <- dplyr::tbl(pool, "attributes_users_map") %>% 
            dplyr::filter(.data$user_id == !!user_id &
                              .data$project_id == !!as.numeric(project_id) &  
                              .data$attribute_id == !!user_attributes_df$attribute_id[x]) %>%
            dplyr::collect()
        
        if(nrow(existing_attr)){
            attr_id <- user_attributes_df$attribute_id[x]
            attr_value <- user_attributes_df$attribute_value_id[x]
            
            if(existing_attr$attribute_value_id != attr_value){
                
                update_attributes_sql <- glue::glue_sql("UPDATE attributes_users_map
                 SET attribute_value_id = {attr_value}
                 WHERE user_id = {user_id} 
                 AND attribute_id = {attr_id}
                 AND project_id = {project_id}", .con = pool)
                
                DBI::dbExecute(pool, update_attributes_sql)
                log_change_user_attribute(pool,
                                          user_id = user_id,
                                          project_id = local(project_id),
                                          user_attribute = data.frame(
                                              attribute_id = attr_id,
                                              attribute_value_id = attr_value
                                          ))
            }
        }else{
            values_df <- user_attributes_df[x,] %>% 
                dplyr::select(user_id, attribute_id, 
                              attribute_value_id, project_id)
            DBI::dbWriteTable(pool, "attributes_users_map", 
                              values_df, append = TRUE, row.names = FALSE)
            log_change_user_attribute(pool,
                                      project_id = local(project_id),
                                      user_id = user_id,
                                      user_attribute = values_df)
        }
    })
}

read_user_attributes_by_id <- function(pool, user_id, project_id){
    dplyr::tbl(pool, "attributes_users_map") %>% 
        dplyr::filter(.data$user_id == !!user_id & 
                          .data$project_id == !!as.numeric(project_id)) %>% 
        dplyr::left_join(., dplyr::tbl(pool, "attributes") %>% dplyr::select(-user_id), 
                         by = c("attribute_id", "project_id"), 
                         suffix = c(".x", ".y")) %>%
        dplyr::left_join(., dplyr::tbl(pool, "attribute_values"), 
                         by = c("attribute_id", "attribute_value_id"), 
                         suffix = c(".x", ".y")) %>% 
        dplyr::collect()
}

transform_user_table <- function(user_table) {
user_table %>%
  dplyr::mutate(across(-c(user_id, created_at, user_mail, user_name, user_login), 
        .fn = function(x) {
          ifelse(x == 1, 
          '&#x25CF;', 
          '&#x25CB;')
        })) %>%
        tidyr::unite("Data", tidyselect::starts_with("data"), sep = "") %>%
        tidyr::unite("Attributes", tidyselect::starts_with("attributes"), sep = "") %>%
        tidyr::unite("Codebook", tidyselect::starts_with("codebook"), sep = "") %>%
        tidyr::unite("Annotate", tidyselect::starts_with("annotation"), sep = "") %>%
        tidyr::unite("Analyze", tidyselect::starts_with("analysis"), sep = "") %>%
        tidyr::unite("Report", tidyselect::starts_with("report"), sep = "") %>%
        dplyr::mutate(created_at = format(
          strptime(created_at, format = "%Y-%m-%d %H:%M:%S"),
          format = "%Y-%m-%d %H:%M")
         ) %>%
        dplyr::select(
          "Login" = user_login,
          "Name"  = user_name,
          "Mail"  = user_mail,
          "Data",
          "Attributes",
          "Annotate",
          "Analyze",
          "Report",
          "Created" = created_at
          )
}