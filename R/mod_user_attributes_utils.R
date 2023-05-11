utils::globalVariables(c("attribute_id", "attribute_name", "attribute_value_id", "attribute_value")) 

add_attribute <- function(pool, attribute_name, type = "categorical", object, project_id, user_id){
    new_attribute <- data.frame(
        attribute_name = attribute_name, 
        attribute_object = object, 
        attribute_type = type, 
        project_id = project_id, 
        user_id = user_id
    )
    
    res <- DBI::dbWriteTable(pool, "attributes", new_attribute, append = TRUE, row.names = FALSE)
    
    if(!res) warning("attribute was not added")
}

split_values <- function(x){
  strsplit(x, "[,;\r\n]") %>% 
    unlist() %>% 
    trimws()
}

add_attribute_values <- function(pool, attribute_id, attribute_values){
  
    values_df <- data.frame(
        attribute_id = attribute_id, 
        value = split_values(attribute_values)
    )
    
    res <- DBI::dbWriteTable(pool, "attribute_values", values_df, append = TRUE, row.names = FALSE)
    
    if(!res) warning("attribute values were not added")
}

read_user_attributes <- function(pool, project_id){
    
    dplyr::tbl(pool, "attributes") %>%
        dplyr::filter(.data$attribute_object == "user") %>%
        dplyr::filter(.data$project_id == !!as.numeric(project_id)) %>% 
        dplyr::select(attribute_id, attribute_name) %>%
        dplyr::left_join(., dplyr::tbl(pool, "attribute_values"),
                         by = "attribute_id", 
                         suffix = c(".x", ".y")) %>%
        dplyr::collect()
}

get_user_attributes_data_table <- function(pool, project_id){
  # create memo as link ----
  js_fun <- "Shiny.setInputValue('user_attributes_ui_1-selected_attr', this.name, {priority: 'event'});"
  quote_sign <- '"'
  
  read_user_attributes(pool, project_id) %>% 
    dplyr::group_by(attribute_id, attribute_name) %>%
    dplyr::summarise(values = paste0(value, collapse = ", ")) %>% 
    dplyr::mutate(
      button = paste0('<a class="action-button memo_name shiny-bound-input" href="#" name="', attribute_id, '" onclick=', quote_sign,js_fun,quote_sign, '">Delete attribute</a>')
    )
}

delete_user_attribute <- function(pool, project_id, user_id, user_attribute_id) {
  res <- DBI::dbExecute(pool,
                        glue::glue_sql("DELETE from attributes
                   WHERE attribute_object = 'user'
                   AND attribute_id IN ({user_attribute_id})",
                   .con = pool)
  )
  
  if(res & length(user_attribute_id)){
    log_delete_user_attribute(pool, project_id, user_attribute_id, user_id)
  }
}

get_user_attributes_summary <- function(pool, project_id){
  # Get list of users that are active in the current project
  permissions <- dplyr::tbl(pool, "user_permissions") %>% 
    dplyr::filter(project_id == !!as.numeric(project_id)) %>% 
    dplyr::collect()
  
  users <- dplyr::tbl(pool, "users") %>% 
    dplyr::collect() %>% 
    dplyr::inner_join(., permissions, by = "user_id")
  
  # Get user attributes
  attr_user_map <- dplyr::tbl(pool, "attributes_users_map") %>% 
    dplyr::filter(project_id == !!as.numeric(project_id)) %>% 
    dplyr::collect()
  
  attribute_values <- dplyr::tbl(pool, "attributes") %>% 
    dplyr::left_join(., dplyr::tbl(pool, "attribute_values"), by = "attribute_id") %>% 
    dplyr::select(attribute_id, attribute_name, attribute_value_id, value) %>% 
    dplyr::collect()
  
  user_attributes <- attr_user_map %>% 
    dplyr::left_join(., attribute_values, by = c("attribute_id", "attribute_value_id")) %>% 
    dplyr::select(user_id, attribute_name, attribute_value = value)
  
  tidyr::expand_grid(user_id = unique(users$user_id), 
                     attribute_name = unique(user_attributes$attribute_name)) %>% 
    dplyr::left_join(., user_attributes, by = c("user_id", "attribute_name")) %>% 
    dplyr::mutate(attribute_value = dplyr::if_else(is.na(attribute_value), 
                                                   "Missing value", attribute_value)) %>% 
    dplyr::count(attribute_name, attribute_value) %>% 
    dplyr::group_by(attribute_name) %>% 
    dplyr::mutate(share = n / sum(n)) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(!is.na(attribute_name))
}


