utils::globalVariables(c("attribute_id", "attribute_name", "attribute_value_id")) 

add_attribute <- function(pool, attribute_name, type = "categorical", object, project_id){
    new_attribute <- data.frame(
        attribute_name = attribute_name, 
        attribute_object = object, 
        attribute_type = type, 
        project_id = project_id
    )
    
    res <- DBI::dbWriteTable(pool, "attributes", new_attribute, append = TRUE, row.names = FALSE)
    
    if(res){
        # TODO: log
    }else{
        warning("attribute was not added")
    }
}

add_attribute_values <- function(pool, attribute_id, attribute_values){
  
    values <- strsplit(attribute_values, ",") %>% 
        unlist() %>% 
        trimws()
    
    values_df <- data.frame(
        attribute_id = attribute_id, 
        value = values
    )
    
    res <- DBI::dbWriteTable(pool, "attribute_values", values_df, append = TRUE, row.names = FALSE)
    
    if(res){
        # TODO: log
    }else{
        warning("attribute was not added")
    }
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
  read_user_attributes(pool, project_id) %>% 
    dplyr::group_by(attribute_id, attribute_name) %>%
    dplyr::summarise(values = paste0(value, collapse = ", ")) %>% 
    dplyr::mutate(
      button = as.character(actionButton(ns(paste0('button_', attribute_id)), 
                                         label = "Delete"))
    )
}
