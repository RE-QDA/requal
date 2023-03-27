utils::globalVariables(c("attribute_id", "attribute_name", "attribute_value_id")) 

add_attribute <- function(pool, attribute_name, type, min, max, object){
    new_attribute <- data.frame(
        attribute_name = attribute_name, 
        attribute_object = object, 
        attribute_type = type, 
        attribute_min = ifelse(is.null(min), NA_integer_, min), 
        attribute_max = ifelse(is.null(max), NA_integer_, max)
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

read_user_attributes <- function(pool){
   
    dplyr::tbl(pool, "attributes") %>%
        dplyr::filter(.data$attribute_object == "user") %>%
        dplyr::select(attribute_id, attribute_name, attribute_type, 
                      attribute_min, attribute_max) %>%
        dplyr::left_join(., dplyr::tbl(pool, "attribute_values"),
                         by = "attribute_id", 
                         suffix = c(".x", ".y")) %>%
        dplyr::collect()
}

summarise_user_attributes <- function(df){
  df %>% 
    dplyr::group_by(attribute_name, attribute_type, attribute_min, attribute_max) %>%
    dplyr::summarise(values = paste0(value, collapse = ", ")) %>% 
    dplyr::mutate(values = dplyr::case_when(
      attribute_type == "numeric" & !is.na(attribute_min) & !is.na(attribute_max) ~ 
        paste0(c("range: ", attribute_min, " to ", attribute_max), collapse = ""), 
      attribute_type == "numeric" & !is.na(attribute_min)  ~ 
        paste0(c("range: >=", attribute_min), collapse = ""), 
      attribute_type == "numeric" & !is.na(attribute_max) ~ 
        paste0(c("range: <=", attribute_max), collapse = ""), 
      TRUE ~ values
    )) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(attribute_name, attribute_type, values)
}
