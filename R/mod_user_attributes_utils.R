utils::globalVariables(c("attribute_id", "attribute_name", "attribute_value_id")) 

add_attribute <- function(pool, attribute_name, type = "categorical", object){
    new_attribute <- data.frame(
        attribute_name = attribute_name, 
        attribute_object = object, 
        attribute_type = type
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
        dplyr::select(attribute_id, attribute_name) %>%
        dplyr::left_join(., dplyr::tbl(pool, "attribute_values"),
                         by = "attribute_id", 
                         suffix = c(".x", ".y")) %>%
        dplyr::collect()
}
