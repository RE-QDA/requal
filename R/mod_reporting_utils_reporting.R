# Load logs for reporting -------------------------------------------
load_logs_for_reporting <- function(pool, active_project) {
    logs <- dplyr::tbl(pool, "logs") %>%
        dplyr::filter(.data$project_id == as.integer(active_project)) %>%
        dplyr::left_join(., dplyr::tbl(pool, "users") %>% 
                             dplyr::select(.data$user_id, .data$user_name), 
                         by = "user_id") %>% 
        dplyr::select(.data$action, 
                      user = .data$user_name, 
                      detail = .data$payload, 
                      .data$created_at) %>%
        dplyr::collect()
    
    return(logs)
}

collapse_array <- function(x){
    if(length(x) > 1){
        paste0(x, collapse = ", ")
    }else{
        x
    }
}

shorten_value <- function(x){
    if(nchar(x) > 100){
        paste0(substr(x, 1, 100), "...")
    }else{
        x
    }
}

parse_payload_json <- function(x){
    jsonlite::fromJSON(x) %>% 
        purrr::map2_chr(names(.), ., function(name, value) {
            paste0(toupper(name), ": ", shorten_value(collapse_array(value)))
        }) %>% 
        paste0(., collapse = "; ")
}

possibly_parse_payload_json <- purrr::possibly(parse_payload_json, "")
