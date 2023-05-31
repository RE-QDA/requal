
# Load users who are active in the current project
get_users_in_project <- function(pool, project_id){
    users <- dplyr::tbl(pool, "users") %>% 
        dplyr::left_join(dplyr::tbl(pool, "user_permissions"), 
                         by = "user_id") %>% 
        dplyr::filter(project_id == !!as.integer(project_id)) %>% 
        dplyr::select(user_id, user_name) %>% 
        dplyr::collect()
}

# Load logs for reporting -------------------------------------------
load_logs_for_reporting <- function(pool, active_project) {
    logs <- dplyr::tbl(pool, "logs") %>%
        dplyr::filter(.data$project_id == as.integer(active_project)) %>%
        dplyr::left_join(., dplyr::tbl(pool, "users") %>% 
                             dplyr::select(.data$user_id, .data$user_name), 
                         by = "user_id", 
                         suffix = c(".x", ".y")) %>% 
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

parse_payload_json <- function(x, sanitize = FALSE){
    tmp <- jsonlite::fromJSON(x)
    
    if(sanitize){
        tmp <- tmp %>% 
            dplyr::select(-dplyr::any_of(c("segment_text", "segment_start", "segment_end")))
    }
    
    tmp %>% 
        purrr::map2_chr(names(.), ., function(name, value) {
            paste0(toupper(name), ": ", shorten_value(collapse_array(value)))
        }) %>% 
        paste0(., collapse = "; ")
}

possibly_parse_payload_json <- purrr::possibly(parse_payload_json, "")
