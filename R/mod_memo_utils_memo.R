utils::globalVariables(c("memo_title"))

# list existing memos ------
list_memo_records <- function(pool, project) {
    dplyr::tbl(pool, "memos") %>%
        dplyr::filter(.data$project_id == local(as.integer(project))) %>%
        dplyr::select(
            memo_id,
            memo_name = text, 
            # memo_text = text, 
            user_id
        ) %>%
        dplyr::collect() %>% 
        dplyr::mutate(
            memo_name = substr(stringr::str_extract(.data$memo_name, "\\A.*"), 
                               1, 50)) 
}

read_memo_by_id <- function(pool, project, memo_id) {
    dplyr::tbl(pool, "memos") %>%
        dplyr::filter(.data$project_id == local(as.integer(project))) %>%
        dplyr::filter(.data$memo_id == local(as.integer(memo_id))) %>% 
        dplyr::select(
            memo_id,
            memo_name = text, 
            memo_text = text, 
            user_id
        ) %>%
        dplyr::collect() %>% 
        dplyr::mutate(
            memo_name = substr(stringr::str_extract(.data$memo_name, "\\A.*"), 
                               1, 50)) 
}

find_memo_permission <- function(memo_user_id, user){
    if(user$data$memo_modify == 0){
        FALSE
    }else if(memo_user_id == user$user_id){
        TRUE
    }else if(user$data$memo_other_modify == 1){
        TRUE
    }else{
        FALSE
    }
}

# write new free memo to db ------
add_memo_record <- function(pool, project, text, user_id) {
    
    memo_df <- data.frame(project_id = local(project),
                          user_id = user_id, 
                          text = text)
    
    res <- DBI::dbWriteTable(pool, "memos", memo_df, append = TRUE, row.names = FALSE)
    if(res){
        memo_id <- dplyr::tbl(pool, "memos") %>% 
            dplyr::filter(.data$project_id == !!memo_df$project_id, 
                          .data$text == !!memo_df$text) %>% 
            dplyr::filter(memo_id == max(memo_id))  %>% 
            dplyr::pull(memo_id)
        log_add_memo_record(pool, 
                            project = memo_df$project_id, 
                            user_id = user_id,
                            df = memo_df %>% 
                                dplyr::mutate(memo_id = max(memo_id))
                            )
    }
    return(memo_id)
}

# render memos -----
render_memos <- function(id, memo_df) {
    
    ns <- NS(id)
    
    purrr::map2(memo_df$memo_id, memo_df$memo_name, 
                ~tagList(actionLink(inputId = as.character(.x), 
                                    label = .y,
                                    name = as.character(.x),
                                    onclick = paste0("Shiny.setInputValue('", 
                                                     ns("selected_memo"), "', 
                                                         this.name, {priority: 'event'});")
                ),
                br()
                )
    )
}

# update memo record ------

update_memo_record <- function(pool, project, memo_id, memo_text, user_id) {
    memo_id <- as.integer(memo_id)
    
    update_memo_sql <- glue::glue_sql("UPDATE memos
                 SET text = {memo_text}
                 WHERE memo_id = {memo_id}", .con = pool)
    
    DBI::dbExecute(pool, update_memo_sql)
    
    log_update_memo_record(pool, local(project), 
                           data.frame(memo_id = memo_id, text = memo_text), 
                           user_id = user_id)
    
}

# delete memo record -----
delete_memo_record <- function(pool, project, memo_id, user_id) {
    memo_id <- as.integer(memo_id)
    
    delete_memo_sql <- glue::glue_sql("DELETE from memos
                   WHERE memo_id = {memo_id}", .con = pool)
    
    DBI::dbExecute(pool, delete_memo_sql)
    log_delete_memo_record(pool, local(project), memo_id, 
                           user_id = user_id)
    
}

export_memos <- function(pool, project) {
   
    dplyr::tbl(pool, "memos") %>%
    dplyr::filter(.data$project_id == local(as.integer(project))) %>%
    dplyr::select(
        memo_id,
        memo_text = text, 
        user_id
    ) %>%
    dplyr::left_join(dplyr::tbl(pool, "users") %>% 
                         dplyr::select(user_id, user_name), by = "user_id") %>% 
    dplyr::select(-user_id) %>% 
    dplyr::collect()  %>% 
    dplyr::mutate(
            memo_title = substr(
                stringr::str_extract(.data$memo_text, "\\A.*"), 
                               1, 50)
                ) %>% 
    dplyr::relocate(memo_title, 2)

}


# memo table styling ----
memo_table_options <- function() {
    list(
        dom = 'lfrtp',
        #Bfrtip
        pageLength = 20,
        searching = TRUE,
        lengthChange = FALSE
    )
}

# create memo as link ----
memo_link <- function(id, text) {
    js_fun <- "Shiny.setInputValue('memo_ui_1-text_memo_click', this.name, {priority: 'event'});"
    quote_sign <- '"'
    paste0('<a class="action-button memo_name shiny-bound-input" href="#" name="', id, '" onclick=', quote_sign,js_fun,quote_sign, '">', text, '</a>')
}
