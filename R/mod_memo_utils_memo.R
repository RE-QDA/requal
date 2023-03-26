# list existing memos ------
list_memo_records <- function(pool, project) {
    memos_df <- dplyr::tbl(pool, "memos") %>%
        dplyr::filter(.data$project_id == local(as.integer(project))) %>%
        dplyr::select(
            memo_id,
            memo_name = text
        ) %>%
        dplyr::collect() %>% 
        dplyr::mutate(
            memo_name = substr(stringr::str_extract(.data$memo_name, "\\A.*"), 
                               1, 50)) 
    
    return(memos_df)
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
            dplyr::pull(memo_id)
        log_add_memo_record(pool, memo_df$project_id, memo_df %>% 
                                dplyr::mutate(memo_id = memo_id), 
                            user_id = user_id)
    }
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

# read specific memo text from db ----
read_memo_db <- function(pool, memo_id) {
    memo_text <- dplyr::tbl(pool, "memos") %>%
        dplyr::filter(.data$memo_id == as.integer(.env$memo_id)) %>%
        dplyr::pull(text)
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


# dropdown2 function ----
dropdownBlock2 <- function (..., id, icon = NULL, title = NULL, badgeStatus = "danger") 
{
    if (!is.null(badgeStatus)) 
        validateStatus(badgeStatus)
    items <- c(list(...))
    dropdownClass <- paste0("dropdown")
    numItems <- length(items)
    if (is.null(badgeStatus)) {
        badge <- NULL
    }
    else {
        badge <- dashboardLabel(status = badgeStatus, numItems)
    }
    shiny::tags$li(class = dropdownClass, id = id, shiny::tags$a(href = "#",
                                                                 class = "dropdown-toggle",
                                                                 `data-toggle` = "dropdown", 
                                                                 icon, 
                                                                 title, 
                                                                 badge), 
                   shiny::tags$ul(class = "dropdown-menu", 
                                  style = "left: 0; right: auto; max-height: 80vh", 
                                  shiny::tags$li(shiny::tags$ul(class = "menu", 
                                                                shiny::tags$div(style = "margin-left: auto; margin-right: 0; width: 80%;",
                                                                                items)))))
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
    js_fun <- "Shiny.setInputValue('memo_ui_1-selected_memo', this.name, {priority: 'event'});"
    quote_sign <- '"'
    paste0('<a class="action-button memo_name shiny-bound-input" href="#" name="', id, '" onclick=', quote_sign,js_fun,quote_sign, '">', text, '</a>')
}
