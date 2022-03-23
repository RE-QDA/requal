# list existing memos ------
list_memo_records <- function(project) {
    ## To pass R CMD check and define DB variables as global variables for the function https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
    
    con <- DBI::dbConnect(
        RSQLite::SQLite(),
        project()$project_db
    )
    active_project <- as.integer(project()$active_project)
    on.exit(DBI::dbDisconnect(con))
  
    memos_df <- dplyr::tbl(con, "memos") %>%
        dplyr::filter(.data$project_id == active_project) %>%
        dplyr::select(
            memo_id,
            memo_name = text
        ) %>%
        dplyr::mutate(memo_name = substr(memo_name, 1, 25)) %>% 
        dplyr::collect() 
    
    return(memos_df)
}



# write new free memo to db ------
add_memo_record <- function(project, text) {
    
    con <- DBI::dbConnect(RSQLite::SQLite(), project()$project_db)
    on.exit(DBI::dbDisconnect(con))
    
    memo_df <- data.frame(project_id = project()$active_project,
                          text = text)
    
    res <- DBI::dbWriteTable(con, "memos", memo_df, append = TRUE)
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

read_memo_db <- function(project, memo_id) {
    
    con <- DBI::dbConnect(RSQLite::SQLite(), project()$project_db)
    on.exit(DBI::dbDisconnect(con))
    
    memo_text <- dplyr::tbl(con, "memos") %>%
        dplyr::filter(.data$memo_id == as.integer(.env$memo_id)) %>%
        dplyr::pull(text)
}

# update memo record ------

update_memo_record <- function(project, memo_id, memo_text) {
    
    
    con <- DBI::dbConnect(RSQLite::SQLite(),
                          project()$project_db)
    on.exit(DBI::dbDisconnect(con))
    
    memo_id <- as.integer(memo_id)
    
    update_memo_sql <- glue::glue_sql("UPDATE memos
                 SET text = {memo_text}
                 WHERE memo_id = {memo_id}", .con = con)
    
    res <- DBI::dbSendStatement(con, update_memo_sql)
    DBI::dbClearResult(res)
    
}