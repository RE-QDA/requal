utils::globalVariables(c("memo_title"))

# list existing memos ------
list_memo_records <- function(pool, project) {
    dplyr::tbl(pool, "memos") %>%
        dplyr::filter(.data$project_id == local(as.integer(project))) %>%
        dplyr::select(
            memo_id,
            memo_text = text,
            user_id
        ) %>%
        dplyr::collect() %>%
        dplyr::mutate(
            memo_name = entitle_memo(memo_text)
        )
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
            memo_name = entitle_memo(memo_name)
        )
}

find_memo_permission <- function(memo_user_id, user) {
    if (user$data$memo_modify == 0) {
        FALSE
    } else if (memo_user_id == user$user_id) {
        TRUE
    } else if (user$data$memo_other_modify == 1) {
        TRUE
    } else {
        FALSE
    }
}

# write new free memo to db ------
add_memo_record <- function(pool, project, text, user_id) {
    memo_df <- data.frame(
        project_id = local(project),
        user_id = user_id,
        text = text
    )

    res <- DBI::dbWriteTable(
        pool,
        "memos",
        memo_df,
        append = TRUE,
        row.names = FALSE
    )
    if (res) {
        memo_id <- dplyr::tbl(pool, "memos") %>%
            dplyr::filter(
                .data$project_id == !!memo_df$project_id,
                .data$text == !!memo_df$text
            ) %>%
            dplyr::filter(memo_id == max(memo_id)) %>%
            dplyr::pull(memo_id)
        log_add_memo_record(
            pool,
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

    purrr::map2(
        memo_df$memo_id,
        memo_df$memo_name,
        ~ tagList(
            actionLink(
                inputId = as.character(.x),
                label = .y,
                name = as.character(.x),
                onclick = paste0(
                    "Shiny.setInputValue('",
                    ns("selected_memo"),
                    "', 
                                                         this.name, {priority: 'event'});"
                )
            ),
            br()
        )
    )
}

# update memo record ------

update_memo_record <- function(pool, project, memo_id, memo_text, user_id) {
    memo_id <- as.integer(memo_id)

    update_memo_sql <- glue::glue_sql(
        "UPDATE memos
                 SET text = {memo_text}
                 WHERE memo_id = {memo_id}",
        .con = pool
    )

    DBI::dbExecute(pool, update_memo_sql)

    log_update_memo_record(
        pool,
        local(project),
        data.frame(memo_id = memo_id, text = memo_text),
        user_id = user_id
    )
}

# delete memo record -----
delete_memo_record <- function(pool, project, memo_id, user_id) {
    memo_id <- as.integer(memo_id)

    delete_memo_sql <- glue::glue_sql(
        "DELETE from memos
                   WHERE memo_id = {memo_id}",
        .con = pool
    )

    DBI::dbExecute(pool, delete_memo_sql)
    log_delete_memo_record(pool, local(project), memo_id, user_id = user_id)
}

# check memo exists -----
exists_memo_db <- function(pool, memo_id) {
    check_df <- dplyr::tbl(pool, "memos") %>%
        dplyr::filter(.data$memo_id == local(as.integer(memo_id))) %>%
        dplyr::collect() # Collect the data into a local data frame

    if (nrow(check_df) > 0) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}


export_memos <- function(pool, project) {
    dplyr::tbl(pool, "memos") %>%
        dplyr::filter(.data$project_id == local(as.integer(project))) %>%
        dplyr::select(
            memo_id,
            memo_text = text,
            user_id
        ) %>%
        dplyr::left_join(
            dplyr::tbl(pool, "users") %>%
                dplyr::select(user_id, user_name),
            by = "user_id"
        ) %>%
        dplyr::select(-user_id) %>%
        dplyr::collect() %>%
        dplyr::mutate(memo_title = entitle_memo(.data$memo_text)) %>%
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
memo_link <- function(ns_input, id, text) {
    js_fun <- paste0(
        "Shiny.setInputValue('",
        ns_input,
        "', this.name, {priority: 'event'});"
    )
    quote_sign <- '"'
    paste0(
        '<a class="action-button memo_name shiny-bound-input" href="#" name="',
        id,
        '" onclick=',
        quote_sign,
        js_fun,
        quote_sign,
        '">',
        ifelse(text == "", "untitled", text),
        '</a>'
    )
}

# create memo segment as link ----
memo_segment_link <- function(segment_document_id, segment_id) {
    if (!is.na(segment_id)) {
        link <- actionLink(
            paste0("segment_id-", segment_id),
            label = "Segment",
            onclick = paste0(
                "Shiny.setInputValue('analyze_link', {tab_menu: 'Annotate', doc_id: ",
                segment_document_id,
                ", segment_id: ",
                segment_id,
                "}, {priority: 'event'});"
            )
        )
        as.character(link)
    } else {
        "Free"
    }
}


# Helper function to enrich memo table
enrich_memo_table <- function(memo_table, pool, ns) {
    memos_segments_map <- dplyr::tbl(pool, "memos_segments_map") %>%
        dplyr::filter(memo_id %in% !!memo_table$memo_id) %>%
        dplyr::collect()

    segment_df <- dplyr::tbl(pool, "segments") %>%
        dplyr::select(segment_id, doc_id, segment_text) %>%
        dplyr::filter(segment_id %in% !!memos_segments_map$segment_id) %>%
        dplyr::collect()

    documents_df <- dplyr::tbl(pool, "documents") %>%
        dplyr::select(doc_id, doc_name) %>%
        dplyr::filter(doc_id %in% !!segment_df$doc_id) %>%
        dplyr::collect()

    user_df <- dplyr::tbl(pool, "users") %>%
        dplyr::filter(user_id %in% !!memo_table$user_id) %>%
        dplyr::select(user_id, user_name, user_login) %>%
        dplyr::collect()

    memo_table %>%
        dplyr::left_join(memos_segments_map, by = "memo_id") %>%
        dplyr::left_join(segment_df, by = "segment_id") %>%
        dplyr::left_join(documents_df, by = "doc_id") %>%
        dplyr::left_join(user_df, by = "user_id") %>%
        dplyr::mutate(
            memo_title = memo_link(ns("text_memo_click"), memo_id, memo_name),
            memo_type = purrr::map2_chr(doc_id, segment_id, memo_segment_link)
        ) %>%
        dplyr::arrange(dplyr::desc(memo_id)) %>%
        dplyr::select(
            memo_id,
            memo_title,
            memo_type,
            doc_name,
            memo_text,
            segment_text,
            user_name,
            user_id
        )
}

# Helper function to pin a memo
pin_memo <- function(memo_id, pool, active_project, ns) {
    pin_id <- paste0("pin_id-", memo_id)
    pinned_text <- read_memo_by_id(pool, active_project, memo_id) %>%
        dplyr::pull(memo_text)

    insertUI(
        selector = "div.content-wrapper",
        where = "afterBegin",
        div(
            id = pin_id,
            class = "pinned_memo",
            div(
                id = "pin_header",
                class = "pin_header",
                icon("thumbtack"),
                div(
                    class = "unpin",
                    actionButton(
                        paste0("unpin_", pin_id),
                        "",
                        icon("xmark"),
                        class = "unpin_btn",
                        `data-id` = pin_id,
                        onclick = paste0(
                            "Shiny.setInputValue('",
                            ns("unpin"),
                            "', this.dataset.id, {priority: 'event'})"
                        )
                    )
                )
            ),
            div(class = "inner_pin", pinned_text),
            div(id = "resize_handle", class = "resizer")
        )
    )
    golem::invoke_js("makeDraggable", list(id = pin_id))
}
