read_doc_db <- function(pool, active_project) {
    doc_name <- NULL
    if (isTruthy(active_project)) {
        
        db_docs_df <- dplyr::tbl(pool, "documents") %>%
            dplyr::filter(.data$project_id == as.integer(active_project)) %>%
            dplyr::select(doc_name, doc_id) %>%
            dplyr::collect() %>%
            dplyr::mutate(doc_name = ifelse(is.na(doc_name),
                                            as.character(doc_id),
                                            doc_name))
        
        named_ids <- db_docs_df %>%
            dplyr::select(dplyr::ends_with("_name")) %>%
            dplyr::pull()
        ids <- db_docs_df %>%
            dplyr::select(dplyr::ends_with("_id")) %>%
            dplyr::pull()
        
        choices <- ids
        names(choices) <- named_ids
        
        return(choices)
        
        
    } else {""}
    
}

read_visible_docs <- function(pool, active_project, user_id){
    docs <- dplyr::tbl(pool, "documents") %>% 
        dplyr::filter(.data$project_id == !!as.integer(active_project), 
                      .data$user_id == !!as.integer(user_id)) %>%
        dplyr::select(doc_name, doc_id) %>%
        dplyr::collect() %>%
        dplyr::mutate(doc_name = ifelse(is.na(doc_name),
                                        as.character(doc_id),
                                        doc_name))
    
    stats::setNames(docs$doc_id, docs$doc_name)
}

# Load documents to display -----------------------------

load_doc_db <- function(pool, active_project, doc_id) {
    
    if (isTruthy(active_project)) {
        
        doc_text <- dplyr::tbl(pool, "documents") %>%
            dplyr::filter(.data$project_id == as.integer(active_project), 
                          .data$doc_id == as.integer(.env$doc_id)) %>%
            dplyr::pull(doc_text)
        
        return(doc_text)
        
    } else {""}
    
}

load_doc <- function(pool, project_id, doc_id){
    doc_text <- NULL
    
    dplyr::tbl(pool, "documents") %>%
        dplyr::filter(.data$project_id == as.integer(.env$project_id), 
                      .data$doc_id == as.integer(.env$doc_id)) %>%
        dplyr::pull(doc_text)
}

# Load segments for document display  -------------------------------------------

load_segments_db <- function(pool, active_project, user, doc_id) {
    code_id <- segment_start <- segment_end <- NULL
    if (isTruthy(active_project)) {
        
        segments <- dplyr::tbl(pool, "segments") %>%
            dplyr::filter(.data$project_id == as.integer(active_project), 
                          .data$doc_id == as.integer(.env$doc_id)) %>%
            dplyr::select(segment_id,
                          code_id,
                          segment_start,
                          segment_end, 
                          user_id) %>%
            dplyr::collect()
        
        if(user$data$annotation_other_view == 0){
            segments <- segments %>% 
                dplyr::filter(user_id == !!user$user_id)
        }
        
        return(segments %>% 
                   dplyr::select(segment_id, code_id, segment_start, segment_end))
        
    } else {""}
}

# Check overlap between already coded segments and newly coded segment
# using its start and end position
check_overlap <- function(coded_segments, startOff, endOff){
    overlapping <- segment_start <- segment_end <- NULL
    if(!nrow(coded_segments)){
        return(data.frame())
    }
    
    overlapping_segments <- coded_segments %>%
        dplyr::rowwise() %>%
        dplyr::mutate(overlapping = dplyr::between(segment_start, startOff, endOff) |
                          dplyr::between(segment_end, startOff, endOff)) %>%
        dplyr::filter(overlapping)
    
    if(!nrow(overlapping_segments)){
        return(data.frame())
    }
    
    overlapping_segments
}

summarise_new_segment_range <- function(overlap_df, startOff, endOff){
    code_id <- segment_start <- segment_end <- NULL
    overlap_df %>%
        dplyr::group_by(project_id, doc_id, code_id) %>%
        dplyr::summarise(segment_start = min(startOff, segment_start),
                         segment_end = max(endOff, segment_end)) %>%
        dplyr::ungroup()
}

get_segment_text <- function(pool, project_id, doc_id, start, end){
    load_doc(pool, project_id, doc_id) %>%
        substr(start = start, stop = end)
}

# Write document segments to DB -------------------------------------------

write_segment_db <- function(
        pool, 
        active_project,
        user_id,
        doc_id,
        code_id,
        startOff,
        endOff
) {
    
    # Check overlap
    # - return segments with the same document ID and code ID from the DB
    coded_segments <- dplyr::tbl(pool, "segments") %>%
        dplyr::filter(.data$project_id == as.integer(active_project), 
                      .data$user_id == as.integer(.env$user_id), 
                      .data$doc_id == as.integer(.env$doc_id), 
                      .data$code_id == as.integer(.env$code_id)) %>%
        dplyr::select(project_id, user_id, doc_id, code_id, segment_id,
                      segment_start,
                      segment_end) %>%
        dplyr::collect()
    # - check overlap
    overlap <- check_overlap(coded_segments, startOff, endOff)
    
    if(nrow(overlap)){
        # update existing record(s)
        segment_df <- overlap %>%
            summarise_new_segment_range(startOff, endOff) %>%
            dplyr::mutate(segment_text = get_segment_text(pool,
                                                          project_id = active_project,
                                                          doc_id,
                                                          .data$segment_start,
                                                          .data$segment_end), 
                          user_id = .env$user_id)
        
        
        # delete_segments that are to be replaced by segment_df
        query <- glue::glue_sql("DELETE FROM segments
                       WHERE project_id = {active_project}
                       AND doc_id = {doc_id}
                       AND code_id = {code_id}
                       AND user_id = {user_id}
                       AND segment_start = {overlap$segment_start}
                       AND segment_end = {overlap$segment_end}", .con = pool)
        
        purrr::walk(query, function(x) {
            DBI::dbExecute(pool, x)
            log_delete_segment_record(pool, active_project, overlap$segment_id, user_id)
        })
        
        res <- DBI::dbWriteTable(pool, "segments", segment_df, append = TRUE, row.names = FALSE)
        
    }else{
        # in case of no overlap write in DB directly
        segment_df <- data.frame(project_id = active_project,
                                 user_id = user_id,
                                 doc_id = doc_id,
                                 code_id = code_id,
                                 segment_start = startOff,
                                 segment_end = endOff,
                                 segment_text = get_segment_text(pool, 
                                                                 project_id = active_project,
                                                                 doc_id,
                                                                 startOff,
                                                                 endOff)
        )
        
        res <- DBI::dbWriteTable(pool, "segments", segment_df, append = TRUE, row.names = FALSE)
    }
    
    if(res){
        written_segment_id <- dplyr::tbl(pool, "segments") %>% 
            dplyr::filter(.data$project_id == !!segment_df$project_id, 
                          .data$doc_id == !!segment_df$doc_id, 
                          .data$code_id == !!segment_df$code_id, 
                          .data$segment_start == !!segment_df$segment_start, 
                          .data$segment_end == !!segment_df$segment_end, 
                          .data$user_id == !!segment_df$user_id) %>% 
            dplyr::pull(segment_id)
        
        log_add_segment_record(pool, project_id = active_project, segment_df %>% 
                                   dplyr::mutate(segment_id = written_segment_id), 
                               user_id = user_id)
    }else{
        warning("segment not added")
    }
    
}

# calculate code overlap for doc display ----
calculate_code_overlap <- function(raw_segments, paragraphs) {

events_df <- dplyr::bind_rows(
    raw_segments, paragraphs
)

extra_start_events <- purrr::map_int(unique(events_df$segment_end), .f = function(x) {
    active_segments <- events_df %>%
    dplyr::filter(x >= (segment_start) & x <= (segment_end)) 
    if (any(active_segments$segment_end > x)) {
        span_event <- x + 1
    } else {
        0
    }
})
extra_end_events <- purrr::map_int(unique(events_df$segment_start), .f = function(x) {
    active_segments <- events_df %>%
    dplyr::filter(x >= (segment_start) & x <= (segment_end)) 
    if (any(active_segments$segment_start < x)) {
        span_event <- x - 1
    } else {
        0
    }
})

start_events <- sort(c(unique(events_df$segment_start), extra_start_events[extra_start_events > 0] ))
end_events <- sort(c(unique(events_df$segment_end), extra_end_events[extra_end_events > 0] ))

res_prep <- tibble::tibble(
    segment_start = start_events,
    segment_end = end_events
)

res <- res_prep %>%
    dplyr::mutate(
        code_id = purrr::map2(segment_start, segment_end, .f = function(s, e) {
             raw_segments %>%
                dplyr::filter(s <= segment_end & e >= segment_start) %>%
                dplyr::pull(code_id)
        }),
        segment_id = purrr::map2(segment_start, segment_end, .f = function(s, e) {
            raw_segments %>%
                dplyr::filter(s <= segment_end & e >= segment_start) %>%
                dplyr::pull(segment_id)
        })
    ) %>%
    dplyr::mutate(
        code_id = purrr::map_chr(code_id, ~ paste0(.x, collapse = "-")),
        segment_id = purrr::map_chr(segment_id, ~ paste0(.x, collapse = "-"))
    ) %>%
    tibble::rownames_to_column("highlight_id")

return(res) 
}

# Generate text to be displayed -----
load_doc_to_display <- function(pool, 
                                active_project,
                                user,
                                doc_selector,
                                codebook,
                                highlight,
                                ns){
    end <- highlight_id <- par_id <- NULL
    raw_text <- load_doc_db(pool, active_project, doc_selector)
    total_chars <- nchar(raw_text)
    paragraph_indices <- tibble::as_tibble(stringr::str_locate_all(raw_text, "\n|\r")[[1]])
    if (!nrow(paragraph_indices)) paragraph_indices  <- tibble::tibble(end = total_chars)
    paragraphs <- paragraph_indices %>%
    dplyr::transmute(
        segment_start = as.integer(dplyr::lag(end+1, default = 1)),
        segment_end = as.integer(dplyr::lead(segment_start-1, default = max(end)))
    ) %>% 
    tibble::rowid_to_column("par_id") 
    if (total_chars > max(paragraphs$segment_end)) {
        # safeguard against text without final linebreak
        paragraphs <- dplyr::bind_rows(
        paragraphs,
        tibble::tibble(
            par_id = max(paragraphs$par_id) + 1,
            segment_start = as.integer(max(paragraphs$segment_end) + 1),
            segment_end = as.integer(total_chars)
             )
        ) 
    }
    coded_segments <- load_segments_db(pool, 
                                       active_project,
                                       user,
                                       doc_selector) 


    if(nrow(coded_segments)){
        
    spans_data <- calculate_code_overlap(coded_segments, paragraphs) %>%  
      dplyr::left_join(
           paragraphs %>% 
           dplyr::select(segment_start, par_id), by = "segment_start"
           ) %>% 
           tidyr::fill(par_id, .direction = "down")
 
    code_names <- codebook %>%
        dplyr::select(code_id, code_name, code_color) %>%
        dplyr::mutate(code_id = as.character(code_id))

    distinct_code_ids <- as.character(
        unique(spans_data %>% dplyr::pull(code_id))
        )
        
    code_names_lookup <- tibble::tibble(
        code_id = distinct_code_ids,
        code_name = sapply(distinct_code_ids,
                          blend_codes,
                          code_names),
        code_color = sapply(distinct_code_ids,
                            blend_colors,
                            code_names)
        )

    spans <- spans_data %>% 
        dplyr::left_join(code_names_lookup, by = "code_id") %>%
        dplyr::mutate(text = purrr::pmap(
            list(segment_start, segment_end, highlight_id, segment_id, code_id, code_name, code_color, raw_text, highlight),
            make_span
        )) %>% 
        dplyr::select(par_id,text) %>% 
        dplyr::summarise(text = list(htmltools::p(text, class = "docpar", .noWS = "outside")), .by = "par_id") %>% 
        dplyr::mutate(text = purrr::map(text, ~ htmltools::tagAppendChild(.x, htmltools::span(HTML("&#8203"), class = "br", .noWS = "outside")))) 
     
    } else {
  
    spans <- paragraphs %>%  
        dplyr::mutate(text = purrr::pmap(
            list(segment_start, segment_end, raw_text = raw_text),
            make_span
        )) %>% 
        dplyr::select(par_id,text) %>% 
        dplyr::summarise(text = list(htmltools::p(text, span(HTML("&#8203"), class = "br", .noWS = "outside"), class = "docpar", .noWS = "outside")), .by = "par_id")
    }
    return(tags$article(id = "article", spans$text))

}


# Load codes for a segment -------------------------------------------

load_segment_codes_db <- function(pool, 
                                  active_project, 
                                  user_id,
                                  active_doc,
                                  marked_codes) {
    
    segment_codes_df <- dplyr::tbl(pool, "segments") %>%
        dplyr::inner_join(dplyr::tbl(pool, "codes") %>%
                              dplyr::select(code_id, 
                                            code_name
                              ),
                          by = "code_id", 
                          suffix = c(".x", ".y")
        ) %>%
        dplyr::filter(.data$project_id == as.integer(active_project), 
                      .data$doc_id == as.integer(active_doc)) %>%
        dplyr::filter(dplyr::between(marked_codes,
                                     .data$segment_start,
                                     .data$segment_end)) %>%
        dplyr::select(code_id, code_name, segment_id, user_id) %>%
        dplyr::collect() 
    
    if(!is.null(user_id)){
        segment_codes_df <- segment_codes_df %>% 
            dplyr::filter(.data$user_id == as.integer(.env$user_id))
    }
    
    segment_codes_df %>% 
        dplyr::select(code_id, code_name, segment_id)
}

# Parse tag position -----------

parse_tag_pos <- function(tag_postion, which_part) {
    
    startOff <- as.integer(unlist(strsplit(tag_postion, "-")))[1]
    endOff <- as.integer(unlist(strsplit(tag_postion, "-")))[2]
    
    switch(which_part,
           "start" = startOff,
           "end" = endOff,
           "both" = c(startOff, endOff)
    )
}



# Remove codes from a document ----------------

delete_segment_codes_db <- function(pool, 
                                    active_project,
                                    user_id,
                                    doc_id,
                                    segment_id) {
    
    # delete code from a segment
    query <- glue::glue_sql("DELETE FROM segments
                       WHERE project_id = {active_project}
                       AND doc_id = {doc_id}
                       AND segment_id = {segment_id}", .con = pool)
    
    purrr::walk(query, function(x) {DBI::dbExecute(pool, x)})
    
    log_delete_segment_record(pool, project_id = active_project, segment_id, user_id)
}


# Generate coding tools -------------------------------------------------


generate_coding_tools <- function(ns, code_id, code_name, code_color, code_desc) {

    actionButton(inputId = ns(code_id),
               label = code_name,
               name = code_id,
               class = "code-button",
               title = paste(code_desc),
               style = paste0("background: none;
                       width: 100%;
                       border-left: 5px solid ", code_color, ";"),
                 onclick = paste0("Shiny.setInputValue('", ns("selected_code"), "', this.name, {priority: 'event'});"))
    
    
}

# Tooltip helper for overlaying codes ----

blend_codes <- function(string_id, code_names_df) {
    
    ids <- unlist(strsplit(string_id, split = "\\-"))
    
    names <- code_names_df %>%
        dplyr::filter(code_id %in% ids) %>%
        dplyr::pull(code_name)
    
    paste(names, collapse = " | ")
    
}


# Color helper for overlaying codes ----

blend_colors <- function(string_id, code_names) {
    
    ids <- unlist(strsplit(string_id, split = "\\-"))
    
    colors_multiple <- code_names %>%
        dplyr::filter(code_id %in% ids) %>%
        dplyr::pull(code_color)
    
    color_mean <- colors_multiple %>%
        stringr::str_extract_all("\\d+") %>%
        purrr::map(as.numeric) %>%
        as.data.frame() %>%
        rowMeans() %>%
        round()
    
    color_mean_string <- paste0(color_mean, collapse = ",")
    
    paste0("rgb(", color_mean_string, ")")
    
}

# highlight/underline ----

.highlight_style <- function(choice) {
    switch(choice,
           underline = "text-decoration: underline; text-decoration-color",
           background = "background-color",
    )
}
.span_style <- function(x, style_spec = NULL) {
    excluded <- c("rgb()")
    if (!x %in% excluded) {
        if (!is.null(style_spec)) {
            paste0(style_spec, ": ", x, ";")
        } else {
            x
        }
    } else {
        NULL
    }
}
# make span  ----
make_span  <- function(segment_start, segment_end, highlight_id = NULL, segment_id = NULL, code_id = NULL, code_name = NULL, code_color = NULL, raw_text, highlight = NULL) {
        # Check if a code_id is assigned
        code_assigned <- isTruthy(code_id)
        
        # Extract the text segment and remove newlines
        text <- substr(raw_text, segment_start, segment_end)
        text <- stringr::str_replace_all(text, "\n|\r", "")

        # Return NULL if the text is empty
        # TODO - check if this condition is still needed
        if (nchar(text) < 1 && !code_assigned) {
            return(NULL)
        } else {
            # Create a span element with attributes and data
            htmltools::span(
                text,
                title = if (code_assigned) code_name else NULL,
                style = if (code_assigned) .span_style(code_color, .highlight_style(highlight)) else NULL,
                class = if (code_assigned) "segment" else NULL,
                `data-color` = if (code_assigned) code_color else NULL,
                `data-startend` = paste(segment_start, segment_end),
                `data-codes` = if (code_assigned) paste(code_id) else NULL,
                .noWS = "outside"
            ) 
        }
    }


