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
calculate_code_overlap <- function(raw_segments) {
is_within_range <- function(position, range) {
  position >= range[1] & position <= range[2]
}
ranges <- purrr::map2(raw_segments$segment_start, raw_segments$segment_end, range)

positions <- sort(c(raw_segments$segment_start, raw_segments$segment_end))
duplicated_positions <- purrr::map(positions, .f = function(x){
  times <- sum(purrr::map_lgl(ranges, ~is_within_range(x, .x)))
  rep(x, times)

})
updated_positions <- unlist(duplicated_positions)
highligths_df <- tibble::tibble(
segment_start = updated_positions[seq_along(updated_positions) %% 2 == 1],
segment_end = updated_positions[seq_along(updated_positions) %% 2 == 0]
)
highligths_df |> 
dplyr::mutate(segment_id = purrr::map2_chr(segment_start, segment_end, .f = function(x,y){
  raw_segments |> 
 dplyr::filter(segment_end >= y)    |> 
 dplyr::filter(segment_start <= x) |> 
 dplyr::pull(segment_id) |> 
  paste(collapse = "+")
})) |> 
dplyr::mutate(code_id = purrr::map2_chr(segment_start, segment_end, .f = function(x,y){
  raw_segments |> 
 dplyr::filter(segment_end >= y)    |> 
 dplyr::filter(segment_start <= x) |> 
 dplyr::pull(code_id) |> 
  paste(collapse = "+")
}))   |> 
dplyr::arrange(segment_start, segment_end)  |> 
 dplyr::mutate(
 highlight_id = cumsum(segment_id != dplyr::lag(segment_id, default = ""))
)  |> 
dplyr::summarise(
  segment_id = unique(segment_id),
  code_id = unique(code_id),
  segment_start = min(segment_start),
  segment_end = max(segment_end),
  .by = highlight_id
) 

}

# Generate text to be displayed -----

load_doc_to_display <- function(pool, 
                                active_project,
                                user,
                                doc_selector,
                                codebook,
                                highlight,
                                ns){
    position_type <- position_start <- tag_start <- tag_end <- NULL
    raw_text <- load_doc_db(pool, active_project, doc_selector)
    
    coded_segments <- load_segments_db(pool, 
                                       active_project,
                                       user,
                                       doc_selector) %>% 
        calculate_code_overlap()
    
    
    code_names <- codebook %>%
        dplyr::select(code_id, code_name, code_color) %>%
        dplyr::mutate(code_id = as.character(code_id))
    
    if(nrow(coded_segments)){
        
        distinct_code_ids <- as.character(
            coded_segments %>%
                dplyr::distinct(code_id) %>%
                dplyr::pull(code_id)
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
       
        content_df <- coded_segments %>% 
            dplyr::filter(segment_start <= segment_end) %>% # patch for failing calculate_code_overlap() function in case of identical code positions
            dplyr::mutate(code_id = as.character(code_id)) %>% 
            tidyr::pivot_longer(cols = c(segment_start, segment_end),
                                values_to = "position_start", 
                                names_to = "position_type",
                                values_drop_na = TRUE) %>% 
            dplyr::left_join(code_names_lookup, by = c("code_id")) %>%
            dplyr::mutate(tag_end = "</b>",
                          tag_start = paste0('<b id="',
                                             code_id,
                                             highlight_style(highlight),
                                             code_color,
                                             '" data-color="',  
                                             code_color,
                                             '" data-segment_start="',  
                                             position_start,                                              
                                             '" title="',
                                             code_name,
                                              '" onclick="Shiny.setInputValue(\'', ns("clicked_title"), '\', this.title, {priority: \'event\'});">')) %>% 
            dplyr::bind_rows(
                # start doc
                tibble::tibble(position_start = 0,
                               position_type =  "segment_start",
                               tag_start = "<article id='article'><p class='docpar'>"),
                # content
                .,
                # end doc
                tibble::tibble(position_start = nchar(raw_text),
                               position_type = "segment_end",
                               tag_end = "</p></article>")
            ) %>% 
            dplyr::mutate(position_start = ifelse(position_type == "segment_end",
                                                  position_start+1,
                                                  position_start)) %>% 
            dplyr::group_by(position_start, position_type) %>% 
            dplyr::summarise(tag_start = paste(tag_start, collapse = ""),
                             tag_end = paste(tag_end, collapse = ""),
                             .groups = "drop")  %>% 
            dplyr::group_by(position_start) %>% 
            dplyr::transmute(tag = ifelse(position_type == "segment_start",
                                          tag_start, 
                                          tag_end)) %>% 
            dplyr::ungroup()  %>% 
            dplyr::mutate(position_end = dplyr::lead(position_start-1, default = max(position_start))) 
        
        
        html_content <- paste0(purrr::pmap_chr(list(content_df$position_start,
                                                    content_df$position_end,
                                                    content_df$tag),
                                               ~paste0(..3, 
                                                       htmltools::htmlEscape(substr(
                                                           
                                                           raw_text, 
                                                           ..1, 
                                                           ..2)))),
                               collapse = "") %>% 
            stringr::str_replace_all("[\\n\\r]",
                                     "<span class='br'>\\&#8203</span></p><p class='docpar'>")
        
        
        
    }else{
        
        df_non_coded <- paste0(
            "<article id='article'><p class='docpar'>",
            
            htmltools::htmlEscape(raw_text) %>%
                stringr::str_replace_all("[\\n\\r]",
                                         "<span class='br'>\\&#8203</span></p><p class='docpar'>"),
            
            "</p></article")
        
    }
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
    
    ids <- unlist(strsplit(string_id, split = "\\+"))
    
    names <- code_names_df %>%
        dplyr::filter(code_id %in% ids) %>%
        dplyr::pull(code_name)
    
    paste(names, collapse = " | ")
    
}


# Color helper for overlaying codes ----

blend_colors <- function(string_id, code_names) {
    
    ids <- unlist(strsplit(string_id, split = "\\+"))
    
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

highlight_style <- function(choice) {

    switch(choice,
           underline = '" class="segment" style="padding:0; text-decoration: underline; text-decoration-color:',
           background = '" class="segment" style="padding:0; background-color:',
    )

    
}