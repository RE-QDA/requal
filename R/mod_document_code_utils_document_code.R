read_doc_db <- function(active_project, project_db) {
    doc_name <- NULL
    if (isTruthy(active_project)) {

        con <- DBI::dbConnect(RSQLite::SQLite(),
                              project_db)
        on.exit(DBI::dbDisconnect(con))

        db_docs_df <- dplyr::tbl(con, "documents") %>%
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

# Load documents to display -----------------------------

load_doc_db <- function(active_project, project_db, doc_id) {

    if (isTruthy(active_project)) {

        con <- DBI::dbConnect(RSQLite::SQLite(),
                              project_db)
        on.exit(DBI::dbDisconnect(con))

        doc_text <- dplyr::tbl(con, "documents") %>%
            dplyr::filter(.data$project_id == as.integer(active_project)) %>%
            dplyr::filter(.data$doc_id == as.integer(.env$doc_id)) %>%
            dplyr::pull(doc_text)



        return(doc_text)


    } else {""}

}

load_doc <- function(con, project_id, doc_id){
    doc_text <- NULL

        dplyr::tbl(con, "documents") %>%
        dplyr::filter(.data$project_id == as.integer(.env$project_id)) %>%
        dplyr::filter(.data$doc_id == as.integer(.env$doc_id)) %>%
        dplyr::pull(doc_text)
}

# Load segments for document display  -------------------------------------------

load_segments_db <- function(active_project, project_db, doc_id) {
    code_id <- segment_start <- segment_end <- NULL
    if (isTruthy(active_project)) {

        con <- DBI::dbConnect(RSQLite::SQLite(),
                              project_db)
        on.exit(DBI::dbDisconnect(con))

        segments <- dplyr::tbl(con, "segments") %>%
            dplyr::filter(.data$project_id == as.integer(active_project)) %>%
            dplyr::filter(.data$doc_id == as.integer(.env$doc_id)) %>%
            dplyr::select(code_id,
                          segment_start,
                          segment_end) %>%
            dplyr::collect()


        return(segments)


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
        dplyr::mutate(overlapping = dplyr::between(startOff, segment_start, segment_end) |
                          dplyr::between(endOff, segment_start, segment_end)) %>%
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

get_segment_text <- function(con, project_id, doc_id, start, end){

        load_doc(con, project_id, doc_id) %>%
        substr(start = start, stop = end)
}

# Write document segments to DB -------------------------------------------

write_segment_db <- function(
    active_project,
    project_db,
    doc_id,
    code_id,
    startOff,
    endOff
) {

    con <- DBI::dbConnect(RSQLite::SQLite(),
                          project_db)
    on.exit(DBI::dbDisconnect(con))

    # Check overlap
    # - return segments with the same document ID and code ID from the DB

        coded_segments <- dplyr::tbl(con, "segments") %>%
        dplyr::filter(.data$project_id == as.integer(active_project)) %>%
        dplyr::filter(.data$doc_id == as.integer(.env$doc_id)) %>%
        dplyr::filter(.data$code_id == as.integer(.env$code_id)) %>%
        dplyr::select(project_id, doc_id, code_id,
                      segment_start,
                      segment_end) %>%
        dplyr::collect()
    # - check overlap
    overlap <- check_overlap(coded_segments, startOff, endOff)

    if(nrow(overlap)){
        # update existing record(s)
        new_segment_df <- overlap %>%
            summarise_new_segment_range(startOff, endOff) %>%
            dplyr::mutate(segment_text = get_segment_text(con,
                                                   project_id = active_project,
                                                   doc_id,
                                                   .data$segment_start,
                                                   .data$segment_end))


        # delete_segments that are to be replaced by segment_df
        query <- glue::glue_sql("DELETE FROM segments
                       WHERE project_id = {active_project}
                       AND doc_id = {doc_id}
                       AND code_id = {code_id}
                       AND segment_start = {overlap$segment_start}
                       AND segment_end = {overlap$segment_end}", .con = con)

        purrr::walk(query, function(x) {DBI::dbExecute(con, x)})

        res <- DBI::dbWriteTable(con, "segments", new_segment_df, append = TRUE)

    }else{
        # in case of no overlap write in DB directly
        segment_df <- data.frame(project_id = active_project,
                                 doc_id = doc_id,
                                 code_id = code_id,
                                 segment_start = startOff,
                                 segment_end = endOff,
                                 segment_text = get_segment_text(con,
                                                                 project_id = active_project,
                                                                 doc_id,
                                                                 startOff,
                                                                 endOff)
        )

        res <- DBI::dbWriteTable(con, "segments", segment_df, append = TRUE)
    }

    if(res){
        log_add_segment_record(con, project_id = active_project, segment_df)
    }else{
        warning("segment not added")
    }

}

calculate_code_overlap <- function(raw_segments){

    segment_start <- segment_end <- char_no <- code_id <- segment_id <- segment_break <- NULL

    raw_segments %>%
        dplyr::mutate(char_no = purrr::map2(segment_start,
                                            segment_end,
                                            function(x, y) seq(from = x, to = y, by = 1))
                      ) %>%
        tidyr::unnest(char_no) %>%
        dplyr::group_by(char_no) %>%
        dplyr::summarise(code_id = paste0(code_id, collapse = "+"),
                         count_codes = dplyr::n(),
                         .groups = "drop") %>%
        dplyr::mutate(segment_break = code_id != dplyr::lag(code_id) |
                          char_no != dplyr::lag(char_no) + 1) %>%
        dplyr::mutate(segment_break = ifelse(is.na(segment_break), FALSE, segment_break)) %>%
        dplyr::mutate(segment_id = cumsum(segment_break)) %>%
        dplyr::group_by(segment_id, code_id) %>%
        dplyr::summarise(segment_start = min(char_no),
                         segment_end = max(char_no),
                         .groups = "drop")
}

# Generate text to be displayed -----

load_doc_to_display <- function(active_project,
                                project_db,
                                doc_selector,
                                codebook,
                                ns){


    raw_text <- load_doc_db(active_project, project_db, doc_selector)

    coded_segments <- load_segments_db(active_project,
                                       project_db,
                                       doc_selector)
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
        
        initial_row <- tibble::tibble(code_id = "_NA",
                                      segment_start = 0,
                                      segment_end = nchar(raw_text)+1,
                                      tag_start = "<article><p class='docpar'>",
                                      tag_end = "</p></article>")

        
browser()
        content_df <- dplyr::bind_rows(initial_row,
                                       coded_segments %>% 
                                           dplyr::mutate(code_id = as.character(code_id))) %>% 
            tidyr::pivot_longer(cols = c(segment_start, segment_end),
                                values_to = "position_start", 
                                names_to = "position_type",
                                values_drop_na = TRUE
                                    ) %>% 
            dplyr::left_join(code_names_lookup, by = c("code_id")) %>%
            dplyr::mutate(tag_end = ifelse(is.na(tag_end),
                                            "</b>",
                                           tag_end),
                          tag_start = ifelse(is.na(tag_start),
                                           
                                           
                                           paste0('<b id="',
                                                  code_id,
                                                  '" class="segment" style="padding:0; background-color:',
                                                  code_color,
                                                  '" title="',
                                                  code_name,
                                                  '">'),
                                           tag_start)) %>% 
            dplyr::group_by(position_start, position_type) %>% 
            dplyr::summarise(tag_start = paste(tag_start, collapse = ""),
                             tag_end = paste(tag_end, collapse = ""),
                             .groups = "drop")  %>% 
            dplyr::group_by(position_start) %>% 
            dplyr::transmute(tag = ifelse(position_type == "segment_start",
                                       tag_start, 
                                       tag_end)) %>% 
           dplyr::ungroup() %>% 
           dplyr::mutate(position_end = (dplyr::lead(position_start, default = max(position_start)+1)),
                         position_end = ifelse(is.na(position_end),
                                               position_start, 
                                               position_end),
                         position_start = position_start+1) 
           
            
        
       html_content <- paste0(purrr::pmap_chr(list(content_df$position_start,
                                                   content_df$position_end,
                                                   content_df$tag),
                                              ~paste0(..3, 
                                                      substr(
                                                                 
                                                          raw_text, 
                                                             ..1, 
                                                             ..2))),
                              collapse = "") %>% 
           stringr::str_replace_all("[\\n\\r]",
                                    "<span class='br'>\\&#8203</span></p><p class='docpar'>")
         


    }else{

        df_non_coded <- paste0(
            "<article><p class='document_par'>",
            
            raw_text %>%
            stringr::str_replace_all("[\\n\\r]",
                               "</p><div class='br'>\\&#8203</div><p class='docpar'>"),
            
            "</p></article")

    }
}


# Load codes for a segment -------------------------------------------

load_segment_codes_db <- function(active_project, 
                                  project_db,
                                  active_doc,
                                  marked_codes) {

        con <- DBI::dbConnect(RSQLite::SQLite(),
                              project_db)
        on.exit(DBI::dbDisconnect(con))

        segment_codes_df <- dplyr::tbl(con, "segments") %>%
            dplyr::inner_join( dplyr::tbl(con, "codes") %>%
                                   dplyr::select(code_id, 
                                                 code_name
                                                 ),
                               by = "code_id"
                               ) %>%
            dplyr::filter(.data$project_id == as.integer(active_project) &
                          .data$doc_id == as.integer(active_doc)) %>%
            dplyr::filter(dplyr::between(marked_codes,
                                         .data$segment_start,
                                         .data$segment_end)) %>%
            dplyr::select(code_id, code_name, segment_id) %>%
            dplyr::collect()

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

delete_segment_codes_db <- function(project_db,
                                    active_project,
                                    doc_id,
                                    segment_id) {

    con <- DBI::dbConnect(RSQLite::SQLite(),
                          project_db)
    on.exit(DBI::dbDisconnect(con))

    # delete code from a segment
    query <- glue::glue_sql("DELETE FROM segments
                       WHERE project_id = {active_project}
                       AND doc_id = {doc_id}
                       AND segment_id = {segment_id}", .con = con)

    purrr::walk(query, function(x) {DBI::dbExecute(con, x)})
    
    log_delete_segment_record(con, project_id = active_project, segment_id)

}


# Generate coding tools -------------------------------------------------


generate_coding_tools <- function(ns, code_id, code_name, code_color) {

    actionButton(inputId = ns(code_id),
               label = code_name,
               name = code_id,
               class = "code-button",
               title = code_name,
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
