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

# Load documents to display  -------------------------------------------

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
    segment_start <- segment_end <- NULL
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
    
    
    # TODO: logging
    
    # if(res){
    #     log_add_document_record(con, project_id, document_df)    
    # }else{
    #     warning("document not added")
    # }
    
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

load_doc_to_display <- function(active_project, project_db, doc_selector){
    
    code_id <- segment_start <- segment_end <- code_end <- NULL
    
    raw_text <- load_doc_db(active_project, project_db, doc_selector)
    
    coded_segments <- load_segments_db(active_project, 
                                       project_db, 
                                       doc_selector)
    
    if(nrow(coded_segments)){
        
        highlighted_segments <- coded_segments %>%
            calculate_code_overlap()
        
        df <- strsplit(raw_text, "")[[1]] %>% 
            tibble::enframe() %>%
            dplyr::left_join(highlighted_segments %>% 
                                 dplyr::select(code_id, segment_start), 
                             by=c("name" = "segment_start")
                             ) %>% 
            dplyr::left_join(highlighted_segments %>% 
                                 dplyr::select(code_end = code_id, segment_end), 
                             by=c("name" = "segment_end")
                             ) %>%
            dplyr::mutate(code_end = ifelse(!is.na(code_end), 
                                            "</mark>", 
                                            ""),
                          code_id = ifelse(!is.na(code_id), 
                                           paste0('<mark id="', 
                                                  code_id, 
                                                  '" class="code" style="padding:0; background:yellow">'), 
                                           ""))
        
        paste0(df$code_id, df$value, df$code_end, collapse = "")    
        
    }else{
        
        raw_text
    }
}


# Load codes for a segment -------------------------------------------

load_segment_codes_db <- function(active_project, project_db, code_coord) {
 
    #browser()
    Off <- as.numeric(unlist(strsplit(code_coord, "-")))[1]+1
    
        con <- DBI::dbConnect(RSQLite::SQLite(),
                              project_db)
        on.exit(DBI::dbDisconnect(con))
        
        segment_codes <- dplyr::tbl(con, "segments") %>%
            dplyr::filter(.data$project_id == as.integer(active_project)) %>%
            dplyr::filter(dplyr::between(Off, 
                                         .data$segment_start,
                                         .data$segment_end)
                          ) %>% 
            dplyr::pull(code_id)
        
        
        return(segment_codes)
        
}


# Generate coding toolbox -------------------------------------------------


generate_coding_tools <- function(input_id, code_id, code_name) {
    

    tags$table(style = "border: 1px solid black; width: 100%; padding: 5%;", tags$tr(tags$td(
            actionLink(paste0(input_id, "-", code_id),
                               label = code_name,
                       style = "background: none;
                       width: 100%")
)
)
            )
          

    
    
}
