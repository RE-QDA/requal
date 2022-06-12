load_all_segments_db <- function(active_project, project_db) {
    code_id <- segment_start <- segment_end <- segment_id <- NULL
    if (isTruthy(active_project)) {
        
        con <- DBI::dbConnect(RSQLite::SQLite(),
                              project_db)
        on.exit(DBI::dbDisconnect(con))
        
        segments <- dplyr::tbl(con, "segments") %>%
            dplyr::filter(.data$project_id == as.integer(active_project)) %>%
            dplyr::select(user_id, 
                          code_id,
                          doc_id, 
                          segment_id, 
                          segment_start,
                          segment_end) %>%
            dplyr::collect()
        
        return(segments)
    } else {""}
}

load_all_docs_db <- function(active_project, project_db){
    if (isTruthy(active_project)) {
        
        con <- DBI::dbConnect(RSQLite::SQLite(),
                              project_db)
        on.exit(DBI::dbDisconnect(con))
        
        docs <- dplyr::tbl(con, "documents") %>%
            dplyr::filter(.data$project_id == as.integer(active_project)) %>%
            dplyr::collect()
        
        return(docs)
    } else {""}
}

load_users_names <- function(active_project, project_db){
    user_id <- user_name <- NULL
    if (isTruthy(active_project)) {
        
        con <- DBI::dbConnect(RSQLite::SQLite(),
                              project_db)
        on.exit(DBI::dbDisconnect(con))
        
        users <- dplyr::tbl(con, "users") %>%
            dplyr::select(user_id, 
                          user_name) %>%
            dplyr::collect()
        
        return(users)
    } else {""}
}

load_codes_names <- function(active_project, project_db){
    code_id <- code_name <- NULL
    if (isTruthy(active_project)) {
        
        con <- DBI::dbConnect(RSQLite::SQLite(),
                              project_db)
        on.exit(DBI::dbDisconnect(con))
        
        codes <- dplyr::tbl(con, "codes") %>%
            dplyr::filter(.data$project_id == as.numeric(active_project)) %>% 
            dplyr::select(code_id, 
                          code_name) %>%
            dplyr::collect()
        
        return(codes)
    } else {""}
}

calculate_code_overlap_by_users <- function(segments){
    user_id <- V1 <- V2 <- coder1_id <- coder2_id <- NULL
    segment_vector <- total_overlap <- n_char <- NULL
    coder1 <- coder2 <- coder1_missing_char <- coder2_missing_char <- NULL
    
    unique_coders <- segments %>% 
        dplyr::pull(user_id) %>% unique()
    
    # Create a combination of all coders who coded something
    coders_grid <- as.data.frame(t(utils::combn(unique_coders, m = 2))) %>% 
        dplyr::rename(coder1_id = V1, coder2_id = V2)
    
    # Calculate overlap between coders by doc_id and code_id
    # for each pair of coders
    purrr::map_df(seq_len(nrow(coders_grid)), function(x) {
        coder1_id <- coders_grid$coder1_id[x]
        coder2_id <- coders_grid$coder2_id[x]
        
        coded_segments_by_two <- segments %>% 
            dplyr::filter(user_id %in% c(coder1_id, coder2_id))
        
        fst_coder <- coded_segments_by_two %>% 
            dplyr::filter(user_id == coder1_id) %>% 
            dplyr::mutate(segment_vector = purrr::map2(segment_start, segment_end, ~c(.x:.y))) %>% 
            tidyr::unnest(., segment_vector)
        
        snd_coder <- coded_segments_by_two %>% 
            dplyr::filter(user_id == coder2_id) %>% 
            dplyr::mutate(segment_vector = purrr::map2(segment_start, segment_end, ~c(.x:.y))) %>% 
            tidyr::unnest(., segment_vector)
        
        tmp <- dplyr::full_join(
            fst_coder %>% dplyr::select(coder1 = user_id, doc_id, code_id, segment_vector), 
            snd_coder %>% dplyr::select(coder2 = user_id, doc_id, code_id, segment_vector), 
            by = c("segment_vector", "doc_id", "code_id")
        )
        
        tmp %>% 
            dplyr::group_by(code_id) %>% 
            dplyr::summarise(coder1_id = coder1_id, 
                             coder1_missing_char = sum(is.na(coder1)), 
                             coder2_id = coder2_id,
                             coder2_missing_char = sum(is.na(coder2)), 
                             n_char = dplyr::n()) %>% 
            dplyr::mutate(total_overlap = 1 - ((coder1_missing_char + coder2_missing_char) / n_char))
    })
}

calculate_segment_overlap_by_users <- function(segments){
    user_id <- V1 <- V2 <- coder1_id <- coder2_id <- NULL
    segment_vector <- total_overlap <- n_char <- NULL
    coder1 <- coder2 <- coder1_missing_char <- coder2_missing_char <- NULL
    
    unique_coders <- segments %>% 
        dplyr::pull(user_id) %>% unique()
    
    # Create a combination of all coders who coded something
    coders_grid <- as.data.frame(t(utils::combn(unique_coders, m = 2))) %>% 
        dplyr::rename(coder1_id = V1, coder2_id = V2)
    
    purrr::map_df(seq_len(nrow(coders_grid)), function(x) {
        coder1_id <- coders_grid$coder1_id[x]
        coder2_id <- coders_grid$coder2_id[x]
        
        coded_segments_by_two <- segments %>% 
            dplyr::filter(user_id %in% c(coder1_id, coder2_id))
        
        coded_segments_by_two <- segments %>% 
            dplyr::filter(user_id %in% c(coder1_id, coder2_id))
        
        fst_coder <- coded_segments_by_two %>% 
            dplyr::filter(user_id == coder1_id) %>% 
            dplyr::mutate(segment_vector = purrr::map2(segment_start, segment_end, ~c(.x:.y))) %>% 
            tidyr::unnest(., segment_vector)
        
        snd_coder <- coded_segments_by_two %>% 
            dplyr::filter(user_id == coder2_id) %>% 
            dplyr::mutate(segment_vector = purrr::map2(segment_start, segment_end, ~c(.x:.y))) %>% 
            tidyr::unnest(., segment_vector)
        
        tmp <- dplyr::full_join(
            fst_coder %>% dplyr::select(coder1 = user_id, coder1_segment_id = segment_id, 
                                        doc_id, code_id, segment_vector), 
            snd_coder %>% dplyr::select(coder2 = user_id, coder2_segment_id = segment_id, 
                                        doc_id, code_id, segment_vector), 
            by = c("segment_vector", "doc_id", "code_id")) 
        
        coder1 <- tmp %>% 
            dplyr::ungroup() %>% 
            dplyr::group_by(doc_id, code_id, coder1, coder1_segment_id) %>% 
            dplyr::summarise(overlap = length(segment_vector), 
                             is_overlap = any(!is.na(coder2) & !is.na(coder1_segment_id)), 
                             .groups = "drop") %>% 
            dplyr::rename(segment_id = coder1_segment_id) %>% 
            dplyr::mutate(coder1 = coder1_id, 
                          coder2 = coder2_id)
        
        coder2 <- tmp %>% 
            dplyr::ungroup() %>% 
            dplyr::group_by(doc_id, code_id, coder2, coder2_segment_id) %>% 
            dplyr::summarise(overlap = length(segment_vector), 
                             is_overlap = any(!is.na(coder1) & !is.na(coder2_segment_id)), 
                             .groups = "drop") %>%
            dplyr::rename(segment_id = coder2_segment_id) %>% 
            dplyr::mutate(coder1 = coder1_id, 
                          coder2 = coder2_id)
        
        dplyr::bind_rows(coder1, coder2) %>% 
            dplyr::rename(coder1_id = coder1, 
                          coder2_id = coder2)
    })
}

make_overlap_df_symmetrical <- function(df){
    dplyr::bind_rows(df, 
                     df %>% 
                         dplyr::rename(coder2_id2 = coder1_id, 
                                       coder2_name2 = coder1_name, 
                                       coder1_id2 = coder2_id, 
                                       coder1_name2 = coder2_name) %>% 
                         dplyr::rename(coder2_id = coder2_id2, 
                                       coder1_id = coder1_id2, 
                                       coder1_name = coder1_name2, 
                                       coder2_name = coder2_name2)) %>% 
        unique()
}