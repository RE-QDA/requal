#' Import data from RQDA to Requal
#'
#' @param rqda_file Path to RQDA database
#' @param requal_connection Connection to REQUAL database
#'
#' @export
#' @importFrom rlang .data
import_rqda <- function(rqda_file, requal_connection){
    
    # Load RQDA DB
    rqda_con <- DBI::dbConnect(RSQLite::SQLite(), rqda_file)
    
    # Load Data from RQDA
    project_df <- dplyr::tbl(rqda_con, "project") %>% 
        dplyr::collect() %>% 
        dplyr::mutate(project_name = "RQDA import", 
                      project_description = paste0(memo, "| RQDA version ", 
                                                   databaseversion)) %>% 
        dplyr::select(project_name, project_description)
    
    rqda_documents <- dplyr::tbl(rqda_con, "source") %>% 
        dplyr::filter(.data$status == 1) %>% 
        dplyr::collect() %>% 
        dplyr::select(doc_id = id, 
                      doc_name = name, 
                      doc_description = memo, 
                      doc_text = file)
    
    rqda_codes <- dplyr::tbl(rqda_con, "freecode") %>% 
        dplyr::filter(.data$status == 1) %>% 
        dplyr::collect() %>% 
        dplyr::select(code_id = id, 
                      code_name = name, 
                      code_description = memo, 
                      code_color = color)
    
    rqda_segments <- dplyr::tbl(rqda_con, "coding") %>% 
        dplyr::filter(.data$status == 1) %>% 
        dplyr::collect() %>% 
        dplyr::select(doc_id = fid, 
                      code_id = cid, 
                      segment_start = selfirst, 
                      segment_end = selend)
    
    # Create requal schema
    requal:::create_db_schema(requal_connection)
    
    # Import to requal
    create_project_record(requal_connection, project_df)
    
    requal_project_id <- dplyr::tbl(requal_connection, "projects") %>% 
        dplyr::collect() %>% 
        dplyr::pull(project_id) %>% 
        tail(1)
    
    documents_df <- rqda_documents %>% 
        dplyr::mutate(project_id = requal_project_id)
    purrr::walk(1:nrow(documents_df), function(x) {
        add_documents_record(requal_connection, requal_project_id, documents_df[x, ])
    })
    
    codes_df <- rqda_codes %>% 
        dplyr::mutate(project_id = requal_project_id, 
                      code_color = ifelse(is.na(code_color), 
                                          "rgb(255,255,0)", code_color))
    purrr::walk(1:nrow(codes_df), function(x) {
        add_codes_record(requal_connection, requal_project_id, codes_df[x, ])    
    })
    
    
    segments_df <- rqda_segments %>% 
        dplyr::mutate(project_id = requal_project_id, 
                      segment_text = purrr::pmap_chr(
                          list(doc_id, segment_start, segment_end), 
                          function(doc, start, end) {
                              get_segment_text(requal_connection, requal_project_id, 
                                               doc, start, end)
                              }))
    
    purrr::walk(1:nrow(segments_df), function(x) {
        DBI::dbWriteTable(requal_connection, "segments", segments_df[x, ], append = TRUE)
        log_add_segment_record(requal_connection, requal_project_id, segments_df[x, ])    
    })
}
