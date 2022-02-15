# Load segments for analysis -------------------------------------------

load_segments_analysis <- function(project_db, 
                                  active_project, 
                                  selected_codes,
                                  selected_docs) {
   
    if (isTruthy(selected_codes)) {
        
        
        con <- DBI::dbConnect(RSQLite::SQLite(),
                              project_db)
        on.exit(DBI::dbDisconnect(con))
        
        segments <- dplyr::tbl(con, "segments") %>%
            dplyr::filter(.data$project_id == as.integer(active_project)) %>%
            dplyr::filter(code_id  %in% !! as.integer(selected_codes)) %>%
            dplyr::filter(doc_id  %in% !! as.integer(selected_docs)) %>%
            dplyr::select(code_id,
                          doc_id,
                          segment_id,
                          segment_text,
                          segment_start,
                          segment_end
                          ) %>%
            dplyr::collect()
        
        
        return(segments)
        
        
    } else {as.data.frame(NULL)}
    
}



format_cutouts <- function(segment_text, segment_document, segment_code, segment_color) {


  tags$div(

    segment_text %>% 
    tags$blockquote(class = "quote", style =  paste0("border-left: 5px solid ", segment_color, "; margin-bottom: 0px !important;")),
    
tags$div(
    segment_document %>% 
      tags$div(class = "segment_badge"),
    
    segment_code %>% 
      tags$div(class = "segment_badge", style = paste0("background-color: ", segment_color, " !important;")),
    
    style = "text-align: right; margin: 0 0 10px;")
)
}
