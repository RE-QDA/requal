# Load segments for analysis -------------------------------------------

load_segments_analysis <- function(pool,
                                  active_project,
                                  selected_codes,
                                  selected_categories,
                                  selected_docs,
                                  selected_users = NULL) {

    if (isTruthy(selected_codes) | isTruthy(selected_categories)) {

        category_edges <- dplyr::tbl(pool, "categories_codes_map") %>%
        dplyr::filter(.data$category_id %in% !!selected_categories) %>%
        dplyr::pull(code_id)

      code_filter <- as.integer(unique(c(category_edges[isTruthy(category_edges)], selected_codes[isTruthy(selected_codes)])))
      
        segments_input <- dplyr::tbl(pool, "segments") %>%
            dplyr::filter(.data$project_id == as.integer(active_project)) %>%
            dplyr::filter(code_id  %in% !!code_filter) %>%
            dplyr::filter(doc_id  %in% !!as.integer(selected_docs)) %>%
            dplyr::select(code_id,
                          doc_id,
                          segment_id,
                          segment_text,
                          segment_start,
                          segment_end, 
                          user_id
                          ) %>%
            dplyr::collect()

          if (!is.null(selected_users)) {
            segments_input <- segments_input %>%
             dplyr::filter(user_id  %in% !!as.integer(selected_users))
            } 

            users <- dplyr::tbl(pool, "users") %>% 
                     dplyr::select(user_id, user_name) %>% 
                     dplyr::collect()

            segments <- dplyr::left_join(segments_input, users, by = "user_id")


        return(segments)


    } else {as.data.frame(NULL)}

}



format_segments <- function(segment_id, segment_text, segment_document_id, segment_document_name, segment_code, segment_color) {


  tags$div(

    segment_text %>%
    tags$blockquote(class = "quote", style =  paste0("border-left: 5px solid ", segment_color, "; margin-bottom: 0px !important;")),

tags$div(
  tags$div(class = "segment_badge",
       actionLink(paste0("segment_id-", segment_id), label = segment_document_name, 
       onclick =  paste0("Shiny.setInputValue('analyze_link', {tab_menu: 'Annotate', doc_id: ", segment_document_id,", segment_id: ", segment_id, "}, {priority: 'event'});")
      )
       ),

    segment_code %>%
      tags$div(class = "segment_badge", style = paste0("background-color: ", segment_color, " !important;")),

    style = "text-align: right; margin: 0 0 10px;")
)
}
