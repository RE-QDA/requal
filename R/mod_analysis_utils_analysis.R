# Load segments for analysis -------------------------------------------

load_segments_analysis <- function(pool,
                                  active_project,
                                  selected_codes,
                                  selected_categories,
                                  selected_docs) {

    if (isTruthy(selected_codes) | isTruthy(selected_categories)) {

        category_edges <- dplyr::tbl(pool, "categories_codes_map") %>%
        dplyr::filter(.data$category_id %in% !!selected_categories) %>%
        dplyr::pull(code_id)

      code_filter <- as.integer(unique(c(category_edges[isTruthy(category_edges)], selected_codes[isTruthy(selected_codes)])))


        segments <- dplyr::tbl(pool, "segments") %>%
            dplyr::filter(.data$project_id == as.integer(active_project)) %>%
            dplyr::filter(code_id  %in% !!code_filter) %>%
            dplyr::filter(doc_id  %in% !!as.integer(selected_docs)) %>%
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
