#' reproducibility UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_reproducibility_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("metrics_select"),
      "Select metrics",
      choices = c("",
        # "Total overlap [character]" = "total",
        # "Overlap by code [character]" = "by_code",
        # "Overlap by coder [character]" = "by_user",
        # "Overlap by coder and code [character]" = "by_user_code",
        "Total overlap [segments]" = "total_segment",
        "Overlap by code [segments]" = "by_code_segment",
        "Overlap by coder [segments]" = "by_user_segment",
        "Overlap by coder and code [segments]" = "by_user_code_segment",
        "Browse documents" = "docs"
      )
    ),
    uiOutput(ns("docs_select")),
    uiOutput(ns("code_select")),
    actionButton(ns("calculate"), "Calculate"),
    uiOutput(ns("overlap_table")),
    uiOutput(ns("overlap_documents")),
    plotOutput(ns("overlap_plot"))
  )
}

#' reproducibility Server Functions
#'
#' @noRd
mod_reproducibility_server <- function(id, glob) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    reproducibility_message <- "Reproducibility measures cannot be computed for projects with one active coder."

    observeEvent({
      req(input$metrics_select == "docs")
      # input$calculate
    }, {
      output$docs_select <- renderUI({
        # browser()
        if(isTruthy(input$docs_select)){
          selected_doc <- input$docs_select
        }else{
          selected_doc <- NULL
        }
        selectInput(ns("docs_select"), "Select document", 
                    choices = c("", glob$documents), 
                    selected = selected_doc)  
      })
      
      output$code_select <- renderUI({
        if(isTruthy(input$code_select)){
          selected_code <- input$code_select
        }else{
          selected_code <- NULL
        }
        selectInput(ns("code_select"), "Select code", 
                    choices = c("", stats::setNames(
                      glob$codebook$code_id,
                      glob$codebook$code_name
                    )),
                    selected = selected_code)  
      })
    })    
    
    # total_segment ----
        observeEvent({req(input$metrics_select == "total_segment")
                    input$calculate}, {
          segments <- load_all_segments_db(
            pool = glob$pool,
            active_project = glob$active_project
          )

          if (length(unique(segments$user_id)) > 1) {
            overlap_df <- calculate_segment_overlap_by_users(segments) %>%
              dplyr::summarise(
                `Total Overlap` = mean(is_overlap),
                `N segments` = length(unique(segment_id)),
                `N coders` = length(unique(c(coder1_id, coder2_id)))
              )

            output$overlap_documents <- NULL
            output$overlap_plot <- NULL
            output$overlap_table <- renderTable({
              overlap_df
            })
          } else {
            output$overlap_table <- renderText(reproducibility_message)
          }
        })
      
   # by_code_segment ----
        observeEvent({req(input$metrics_select == "by_code_segment") 
                    input$calculate}, {
          segments <- load_all_segments_db(
            pool = glob$pool,
            active_project = glob$active_project
          )

          codes <- load_codes_names(
            active_project = glob$active_project,
            pool = glob$pool
          )

          if (length(unique(segments$user_id)) > 1) {
            overlap_df <- calculate_segment_overlap_by_users(segments) %>%
              dplyr::group_by(code_id) %>%
              dplyr::summarise(
                total_overlap = mean(is_overlap),
                n_segments = length(unique(segment_id)),
                n_coders = length(unique(c(coder1_id, coder2_id)))
              ) %>%
              dplyr::left_join(., codes, by = "code_id") %>%
              dplyr::select(
                code_name, total_overlap,
                n_segments, n_coders
              ) %>%
              dplyr::arrange(dplyr::desc(total_overlap)) %>%
              dplyr::rename(
                `Code name` = code_name,
                `Total overlap` = total_overlap,
                `N segments` = n_segments,
                `N coders` = n_coders
              )

            output$overlap_documents <- NULL
            output$overlap_plot <- NULL
            output$overlap_table <- renderTable({
              overlap_df
            })
          } else {
            output$overlap_table <- renderText(reproducibility_message)
          }
        })
      
    # by_user_segment ----
        observeEvent({req(input$metrics_select == "by_user_segment")
                    input$calculate}, {
          segments <- load_all_segments_db(
            pool = glob$pool,
            active_project = glob$active_project
          )

          users <- load_users_names(
            pool = glob$pool,
            active_project = glob$active_project
          )

          if (length(unique(segments$user_id)) > 1) {
            overlap_df <- calculate_segment_overlap_by_users(segments) %>%
              join_user_names(., users) %>%
              dplyr::group_by(coder1_name, coder2_name, coder1_id, coder2_id) %>%
              dplyr::summarise(
                total_overlap = mean(is_overlap),
                .groups = "drop"
              ) %>%
              make_overlap_df_symmetrical()

            overlap_heatmap <- create_overlap_heatmap(overlap_df, fill = total_overlap)

            output$overlap_documents <- NULL
            output$overlap_table <- NULL
            output$overlap_plot <- renderPlot({
              overlap_heatmap
            })
          } else {
            output$overlap_table <- renderText(reproducibility_message)
          }
        })
      
    # by_user_code_segment ----
        observeEvent({req(input$metrics_select == "by_user_code_segment")
                input$calculate}, {
          segments <- load_all_segments_db(
            pool = glob$pool,
            active_project = glob$active_project
          )
          users <- load_users_names(
            pool = glob$pool,
            active_project = glob$active_project
          )
          codes <- load_codes_names(
            pool = glob$pool,
            active_project = glob$active_project
          )

          if (length(unique(segments$user_id)) > 1) {
            overlap_df <- calculate_segment_overlap_by_users(segments) %>%
              dplyr::group_by(code_id, coder1_id, coder2_id) %>%
              dplyr::summarise(
                total_overlap = mean(is_overlap),
                .groups = "drop"
              ) %>%
              join_user_names(., users) %>%
              dplyr::left_join(., codes, by = "code_id") %>%
              make_overlap_df_symmetrical()

            overlap_heatmap <- create_overlap_heatmap(overlap_df, fill = total_overlap) +
              ggplot2::facet_wrap(ggplot2::vars(code_name))

            output$overlap_documents <- NULL
            output$overlap_table <- NULL
            output$overlap_plot <- renderPlot({
              overlap_heatmap
            })
          } else {
            output$overlap_table <- renderText(reproducibility_message)
          }
        })
      

   # docs ----
        observeEvent({req(input$metrics_select == "docs")
                input$calculate}, {
          
          req(input$docs_select)          
          docs <- load_all_docs_db(
                    pool = glob$pool,
                    active_project = glob$active_project
                  )
          
          DOC_ID <- input$docs_select
          CODE_ID <- input$code_select
          
          doc_to_display <- docs %>%
            dplyr::filter(doc_id == DOC_ID) %>%
            dplyr::pull(doc_text)

          all_segments <- load_all_segments_db(
            pool = glob$pool,
            active_project = glob$active_project
          ) %>% dplyr::filter(doc_id == DOC_ID)
          
          segments <- all_segments %>%
            dplyr::filter(code_id == CODE_ID)

          if (length(unique(segments$user_id)) > 1) {
            overlap <- calculate_coders_overlap(segments)
            max_n <- max(overlap$n)
            palette <- viridisLite::viridis(max_n)
            font_colour <- c(rep("white", times = floor(max_n / 2)), 
                             rep("black", times = ceiling(max_n / 2)))
            
            overlap_df <- overlap %>%
              dplyr::rename(
                segment_start = min_intersect,
                segment_end = max_intersect
              ) %>%
              tidyr::pivot_longer(
                cols = c(segment_start, segment_end),
                values_to = "position_start",
                names_to = "position_type",
                values_drop_na = TRUE
              ) %>%
              dplyr::mutate(
                tag_end = "</b>",
                tag_start = paste0(
                  '<b id="consensus_',
                  as.character(n),
                  '" title="', 
                  coders,
                  '" class="segment" style="padding:0; background-color:',
                  palette[n],
                  '; color:', 
                  font_colour[n],
                  '">'
                )
              ) %>%
              dplyr::bind_rows(
                # start doc
                tibble::tibble(
                  position_start = 0,
                  position_type = "segment_start",
                  tag_start = "<article><p class='docpar'>"
                ),

                # content
                .,
                # end doc
                tibble::tibble(
                  position_start = nchar(doc_to_display),
                  position_type = "segment_end",
                  tag_end = "</p></article>"
                )
              ) %>%
              dplyr::mutate(position_start = ifelse(position_type == "segment_end",
                position_start + 1,
                position_start
              )) %>%
              dplyr::group_by(position_start, position_type) %>%
              dplyr::summarise(
                tag_start = paste(tag_start, collapse = ""),
                tag_end = paste(tag_end, collapse = ""),
                .groups = "drop"
              ) %>%
              dplyr::group_by(position_start) %>%
              dplyr::transmute(tag = ifelse(position_type == "segment_start",
                tag_start,
                tag_end
              )) %>%
              dplyr::ungroup() %>%
              dplyr::mutate(position_end = dplyr::lead(position_start - 1, default = max(position_start)))

            legend <- purrr::map_chr(1:max_n, function(x) {
              paste0('<li><b id="consensus_',
                     as.character(x),
                     '" class="segment" style="padding:0; background-color:',
                     palette[x],
                     '; color:', 
                     font_colour[x],
                     '">Segment coded by ', 
                     as.character(x), ifelse(x == 1, " coder", " coders"), 
                     '</b></li>')
            }) %>% paste0(., collapse = "")
            
            html_content <- paste0(
              purrr::pmap_chr(
              list(
                overlap_df$position_start,
                overlap_df$position_end,
                overlap_df$tag
              ),
              ~ paste0(
                ..3,
                substr(
                  doc_to_display,
                  ..1,
                  ..2
                )
              )
            ),
            collapse = ""
            ) %>%
              stringr::str_replace_all(
                "[\\n\\r]",
                "<span class='br'>\\&#8203</span></p><p class='docpar'>"
              ) %>% 
              paste0("<h2>Colour guide</h2><p>Overlap between coders is highlighted as follows:<ul>", 
                     legend, 
                     "</ul></p><h2>Coded document</h2>", 
                     .)

            output$overlap_documents <- renderText({
              html_content
            })
            output$overlap_table <- NULL
            output$overlap_plot <- NULL
          } else {
            if(!(length(unique(all_segments$user_id)) > 1)){
              warning_message <- "Selected document was not coded by multiple coders"
            }else{
              warning_message <- "Selected code was not used in the selected document"
            }
            output$overlap_documents <- renderText(warning_message)
            output$overlap_table <- NULL
            output$overlap_plot <- NULL
          }
        })
      
    })

}
