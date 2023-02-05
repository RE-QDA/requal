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
                            "Total overlap [character]" = "total",
                            "Overlap by code [character]" = "by_code",
                            "Overlap by coder [character]" = "by_user",
                            "Overlap by coder and code [character]" = "by_user_code",
                            "Total overlap [segments]" = "total_segment",
                            "Overlap by code [segments]" = "by_code_segment",
                            "Overlap by coder [segments]" = "by_user_segment",
                            "Overlap by coder and code [segments]" = "by_user_code_segment"
                )
    ),
    checkboxGroupInput(ns("repro_coders"), "Select coders:", 
                       choices = ""),
    actionButton(ns("calculate"), "Calculate"),
    uiOutput(ns("overlap_table")),
    plotOutput(ns("overlap_plot"), height = "600px")
  )
}

#' reproducibility Server Functions
#'
#' @noRd
mod_reproducibility_server <- function(id, glob) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    reproducibility_message <- "Reproducibility measures cannot be computed for projects with one active coder."
    
    observeEvent(glob$documents, {
      if (isTruthy(glob$active_project)) {
        users <- dplyr::tbl(glob$pool, "users") %>% 
          dplyr::select(user_id, user_name) %>% 
          dplyr::collect()
        
        updateCheckboxGroupInput(
          session = session, 
          "repro_coders", 
          choices = c(
            stats::setNames(
              users$user_id,
              users$user_name
            )
          )
        )
      }
    })
    
    # total ----
    observeEvent({req(input$metrics_select == "total") 
      input$calculate}, {
        segments <- load_all_segments_db(
          pool = glob$pool,
          active_project = glob$active_project
        ) %>% dplyr::filter(user_id %in% as.numeric(input$repro_coders))
        
        if (length(unique(segments$user_id)) > 1) {
          overlap_df <- calculate_code_overlap_by_users(segments) %>%
            dplyr::summarise(
              `Weighted total overlap` = stats::weighted.mean(total_overlap, n_char),
              `N characters coded` = sum(n_char),
              `N coders` = length(unique(c(coder1_id, coder2_id)))
            )
          
          output$overlap_plot <- NULL
          output$overlap_table <- renderTable({
            overlap_df
          })
        } else {
          output$overlap_table <- renderText(reproducibility_message)
        }
      })
    
    # total_segment ----
    observeEvent({req(input$metrics_select == "total_segment")
      input$calculate}, {
        segments <- load_all_segments_db(
          pool = glob$pool,
          active_project = glob$active_project
        ) %>% dplyr::filter(user_id %in% as.numeric(input$repro_coders))
        
        if (length(unique(segments$user_id)) > 1) {
          overlap_df <- calculate_segment_overlap_by_users(segments) %>%
            dplyr::summarise(
              `Total Overlap` = mean(is_overlap),
              `N segments` = length(unique(segment_id)),
              `N coders` = length(unique(c(coder1_id, coder2_id)))
            )
          
          output$overlap_plot <- NULL
          output$overlap_table <- renderTable({
            overlap_df
          })
        } else {
          output$overlap_table <- renderText(reproducibility_message)
        }
      })
    
    # 2. stejný výpočet a vizualizaci pro každý kód
    
    observeEvent({req(input$metrics_select == "by_code") 
      input$calculate}, {
        segments <- load_all_segments_db(
          pool = glob$pool,
          active_project = glob$active_project
        ) %>% dplyr::filter(user_id %in% as.numeric(input$repro_coders))
        
        codes <- load_codes_names(
          active_project = glob$active_project,
          pool = glob$pool
        )
        
        if (length(unique(segments$user_id)) > 1) {
          overlap_df <- calculate_code_overlap_by_users(segments) %>%
            dplyr::group_by(code_id) %>%
            dplyr::summarise(
              w_total_overlap = stats::weighted.mean(total_overlap, n_char),
              n_char_coded = sum(n_char),
              n_coders = length(unique(c(coder1_id, coder2_id))),
              .groups = "drop"
            ) %>%
            dplyr::left_join(., codes, by = "code_id") %>%
            dplyr::select(
              code_name, w_total_overlap,
              n_char_coded, n_coders
            ) %>%
            dplyr::arrange(dplyr::desc(w_total_overlap)) %>%
            dplyr::rename(
              `Code name` = code_name,
              `Weighted total overlap` = w_total_overlap,
              `N characters coded` = n_char_coded,
              `N coders` = n_coders
            )
          
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
        ) %>% dplyr::filter(user_id %in% as.numeric(input$repro_coders))
        
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
          
          output$overlap_plot <- NULL
          output$overlap_table <- renderTable({
            overlap_df
          })
        } else {
          output$overlap_table <- renderText(reproducibility_message)
        }
      })
    
    
    # by_user ----
    observeEvent({req(input$metrics_select == "by_user")
      input$calculate}, {
        segments <- load_all_segments_db(
          pool = glob$pool,
          active_project = glob$active_project
        ) %>% dplyr::filter(user_id %in% as.numeric(input$repro_coders))
        
        users <- load_users_names(
          pool = glob$pool,
          active_project = glob$active_project
        )
        
        if (length(unique(segments$user_id)) > 1) {
          overlap_df <- calculate_code_overlap_by_users(segments) %>%
            join_user_names(., users) %>%
            dplyr::group_by(coder1_name, coder2_name, coder1_id, coder2_id) %>%
            dplyr::summarise(
              w_total_overlap = stats::weighted.mean(total_overlap, n_char),
              .groups = "drop"
            ) %>%
            make_overlap_df_symmetrical()
          
          overlap_heatmap <- create_overlap_heatmap(overlap_df, fill = w_total_overlap)
          
          output$overlap_table <- NULL
          output$overlap_plot <- renderPlot({
            overlap_heatmap
          }, height = "auto", width = "auto")
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
        ) %>% dplyr::filter(user_id %in% as.numeric(input$repro_coders))
        
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
          
          output$overlap_table <- NULL
          output$overlap_plot <- renderPlot({
            overlap_heatmap
          }, height = "auto", width = "auto")
        } else {
          output$overlap_table <- renderText(reproducibility_message)
        }
      })
    
    
    # by_user_code ----
    observeEvent({req(input$metrics_select == "by_user_code")
      input$calculate}, {
        segments <- load_all_segments_db(
          pool = glob$pool,
          active_project = glob$active_project
        ) %>% dplyr::filter(user_id %in% as.numeric(input$repro_coders))
        
        users <- load_users_names(
          pool = glob$pool,
          active_project = glob$active_project
        )
        codes <- load_codes_names(
          pool = glob$pool,
          active_project = glob$active_project
        )
        
        if (length(unique(segments$user_id)) > 1) {
          overlap_df <- calculate_code_overlap_by_users(segments) %>%
            join_user_names(., users) %>%
            dplyr::left_join(., codes, by = "code_id") %>%
            make_overlap_df_symmetrical()
          
          n_codes <- length(unique(overlap_df$code_name))
          
          if(n_codes > 6){
            overlap_heatmap <- create_overlap_heatmap(overlap_df, fill = total_overlap) +
              ggplot2::facet_wrap(ggplot2::vars(code_name), ncol = 3)
          }else{
            overlap_heatmap <- create_overlap_heatmap(overlap_df, fill = total_overlap) +
              ggplot2::facet_wrap(ggplot2::vars(code_name))
          }
          output$overlap_table <- NULL
          output$overlap_plot <- renderPlot({
            overlap_heatmap
          }, height = "auto", width = "auto")
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
        ) %>% dplyr::filter(user_id %in% as.numeric(input$repro_coders))
        
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
          
          n_codes <- length(unique(overlap_df$code_name))
          if(n_codes > 6){
            nrows <- ceiling(n_codes / 3)
            height <- 200 * nrows
            overlap_heatmap <- create_overlap_heatmap(overlap_df, fill = total_overlap) +
              ggplot2::facet_wrap(ggplot2::vars(code_name), ncol = 3)
          }else{
            height <- "auto"
            overlap_heatmap <- create_overlap_heatmap(overlap_df, fill = total_overlap) +
              ggplot2::facet_wrap(ggplot2::vars(code_name))
          }
          
          output$overlap_table <- NULL
          output$overlap_plot <- renderPlot({
            overlap_heatmap
          }, height = height, width = "auto")
        } else {
          output$overlap_table <- renderText(reproducibility_message)
        }
      })
    
  })
  
}
