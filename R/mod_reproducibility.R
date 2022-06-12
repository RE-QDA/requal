#' reproducibility UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_reproducibility_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("metrics_select"), 
                "Select metrics", 
                choices = c("", 
                            "Total overlap [character]" = "total", 
                            "Overlap by code [character]" = "by_code", 
                            "Overlap by coder [character]" = "by_user", 
                            "Overlap by coder and code [character]" = "by_user_code", 
                            "Total overlap [segments]"="total_segment", 
                            "Overlap by code [segments]" = "by_code_segment", 
                            "Overlap by coder [segments]" = "by_user_segment", 
                            "Overlap by coder and code [segments]" = "by_user_code_segment", 
                            "Browse documents"="docs")),
    actionButton(ns("test"), "Calculate"),
    uiOutput(ns("overlap_table")), 
    uiOutput(ns("overlap_documents")),
    plotOutput(ns("overlap_plot"))
  )
}
    
#' reproducibility Server Functions
#'
#' @noRd 
mod_reproducibility_server <- function(id, project){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observe({
      if(input$metrics_select == "total"){
        observeEvent(input$test, {
          
        segments <- load_all_segments_db(project_db = project()$project_db, 
                                           active_project = project()$active_project)
          
          if(length(unique(segments$user_id)) > 1){
              overlap_df <- calculate_code_overlap_by_users(segments) %>% 
                  dplyr::summarise(`Weighted total overlap` = stats::weighted.mean(total_overlap, n_char), 
                                   `N characters coded` = sum(n_char), 
                                   `N coders` = length(unique(c(coder1_id, coder2_id))))
              
              output$overlap_documents <- NULL
              output$overlap_plot <- NULL
              output$overlap_table <- renderTable({overlap_df})
          }else{
              output$overlap_table <- renderText("Only one user coded texts. Reproducibility measures cannot be computed.")   
          }
      })
      }
      
      if(input$metrics_select == "total_segment"){
        observeEvent(input$test, {
            segments <- load_all_segments_db(project_db = project()$project_db, 
                                             active_project = project()$active_project)
            
            if(length(unique(segments$user_id)) > 1){
                
                overlap_df <- calculate_segment_overlap_by_users(segments) %>% 
                    dplyr::summarise(`Total Overlap` = mean(is_overlap), 
                                     `N segments` = length(unique(segment_id)), 
                                     `N coders` = length(unique(c(coder1_id, coder2_id))))
                
                output$overlap_documents <- NULL
                output$overlap_plot <- NULL
                output$overlap_table <- renderTable({overlap_df})
            }else{
                output$overlap_table <- renderText("Only one user coded texts. Reproducibility measures cannot be computed.")   
            }
        })
      }
      
      # 2. stejný výpočet a vizualizaci pro každý kód
      if(input$metrics_select == "by_code"){
        observeEvent(input$test, {
          segments <- load_all_segments_db(project_db = project()$project_db, 
                                           active_project = project()$active_project)
          
          codes <- load_codes_names(active_project = project()$active_project, 
                                    project_db = project()$project_db)
          
          if(length(unique(segments$user_id)) > 1){
              overlap_df <- calculate_code_overlap_by_users(segments) %>% 
                  dplyr::group_by(code_id) %>% 
                  dplyr::summarise(w_total_overlap = stats::weighted.mean(total_overlap, n_char), 
                                   n_char_coded = sum(n_char), 
                                   n_coders = length(unique(c(coder1_id, coder2_id))), 
                                   .groups = "drop") %>% 
                  dplyr::left_join(., codes, by = "code_id") %>% 
                  dplyr::select(code_name, w_total_overlap, 
                                n_char_coded, n_coders) %>% 
                  dplyr::arrange(dplyr::desc(w_total_overlap)) %>% 
                  dplyr::rename(`Code name` = code_name, 
                                `Weighted total overlap` = w_total_overlap, 
                                `N characters coded` = n_char_coded, 
                                `N coders` = n_coders)
              
              output$overlap_documents <- NULL
              output$overlap_plot <- NULL
              output$overlap_table <- renderTable({overlap_df})
          }else{
              output$overlap_table <- renderText("Only one user coded texts. Reproducibility measures cannot be computed.")   
          }
        })
      }
      
      if(input$metrics_select == "by_code_segment"){
        observeEvent(input$test, {
          segments <- load_all_segments_db(project_db = project()$project_db, 
                                           active_project = project()$active_project)
          
          codes <- load_codes_names(active_project = project()$active_project, 
                                    project_db = project()$project_db)
          
          if(length(unique(segments$user_id)) > 1){
              overlap_df <- calculate_segment_overlap_by_users(segments) %>% 
                  dplyr::group_by(code_id) %>% 
                  dplyr::summarise(total_overlap = mean(is_overlap), 
                                   n_segments = length(unique(segment_id)), 
                                   n_coders = length(unique(c(coder1_id, coder2_id)))) %>% 
                  dplyr::left_join(., codes, by = "code_id") %>% 
                  dplyr::select(code_name, total_overlap, 
                                n_segments, n_coders) %>% 
                  dplyr::arrange(dplyr::desc(total_overlap)) %>% 
                  dplyr::rename(`Code name` = code_name, 
                                `Total overlap` = total_overlap, 
                                `N segments` = n_segments, 
                                `N coders` = n_coders)
          
              output$overlap_documents <- NULL
              output$overlap_plot <- NULL
              output$overlap_table <- renderTable({overlap_df})
          }else{
              output$overlap_table <- renderText("Only one user coded texts. Reproducibility measures cannot be computed.")   
          }
        })
      }
      
      if(input$metrics_select == "by_user"){
        observeEvent(input$test, {
          segments <- load_all_segments_db(project_db = project()$project_db, 
                                           active_project = project()$active_project)
          
          users <- load_users_names(project_db = project()$project_db, 
                                    active_project = project()$active_project)
          
          if(length(unique(segments$user_id)) > 1){
              overlap_df <- calculate_code_overlap_by_users(segments) %>% 
                  join_user_names(., users) %>% 
                  dplyr::group_by(coder1_name, coder2_name, coder1_id, coder2_id) %>% 
                  dplyr::summarise(w_total_overlap = stats::weighted.mean(total_overlap, n_char), 
                                   .groups = "drop") %>% 
                  make_overlap_df_symmetrical()
          
              
              overlap_heatmap <- create_overlap_heatmap(overlap_df, fill = w_total_overlap)
              
              output$overlap_documents <- NULL
              output$overlap_table <- NULL
              output$overlap_plot <- renderPlot({overlap_heatmap})
          }else{
              output$overlap_table <- renderText("Only one user coded texts. Reproducibility measures cannot be computed.")   
          }
        })
      }
      
      if(input$metrics_select == "by_user_segment"){
        observeEvent(input$test, {
          segments <- load_all_segments_db(project_db = project()$project_db, 
                                           active_project = project()$active_project)
          
          users <- load_users_names(project_db = project()$project_db, 
                                    active_project = project()$active_project)
          
          if(length(unique(segments$user_id)) > 1){
              overlap_df <- calculate_segment_overlap_by_users(segments) %>% 
                  join_user_names(., users) %>% 
                  dplyr::group_by(coder1_name, coder2_name, coder1_id, coder2_id) %>% 
                  dplyr::summarise(total_overlap = mean(is_overlap), 
                                   .groups = "drop") %>% 
                  make_overlap_df_symmetrical()
              
              overlap_heatmap <- create_overlap_heatmap(overlap_df, fill = total_overlap)
          
              output$overlap_documents <- NULL
              output$overlap_table <- NULL
              output$overlap_plot <- renderPlot({overlap_heatmap})
          }else{
              output$overlap_table <- renderText("Only one user coded texts. Reproducibility measures cannot be computed.")   
          }
        })
      }
      
      if(input$metrics_select == "by_user_code"){
        observeEvent(input$test, {
          segments <- load_all_segments_db(project_db = project()$project_db, 
                                           active_project = project()$active_project)
          users <- load_users_names(project_db = project()$project_db, 
                                    active_project = project()$active_project)
          codes <- load_codes_names(project_db = project()$project_db, 
                                    active_project = project()$active_project)
          
          if(length(unique(segments$user_id)) > 1){
              overlap_df <- calculate_code_overlap_by_users(segments) %>% 
                  join_user_names(., users) %>% 
                  dplyr::left_join(., codes, by = "code_id") %>% 
                  make_overlap_df_symmetrical()
              
              overlap_heatmap <- create_overlap_heatmap(overlap_df, fill = total_overlap) + 
                  ggplot2::facet_wrap(ggplot2::vars(code_name))
              
              output$overlap_documents <- NULL
              output$overlap_table <- NULL
              output$overlap_plot <- renderPlot({overlap_heatmap})
          }else{
              output$overlap_table <- renderText("Only one user coded texts. Reproducibility measures cannot be computed.")   
          }
        })
      }
      
      if(input$metrics_select == "by_user_code_segment"){
        observeEvent(input$test, {
          segments <- load_all_segments_db(project_db = project()$project_db, 
                                           active_project = project()$active_project)
          users <- load_users_names(project_db = project()$project_db, 
                                    active_project = project()$active_project)
          codes <- load_codes_names(project_db = project()$project_db, 
                                    active_project = project()$active_project)
          
          if(length(unique(segments$user_id)) > 1){
              overlap_df <- calculate_segment_overlap_by_users(segments) %>% 
                  dplyr::group_by(code_id, coder1_id, coder2_id) %>% 
                  dplyr::summarise(total_overlap = mean(is_overlap), 
                                   .groups = "drop") %>% 
                  join_user_names(., users) %>% 
                  dplyr::left_join(., codes, by = "code_id") %>% 
                  make_overlap_df_symmetrical()
          
              overlap_heatmap <- create_overlap_heatmap(overlap_df, fill = total_overlap) + 
                  ggplot2::facet_wrap(ggplot2::vars(code_name))
              
              output$overlap_documents <- NULL
              output$overlap_table <- NULL
              output$overlap_plot <- renderPlot({overlap_heatmap})
          }else{
              output$overlap_table <- renderText("Only one user coded texts. Reproducibility measures cannot be computed.")   
          }
        })
      }
        
        if(input$metrics_select == "docs"){
            # TODO: select doc_id and code_id
            observeEvent(input$test, {
                DOC_ID <- 2
                docs <- load_all_docs_db(project_db = project()$project_db, 
                                         active_project = project()$active_project)
                doc_to_display <- docs %>% dplyr::filter(doc_id == DOC_ID) %>% 
                    dplyr::pull(doc_text)
                
                segments <- load_all_segments_db(project_db = project()$project_db, 
                                                 active_project = project()$active_project) %>% 
                    dplyr::filter(doc_id == DOC_ID)
                
                if(length(unique(segments$user_id)) > 1){
                    overlap <- segments %>% 
                        dplyr::filter(code_id == 5) %>% 
                        dplyr::mutate(marked = purrr::map2(segment_start, segment_end, 
                                                           function(x, y) seq(from = x, to = y, by = 1))) %>% 
                        tidyr::unnest(., marked) %>% 
                        dplyr::count(marked) %>% 
                        dplyr::mutate(segment_break = marked != dplyr::lag(marked) + 1 | n != dplyr::lag(n)) %>%
                        dplyr::mutate(segment_break = ifelse(is.na(segment_break), FALSE, segment_break)) %>%
                        dplyr::mutate(segment_id = cumsum(segment_break)) %>%
                        dplyr::group_by(segment_id, n) %>%
                        dplyr::summarise(min_intersect = min(marked), 
                                         max_intersect = max(marked), 
                                         intersect_length = max_intersect - min_intersect + 1)
                    
                    max_n <- max(overlap$n)
                    palette <- viridisLite::viridis(max_n)
                    
                    overlap_df <- overlap %>% 
                        dplyr::rename(segment_start = min_intersect, 
                                      segment_end = max_intersect) %>% 
                        tidyr::pivot_longer(cols = c(segment_start, segment_end),
                                            values_to = "position_start", 
                                            names_to = "position_type",
                                            values_drop_na = TRUE
                                            ) %>% 
                        dplyr::mutate(tag_end = "</b>",
                                      tag_start = paste0('<b id="consensus_',
                                                         as.character(n),
                                                         '" class="segment" style="padding:0; background-color:',
                                                         palette[n],
                                                         '">')) %>% 
                        dplyr::bind_rows(
                            # start doc
                            tibble::tibble(position_start = 0,
                                           position_type =  "segment_start",
                                           tag_start = "<article><p class='docpar'>"),
                            
                            # content
                            .,
                            # end doc
                            tibble::tibble(position_start = nchar(doc_to_display),
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
                    
                    html_content <- paste0(purrr::pmap_chr(list(overlap_df$position_start,
                                                                overlap_df$position_end,
                                                                overlap_df$tag),
                                                           ~paste0(..3, 
                                                                   substr(
                                                                       doc_to_display, 
                                                                       ..1, 
                                                                       ..2))),
                                           collapse = "") %>% 
                        stringr::str_replace_all("[\\n\\r]",
                                                 "<span class='br'>\\&#8203</span></p><p class='docpar'>")
                    
                    output$overlap_documents <- renderText({html_content})
                    output$overlap_table <- NULL
                    output$overlap_plot <- NULL
                }else{
                    output$overlap_table <- renderText("Only one user coded texts. Reproducibility measures cannot be computed.")   
                }
        })
        }
  })
})
}    
