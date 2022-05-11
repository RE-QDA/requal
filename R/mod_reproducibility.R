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
                            "Total overlap" = "total", 
                            "Overlap by code" = "by_code", 
                            "Overlap by coder" = "by_user", 
                            "Overlap by coder and code" = "by_user_code")),
    actionButton(ns("test"), "Calculate"),
    uiOutput(ns("overlap_table")), 
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
          
          overlap_df <- calculate_code_overlap_by_users(segments) %>% 
            dplyr::summarise(w_total_overlap = weighted.mean(total_overlap, n_char), 
                             n_char_coded = sum(n_char), 
                             n_coders = length(unique(c(coder1_id, coder2_id))))
          
          # TODO: update function
          # 1. kolik % segmentů má překryv a kolik ne → zobrazit prostý referenční překryv, tzn. číslo 
          # (zohlednit nějak počet kodérů) a jeden dokument se všemi kódovanými texty pod sebou; dvěma 
          # barvami vyznačené překrývající a nepřekrývající segmenty
          
          output$overlap_plot <- NULL
          output$overlap_table <- renderTable({overlap_df})
      })
      }
      
      # 2. stejný výpočet a vizualizaci pro každý kód
      if(input$metrics_select == "by_code"){
        observeEvent(input$test, {
          segments <- load_all_segments_db(project_db = project()$project_db, 
                                           active_project = project()$active_project)
          
          codes <- load_codes_names(active_project = project()$active_project, 
                                    project_db = project()$project_db)
          
          overlap_df <- calculate_code_overlap_by_users(segments) %>% 
            dplyr::group_by(code_id) %>% 
            dplyr::summarise(w_total_overlap = weighted.mean(total_overlap, n_char), 
                             n_char_coded = sum(n_char), 
                             n_coders = length(unique(c(coder1_id, coder2_id))), 
                             .groups = "drop") %>% 
            dplyr::left_join(., codes, by = "code_id") %>% 
            dplyr::select(code_name, w_total_overlap, 
                          n_char_coded, n_coders) %>% 
            dplyr::arrange(dplyr::desc(w_total_overlap))
          
          output$overlap_plot <- NULL
          output$overlap_table <- renderTable({overlap_df})
        })
      }
      
      if(input$metrics_select == "by_user"){
        observeEvent(input$test, {
          segments <- load_all_segments_db(project_db = project()$project_db, 
                                           active_project = project()$active_project)
          
          users <- load_users_names(project_db = project()$project_db, 
                                    active_project = project()$active_project)
          
          overlap_df <- calculate_code_overlap_by_users(segments) %>% 
            dplyr::left_join(., users %>% dplyr::rename(coder1_name = user_name), 
                             by = c("coder1_id"="user_id")) %>% 
            dplyr::left_join(., users %>% dplyr::rename(coder2_name = user_name), 
                             by = c("coder2_id"="user_id")) %>% 
            dplyr::group_by(coder1_name, coder2_name, coder1_id, coder2_id) %>% 
            dplyr::summarise(w_total_overlap = weighted.mean(total_overlap, n_char), 
                             .groups = "drop")
          
          # make it symmetrical
          overlap_df <- dplyr::bind_rows(overlap_df, 
                    overlap_df %>% 
                      dplyr::rename(coder2_id2 = coder1_id, 
                                    coder2_name2 = coder1_name, 
                                    coder1_id2 = coder2_id, 
                                    coder1_name2 = coder2_name) %>% 
                      dplyr::rename(coder2_id = coder2_id2, 
                                    coder1_id = coder1_id2, 
                                    coder1_name = coder1_name2, 
                                    coder2_name = coder2_name2)
                    ) %>% unique()
          
          overlap_heatmap <- ggplot2::ggplot(overlap_df, 
                                             ggplot2::aes(x = factor(coder1_name), 
                                                          y = factor(coder2_name), 
                                                          fill = w_total_overlap)) + 
            ggplot2::geom_tile() + 
            ggplot2::scale_fill_viridis_c(limits = c(0, 1)) + 
            ggplot2::theme_minimal() + 
            ggplot2::labs(x = "Coder 1", 
                          y = "Coder 2", 
                          fill = "Overlap") + 
            ggplot2::coord_fixed()
            
          output$overlap_table <- NULL
          output$overlap_plot <- renderPlot({overlap_heatmap})
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
          
          overlap_df <- calculate_code_overlap_by_users(segments) %>% 
            dplyr::left_join(., users %>% dplyr::rename(coder1_name = user_name), 
                             by = c("coder1_id"="user_id")) %>% 
            dplyr::left_join(., users %>% dplyr::rename(coder2_name = user_name), 
                             by = c("coder2_id"="user_id")) %>% 
            dplyr::left_join(., codes, by = "code_id")
          
          overlap_df <- dplyr::bind_rows(overlap_df, 
                                         overlap_df %>% 
                                           dplyr::rename(coder2_id2 = coder1_id, 
                                                         coder2_name2 = coder1_name, 
                                                         coder1_id2 = coder2_id, 
                                                         coder1_name2 = coder2_name) %>% 
                                           dplyr::rename(coder2_id = coder2_id2, 
                                                         coder1_id = coder1_id2, 
                                                         coder1_name = coder1_name2, 
                                                         coder2_name = coder2_name2)
          ) %>% unique()
          
          overlap_heatmap <- ggplot2::ggplot(overlap_df, 
                                             ggplot2::aes(x = factor(coder1_name), 
                                                          y = factor(coder2_name), 
                                                          fill = total_overlap)) + 
            ggplot2::geom_tile() + 
            ggplot2::scale_fill_viridis_c(limits = c(0, 1)) + 
            ggplot2::theme_minimal() + 
            ggplot2::labs(x = "Coder 1", 
                          y = "Coder 2", 
                          fill = "Overlap") + 
            ggplot2::coord_fixed() + 
            ggplot2::facet_wrap(ggplot2::vars(code_name))
          
          output$overlap_table <- NULL
          output$overlap_plot <- renderPlot({overlap_heatmap})
        })
      }
      
    })

    
    # 3. pro vybraný kód spočítat, kolik % segmentů má překryv dvou, tří a čtyř kodérů + 
    # heatmapa hodnot překryvu jednotlivých kodérů
    
    
    
  })
}
    
