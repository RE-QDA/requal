utils::globalVariables(c("attribute_object", "attribute_value", "attribute_value1", "attribute_value2"))

#' agreement UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_agreement_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("metrics_select"),
                "Select metrics",
                choices = c("",
                            "Total overlap [character]" = "total",
                            "Overlap by code [character]" = "by_code",
                            "Overlap by coder [character]" = "by_user",
                            "Overlap by coder and code [character]" = "by_user_code",
                            "Overlap by user attribute [character]" = "by_attribute",
                            "Overlap by user attribute and code [character]" = "by_attribute_code",
                            "Total overlap [segments]" = "total_segment",
                            "Overlap by code [segments]" = "by_code_segment",
                            "Overlap by coder [segments]" = "by_user_segment",
                            "Overlap by coder and code [segments]" = "by_user_code_segment", 
                            "Overlap by user attribute [segments]" = "by_attribute_segment", 
                            "Overlap by user attribute and code [segments]" = "by_attribute_segment_code"
                )
    ),
    checkboxGroupInput(ns("repro_coders"), "Select coders:", 
                       choices = ""),
    uiOutput(ns("attributes_select")),
    # selectInput(, 
    #             "Select user attribute", 
    #             choices = ""),
    actionButton(ns("calculate"), "Calculate"),
    uiOutput(ns("overlap_table")),
    plotOutput(ns("overlap_plot"), height = "600px")
  )
}

#' agreement Server Functions
#'
#' @noRd
mod_agreement_server <- function(id, glob) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    agreement_message <- "Agreement measures cannot be computed for a single active coder."
    
    observeEvent(glob$documents, {
      if (isTruthy(glob$active_project)) {
        users <- get_users_in_project(glob$pool, glob$active_project)
        
        if(!is.null(glob$user$data) && 
           !is.null(glob$user$data$report_other_view) &&
           glob$user$data$report_other_view != 1){
          users <- users %>% 
            dplyr::filter(user_id == glob$user$user_id)
        }
        
        updateCheckboxGroupInput(
          session = session, 
          "repro_coders", 
          choices = c(
            stats::setNames(
              users$user_id,
              users$user_name
            )
          ), 
          selected = users$user_id
        )
      }
    })
    
    observeEvent(req(input$metrics_select %in% c("by_attribute", "by_attribute_segment", 
                                                 "by_attribute_code", "by_attribute_segment_code")), {
      user_attributes <- dplyr::tbl(glob$pool, "attributes") %>% 
        dplyr::filter(project_id == !!as.numeric(glob$active_project) & 
                        attribute_object == "user") %>% 
        dplyr::select(attribute_id, attribute_name) %>% 
        dplyr::collect()
      
      output$attributes_select <- renderUI({
        selectInput(ns("attributes_select_ui"), 
                    label = "Select attribute", 
                    choices = c(
                      stats::setNames(
                        user_attributes$attribute_id,
                        user_attributes$attribute_name
                      )))
      })
    })
    
    observeEvent(req(!input$metrics_select %in% c("by_attribute", "by_attribute_segment", 
                                                  "by_attribute_code", "by_attribute_segment_code")), {
      output$attributes_select <- NULL
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
          output$overlap_table <- renderText(agreement_message)
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
              `Total Overlap` = mean(is_overlap)
            ) %>% 
            dplyr::mutate(
              `N segments` = nrow(segments),
              `N coders` = length(unique(segments$user_id))
            )
          
          output$overlap_plot <- NULL
          output$overlap_table <- renderTable({
            overlap_df
          })
        } else {
          output$overlap_table <- renderText(agreement_message)
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
          output$overlap_table <- renderText(agreement_message)
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
          segment_count <- segments %>% 
            dplyr::group_by(code_id) %>% 
            dplyr::summarise(
              n_segments = dplyr::n(), 
              n_coders = length(unique(user_id))
            )
          
          overlap_df <- calculate_segment_overlap_by_users(segments) %>%
            dplyr::group_by(code_id) %>%
            dplyr::summarise(
              total_overlap = mean(is_overlap)
            ) %>%
            dplyr::left_join(., codes, by = "code_id") %>%
            dplyr::left_join(., segment_count, by = "code_id") %>% 
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
          output$overlap_table <- renderText(agreement_message)
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
          output$overlap_table <- renderText(agreement_message)
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
          output$overlap_table <- renderText(agreement_message)
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
          output$overlap_table <- renderText(agreement_message)
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
          output$overlap_table <- renderText(agreement_message)
        }
      })
    
    # by_attribute ----
    observeEvent({req(input$metrics_select == "by_attribute")
      req(input$attributes_select_ui)
      input$calculate}, {
        
        attribute_id <- as.numeric(input$attributes_select_ui)
        segments <- load_all_segments_db(
          pool = glob$pool,
          active_project = glob$active_project
        ) %>% dplyr::filter(user_id %in% as.numeric(input$repro_coders))
        
        attr_user_map <- dplyr::tbl(glob$pool, "attributes_users_map") %>% 
          dplyr::filter(project_id == !!as.numeric(glob$active_project) & 
                          attribute_id == !!as.numeric(attribute_id)) %>% 
          dplyr::collect()
        
        attribute_values <- dplyr::tbl(glob$pool, "attributes") %>% 
          dplyr::filter(attribute_id == !!attribute_id) %>% 
          dplyr::left_join(., dplyr::tbl(glob$pool, "attribute_values"), by = "attribute_id") %>% 
          dplyr::select(attribute_id, attribute_name, attribute_value_id, value) %>% 
          dplyr::collect()
        
        user_attributes <- attr_user_map %>% 
          dplyr::left_join(., attribute_values, by = c("attribute_id", "attribute_value_id")) %>% 
          dplyr::select(user_id, attribute_name, attribute_value = value)
        
        users <- load_users_names(
          pool = glob$pool,
          active_project = glob$active_project
        )
        
        codes <- load_codes_names(
          pool = glob$pool,
          active_project = glob$active_project
        )
        
        if (length(unique(segments$user_id)) > 1) {
          # jaká je interpretace n ?
          overlap_df <- calculate_code_overlap_by_users(segments) %>%
            join_user_names(., users) %>%
            dplyr::group_by(coder1_name, coder2_name, coder1_id, coder2_id) %>%
            dplyr::summarise(
              w_total_overlap = stats::weighted.mean(total_overlap, n_char),
              .groups = "drop"
            ) %>%
            make_overlap_df_symmetrical() %>% 
            dplyr::left_join(., user_attributes %>% dplyr::select(user_id, attribute_value1 = attribute_value), by = c("coder1_id"="user_id")) %>% 
            dplyr::left_join(., user_attributes %>% dplyr::select(user_id, attribute_value2 = attribute_value), by = c("coder2_id"="user_id")) %>% 
            dplyr::group_by(attribute_value1, attribute_value2) %>% 
            dplyr::summarise(
              min_total_overlap = min(w_total_overlap), 
              mean_total_overlap = mean(w_total_overlap), 
              max_total_overlap = max(w_total_overlap), 
              n = dplyr::n()
            )
          
          overlap_heatmap <- create_overlap_heatmap_attribute(overlap_df, fill = mean_total_overlap)
          
          output$overlap_table <- NULL # renderTable(overlap_df)
          output$overlap_plot <- renderPlot({
            overlap_heatmap
          }, height = "auto", width = "auto")
        } else {
          output$overlap_plot <- NULL
          output$overlap_table <- renderText(agreement_message)
        }
      })
    
    # by_attribute_code ----
    observeEvent({req(input$metrics_select == "by_attribute_code")
      input$calculate}, {
        
        attribute_id <- as.numeric(input$attributes_select_ui)
        segments <- load_all_segments_db(
          pool = glob$pool,
          active_project = glob$active_project
        ) %>% dplyr::filter(user_id %in% as.numeric(input$repro_coders))
        
        attr_user_map <- dplyr::tbl(glob$pool, "attributes_users_map") %>% 
          dplyr::filter(project_id == !!as.numeric(glob$active_project) & 
                          attribute_id == !!as.numeric(attribute_id)) %>% 
          dplyr::collect()
        
        attribute_values <- dplyr::tbl(glob$pool, "attributes") %>% 
          dplyr::filter(attribute_id == !!attribute_id) %>% 
          dplyr::left_join(., dplyr::tbl(glob$pool, "attribute_values"), by = "attribute_id") %>% 
          dplyr::select(attribute_id, attribute_name, attribute_value_id, value) %>% 
          dplyr::collect()
        
        user_attributes <- attr_user_map %>% 
          dplyr::left_join(., attribute_values, by = c("attribute_id", "attribute_value_id")) %>% 
          dplyr::select(user_id, attribute_name, attribute_value = value)
        
        users <- load_users_names(
          pool = glob$pool,
          active_project = glob$active_project
        )
        
        codes <- load_codes_names(
          pool = glob$pool,
          active_project = glob$active_project
        )
        
        if (length(unique(segments$user_id)) > 1) {
          overlap_df <- calculate_code_overlap_by_users_code(segments) %>%
            join_user_names(., users) %>%
            dplyr::group_by(code_id, coder1_name, coder2_name, coder1_id, coder2_id) %>%
            dplyr::summarise(
              w_total_overlap = stats::weighted.mean(total_overlap, n_char),
              .groups = "drop"
            ) %>%
            make_overlap_df_symmetrical() %>% 
            dplyr::left_join(., user_attributes %>% dplyr::select(user_id, attribute_value1 = attribute_value), by = c("coder1_id"="user_id")) %>% 
            dplyr::left_join(., user_attributes %>% dplyr::select(user_id, attribute_value2 = attribute_value), by = c("coder2_id"="user_id")) %>% 
            dplyr::group_by(code_id, attribute_value1, attribute_value2) %>% 
            dplyr::summarise(
              min_total_overlap = min(w_total_overlap), 
              mean_total_overlap = mean(w_total_overlap), 
              max_total_overlap = max(w_total_overlap), 
              n = dplyr::n()
            ) %>% 
            dplyr::left_join(., codes, by = "code_id")
          
          n_codes <- length(unique(overlap_df$code_name))
          
          if(n_codes > 6){
            overlap_heatmap <- create_overlap_heatmap_attribute(overlap_df, fill = mean_total_overlap) +
              ggplot2::facet_wrap(ggplot2::vars(code_name), ncol = 3)
          }else{
            overlap_heatmap <- create_overlap_heatmap_attribute(overlap_df, fill = mean_total_overlap) +
              ggplot2::facet_wrap(ggplot2::vars(code_name))
          }
          output$overlap_table <- NULL
          output$overlap_plot <- renderPlot({
            overlap_heatmap
          }, height = "auto", width = "auto")
        } else {
          output$overlap_table <- renderText(agreement_message)
        }
      })
    
    # by_attribute_segment -----
    observeEvent({req(input$metrics_select == "by_attribute_segment")
      req(input$attributes_select_ui)
      input$calculate}, {
        
        attribute_id <- as.numeric(input$attributes_select_ui)
        segments <- load_all_segments_db(
          pool = glob$pool,
          active_project = glob$active_project
        ) %>% dplyr::filter(user_id %in% as.numeric(input$repro_coders))
        
        attr_user_map <- dplyr::tbl(glob$pool, "attributes_users_map") %>% 
          dplyr::filter(project_id == !!as.numeric(glob$active_project) & 
                          attribute_id == !!as.numeric(attribute_id)) %>% 
          dplyr::collect()
        
        attribute_values <- dplyr::tbl(glob$pool, "attributes") %>% 
          dplyr::filter(attribute_id == !!attribute_id) %>% 
          dplyr::left_join(., dplyr::tbl(glob$pool, "attribute_values"), by = "attribute_id") %>% 
          dplyr::select(attribute_id, attribute_name, attribute_value_id, value) %>% 
          dplyr::collect()
        
        user_attributes <- attr_user_map %>% 
          dplyr::left_join(., attribute_values, by = c("attribute_id", "attribute_value_id")) %>% 
          dplyr::select(user_id, attribute_name, attribute_value = value)
        
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
            join_user_names(., users) %>%
            dplyr::group_by(coder1_name, coder2_name, coder1_id, coder2_id) %>%
            dplyr::summarise(
              total_overlap = mean(is_overlap),
              .groups = "drop"
            ) %>%
            make_overlap_df_symmetrical() %>% 
            dplyr::left_join(., user_attributes %>% dplyr::select(user_id, attribute_value1 = attribute_value), by = c("coder1_id"="user_id")) %>% 
            dplyr::left_join(., user_attributes %>% dplyr::select(user_id, attribute_value2 = attribute_value), by = c("coder2_id"="user_id")) %>% 
            dplyr::group_by(attribute_value1, attribute_value2) %>% 
            dplyr::summarise(
              min_total_overlap = min(total_overlap), 
              mean_total_overlap = mean(total_overlap), 
              max_total_overlap = max(total_overlap), 
              n = dplyr::n()
            )
          
          overlap_heatmap <- create_overlap_heatmap_attribute(overlap_df, fill = mean_total_overlap)
          
          output$overlap_table <- NULL # renderTable(overlap_df)
          output$overlap_plot <- renderPlot({
            overlap_heatmap
          }, height = "auto", width = "auto")
        } else {
          output$overlap_plot <- NULL
          output$overlap_table <- renderText(agreement_message)
        }
      })
    
    # by_attribute_code ----
    observeEvent({req(input$metrics_select == "by_attribute_segment_code")
      req(input$attributes_select_ui)
      input$calculate}, {
        
        attribute_id <- as.numeric(input$attributes_select_ui)
        segments <- load_all_segments_db(
          pool = glob$pool,
          active_project = glob$active_project
        ) %>% dplyr::filter(user_id %in% as.numeric(input$repro_coders))
        
        attr_user_map <- dplyr::tbl(glob$pool, "attributes_users_map") %>% 
          dplyr::filter(project_id == !!as.numeric(glob$active_project) & 
                          attribute_id == !!as.numeric(attribute_id)) %>% 
          dplyr::collect()
        
        attribute_values <- dplyr::tbl(glob$pool, "attributes") %>% 
          dplyr::filter(attribute_id == !!attribute_id) %>% 
          dplyr::left_join(., dplyr::tbl(glob$pool, "attribute_values"), by = "attribute_id") %>% 
          dplyr::select(attribute_id, attribute_name, attribute_value_id, value) %>% 
          dplyr::collect()
        
        user_attributes <- attr_user_map %>% 
          dplyr::left_join(., attribute_values, by = c("attribute_id", "attribute_value_id")) %>% 
          dplyr::select(user_id, attribute_name, attribute_value = value)
        
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
            make_overlap_df_symmetrical() %>% 
            dplyr::left_join(., user_attributes %>% dplyr::select(user_id, attribute_value1 = attribute_value), by = c("coder1_id"="user_id")) %>% 
            dplyr::left_join(., user_attributes %>% dplyr::select(user_id, attribute_value2 = attribute_value), by = c("coder2_id"="user_id")) %>% 
            dplyr::group_by(code_id, attribute_value1, attribute_value2) %>% 
            dplyr::summarise(
              mean_total_overlap = mean(total_overlap), 
              n = dplyr::n()
            ) %>% 
            dplyr::left_join(., codes, by = "code_id")
          
          n_codes <- length(unique(overlap_df$code_name))
          
          if(n_codes > 6){
            overlap_heatmap <- create_overlap_heatmap_attribute(overlap_df, fill = mean_total_overlap) +
              ggplot2::facet_wrap(ggplot2::vars(code_name), ncol = 3)
          }else{
            overlap_heatmap <- create_overlap_heatmap_attribute(overlap_df, fill = mean_total_overlap) +
              ggplot2::facet_wrap(ggplot2::vars(code_name))
          }
          output$overlap_table <- NULL
          output$overlap_plot <- renderPlot({
            overlap_heatmap
          }, height = "auto", width = "auto")
        } else {
          output$overlap_table <- renderText(agreement_message)
        }
      })
    
    
  })
  
}
