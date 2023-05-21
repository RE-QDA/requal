#' browser UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_browser_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("browser_doc"), 
                "Select document: ", 
                choices = ""), 
    selectInput(ns("browser_code"), 
                "Select code: ", 
                choices = ""), 
    rql_picker_UI(ns("browser_coders"), "Select coders:", 
                       choices = ""),
    actionButton(ns("calculate"), "Browse"),
    uiOutput(ns("document_viewer"))
  )
}
    
#' browser Server Functions
#'
#' @noRd 
mod_browser_server <- function(id, glob){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    agreement_message <- "Agreement measures cannot be computed for projects with one active coder."
    
    observeEvent(glob$documents, {
      if (isTruthy(glob$active_project)) {

        documents <- glob$documents

        if (!is.null(glob$user$data) &&
          glob$user$data$data_other_view != 1) {
          user_docs <- dplyr::tbl(glob$pool, "documents") %>%
            dplyr::filter(user_id == !!glob$user$user_id) %>%
            dplyr::collect()

          if (nrow(user_docs)) {
            documents <- documents[documents %in% user_docs$doc_id]
          } else {
            documents <- NULL
          }
        }
        
        updateSelectInput(
          session = session,
          "browser_doc",
          choices = c("", documents)
        )
        
        users <- get_users_in_project(glob$pool, glob$active_project)
          
        if (!is.null(glob$user$data) &&
          !is.null(glob$user$data$report_other_view) &&
          glob$user$data$report_other_view != 1) {
          users <- users %>%
            dplyr::filter(user_id == glob$user$user_id)
        }

        shinyWidgets::updatePickerInput(
          session = session,
          "browser_coders",
          choices = stats::setNames(
            users$user_id,
            users$user_name
          ),
          selected = users$user_id
        )
      }
    })
    
    observeEvent(glob$codebook, {
      if (isTruthy(glob$active_project)) {
        codebook <- glob$codebook
        if(!is.null(glob$user$data) && 
           glob$user$data$codebook_other_view != 1){
          codebook <- codebook %>% 
            dplyr::filter(user_id == !!glob$user$user_id)
        }
        
        updateSelectInput(
          session = session, 
          "browser_code", 
          choices = c("", stats::setNames(
            codebook$code_id,
            codebook$code_name
          ))
        )
      }
    })
    
    observeEvent({input$calculate}, {
    
      docs <- load_all_docs_db(
        pool = glob$pool,
        active_project = glob$active_project
      )
      
      doc_to_display <- docs %>%
        dplyr::filter(doc_id == input$browser_doc) %>%
        dplyr::pull(doc_text)
      
      segments <- load_all_segments_db(
        pool = glob$pool,
        active_project = glob$active_project
      ) %>%
        dplyr::filter(doc_id == input$browser_doc)
      
      coded_segments <- segments %>%
        dplyr::filter(code_id == input$browser_code) %>% 
        dplyr::filter(user_id %in% as.numeric(input$browser_coders))
    
      if (nrow(coded_segments) > 0) {
        
        overlap <- coded_segments %>%
          dplyr::mutate(marked = purrr::map2(
            segment_start, segment_end,
            function(x, y) seq(from = x, to = y, by = 1)
          )) %>%
          tidyr::unnest(., marked) %>%
          dplyr::group_by(marked) %>% 
          dplyr::summarise(n = dplyr::n(), 
                           users = paste0(user_name, collapse = " | ")) %>%
          dplyr::mutate(segment_break = marked != dplyr::lag(marked) + 1 | n != dplyr::lag(n)) %>%
          dplyr::mutate(segment_break = ifelse(is.na(segment_break), FALSE, segment_break)) %>%
          dplyr::mutate(segment_id = cumsum(segment_break)) %>%
          dplyr::group_by(segment_id, n, users) %>%
          dplyr::summarise(
            min_intersect = min(marked),
            max_intersect = max(marked),
            intersect_length = max_intersect - min_intersect + 1
          )
        
        max_n <- max(overlap$n)
        palette <- viridisLite::viridis(max_n)
        
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
              '" class="segment" style="padding:0; background-color:',
              palette[n], dplyr::if_else(n <= (max_n / 2) | max_n == 1, "; color:white", ""),
              '" title="coded by: ', 
              users, 
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
        
        html_content <- paste0(purrr::pmap_chr(
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
          )
        
        legend <- create_palette_legend(max_n, palette)
        
        output$document_viewer <- renderText({
          paste0(legend, "<h2>Document</h2>", html_content)
        })
      } else {
        output$document_viewer <- renderText("No segments were coded with the selected code.")
        # output$overlap_table <- renderText(agreement_message)
      }
        
    })
 
  })
}
    
## To be copied in the UI
# mod_browser_ui("browser_1")
    
## To be copied in the server
# mod_browser_server("browser_1")
