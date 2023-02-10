#' summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_summary_ui <- function(id){
  ns <- NS(id)
  tagList(
    checkboxGroupInput(ns("summary_coders"), "Select coders:", 
                       choices = ""),
    
    actionButton(ns("calculate"), "Calculate"),
    # dataTableOutput(ns("summary_table"))
    tableOutput(ns("summary_table"))
  )
}

#' summary Server Functions
#'
#' @noRd 
mod_summary_server <- function(id, glob){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(glob$active_project, {
      if (isTruthy(glob$active_project)) {
        users <- dplyr::tbl(glob$pool, "users") %>% 
          dplyr::select(user_id, user_name) %>% 
          dplyr::collect()
        
        updateCheckboxGroupInput(
          session = session, 
          "summary_coders", 
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
    
    observeEvent({input$calculate}, {
      codes <- load_codes_names(pool = glob$pool, 
                                active_project = glob$active_project)
      
      docs <- load_all_docs_db(pool = glob$pool, 
                               active_project = glob$active_project) %>% 
        dplyr::select(doc_id, doc_name)
      
      coded_segments <- load_all_segments_db(
        pool = glob$pool, 
        active_project = glob$active_project) %>%
        dplyr::left_join(., codes, by = "code_id") %>% 
        dplyr::left_join(., docs, by = "doc_id") %>% 
        dplyr::filter(user_id %in% as.numeric(input$summary_coders))
      
      if(nrow(coded_segments) > 0){
        summary_df <- coded_segments %>% 
          dplyr::count(doc_id, doc_name, code_name, code_id) %>% 
          dplyr::arrange(doc_id, code_id) %>% 
          dplyr::select(-c(code_id, doc_id)) %>% 
          tidyr::pivot_wider(., id_cols = "code_name", 
                             names_from = "doc_name", 
                             values_from = "n", 
                             values_fill = 0) %>% 
          dplyr::rename(code = code_name)
        
        output$summary_table <- renderTable(summary_df)
        # output$summary_table <- renderDataTable(summary_df)
      }else{
        output$summary_table <- NULL
      }
    })
    
  })
}

## To be copied in the UI
# mod_summary_ui("summary_1")

## To be copied in the server
# mod_summary_server("summary_1")