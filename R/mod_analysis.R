#' analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(column(width = 8,
                    uiOutput(ns("segments"))
                    
    ),
    column(width = 4,
           
    selectInput(ns("code_filter"),
                   label = "Filter by code",
                   choices = "",
                   multiple = TRUE,
                selectize = FALSE,
                width = "100%",
                size = 15),
    
    selectInput(ns("document_filter"),
                label = "Filter by document",
                choices = "",
                multiple = TRUE,
                selectize = FALSE,
                width = "100%",
                size = 15)
    
    )
    
    
    )
  )
}
    
#' analysis Server Functions
#'
#' @noRd 
mod_analysis_server <- function(id, project, codebook, documents){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    observeEvent(codebook(), { 
      
      if ( isTruthy(project()$active_project) & isTruthy(codebook()) & isTruthy(documents()) ) {
        
      updateSelectInput(session = session,
                        "code_filter",
                        choices = setNames(codebook()$code_id,
                                           codebook()$code_name),
                        selected = codebook()$code_id
                           )
      
      }
      
      })
    
    observeEvent(documents(), { 
      
      if ( isTruthy(project()$active_project) & isTruthy(codebook()) & isTruthy(documents()) ) {
        
        updateSelectInput(session = session,
                          "document_filter",
                          choices = documents(),
                          selected = documents()
        )
        
      }
      
    })



    segments_intialize <- eventReactive(c(input$code_filter, input$document_filter), {
      
      req(input$code_filter, input$document_filter)
      
      if ( isTruthy(project()$active_project) & isTruthy(codebook()) & isTruthy(documents()) ) {

        segments_df <- load_segments_analysis(project()$project_db,
                             project()$active_project,
                             input$code_filter,
                             input$document_filter) %>% 
        dplyr::left_join(codebook(),
                         by = "code_id") %>% 
        dplyr::left_join(tibble::enframe(documents(),
                                         name = "doc_name",
                                         value = "doc_id"),
                         by = "doc_id")
   
      
     purrr::pmap(list(segments_df$segment_text, 
                           segments_df$doc_name, 
                           segments_df$code_name, 
                           segments_df$code_color),
                      ~format_cutouts(segment_text = ..1,
                                      segment_document = ..2,
                                      segment_code = ..3,
                                      segment_color = ..4)
                      ) 
      } else (
        
        "No coded segments in the project."
        
      )
  
    })
    
    output$segments <- renderUI({
      segments_intialize()
    })
    
  })
}

