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
                    
                    uiOutput(ns("segments")) %>% 
                      tagAppendAttributes(class = "scrollable90")
                    
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
    
    ),
    
    uiOutput(ns("download"))
    
    
    
    )
  )
}
    
#' analysis Server Functions
#'
#' @noRd 
mod_analysis_server <- function(id, project, codebook, documents, segments) {
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    

    observeEvent(codebook(), { 
      
     
        
      updateSelectInput(session = session,
                        "code_filter",
                        choices = setNames(codebook()$code_id,
                                           codebook()$code_name),
                        selected = codebook()$code_id
                           )
      
      
      
      })
    
    observeEvent(documents(), { 
      
     
        
        updateSelectInput(session = session,
                          "document_filter",
                          choices = documents(),
                          selected = documents()
        )
        
      
      
    })


    segments_df <- reactiveVal()
    
   observeEvent({input$code_filter
      input$document_filter
      segments()
      codebook()
      documents()
      project()$active_project}, {
      

      

        temp_df <- load_segments_analysis(project()$project_db,
                             project()$active_project,
                             input$code_filter,
                             input$document_filter) 
        
        if (nrow(temp_df) > 0) {
          segments_df(
            temp_df %>% 
        dplyr::left_join(codebook(),
                         by = "code_id") %>% 
        dplyr::left_join(tibble::enframe(documents(),
                                         name = "doc_name",
                                         value = "doc_id"),
                         by = "doc_id")
        ) } else {
                           
          segments_df(as.data.frame(NULL))
          
                         } 
      })
   
      
    segments_taglist <- eventReactive(segments_df(), { 
      
      if (nrow(segments_df()) > 1) {
     purrr::pmap(list(segments_df()$segment_text, 
                           segments_df()$doc_name, 
                           segments_df()$code_name, 
                           segments_df()$code_color),
                      ~format_cutouts(segment_text = ..1,
                                      segment_document = ..2,
                                      segment_code = ..3,
                                      segment_color = ..4)
                      ) 

      }
      
  
    })
    
    output$segments <- renderUI({
      if (nrow(req(segments_df())) > 1) {
      segments_taglist()
        
      } else {
        "No coded segments detected."
      }
    })
    
    output$download <- renderUI({
      
if (nrow(req(segments_df())) > 1) {
  mod_download_handler_ui("download_handler_ui_1")
} else {""}
      
    })
    

return(reactive({
  
  if (nrow(req(segments_df())) > 1) {
    segments_df() %>% dplyr::select(doc_name, doc_id, code_name, code_id, segment_text)
  } else {as.data.frame(NULL)}
  
  
  }))
    
  })
}

