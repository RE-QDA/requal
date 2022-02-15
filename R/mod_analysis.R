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
                    htmlOutput(ns("segments"))
                    
    ),
    column(width = 4,
    selectInput(ns("code_filter"),
                   label = "Filter by code",
                   choices = "",
                   multiple = TRUE,
                selectize = FALSE,
                width = "100%",
                size = 30),
                   
    ))
  )
}
    
#' analysis Server Functions
#'
#' @noRd 
mod_analysis_server <- function(id, project, codebook, documents){
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



    segments_intialize <- eventReactive(project()$active_project, {
      
      print(project()$active_project)
      print(codebook())
      print(documents())
      # print(input$code_filter)
  
    })
    
    output$segments <- renderText({
      segments_intialize()
    })
    
  })
}

