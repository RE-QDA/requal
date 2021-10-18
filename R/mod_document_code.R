#' document_code UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_document_code_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    tags$head(
      tags$script(
        src = "www/document_code_js.js"
      )
    ),
    
    selectInput(ns("doc_selector"), label = "Documents", 
                choices = "",
                selected = ""),
    
    actionButton(ns("refresh"), label = "Refresh"),
    
    actionButton(ns("add_code"), label = "code"),
    
    htmlOutput(ns("focal_text")),

    textOutput(ns("captured_range"))
  )
}
    
#' document_code Server Functions
#'
#' @noRd 
mod_document_code_server <- function(id, project){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Selection of documents to code
    
    doc_choices <- reactiveVal("")
    
    observeEvent(input$refresh, {
      doc_choices <- read_doc_db(project$active_project, project$project_db)
      
      updateSelectInput(session = session, "doc_selector", choices = doc_choices)
      })
    
    # Displayed text
    
    text <- eventReactive(input$doc_selector, {
    if (isTruthy(input$doc_selector)) {
      load_doc_db(project$active_project, project$project_db, input$doc_selector)
    } else {
      ""
    }
    })
    # Render selected text
    output$focal_text <- renderUI({
      
     HTML(text())

      
    })
    
    # Coding tools
    observeEvent(input$add_code, {
      
      # browser()
      # 
      golem::invoke_js("highlight", list("yellow"))
      
      startOff <- as.numeric(unlist(strsplit(input$tag_position, "-")))[1]+1
      endOff <- as.numeric(unlist(strsplit(input$tag_position, "-")))[2]+1
      segment <- substr(text(), startOff, endOff)
      
      write_segment_db(project$active_project,
                       project$project_db,
                       doc_id = doc_choices(),
                       code_id = 2,
                       segment, 
                       startOff, 
                       endOff
                       )


    })

    
    # Helper (to be commented out)
    
    output$captured_range <- renderText({input$tag_position})
 
  })
}
