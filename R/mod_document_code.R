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
    
    
    doc_choices <- reactiveVal("")
    
    observeEvent(input$refresh, {
      doc_choices <- read_doc_db(project$active_project, project$project_db)
      
      updateSelectInput(session = session, "doc_selector", choices = doc_choices)
      })
    
    output$focal_text <- renderUI({
      
      if (isTruthy(input$doc_selector)) {
      HTML(load_doc_db(project$active_project, project$project_db, input$doc_selector))
      } else {
        ""
      }

      
    })
    
    observeEvent(input$add_code, {

      golem::invoke_js("highlight", list("yellow"))


    })

    # observeEvent(input$tag_position, {
    # print(input$tag_position)
    # })
    
    output$captured_range <- renderText({input$tag_position})
 
  })
}
