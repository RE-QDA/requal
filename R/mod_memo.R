#' memo UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_memo_ui <- function(id) {
  ns <- NS(id)

 
    uiOutput(ns("memo")) 
     
  
}

#' memo Server Functions
#'
#' @noRd
mod_memo_server <- function(id, project) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    
    output$memo <- renderUI({
      
      if (isTruthy(project()$active_project)) {
        tagList(
        h4("Memos"),
        actionButton(ns("new_memo"), "New memo"),
        hr(),
        p("Works")
        )
      } else {
        "No active project."
      }
    })
    
    observeEvent(input$new_memo, {
      
      showModal(modalDialog())
      
    })

    
  }) 

}


