#' memo_segment UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_memo_segment_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_rql_hidden_ui_ui(ns("rql_hidden_ui_2"), title = "Toggle coding toolbox", hidden_tags = tagList(
      div(style = "display: flex; flex-direction: column; align-items: flex-end;",
          shinyWidgets::radioGroupButtons(inputId = ns("docmemo_view"), NULL, choices = c("Memo input", "Memo editor", "Both")),
          checkboxInput(ns("memo_show"), "Show memos", value = TRUE, width = "120px"),
      )
    )),
   div(class = "memo_segment_container",
                        tags$iframe(
                          src = "www/memo_segment.html",
                          class = "memo_segment_input"
                        )
  ),
                actionButton(
                  ns("add_segment_memo"),
                  "Add segment memo",
                  width = "100%"
                )
                )
}
    
#' memo_segment Server Functions
#'
#' @noRd 
mod_memo_segment_server <- function(id, glob){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    glob$docmemo <- NULL
    mod_rql_hidden_ui_server("rql_hidden_ui_2")


  observeEvent(input$memo_show, {
    glob$docmemo$memo_show <- input$memo_show
  })


  observeEvent(input$add_segment_memo, {
    glob$docmemo$add_segment_memo <- input$add_segment_memo
  })
    

  })
}
    