#' memo_segment UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_memo_segment_ui <- function(id) {
  ns <- NS(id)
  tagList(
    mod_rql_hidden_ui_ui(ns("rql_hidden_ui_2"), title = "Toggle coding toolbox", hidden_tags = tagList(
      div(
        checkboxInput(ns("memo_show"), "Show memos", value = TRUE, width = "120px")
      )
    )),
    mod_memo_editor_ui(ns("memo_editor_1"))
  )
}

#' memo_segment Server Functions
#'
#' @noRd
mod_memo_segment_server <- function(id, glob) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    loc <- reactiveValues()
    mod_rql_hidden_ui_server("rql_hidden_ui_2")
   # memo editor segment
   mod_memo_editor_server("memo_editor_1", glob, type = "free_segment")

       ## Observe text_memo_edit_click ----
    observeEvent(req(input$text_memo_click), {
      golem::invoke_js("updateEditorInput", 
              list(ns_memo_id = paste0(ns("memo_editor_1"), "-memo_id"),
                    id = parse_memo_id(input$text_memo_click)))
      golem::invoke_js("resetMemoClick", 
              list(ns_text_memo_click = ns("text_memo_click")))
      glob$selected_documentcode_tabset <- "memotools_tabset"
    })

  
    observeEvent(input$memo_show, {
      glob$memo_show <- input$memo_show
    })

    return(NULL)
  })
}