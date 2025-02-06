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
        style = "display: flex; flex-direction: column; align-items: flex-end;",
        shinyWidgets::checkboxGroupButtons(inputId = ns("docmemo_view"), NULL, choices = c("Composer" = "composer", "Editor" = "editor")),
        checkboxInput(ns("memo_show"), "Show memos", value = TRUE, width = "120px")
      )
    )),
    ac
    shinyhjs::hidden(mod_memo_editor_ui(ns("memo_editor_1")))
  )
}

#' memo_segment Server Functions
#'
#' @noRd
mod_memo_segment_server <- function(id, glob, memo_id = NULL, segment_start = NULL, segment_end = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    loc <- reactiveValues()
    loc$memo_id <- memo_id
    loc$segment_start <- segment_start
    loc$segment_end <- segment_end
    memo_editor <- reactiveValues() # messages from nested module
    return_values <- reactiveValues() # messages to nesting module

    mod_rql_hidden_ui_server("rql_hidden_ui_2")
   
    observeEvent(input$new_memo, {
    memo_editor <- mod_memo_editor_server("memo_editor_1", glob = glob, memo_id = NULL, segment_start = loc$segment_start, segment_end = loc$segment_end, ifram)
      })
    observeEvent(c(req(loc$memo_id), glob$active_project), {
                  print(paste("memos tabset screen", loc$memo_id))
    memo_editor <- mod_memo_editor_server("memo_editor_1", glob = glob, memo_id = req(loc$memo_id), segment_start = loc$segment_start, segment_end = loc$segment_end)
      })
    ## Add new free segment memo ----
    observeEvent(glob$add_segment_memo, {
   
      if (loc$endOff >= loc$startOff) {
       new_segment_id <- write_memo_segment_db(
          pool = glob$pool,
          active_project = glob$active_project,
          user_id = glob$user$user_id,
          doc_id = loc$doc_selector,
          code_id = NA,
          loc$startOff,
          loc$endOff
        )
      new_memo_id  <- add_memo_record(
        pool = glob$pool,
        project = glob$active_project,
        text = input$segment_memo$text,
        user_id = glob$user$user_id
      )
      new_memo_segment_map <- data.frame(memo_id = new_memo_id, segment_id = new_segment_id)
      #add_memo_segment_map(...)
      DBI::dbWriteTable(glob$pool, "memos_segments_map", new_memo_segment_map, append = TRUE, row.names = FALSE)
      golem::invoke_js('refreshMemoIframe', list())
      loc$display_observer <- loc$display_observer + 1
      loc$memos_observer <- loc$memos_observer + 1
      #glob$memos_observer <- glob$memos_observer + 1
      }
    })
    ## Add new coded segment memo ----
    ## Add new free segment memo ----

    
    observeEvent(memo_editor$memo_text, {
      print(memo_editor$memo_text)
    })

    observeEvent(input$memo_show, {
      glob$memo_show <- input$memo_show
    })

    observeEvent(input$add_segment_memo, {
      glob$add_segment_memo <- input$add_segment_memo
    })

    return(NULL)
  })
}