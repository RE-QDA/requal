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
  fluidRow(
    div(
      style = "display: flex; align-items: flex-start; margin-left: 30px;",
      div(
        style = "min-width: 40vh;",
        mod_memo_editor_ui(ns("memo_main_editor"))
      ),
      div(
        style = "display: flex; align-items: center; margin-left: 10px;", # Use flexbox for alignment
        actionButton(ns("pin"), "", title = "Pin memo", icon = icon("thumbtack"), class = "pinned"),
        actionButton(ns("unpin"), "", title = "Unpin memos", icon = icon("xmark"), class = "unpinned", style = "margin-left: 5px;")
      )
    ),
    hr(),
    fluidRow(
      div(
        style = "margin-left: 30px; overflow-x: scroll",
        DT::dataTableOutput(ns("memo"))
      )
    )
    # downloadButton(ns("export_memo"), label = "Export memos") %>%
    #     tagAppendAttributes(style = "display: inline-block; float: right", class = "scrollable80")
  )
}

#' memo Server Functions
#'
#' @noRd
mod_memo_server <- function(id, glob) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    loc <- reactiveValues()
    mod_memo_editor_server("memo_main_editor", glob, type = "free_memo")

    ## Observe free_memo_edit_click ----
    observeEvent(input$text_memo_click, {
      req(input$text_memo_click)
      req(glob$free_memo_observer > 0)
      loc$memo_id <- parse_memo_id(input$text_memo_click) # grab the active memo id for this module
      golem::invoke_js(
        "updateEditorInput",
        list(
          ns_memo_id = paste0(ns("memo_main_editor"), "-memo_id"),
          id = loc$memo_id
        )
      )
      golem::invoke_js(
        "resetMemoClick",
        list(ns_text_memo_click = ns("text_memo_click"))
      )
    })

    output$memo <- DT::renderDataTable({
      if (isTruthy(glob$active_project)) {
        memo_table <- list_memo_records(glob$pool, glob$active_project)
        if (glob$user$data$memo_other_view == 0 && nrow(memo_table) > 0) {
          memo_table <- memo_table %>%
            dplyr::filter(user_id == glob$user$user_id)
        }
        req(nrow(memo_table) > 0)
        loc$enriched_memo_table <- memo_table |>
          dplyr::left_join(
            memos_segments_map <- dplyr::tbl(glob$pool, "memos_segments_map") |>
              dplyr::filter(memo_id %in% !!memo_table$memo_id) |>
              dplyr::collect(),
              by = "memo_id"
          ) |>
          dplyr::left_join(
            segment_df <- dplyr::tbl(glob$pool, "segments") %>%
              dplyr::select(segment_id, doc_id) |>
              dplyr::filter(.data$segment_id %in% !!memos_segments_map$segment_id) %>%
              dplyr::collect(),
              by = "segment_id"
          ) |>
          dplyr::left_join(
            documents_df <- dplyr::tbl(glob$pool, "documents") %>%
              dplyr::select(doc_id, doc_name) %>%
              dplyr::filter(.data$doc_id %in% !!segment_df$doc_id) %>%
              dplyr::collect(),
              by = "doc_id"
          ) |>
          dplyr::mutate(
            memo_text = memo_name,
            memo_name = memo_link(ns("text_memo_click"), memo_id, memo_name)
          ) |>
          dplyr::arrange(dplyr::desc(memo_id))

        DT::datatable(
          loc$enriched_memo_table,
          filter = "top",
          escape = FALSE,
          extensions = c("Buttons"),
          options = dt_memo_options(),
          class = "display",
          selection = "none"
        )
      }
    })

    # pin ----
    observeEvent(input$pin, {
      req(loc$memo_id)
      pin_id <- paste0("pin_id-", loc$memo_id)
      pinned_text <- read_memo_by_id(glob$pool, glob$active_project, loc$memo_id) |>
        dplyr::pull(memo_text)

      insertUI(
        selector = "div.content-wrapper", where = "afterBegin",
        div(
          id = pin_id, class = "pinned_memo",
          div(id = "pin_header", class = "pin_header", icon("thumbtack")),
          div(class = "inner_pin", pinned_text),
          div(id = "resize_handle", class = "resizer")
        )
      )
      golem::invoke_js("makeDraggable", list(id = pin_id))
    })
    observeEvent(input$unpin, {
      golem::invoke_js("removeAllPinnedMemos", list(class = "pinned_memo"))
    })

    # # Memo export ----
    # output$export_memo <- downloadHandler(
    #     filename = function() {
    #         "requal_memo_export.csv"
    #     },
    #     content = function(file) {
    #         memos <- export_memos(glob$pool, glob$active_project)
    #         utils::write.csv(memos, file)
    #     }
    # )
  })
}
