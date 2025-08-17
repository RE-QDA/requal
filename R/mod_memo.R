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
      )
    ),
    hr(),
    fluidRow(
      div(
        style = "margin-left: 30px; max-width: 80vh;",
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
    loc$memo_observer <- 0
    mod_memo_editor_server("memo_main_editor", glob, type = "free_memo")
    observeEvent(glob$active_project, {
      loc$memo_observer <- loc$memo_observer + 1
    })
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

    observeEvent(c(loc$memo_observer, glob$memo_segment_observer, glob$free_memo_observer), {
      output$memo <- DT::renderDataTable({
        if (isTruthy(glob$active_project)) {
          memo_table <- list_memo_records(glob$pool, glob$active_project)
          if (glob$user$data$memo_other_view == 0 && nrow(memo_table) > 0) {
            memo_table <- memo_table %>%
              dplyr::filter(user_id == glob$user$user_id)
          }
          req(nrow(memo_table) > 0)
          memos_segments_map <- dplyr::tbl(glob$pool, "memos_segments_map") %>%
            dplyr::filter(memo_id %in% !!memo_table$memo_id) %>%
            dplyr::collect()
          segment_df <- dplyr::tbl(glob$pool, "segments") %>%
            dplyr::select(segment_id, doc_id, segment_text) %>%
            dplyr::filter(.data$segment_id %in% !!memos_segments_map$segment_id) %>%
            dplyr::collect()
          documents_df <- dplyr::tbl(glob$pool, "documents") %>%
            dplyr::select(doc_id, doc_name) %>%
            dplyr::filter(.data$doc_id %in% !!segment_df$doc_id) %>%
            dplyr::collect()
          loc$enriched_memo_table <- memo_table %>%
            dplyr::left_join(
              memos_segments_map,
              by = "memo_id"
            ) %>%
            dplyr::left_join(
              segment_df,
              by = "segment_id"
            ) %>%
            dplyr::left_join(
              documents_df,
              by = "doc_id"
            ) %>%
            dplyr::mutate(
              memo_title = memo_link(ns("text_memo_click"), memo_id, memo_name),
              memo_type = purrr::map2_chr(doc_id, segment_id, memo_segment_link)
            ) %>%
            dplyr::arrange(dplyr::desc(memo_id)) %>%
            dplyr::select(memo_id, memo_title, memo_type, doc_name, memo_text, segment_text)

          DT::datatable(
            loc$enriched_memo_table,
            rownames = FALSE,
            width = "800px",
            colnames = c("ID" = "memo_id", "Title" = "memo_title", "Type" = "memo_type", "Document" = "doc_name"),
            filter = "top",
            escape = FALSE,
            extensions = c("Buttons"),
            options = dt_memo_options(),
            class = "display",
            selection = "none"
          )
        }
      })
    })

    # pin ----
    observeEvent(input$pin, {
      req(loc$memo_id)
      pin_id <- paste0("pin_id-", loc$memo_id)
      pinned_text <- read_memo_by_id(glob$pool, glob$active_project, loc$memo_id) %>%
        dplyr::pull(memo_text)

      insertUI(
        selector = "div.content-wrapper", where = "afterBegin",
        div(
          id = pin_id, class = "pinned_memo",
          div(
            id = "pin_header", class = "pin_header", icon("thumbtack"),
            div(
              class = "unpin",
              actionButton(paste0("unpin_", pin_id), "", icon("xmark"), class = "unpin_btn", `data-id` = pin_id, onclick = "Shiny.setInputValue('memo_ui_1-unpin', this.dataset.id, {priority: 'event'})")
            ),
          ),
          div(class = "inner_pin", pinned_text),
          div(id = "resize_handle", class = "resizer")
        )
      )
      golem::invoke_js("makeDraggable", list(id = pin_id))
    })

    # unpin -----
    observeEvent(input$unpin, {
      removeUI(paste0("#", input$unpin))
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
