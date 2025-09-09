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
      style = "display: flex; align-items: flex-start; justify-content: space-between; width: 100%;",
      div(
        style = "display: flex; align-items: flex-start; margin-left: 30px;",
        div(
          style = "min-width: 40vh;",
          mod_memo_editor_ui(ns("memo_main_editor"))
        ),
        div(
          style = "display: flex; align-items: center; margin-left: 10px;",
          actionButton(
            ns("pin"),
            "",
            title = "Pin memo",
            icon = icon("thumbtack"),
            class = "pinned"
          )
        )
      ),
      div(
        style = "margin-right: 30px;",
        actionButton(
          ns("reload_memo_table"),
          NULL,
          icon = icon("sync")
        )
      )
    ),
    hr(),
    fluidRow(
      style = "margin-left: 30px;",
      div(style = "width: 60vw;", DT::dataTableOutput(ns("memo")))
    )
  )
}

#' memo Server Functions
#'
#' @noRd
mod_memo_server <- function(id, glob) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    loc <- reactiveValues(memo_observer = 0)

    mod_memo_editor_server("memo_main_editor", glob, type = "free_memo")

    observeEvent(glob$active_project, {
      loc$memo_observer <- loc$memo_observer + 1
    })

    observeEvent(input$text_memo_click, {
      req(input$text_memo_click, glob$free_memo_observer > 0)
      loc$memo_id <- parse_memo_id(input$text_memo_click)
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

    observeEvent(
      c(
        loc$memo_observer,
        glob$memo_segment_observer,
        glob$free_memo_observer,
        input$reload_memo_table
      ),
      {
        output$memo <- DT::renderDataTable({
          req(glob$active_project)
          memo_table <- list_memo_records(glob$pool, glob$active_project)
          if (glob$user$data$memo_other_view == 0) {
            memo_table <- dplyr::filter(
              memo_table,
              user_id == glob$user$user_id
            )
          }
          req(nrow(memo_table) > 0)

          enriched_memo_table <- enrich_memo_table(memo_table, glob$pool, ns)
          loc$enriched_memo_table <- enriched_memo_table

          DT::datatable(
            enriched_memo_table,
            rownames = FALSE,
            width = "100%",
            colnames = c(
              "ID" = "memo_id",
              "Title" = "memo_title",
              "Type" = "memo_type",
              "Document" = "doc_name",
              "Creator" = "user_name",
              "Creator ID" = "user_id"
            ),
            filter = "top",
            escape = FALSE,
            extensions = c("Buttons"),
            options = dt_memo_options(),
            class = "display",
            selection = "none"
          )
        })
      }
    )

    observeEvent(input$pin, {
      req(loc$memo_id)
      pin_memo(loc$memo_id, glob$pool, glob$active_project, ns)
    })

    observeEvent(input$unpin, {
      removeUI(paste0("#", input$unpin))
    })
  })
}
