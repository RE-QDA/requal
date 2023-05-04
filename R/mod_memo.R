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
  div(
    tags$b("Memos"),
    uiOutput(ns("new_memo_btn")) %>% 
        tagAppendAttributes(style = "display: inline-block; float: right"),
    hr(),
    DT::dataTableOutput(ns("memo"))
  ) %>% tagAppendAttributes(class = "scrollable80")
}

#' memo Server Functions
#'
#' @noRd
mod_memo_server <- function(id, glob) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    memo_list <- reactiveVal()
    output$new_memo_btn <- renderUI({
        actionButton(ns("new_memo"), "New memo")
    })

    output$memo <- DT::renderDataTable({
      if (isTruthy(glob$active_project)) {
        memo_list(list_memo_records(glob$pool, glob$active_project))
        
        DT::datatable(memo_list() %>%
          dplyr::arrange(dplyr::desc(memo_id)) %>%
          dplyr::mutate(memo_name = memo_link(memo_id, memo_name)) %>%
          dplyr::select(memo_name),
        options = memo_table_options(),
        class = "compact",
        escape = FALSE,
        rownames = FALSE,
        colnames = NULL,
        selection = "none"
        )
      }
    })


    # new memo ----
    observeEvent(input$new_memo, {
      showModal(
        modalDialog(
          title = "New memo",
          textAreaInput(ns("memo_text"), "Text",
            value = "", width = "100%", height = "100%",
            placeholder = "First 50 characters of the first line will become a searchable title..."
          ) %>% tagAppendAttributes(style = "height: 50vh"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("save_close"), "Save & Close", class = "btn-success")
          )
        )
      )
    })

    observeEvent(input$save_close, {
      add_memo_record(
        pool = glob$pool,
        project = glob$active_project,
        text = req(input$memo_text),
        user_id = glob$user$user_id
      )

      memo_list(list_memo_records(glob$pool, glob$active_project))

      removeModal()
    })


    # Display selected memo -----
    observeEvent(input$selected_memo, {
      showModal(
        modalDialog(
          title = paste0(memo_list() %>%
            dplyr::filter(memo_id == as.integer(input$selected_memo)) %>%
            dplyr::pull(.data$memo_name)),
          textAreaInput(ns("displayed_memo_text"), "Text",
            value = read_memo_db(glob$pool, input$selected_memo),
            width = "100%", height = "100%",
            placeholder = "First 50 characters of the first line will become a searchable title..."
          ) %>% tagAppendAttributes(style = "height: 50vh"),
          footer = tagList(
            modalButton("Close"),
            actionButton(ns("save_changes"), "Save & Close", class = "btn-success"),
            actionButton(ns("delete_memo"), "Delete", class = "btn-danger")
          )
        )
      )
    })

    observeEvent(input$save_changes, {
      update_memo_record(glob$pool, glob$project, 
                         input$selected_memo, req(input$displayed_memo_text),
        user_id = glob$user$user_id
      )

      memo_list(list_memo_records(glob$pool, glob$active_project))

      removeModal()
    })

    observeEvent(input$delete_memo, {
      delete_memo_record(glob$pool, glob$active_project, input$selected_memo,
        user_id = glob$user$user_id
      )

      memo_list(list_memo_records(glob$pool, glob$active_project))

      removeModal()
    })
  })
}
