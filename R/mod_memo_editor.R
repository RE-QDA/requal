#' memo_editor UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_memo_editor_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "memo_editor",
      div(
        class = "memo_segment_container",
        width = "100%",
        uiOutput(ns("text_input_area"))
      ),
      div(
        style = "display: flex;",
        width = "100%",
        actionButton(ns("cancel"), "Cancel"),
        actionButton(ns("save"), "", icon = icon("save")),
        actionButton(ns("save_close"), "Save & Close", class = "btn-success"),
        actionButton(ns("delete_memo"), "Delete", class = "btn-danger")
      )
    )
  )
}

#' memo_editor Server Functions
#'
#' @noRd
mod_memo_editor_server <- function(id, glob, type = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    loc <- reactiveValues()
    print(ns("memo_text_input"))
    output$text_input_area <- renderUI({
      loc$editor_ui
    })  
    observeEvent(input$memo_id, {
      if (isTruthy(input$memo_id)) {
         memo_called_df <- read_memo_by_id(glob$pool, glob$active_project, input$memo_id)
        memos_segments_map <- dplyr::tbl(pool, "memos_segments_map") |>
          dplyr::filter(memo_id == !!memo_called_df$memo_id) |>
          dplyr::collect()
        loc$can_modify <- find_memo_permission(memo_called_df$user_id, glob$user)
        loc$editor_ui <- editor_ui(type = NULL, ns = ns, memo_text = memo_called_df$memo_text)
      } else {
        loc$editor_ui <- editor_ui(type = type, ns = ns, memo_text = NULL)

      }
    }, ignoreNULL = FALSE)
    observe(print(input$memo_text_input))

   observeEvent(input$cancel, {
       golem::invoke_js("refreshMemoIframe")
       golem::invoke_js("updateEditorInput", 
              list(ns_memo_id = ns("memo_id"),
                    id = ""))
    })

#     observeEvent(input$save_close, {
#       browser()
#     })



  })
}

## To be copied in the UI
# mod_memo_editor_ui("memo_editor_1")

## To be copied in the server
# mod_memo_editor_server("memo_editor_1")
# library(shiny)
# ui <- shiny::fluidPage(
#   mod_memo_editor_ui("testModule")
# )

# # Define the server logic for the test app
# server <- function(input, output, session) {
#   mod_memo_editor_server("testModule")
# }

# # Run the test app
# shiny::shinyApp(ui, server)




editor_ui <- function(type, ns, memo_text = NULL) {
  tagList(
    if (is.null(type)) {
      textAreaInput(
        ns("memo_text_editor"), 
        NULL, 
        value = if (!is.null(memo_text)) memo_text, 
        width = "100%", 
        height = "100%",
        resize = "vertical"
      ) |> tagAppendAttributes(class = "memo_segment_input")
    } else {
      golem::invoke_js("initializeIframeHandler")
       div(
        style = "width: 100%; height: 100%; resize: vertical",
        tags$iframe(
        src = "www/memo.html",
        class = "memo_segment_input"
      ))
      
    }
  )
}