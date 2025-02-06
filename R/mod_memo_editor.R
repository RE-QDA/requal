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
  print(ns("test"))
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
mod_memo_editor_server <- function(id, glob, memo_id = NULL, segment_start = NULL, segment_end = NULL, iframe = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    loc <- reactiveValues()
    return_values <- reactiveValues(  ) # messages to nesting module
    loc$memo_id  <-  memo_id
    loc$segment_start  <- segment_start
    loc$segment_end  <-  segment_end
    observeEvent(req(glob$active_project), {
    return_values$memo_text  <- NULL
    return_values$memo_id <- NULL
    return_values$segment_start <- NULL
    return_values$segment_end <- NULL
    })

  observeEvent(input$memo_text_external, {
    print(input$memo_text_external)
  })
      observe(print(loc$memo_id))
      observe(print(isTruthy(loc$memo_id)))
    output$text_input_area <- renderUI({
      glob$active_project
        if (isTruthy(loc$memo_id))  {
                    textAreaInput(ns("memo_text_internal"), NULL, placeholder = "Memo text", width = "100%", height = "100%", resize = "none") |> tagAppendAttributes(class = "memo_segment_input")

        } else {
          tags$iframe(
            src = "www/memo.html",
            class = "memo_segment_input"
          )
        }
    })
    # Open memo to read/edit ----
    observeEvent(req(loc$memo_id), {

      
  
        print(paste("Triggered by memo_id:", loc$memo_id))
      memo_called_df <- read_memo_by_id(glob$pool, glob$active_project, loc$memo_id)
          memos_segments_map <- dplyr::tbl(pool, "memos_segments_map") |> 
        dplyr::filter(memo_id == !!memo_called_df$memo_id) |> 
        dplyr::collect()
      loc$can_modify <- find_memo_permission(memo_called_df$user_id, glob$user)
      print(memo_called_df$memo_text)

    })

     observeEvent(input$cancel, {
      loc$memo_id <- NULL
      loc$segment_start <- NULL
      loc$segment_end <- NULL
      golem::invoke_js('refreshMemoIframe', list())

    })
    observeEvent(input$save_close, {
      browser()
    })
   
    
    return(return_values)
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
