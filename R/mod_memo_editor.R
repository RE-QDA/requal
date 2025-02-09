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
        shinyjs::hidden(actionButton(ns("save"), "", icon = icon("save"), class = "btn-warning")),
        shinyjs::hidden(actionButton(ns("create_new"), "Create new", class = "btn-success")),
        shinyjs::hidden(actionButton(ns("save_close"), "Save & Close", class = "btn-success")),
        shinyjs::hidden(actionButton(ns("delete_memo"), "Delete", class = "btn-danger"))
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
    loc$memo_text_input <- ""
    loc$refresh_observer <- 0
    loc$save_observer <- 0


    # Render composer/editor ----
    output$text_input_area <- renderUI({
      loc$editor_ui$taglist
    })  
    observeEvent(input$memo_id, {
      if (isTruthy(input$memo_id)) {
               golem::invoke_js("resetSegmentMemoInput")

         memo_called_df <- read_memo_by_id(glob$pool, glob$active_project, input$memo_id)
                 loc$edited_memo_id <- memo_called_df$memo_id

        memos_segments_map <- dplyr::tbl(pool, "memos_segments_map") |>
          dplyr::filter(memo_id == !!loc$edited_memo_id) |>
          dplyr::collect()
        loc$segment_id <- memos_segments_map$segment_id
        loc$can_modify <- find_memo_permission(memo_called_df$user_id, glob$user)
        loc$edited_memo_text <- memo_called_df$memo_text
        loc$editor_ui <- editor_ui(type = NULL, ns = ns, memo_text = loc$edited_memo_text)
      } else {
        loc$editor_ui <- editor_ui(type = type, ns = ns, memo_text = NULL)

      }
    }, ignoreNULL = FALSE)

   # Refresh composer
   observeEvent(loc$refresh_observer, {
      req(loc$refresh_observer > 0)
       golem::invoke_js("resetSegmentMemoInput", list())
       golem::invoke_js("updateEditorInput", 
              list(ns_memo_id = ns("memo_id"),
                    id = ""))
      loc$memo_text_input <- ""
    })
    observeEvent(input$cancel, {
      loc$refresh_observer <- loc$refresh_observer + 1
    })
    

    # Adjust UI to different editor states ----
    observeEvent(loc$editor_ui$editor_state, {
        if (loc$editor_ui$editor_state == "composer") {
           shinyjs::show("create_new", animType = "fade")
           shinyjs::hide("save_close")
           shinyjs::hide("save")
        } else if(loc$editor_ui$editor_state == "editor") {
           shinyjs::hide("create_new")
        }
    })

    # Monitor difference between saved and input text, adjust UI 
    observeEvent(input$memo_text_editor, {
      if (input$memo_text_editor != loc$edited_memo_text && 
      loc$editor_ui$editor_state == "editor") {
           shinyjs::show("save", animType = "fade")
           shinyjs::show("save_close", animType = "fade")
        } else if (input$memo_text_editor == loc$edited_memo_text && 
      loc$editor_ui$editor_state == "editor") {
           shinyjs::hide("save")
           shinyjs::hide("save_close")
        }
    })
    # Consolidate input text ----
    # from different sources into loc$memo_text_input
    observeEvent(c(input$memo_text_editor, input$memo_text_input), {
      if (loc$editor_ui$editor_state == "editor") {
              loc$memo_text_input <- input$memo_text_editor
      } else {
             loc$memo_text_input <- input$memo_text_input
      }
    })
       
    # Add new free segment memo ----
    observeEvent(input$create_new, {
      
      if (type == "free_segment" & glob$endOff >= glob$startOff)
       new_segment_id <- write_memo_segment_db(
          pool = glob$pool,
          active_project = glob$active_project,
          user_id = glob$user$user_id,
          doc_id = glob$doc_selector,
          code_id = NA,
          glob$startOff,
          glob$endOff
        )
      new_memo_id  <- add_memo_record(
        pool = glob$pool,
        project = glob$active_project,
        text = loc$memo_text_input,
        user_id = glob$user$user_id
      )
      new_memo_segment_map <- data.frame(memo_id = new_memo_id, segment_id = new_segment_id)
    # the function below can be wrapped to a new add_memo_segment_map(...)
      DBI::dbWriteTable(glob$pool, "memos_segments_map", new_memo_segment_map, append = TRUE, row.names = FALSE)
      glob$memo_segment_observer <- glob$memo_segment_observer + 1
       loc$refresh_observer <- loc$refresh_observer + 1
    #   #glob$memos_observer <- glob$memos_observer + 1 enable after memo screen exists to initialize the glob
    })

    # Save edits ----
    observeEvent(loc$save_observer, {
      req(loc$save_observer > 0)
      update_memo_record(
        pool = glob$pool,
        project = glob$active_project,
        memo_id = loc$edited_memo_id,
        memo_text = loc$memo_text_input,
        user_id = glob$user$user_id
        )
       segment_df <-  dplyr::tbl(pool, "segments") %>%
            dplyr::filter(.data$segment_id == !!loc$segment_id) %>%
            dplyr::collect()
        glob$startOff <- segment_df$segment_start
                glob$endOff <- segment_df$segment_end
        glob$memo_segment_observer <- glob$memo_segment_observer + 1
    })
    observeEvent(input$save, {
      loc$save_observer <- loc$save_observer + 1
    })

    observeEvent(input$save_close, {
      loc$save_observer <- loc$save_observer + 1
      loc$refresh_observer <- loc$refresh_observer + 1
    })



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
  
    if (is.null(type)) {
      list(editor_state = "editor",
            taglist = textAreaInput(
        ns("memo_text_editor"), 
        NULL, 
        value = if (!is.null(memo_text)) memo_text, 
        width = "100%", 
        height = "100%",
        resize = "vertical"
      ) |> tagAppendAttributes(class = "memo_segment_input"))
    } else {
      golem::invoke_js("initializeIframeHandler")
      list(editor_state = "composer",
        taglist = div(
        style = "width: 100%; height: 100%; resize: vertical",
        tags$iframe(
        src = "www/memo.html",
        class = "memo_segment_input"
      )))
      
    }
}