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
        class = "memo_container",
        width = "100%",
        uiOutput(ns("text_input_area"), style = "height: 100%")
      ),
      div(
        style = "display: flex; flex-wrap: wrap;",
        width = "100%",
        actionButton(ns("cancel"), "Cancel"),
        shinyjs::hidden(actionButton(ns("save"), "Save", class = "btn-success")),
        shinyjs::hidden(actionButton(ns("create_new"), "Create new", class = "btn-success")),
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
    observeEvent(glob$active_project, {
    loc$memo_text_input <- ""
    loc$refresh_observer <- 0
    loc$save_observer <- 0
    })



    # Render composer/editor ----
    output$text_input_area <- renderUI({
      loc$editor_ui$taglist
    })
    observeEvent(input$memo_id,
      {
        if (isTruthy(input$memo_id)) {
            # check if not removed elsewhere
            check_memo <- exists_memo_db(glob$pool, input$memo_id)
            if (!check_memo) {
              warn_user("The memo appears to have been deleted elsewhere. Refresh the document.")
              req(check_memo)
            }
          collect_memo_data_LF()
          loc$can_modify <- find_memo_permission(loc$editing_data$user_id, glob$user)
          # render editor
          loc$editor_ui <- editor_ui(type = NULL, ns = ns, memo_text = loc$editing_data$memo_text)
        } else {
          # render composer
          loc$editor_ui <- editor_ui(type = type, ns = ns, memo_text = NULL)
        }
      },
      ignoreNULL = FALSE
    )

    # Refresh composer ----
    observeEvent(loc$refresh_observer, {
      req(loc$refresh_observer > 0)
      if (type == "free_segment") {
        golem::invoke_js("resetSegmentMemoInput", list())
      } else {
      updateTextAreaInput(session = session, "memo_text_input", value = "")
      }
      # reset memo id input
      golem::invoke_js(
        "updateEditorInput",
        list(
          ns_memo_id = ns("memo_id"),
          id = NULL
        )
      )
      loc$memo_text_input <- ""
    })
    observeEvent(input$cancel, {
      loc$refresh_observer <- loc$refresh_observer + 1
    })


    # Adjust UI to different editor states ----
    observeEvent(loc$editor_ui$editor_state, {
      if (loc$editor_ui$editor_state == "composer") {
        shinyjs::show("create_new", animType = "fade")
        shinyjs::hide("save")
        shinyjs::hide("delete_memo")
      } else if (loc$editor_ui$editor_state == "editor") {
        shinyjs::hide("create_new")
        shinyjs::show("delete_memo")
      }
    })

    # Monitor difference between saved and input text, adjust UI
    observeEvent(c(input$memo_text_editor, loc$save_observer), {
      if (input$memo_text_editor != loc$editing_data$memo_text &&
        loc$editor_ui$editor_state == "editor") {
        shinyjs::show("save", animType = "fade")
      } else if (input$memo_text_editor == loc$editing_data$memo_text &&
        loc$editor_ui$editor_state == "editor") {
        shinyjs::hide("save")
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

    # Add new free memo ----
    observeEvent(input$create_new, {
      create_memo_LF()
      loc$refresh_observer <- loc$refresh_observer + 1
      #   #glob$memos_observer <- glob$memos_observer + 1 enable after memo screen exists to initialize the glob
    })

    # Save edits ----
    observeEvent(loc$save_observer, {
      req(loc$save_observer > 0)
      update_memo_record(
        pool = glob$pool,
        project = glob$active_project,
        memo_id = loc$editing_data$memo_id,
        memo_text = loc$memo_text_input,
        user_id = glob$user$user_id
      )
      if (type == "free_segment") {
      glob$startOff <- loc$editing_data$startOff
      glob$endOff <- loc$editing_data$endOff
      glob$memo_segment_observer <- glob$memo_segment_observer + 1
      memo_html <- icon("sticky-note", class = "fas text_memo_btn", `data-memo` = loc$memo_text_input, .noWS = c("outside", "after-begin", "before-end"))
      golem::invoke_js("updateElementContent", list(id =  paste0("memo_id_", loc$editing_data$memo_id), content = as.character(memo_html)))
      # golem::invoke_js("addMemoHighlight", list(id =  paste0("memo_id_", loc$editing_data$memo_id)))
      } else if (type == "free_memo") {
        glob$free_memo_observer <- glob$free_memo_observer + 1
      }
      loc$editing_data$memo_text <- loc$memo_text_input
    })
    observeEvent(input$save, {
      loc$save_observer <- loc$save_observer + 1
    })

    # Delete memo ----
    observeEvent(input$delete_memo, {
      delete_memo_LF()
      if (type == "free_memo") glob$free_memo_observer <- glob$free_memo_observer + 1
      loc$refresh_observer <- loc$refresh_observer + 1
    })

    #  observeEvent(input$delete_memo, {
  
    #   showModal(
    #     modalDialog(
    #       title = "Are you sure?",
    #         tags$span("You are about to delete the memo and lose its content:",
    #         tags$b(loc$memo$memo_name)),
    #       easyClose = TRUE,
    #       footer = tagList(
    #         modalButton("Dismiss"),
    #         actionButton(ns("delete_memo_confirmation"),
    #           "Yes, I am sure.",
    #           class = "btn-danger"
    #         )
    #       ),
    #       fade = TRUE
    #     )
    #   )
    # })

    # observeEvent(input$delete_memo_confirmation, {
    #    delete_memo_record(glob$pool, glob$active_project, input$selected_memo,
    #     user_id = glob$user$user_id
    #   )
    # removeModal()
    #   })

    # create_memo_LF ----
    create_memo_LF <- function() {
      if (!isTruthy(loc$memo_text_input)) rql_message("Missing text input for memo.")
      req(loc$memo_text_input)
      ## create free segment ----
      if (type == "free_segment" & glob$endOff >= glob$startOff) {
        if (glob$doc_selector < 1) rql_message("Missing document input for memo.")
        req(glob$doc_selector > 0)
        new_segment_id <- write_memo_segment_db(
          pool = glob$pool,
          active_project = glob$active_project,
          user_id = glob$user$user_id,
          doc_id = glob$doc_selector,
          code_id = NA,
          glob$startOff,
          glob$endOff
        )
        loc$new_memo_id <- add_memo_record(
          pool = glob$pool,
          project = glob$active_project,
          text = loc$memo_text_input,
          user_id = glob$user$user_id
        )
       loc$memo_text_data <- loc$memo_text_input
        new_memo_segment_map <- data.frame(memo_id = loc$new_memo_id , segment_id = new_segment_id)
        DBI::dbWriteTable(glob$pool, "memos_segments_map", new_memo_segment_map, append = TRUE, row.names = FALSE)
        glob$memo_segment_observer <- glob$memo_segment_observer + 1
        golem::invoke_js("getMemoParagraph", list(startOff = glob$startOff))

      } else if (type == "free_memo") {
        ## create free memo ----
        add_memo_record(
          pool = glob$pool,
          project = glob$active_project,
          text = loc$memo_text_input,
          user_id = glob$user$user_id
        )
        glob$free_memo_observer <- glob$free_memo_observer + 1

      }
    }
   ## update memo UI after create ----
    observeEvent(req(input$active_memo_par), {
      memo_html <- span(id = paste0("memo_id_", loc$new_memo_id), icon("sticky-note", class = "fas text_memo_btn", `data-memo` = loc$memo_text_data, .noWS = c("outside", "after-begin", "before-end")),
                            .noWS = c("outside", "after-begin", "before-end"))
     insertUI(paste0("#", input$active_memo_par$id), where = "beforeEnd", ui = memo_html)

    })

    # collect_memo_data_LF -----
    collect_memo_data_LF <- function() {
      # if memo_id is provided, collect data on existing memo
      memo_df <- read_memo_by_id(glob$pool, glob$active_project, input$memo_id)
      if (type == "free_segment") {
        golem::invoke_js("resetSegmentMemoInput")
        memos_segments_map <- dplyr::tbl(glob$pool, "memos_segments_map") %>%
          dplyr::filter(memo_id == !!memo_df$memo_id) %>%
          dplyr::collect()
        segment_df <- dplyr::tbl(glob$pool, "segments") %>%
          dplyr::filter(.data$segment_id == !!memos_segments_map$segment_id) %>%
          dplyr::collect()
        loc$editing_data <- list(
        segment_id = memos_segments_map$segment_id,
        startOff = segment_df$segment_start,
        endOff = segment_df$segment_start
      )
      } else if (type == "free_memo") {
        NULL
      }
      loc$editing_data$user_id <- memo_df$user_id
      loc$editing_data$memo_id <- memo_df$memo_id
      loc$editing_data$memo_text <- memo_df$memo_text
      
    }
    # delete_memo_LF ----
    delete_memo_LF <- function() {
      ## delete free segment ----
      if (type == "free_segment") {
        # check if not removed elsewhere
        check_memo <- exists_memo_db(glob$pool, loc$editing_data$memo_id)
        if (!check_memo) {
          warn_user("The memo appears to have been deleted elsewhere. Refresh the document.")
           req(check_memo)
        }
        
        delete_memo_record(
          pool = glob$pool,
          project = glob$active_project,
          memo_id = loc$editing_data$memo_id,
          user_id = glob$user$user_id
        )
        delete_segment_codes_db(
          pool = glob$pool,
          active_project = glob$active_project,
          user_id = glob$user$user_id,
          segment_id = loc$editing_data$segment_id
        )
        glob$startOff <- loc$editing_data$startOff
        glob$endOff <- loc$editing_data$endOff
        glob$memo_segment_observer <- glob$memo_segment_observer + 1
        removeUI(selector = paste0("#memo_id_", loc$editing_data$memo_id))
        golem::invoke_js("removeMemoFromText", list(id = loc$editing_data$memo_id))
      } else if (type == "free_memo") {
        ## delete free  ----
        delete_memo_record(
          pool = glob$pool,
          project = glob$active_project,
          memo_id = loc$editing_data$memo_id,
          user_id = glob$user$user_id
        )
      }
    }
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
    list(
      editor_state = "editor",
      taglist = textAreaInput(
        ns("memo_text_editor"),
        NULL,
        value = if (!is.null(memo_text)) memo_text,
        width = "100%",
        height = "100%",
        resize = "none"
      ) %>% tagAppendAttributes(class = "memo_input")
    )
  } else if (type == "free_segment") {
    golem::invoke_js("initializeIframeHandler")
    list(
      editor_state = "composer",
      taglist = div(
        style = "width: 100%; height: 100%; scrollbar-width: none;",
        tags$iframe(
          src = "www/memo.html",
          class = "memo_input"
        )
      )
    )
  } else if (type != "free_segment") {
    list(
      editor_state = "composer",
      taglist = textAreaInput(
        ns("memo_text_input"),
        NULL,
        value = "",
        width = "100%",
        height = "100%",
        resize = "none"
      ) %>% tagAppendAttributes(class = "memo_input")
    )
  }
}