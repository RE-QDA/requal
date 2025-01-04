#' document_code UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_document_code_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    tags$head(
      tags$script(src = "www/split.min.js"),
      tags$script(src = "www/highlight_style.js"),
      tags$script(src = "www/document_code_js.js"),
      tags$script(HTML("
  document.addEventListener('DOMContentLoaded', (event) => {
    Split(['#split-1', '#split-2'], {
      sizes: [80, 20],
      minSize: [100, 100]
    });
  });
"))
    ),
    fluidRow(
      column(
        width = 5,
        selectInput(
          ns("doc_selector"),
          label = "Select a document to code",
          choices = "",
          selected = ""
        )
      ),
      column(
        width = 5,
        div(style = "float: right;",
        actionButton(
          ns("toggle_style"),
          label = "",
          icon = icon("highlighter")
        ) %>% tagAppendAttributes(title = "Highlight style"),
        actionButton(
          ns("doc_refresh"),
          label = "",
          icon = icon("sync")
        ) %>% tagAppendAttributes(title = "Reload document")
        )), 
      column(
        width = 2,
        div(style = "text-align: right;",
          actionButton(
          ns("code_columns"),
          label = "",
          icon = icon("table-columns")
        ) %>% tagAppendAttributes(title = "Code columns"),
        br(), "Selection:", br(),
        textOutput(ns("captured_range"))
      )
      )
    ),
    fluidRow(
      style = "height: 90%",
      tags$div(
        style = "display: flex;",
        class = "split",
        tags$div(
          id = "split-1",
          style = "flex-grow: 1; flex-shrink: 1; overflow: auto;",
          htmlOutput(ns("focal_text")) %>% tagAppendAttributes(class = "scrollable80")
        ),
        tags$div(
          id = "split-2",
          style = "flex-grow: 1; flex-shrink: 1; overflow: auto;",
          tags$b("Codes"),
          br(),
          actionButton(
            ns("remove_codes"),
            "Remove code",
            class = "btn-danger",
            width = "100%"
          ),
          tags$div(
            style = "height: calc(1.5em + .75rem + 10px); display: flex; align-items: center; margin-top: 0.5em; margin-bottom: 0.5em;",
            tags$div(
              style = "flex-grow: 1; height: calc(1.5em + .75rem + 10px);",
              tags$iframe(
                src = "www/quickcode.html",
                style = "height: calc(1.5em + .75rem + 10px); width: 100%; border: none;"
              )
            ),
            actionButton(ns("quickcode_btn"), "Quick tag", icon = icon("bolt-lightning"), style = "height: calc(1.5em + .75rem + 10px); margin-left: 0px;")
          ),
          uiOutput(ns("code_list"))  %>% tagAppendAttributes(class = "scrollable80")
        )
      )
    )
  )
}

#' document_code Server Functions
#'
#' @noRd
mod_document_code_server <- function(id, glob) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    loc <- reactiveValues()
    loc$highlight <- "background"
    loc$code <- NULL
    observeEvent(req(glob$active_project), {
    loc$codes_menu_observer <- 0
    loc$code_action_observer <- 0
    loc$text_observer <- 0
    loc$scroll <- 0
    })

    # Observers - definitions ----
    ## Observe click on coded text ----
    observeEvent(input$clicked_title, {
      showNotification(input$clicked_title)
    })
    ## Observe choice of highlight style ----
    observeEvent(input$toggle_style, {
      loc$highlight <- ifelse(loc$highlight == "underline", "background", "underline")
      # Send a message to the client to toggle the style
      session$sendCustomMessage("toggleStyle", message = loc$highlight)
    })

    ## Observe changes in documents
    observeEvent(glob$documents, {
      if (isTruthy(glob$active_project)) {
        if (glob$user$data$data_other_view == 1) {
          updateSelectInput(
            session = session,
            "doc_selector",
            choices = c("", glob$documents)
          )
        } else {
          visible_docs <- read_visible_docs(
            glob$pool, glob$active_project,
            glob$user$user_id
          )
          updateSelectInput(
            session = session,
            "doc_selector",
            choices = c("", visible_docs)
          )
        }
      }
    })
    ## Doc sel or refresh ----
    # Update loc$text when input$doc_selector or input$doc_refresh changes
    observeEvent(c(input$doc_selector, 
                  input$doc_refresh), {
        req(input$doc_selector)
        loc$codes_menu_observer  <- loc$codes_menu_observer + 1 # must run first
        loc$text_observer <- loc$text_observer + 1
    })

    ## Observe refresh ----
    # Update loc$codes_menu when input$doc_refresh or glob$codebook changes
    observeEvent(input$doc_refresh, {
      loc$codes_menu_observer <- loc$codes_menu_observer + 1
    })

    ## Code columns observer -----
    observeEvent(input$code_columns, {
      shinyjs::toggleClass(id = "codes_menu", "two_columns", asis = TRUE)
    })

    ## Codes menu observer ---- 
    observeEvent(req(loc$codes_menu_observer), {
      req(loc$codes_menu_observer > 0) # ignore init value
      loc$codebook <- list_db_codes(
        glob$pool,
        glob$active_project,
        user = glob$user
      )
     
      code_labels <- purrr::pmap(
          list(
            loc$codebook$code_id,
            loc$codebook$code_name,
            loc$codebook$code_color,
            loc$codebook$code_description
          ),
          ~ generate_coding_tools(
            ns = ns,
            code_id = ..1,
            code_name = ..2,
            code_color = ..3,
            code_desc = ..4
          ))
        
      loc$codes_menu <- sortable::rank_list(
        input_id = "codes_menu",
        labels = code_labels
      ) 
    })
  ## Observe Analyze screen ----
  # Listen to message from Analyze screen
  observeEvent(glob$analyze_link, {
  updateSelectInput(session = session, "doc_selector", choices = c("", glob$documents), selected = glob$analyze_link$doc_id)
  loc$codes_menu_observer  <- loc$codes_menu_observer + 1
  loc$text_observer <- loc$text_observer + 1
  loc$scroll <- loc$scroll + 1
  })
  observeEvent(loc$scroll , {
      req(isTruthy(loc$scroll))
      session$sendCustomMessage(type = 'scrollToSegment', message = glob$analyze_link$segment_start)
  })

    # Render text and codes ----
    output$focal_text <- renderText({
      req(isTruthy(loc$text))
      loc$text
    })
    output$code_list <- renderUI({
      req(isTruthy(loc$codes_menu))
      loc$codes_menu
    })

    # Load text observer ----
    observeEvent(req(loc$text_observer), {
      req(loc$text_observer > 0) # ignore init value
      loc$text <- load_doc_to_display(
          glob$pool,
          glob$active_project,
          user = glob$user,
          input$doc_selector,
          loc$codebook,
          highlight = loc$highlight,
          ns = NS(id)
        )
    })

    # Coding tools ------------------------------------------------------------
    observeEvent(req(input$selected_code), {
      # We need a document and selection positions
      req(input$doc_selector)
      req(input$tag_position)
      # Register code for which action is executed
      loc$code <- input$selected_code
      # Call code action observer
      loc$code_action_observer <- loc$code_action_observer + 1
    })
    ## Codes action observer ----
    # Write code to DB when observer is updated
    observeEvent(req(loc$code_action_observer), {
      req(loc$code_action_observer > 0) # ignore init value
      req(loc$code)
      # To execute, we need a document and a selection
      startOff <- parse_tag_pos(req(input$tag_position), "start")
      endOff <- parse_tag_pos(req(input$tag_position), "end")

      if (endOff >= startOff) {
        write_segment_db(
          glob$pool,
          glob$active_project,
          user_id = glob$user$user_id,
          doc_id = input$doc_selector,
          code_id = loc$code,
          startOff,
          endOff
        )
        # TODO JS implementation of coding
        #     code_info <- glob$codebook |>
        #      dplyr::filter(code_id == input$selected_code)

        # session$sendCustomMessage(type = 'wrapTextWithBold', message = list(
        # startOffset = startOff-1, #convert to javascript
        # endOffset = endOff,
        # newId = input$selected_code,
        # color = code_info$code_color,
        # title = code_info$code_name
        # ))

        loc$text_observer <- loc$text_observer + 1
        glob$segments_observer <- glob$segments_observer + 1
      }
    })
    
    # Quick code tools ----
    observeEvent(input$quickcode_btn, {
      
      # We need a document and selection positions
      req(input$doc_selector)
      req(input$tag_position)
     if (parse_tag_pos(input$tag_position, "start") < parse_tag_pos(input$tag_position, "end")) {
         session$sendCustomMessage(type = "getIframeContent", message = list())
         session$sendCustomMessage(type = 'refreshIframe', message = list())
      } else {
         rql_message("Missing selected text segment.")
      } 

    })
    # After quickcode button is pressed, we wait for the quickcode value 
    observeEvent(req(input$quickcode), {
     # check if code name is unique
     if (input$quickcode %in% glob$codebook$code_name) {
        warn_user("Code names must be unique and non-empty.")
      } else {
        codes_input_df <- data.frame(
          project_id = glob$active_project,
          code_name = input$quickcode,
          code_description = "",
          code_color = "rgb(255,255,0)",
          user_id = glob$user$user_id
        )
        # Add new quickcode to codes database
        # and register the new code_id
         loc$code <- add_quickcode_record(
          pool = glob$pool,
          project_id = glob$active_project,
          codes_df = codes_input_df, 
          user_id = glob$user$user_id
        )
        # Refresh codes menu
        loc$codes_menu_observer <- loc$codes_menu_observer + 1
        # Execute coding action
        loc$code_action_observer <- loc$code_action_observer + 1
        # Refresh text 
         loc$text_observer <- loc$text_observer + 1
        # Notify codebook screen about new quick code
        glob$codebook_observer  <- ifelse(
          !isTruthy(glob$codebook_observer), 
          0, glob$codebook_observer + 1)
        # Notify user
        rql_message(paste(input$quickcode,"added to codebook."))
      } 
    })

    # Segment removal ----------
    observeEvent(input$remove_codes, {
      req(glob$active_project)
      req(input$doc_selector)

      if (glob$user$data$annotation_other_modify == 0) {
        loc$marked_segments_df <- load_segment_codes_db(
          glob$pool,
          glob$active_project,
          user_id = glob$user$user_id,
          active_doc = input$doc_selector,
          marked_codes = parse_tag_pos(
            input$tag_position,
            "start"
          )
        )
      } else {
        loc$marked_segments_df <- load_segment_codes_db(
          glob$pool,
          glob$active_project,
          user_id = NULL,
          active_doc = input$doc_selector,
          marked_codes = parse_tag_pos(
            input$tag_position,
            "start"
          )
        )
      }

      if (nrow(loc$marked_segments_df) == 0) {
        NULL
      } else if (nrow(loc$marked_segments_df) == 1) {
        delete_segment_codes_db(
          glob$pool,
          glob$active_project,
          user_id = glob$user$user_id,
          doc_id = input$doc_selector,
          segment_id = loc$marked_segments_df$segment_id
        )
        # Refresh text
        loc$text_observer <- loc$text_observer + 1
        # Notify analysis screen
        glob$segments_observer <- glob$segments_observer + 1
      } else {
        # Obtain additional input if multiple segments are to be removed
        showModal(
          modalDialog(
            checkboxGroupInput(ns("codes_to_remove"),
              label = "",
              choiceValues = loc$marked_segments_df$segment_id,
              choiceNames = loc$marked_segments_df$code_name,
              selected = FALSE
            ),
            title = "Codes in this segment",
            "Remove selected codes from segment?",
            easyClose = TRUE,
            footer = tagList(
              modalButton("Dismiss"),
              actionButton(ns("remove_codes_select"),
                "Remove selected",
                class = "btn-danger"
              )
            ),
            fade = TRUE
          )
        )
      }
    })

    # Multiple segments removal ----
    observeEvent(input$remove_codes_select, {
      if (isTruthy(input$codes_to_remove)) {
        delete_segment_codes_db(
          glob$pool,
          glob$active_project,
          user_id = glob$user$user_id,
          doc_id = input$doc_selector,
          segment_id = input$codes_to_remove
        )
        removeModal()

        # Refresh text
        loc$text_observer <- loc$text_observer + 1
        # Notify analysis screen
        glob$segments_observer <- glob$segments_observer + 1
      }
    })

    # Helper: position counter ---------------
    output$captured_range <- renderText({
      req(isTruthy(input$tag_position))
      splitted_range <- strsplit(input$tag_position, split = "-")
      if (splitted_range[[1]][1] == splitted_range[[1]][2]) {
        reported_range <- unique(splitted_range[[1]])
      } else {
        reported_range <- input$tag_position
      }
      paste(reported_range)
    })

    # returns glob$segments_observer and glob$codebook
  })
}
