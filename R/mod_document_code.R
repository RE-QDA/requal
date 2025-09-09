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
      tags$script(HTML(
        "
  document.addEventListener('DOMContentLoaded', (event) => {
    Split(['#split-1', '#split-2'], {
      sizes: [80, 20],
      minSize: [100, 100]
    });
  });
"
      )),
    ),
    fluidRow(
      style = "height: 90%",
      tags$div(
        style = "display: flex;",
        class = "split",
        tags$div(
          id = "split-1",
          style = "flex-grow: 1; flex-shrink: 1;",
          fluidRow(
            class = "doc_sel_row",
            div(
              style = "flex: 1; margin-right: 10px;",
              selectInput(
                ns("doc_selector"),
                label = "Select a document to code",
                choices = "",
                selected = ""
              ) %>%
                tagAppendAttributes(class = "full-width")
            ),
            div(
              style = "display: flex; justify-content: flex-end; align-items: center;",
              actionButton(
                ns("doc_refresh"),
                label = "",
                icon = icon("sync"),
                title = "Reload document",
                style = "margin-bottom: -10px"
              ),
              div(
                id = ns("selection_displayer"),
                style = "margin-left: 20px; margin-right: 5px; min-width: 75px;",
                "Selection:",
                br(),
                textOutput(ns("captured_range"))
              )
            )
          ),
          fluidRow(
            div(
              id = ns("focal_text"),
              tags$article(
                id = "article",
                class = "scrollable80",
                .noWS = "outside"
              ),
              .noWS = "outside"
            )
          )
        ),
        tags$div(
          id = "split-2",
          style = "flex-grow: 1; flex-shrink: 1; overflow: auto;",
          tabsetPanel(
            type = "tabs",
            id = ns("documentcode_tabset"),
            # Codes panel ----
            tabPanel(
              "Codes",
              id = ns("codetools_tab"),
              value = "codetools_tab",
              br(),
              mod_rql_hidden_ui_ui(
                ns("rql_hidden_ui_1"),
                title = "Toggle coding toolbox",
                hidden_tags = tagList(
                  div(
                    style = "display: flex; justify-content: flex-end; align-items: center;",
                    actionButton(
                      ns("code_columns"),
                      label = "",
                      icon = icon("table-columns"),
                      title = "Code columns"
                    ),
                    actionButton(
                      ns("toggle_style"),
                      label = "",
                      icon = icon("highlighter"),
                      title = "Highlight style"
                    ),
                  ),
                  div(
                    style = "height: calc(1.5em + .75rem + 10px); display: flex; align-items: center; margin-top: 0.5em; margin-bottom: 0.5em;",
                    actionButton(
                      ns("clear_iframe"),
                      "",
                      icon = icon("x"),
                      title = "Clear form",
                      style = "margin-right: -15px; padding-right: 0px; padding-left: 5px; position: relative; font-size: x-small; color: gray; border: none; background: transparent;"
                    ),
                    tags$div(
                      style = "flex-grow: 1; height: calc(1.5em + .75rem + 10px);",
                      tags$iframe(
                        src = "www/quickcode.html",
                        style = "height: calc(1.5em + .75rem + 10px); width: 100%; border: none;"
                      )
                    ),
                    actionButton(
                      ns("quickcode_btn"),
                      "Quick tag",
                      icon = icon("bolt-lightning"),
                      title = "Apply new code to selection",
                      style = "height: calc(1.5em + .75rem + 10px); margin-left: 0px;"
                    )
                  )
                )
              ),
              actionButton(
                ns("remove_codes"),
                "Remove code",
                class = "btn-danger",
                width = "100%"
              ),
              uiOutput(ns("code_list")) %>%
                tagAppendAttributes(class = "scrollable80")
            ),
            # Memos panel ----
            tabPanel(
              "Memos",
              id = ns("memotools_tab"),
              value = "memotools_tab",
              br(),
              mod_memo_segment_ui(ns("memo_segment_1"))
            )
          )
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
    # DEVNOTE
    # At this point, it is very messy, but a more suistainable pattern of building html
    # is implemented in text display rendering and it has been partially done
    # for coding actions too. It needs still lot of cleaning and can be optimized
    # the same system needs to be implemented for code deletion
    # I now use JS for highlighting but it will be better to return to css, which
    # means also restoring the old way of calculating overlapping colors
    # th calculation of overlaps seems more or less ok - though it could be improved
    # unless outright bugs are found, we can live with for some time
    # btw, wrapping things in functions can make it more easily maintainable...
    # it seems a simple wrap suffices
    # file with JS will also require some clean up

    ns <- session$ns
    loc <- reactiveValues()
    segment_memos <- reactiveValues() # messages from nested module
    loc$highlight <- "background"
    loc$code <- NULL
    mod_memo_segment_server("memo_segment_1", glob)
    observeEvent(req(glob$active_project), {
      loc$codes_menu_observer <- 0
      loc$code_action_observer <- 0
      glob$doc_selector <- 0
      loc$text_observer <- 0
      loc$display_observer <- 0
      glob$startOff <- 0
      glob$endOff <- 0
      glob$selected_documentcode_tabset <- NULL
      glob$memo_segment_observer <- 0
      golem::invoke_js('clearArticle', list())
      golem::invoke_js('refreshIframe', list())
    })
    mod_rql_hidden_ui_server("rql_hidden_ui_1")
    # Observers - definitions ----

    ## Observe choice of highlight style ----
    observeEvent(input$toggle_style, {
      toggle_style_LF()
    })

    ## Observe offset positions -------
    observeEvent(input$tag_position, {
      glob$startOff <- parse_tag_pos(req(input$tag_position), "start")
      glob$endOff <- parse_tag_pos(req(input$tag_position), "end")
    })
    ## Observe changes in available documents ----
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
            glob$pool,
            glob$active_project,
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
    observeEvent(c(input$doc_selector, input$doc_refresh), {
      golem::invoke_js("setArticleStatusValue", list(status = "loading"))
      req(input$doc_selector)
      glob$doc_selector <- input$doc_selector
      loc$codes_menu_observer <- loc$codes_menu_observer + 1 # must run first
      loc$text_observer <- loc$text_observer + 1
      removeUI("#code_extra_div") # remove code extra div so values can recalculate
      loc$selected_code_extra <- NULL # reset code extra div observer
    })

    ## Observe refresh ----
    # Update loc$codes_menu when input$doc_refresh or glob$codebook changes
    observeEvent(glob$codebook, {
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
        )
      )

      loc$codes_menu <- sortable::rank_list(
        input_id = "codes_menu",
        css_id = "codes_menu",
        labels = code_labels
      )
    })
    ## Observe Analyze screen ----
    # Listen to message from Analyze screen
    observeEvent(glob$analyze_link, {
      if (glob$doc_selector != glob$analyze_link$doc_id) {
        glob$doc_selector <- glob$analyze_link$doc_id
        updateSelectInput(
          session = session,
          "doc_selector",
          choices = c("", glob$documents),
          selected = glob$doc_selector
        )
        #loc$codes_menu_observer  <- loc$codes_menu_observer + 1
        loc$text_observer <- loc$text_observer + 1
      }
    })

    ## Observe memo show ----
    observeEvent(glob$memo_show, {
      if (glob$memo_show) {
        shinyjs::show(selector = ".text_memo_btn, .text_memo_edit.show_edit")
      } else {
        shinyjs::hide(selector = ".text_memo_btn, .text_memo_edit.show_edit")
      }
    })

    ## Observe memo tab ----
    observeEvent(req(glob$selected_documentcode_tabset), {
      updateTabsetPanel(
        inputId = "documentcode_tabset",
        selected = "memotools_tab"
      )
      glob$selected_documentcode_tabset <- NULL
    })

    ## Observe memo segments ----
    observeEvent(glob$memo_segment_observer, {
      req(glob$memo_segment_observer > 0)
      loc$display_observer <- loc$display_observer + 1
    })

    # Render codes ----
    output$code_list <- renderUI({
      req(isTruthy(loc$codes_menu))
      loc$codes_menu
    })

    # Initialize new text skelet $text_observer ----
    observeEvent(req(loc$text_observer), {
      #browser()
      req(loc$text_observer > 0) # ignore init value
      progress <- shiny::Progress$new()
      progress$set(message = 'Loading document text.')
      golem::invoke_js('clearArticle', list())
      # define text values that may be useful outside of this observer
      loc$raw_text <- load_doc_db(
        glob$pool,
        glob$active_project,
        glob$doc_selector
      )
      loc$total_chars <- nchar(loc$raw_text)
      paragraph_indices <- tibble::as_tibble(stringr::str_locate_all(
        loc$raw_text,
        "\n|\r"
      )[[1]])
      if (!nrow(paragraph_indices)) {
        paragraph_indices <- tibble::tibble(end = loc$total_chars)
      }
      loc$paragraphs <- paragraph_indices %>%
        dplyr::transmute(
          segment_start = as.integer(dplyr::lag(end + 1, default = 1)),
          segment_end = as.integer(dplyr::lead(
            segment_start - 1,
            default = max(end)
          ))
        ) %>%
        tibble::rowid_to_column("par_id") %>%
        dplyr::mutate(par_id = paste("par_id", par_id, sep = "-"))

      if (loc$total_chars > max(loc$paragraphs$segment_end)) {
        # safeguard against text without final linebreak
        loc$paragraphs <- dplyr::bind_rows(
          loc$paragraphs,
          tibble::tibble(
            par_id = paste0("par_id-", length(loc$paragraphs$par_id) + 1),
            segment_start = as.integer(max(loc$paragraphs$segment_end) + 1),
            segment_end = as.integer(loc$total_chars)
          )
        )
      }

      text_plain <- purrr::pmap_chr(
        loc$paragraphs,
        .f = function(par_id, segment_start, segment_end) {
          paste0(
            '<div class= "docpar_wrap"><div class="partext"><p id ="',
            par_id,
            '" class = "docpar" data-startend="',
            segment_start,
            ' ',
            segment_end,
            '">',
            gsub(
              "\n",
              "",
              htmltools::htmlEscape(substr(
                loc$raw_text,
                segment_start,
                segment_end
              ))
            ),
            '<span class = "br">&#8203</span></p></div><div p id = "info_',
            par_id,
            '" class="extra_info"></div></div>'
          )
        }
      )
      golem::invoke_js(
        "appendContent",
        list(id = "article", html = paste(text_plain, collapse = ""))
      )
      glob$startOff <- 1
      glob$endOff <- loc$total_chars

      progress$set(message = 'Loading document segments.')
      edit_display_LF(reduce = TRUE)
      progress$set(message = 'Loading document memos.')
      edit_memos_LF()
      golem::invoke_js("setArticleStatusValue", list(status = "loaded"))
      progress$close()
    })

    # Display/edit text $display_observer -----
    observeEvent(loc$display_observer, {
      req(loc$display_observer > 0)
      edit_display_LF()
    })
    observeEvent(c(input$doc_status, req(glob$analyze_link$segment_id)), {
      if (input$doc_status == "loaded") {
        golem::invoke_js(
          'scrollToSegment',
          list(target_id = glob$analyze_link$segment_id)
        )
        glob$analyze_link$segment_id <- NULL
      }
    })
    # Coding tools ------------------------------------------------------------
    observeEvent(req(input$selected_code), {
      # We need a document and selection positions
      req(glob$doc_selector)
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

      if (glob$endOff >= glob$startOff) {
        write_segment_db(
          glob$pool,
          glob$active_project,
          user_id = glob$user$user_id,
          doc_id = glob$doc_selector,
          code_id = loc$code,
          glob$startOff,
          glob$endOff
        )

        loc$display_observer <- loc$display_observer + 1
        glob$segments_observer <- glob$segments_observer + 1
      }
    })

    ## Quick code tools ----
    observeEvent(input$quickcode, {
      if (isTruthy(input$quickcode)) {
        removeUI("#code_extra_div")
        # Backlight reset
        loc$backlight_code_id <- NULL
        non_matched_codes <- loc$codebook %>%
          dplyr::filter(
            !stringr::str_detect(code_name, paste0("(?i)", input$quickcode))
          ) %>%
          dplyr::pull(code_id)
        purrr::map(non_matched_codes, shinyjs::hide)
        matched_codes <- loc$codebook$code_id[
          !loc$codebook$code_id %in% non_matched_codes
        ]
        purrr::map(matched_codes, shinyjs::show)
      } else {
        purrr::map(loc$codebook$code_id, shinyjs::show)
      }
    })
    # Clear quick code form
    observeEvent(input$clear_iframe, {
      golem::invoke_js('refreshIframe', message = list())
      # Refresh codes menu
      purrr::map(loc$codebook$code_id, shinyjs::show)
    })
    # After quickcode button is pressed
    observeEvent(req(input$quickcode_btn), {
      req(glob$doc_selector)
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
        # Notify codebook screen about new quick code
        glob$codebook_observer <- ifelse(
          !isTruthy(glob$codebook_observer),
          1,
          (glob$codebook_observer + 1)
        )
        # Notify user
        rql_message(paste(input$quickcode, "added to codebook."))
        golem::invoke_js('refreshIframe', message = list())
      }
    })

    ## Codes extras -----
    # Observe changes to input$selected_code_extra and update the reactive value
    observeEvent(input$selected_code_extra, {
      loc$selected_code_extra <- input$selected_code_extra
    })
    # When selected_code_extra changes generate code extra UI
    observeEvent(req(loc$code_extra_sel), {
      req(glob$doc_selector)
      removeUI("#code_extra_div")
      generate_code_extra_LF()
      loc$code_extra_sel <- NULL
    })
    observeEvent(c(loc$selected_code_extra, glob$segments_observer), {
      req(loc$selected_code_extra)
      loc$segments_count <- dplyr::tbl(glob$pool, "segments") %>%
        dplyr::filter(project_id == local(as.integer(glob$active_project))) %>%
        dplyr::filter(code_id == local(as.integer(loc$selected_code_extra))) %>%
        dplyr::collect() %>%
        dplyr::summarise(
          document_freq = sum(doc_id == local(as.integer(glob$doc_selector))),
          total_freq = dplyr::n()
        )
      loc$code_extra_sel <- paste0(
        "#",
        ns(paste0("more-", loc$selected_code_extra))
      )
      shinyjs::html(
        "code_extra_freq",
        loc$segments_count$document_freq,
        asis = TRUE
      )
      shinyjs::html(
        "code_extra_total_freq",
        loc$segments_count$total_freq,
        asis = TRUE
      )
    })
    observeEvent(req(input$close_code_extra_div), {
      removeUI("#code_extra_div")
      loc$backlight_code_id <- NULL
      loc$selected_code_extra <- NULL
      loc$code_extra_sel <- NULL
    })

    # Segment removal ----------
    observeEvent(input$remove_codes, {
      req(glob$active_project)
      req(glob$doc_selector)

      if (glob$user$data$annotation_other_modify == 0) {
        loc$marked_segments_df <- load_segment_codes_db(
          glob$pool,
          glob$active_project,
          user_id = glob$user$user_id,
          active_doc = glob$doc_selector,
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
          active_doc = glob$doc_selector,
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
          doc_id = glob$doc_selector,
          segment_id = loc$marked_segments_df$segment_id
        )
        # Refresh text
        glob$startOff <- min(loc$marked_segments_df$segment_start)
        glob$endOff <- max(loc$marked_segments_df$segment_end)
        loc$display_observer <- loc$display_observer + 1
        # Notify analysis screen
        glob$segments_observer <- glob$segments_observer + 1
      } else {
        # Obtain additional input if multiple segments are to be removed
        showModal(
          modalDialog(
            checkboxGroupInput(
              ns("codes_to_remove"),
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
              actionButton(
                ns("remove_codes_select"),
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
          doc_id = glob$doc_selector,
          segment_id = input$codes_to_remove
        )
        removeModal()

        # Refresh text

        glob$startOff <- min(loc$marked_segments_df$segment_start)
        glob$endOff <- max(loc$marked_segments_df$segment_end)
        loc$display_observer <- loc$display_observer + 1
        # Notify analysis screen
        glob$segments_observer <- glob$segments_observer + 1
      }
    })

    # Helper: position counter ---------------
    output$captured_range <- renderText({
      req(isTruthy(input$tag_position))
      if (glob$startOff == glob$endOff) {
        reported_range <- glob$startOff
      } else if (glob$startOff > glob$endOff) {
        reported_range <- ""
      } else {
        reported_range <- input$tag_position
      }
      as.character(reported_range)
    })

    # Backlight ------
    observeEvent(
      c(
        input$code_extra_backlight,
        glob$segments_observer
      ),
      {
        if (button_is_on(req(input$code_extra_backlight))) {
          if (!isTruthy(loc$backlight_code_id)) {
            loc$backlight_observer <- 1
          } else {
            loc$backlight_observer <- loc$backlight_observer + 1
          }
          loc$backlight_code_id <- paste0("code_id_", input$js_backlight_value)
        } else {
          loc$backlight_code_id <- NULL
        }
      }
    )

    observeEvent(c(loc$backlight_observer, loc$backlight_code_id), {
      if (isTruthy(loc$backlight_code_id)) {
        # Add frontlight class to all docpar elements
        shinyjs::addClass(
          class = "code_extra_frontlight",
          selector = ".docpar"
        )

        # Add frontlight class to all code elements except the targeted one
        shinyjs::addClass(
          class = "code_extra_frontlight",
          selector = paste0(".code:not(.", loc$backlight_code_id, ")")
        )

        # Remove backlight class from all non-targeted code elements
        shinyjs::removeClass(
          class = "code_extra_backlight",
          selector = paste0(".code:not(.", loc$backlight_code_id, ")")
        )

        # Add backlight class to the targeted code element
        shinyjs::addClass(
          class = "code_extra_backlight",
          selector = paste0(".", loc$backlight_code_id)
        )
      } else {
        # Remove backlight class from all code elements
        shinyjs::removeClass(
          class = "code_extra_backlight",
          selector = ".code"
        )
        # Remove frontlight class from all code elements
        shinyjs::removeClass(
          class = "code_extra_frontlight",
          selector = ".code"
        )
        # Remove frontlight class from all docpar elements
        shinyjs::removeClass(
          class = "code_extra_frontlight",
          selector = ".docpar"
        )
      }
    })

    # Local functions -------------------
    ## toggle_style_LF  -------------------

    toggle_style_LF <- function() {
      values <- c("background", "underline", "none")
      # Find the current index of the highlight style
      current_style_index <- match(loc$highlight, values)
      new_style_index <- (current_style_index %% length(values)) + 1
      # Update loc$highlight with the new style
      loc$highlight <- values[new_style_index]
      # Send a message to the client to toggle the style
      #session$sendCustomMessage("toggleStyle", message = loc$highlight)
      if (loc$highlight == "background") {
        shinyjs::addClass(class = "background", selector = ".segment.code")
        rql_message("Segment style: Highlight")
      } else if (loc$highlight == "underline") {
        shinyjs::removeClass(class = "background", selector = ".segment.code")
        shinyjs::addClass(class = "underline", selector = ".segment.code")
        # shinyjs::addClass(class = "memo_highlight", selector = ".segment.memo")
        rql_message("Segment style: Underline")
      } else {
        shinyjs::removeClass(class = "underline", selector = ".segment.code")
        # shinyjs::removeClass(class = "memo_highlight", selector = ".segment.memo")
        rql_message("Segment style: None")
      }
    }

    ## edit_display_LF  -------------------
    edit_display_LF <- function(reduce = FALSE) {
      loc$par_index <- loc$paragraphs %>%
        dplyr::filter(
          segment_start <= glob$endOff,
          segment_end >= glob$startOff
        )
      loc$text_data <- load_doc_to_display(
        pool = glob$pool,
        active_project = glob$active_project,
        user = glob$user,
        doc_selector = glob$doc_selector,
        raw_text = loc$raw_text,
        paragraphs = loc$par_index,
        codebook = glob$codebook,
        highlight = loc$highlight
      )
      if (reduce) {
        loc$text_data <- loc$text_data %>%
          dplyr::filter(any(!is.na(segment_id)), .by = "par_id")
      }
      loc$par_ids <- unique(loc$text_data$par_id)

      purrr::walk(loc$par_ids, .f = function(x_par) {
        par <- loc$text_data %>%
          dplyr::filter(par_id == x_par) %>%
          dplyr::select(-par_id)
        spans <- purrr::pmap(
          par,
          make_span,
          raw_text = loc$raw_text,
          highlight = loc$highlight
        )
        new_text <- purrr::map(spans, as.character) %>%
          paste0(collapse = "") %>%
          paste0('<span class = "br">&#8203</span>')
        session$sendCustomMessage(
          "updateParagraphContent",
          list(id = x_par, data = new_text)
        )
      })
    }

    ## edit_memos_LF  -------------------
    edit_memos_LF <- function() {
      loc$text_memos <- load_memos_to_display(
        pool = glob$pool,
        active_project = glob$active_project,
        user = glob$user,
        doc_selector = glob$doc_selector
      )
      if (!is.null(loc$text_memos)) {
        text_memos <- loc$text_memos %>%
          dplyr::mutate(memo_id = paste0("memo_id_", memo_id))

        memos_data <- loc$text_data %>%
          dplyr::select(par_id, memo_id) %>%
          dplyr::filter(par_id %in% loc$par_ids) %>%
          dplyr::mutate(par_id = paste0("info_", par_id)) %>%
          dplyr::filter(!is.na(memo_id)) %>%
          dplyr::mutate(memo_id = strsplit(memo_id, " ")) %>%
          tidyr::unnest(cols = c("memo_id")) %>%
          dplyr::filter(memo_id != "memo") %>%
          dplyr::distinct(memo_id, .keep_all = TRUE) %>%
          dplyr::inner_join(
            text_memos %>%
              dplyr::select(memo_id, text),
            by = "memo_id"
          ) %>%
          dplyr::arrange(memo_id)

        if (nrow(memos_data) > 0) {
          purrr::pmap(memos_data, function(par_id, memo_id, text) {
            memo_html <- span(
              id = memo_id,
              icon(
                "sticky-note",
                class = "fas text_memo_btn",
                `data-memo` = text,
                .noWS = c("outside", "after-begin", "before-end")
              ),
              .noWS = c("outside", "after-begin", "before-end")
            )
            golem::invoke_js(
              "updateElementContent",
              list(id = par_id, content = "")
            )
            insertUI(paste0("#", par_id), where = "afterBegin", ui = memo_html)
          })
        }
      }
    }

    ## generate_code_extra_LF  -------------------
    generate_code_extra_LF <- function() {
      selected_code_extra <- as.integer(loc$selected_code_extra)

      code_info <- glob$codebook %>%
        dplyr::filter(
          code_id == selected_code_extra
        )

      insertUI(
        selector = loc$code_extra_sel,
        where = "afterEnd",
        ui = tags$div(
          id = "code_extra_div",
          style = "position: relative; border: 1px solid #ccc; background: white; text-align: left; padding-left: 5px; margin-bottom: 5px;",
          div(
            style = "position: absolute; top: 0px; right: 0px;",
            actionButton(
              ns("close_code_extra_div"),
              "",
              icon = icon("x"),
              style = "background: transparent; border: none; color: lightgray;"
            )
          ),
          "Code:",
          tags$b(code_info$code_name),
          br(),
          "Document frequency:",
          tags$b(tags$span(
            id = "code_extra_freq",
            loc$segments_count$document_freq
          )),
          br(),
          "Total frequency:",
          tags$b(tags$span(
            id = "code_extra_total_freq",
            loc$segments_count$total_freq
          )),
          br(),
          actionButton(
            ns("code_extra_backlight"),
            label = NULL,
            icon = icon("glasses"),
            `data-code_id` = selected_code_extra,
            onclick = paste0(
              "Shiny.setInputValue('",
              ns("js_backlight_value"),
              "', this.getAttribute('data-code_id'), {priority: 'event'});"
            )
          ) %>%
            tagAppendAttributes(class = "code_extra_btn")
        )
      )
    }

    # returns glob$segments_observer and glob$codebook
  })
}
