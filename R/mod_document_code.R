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
      tags$script(src = "www/highlight_tool.js"),
      tags$script(HTML("
  document.addEventListener('DOMContentLoaded', (event) => {
    Split(['#split-1', '#split-2'], {
      sizes: [80, 20],
      minSize: [100, 100]
    });
  });
")),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('appendContent', function(message) {
      var article = document.getElementById(message.id);
      article.insertAdjacentHTML('beforeend', message.html);
    });
  ")),
    tags$script(HTML("
    Shiny.addCustomMessageHandler('clearArticle', function(message) {
      $('#article').empty();
    });
  "))
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
                selectInput(ns("doc_selector"), label = "Select a document to code", choices = "", selected = "")  %>% 
                tagAppendAttributes(class = "full-width")
              ),
              div(
                style = "display: flex; justify-content: flex-end; align-items: center;",
                actionButton(ns("doc_refresh"), label = "", icon = icon("sync"), title = "Reload document", style = "margin-bottom: -10px"),
                div(
                  id = ns("selection_displayer"),
                  style = "margin-left: 20px; margin-right: 5px; min-width: 75px;", 
                  "Selection:", br(), textOutput(ns("captured_range"))
                )
              )
            ),
          fluidRow(
                 div(id = ns("focal_text"), tags$article(id = "article", class = "scrollable80", .noWS = "outside"), .noWS = "outside")
          )
        ),
        tags$div(
          id = "split-2",
          style = "flex-grow: 1; flex-shrink: 1; overflow: auto;",
          tabsetPanel(
            type = "tabs",
            id = ns("documentcode_tabset"),
            # Codes panel ----
            tabPanel("Codes",
              id = ns("codetools_tabset"),
              value = "codetools_tabset",
                br(),
                mod_rql_hidden_ui_ui(ns("rql_hidden_ui_1"), title = "Toggle coding toolbox", hidden_tags = tagList(
                    div(
                          style = "display: flex; justify-content: flex-end; align-items: center;",
                          actionButton(
                                    ns("code_columns"),
                                    label = "",
                                    icon = icon("table-columns"),
                                    title = "Code columns"
                                  ),
                          actionButton(ns("toggle_style"), label = "", icon = icon("highlighter"), title = "Highlight style"),
                        ),
                      div(
                      style = "height: calc(1.5em + .75rem + 10px); display: flex; align-items: center; margin-top: 0.5em; margin-bottom: 0.5em;",
                      actionButton(ns("clear_iframe"), "", icon = icon("x"), title = "Clear form",
                          style = "margin-right: 0px; padding-right: 0px; padding-left: 5px; position: absolute; font-size: x-small; color: gray; border: none; background: transparent;"),
                      tags$div(
                        style = "flex-grow: 1; height: calc(1.5em + .75rem + 10px);",
                        tags$iframe(
                          src = "www/quickcode.html",
                          style = "height: calc(1.5em + .75rem + 10px); width: 100%; border: none;"
                        )
                      ),
                      actionButton(ns("quickcode_btn"), "Quick tag", icon = icon("bolt-lightning"), title = "Apply new code to selection",
                        style = "height: calc(1.5em + .75rem + 10px); margin-left: 0px;")
                    )
                )),
                actionButton(
                  ns("remove_codes"),
                  "Remove code",
                  class = "btn-danger",
                  width = "100%"
                ),
                uiOutput(ns("code_list"))  %>% tagAppendAttributes(class = "scrollable80")
              ),
              # Memos panel ----
              tabPanel("Memos",
              id = ns("memotools_tabset"),
              value = "memotools_tabset",
                br(),
                actionButton(
                  ns("add_segment_memo"),
                  "Add segment memo",
                  width = "100%"
                ),
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
    loc$highlight <- "background"
    loc$code <- NULL
    observeEvent(req(glob$active_project), {
    loc$codes_menu_observer <- 0
    loc$code_action_observer <- 0
    loc$text_observer <- 0
    golem::invoke_js('clearArticle', list())
    golem::invoke_js('refreshIframe', message = list())
    })
    mod_rql_hidden_ui_server("rql_hidden_ui_1")
    # Observers - definitions ----
    ## Observe click on coded text ----
    observeEvent(input$clicked_title, {
      showNotification(input$clicked_title)
    })
    ## Observe choice of highlight style ----
    observeEvent(input$toggle_style, {
      toggle_style_LF()
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
          ))
        
      loc$codes_menu <- sortable::rank_list(
        input_id = "codes_menu",
        css_id = "codes_menu",
        labels = code_labels
      ) 
    })
  ## Observe Analyze screen ----
  # Listen to message from Analyze screen
  observeEvent(glob$analyze_link, {
    if (input$doc_selector != glob$analyze_link$doc_id) {
      updateSelectInput(session = session, "doc_selector", choices = c("", glob$documents), selected = glob$analyze_link$doc_id)
      #loc$codes_menu_observer  <- loc$codes_menu_observer + 1
      loc$text_observer <- loc$text_observer + 1
    }
  golem::invoke_js('scrollToSegment', list(target_start = glob$analyze_link$segment_start))
  })

    # Render codes ----
    output$code_list <- renderUI({
      req(isTruthy(loc$codes_menu))
      loc$codes_menu
    })

    # Displayed text observer ----
    observeEvent(req(loc$text_observer), {
      #browser()
      req(loc$text_observer > 0) # ignore init value
      golem::invoke_js('clearArticle', list())
      # define text values that may be useful outside of this observer
      loc$raw_text <- load_doc_db(glob$pool, glob$active_project,  ifelse(isTruthy(input$doc_selector), input$doc_selector, glob$analyze_link$doc_id))
      loc$total_chars <- nchar(loc$raw_text)
      paragraph_indices <- tibble::as_tibble(stringr::str_locate_all(loc$raw_text, "\n|\r")[[1]])
      if (!nrow(paragraph_indices)) paragraph_indices  <- tibble::tibble(end = loc$total_chars)
      loc$paragraphs <- paragraph_indices %>%
          dplyr::transmute(
          segment_start = as.integer(dplyr::lag(end+1, default = 1)),
          segment_end = as.integer(dplyr::lead(segment_start-1, default = max(end)))
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
      

        text_plain <- purrr::pmap_chr(loc$paragraphs, .f = function(par_id, segment_start, segment_end) {
         
         paste0(
            '<p id ="',
            par_id,
            '" class = "docpar" data-startend',
            segment_start,
            '-',
            segment_end,
            '>',
            gsub("\n", "", htmltools::htmlEscape(substr(loc$raw_text, segment_start, segment_end))),
            '<span class = "br">&#8203</span></p>')
        })

        

          golem::invoke_js("appendContent", list(id = "article", html = paste(text_plain, collapse = "")))

      text_data <- load_doc_to_display(
          glob$pool,
          glob$active_project,
          user = glob$user,
          doc_selector = ifelse(isTruthy(input$doc_selector), input$doc_selector, glob$analyze_link$doc_id),
          raw_text = loc$raw_text,
          paragraphs = loc$paragraphs,
          loc$codebook,
          highlight = loc$highlight,
          ns = NS(id)
        ) |> 
        dplyr::filter(!all(is.na(code_id)), .by = par_id)

        par_ids <- unique(text_data$par_id)

                purrr::walk(par_ids, .f = function(x_par) {
         
           par <- text_data |> dplyr::filter(par_id == x_par)  |> dplyr::select(-span_id, -par_id)
           spans <- purrr::pmap(par, make_span, raw_text = loc$raw_text, highlight = loc$highlight) 
            new_text <- purrr::map_chr(spans, as.character) |> paste0(collapse = "") |> paste0('<span class = "br">&#8203</span></p>')
            session$sendCustomMessage("updateParagraphContent", list(id = x_par, data = new_text))
                })

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
        
        
        loc$par_coded <- loc$paragraphs |> 
        dplyr::filter(segment_start < endOff,  segment_end > startOff) 

        edit_paragraphs_LF()

        glob$segments_observer <- glob$segments_observer + 1
      }
    })
    
    ## Quick code tools ----
    observeEvent(input$quickcode, {
        if (isTruthy(input$quickcode)) {
        non_matched_codes <- loc$codebook |> 
        dplyr::filter(!stringr::str_detect(code_name, paste0("(?i)", input$quickcode))) |> 
        dplyr::pull(code_id)
        purrr::map(non_matched_codes, shinyjs::hide)
        matched_codes <- loc$codebook$code_id[!loc$codebook$code_id %in% non_matched_codes]
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
      req(input$doc_selector)
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
        glob$codebook_observer  <- ifelse(!isTruthy(glob$codebook_observer), 1, (glob$codebook_observer + 1))
        # Notify user
        rql_message(paste(input$quickcode,"added to codebook."))
        golem::invoke_js('refreshIframe', message = list())
      } 
    })

    ## Codes extras -----
    observeEvent(req(input$selected_code_extra), {
      req(input$doc_selector)
      removeUI("#code_extra_div")
      sel <- paste0("#", ns(paste0("more-", input$selected_code_extra)))
      generate_code_extra_LF(sel)
    })
    observeEvent(req(input$close_code_extra_div), {
      removeUI("#code_extra_div")
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
        
       loc$par_coded <- loc$paragraphs |> 
        dplyr::filter(segment_start < max(loc$marked_segments_df$segment_end),  segment_end > min(loc$marked_segments_df$segment_start))

        edit_paragraphs_LF()
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
        
       loc$par_coded <- loc$paragraphs |> 
        dplyr::filter(segment_start < max(loc$marked_segments_df$segment_end),  segment_end > min(loc$marked_segments_df$segment_start))

        edit_paragraphs_LF()
        # Notify analysis screen
        glob$segments_observer <- glob$segments_observer + 1
      }
    })
    
    # Memo tools ----
    observeEvent(input$add_segment_memo, {
      #browser()
      startOff <- parse_tag_pos(req(input$tag_position), "start")
      endOff <- parse_tag_pos(req(input$tag_position), "end")

      if (endOff >= startOff) {
       new_segment_id <- write_memo_segment_db(
          pool = glob$pool,
          active_project = glob$active_project,
          user_id = glob$user$user_id,
          doc_id = input$doc_selector,
          code_id = NA,
          startOff,
          endOff
        )
      new_memo_id  <- add_memo_record(
        pool = glob$pool,
        project = glob$active_project,
        text = "TODO grab memo text",
        user_id = glob$user$user_id
      )
      new_memo_segment_map <- data.frame(memo_id = new_memo_id, segment_id = new_segment_id)
      #add_memo_segment_map(...)
      DBI::dbWriteTable(glob$pool, "memos_segments_map", new_memo_segment_map, append = TRUE, row.names = FALSE)
      loc$par_memoed <- loc$paragraphs |> 
        dplyr::filter(segment_start < endOff,  segment_end > startOff) 
      edit_memos_LF()

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

    # Local functions -------------------
    ## toggle_style_LF  -------------------

    toggle_style_LF <- function() {
      values <- c("background", "underline", "none")
      # Find the current index of the highlight style
      current_style_index <- match(loc$highlight, values)
      new_style_index <- (current_style_index %% length(values)) + 1
      # Update loc$highlight with the new style
      loc$highlight  <- values[new_style_index]
        # Send a message to the client to toggle the style
      #session$sendCustomMessage("toggleStyle", message = loc$highlight)
      if (loc$highlight == "background") {
        shinyjs::addClass(class = "background", selector = ".segment-code")
        rql_message("Segment style: Highlight")
      } else if (loc$highlight == "underline") {
         shinyjs::removeClass(class = "background", selector = ".segment-code")
         shinyjs::addClass(class = "underline", selector = ".segment-code")
         rql_message("Segment style: Underline")
      } else {
        shinyjs::removeClass(class = "underline", selector = ".segment-code")
        rql_message("Segment style: None")
      }
    }

    ## edit_paragraphs_LF  -------------------
    edit_paragraphs_LF <- function() {
                local_text_data <- load_doc_to_display(
          glob$pool,
          glob$active_project,
          user = glob$user,
          doc_selector = input$doc_selector,
          raw_text = loc$raw_text,
          paragraphs = loc$par_coded,
          loc$codebook,
          highlight = loc$highlight,
          ns = NS(id)
        ) |> 
        dplyr::filter(segment_start >= min(loc$par_coded$segment_start), segment_end <= max(loc$par_coded$segment_end))

         par_ids_local <- unique(local_text_data$par_id)

                purrr::walk(par_ids_local, .f = function(x) {
         
           par_local <- local_text_data |> dplyr::filter(par_id == x)  |> dplyr::select(-span_id, -par_id)
           spans <- purrr::pmap(par_local, make_span, raw_text = loc$raw_text, highlight = loc$highlight) 
            new_text <- purrr::map_chr(spans, as.character) |> paste0(collapse = "") |> paste0('<span class = "br">&#8203</span></p>')
            session$sendCustomMessage("updateParagraphContent", list(id = x, data = new_text))
                })
    }


    ## edit_memos_LF  -------------------
    edit_memos_LF <- function() {
                local_text_data <- load_doc_to_display(
          pool = glob$pool,
          active_project = glob$active_project,
          user = glob$user,
          doc_selector = input$doc_selector,
          raw_text = loc$raw_text,
          paragraphs = loc$par_memoed,
          codebook = glob$codebook,
          highlight = loc$highlight,
          ns = NS(id)
        ) |> 
        dplyr::filter(segment_start >= min(loc$par_memoed$segment_start), segment_end <= max(loc$par_memoed$segment_end))
         

         par_ids_local <- unique(local_text_data$par_id)

                purrr::walk(par_ids_local, .f = function(x) {
         
           par_local <- local_text_data |> dplyr::filter(par_id == x)  |> dplyr::select(-span_id, -par_id)
           spans <- purrr::pmap(par_local, make_span, raw_text = loc$raw_text, highlight = loc$highlight) 
            new_text <- purrr::map_chr(spans, as.character) |> paste0(collapse = "") |> paste0('<span class = "br">&#8203</span></p>')
            session$sendCustomMessage("updateParagraphContent", list(id = x, data = new_text))
                })
    }

    ## generate_code_extra_LF  -------------------
    generate_code_extra_LF <- function(sel) {
      doc_group <- NULL
      selected_code_extra <- as.integer(input$selected_code_extra)
      active_project <- as.integer(glob$active_project)
      active_doc <- as.integer(input$doc_selector)
      code_info <- glob$codebook  |> dplyr::filter(
        code_id == selected_code_extra
      )
      segments_info <- dplyr::tbl(pool, "segments") %>%
            dplyr::filter(project_id == active_project) %>%
            dplyr::filter(code_id == selected_code_extra) %>% 
            dplyr::mutate(doc_group = ifelse(doc_id == active_doc, "this_document", "other_documents")) %>%
            dplyr::count(doc_group) %>%
            dplyr::collect()
       insertUI(
         selector = sel,
         where = "afterEnd",
         ui =   tags$div(id = "code_extra_div",
      style = "position: relative; border: 1px solid #ccc; background: white; text-align: left; padding-left: 5px; margin-bottom: 5px;",
      div(
        style = "position: absolute; top: 0px; right: 0px;",
        actionButton(ns("close_code_extra_div"), "", icon = icon("x"), style = "background: transparent; border: none; color: lightgray;")
      ),
      "Code:", tags$b(code_info$code_name), br(),
      "Document:", tags$b(segments_info$n[1]), br(),
      "Total:", tags$b(sum(segments_info$n)), br()
  )
        )
    }

    # returns glob$segments_observer and glob$codebook
  })
}
