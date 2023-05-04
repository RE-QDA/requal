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
  fluidRow(
    tags$head(
      tags$script(
        src = "www/document_code_js.js"
      ),
    ),
    column(
      width = 10,
      selectInput(ns("doc_selector"),
        label = "Select a document to code",
        choices = "", selected = ""
      ),
      htmlOutput(ns("focal_text")) %>% tagAppendAttributes(class = "scrollable80"),
      textOutput(ns("captured_range")),
      verbatimTextOutput(ns("printLabel"))
    ),
    column(
      width = 2,
      tags$b("Codes"),
      br(),
      actionButton(ns("remove_codes"),
        "Remove code",
        class = "btn-danger",
        width = "100%"
      ),
      br(), br(),
      uiOutput(ns("code_list"))
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
    
    # Selection of documents to code ------------------------------------------
    loc$doc_choices <- NULL

    # Refresh list of documents when documents are added/removed --------
    observeEvent(glob$documents, {
      if (isTruthy(glob$active_project)) {
        if(glob$user$data$data_other_view == 1){
          updateSelectInput(
            session = session,
            "doc_selector",
            choices = c("", glob$documents)
          )  
        }else{
          visible_docs <- read_visible_docs(glob$pool, glob$active_project, 
                                            glob$user$user_id)
          updateSelectInput(
            session = session, 
            "doc_selector", 
            choices = c("", visible_docs)
          )
        }
        
      }
    })

    # Displayed text ----------------------------------------------------------
    loc$text <- ""

    observeEvent(input$doc_selector, {
      if (isTruthy(input$doc_selector)) {
          loc$text <- load_doc_to_display(
            glob$pool, 
            glob$active_project,
            user = glob$user,
            input$doc_selector,
            loc$code_df$active_codebook,
            ns = NS(id)
        )
        glob$segments <- glob$segments + 1
      }
    })
    # Render selected text
    output$focal_text <- renderText({
        loc$text
    })



    # List out available codes ------------------------------------------------

    output$code_list <- renderUI({
      if (isTruthy(glob$active_project)) {
        if (isTruthy(glob$codebook)) {
          loc$code_df$active_codebook <- glob$codebook
        } else {
          loc$code_df$active_codebook <- list_db_codes(
              glob$pool, 
              glob$active_project
          )
        }

        if (isTruthy(req(input$doc_selector))) {
          loc$text <- load_doc_to_display(
              glob$pool, 
              glob$active_project,
              user = glob$user,
              input$doc_selector,
              loc$code_df$active_codebook,
              ns = NS(id)
          )
        }

        purrr::pmap(
          list(
            loc$code_df$active_codebook$code_id,
            loc$code_df$active_codebook$code_name,
            loc$code_df$active_codebook$code_color,
            loc$code_df$active_codebook$code_description
          ),
          ~ generate_coding_tools(
            ns = ns,
            code_id = ..1,
            code_name = ..2,
            code_color = ..3
            code_desc = ..4)
          )
        )
      } else {
        ""
      }
    })

    # Coding tools ------------------------------------------------------------
    observeEvent(input$selected_code, {
      req(input$selected_code, input$tag_position)

      startOff <- parse_tag_pos(input$tag_position, "start")
      endOff <- parse_tag_pos(input$tag_position, "end")

      if (endOff - startOff > 1 & endOff > startOff) {
        write_segment_db(
            glob$pool, 
            glob$active_project,
            user_id = glob$user$user_id,
            doc_id = input$doc_selector,
            code_id = input$selected_code,
            startOff,
            endOff
        )

        loc$text <- load_doc_to_display(
            glob$pool, 
            glob$active_project,
            user = glob$user,
            input$doc_selector,
            loc$code_df$active_codebook,
            ns = NS(id)
        )
        glob$segments <- glob$segments + 1
      }
    })

    # Segment removal ----------
    observeEvent(input$remove_codes, {
      req(glob$active_project)
      
      if(glob$user$data$annotation_other_modify == 0){
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
      }else{
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

        loc$text <- load_doc_to_display(
            glob$pool, 
            glob$active_project,
            user = glob$user,
            input$doc_selector,
            loc$code_df$active_codebook,
            ns = NS(id)
        )
        glob$segments <- glob$segments + 1
        
      } else {
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

        loc$text <- load_doc_to_display(
            glob$pool, 
            glob$active_project,
            user = glob$user,
            input$doc_selector,
            loc$code_df$active_codebook,
            ns = NS(id)
        )
        glob$segments <- glob$segments + 1
      }
    })

    #  # Helper (to be commented out in prod): position counter ---------------

    output$captured_range <- renderText({
      input$tag_position
    })

    # returns glob$segments
  })
}
