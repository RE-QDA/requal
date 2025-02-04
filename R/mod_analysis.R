#' analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      class = "module_tools",
      div(mod_rql_button_ui(ns("filter_ui"),
        label = "Filter segments",
        icon = "filter"
      )) %>% tagAppendAttributes(style = "padding-right: 25px;"),
      mod_rql_button_ui(ns("download_ui"),
        label = "Download segments",
        icon = "download"
      )
      ),
    fluidRow(
      class = "module_content",
        uiOutput(ns("segments")) %>%
          tagAppendAttributes(class = "scrollable90")
      )
    # ###########
    #   column(
    #     width = 4,
    
    #   )
    #   uiOutput(ns("download"))
    # )
  )
}

#' analysis Server Functions
#'
#' @noRd
mod_analysis_server <- function(id, glob) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    loc <- reactiveValues()

   if (golem::get_golem_options(which = "mode") == "server") {
    observeEvent(input$user_filter, {
      loc$user_filter <- input$user_filter
    })
   } else {
    # local version does not have user filter UI
    # so we set the user filter to 1 
    # to always select the user in desktop version
    loc$user_filter <- 1  # for local version
   }

   # UI ----

    #--- Filter UI --------------
      mod_rql_button_server(
        id = "filter_ui",
        custom_title = "Filter segments",
        custom_tagList = tagList(
          rql_picker_UI(ns("code_filter"),
          label = "Filter by code",
          choices = "",
          multiple = TRUE,
          none = "Off"
          ),
        rql_picker_UI(ns("category_filter"),
          label = "Filter by category",
          choices = "",
          multiple = TRUE,
          none = "Off"
          ),
        rql_picker_UI(ns("document_filter"),
          label = "Filter by document",
          choices = "",
          multiple = TRUE,
          none = "Off"
        ),
        if (golem::get_golem_options(which = "mode") == "server") {
        rql_picker_UI(ns("user_filter"),
          label = "Filter by user",
          choices = "",
          multiple = TRUE,
          none = "Off"
        )
        }
        ),
        glob
      )
      # Download UI ----
mod_rql_button_server(
        id = "download_ui",
        custom_title = "Download segments",
        custom_tagList = tagList(
          tags$p(mod_download_csv_ui("download_csv_ui_1", "download_analysis")),
          tags$p(mod_download_html_ui("download_html_ui_1"))
        )

      )
            
    # Filters ----

    observeEvent(glob$codebook, {
      shinyWidgets::updatePickerInput(
        session = session,
        "code_filter",
        choices = stats::setNames(
          glob$codebook$code_id,
          glob$codebook$code_name
        ),
        selected = glob$codebook$code_id
      )
    })

    observeEvent(glob$documents, {
      shinyWidgets::updatePickerInput(
        session = session,
        "document_filter",
        choices = glob$documents,
        selected = glob$documents
      )
    })
    observeEvent(glob$category, {
      shinyWidgets::updatePickerInput(
        session = session,
        "category_filter",
        choices = glob$category,
        selected = glob$category
      )
    })

    observeEvent(glob$users_observer, {
      req(golem::get_golem_options(which = "mode") == "server")
      shinyWidgets::updatePickerInput(
        session = session,
        "user_filter",
        choices = get_user_permissions(
        glob$pool,
        glob$active_project
        ) %>% dplyr::pull(user_id, name = "user_name")        ,
        selected = get_user_permissions(
        glob$pool,
        glob$active_project
        ) %>% dplyr::pull(user_id)
      )
    })

    # Segments to display and filter ----
    observeEvent(
      c(
        input$code_filter,
        input$category_filter,
        input$document_filter,
        loc$user_filter,
        glob$segments_observer,
        glob$codebook,
        glob$category,
        glob$documents,
        glob$active_project
      ),
      { 
        loc$temp_df <- load_segments_analysis(
          pool = glob$pool,
          active_project = as.integer(glob$active_project),
          selected_codes = as.integer(input$code_filter),
          selected_categories = as.integer(input$category_filter),
          selected_docs = as.integer(input$document_filter),
          selected_users = as.integer(loc$user_filter)
        )
        
        if (nrow(loc$temp_df) > 0) {
          # handle view permissions
          if (glob$user$data$analysis_other_view != 1){
            loc$temp_df <- loc$temp_df %>% 
              dplyr::filter(user_id == glob$user$user_id)
          }
          
          loc$segments_df <- loc$temp_df %>%
            dplyr::left_join(glob$codebook,
              by = "code_id"
            ) %>%
            dplyr::left_join(tibble::enframe(glob$documents,
              name = "doc_name",
              value = "doc_id"
            ),
            by = "doc_id"
            )
        } else {
          loc$segments_df <- as.data.frame(NULL)
        }
      }
    )

    observeEvent(loc$segments_df, {
      if (nrow(loc$segments_df) > 0) {
        loc$segments_taglist <- purrr::pmap(
          list(
            loc$segments_df$segment_id,
            loc$segments_df$segment_text,
            loc$segments_df$doc_id,
            loc$segments_df$doc_name,
            loc$segments_df$code_name,
            loc$segments_df$code_color
          ),
          ~ format_segments(
            segment_id = ..1,
            segment_text = ..2,
            segment_document_id = ..3,
            segment_document_name = ..4,
            segment_code = ..5,
            segment_color = ..6
          )
        )
      }
    })

    output$segments <- renderUI({
      if (nrow(req(loc$segments_df)) > 0) {
        loc$segments_taglist
      } else {
        "No coded segments detected."
      }
    })

    # for download modules
    observeEvent(
      {
        loc$segments_df
        loc$segments_taglist
        glob$active_project
      },
      {
        if (!is.null(loc$segments_df) && nrow(loc$segments_df) > 0) {
          shinyjs::enable("download_csv_ui_1-download", asis = TRUE)
          shinyjs::enable("download_html_ui_1-report", asis = TRUE)
          glob$segments_df <- loc$segments_df %>%
            dplyr::select(doc_name, doc_id, segment_start, segment_end, code_name, 
                          code_id, segment_text, user_name)
          glob$segments_taglist <- loc$segments_taglist
        } else {
          shinyjs::disable("download_csv_ui_1-download", asis = TRUE)
          shinyjs::disable("download_html_ui_1-report", asis = TRUE)

        }
      }
    )
  })
}
