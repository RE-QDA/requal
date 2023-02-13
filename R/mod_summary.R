#' summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_summary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$br(),
    shinyWidgets::pickerInput(ns("summary_coders"), "Select users:",
      choices = "", multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `select-all-text` = "Select all",
        `deselect-all-text` = "Reset",
        `none-selected-text` = "Users to include in summary"
      )
    ),
    shinyWidgets::pickerInput(ns("summary_codes"), "Select codes:",
      choices = "", multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `select-all-text` = "Select all",
        `deselect-all-text` = "Reset",
        `none-selected-text` = "Codes to include in summary"
      )
    ),
    shinyWidgets::pickerInput(ns("summary_docs"), "Select documents:",
      choices = "", multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `select-all-text` = "Select all",
        `deselect-all-text` = "Reset",
        `none-selected-text` = "Documents to include in summary"
      )
    ),
    actionButton(ns("calculate"), "Calculate"),
    h2("Codes-documents frequency"),
    textOutput(ns("summary_message")),
    # dataTableOutput(ns("summary_table"))
    tableOutput(ns("summary_table"))
  )
}

#' summary Server Functions
#'
#' @noRd
mod_summary_server <- function(id, glob) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    loc <- reactiveValues()

    observeEvent({
      glob$active_project
      glob$codebook
      glob$documents
    }, {
      if (isTruthy(glob$active_project)) {
        # list users
        loc$users <- dplyr::tbl(glob$pool, "users") %>%
          dplyr::select(user_id, user_name) %>%
          dplyr::collect()
        shinyWidgets::updatePickerInput(
          session = session,
          "summary_coders",
          choices = c(
            stats::setNames(
              loc$users$user_id,
              loc$users$user_name
            )
          ),
          selected = loc$users$user_id
        )
        # list codes
        loc$codes <- load_codes_names(
          pool = glob$pool,
          active_project = glob$active_project
        )
        shinyWidgets::updatePickerInput(
          session = session,
          "summary_codes",
          choices = c(
            stats::setNames(
              loc$codes$code_id,
              loc$codes$code_name
            )
          ),
          selected = loc$codes$code_id
        )
      
      # list docs
      loc$docs <- load_all_docs_db(
        pool = glob$pool,
        active_project = glob$active_project
      ) %>%
        dplyr::select(doc_id, doc_name)
      shinyWidgets::updatePickerInput(
        session = session,
        "summary_docs",
        choices = c(
          stats::setNames(
            loc$docs$doc_id,
            loc$docs$doc_name
          )
        ),
        selected = loc$docs$doc_id
      )
    }
    })

    output$summary_message <- renderText("Click 'Calculate' to display table")

    observeEvent(input$calculate, {
      req(glob$active_project)


      loc$coded_segments <- load_all_segments_db(
        pool = glob$pool,
        active_project = glob$active_project
      ) %>%
        dplyr::left_join(., loc$codes, by = "code_id") %>%
        dplyr::left_join(., loc$docs, by = "doc_id") %>%
        dplyr::filter(
          user_id %in% as.integer(input$summary_coders) &
            code_id %in% as.integer(input$summary_codes) &
            doc_id %in% as.integer(input$summary_docs)
        )


      if (nrow(loc$coded_segments) > 0) {
        loc$summary_df <- loc$coded_segments %>%
          dplyr::count(doc_id, doc_name, code_name, code_id) %>%
          dplyr::arrange(doc_id, code_id) %>%
          dplyr::select(-c(code_id, doc_id)) %>%
          tidyr::pivot_wider(.,
            id_cols = "code_name",
            names_from = "doc_name",
            values_from = "n",
            values_fill = 0
          ) %>%
          dplyr::rename(code = code_name)

        output$summary_message <- renderText("")
        output$summary_table <- renderTable(
          loc$summary_df,
          caption = "The table displays the number of segments by code and document coded by the selected coders."
        )
        # output$summary_table <- renderDataTable(summary_df)
      } else {
        output$summary_message <- renderText("No segments were coded by selected coders.")
        output$summary_table <- NULL
      }
    })
  })
}
