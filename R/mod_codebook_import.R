#' codebook_import UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_codebook_import_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(
      ns("file"),
      "Choose CSV File",
      accept = c(
        "text/csv",
        "text/comma-separated-values,text/plain",
        ".csv"
      )
    ),
    tags$hr(),
    checkboxInput(ns("header"), "Header", TRUE),
    radioButtons(
      ns("sep"),
      "Separator",
      choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
      selected = ","
    ),
    actionButton(ns("import"), "Import"),
    tableOutput(ns("contents")) # Ensure the table output is included in the UI
  )
}

#' codebook_import Server Functions
#'
#' @noRd
mod_codebook_import_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    loc <- reactiveValues()

    observeEvent(input$import, {
      req(input$file)
      loc$input_df <- read.csv(
        input$file$datapath,
        header = input$header,
        sep = input$sep
      )
      loc$available_colnames <- colnames(loc$input_df)
      loc$used_colnames <- NA
      # Initialize the imported_df with blank columns
      loc$imported_df <- tibble::tibble(
        `Code name` = "",
        `Code description` = "",
        `Code color` = ""
      )

      showModal(modalDialog(
        title = "Map imported columns",
        selectInput(
          ns("code_name"),
          "Select column for code name",
          choices = c(
            "",
            setdiff(
              colnames(loc$input_df),
              c(input$code_description, input$code_color)
            )
          ),
          selected = ""
        ),
        selectInput(
          ns("code_description"),
          "Select column for code description",
          choices = c(
            "",
            setdiff(
              colnames(loc$input_df),
              c(input$code_name, input$code_color)
            )
          ),
          selected = ""
        ),
        selectInput(
          ns("code_color"),
          "Select column for code color",
          choices = c(
            "",
            setdiff(
              colnames(loc$input_df),
              c(input$code_name, input$code_description)
            )
          ),
          selected = ""
        ),
        "Preview",
        DT::dataTableOutput(ns("preview")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm"), "Confirm")
        )
      ))
    })
    observeEvent(c(input$code_name, input$code_description, input$code_color), {
      updateSelectInput(
        session,
        "code_name",
        selected = input$code_name,
        choices = c(
          "",
          setdiff(
            colnames(loc$input_df),
            c(input$code_color, input$code_description)
          )
        )
      )

      updateSelectInput(
        session,
        "code_description",
        selected = input$code_description,

        choices = c(
          "",
          setdiff(
            colnames(loc$input_df),
            c(input$code_name, input$code_color)
          )
        )
      )

      updateSelectInput(
        session,
        "code_color",
        selected = input$code_color,
        choices = c(
          "",
          setdiff(
            colnames(loc$input_df),
            c(input$code_name, input$code_description)
          )
        )
      )
    })

    # Render preview of the data frame with selected columns
    output$preview <- DT::renderDT({
      req(input$code_name)

      # Initialize a list to store selected columns
      selected_columns <- input$code_name

      # Conditionally add optional columns if they are specified
      if (isTruthy(input$code_description)) {
        selected_columns <- c(selected_columns, input$code_description)
      }
      if (isTruthy(input$code_color)) {
        selected_columns <- c(selected_columns, input$code_color)
      }

      # Select the specified columns from the data frame
      preview_df <- loc$input_df %>%
        dplyr::select(all_of(selected_columns))

      # Truncate the code description if it is part of the selected columns
      if (input$code_description %in% selected_columns) {
        preview_df <- preview_df %>%
          dplyr::mutate(
            !!input$code_description := stringr::str_trunc(
              .data[[input$code_description]],
              width = 30,
              side = "right",
              ellipsis = "..."
            )
          )
      }

      # Render the datatable
      datatable <- DT::datatable(
        preview_df,
        escape = FALSE,
        options = list(dom = 't', ordering = FALSE)
      )

      # Apply color formatting if 'Code color' is part of the selected columns
      if (input$code_color %in% selected_columns) {
        datatable <- datatable %>%
          DT::formatStyle(
            input$code_color,
            backgroundColor = DT::styleEqual(
              unique(preview_df[[input$code_color]]),
              unique(preview_df[[input$code_color]])
            )
          )
      }

      datatable
    })

    observeEvent(input$confirm, {
      removeModal()
      # Final processing of the data frame can be done here
    })
  })
}

## To be copied in the UI
# mod_codebook_import_ui("codebook_import_1")

## To be copied in the server
# mod_codebook_import_server("codebook_import_1")
