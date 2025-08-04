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
    checkboxInput(ns("header"), "Header (file includes column names)", TRUE),
    "",
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
        title = "Declare imported columns",
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

      # Create and process dataframe
      preview_df <- loc$input_df %>%
        dplyr::select(all_of(selected_columns)) %>%
        {
          if (
            isTruthy(input$code_description) &&
              input$code_description %in% selected_columns
          ) {
            dplyr::mutate(
              .,
              !!input$code_description := stringr::str_trunc(
                .data[[input$code_description]],
                width = 30,
                side = "right",
                ellipsis = "..."
              )
            )
          } else {
            .
          }
        }

      # Rename columns to standardized names
      column_names <- names(preview_df)
      new_names <- column_names

      # Map original column names to display names
      if (input$code_name %in% column_names) {
        new_names[new_names == input$code_name] <- "Code name"
      }
      if (
        isTruthy(input$code_description) &&
          input$code_description %in% column_names
      ) {
        new_names[new_names == input$code_description] <- "Code description"
      }
      if (isTruthy(input$code_color) && input$code_color %in% column_names) {
        new_names[new_names == input$code_color] <- "Code color"
      }

      names(preview_df) <- new_names

      # Render the datatable
      datatable <- DT::datatable(
        preview_df,
        escape = FALSE,
        options = list(dom = 't', ordering = FALSE)
      )

      # Apply color styling if needed (use new column name)
      if (
        isTruthy(input$code_color) && input$code_color %in% selected_columns
      ) {
        unique_colors <- unique(preview_df[["Code color"]])
        datatable <- datatable %>%
          DT::formatStyle(
            "Code color",
            backgroundColor = DT::styleEqual(unique_colors, unique_colors)
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

convert_to_rgb <- function(color) {
  # Check if the color is a vector of three numeric values first
  if (
    is.numeric(color) && length(color) == 3 && all(color >= 0 & color <= 255)
  ) {
    return(paste0("rgb(", paste(color, collapse = ", "), ")"))
  }

  # Ensure color is a character string for regex operations
  if (!is.character(color) || length(color) != 1) {
    return(FALSE)
  }

  # Improved regex for checking if the color is already in the rgb(r, g, b) format
  if (
    grepl(
      "^rgb\\s*\\(\\s*\\d{1,3}\\s*,\\s*\\d{1,3}\\s*,\\s*\\d{1,3}\\s*\\)$",
      color
    )
  ) {
    return(color)
  }

  # Check if the color is in hex format
  if (grepl("^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", color)) {
    # Convert hex to rgb
    hex_to_rgb <- function(hex) {
      hex <- gsub("#", "", hex)
      if (nchar(hex) == 3) {
        hex <- paste0(rep(strsplit(hex, "")[[1]], each = 2), collapse = "")
      }
      rgb_vals <- as.numeric(strtoi(
        substring(hex, seq(1, 6, 2), seq(2, 6, 2)),
        16
      ))
      return(paste0("rgb(", paste(rgb_vals, collapse = ", "), ")"))
    }
    return(hex_to_rgb(color))
  }

  # Check if the color is comma-separated RGB values (e.g., "234, 121, 21")
  if (grepl("^\\s*\\d{1,3}\\s*,\\s*\\d{1,3}\\s*,\\s*\\d{1,3}\\s*$", color)) {
    # Extract the numbers
    rgb_vals <- as.numeric(strsplit(gsub("\\s", "", color), ",")[[1]])
    # Validate that all values are in the valid range (0-255)
    if (all(rgb_vals >= 0 & rgb_vals <= 255)) {
      return(paste0("rgb(", paste(rgb_vals, collapse = ", "), ")"))
    }
  }

  # If no format is matched, return FALSE
  return(FALSE)
}
