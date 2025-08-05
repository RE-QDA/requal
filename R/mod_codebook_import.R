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
    checkboxInput(
      ns("header"),
      "Header (the file includes column names)",
      TRUE
    ),
    "",
    radioButtons(
      ns("sep"),
      "Separator",
      choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
      selected = ","
    ),
    actionButton(ns("import"), "Import")
  )
}

#' codebook_import Server Functions
#'
#' @noRd
mod_codebook_import_server <- function(id, glob) {
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

      # This pop-up modal serves as an import wizard
      showModal(modalDialog(
        title = "Specify imported codes",
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
        ) %>%
          tagAppendAttributes(class = "required"),
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

    # This observer ensures that column names available to user
    # are updating so that no column can be selected twice
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

    observeEvent(c(input$code_name, input$code_description, input$code_color), {
      req(input$code_name)

      # Initialize a vector of selected columns
      selected_columns <- input$code_name

      # Conditionally add optional columns if they are specified
      if (isTruthy(input$code_description)) {
        selected_columns <- c(selected_columns, input$code_description)
      }
      if (isTruthy(input$code_color)) {
        selected_columns <- c(selected_columns, input$code_color)
      }

      # Create and process preview dataframe
      loc$output_df <- loc$input_df %>%
        dplyr::select(all_of(selected_columns))

      # modify preview if needed
      # display ellipses for long code descriptions
      if (
        isTruthy(input$code_description) &&
          input$code_description %in% selected_columns
      ) {
        loc$output_df[[input$code_description]] <- stringr::str_trunc(
          loc$output_df[[input$code_description]],
          width = 30,
          side = "right",
          ellipsis = "..."
        )
      }
      # modify preview if needed
      # check and convert code colors
      if (
        isTruthy(input$code_color) &&
          input$code_color %in% selected_columns
      ) {
        loc$output_df[[input$code_color]] <- purrr::map_chr(
          loc$output_df[[input$code_color]],
          convert_to_rgb
        )

        if (any(is.na(loc$output_df[[input$code_color]]))) {
          rql_message(
            "Could not identify colors in the selected column. Resorting to default."
          )
          loc$output_df[[input$code_color]] <- "rgb(255,255,0)"
        }
      }

      # Map original column names to display names
      if (input$code_name %in% names(loc$output_df)) {
        names(loc$output_df)[which(
          names(loc$output_df) == input$code_name
        )] <- "Code name"
      }
      if (
        isTruthy(input$code_description) &&
          input$code_description %in% names(loc$output_df)
      ) {
        names(loc$output_df)[which(
          names(loc$output_df) == input$code_description
        )] <- "Code description"
      }
      if (
        isTruthy(input$code_color) &&
          input$code_color %in% names(loc$output_df)
      ) {
        names(loc$output_df)[which(
          names(loc$output_df) == input$code_color
        )] <- "Code color"
      }
    })

    # Render the datatable
    output$preview <- DT::renderDT({
      # The preview should not be generated if code_name is not supplied
      req(input$code_name)
      datatable <- DT::datatable(
        head(loc$output_df),
        escape = FALSE,
        options = list(dom = 't', ordering = FALSE)
      )
      # Apply color styling if needed (use new column name)
      if (
        isTruthy(input$code_color) && "Code color" %in% names(loc$output_df)
      ) {
        unique_colors <- unique(loc$output_df[["Code color"]])
        datatable <- datatable %>%
          DT::formatStyle(
            "Code color",
            backgroundColor = DT::styleEqual(unique_colors, unique_colors)
          )
      }
      # render datatable
      datatable
    })

    observeEvent(input$confirm, {
      removeModal()
      # Writing imported codes to database
      # check if code names are unique
      browser()
      code_names <- list_db_codes(
        pool = glob$pool,
        project_id = glob$active_project,
        user = glob$user
      )$code_name

      if (
        !length(c(code_names, loc$output_df[["Code description"]])) ==
          length(unique(c(code_names, loc$output_df[["Code description"]])))
      ) {
        warn_user("Conflicting code names found. Code names should be unique.")
        req(FALSE) # abort if not unique
      }

      # If unique check passed, proceed with writing to database
      if ("Code description" %in% names(loc$output_df)) {
        code_description <- loc$output_df[["Code description"]]
      } else {
        code_description <- NA
      }
      if ("Code color" %in% names(loc$output_df)) {
        code_color <- loc$output_df[["Code color"]]
      } else {
        code_color <- "rgb(255,255,0)"
      }
      imported_df <- data.frame(
        project_id = glob$active_project,
        code_name = loc$output_df[["Code name"]],
        code_description = code_description,
        code_color = code_color,
        user_id = glob$user$user_id
      )

      add_codes_record(
        pool = glob$pool,
        project_id = glob$active_project,
        codes_df = imported_df,
        user_id = glob$user$user_id
      )
      # update codebook observer
      glob$codebook_observer <- glob$codebook_observer + 1
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
  return(NA)
}
