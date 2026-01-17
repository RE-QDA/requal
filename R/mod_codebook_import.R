utils::globalVariables(c(
  "parent_guid", "guid", "color", 
  "isCodable", "description", "set_guid", 
  "code_guid", "category_guids")
)


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
    selectInput(
      ns("file_type"),
      "Select import format",
      choices = c("REFI-QDA" = "refi-qda", "CSV" = "csv"),
      selected = "refi-qda"
    ),
    tags$hr(),

    # CSV input ----
    conditionalPanel(
      condition = "input.file_type == 'csv'",
      ns = ns,
      fileInput(
        ns("file"),
        with_help(
          "Choose CSV File",
          help_item = "codebook_csv_import",
          visible = TRUE
        ),
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv",
          "text/tab-separated-values",
          ".tsv"
        )
      ),
      tags$hr(),
      checkboxInput(
        ns("header"),
        "Header (the file includes column names)",
        TRUE
      ),
      radioButtons(
        ns("sep"),
        "Separator",
        choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
        selected = ","
      ),
      actionButton(ns("import"), "Import")
    ),

    # REFI-QDA input ----
    conditionalPanel(
      condition = "input.file_type == 'refi-qda'",
      ns = ns,
      fileInput(
        ns("qdc_file"),
        "Choose QDC File",
        accept = c(".qdc")
      ),
      actionButton(ns("import_qdc"), "Import")
    )
  )
}

#' codebook_import Server Functions
#'
#' @noRd
mod_codebook_import_server <- function(id, glob) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    loc <- reactiveValues()

    # Internal module helper functions ----
    # (scope includes input, session, loc, or ns)
    .get_column_choices_for <- function(field) {
      exclude_cols <- switch(
        field,
        "code_name" = c(input$code_description, input$code_color),
        "code_description" = c(input$code_name, input$code_color),
        "code_color" = c(input$code_name, input$code_description)
      )
      get_column_choices_excluding(colnames(loc$input_df), exclude_cols)
    }

    .show_import_wizard <- function() {
      showModal(modalDialog(
        title = "Specify imported codes",
        .create_column_selectors(),
        "Preview",
        DT::dataTableOutput(ns("preview")),
        footer = tagList(
          modalButton("Cancel"),
          shinyjs::disabled(actionButton(ns("confirm"), "Confirm"))
        )
      ))
    }

    .create_column_selectors <- function() {
      tagList(
        selectInput(
          ns("code_name"),
          "Select column for code name",
          choices = .get_column_choices_for("code_name"),
          selected = ""
        ) %>%
          tagAppendAttributes(class = "required"),

        selectInput(
          ns("code_description"),
          "Select column for code description",
          choices = .get_column_choices_for("code_description"),
          selected = ""
        ),

        selectInput(
          ns("code_color"),
          "Select column for code color",
          choices = .get_column_choices_for("code_color"),
          selected = ""
        )
      )
    }

    .update_all_selectors <- function() {
      selectors <- c("code_name", "code_description", "code_color")

      purrr::walk(selectors, function(selector) {
        updateSelectInput(
          session,
          selector,
          selected = input[[selector]],
          choices = .get_column_choices_for(selector)
        )
      })
    }

    # Server business logic ----

    ## CSV Import -----
    ### Load import file -----
    observeEvent(input$import, {
      req(input$file)
      loc$input_df <- utils::read.csv(
        input$file$datapath,
        header = input$header,
        sep = input$sep
      )
      .show_import_wizard()
    })

    ### Update column choices when selections change ----
    observeEvent(c(input$code_name, input$code_description, input$code_color), {
      if (isTruthy(input$code_name)) {
        shinyjs::enable("confirm")
      } else {
        shinyjs::disable("confirm")
      }
      .update_all_selectors()
    })

    ### Process imported dataframe -----
    observeEvent(c(input$code_name, input$code_description, input$code_color), {
      req(input$code_name)

      selected_columns <- build_selected_columns(
        input$code_name,
        input$code_description,
        input$code_color
      )

      loc$output_df <- loc$input_df %>%
        dplyr::select(dplyr::all_of(selected_columns)) %>%
        process_description_column(input$code_description) %>%
        process_color_column(input$code_color) %>%
        rename_columns_to_standard(
          input$code_name,
          input$code_description,
          input$code_color
        )
    })

    ### Render the preview datatable ----
    output$preview <- DT::renderDT({
      req(input$code_name)

      datatable <- DT::datatable(
        utils::head(loc$output_df),
        escape = FALSE,
        options = list(dom = 't', ordering = FALSE)
      )

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

      datatable
    })

    ### Import CSV execute -----
    observeEvent(input$confirm, {
      removeModal()

      # Get existing codes
      existing_codes <- list_db_codes(
        pool = glob$pool,
        project_id = glob$active_project,
        user = glob$user
      )$code_name

      # Validate and import
      if (validate_code_names(loc$output_df, existing_codes)) {
        imported_df <- prepare_import_dataframe(
          loc$output_df,
          glob$active_project,
          glob$user$user_id
        )

        db_import_codes(
          imported_df,
          glob$pool,
          glob$active_project,
          glob$user$user_id
        )

        # Update global state
        glob$codebook_observer <- ifelse(
          !isTruthy(glob$codebook_observer),
          1,
          (glob$codebook_observer + 1)
        )
      }
    })

    ## QDC Import -----
    observeEvent(input$import_qdc, {
      req(input$qdc_file)

      qdc_codebook_src <- parse_qdc(input$qdc_file$datapath)

      qdc_codebook_df <- qdc_codebook_src$codebook
      qdc_categories_df <- qdc_codebook_src$categories
      
      ### Import QDC execute ----
      if (nrow(qdc_categories_df)) {
        qdc_categories_df_full <- qdc_categories_df %>%
          dplyr::mutate(project_id = as.integer(glob$active_project)) %>%
          dplyr::mutate(user_id = as.integer(glob$user$user_id))

        imported_category_ids <- db_import_categories(
          qdc_categories_df_full,
          glob$pool,
          glob$active_project,
          glob$user$user_id
        )
      }

      if (nrow(qdc_codebook_df)) {
        # We add the info for current project and user to the imported dataframe
        qdc_codebook_df_full <- qdc_codebook_df %>%
        dplyr::mutate(project_id = as.integer(glob$active_project)) %>%
        dplyr::mutate(user_id = as.integer(glob$user$user_id))

        imported_code_ids <- db_import_codes(
          qdc_codebook_df_full,
          glob$pool,
          glob$active_project,
          glob$user$user_id
        )

        if ("category_guids" %in% colnames(qdc_codebook_df_full)) {
          qdc_code_category_map <- qdc_codebook_df_full %>%
            dplyr::select(code_guid, category_guids) %>%
            tidyr::unnest(category_guids, keep_empty = FALSE) %>%
            dplyr::rename(category_guid = category_guids) %>%
            dplyr::left_join(imported_code_ids, by = "code_guid") %>%
            dplyr::left_join(imported_category_ids, by = "category_guid") %>%
            dplyr::select(code_id, category_id)

          if (nrow(qdc_code_category_map)) {
            db_import_code_category_map(
              qdc_code_category_map,
              glob$pool,
              glob$active_project,
              glob$user$user_id
            )
          }
        }
      }

      
      # Update global state
      glob$codebook_observer <- ifelse(
        !isTruthy(glob$codebook_observer),
        1,
        (glob$codebook_observer + 1)
      )
    })
  })
}

# Helper functions for this module -----
## CSV helpers ----
convert_to_rgb <- function(color) {
  # Check if the color is a vector of three numeric values first
  if (
    is.numeric(color) && length(color) == 3 && all(color >= 0 & color <= 255)
  ) {
    return(paste0("rgb(", paste(color, collapse = ", "), ")"))
  }

  if (color %in% grDevices::colors()) {
    return(paste0(
      "rgb(",
      paste0(grDevices::col2rgb(color)[, 1], collapse = ", "),
      ")"
    ))
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
  return(NA_character_)
}


get_column_choices_excluding <- function(df_cols, exclude_cols) {
  c("", setdiff(df_cols, exclude_cols))
}

build_selected_columns <- function(
  code_name,
  code_description = NULL,
  code_color = NULL
) {
  columns <- code_name
  if (isTruthy(code_description)) {
    columns <- c(columns, code_description)
  }
  if (isTruthy(code_color)) {
    columns <- c(columns, code_color)
  }
  return(columns)
}

process_description_column <- function(df, description_col_name) {
  if (isTruthy(description_col_name) && description_col_name %in% names(df)) {
    df[[description_col_name]] <- stringr::str_trunc(
      df[[description_col_name]],
      width = 30,
      side = "right",
      ellipsis = "..."
    )
  }
  return(df)
}

process_color_column <- function(df, color_col_name) {
  if (isTruthy(color_col_name) && color_col_name %in% names(df)) {
    df[[color_col_name]] <- purrr::map_chr(
      df[[color_col_name]],
      convert_to_rgb
    )

    # Handle invalid colors
    if (any(is.na(df[[color_col_name]]))) {
      rql_message(
        "Could not identify colors in the selected column. Resorting to default."
      )
      df[[color_col_name]][is.na(df[[color_col_name]])] <- "rgb(255, 255, 0)"
    }
  }
  return(df)
}

rename_columns_to_standard <- function(
  df,
  code_name,
  code_description = NULL,
  code_color = NULL
) {
  column_mappings <- list(
    list(old = code_name, new = "Code name"),
    list(old = code_description, new = "Code description"),
    list(old = code_color, new = "Code color")
  )

  for (mapping in column_mappings) {
    if (isTruthy(mapping$old) && mapping$old %in% names(df)) {
      names(df)[which(names(df) == mapping$old)] <- mapping$new
    }
  }
  return(df)
}

validate_code_names <- function(output_df, existing_codes) {
  code_names_imported <- as.character(output_df[["Code name"]])

  conflict_exists <- any(code_names_imported %in% existing_codes)
  duplication_exists <- any(duplicated(code_names_imported))

  if (conflict_exists || duplication_exists) {
    warn_user(
      "Conflicting or duplicate code names found. Code names should be unique."
    )
    return(FALSE)
  }
  return(TRUE)
}

prepare_import_dataframe <- function(output_df, project_id, user_id) {
  # Extract values with defaults
  code_description <- if ("Code description" %in% names(output_df)) {
    output_df[["Code description"]]
  } else {
    NA
  }

  code_color <- if ("Code color" %in% names(output_df)) {
    output_df[["Code color"]]
  } else {
    "rgb(255,255,0)"
  }

  data.frame(
    project_id = project_id,
    code_name = as.character(output_df[["Code name"]]),
    code_description = code_description,
    code_color = code_color,
    user_id = user_id
  )
}

db_import_codes <- function(imported_df, pool, project_id, user_id) {
  # Create a list to store mappings
  mappings <- purrr::map_df(seq(nrow(imported_df)), function(i) {
    row_df <- imported_df[i, ]
    current_guid <- row_df$code_guid

    # Add the code and get the new ID
    new_code_id <- add_codes_record(
      pool = pool,
      project_id = project_id,
      codes_df = row_df %>%
        dplyr::select(
          project_id,
          user_id,
          code_name,
          code_description,
          code_color #TODO, code_parent_id
        ),
      user_id = user_id
    )

    # Return the mapping as a tibble
    tibble::tibble(
      code_guid = current_guid,
      code_id = new_code_id
    )
  })

  return(mappings)
}

db_import_categories <- function(imported_df, pool, project_id, user_id) {
  mappings <- purrr::map_df(seq(nrow(imported_df)), function(i) {
    row_df <- imported_df[i, ]
    current_guid <- row_df$category_guid

    new_category_id <- add_category_record(
      pool = pool,
      project_id = project_id,
      categories_df = row_df %>%
        dplyr::select(project_id, user_id, category_name, category_description),
      user_id = user_id
    )

    tibble::tibble(
      category_guid = current_guid,
      category_id = new_category_id
    )
  })

  return(mappings)
}

db_import_code_category_map <- function(
  imported_df,
  pool,
  project_id,
  user_id
) {
  add_category_code_record(
    pool = pool,
    active_project = project_id,
    user_id = user_id,
    edge = imported_df
  )
}

## QDC helpers ----
parse_qdc <- function(qdc_filepath) {
  converting_msg <- showNotification(
    "Converting REFI-QDA codebook...",
    duration = NULL,
    closeButton = FALSE
  )
  on.exit(removeNotification(converting_msg), add = TRUE)

  tryCatch(
    {
      qdc_file <- xml2::read_xml(qdc_filepath)
      qdc_ns <- c(qdc = xml2::xml_attr(qdc_file, "xmlns")[1])
      qdc_schema <- xml2::read_xml(system.file('qdc.xsd', package = 'requal'))
      #TODO check if we could validate against qdpx schema
      # check file against validation schema
      validatition_res <- xml2::xml_validate(qdc_file, qdc_schema)
      if (as.logical(validatition_res)) {
        rql_message("Validation of QDC file format: Success!")
      } else {
        errors <- paste(attributes(validatition_res)$errors, collapse = ", ")
        rql_message(paste("Validation of QDC file format: Error!", errors))
      }
      imported_codes <- get_qdc_codebook(qdc_file, qdc_ns)
      imported_categories <- get_qdc_categories(qdc_file, qdc_ns)
      list(codebook = imported_codes, categories = imported_categories)
    },
    error = function(e) {
      rql_message(paste("Error parsing QDC file:", e$message, "\nCodebook not imported."), type = "error")
      list(codebook = data.frame(), categories = data.frame())
    }
  )
}

get_qdc_codebook <- function(qdc_file, qdc_ns) {
  codes <- xml2::xml_find_all(
    qdc_file,
    "//qdc:CodeBook/qdc:Codes//qdc:Code",
    ns = qdc_ns
  )

  parent_guids <- purrr::map_df(codes, .f = function(x) {
    tibble::tibble(
      guid = xml2::xml_attr(x, "guid"),
      parent_guid = xml2::xml_parent(x) %>%
        xml2::xml_attr("guid")
    )
  })

  codes_df <- xml2::xml_attrs(codes) %>% dplyr::bind_rows()

  # join code relationships to codes dataframe and reassign hierarchical names
  codes_df_joined <- codes_df %>%
    dplyr::left_join(parent_guids, by = "guid")
  codes_df_renamed <- codes_df_joined %>%
    dplyr::mutate(
      name = purrr::map2_chr(parent_guid, name, .f = function(x, y) {
        code_name <- y
        while (!is.na(x)) {
          df <- codes_df_joined %>%
            dplyr::filter(guid == x)
          # get code name
          code_name <- paste(
            df %>% dplyr::pull(name),
            code_name,
            sep = " > "
          )
          # update x
          # (the while-loop continues until parent_guid is NA)
          x <- df %>%
            dplyr::pull(parent_guid)
        }
        code_name
      })
    )

  # get codes descriptions
  descriptions <- purrr::map_chr(codes, .f = function(code_node) {
    desc <- xml2::xml_find_first(code_node, ".//qdc:Description", ns = qdc_ns)
    if (is.na(desc)) {
      return("")
    } else {
      return(xml2::xml_text(desc))
    }
  })

  # TODO add import info from qdc header to descriptions

  # add descriptions
  codes_df_renamed <- codes_df_renamed %>%
    dplyr::mutate(description = descriptions)
  # add default color
  if (!"color" %in% names(codes_df_renamed)) {
    codes_df_renamed <- codes_df_renamed %>%
      dplyr::mutate(color = "#FFFF00")
  } else {
    if (any(is.na(codes_df_renamed$color))) {
      missing_colors <- which(is.na(codes_df_renamed$color))
      codes_df_renamed$color[missing_colors] <- "#FFFF00"
      # TODO add a check for non-hex colors
    }
  }

  # Warning for non-codable codes
  if (
    any(!isTRUE(stringr::str_detect(codes_df_renamed$isCodable, "[Tt]rue")))
  ) {
    rql_message(
      "Non-codable codes found in QDC file. Only codable codes will be imported."
    )
  }

  # clean up result
  codes_converted <- codes_df_renamed %>%
    dplyr::mutate(description = descriptions) %>%
    dplyr::mutate(
      code_color = purrr::map_chr(color, .f = function(input_color) {
        rgb_colours <- grDevices::col2rgb(input_color)
        paste0(
          "rgb(",
          rgb_colours[1],
          ",",
          rgb_colours[2],
          ",",
          rgb_colours[3],
          ")"
        )
      })
    ) %>%
    dplyr::mutate(isCodable = stringr::str_detect(isCodable, "[Tt]rue")) %>%
    # so far no support for non-codable codes in requal
    # either we remove them or enforce codability for all codes
    dplyr::filter(isCodable) %>%
    dplyr::select(
      code_name = name,
      code_description = description,
      code_color,
      guid = guid,
      parent_guid
    )
  #TODO unique code names check and paste

  # Get all sets with their member codes
  sets <- xml2::xml_find_all(
    qdc_file,
    "//qdc:CodeBook/qdc:Sets/qdc:Set",
    ns = qdc_ns
  )
  if (length(sets) > 0) {
    # Create a mapping of code GUID to set GUIDs
    # (a code can belong to multiple sets)
    code_to_sets <- purrr::map_df(sets, .f = function(set) {
      set_guid <- xml2::xml_attr(set, "guid")
      member_codes <- xml2::xml_find_all(set, ".//qdc:MemberCode", ns = qdc_ns)

      if (length(member_codes) > 0) {
        purrr::map_df(member_codes, .f = function(member) {
          tibble::tibble(
            guid = xml2::xml_attr(member, "guid"),
            set_guid = set_guid
          )
        })
      } else {
        tibble::tibble(guid = character(), set_guid = character())
      }
    })

    # Aggregate set GUIDs into a list column
    code_sets <- code_to_sets %>%
      dplyr::group_by(guid) %>%
      dplyr::summarise(category_guids = list(set_guid), .groups = "drop")

    # Join codes with their sets
    codes_converted <- codes_converted %>%
      dplyr::left_join(code_sets, by = "guid")
  }
  return(
    codes_converted %>%
      # to be explicit about which guid is which later on
      dplyr::rename(code_guid = guid)
  )
}

get_qdc_categories <- function(qdc_file, qdc_ns) {
  sets <- xml2::xml_find_all(
    qdc_file,
    "//qdc:CodeBook/qdc:Sets/qdc:Set",
    ns = qdc_ns
  )

  # If no sets found, return empty tibble
  if (length(sets) == 0) {
    return(tibble::tibble(
      category_name = character(),
      category_description = character(),
      category_guid = character(),
      member_code_guids = list()
    ))
  }

  # Get set attributes (guid and name)
  sets_df <- xml2::xml_attrs(sets) %>% dplyr::bind_rows()

  # Get set descriptions
  descriptions <- purrr::map_chr(sets, .f = function(set_node) {
    desc <- xml2::xml_find_first(set_node, ".//qdc:Description", ns = qdc_ns)
    if (is.na(desc)) {
      return("")
    } else {
      return(xml2::xml_text(desc))
    }
  })

  # Build categories dataframe
  categories_converted <- sets_df %>%
    dplyr::mutate(
      category_description = descriptions
    ) %>%
    dplyr::select(
      category_name = name,
      category_description,
      category_guid = guid
    )

  return(categories_converted)
}
