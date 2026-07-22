# File structure:
# 1. Main parsing function
# 1.1 Parsing helpers
# 1.2 Parsing utils
# 2. Import function
# 2.1 Import helpers
# 2.2 Import utils

# 1. Main parsing function ----

parse_qdpx <- function(path) {
  rql_message("Converting project...")
  import_dir <- paste0(tempdir(), .Platform$file.sep, "QDPX_import")
  utils::unzip(path, exdir = import_dir)
  refi_ns <- c(qda = "urn:QDA-XML:project:1.0")
  # xml_file <- xml2::read_xml("~/ownCloud/Temp_ownCloud/Die-Hexen-und-der-Böse-Feind/project.qde")
  xml_file <- xml2::read_xml(paste0(
    import_dir,
    .Platform$file.sep,
    "project.qde"
  ))
  xml_schema <- xml2::read_xml(system.file('refi.xsd', package = 'requal'))

  # check file against validation schema
  if (as.logical(xml2::xml_validate(xml_file, xml_schema))) {
    rql_message("Valid REFI-QDA file.")
  } else {
    rql_message("Invalid REFI-QDA file.")
  }
  # extract data from file
  imp_project <- list()
  imp_project$project <- .refi_get_project(xml_file, refi_ns = refi_ns)
  imp_project$users <- .refi_get_users(xml_file, refi_ns = refi_ns)
  imp_project$codebook <- .refi_get_codebook(xml_file, refi_ns = refi_ns)
  imp_project$sources <- .refi_get_sources(
    xml_file,
    refi_ns = refi_ns,
    import_dir = import_dir
  )
  imp_project$selections <- .refi_get_selections(
    xml_file,
    refi_ns = refi_ns
  )
  imp_project$notes <- .refi_get_notes(
    xml_file,
    refi_ns = refi_ns,
    import_dir = import_dir
  )
  #imp_project$sets <- .xml2df(xml_file, "//qda:Project/qda:Sets/qda:Set")
  #xml2::xml_find_all(xml_file, "//qda:Project/qda:Links", ns = refi_ns)
  return(imp_project)
}

# 1.1 Parsing helpers ----
# Parse project information ----
.refi_get_project <- function(xml_file, refi_ns) {
  project_node <- xml2::xml_find_all(xml_file, "//qda:Project", ns = refi_ns)
  description <- xml2::xml_find_first(
    project_node,
    ".//qda:Description",
    ns = refi_ns
  ) |>
    xml2::xml_text()
  origin <- project_node |> xml2::xml_attr("origin")
  createdDateTime <- project_node |>
    xml2::xml_attr("createdDateTime") |>
    strptime(format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  modifiedDateTime <- project_node |>
    xml2::xml_attr("modifiedDateTime") |>
    strptime(format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  project_description <- glue::glue(
    description,
    "\n---\n",
    "Origin\n: {origin}\n",
    "Created at\n: {createdDateTime}\n",
    "Modified at\n: {modifiedDateTime}\n"
  )
  project_name <- project_node |> xml2::xml_attr("name")
  project_df <- tibble::tibble(
    project_name = as.character(project_name),
    project_description = as.character(project_description),
  )
  return(project_df)
}

# Parse users information ----
.refi_get_users <- function(xml_file, refi_ns) {
  users_node <- xml2::xml_find_all(
    xml_file,
    "//qda:Users/qda:User",
    ns = refi_ns
  )
  user_names <- users_node |> xml2::xml_attr("name")
  user_ids <- users_node |> xml2::xml_attr("id")
  user_ids[is.na(user_ids)] <- user_names[is.na(user_ids)]
  user_ids <- make.unique(user_ids, sep = "_")
  user_guids <- users_node |> xml2::xml_attr("guid")
  users_df <- tibble::tibble(
    user_login = as.character(user_ids),
    user_name = as.character(user_names),
    user_guid = as.character(user_guids)
  )
  return(users_df)
}

# Parse codebook information ----
.refi_get_codebook <- function(xml_file, refi_ns) {
  codes <- xml2::xml_find_all(
    xml_file,
    "//qda:Project/qda:CodeBook/qda:Codes//qda:Code",
    ns = refi_ns
  )
  parent_guids <- purrr::map_df(codes, .f = function(x) {
    tibble::tibble(
      guid = xml2::xml_attr(x, "guid"),
      parent_guid = xml2::xml_parent(x) |>
        xml2::xml_attr("guid")
    )
  })

  codes_df <- xml2::xml_attrs(codes) |> dplyr::bind_rows()

  # join code relationships to codes dataframe and reassign names
  codes_df_joined <- codes_df |>
    dplyr::left_join(parent_guids, by = "guid")
  codes_df_renamed <- codes_df_joined |>
    dplyr::mutate(
      name = purrr::map2_chr(parent_guid, name, .f = function(x, y) {
        code_name <- y
        while (!is.na(x)) {
          df <- codes_df_joined |>
            dplyr::filter(guid == x)
          # get code name
          code_name <- paste(
            df |> dplyr::pull(name),
            code_name,
            sep = " :: "
          )
          # update x
          x <- df |>
            dplyr::pull(parent_guid)
        }
        code_name
      })
    )

  # get codes descriptions
  descriptions <- purrr::map_chr(codes, .f = function(code_node) {
    desc <- xml2::xml_find_first(code_node, ".//qda:Description", ns = refi_ns)
    if (is.na(desc)) {
      return("")
    } else {
      return(xml2::xml_text(desc))
    }
  })

  # clean up result
  codes_converted <- codes_df_renamed |>
    dplyr::mutate(description = descriptions) |>
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
    ) |>
    dplyr::mutate(isCodable = stringr::str_detect(isCodable, "[Tt]rue")) |>
    # so far no support for non-codable codes in requal
    dplyr::filter(isCodable) |>
    dplyr::select(
      code_name = name,
      code_description = description,
      code_color,
      guid,
      parent_guid
    )

  # Add row-based code_id and resolve parent GUIDs to parent code_ids
  codes_with_ids <- codes_converted |>
    tibble::rownames_to_column("code_id") |>
    dplyr::mutate(code_id = as.integer(code_id))

  # Create a lookup for parent GUID to parent code_id
  parent_lookup <- codes_with_ids |>
    dplyr::select(parent_guid_for_lookup = guid, code_parent_id = code_id)

  # Resolve parent_guid to code_parent_id (the row number of the parent code)
  codes_final <- codes_with_ids |>
    dplyr::left_join(
      parent_lookup,
      by = c("parent_guid" = "parent_guid_for_lookup")
    ) |>
    dplyr::select(code_id, code_name, code_description, code_color, guid, code_parent_id)

  return(codes_final)
}

# Parse sources ----
.refi_get_sources <- function(xml_file, refi_ns, import_dir) {
  sources <- xml2::xml_find_all(
    xml_file,
    "//qda:Project/qda:Sources",
    ns = refi_ns
  )
  img_df <- .refi_get_src_type(sources, src_type = "PictureSource")
  pdf_df <- .refi_get_src_type(sources, src_type = "PDFSource")
  audio_df <- .refi_get_src_type(sources, src_type = "AudioSource")
  video_df <- .refi_get_src_type(sources, src_type = "VideoSource")

  # TODO when support for sources implemented
  purrr::walk2(
    list(img_df, pdf_df, audio_df, video_df),
    list("Picture", "PDF", "Audio", "Video"),
    .f = function(x, y) {
      msg <- paste0("Ignoring ", nrow(x), " sources of type '", y, "'.")
      rql_message(msg)
    }
  )
  txt_nodes <- xml2::xml_find_all(sources, ".//qda:TextSource", ns = refi_ns)
  rql_message(paste0(
    "Importing ",
    length(txt_nodes),
    " sources of type 'Text'."
  ))

  # Handle case with no text sources
  if (length(txt_nodes) == 0) {
    rql_message("No text sources found in QDPX file.")
    return(tibble::tibble(
      source_guid = character(),
      name = character(),
      doc_description = character(),
      doc_text = character(),
      modifiedDateTime = character()
    ))
  }

  txt_Description <- .get_xml_el_txt(txt_nodes, "Description")
  txt_PlainTextContent <- .get_xml_el_txt(txt_nodes, "PlainTextContent")
  txt_NoteRef <- .get_xml_el_txt(txt_nodes, "NoteRef")
  txt_VariableValue <- .get_xml_el_txt(txt_nodes, "VariableValue")
  # xml2::xml_find_all(sources, ".//qda:PlainTextSelection", ns = refi_ns) |>
  # xml2::xml_attrs() |> dplyr::bind_rows()
  txt_df <- xml2::xml_attrs(txt_nodes) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      doc_description = txt_Description,
      PlainTextContent = txt_PlainTextContent
    )

  # Handle doc_text: use PlainTextContent if embedded, otherwise read from file
  has_embedded_content <- any(nzchar(trimws(txt_df$PlainTextContent)))
  rql_message(paste0("PlainTextContent check: has_embedded_content = ", has_embedded_content))

  if (!has_embedded_content) {
    # No embedded content, read from plainTextPath files
    rql_message("No embedded PlainTextContent found, attempting to read from files...")
    txt_df <- txt_df |>
      dplyr::mutate(
        doc_text = purrr::map_chr(
          plainTextPath,
          ~ .read_plaintext_path(.x, import_dir)
        )
      )
  } else {
    # Use embedded PlainTextContent
    rql_message("Using embedded PlainTextContent for documents.")
    txt_df <- txt_df |>
      dplyr::mutate(doc_text = PlainTextContent)
  }

  # Preserve source guid for segment mapping
  txt_df <- txt_df |>
    dplyr::rename(source_guid = guid)

  # Add modifiedDateTime from the TextSource nodes if available
  txt_modifiedDateTime <- .get_xml_el_txt(txt_nodes, "modifiedDateTime")
  txt_df <- txt_df |>
    dplyr::mutate(
      modifiedDateTime = ifelse(
        txt_modifiedDateTime == "",
        format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
        txt_modifiedDateTime
      )
    )

  return(txt_df)
}

# Parse selections (coded segments) ----
# Returns a tidy tibble of coded selections. Each row is one code applied to one
# plain-text selection. Documents and codes are referenced by GUID and matched to
# their new database ids at import time (see .import_segments()).
.refi_get_selections <- function(xml_file, refi_ns) {
  empty_selections <- tibble::tibble(
    source_guid = character(),
    code_guid = character(),
    startPosition = integer(),
    endPosition = integer()
  )

  sources <- xml2::xml_find_all(xml_file, "//qda:TextSource", ns = refi_ns)

  if (length(sources) == 0) {
    return(empty_selections)
  }

  selections <- purrr::map_df(sources, .f = function(source) {
    source_guid <- xml2::xml_attr(source, "guid")
    sels <- xml2::xml_find_all(
      source,
      ".//qda:PlainTextSelection",
      ns = refi_ns
    )
    if (length(sels) == 0) {
      return(empty_selections)
    }
    purrr::map_df(sels, .f = function(sel) {
      code_refs <- xml2::xml_find_all(
        sel,
        ".//qda:Coding/qda:CodeRef",
        ns = refi_ns
      )
      if (length(code_refs) == 0) {
        return(empty_selections)
      }
      tibble::tibble(
        source_guid = source_guid,
        code_guid = xml2::xml_attr(code_refs, "targetGUID"),
        startPosition = as.integer(xml2::xml_attr(sel, "startPosition")),
        endPosition = as.integer(xml2::xml_attr(sel, "endPosition"))
      )
    })
  })

  return(selections)
}

# Parse notes ----
.refi_get_notes <- function(xml_file, refi_ns, import_dir = tempdir()) {
  empty_notes <- tibble::tibble(
    note_guid = character(),
    creating_user = character(),
    note_name = character(),
    text = character()
  )

  notes_nodes <- xml2::xml_find_all(xml_file, "//qda:Notes/qda:Note", ns = refi_ns)

  if (length(notes_nodes) == 0) {
    return(empty_notes)
  }

  notes_df <- purrr::map_df(seq_along(notes_nodes), .f = function(i) {
    note <- notes_nodes[[i]]
    tryCatch({
      note_guid <- xml2::xml_attr(note, "guid") %||% ""
      creating_user <- xml2::xml_attr(note, "creatingUser") %||% ""
      note_name <- xml2::xml_attr(note, "name") %||% ""
      plain_text_path <- xml2::xml_attr(note, "plainTextPath")

      # Try to get text from PlainTextContent element first
      text_node <- xml2::xml_find_first(note, ".//qda:PlainTextContent", ns = refi_ns)
      text <- if (length(text_node) > 0) xml2::xml_text(text_node) else ""

      # If no embedded content and plainTextPath exists, try to read from file
      if (text == "" && !is.na(plain_text_path) && plain_text_path != "") {
        # Handle internal:// paths - these are typically stored in the sources folder
        if (grepl("^internal://", plain_text_path)) {
          # Extract the filename from the internal path
          file_id <- sub("^internal://", "", plain_text_path)
          # Try common locations for the file
          possible_paths <- c(
            paste0(import_dir, .Platform$file.sep, "sources", .Platform$file.sep, file_id),
            paste0(import_dir, .Platform$file.sep, "sources", .Platform$file.sep, basename(file_id)),
            paste0(import_dir, .Platform$file.sep, file_id)
          )
          for (path in possible_paths) {
            if (file.exists(path)) {
              text <- paste0(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
              break
            }
          }
        } else {
          # Regular file path
          path_to_file <- paste0(import_dir, .Platform$file.sep, plain_text_path)
          if (file.exists(path_to_file)) {
            text <- paste0(readLines(path_to_file, encoding = "UTF-8", warn = FALSE), collapse = "\n")
          }
        }
      }

      tibble::tibble(
        note_guid = note_guid,
        creating_user = creating_user,
        note_name = note_name,
        text = text
      )
    }, error = function(e) {
      rql_message(paste("Warning: Could not parse note", i, "-", e$message))
      tibble::tibble(
        note_guid = "",
        creating_user = "",
        note_name = "",
        text = ""
      )
    })
  })

  return(notes_df)
}

# 1.2 Parsing utils ----
.refi_get_child <- function(
  node,
  xpath,
  ns = c(qda = "urn:QDA-XML:project:1.0")
) {
  node |>
    xml2::xml_find_all(xpath, ns = ns)
}

.refi_get_src_type <- function(sources, src_type = "TextSource") {
  src <- sources |>
    .refi_get_child(paste0("//qda:", src_type))
  type_descriptions <- purrr::map(
    src,
    ~ .refi_get_child(.x, "qda:Description")
  ) |>
    purrr::map_chr(.f = function(x) {
      if (length(x) == 0) {
        ""
      } else {
        xml2::xml_text(x)
      }
    })
  type_df <- sources |>
    .refi_get_child(paste0("//qda:", src_type)) |>
    xml2::xml_attrs() |>
    (function(x) do.call(rbind, x))()
  tibble::as_tibble(type_df) |>
    dplyr::mutate(doc_description = type_descriptions)
}

.get_xml_el_txt <- function(node, el) {
  txt_vec <- purrr::map(
    node,
    xml2::xml_find_all,
    paste0("qda:", el),
    ns = c(qda = "urn:QDA-XML:project:1.0")
  ) |>
    purrr::map_chr(.f = function(x) {
      y <- xml2::xml_text(x)
      if (length(y) == 0) {
        y <- as.character("")
      }
      return(y)
    })
  return(txt_vec)
}

.read_plaintext_path <- function(file, import_dir) {
  # Try multiple possible paths for the plaintext file
  possible_paths <- c(
    paste0(import_dir, .Platform$file.sep, "sources", .Platform$file.sep, basename(file)),
    paste0(import_dir, .Platform$file.sep, basename(file)),
    paste0(import_dir, .Platform$file.sep, file)  # Use full path from XML if relative
  )

  for (path_to_file in possible_paths) {
    if (file.exists(path_to_file)) {
      plain_text <- paste0(
        readLines(path_to_file, encoding = "UTF-8", warn = FALSE),
        collapse = "\n"
      )
      return(plain_text)
    }
  }

  # File not found - try without extension and with .txt
  base_name <- tools::file_path_sans_ext(basename(file))
  txt_path <- paste0(dirname(possible_paths[1]), .Platform$file.sep, base_name, ".txt")
  if (file.exists(txt_path)) {
    plain_text <- paste0(
      readLines(txt_path, encoding = "UTF-8", warn = FALSE),
      collapse = "\n"
    )
    return(plain_text)
  }

  rql_message(paste("Warning: Could not find plaintext file:", file))
  return("")
}

# 2 Main import function ----
import_project <- function(content, user_id, active_project, pool) {
  rql_message("Starting project import...")

  # Ensure all input data is materialized (not lazy tables)
  if (inherits(content$codebook, "tbl")) {
    rql_message("Materializing codebook data...")
    content$codebook <- dplyr::collect(content$codebook)
  }
  if (inherits(content$sources, "tbl")) {
    rql_message("Materializing sources data...")
    content$sources <- dplyr::collect(content$sources)
  }
  if (inherits(content$selections, "tbl")) {
    rql_message("Materializing selections data...")
    content$selections <- dplyr::collect(content$selections)
  }

  rql_message(paste0("Codebook: ", nrow(content$codebook), " codes"))
  rql_message(paste0("Sources: ", nrow(content$sources), " documents"))
  rql_message(paste0("Selections: ", nrow(content$selections), " coded segments"))
  rql_message(paste0("Notes: ", nrow(content$notes), " notes"))

  # Import codebook
  rql_message("Importing codebook...")
  code_map <- tryCatch({
    result <- .import_codebook(
      codebook = content$codebook,
      user_id,
      active_project,
      pool
    )
    rql_message("Codebook import successful.")
    result
  }, error = function(e) {
    rql_message(paste("Error importing codebook:", e$message))
    stop(e)
  })

  # Import sources
  rql_message("Importing sources...")
  doc_map <- tryCatch({
    result <- .import_sources(
      sources = content$sources,
      user_id,
      active_project,
      pool
    )
    rql_message("Sources import successful.")
    result
  }, error = function(e) {
    rql_message(paste("Error importing sources:", e$message))
    stop(e)
  })

  # Import segments
  rql_message("Importing segments...")
  tryCatch({
    .import_segments(
      selections = content$selections,
      code_map = code_map,
      doc_map = doc_map,
      user_id,
      active_project,
      pool
    )
    rql_message("Segments import successful.")
  }, error = function(e) {
    rql_message(paste("Error importing segments:", e$message))
    stop(e)
  })

  # Import notes as memos
  rql_message("Importing notes...")
  tryCatch({
    .import_notes(
      notes = content$notes,
      user_id,
      active_project,
      pool
    )
    rql_message("Notes import successful.")
  }, error = function(e) {
    rql_message(paste("Error importing notes:", e$message))
    # Don't stop import for notes errors
  })

  rql_message("Project import complete.")
}

# 2.1 Import helpers ----
# Import project info ----
.import_project_info <- function(project_info, user_id, active_project, pool) {
  db_update_value(
    pool,
    table = "projects",
    col_val = list(
      c(project_name = project_info$project_name),
      c(project_description = project_info$project_description)
    ),
    by_col_val = c(project_id = active_project)
  )
}


# Import codebook ----
# Returns a named vector: names = code_guid (from QDPX), values = code_id (in DB)
.import_codebook <- function(codebook, user_id, active_project, pool) {
  rql_message(paste0("Importing ", nrow(codebook), " code(s)..."))

  # Keep guid for mapping
  codebook_with_guid <- codebook |>
    dplyr::select(
      code_id,
      code_name,
      code_description,
      code_color,
      code_parent_id,
      guid  # Preserve original QDPX code GUID for mapping
    ) |>
    dplyr::mutate(
      project_id = active_project,
      user_id = user_id
    ) |>
    dplyr::mutate(
      code_id = as.integer(code_id),
      code_parent_id = as.integer(code_parent_id)
    )

  # Build mapping: names = QDPX guid, values = row-based code_id (will be updated after DB insert)
  code_map <- setNames(codebook_with_guid$code_id, codebook_with_guid$guid)

  # Resolve parent relationships in R before writing to DB
  # code_parent_id currently contains row numbers; convert to actual parent code_ids
  if (any(!is.na(codebook_with_guid$code_parent_id))) {
    # Create lookup: row number -> code_id (which is just the row number at this point)
    row_to_code_id <- setNames(codebook_with_guid$code_id, codebook_with_guid$code_id)

    # Update code_parent_id to use actual code_id values using match
    parent_rows <- codebook_with_guid$code_parent_id
    resolved_parents <- rep(NA_integer_, length(parent_rows))
    valid_idx <- !is.na(parent_rows)
    if (any(valid_idx)) {
      matched_ids <- row_to_code_id[as.character(parent_rows[valid_idx])]
      resolved_parents[valid_idx] <- as.integer(matched_ids)
    }

    codebook_resolved <- codebook_with_guid |>
      dplyr::mutate(code_parent_id = resolved_parents)
  } else {
    codebook_resolved <- codebook_with_guid
  }

  # Strip guid and code_id for DB write (code_id will be auto-generated by DB)
  codebook_for_db <- codebook_resolved |>
    dplyr::select(-guid, -code_id) |>
    as.data.frame()

  # Make sure column exists for parent information
  db_helper_column(pool, "codes", "code_parent_id", "add")

  # Get the current max code_id before insert (for parent resolution)
  max_code_id_before <- DBI::dbGetQuery(pool, "SELECT COALESCE(MAX(code_id), 0) as max_id FROM codes")$max_id[1]

  # Reset the sequence to avoid duplicate key errors on PostgreSQL
  tryCatch({
    seq_name_query <- "SELECT pg_get_serial_sequence('codes', 'code_id') as seq_name"
    seq_result <- DBI::dbGetQuery(pool, seq_name_query)
    if (!is.na(seq_result$seq_name[1]) && seq_result$seq_name[1] != "") {
      seq_reset_query <- paste0(
        "SELECT setval('", seq_result$seq_name[1], "', ", max_code_id_before, ", true)"
      )
      DBI::dbExecute(pool, seq_reset_query)
    }
  }, error = function(e) {
    # Sequence might not exist or be named differently - ignore errors
    rql_message(paste("Note: Could not reset code sequence:", e$message))
  })

  # Write to DB (append only - don't delete existing codes from other projects)
  DBI::dbWriteTable(
    pool,
    "codes",
    codebook_for_db,
    append = TRUE,
    row.names = FALSE
  )

  # Read back the newly inserted codes (they should be the ones with code_id > max_code_id_before)
  current_codes_query <- glue::glue_sql(
    "SELECT code_id, code_parent_id FROM codes
     WHERE project_id = {active_project} AND code_id > {max_code_id_before}
     ORDER BY code_id",
    .con = pool
  )
  current_codes <- DBI::dbGetQuery(pool, current_codes_query)

  # Check if we got the expected number of codes
  if (nrow(current_codes) != nrow(codebook_with_guid)) {
    rql_message(paste("Warning: Expected", nrow(codebook_with_guid), "codes but found", nrow(current_codes)))
  }

  # Update code_map to use DB-generated code_ids
  # The order of rows should match the original codebook order
  names(code_map) <- codebook_with_guid$guid  # QDPX GUIDs
  if (length(current_codes$code_id) > 0) {
    code_map[] <- current_codes$code_id  # DB-generated code_ids
  }

  rql_message(paste0("Codebook import complete. ", length(code_map), " codes imported."))
  return(code_map)
}


# Import sources ----
# Returns a named vector: names = source_guid (from QDPX), values = doc_id (in DB)
.import_sources <- function(sources, user_id, active_project, pool) {
  # sources <- imp_project$sources
  # Ensure sources is a regular data frame (not a lazy table)
  if (inherits(sources, "tbl")) {
    sources <- dplyr::collect(sources)
  }

  # Check if sources is empty
  if (nrow(sources) == 0) {
    rql_message("No sources to import.")
    return(named(character()))
  }

  rql_message(paste0("Importing ", nrow(sources), " document(s)..."))

  # Ensure modifiedDateTime exists and handle NA/empty values
  if (!"modifiedDateTime" %in% names(sources)) {
    sources$modifiedDateTime <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  }
  # Replace NA or empty values with current time
  sources$modifiedDateTime[is.na(sources$modifiedDateTime) | sources$modifiedDateTime == ""] <-
    format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

  # Build mapping BEFORE stripping source_guid (documents table has no guid column)
  source_guids <- sources$source_guid

  # Check for required columns
  required_cols <- c("source_guid", "name", "doc_description", "doc_text", "modifiedDateTime")
  missing_cols <- setdiff(required_cols, names(sources))
  if (length(missing_cols) > 0) {
    rql_message(paste("Warning: Missing columns in sources:", paste(missing_cols, collapse = ", ")))
  }

  sources_clean <- sources |>
    dplyr::mutate(
      project_id = as.integer(active_project),
      user_id = as.integer(user_id),
      created_at = as.character(strptime(
        modifiedDateTime,
        format = "%Y-%m-%dT%H:%M:%S",
        tz = "UTC"
      ))
    ) |>
    dplyr::select(
      project_id,
      user_id,
      doc_name = name,
      doc_description,
      doc_text,
      created_at
    ) |>
    as.data.frame()

  # Get the current max doc_id before insert
  max_doc_id_before <- DBI::dbGetQuery(pool, "SELECT COALESCE(MAX(doc_id), 0) as max_id FROM documents")$max_id[1]

  # Write documents to DB (append only)
  rows_written <- DBI::dbWriteTable(
    pool,
    "documents",
    sources_clean,
    append = TRUE,
    row.names = FALSE
  )

  # Read back the newly inserted documents
  docs_query <- glue::glue_sql(
    "SELECT doc_id FROM documents WHERE project_id = {active_project} AND doc_id > {max_doc_id_before} ORDER BY doc_id",
    .con = pool
  )
  docs <- DBI::dbGetQuery(pool, docs_query)

  # Check if we got the expected number of documents
  if (nrow(docs) != length(source_guids)) {
    rql_message(paste("Warning: Expected", length(source_guids), "documents but found", nrow(docs)))
  }

  # Match by order since we just wrote them
  if (nrow(docs) > 0) {
    doc_map <- setNames(docs$doc_id, source_guids[seq_len(nrow(docs))])
  } else {
    doc_map <- setNames(character(), character())
  }

  rql_message(paste0("Imported ", nrow(docs), " document(s)."))
  return(doc_map)
}

# Import coded segments ----
.import_segments <- function(selections, code_map, doc_map, user_id, active_project, pool) {
  # Ensure selections is a regular data frame (not a lazy table)
  if (inherits(selections, "tbl")) {
    selections <- dplyr::collect(selections)
  }

  if (nrow(selections) == 0) {
    rql_message("No coded segments found in QDPX file.")
    return(invisible(NULL))
  }

  # Convert maps to tibbles for joining
  doc_map_tbl <- tibble::tibble(
    source_guid = names(doc_map),
    doc_id = unname(doc_map)
  )
  code_map_tbl <- tibble::tibble(
    code_guid = names(code_map),
    code_id = unname(code_map)
  )

  # Join selections to DB ids via the guid maps
  segments_to_import <- selections |>
    dplyr::left_join(doc_map_tbl, by = "source_guid") |>
    dplyr::left_join(code_map_tbl, by = "code_guid")

  # Drop unmatched rows (orphaned selections) and count them
  matched <- segments_to_import |>
    dplyr::filter(!is.na(doc_id), !is.na(code_id))
  dropped <- nrow(segments_to_import) - nrow(matched)
  if (dropped > 0) {
    rql_message(paste0(
      "Dropped ", dropped, " orphaned segment(s) with missing source or code."
    ))
  }

  if (nrow(matched) == 0) {
    rql_message("No valid segments to import.")
    return(invisible(NULL))
  }

  rql_message(paste0("Importing ", nrow(matched), " coded segment(s)."))

  # Build segments dataframe with correct offset conversion:
  # REFI startPosition = segment_start - 1  =>  segment_start = startPosition + 1
  segments_df <- matched |>
    dplyr::mutate(
      project_id = as.integer(active_project),
      user_id = as.integer(user_id),
      segment_start = startPosition + 1L,
      segment_end = endPosition
    ) |>
    as.data.frame()

  # First, load all document texts into memory to avoid pool checkout issues during segment text extraction
  unique_doc_ids <- unique(segments_df$doc_id)
  doc_texts_list <- list()
  for (did in unique_doc_ids) {
    doc_text <- DBI::dbGetQuery(
      pool,
      glue::glue_sql(
        "SELECT doc_text FROM documents WHERE project_id = {active_project} AND doc_id = {did}",
        .con = pool
      )
    )
    doc_texts_list[[as.character(did)]] <- doc_text$doc_text[1]
  }

  # Extract segment text using pre-loaded document texts
  segment_texts <- mapply(function(doc_id_val, start, end) {
    full_text <- doc_texts_list[[as.character(doc_id_val)]]
    if (is.null(full_text) || is.na(full_text)) {
      return("")
    }
    substr(full_text, start, end)
  }, segments_df$doc_id, segments_df$segment_start, segments_df$segment_end, USE.NAMES = FALSE)

  segments_df$segment_text <- segment_texts

  # Reorder columns to match expected schema
  segments_df <- segments_df |>
    dplyr::select(project_id, user_id, doc_id, code_id, segment_start, segment_end, segment_text)

  # Write segments to DB
  DBI::dbWriteTable(
    pool,
    "segments",
    segments_df,
    append = TRUE,
    row.names = FALSE
  )

  rql_message("Segment import complete.")
  invisible(NULL)
}

# Import notes as memos ----
.import_notes <- function(notes, user_id, active_project, pool) {
  if (nrow(notes) == 0) {
    rql_message("No notes to import.")
    return(invisible(NULL))
  }

  rql_message(paste0("Importing ", nrow(notes), " note(s)..."))

  # Convert notes to memos format
  # If note has a name, prepend it as the first line of the memo text
  memos_df <- notes |>
    dplyr::mutate(
      project_id = as.integer(active_project),
      user_id = as.integer(user_id),
      text = ifelse(
        note_name != "" & !is.na(note_name),
        paste0(note_name, "\n\n", text),
        text
      )
    ) |>
    dplyr::select(project_id, user_id, text) |>
    as.data.frame()

  # Get the current max memo_id before insert
  max_memo_id_before <- DBI::dbGetQuery(pool, "SELECT COALESCE(MAX(memo_id), 0) as max_id FROM memos")$max_id[1]

  # Write memos to DB (append only)
  DBI::dbWriteTable(
    pool,
    "memos",
    memos_df,
    append = TRUE,
    row.names = FALSE
  )

  rql_message(paste0("Imported ", nrow(memos_df), " memo(s)."))
  invisible(NULL)
}

# 2.2 Import utils ----
