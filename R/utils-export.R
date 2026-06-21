utils::globalVariables(c("guid"))

# prepare data.frame with codes and categories to export
export_codebook_csv <- function(glob) {
  categories <- read_db_categories(
    glob$pool,
    glob$active_project,
    glob$user,
    modify = FALSE
  ) %>%
    dplyr::mutate(
      category_title = dplyr::if_else(
        !is.na(category_description) & category_description != "",
        paste0(category_name, " (", category_description, ")"),
        category_name
      )
    ) %>%
    dplyr::select(-c(category_name, category_description))

  categories_map <- dplyr::tbl(glob$pool, "categories_codes_map") %>%
    dplyr::collect() %>%
    dplyr::inner_join(categories, by = "category_id")

  dplyr::left_join(glob$codebook, categories_map, by = "code_id") %>%
    dplyr::group_by(code_id, code_name, code_description, code_color) %>%
    dplyr::summarise(
      categories = paste0(category_title, collapse = " | ")
    ) %>%
    dplyr::mutate(
      code_color = purrr::map_chr(code_color, convert_colour_to_hex)
    )
}

# export codebook to .qdc file
export_codebook_xml <- function(glob) {
  codebook <- glob$codebook %>%
    dplyr::mutate(
      guid = uuid::UUIDgenerate(n = dplyr::n()),
      code_color = purrr::map_chr(code_color, convert_colour_to_hex)
    )

  categories <- read_db_categories(
    glob$pool,
    glob$active_project,
    glob$user
  ) %>%
    dplyr::mutate(guid = uuid::UUIDgenerate(n = dplyr::n()))

  if (nrow(categories)) {
    categories_map <- dplyr::tbl(glob$pool, "categories_codes_map") %>%
      dplyr::filter(project_id == as.numeric(!!glob$active_project)) %>%
      dplyr::filter(category_id %in% categories$category_id) %>%
      dplyr::collect() %>%
      dplyr::left_join(
        codebook %>% dplyr::select(code_id, membercode_guid = guid),
        by = "code_id"
      ) %>%
      dplyr::left_join(
        categories %>% dplyr::select(category_id, guid),
        by = "category_id"
      )
  }

  validator_file <- "qdc.xsd"
  validator_schema <- xml2::read_xml(system.file(
    validator_file,
    package = "requal"
  ))
  validator_ns <- xml2::xml_attr(validator_schema, "targetNamespace")

  codebook_xml <- xml2::xml_new_root(
    "CodeBook",
    "xmlns" = validator_ns,
    "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
    "xsi:schemaLocation" = paste(validator_ns, validator_file),
    "origin" = paste0(
      "requal (version: ",
      get_project_version(glob),
      ")"
    )
  )

  codes <- xml2::xml_add_child(codebook_xml, "Codes")
  # TODO? does not handle nesting of codes
  for (i in seq_len(nrow(codebook))) {
    code <- xml2::xml_add_child(codes, "Code")
    xml2::xml_attr(code, "guid") <- codebook$guid[i]
    xml2::xml_attr(code, "name") <- codebook$code_name[i]
    xml2::xml_attr(code, "color") <- codebook$code_color[i]
    xml2::xml_attr(code, "isCodable") <- "true"
    if (
      codebook$code_description[i] != "" && !is.na(codebook$code_description[i])
    ) {
      description <- xml2::xml_add_child(
        code,
        "Description",
        codebook$code_description[i]
      )
    }
  }
  if (nrow(categories)) {
    sets <- xml2::xml_add_child(codebook_xml, "Sets")
    for (i in seq_len(nrow(categories))) {
      set <- xml2::xml_add_child(sets, "Set")
      xml2::xml_attr(set, "guid") <- categories$guid[i]
      xml2::xml_attr(set, "name") <- categories$category_name[i]
      if (
        categories$category_description[i] != "" &&
          !is.na(categories$category_description[i])
      ) {
        description <- xml2::xml_add_child(
          set,
          "Description",
          categories$category_description[i]
        )
      }
      if (categories$guid[i] %in% categories_map$guid) {
        tmp <- categories_map %>%
          dplyr::filter(guid == categories$guid[i])
        for (j in 1:nrow(tmp)) {
          membercode <- xml2::xml_add_child(set, "MemberCode")
          xml2::xml_attr(membercode, "guid") <- tmp$membercode_guid[j]
        }
      }
    }
  }

  tmp_qdcfile <- tempfile(fileext = ".qdc")
  xml2::write_xml(
    codebook_xml,
    tmp_qdcfile,
    encoding = "utf-8",
    declaration = TRUE
  )
  validation_res <- xml2::xml_validate(
    xml2::read_xml(tmp_qdcfile), # use codebook_xml instead when checking for errors
    validator_schema
  )
  if (validation_res) {
    showNotification(
      "Codebook successfully exported.",
      type = "message"
    )
    return(codebook_xml)
  } else {
    showNotification(
      div(
        p("Error exporting codebook:"),
        p(paste(attr(validation_res, "errors"), collapse = ", "))
      ),
      type = "error",
      duration = NULL
    )
    return(NULL)
  }
}

# write project XML to a .qdpx file (a zip archive containing project.qde)
export_project_qdpx <- function(project_xml, file) {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  qde_path <- file.path(tmp_dir, "project.qde")
  xml2::write_xml(
    project_xml,
    qde_path,
    encoding = "utf-8",
    declaration = TRUE
  )

  # zip from within tmp_dir so paths in the archive are not absolute
  old_wd <- setwd(tmp_dir)
  on.exit(setwd(old_wd), add = TRUE)
  utils::zip(
    zipfile = file,
    files = "project.qde",
    flags = "-r9Xq"
  )

  invisible(file)
}

read_projects_db <- function(pool) {
  dplyr::tbl(pool, "projects") %>%
    dplyr::filter(project_id == !!as.integer(glob$active_project)) %>%
    dplyr::collect()
}

read_documents_db <- function(pool, active_project, user) {
  other_docs_permission <- user$data$data_other_view

  docs <- dplyr::tbl(pool, "documents") %>%
    dplyr::filter(.data$project_id == as.integer(active_project)) %>%
    dplyr::collect()

  if (!other_docs_permission) {
    docs <- docs |>
      dplyr::filter(user_id == user$user_id)
  }

  docs
}

read_segments_db <- function(pool, active_project, user) {
  other_segments_permission <- user$data$annotation_other_view

  segments <- dplyr::tbl(pool, "segments") %>%
    dplyr::filter(.data$project_id == as.integer(active_project)) %>%
    dplyr::collect()

  if (!other_segments_permission) {
    segments <- segments |>
      dplyr::filter(user_id == user$user_id)
  }

  segments
}

read_memos_db <- function(pool, active_project, user) {
  other_memos_permission <- user$data$memo_other_view

  memos <- dplyr::tbl(pool, "memos") %>%
    dplyr::filter(.data$project_id == as.integer(active_project)) %>%
    dplyr::collect()

  if (!other_memos_permission) {
    memos <- memos |>
      dplyr::filter(user_id == user$user_id)
  }

  memos
}


parse_date <- function(x) {
  format(as.POSIXct(x), "%Y-%m-%dT%H:%M:%OS6")
}

export_project_xml <- function(glob) {
  users <- get_users_in_project(glob$pool, glob$active_project) %>%
    dplyr::mutate(guid = uuid::UUIDgenerate(n = dplyr::n()))

  project <- read_projects_db(glob$pool)

  validator_file <- "refi.xsd"
  validator_schema <- xml2::read_xml(system.file(
    validator_file,
    package = "requal"
  ))
  validator_ns <- xml2::xml_attr(validator_schema, "targetNamespace")

  log <- load_logs_for_reporting(glob$pool, glob$active_project)

  created_user_id <- log %>%
    dplyr::filter(action == "Create project") %>%
    dplyr::left_join(users, by = c("user" = "user_name")) %>%
    dplyr::pull(user_id)

  created_user_guid <- users %>%
    dplyr::filter(user_id == created_user_id) %>%
    dplyr::pull(guid)

  last_mod <- log %>%
    tail(1)

  modified_user_id <- last_mod %>%
    dplyr::left_join(users, by = c("user" = "user_name")) %>%
    dplyr::pull(user_id)

  modified_user_guid <- users %>%
    dplyr::filter(user_id == modified_user_id) %>%
    dplyr::pull(guid)

  project_xml <- xml2::xml_new_root(
    "Project",
    "xmlns" = validator_ns,
    "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
    "xsi:schemaLocation" = paste(validator_ns, validator_file),
    "origin" = paste0(
      "requal (version: ",
      get_project_version(glob),
      ")"
    ),
    "creatingUserGUID" = created_user_guid,
    "modifiedDateTime" = parse_date(last_mod$created_at),
    "creationDateTime" = parse_date(project$created_at),
    "basePath" = NA,
    "modifyingUserGUID" = modified_user_guid,
    "name" = project$project_name
  )

  # Users
  users_xml <- xml2::xml_add_child(project_xml, "Users")
  for (i in seq_len(nrow(users))) {
    user <- xml2::xml_add_child(users_xml, "User")
    xml2::xml_attr(user, "guid") <- users$guid[i]
    xml2::xml_attr(user, "name") <- users$user_name[i]
  }

  # Codebook
  codebook <- list_db_codes(
    glob$pool,
    glob$active_project,
    glob$user
  )

  if (nrow(codebook)) {
    codebook_xml <- xml2::xml_add_child(project_xml, "CodeBook")
    codes <- xml2::xml_add_child(codebook_xml, "Codes")

    codebook <- codebook |>
      dplyr::arrange(tolower(code_name)) %>%
      dplyr::mutate(
        guid = uuid::UUIDgenerate(n = dplyr::n()),
        code_color = purrr::map_chr(code_color, convert_colour_to_hex)
      )
  }

  # TODO? does not handle nesting of codes
  for (i in seq_len(nrow(codebook))) {
    code <- xml2::xml_add_child(codes, "Code")
    xml2::xml_attr(code, "guid") <- codebook$guid[i]
    xml2::xml_attr(code, "name") <- codebook$code_name[i]
    xml2::xml_attr(code, "color") <- codebook$code_color[i]
    xml2::xml_attr(code, "isCodable") <- "true"
    if (
      codebook$code_description[i] != "" && !is.na(codebook$code_description[i])
    ) {
      description <- xml2::xml_add_child(
        code,
        "Description",
        codebook$code_description[i]
      )
    }
  }

  # Sources
  docs <- read_documents_db(
    glob$pool,
    glob$active_project,
    glob$user
  )

  if (nrow(docs)) {
    sources <- xml2::xml_add_child(project_xml, "Sources")

    docs <- docs |>
      dplyr::mutate(
        guid = uuid::UUIDgenerate(n = dplyr::n())
      )
  }

  segments <- read_segments_db(glob$pool, glob$active_project, glob$user)

  if (nrow(segments)) {
    segments <- segments |>
      dplyr::mutate(
        guid = uuid::UUIDgenerate(n = dplyr::n())
      )
  }

  memos <- read_memos_db(glob$pool, glob$active_project, glob$user)

  if (nrow(memos)) {
    memos <- memos |>
      dplyr::mutate(
        guid = uuid::UUIDgenerate(n = dplyr::n())
      )
  }

  for (i in seq_len(nrow(docs))) {
    src <- xml2::xml_add_child(sources, "TextSource")
    xml2::xml_attr(src, "guid") <- docs$guid[i]
    xml2::xml_attr(src, "name") <- docs$doc_name[i]
    xml2::xml_attr(src, "creatingUser") <- users$guid[
      users$user_id == docs$user_id[i]
    ]
    xml2::xml_attr(src, "creationDateTime") <- parse_date(docs$created_at[i])

    if (
      docs$doc_description[i] != "" &&
        !is.na(docs$doc_description[i])
    ) {
      description <- xml2::xml_add_child(
        src,
        "Description",
        docs$doc_description[i]
      )
    }

    doc_content <- xml2::xml_add_child(src, "PlainTextContent")
    xml2::xml_text(doc_content) <- docs$doc_text[i]

    # Segments
    doc_segments <- segments |>
      dplyr::filter(doc_id == docs$doc_id[i])
    for (i in seq_len(nrow(doc_segments))) {
      sel <- xml2::xml_add_child(src, "PlainTextSelection")
      xml2::xml_attr(sel, "guid") <- doc_segments$guid[i]
      xml2::xml_attr(sel, "startPosition") <- doc_segments$segment_start[i] - 1
      xml2::xml_attr(sel, "endPosition") <- doc_segments$segment_end[i]

      if (!is.na(doc_segments$code_id[i])) {
        coding <- xml2::xml_add_child(sel, "Coding")
        xml2::xml_attr(coding, "guid") <- uuid::UUIDgenerate()
        xml2::xml_attr(coding, "creatingUser") <- users$guid[
          users$user_id == doc_segments$user_id[i]
        ]
        code_ref <- xml2::xml_add_child(coding, "CodeRef")
        xml2::xml_attr(code_ref, "targetGUID") <- codebook$guid[
          codebook$code_id == doc_segments$code_id[i]
        ]
      } else {
        sg_memo_map <- dplyr::tbl(glob$pool, "memos_segments_map") %>%
          dplyr::filter(segment_id == !!doc_segments$segment_id[i]) %>%
          dplyr::collect()
        segment_memo <- xml2::xml_add_child(sel, "NoteRef")
        xml2::xml_attr(segment_memo, "targetGUID") <- memos$guid[
          memos$memo_id == sg_memo_map$memo_id
        ]
      }
    }
    # TODO: memos_documents_map => NoteRef (if memos_documents_map will be implemented)
  }

  # Notes
  if (nrow(memos)) {
    notes <- xml2::xml_add_child(project_xml, "Notes")

    for (i in seq_len(nrow(memos))) {
      note <- xml2::xml_add_child(notes, "Note")
      xml2::xml_attr(note, "guid") <- memos$guid[i]
      xml2::xml_attr(note, "creatingUser") <- users$guid[
        users$user_id == memos$user_id[i]
      ]
      text <- xml2::xml_add_child(note, "PlainTextContent")
      xml2::xml_text(text) <- memos$text[i]
    }
  }

  # Links

  # Sets
  categories <- read_db_categories(
    glob$pool,
    glob$active_project,
    glob$user
  )

  if (nrow(categories)) {
    categories <- categories |>
      dplyr::mutate(guid = uuid::UUIDgenerate(n = dplyr::n()))

    categories_map <- dplyr::tbl(glob$pool, "categories_codes_map") %>%
      dplyr::filter(project_id == as.numeric(!!glob$active_project)) %>%
      dplyr::filter(category_id %in% categories$category_id) %>%
      dplyr::collect() %>%
      dplyr::left_join(
        codebook %>% dplyr::select(code_id, membercode_guid = guid),
        by = "code_id"
      ) %>%
      dplyr::left_join(
        categories %>% dplyr::select(category_id, guid),
        by = "category_id"
      )
  }
  if (nrow(categories)) {
    sets <- xml2::xml_add_child(project_xml, "Sets")
    for (i in seq_len(nrow(categories))) {
      set <- xml2::xml_add_child(sets, "Set")
      xml2::xml_attr(set, "guid") <- categories$guid[i]
      xml2::xml_attr(set, "name") <- categories$category_name[i]
      if (
        categories$category_description[i] != "" &&
          !is.na(categories$category_description[i])
      ) {
        description <- xml2::xml_add_child(
          set,
          "Description",
          categories$category_description[i]
        )
      }
      if (categories$guid[i] %in% categories_map$guid) {
        tmp <- categories_map %>%
          dplyr::filter(guid == categories$guid[i])
        for (j in 1:nrow(tmp)) {
          membercode <- xml2::xml_add_child(set, "MemberCode")
          xml2::xml_attr(membercode, "targetGUID") <- tmp$membercode_guid[j]
        }
      }
    }
  }

  # Graphs

  tmp_qdefile <- tempfile(fileext = ".qde")
  xml2::write_xml(
    project_xml,
    tmp_qdefile,
    encoding = "utf-8",
    declaration = TRUE
  )
  validation_res <- xml2::xml_validate(
    xml2::read_xml(tmp_qdefile), # use codebook_xml instead when checking for errors
    validator_schema
  )
  if (validation_res) {
    showNotification(
      "Project successfully exported.",
      type = "message"
    )
    return(project_xml)
  } else {
    showNotification(
      div(
        p("Error exporting project:"),
        p(paste(attr(validation_res, "errors"), collapse = ", "))
      ),
      type = "error",
      duration = NULL
    )
    return(NULL)
  }
}
