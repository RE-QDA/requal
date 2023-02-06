create_project_db <- function(pool,
                              project_name,
                              project_description,
                              user_id) {
  project_df <- tibble::tibble(project_name,
    project_description,
    created_at = as.character(Sys.time())
  )

  if (!DBI::dbExistsTable(pool, "projects")) {
    # https://github.com/r-dbi/RSQLite/issues/59
    create_db_schema(pool)
  }

  create_project_record(pool, project_df, user_id = as.integer(user_id))

  active_project_df <- dplyr::tbl(pool, "projects") %>%
    dplyr::select(project_id, project_name) %>%
    dplyr::collect() %>%
    dplyr::slice_max(project_id, n = 1)

  project <- active_project_df %>%
    dplyr::pull(project_id)


  # names(active_project) <- active_project_df %>%
  #                                dplyr::pull(project_name)

  return(project)
}


read_project_db_util <- function(pool,
                                 table = "projects") {
  active_project_df <- dplyr::tbl(pool, table) %>%
    dplyr::select(project_id, project_name) %>%
    dplyr::collect()

  active_project <- active_project_df %>%
    dplyr::pull(project_id)

  names(active_project) <- active_project_df %>%
    dplyr::pull(project_name)

  return(active_project)
}

read_project_db <- function(pool, project_id) {
  if (!is.null(pool)) {
    if (!is.null(project_id)) { # filter by id

      project_id <- as.integer(project_id)

      active_project <- read_project_db_util(pool)
      active_project <- active_project[active_project == project_id]

      return(active_project)
    } else { # no filter

      active_project <- read_project_db_util(pool)

      return(active_project)
    }
  } else {
    return("No active project.")
  }
}

# list projects ----------------------------------
#' @importFrom rlang .env
list_db_projects <- function(pool) {
  project_name <- read_project_db(pool)

  return(project_name)
}


# list_db_documents -----------------------------
list_db_documents <- function(pool, active_project) {
  doc_id <- doc_name <- NULL

  active_project <- as.integer(active_project)

  project_documents <- dplyr::tbl(pool, "documents") %>%
    dplyr::filter(.data$project_id == .env$active_project) %>%
    dplyr::select(.data$doc_id, .data$doc_name) %>%
    dplyr::collect() %>%
    dplyr::mutate(doc_name = ifelse(is.na(doc_name), "", doc_name))


  documents_list <- project_documents$doc_id
  names(documents_list) <- project_documents$doc_name

  return(documents_list)
}

list_db_document_table <- function(pool, active_project) {
  active_project <- as.integer(active_project)

  documents_table <- dplyr::tbl(pool, "documents") %>%
    dplyr::filter(.data$project_id == .env$active_project) %>%
    dplyr::collect() %>%
    dplyr::select( # "ID" = doc_id,
      "Name" = doc_name,
      "Description" = doc_description,
      "Date added" = created_at
    )

  return(documents_table)
}

make_doc_table <- function(pool, active_project, doc_list) {
  renderTable(colnames = FALSE, {
    req(active_project)

    if (isTruthy(doc_list)) {
      list_db_document_table(pool,
        active_project = active_project
      )
    } else {
      "This project does not contain any documents yet."
    }
  })
}
