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


  names(project) <- active_project_df %>%
                                 dplyr::pull(project_name)

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
list_db_documents <- function(pool, active_project, user) {
  doc_id <- doc_name <- NULL

  active_project <- as.integer(active_project)

  project_documents <- dplyr::tbl(pool, "documents") %>%
    dplyr::filter(.data$project_id == .env$active_project) %>%
    dplyr::select(.data$doc_id, .data$doc_name, .data$user_id) %>%
    dplyr::collect() %>%
    dplyr::mutate(doc_name = ifelse(is.na(doc_name), "", doc_name))

  if(!is.null(user$data) && user$data$data_other_modify == 0){
    project_documents <- project_documents %>% 
      dplyr::filter(user_id == user$user_id)
  }

  documents_list <- project_documents$doc_id
  names(documents_list) <- project_documents$doc_name

  return(documents_list)
}

list_db_document_table <- function(pool, active_project, user) {
  active_project <- as.integer(active_project)
  
  documents_table <- dplyr::tbl(pool, "documents") %>%
    dplyr::filter(.data$project_id == .env$active_project) %>%
    dplyr::left_join(., dplyr::tbl(pool, "users") %>% 
                       dplyr::select(user_id, user_name), 
                     by = "user_id") %>% 
    dplyr::collect()
  
  if(!is.null(user$data) && user$data$data_other_view == 0){
    documents_table <- documents_table %>% 
      dplyr::filter(user_id == user$user_id)
  }

  documents_table %>% dplyr::select( # "ID" = doc_id,
    "Name" = doc_name,
    "Description" = doc_description,
    "Date added" = created_at, 
    "Created by" = user_name,
  )
}

make_doc_table <- function(glob, doc_list) {
  renderTable(colnames = FALSE, {
    req(glob$active_project)

    if (isTruthy(doc_list)) {
      list_db_document_table(glob$pool, glob$active_project, glob$user)
    } else {
      "This project does not contain any documents yet."
    }
  })
}
