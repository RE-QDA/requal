# Generate boxes of categories -----
gen_categories_ui <- function(id,
                              project_db,
                              active_project,
                              category_id,
                              category_name,
                              category_description) {
  ns <- NS(id)
  tags$div(
    box(
      tags$div(category_description, tags$p(), tags$p(tags$i("Drag & drop codes below"), style = "text-align: center; font-size: 80%;")),
      id = category_id,
      title = category_name,
      closable = FALSE,
      width = NULL,
      background = "blue",
      collapsible = TRUE,
      collapsed = TRUE,
      label = NULL,
      sortable::rank_list(
        input_id = glue::glue(ns("category_list_{category_id}")),
        text = NULL,
        labels = render_category_edges(
          project_db = project_db,
          active_project = active_project,
          category_id = category_id
        ),
        class = "category-rank-list",
        css_id = glue::glue(ns("rank-list_{category_id}")),
        options = sortable::sortable_options(
          group = list(
            name = "categories",
            pull = TRUE,
            put = TRUE
          ),
          onAdd = htmlwidgets::JS("function (evt) { check_categories(evt, this.el); }"),
          onRemove = htmlwidgets::JS("function (evt) { check_categories_delete(evt); }")
        )
      )
    ),
    class = "category-container", # extra div to capture category id
    `data-category_id` = category_id
  )
}


# Render categories -----

render_categories <- function(id,
                              active_project,
                              project_db) {
  if (isTruthy(active_project)) {
    project_categories <- list_db_categories(
      id = id,
      project_db = project_db,
      project_id = active_project
    ) %>%
      dplyr::mutate(
        project_db = project_db,
        active_project = active_project
      )


    if (nrow(project_categories) == 0) {
      "No categories have been created."
    } else {
      purrr::pmap(project_categories, gen_categories_ui)
    }
  } else {
    "No active project."
  }
}

# Read categories--------------------------------------------------------

read_db_categories <- function(project_db, active_project) {
  category_id <- category_description <- NULL


  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    project_db
  )
  on.exit(DBI::dbDisconnect(con))

  project_categories_df <- dplyr::tbl(con, "categories") %>%
    dplyr::filter(.data$project_id == as.integer(.env$active_project)) %>%
    dplyr::select(
      category_id,
      category_name
    ) %>%
    dplyr::collect()

  project_categories <- project_categories_df$category_id
  names(project_categories) <- project_categories_df$category_name

  return(project_categories)
}

# List categories--------------------------------------------------------


# Read categories from the DB

#' @importFrom rlang .env
#' @importFrom rlang .data
list_db_categories <- function(id, project_db, project_id) {

  ## To pass R CMD check and define DB variables as global variables for the function https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  category_id <- category_name <- category_description <- NULL


  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    project_db
  )
  on.exit(DBI::dbDisconnect(con))

  project_categories <- dplyr::tbl(con, "categories") %>%
    dplyr::filter(.data$project_id == as.integer(.env$project_id)) %>%
    dplyr::select(
      category_id,
      category_name,
      category_description
    ) %>%
    dplyr::collect() %>%
    dplyr::bind_cols(tibble::tibble("id" = id))

  return(project_categories)
}

# create category UI -----

create_new_category_UI <- function(id) {
  ns <- NS(id)
  box(
    title = "Create category",
    collapsible = TRUE,
    closable = TRUE,
    width = NULL,
    textInput(
      ns("category_name"),
      label = "Category name",
      placeholder = "Short but informative name"
    ) %>% tagAppendAttributes(class = "required"),
    textAreaInput(
      ns("category_desc"),
      label = "Category description",
      placeholder = "Description and instructions"
    ),
    actionButton(ns("category_add"),
      label = "Create"
    )
  )
}

# delete category UI -----

delete_category_UI <- function(id, project_db, active_project) {
  ns <- NS(id)
  box(
    title = "Delete category",
    collapsible = TRUE,
    closable = TRUE,
    width = NULL,
    selectInput(
      ns("categories_to_del"),
      label = "Select categories to delete",
      choices = c("", read_db_categories(
        project_db = project_db,
        active_project = active_project
      )),
      selected = "",
      multiple = TRUE
    ),
    actionButton(ns("category_remove"),
      label = "Remove",
      class = "btn-danger"
    )
  )
}

# delete category UI -----

delete_db_category <- function(project_db, active_project, user, delete_cat_id) {
  con <- DBI::dbConnect(RSQLite::SQLite(), project_db)
  on.exit(DBI::dbDisconnect(con))


  DBI::dbExecute(con,
    "DELETE from categories
                   WHERE category_id IN (?)",
    params = list(delete_cat_id)
  )
  # TODO
  # if(length(delete_cat_id)){
  #   log_delete_category_record(con, active_project, user, delete_cat_id)
  # }
}

# add category record -----

add_category_record <- function(con, project_id, user, categories_df) {
  res <- DBI::dbWriteTable(con, "categories", categories_df, append = TRUE)
  on.exit(DBI::dbDisconnect(con))
  if (res) {
    # log_add_categories_record(con, project_id, user, categories_df)
  } else {
    warning("category not added")
  }
}

# add edge record -----

add_edge_record <- function(project_db,
                            active_project,
                            user,
                            edge) {
  con <- DBI::dbConnect(RSQLite::SQLite(), project_db)
  on.exit(DBI::dbDisconnect(con))

  edge_df <- as.data.frame(edge)
  edge_df$project_id <- active_project

  res <- DBI::dbWriteTable(con, "categories_edges", edge_df, append = TRUE)
  on.exit(DBI::dbDisconnect(con))

  if (res) {
    # log_add_record_record(con, project_id, user, categories_df)
  } else {
    warning("category not added")
  }
}

# delete edge record -----

delete_db_edge <- function(project_db,
                           active_project,
                           user,
                           edge) {
  con <- DBI::dbConnect(RSQLite::SQLite(), project_db)
  on.exit(DBI::dbDisconnect(con))
  # delete edge
  # delete edges based on categories
  if (!is.null(edge$category_id)) {
  query <- glue::glue_sql("DELETE FROM categories_edges
                       WHERE category_id = {edge$category_id}
                       AND code_id = {edge$code_id}", .con = con)
  DBI::dbExecute(con, 
                 query)
  } else {
    # delete edges based on codebook
    query <- glue::glue_sql("DELETE FROM categories_edges 
                       WHERE code_id IN (?);", .con = con)
    DBI::dbExecute(con,
                   query,
                   params = list(edge$code_id))
  }

 
  # TODO
  # log_delete_edge_record(con, active_project, user, edge)
}

# render existing edges -----

render_category_edges <- function(project_db,
                                  active_project,
                                  category_id) {
  con <- DBI::dbConnect(RSQLite::SQLite(), project_db)
  on.exit(DBI::dbDisconnect(con))

  category_edges <- dplyr::tbl(con, "categories_edges") %>%
    dplyr::filter(.data$category_id == .env$category_id) %>%
    dplyr::pull(code_id)

  project_codes <- list_db_codes(
    project_db = project_db,
    project_id = active_project
  ) %>%
    dplyr::filter(code_id %in% category_edges)

  if (nrow(project_codes) == 0) {
    NULL
  } else {
    purrr::pmap(project_codes, gen_codes_ui)
  }
}
