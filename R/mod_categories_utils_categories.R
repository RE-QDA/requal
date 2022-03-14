# Generate boxes of categories -----
gen_categories_ui <- function(id,
                              category_id,
                              category_name,
                              category_description) {
  ns <- NS(id)

  box(
    category_description,
    id = category_id,
    title = category_name,
    closable = FALSE,
    width = NULL,
    background = "light-blue",
    collapsible = TRUE,
    collapsed = TRUE,
    label = boxLabel(
      text = "category",
      status = "warning"
    ),
    sortable::rank_list(
      input_id = glue::glue(ns("category_list_{category_id}")),
      text = NULL,
      labels = NULL,
      class = "category-rank-list",
      options = sortable::sortable_options(
        group = list(
          name = "categories",
          pull = TRUE,
          put = TRUE 
        ),
        onAdd =  htmlwidgets::JS("function (evt) { var x = evt.item.getElementsByClassName('code_item').item(0).getAttribute('data-id'); console.log(x); console.log(evt.item.getElementsByClassName('code_item').item(0).closest('.category-rank-list'))}")
        #onAdd =  htmlwidgets::JS("function (evt) { console.log(this.el.getAttribute('data-id')); }")
      )
    )
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


# List codes--------------------------------------------------------


# Read codes from the DB

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
