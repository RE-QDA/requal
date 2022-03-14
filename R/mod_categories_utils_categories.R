# Generate boxes of categories -----
gen_categories_ui <- function(id,
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
      labels = NULL,
      class = "category-rank-list",
      css_id = glue::glue(ns("rank-list_{category_id}")),
      options = sortable::sortable_options(
        group = list(
          name = "categories",
          pull = TRUE,
          put = TRUE 
        ),
        onAdd =  htmlwidgets::JS("function (evt) {check_categories(evt, this.el);}")
      )
    )  
  ),
  class = "category-container", # extra div to capture category id
  `data-category_id` = category_id)
  
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

# new category UI -----

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
    
    actionButton(ns("code_add"),
                 label = "Create")
  )
  
}