# Create codebook manager UI

#' @importFrom rlang .data
create_code_UI <- function(id) {
  ns <- NS(id)
  tags$div(
    h4("Create codes"),
    textInput(
      ns("code_name"),
      label = "Code name",
      placeholder = "Short but informative name"
    ) %>% tagAppendAttributes(class = "required"),
    textAreaInput(
      ns("code_desc"),
      label = "Code description",
      placeholder = "Description and instructions"
    ),
    colourpicker::colourInput(
      ns("color_pick"),
      label = "Highlight",
      value = "yellow",
      showColour = "background",
      closeOnClick = TRUE
    ),
    actionButton(ns("code_add"),
      label = "Create"
    )
  )  %>% tagAppendAttributes(style = "text-align: left")
}

merge_code_UI <- function(id, pool, project) {
  ns <- NS(id)
  tags$div(
    h4("Merge codes"),
    selectInput(
      ns("merge_from"),
      label = "Merge from",
      choices = c("", list_db_codes(
        pool,
        project_id = project()
      ) %>%
        pair_code_id()),
      selected = "",
      multiple = FALSE
    ),
    selectInput(
      ns("merge_to"),
      label = "Merge into",
      choices = c("", list_db_codes(
        pool,
        project_id = project()
      ) %>%
        pair_code_id()),
      selected = "",
      multiple = FALSE
    ),
    actionButton(ns("code_merge"),
      label = "Merge",
      class = "btn-warning"
    )
  )  %>% tagAppendAttributes(style = "text-align: left")
}


delete_code_UI <- function(id, pool, project) {
  ns <- NS(id)
  tags$div(
    h4("Delete codes"),
    selectInput(
      ns("code_to_del"),
      label = "Select codes to delete",
      choices = list_db_codes(
        pool,
        project_id = project()
      ) %>%
        pair_code_id(),
      selected = "",
      multiple = TRUE
    ),
    actionButton(ns("code_del_btn"),
      label = "Delete",
      class = "btn-danger"
    )
  )  %>% tagAppendAttributes(style = "text-align: left")
}

# TODO
# box(
#     title = "Edit codes",
#     collapsible = TRUE,
#     collapsed = TRUE,
#     width = NULL,
#
#     selectInput(
#         ns("code_to_edit"),
#         label = "Select code to edit",
#         choices = c("", "placeholder1", "placeholder2"),
#         selected = "",
#         multiple = FALSE
#     ),
#
#     uiOutput(ns("code_editor")),
#
#     actionButton(ns("code_edit"),
#                  label = "Edit",
#                  class = "btn-warning")
#
# ),

# List codes--------------------------------------------------------


# Read codes from the DB

#' @importFrom rlang .env
#' @importFrom rlang .data
list_db_codes <- function(pool, project_id) {

  ## To pass R CMD check and define DB variables as global variables for the function https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  code_id <- code_name <- code_description <- code_color <- NULL

  project_codes <- dplyr::tbl(pool, "codes") %>%
    dplyr::filter(.data$project_id == as.integer(.env$project_id)) %>%
    dplyr::select(
      code_id,
      code_name,
      code_description,
      code_color
    ) %>%
    dplyr::collect()

  return(project_codes)
}

# Pair code names and ids -----

pair_code_id <- function(db_codes_df) {
  ids <- db_codes_df %>%
    dplyr::select(dplyr::ends_with("_id")) %>%
    dplyr::pull()

  named_ids <- db_codes_df %>%
    dplyr::select(dplyr::ends_with("_name")) %>%
    dplyr::pull()

  choices <- ids
  names(choices) <- named_ids

  return(choices)
}



# Generate boxes of codes -----
gen_codes_ui <- function(code_id,
                         code_name,
                         code_description,
                         code_color) {
  box(
    code_description,
    id = code_id,
    title = code_name,
    closable = FALSE,
    width = NULL,
    background = "light-blue",
    collapsible = TRUE,
    collapsed = TRUE,
    boxToolSize = "md",
    label = tagAppendAttributes(
      boxLabel(
        text = "code",
        status = "warning"
      ),
      style = paste0("background-color:", code_color, " !important;"),
      class = "custom_label"
    ),
    # dropdownMenu = boxDropdown(
    #     boxDropdownItem("Edit"),
    #     boxDropdownItem("Merge"),
    #     boxDropdownItem("Delete")
    # ),
    ""
  ) %>% tagAppendAttributes(
    `data-code_id` = code_id,
    class = "code_item",
    style = "max-width: 500px"
  ) 
}

# Delete codes from project ------
delete_db_codes <-
  function(pool, 
           active_project,
           delete_code_id, 
           user_id) {
    
    DBI::dbExecute(pool,
      "DELETE FROM codes
                       WHERE code_id IN (?);",
      params = list(delete_code_id)
    )

    log_delete_code_record(pool, active_project, delete_code_id, user_id)
  }


# Render codes -----

render_codes <- function(pool, active_project) {
    if (isTruthy(active_project)) {
        project_codes <- list_db_codes(
            pool = pool,
            project_id = active_project
        )
        
        if (nrow(project_codes) == 0) {
            "No codes have been created."
        } else {
            purrr::pmap(project_codes, gen_codes_ui)
        }
    } else {
        "No active project."
    }
}


# Merge codes ------

merge_codes <- function(pool,
                        active_project,
                        merge_from,
                        merge_to, 
                        user_id) {
  # should rewrite all merge from ids to the value of merge to in segments
  update_segments_sql <- glue::glue_sql("UPDATE segments
                 SET code_id = {merge_to}
                 WHERE code_id = {merge_from}", .con = pool)
  DBI::dbExecute(pool, update_segments_sql)
  
  # should delete merge from row from codes
  delete_code_sql <- glue::glue_sql("DELETE FROM codes WHERE code_id = {merge_from}",
    .con = pool
  )
  DBI::dbExecute(pool, delete_code_sql)
  
  # should log action with from-to ids
  log_merge_code_record(pool, project_id = active_project, merge_from, merge_to, user_id)
}
