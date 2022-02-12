# Create codebook manager UI

#' @importFrom rlang .data
codebook_manager_UI <- function(id, project_db, project_id) {
    ns <- NS(id)
    tagList(
        box(
            title = "Create codes",
            collapsible = TRUE,
            width = NULL,
            
            textInput(
                ns("code_name"),
                label = NULL,
                placeholder = "Code name"
            ),
            
            textAreaInput(
                ns("code_desc"),
                label = NULL,
                placeholder = "Code description"
            ),
            
            colourpicker::colourInput(
              ns("color_pick"),
              label = NULL,
              value = "yellow",
              closeOnClick = TRUE
            ),
            
            actionButton(ns("code_add"),
                         label = "Create",
                         class = "btn-success")
        ),
        box(
            title = "Edit codes",
            collapsible = TRUE,
            collapsed = TRUE,
            width = NULL,
            
            selectInput(
                ns("code_to_edit"),
                label = "Select code to edit",
                choices = c("", "placeholder1", "placeholder2"),
                selected = "",
                multiple = FALSE
            ),
            
            uiOutput(ns("code_editor")),
            
            actionButton(ns("code_edit"),
                         label = "Edit",
                         class = "btn-warning")
            
        ),
        
        box(
            title = "Merge codes",
            collapsible = TRUE,
            collapsed = TRUE,
            width = NULL,
            
            selectInput(
                ns("code_merge_from"),
                label = "Select codes to merge",
                choices = c("placeholder1", "placeholder2"),
                selected = "",
                multiple = TRUE
            ),
            
            selectInput(
                ns("code_merge_to"),
                label = "Select codes to merge",
                choices = c("placeholder1", "placeholder2"),
                selected = "",
                multiple = TRUE
            ),
            
            actionButton(ns("code_merge"),
                         label = "Merge",
                         class = "btn-warning")
            
        ),
        
        box(
            title = "Delete codes",
            collapsible = TRUE,
            collapsed = TRUE,
            width = NULL,
            
            selectInput(
                ns("code_to_del"),
                label = "Select codes to delete",
                choices = list_db_codes(project_db = project_db,
                                        project_id = project_id) %>% 
                    pair_code_id(),
                selected = "",
                multiple = TRUE
            ),
            
            actionButton(ns("code_del_btn"),
                         label = "Delete",
                         class = "btn-danger")
            
        )
        
    )
}

#List codes--------------------------------------------------------


# Read codes from the DB

#' @importFrom rlang .env
#' @importFrom rlang .data
list_db_codes <- function(project_db, project_id) {
  
## To pass R CMD check and define DB variables as global variables for the function https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  code_id <- code_name <- code_description <- code_color <- NULL

  
    con <- DBI::dbConnect(RSQLite::SQLite(),
                          project_db)
    on.exit(DBI::dbDisconnect(con))
    
    project_codes <- dplyr::tbl(con, "codes") %>%
        dplyr::filter(.data$project_id == as.integer(.env$project_id)) %>%
        dplyr::select(code_id, 
                      code_name, 
                      code_description,
                      code_color) %>%
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
        width = 12,
        background = "light-blue",
        collapsible = TRUE,
        collapsed = TRUE,
        boxToolSize = "md",
        label = tagAppendAttributes(
          boxLabel(text = "code",
                   status = "warning"),
          style = paste0("background-color:", code_color," !important;"),
          class = "custom_label"),
        dropdownMenu = boxDropdown(
            boxDropdownItem("Edit"),
            boxDropdownItem("Merge"),
            boxDropdownItem("Delete")
        ),
        ""
    )
    
}

# Delete codes from project ------
delete_db_codes <-
    function(project_db,
             active_project,
             delete_code_id) {
  
        con <- DBI::dbConnect(RSQLite::SQLite(), project_db)
        on.exit(DBI::dbDisconnect(con))
        
        DBI::dbExecute(con,
                       "DELETE FROM codes 
                       WHERE code_id IN (?);",
                       params = list(delete_code_id))
        
        #log_delete_code_record(con, active_project, delete_code_id)
    }


# Render codes -----

render_codes <- function(active_project,
                         project_db) {

    if (isTruthy(active_project)) {
        project_codes <- list_db_codes(project_db = project_db,
                                       project_id = active_project)
        
        
        if (nrow(project_codes) == 0) {
            "No codes have been created."
            
        } else {
            purrr::pmap(project_codes, gen_codes_ui)
            
        }
        
    } else {
        "No active project."
    }
}

