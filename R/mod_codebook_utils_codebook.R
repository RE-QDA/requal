# Create codebook manager UI

#' @importFrom rlang .data
create_code_UI <- function(ns) {
    tags$div(
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

merge_code_UI <- function(ns, pool, project, user) {
    
    req(user$data)
    
    codes <- list_db_codes(
        pool,
        project_id = project, 
        user = user
    )
    
    if(user$data$codebook_other_modify == 0){
        codes <- codes %>% 
            dplyr::filter(user_id == !!user$user_id)
    }   
    tags$div(
        selectInput(
            ns("merge_from"),
            label = "Merge from",
            choices = c("", stats::setNames(codes$code_id, codes$code_name)),
            selected = "",
            multiple = FALSE
        ),
        selectInput(
            ns("merge_to"),
            label = "Merge into",
            choices = c("", stats::setNames(codes$code_id, codes$code_name)),
            selected = "",
            multiple = FALSE
        ),
        actionButton(ns("code_merge"),
                     label = "Merge",
                     class = "btn-warning"
        )
    )  %>% tagAppendAttributes(style = "text-align: left")
}


edit_code_UI <- function(ns, pool, project, user) {
    
    req(user$data)
    
    codes <- list_db_codes(
        pool,
        project_id = project, 
        user = user
    )
    
    if(user$data$codebook_other_modify == 0){
        codes <- codes %>% 
            dplyr::filter(user_id == !!user$user_id)
    }
    tags$div(
        selectizeInput(
            ns("code_to_edit"),
            label = "Select code to edit",
            choices = c("", stats::setNames(codes$code_id, codes$code_name)),
            selected = NULL,
            multiple = FALSE,
            options = list(
                closeAfterSelect = "true"
            )
        ),
        textInput(
            ns("edit_code_name"),
            label = "Code name"
        ) %>% tagAppendAttributes(class = "required"),
        textAreaInput(
            ns("edit_code_desc"),
            label = "Code description"
                ),
        colourpicker::colourInput(
            ns("edit_color_pick"),
            label = "Highlight",
            value = "white",
            showColour = "background",
            closeOnClick = TRUE
        ),
        actionButton(ns("code_edit_btn"),
                     label = "Edit",
                     class = "btn-warning"
        )
    )  %>% tagAppendAttributes(style = "text-align: left")
}

delete_code_UI <- function(ns, pool, project, user) {
    
    req(user$data)
    
    codes <- list_db_codes(
        pool,
        project_id = project, 
        user = user
    )
    
    if(user$data$codebook_other_modify == 0){
        codes <- codes %>% 
            dplyr::filter(user_id == !!user$user_id)
    }
    tags$div(
        selectizeInput(
            ns("code_to_del"),
            label = "Select codes to delete",
            choices = stats::setNames(codes$code_id, codes$code_name),
            selected = NULL,
            multiple = FALSE,
            options = list(
                closeAfterSelect = "true"
            )
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
list_db_codes <- function(pool, project_id, user) {
    
    ## To pass R CMD check and define DB variables as global variables for the function https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
    code_id <- code_name <- code_description <- code_color <- NULL
    
    project_codes <- dplyr::tbl(pool, "codes") %>%
        dplyr::filter(.data$project_id == as.integer(.env$project_id)) %>%
        dplyr::select(
            code_id,
            code_name,
            code_description,
            code_color, 
            user_id
        ) %>%
        dplyr::collect()
    
    if(!is.null(user$data) && user$data$codebook_other_view == 0){
        project_codes <- project_codes %>% 
            dplyr::filter(user_id == user$user_id)
    }
    
    return(project_codes)
}




# Generate boxes of codes -----
gen_codes_ui <- function(code_id,
                         code_name,
                         code_description,
                         code_color, 
                         user_id) {
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
                   glue::glue_sql("DELETE from codes
                   WHERE code_id IN ({delete_code_id})",
                   .con = pool)
                   )

        
        log_delete_code_record(pool, active_project, delete_code_id, user_id)
    }
# Delete codes from segments table 


delete_codes_segment_db <- function(pool, 
                                    active_project,
                                    user_id,
                                    code_id) {
    
    # delete code from a segment
    query <- glue::glue_sql("DELETE FROM segments
                       WHERE project_id = {active_project}
                       AND code_id = {code_id}", 
                       .con = pool)
    
    purrr::walk(query, function(x) {DBI::dbExecute(pool, x)})
    
   #todo log_delete_segment_record(pool, project_id = active_project, segment_id, user_id)
}

# Render codes -----

render_codes <- function(pool, active_project, user) {
    if (isTruthy(active_project)) {
        project_codes <- list_db_codes(
            pool = pool,
            project_id = active_project, 
            user = user
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
    delete_code_category_sql <- glue::glue_sql("DELETE FROM categories_codes_map WHERE code_id = {merge_from}",
                                      .con = pool
    )
    DBI::dbExecute(pool, delete_code_category_sql)

    delete_code_sql <- glue::glue_sql("DELETE FROM codes WHERE code_id = {merge_from}",
                                      .con = pool
    )
    DBI::dbExecute(pool, delete_code_sql)
    
    # should log action with from-to ids
    log_merge_code_record(pool, project_id = active_project, merge_from, merge_to, user_id)
}

# convert color to hex code
convert_colour_to_hex <- function(color){
    col_num <- as.numeric(stringr::str_extract_all(color, "[0-9]{1,3}", simplify = TRUE))
    grDevices::rgb(red = col_num[1], green = col_num[2], blue = col_num[3], maxColorValue = 255)
}

# prepare data.frame with codes and categories to export
get_codebook_export_table <- function(glob){
    categories <- dplyr::tbl(glob$pool, "categories") %>% 
        dplyr::filter(project_id == as.numeric(!!glob$active_project)) %>% 
        dplyr::select(category_id, category_name, category_description) %>% 
        dplyr::collect() %>% 
        dplyr::mutate(category_title = dplyr::if_else(
            !is.na(category_description) & category_description != "",
            paste0(category_name, " (", category_description, ")"), 
            category_name)) %>% 
        dplyr::select(-c(category_name, category_description))
    
    categories_map <- dplyr::tbl(glob$pool, "categories_codes_map") %>% 
        dplyr::collect() %>% 
        dplyr::inner_join(categories, by = "category_id") 
    
    dplyr::left_join(glob$codebook, categories_map, by = "code_id") %>% 
        dplyr::group_by(code_id, code_name, code_description, code_color) %>% 
        dplyr::summarise(categories = paste0(category_title, collapse = " | ")) %>%
        dplyr::mutate(code_color = purrr::map_chr(code_color, convert_colour_to_hex))
}

# Edit codes ------

edit_db_codes <- function(pool,
                        active_project,
                        user_id,
                        edit_code_id,
                        edit_code_name,
                        edit_code_description,
                        edit_code_color) {

    update_code_sql <- glue::glue_sql("UPDATE codes
                 SET code_name = {edit_code_name}, code_description = {edit_code_description}, code_color = {edit_code_color}
                 WHERE code_id = {edit_code_id}", .con = pool)
    DBI::dbExecute(pool, update_code_sql)
    
    log_edit_code_record(pool, project_id = active_project, 
                         changes = list(
                             code_id = edit_code_id, 
                             code_name = edit_code_name, 
                             code_color = edit_code_color,
                             code_description = edit_code_description), 
                         user_id)

    rql_message(paste("Code", edit_code_name, "was updated."))
}
