# Generate boxes of categories -----
gen_categories_ui <- function(id,
                              pool,
                              active_project,
                              category_id,
                              category_name,
                              category_description, 
                              user) {
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
                    pool, 
                    active_project = active_project,
                    category_id = category_id, 
                    user = user
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
        `data-category_id` = category_id,
        style = "max-width: 500px"
    )
}


render_codes_ui <- function(id, pool, active_project, user){
    ns <- NS(id)
    sortable::rank_list(
        input_id = ns("code_list"),
        text = NULL,
        labels = render_codes(
            pool = pool,
            active_project = active_project, 
            user = user
        ),
        class = "codes-rank-list",
        options = sortable::sortable_options(
            sort = TRUE,
            group = list(
                name = "categories",
                pull = "clone",
                put = TRUE
            ),
            onAdd = htmlwidgets::JS("function (evt) {  this.el.removeChild(evt.item); }")
        )
    )
}

# Render categories -----
render_categories <- function(id, pool, 
                              active_project, 
                              user) {
    if (isTruthy(active_project)) {
        project_categories <- list_db_categories(
            id = id,
            pool, 
            project_id = active_project
        )
    
        if(!is.null(user) && user$data$codebook_other_view == 0){
            project_categories <- project_categories %>% 
                dplyr::filter(user_id == !!user$user_id)
        }
        
        if (nrow(project_categories) == 0) {
            "No categories have been created."
        } else {
            purrr::transpose(project_categories) %>% 
                purrr::map(., ~gen_categories_ui(
                    id = id, pool = pool, 
                    active_project = active_project, 
                    category_id = .x$category_id, 
                    category_name = .x$category_name, 
                    category_description = .x$category_description, 
                    user = user)
                )
        }
    } else {
        "No active project."
    }
}

# Read categories--------------------------------------------------------
read_db_categories <- function(pool, active_project, user) {
    category_id <- category_description <- category_name <- NULL
    
    project_categories_df <- dplyr::tbl(pool, "categories") %>%
        dplyr::filter(.data$project_id == as.integer(.env$active_project)) %>%
        dplyr::select(
            category_id,
            category_name, 
            user_id
        ) %>%
        dplyr::collect()
    
    if(!is.null(user) && 
       !is.null(user$data) && 
       user$data$codebook_other_modify == 0){
        project_categories_df <- project_categories_df %>% 
            dplyr::filter(user_id == !!user$user_id)
    }
    
    project_categories <- project_categories_df$category_id
    names(project_categories) <- project_categories_df$category_name
    
    return(project_categories)
}

read_category_edges <- function(pool, active_project){
    dplyr::tbl(pool, "categories_codes_map") %>%
        dplyr::filter(.data$project_id == as.integer(.env$active_project)) %>%
        dplyr::select(
            category_id,
            code_id
        ) %>%
        dplyr::collect()
}

# List categories--------------------------------------------------------


# Read categories from the DB

#' @importFrom rlang .env
#' @importFrom rlang .data
list_db_categories <- function(id, pool, project_id) {
    
    ## To pass R CMD check and define DB variables as global variables for the function https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
    category_id <- category_name <- category_description <- NULL
    
    project_categories <- dplyr::tbl(pool, "categories") %>%
        dplyr::filter(.data$project_id == as.integer(.env$project_id)) %>%
        dplyr::select(
            category_id,
            category_name,
            category_description, 
            user_id
        ) %>%
        dplyr::collect() %>%
        dplyr::bind_cols(tibble::tibble("id" = id))
    
    return(project_categories)
}

# create category UI -----

create_new_category_UI <- function(ns) {
    tags$div(
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
    ) %>% tagAppendAttributes(style = "text-align: left")
}

# delete category UI -----

delete_category_UI <- function(ns, pool, active_project, user) {
    req(user)
    categories <- read_db_categories(
        pool, active_project = active_project, 
        user = user
    )
    tags$div(
        selectizeInput(
            ns("categories_to_del"),
            label = "Select categories to delete",
            choices = c("", categories),
            selected = NULL,
            multiple = TRUE,
            options = list(
                closeAfterSelect = "true"
            )
        ),
        actionButton(ns("category_remove"),
                     label = "Remove",
                     class = "btn-danger"
        )
    ) %>% tagAppendAttributes(style = "text-align: left")
}

# delete category  -----

delete_db_category <- function(pool, active_project, user_id, delete_cat_id) {
    res <- DBI::dbExecute(pool,
                   glue::glue_sql("DELETE from categories
                   WHERE category_id IN ({delete_cat_id})",
                   .con = pool)
                   )
    
    if(res & length(delete_cat_id)){
        log_delete_category_record(pool, active_project, delete_cat_id, user_id)
    }
}

# add category record -----
add_category_record <- function(pool, project_id, user_id, categories_df) {
    res <- DBI::dbWriteTable(pool, "categories", categories_df, append = TRUE, row.names = FALSE)
    
    if (res) {
        written_category_id <- dplyr::tbl(pool, "categories") %>%
            dplyr::filter(.data$category_name == !!categories_df$category_name) %>%
            dplyr::pull(.data$category_id)
        log_add_category_record(pool, project_id, categories_df %>%
                                    dplyr::mutate(category_id = written_category_id), 
                                user_id = user_id)
    } else {
        warning("category not added")
    }
    
}

# add code to category -----
add_category_code_record <- function(pool,
                                     active_project,
                                     user_id,
                                     edge) {
    edge_df <- as.data.frame(edge)
    edge_df$project_id <- active_project
    
    res <- DBI::dbWriteTable(pool, "categories_codes_map", edge_df, append = TRUE, row.names = FALSE)
    
    if (res) {
        log_add_category_code_record(pool, active_project, edge_df, user_id)
    } else {
        warning("category not added")
    }
    
}

# delete edge record -----
delete_category_code_record <- function(pool,
                                        active_project,
                                        edge, 
                                        user_id) {
    # delete edge
    # delete edges based on codes and categories
    if (!is.null(edge$category_id) & !is.null(edge$code_id) ) {
        res <- DBI::dbExecute(pool,
                              glue::glue_sql("DELETE FROM categories_codes_map
                       WHERE category_id = {edge$category_id}
                       AND code_id = {edge$code_id};", .con = pool))
    } else if (!is.null(edge$category_id) & is.null(edge$code_id) ) {
        # delete edges based on categories
        res <- DBI::dbExecute(pool,
                   glue::glue_sql("DELETE from categories_codes_map
                   WHERE category_id IN ({edge$category_id})",
                   .con = pool)
                   )
   
    } else {
        # delete edges based on codebook
        res <- DBI::dbExecute(pool,
                   glue::glue_sql("DELETE from categories_codes_map
                   WHERE code_id IN ({edge$code_id})",
                   .con = pool)
                   )
    }        
    
    if(res){
        log_delete_category_code_record(pool, active_project, edge, user_id)
    }
}

# render existing edges -----

render_category_edges <- function(pool,
                                  active_project,
                                  category_id, 
                                  user) {
    category_edges <- dplyr::tbl(pool, "categories_codes_map") %>%
        dplyr::filter(.data$category_id == .env$category_id) %>%
        dplyr::pull(code_id)
    
    project_codes <- list_db_codes(
        pool, 
        project_id = active_project, 
        user = user
    ) %>%
        dplyr::filter(code_id %in% category_edges)
    
    if (nrow(project_codes) == 0) {
        NULL
    } else {
        purrr::pmap(project_codes, gen_codes_ui)
    }
}
