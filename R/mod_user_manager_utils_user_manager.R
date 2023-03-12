disabled_checkbox <- function(inputId, label, value = FALSE, width = NULL){
  value <- shiny::restoreInput(id = inputId, default = value)
  inputTag <- tags$input(id = inputId, type = "checkbox", disabled = "disabled")
  if (!is.null(value) && value) 
    inputTag$attribs$checked <- "checked"
  shiny::div(class = "form-group shiny-input-container", 
             style = htmltools::css(width = shiny::validateCssUnit(width)), 
      shiny::div(class = "checkbox", 
                 tags$label(inputTag, tags$span(label))))
}

# read users from credentials DB ----
get_users <- function(credentials_path, credentials_pass) {
  credentials_con <- DBI::dbConnect(
    RSQLite::SQLite(),
    credentials_path
  )
  
  on.exit(DBI::dbDisconnect(credentials_con))
  
  shinymanager::read_db_decrypt(credentials_con,
                                passphrase = credentials_pass
  ) %>% 
    dplyr::select(user_id, 
                  user_login = user,
                  user_name,
                  user_mail)
}

get_user_permissions <- function(pool, project_id) {
  
  # get permissions of users for current project
  users_assigned_df <- dplyr::tbl(pool, "user_permissions") %>%
    dplyr::filter(project_id == !!as.integer(project_id)) %>%
    dplyr::collect()
  
  # get details of users for current project
  users_details_df <- dplyr::tbl(pool, "users") %>%
    dplyr::filter(user_id %in% !!users_assigned_df$user_id) %>%
    dplyr::collect()
  
  # join details and permissions of users for current project
  users_permissions_df <- dplyr::inner_join(
    users_assigned_df,
    users_details_df,
    by = "user_id"
  )
}

# add user permissions to project ----

add_permissions_record <- function(pool, project_id, user_id) {
  
  new_users_df <- tibble::tibble(
    project_id = project_id,
    user_id = user_id,
    data_modify                  = 0,
    data_other_modify            = 0,
    data_other_view              = 1,
    attributes_modify            = 0,
    attributes_other_modify      = 0,
    attributes_other_view        = 1,
    codebook_modify              = 1,
    codebook_other_modify        = 0,
    codebook_other_view          = 1,
    annotation_modify            = 1,
    annotation_other_modify      = 0,
    annotation_other_view        = 0,
    analysis_other_view          = 1,
    report_other_view            = 0,
    permissions_modify           = 0
  )
  res <- DBI::dbWriteTable(pool, "user_permissions", new_users_df, 
                           append = TRUE, row.names = FALSE)
  
  if (res) {
    #TODO
    # log
  }
}


# remove user permissions from project ----

remove_permissions_record <- function(pool, project_id, user_id) {
  
  remove_user_permissions_sql <- glue::glue_sql("DELETE FROM user_permissions
                 WHERE user_id IN ({user_id})
                 AND project_id = {project_id};", .con = pool)
  
  purrr::map(remove_user_permissions_sql, ~DBI::dbExecute(pool, .x))
  #TODO
  # log
}

# modify permissions for project
modify_permissions_record <- function(pool, project_id, permissions_df) {
  
  update_user_permissions_sql <- glue::glue_sql("UPDATE user_permissions
                 SET {`permissions_df$permission`} = {permissions_df$value}
                 WHERE user_id = {permissions_df$user_id}
                 AND project_id = {project_id};", .con = pool)
  
  purrr::map(update_user_permissions_sql, ~DBI::dbExecute(pool, .x))
  #TODO
  # log
}

# Generate user permissions UI -----
gen_permissions_ui <- function(id, 
                               user_permissions, 
                               user_id,
                               permission,
                               value) {
  ns <- NS(id)
  
  if(user_permissions$permissions_modify == 1){
    checkboxInput(
      inputId = ns(paste(user_id, permission, sep = "_")),
      label = permission,
      value = value,
    ) 
  }else{
    disabled_checkbox(
      inputId = ns(paste(user_id, permission, sep = "_")),
      label = permission,
      value = value
    )
  }
  
}

# generate user permission UI

gen_users_permissions_ui <- function(nested_df, id, user_permissions){
  
  purrr::pmap(list(
    user_id = nested_df$user_id_copy, 
    user_name = nested_df$user_name,
    user_data = nested_df$data
  ), 
  .f = function(user_id, user_name, user_data) {
    box(
      tags$div(
        purrr::pmap(user_data, 
                    .f = gen_permissions_ui, id = id, user_permissions = user_permissions
        )
      ),
      id = user_id,
      title = user_name,
      closable = FALSE,
      width = NULL,
      collapsible = TRUE,
      collapsed = TRUE,
      label = NULL
    ) %>% tagAppendAttributes(
      style = "max-width: 500px"
    )
  }
  
  )
  
}

# add_user_UI ----
add_user_UI <- function(id) {
  ns <- NS(id)
  tags$div(
    h4("Add user to project"),
    selectInput(ns("rql_users"),
                label = "Assign selected users to project",
                choices = "",
                multiple = TRUE
    ),
    actionButton(ns("assign"), "Assign") %>% 
      tagAppendAttributes(style = "text-align: left")
  )
}

# remove_user_UI ----
remove_user_UI <- function(id) {
  ns <- NS(id)
  tags$div(
    h4("Remove user from project"),
    selectInput(ns("members_to_remove"),
                label = "Remove selected users from project",
                choices = "",
                multiple = TRUE
    ),
    actionButton(ns("remove_members"), "Remove") %>% 
      tagAppendAttributes(style = "text-align: left")
  )
}

# menu button 2 ----
menu_btn2 <- function(..., label, icon) {
  
  shinyWidgets::dropdown(
    ...,
    label = NULL,
    style = "material-circle",
    tooltip = shinyWidgets::tooltipOptions(
      placement = "right",
      title = label,
      html = FALSE
    ),
    size = "md", 
    width = "400px",
    icon = icon(icon, verify_fa = FALSE) %>% tagAppendAttributes(style = "color: #3c8dbc"), 
    right = FALSE
  ) %>% tagAppendAttributes(style = "padding-right: 5px; padding-top: 10px; top: 1vh; position: relative; min-width: 50%;")
}

