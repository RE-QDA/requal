# read users from credentials DB ----
get_users <- function(credentials_path, credentials_pass) {
  credentials_con <- DBI::dbConnect(
    RSQLite::SQLite(),
    credentials_path
  )

  on.exit(DBI::dbDisconnect(credentials_con))

  shinymanager::read_db_decrypt(credentials_con,
    passphrase = credentials_pass
  )[c("user", "user_id")]
}

# add user permissions to project ----

add_permissions_record <- function(pool, project_id, user_id) {

new_users_df <- tibble::tibble(
  project_id = project_id,
  user_id = user_id,
  data_modify                  = 0,
  data_other_modify            = 0,
  data_other_view              = 0,
  attributes_modify            = 0,
  attributes_other_modify      = 0,
  attributes_other_view        = 0,
  codebook_modify              = 0,
  codebook_other_modify        = 0,
  codebook_other_view          = 0,
  annotation_modify            = 0,
  annotation_other_modify      = 0,
  annotation_other_view        = 0,
  analysis_other_view          = 0,
  report_other_view            = 0,
  permissions_modify           = 0
)
  res <- DBI::dbWriteTable(pool, "user_permissions", new_users_df, append = TRUE, row.names = FALSE)
  
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

    DBI::dbExecute(pool, remove_user_permissions_sql)
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
                         user_id,
                         permission,
                         value) {
    # permission_name <- switch(permission,
    # can_code = "Kódovat",
    # can_modify_codes = "ddd",
    # can_delete_codes = "df",
    # can_modify_documents = "sl",
    # can_delete_documents = "s",
    # can_manage = "d"
    # )

    ns <- NS(id)

checkboxInput(
    inputId = ns(paste(user_id, permission, sep = "_")),
    label = permission,
    value = value,
    )
     

}

# generate user permission UI

gen_users_permissions_ui <- function(nested_df, id){


purrr::pmap(list(
  user_id = nested_df$user_id_copy, 
  user_name = nested_df$user_name,
  user_data = nested_df$data
  ), 
.f = function(user_id, user_name, user_data) {
 box(
            tags$div(
              purrr::pmap(user_data,
.f = gen_permissions_ui, id = id
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
    width = "370px",
    icon = icon(icon, verify_fa = FALSE) %>% tagAppendAttributes(style = "color: #3c8dbc"), 
    right = FALSE
  ) %>% tagAppendAttributes(style = "padding-right: 5px; padding-top: 10px; top: 1vh; position: relative; min-width: 50%;")
}