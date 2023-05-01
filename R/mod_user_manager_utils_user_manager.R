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
    attributes_modify            = 1,
    attributes_other_modify      = 0,
    attributes_other_view        = 0,
    codebook_modify              = 0,
    codebook_other_modify        = 0,
    codebook_other_view          = 1,
    annotation_modify            = 1,
    annotation_other_modify      = 0,
    annotation_other_view        = 0,
    analysis_other_view          = 0,
    report_other_view            = 0,
    permissions_modify           = 0,
    project_admin                = 0
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
  permissions_df <- permissions_df %>%
      dplyr::select(-c(project_admin, user_login, user_name, created_at, user_mail, project_id)) %>%
      tidyr::pivot_longer(-user_id, 
      names_to = "permission",
      values_to = "value"
      )
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
      label = translate_permissions(permission),
      value = value,
    ) 
  }else{
    disabled_checkbox(
      inputId = ns(paste(user_id, permission, sep = "_")),
      label = translate_permissions(permission),
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
    h4("Add users to project"),
    rql_picker_UI(ns("rql_users"),
                label = "Select users"
    ),
    rql_button_UI(ns("assign"), "Add users") %>% 
      tagAppendAttributes(style = "text-align: left;")
  )
}

# remove_user_UI ----
remove_user_UI <- function(id) {
  ns <- NS(id)
  tags$div(
    h4("Remove users from project"),
   rql_picker_UI(ns("members_to_remove"), "Select users:", none = "Users to remove"),
   rql_button_UI(ns("remove_members"), "Remove users")
  )
}

# modify_permissions_UI ----
modify_permissions_UI <- function(id) {
  ns <- NS(id)
  tags$div(
    h4("Modify user permissions for project"),
    rql_picker_UI(
      ns("members_permissions"), 
      "Select users:",
      none = "Users to change permissions"),
    checkboxGroupInput(
      ns("permissions_list"),
      label = NULL,
      choices = stats::setNames(
          permission(),
          translate_permissions(permission())
          )
    ),
    rql_button_UI(ns("save_permissions"), "Save permissions")
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

permission <- function() {
  c(
"data_modify"          ,  
"data_other_modify"      ,
"data_other_view"        ,
"attributes_modify"      ,
"attributes_other_modify",
"attributes_other_view"  ,
"codebook_modify"        ,
"codebook_other_modify"  ,
"codebook_other_view"    ,
"annotation_modify"      ,
"annotation_other_modify",
"annotation_other_view"  ,
"analysis_other_view"    ,
"report_other_view"      ,
"permissions_modify"     
)
}
# translate DB names to natural language

translate_permissions <- function(permission){

dplyr::case_when( 
    permission == "data_modify"             ~ "Data: Modify own"
  , permission == "data_other_modify"       ~ "Data: Modify others"
  , permission == "data_other_view"         ~ "Data: View others"
  , permission == "attributes_modify"       ~ "Attributes: Modify own"
  , permission == "attributes_other_modify" ~ "Attributes: Modify others"  
  , permission == "attributes_other_view"   ~ "Attributes: View others"
  , permission == "codebook_modify"         ~ "Codes and categories: Modify own"
  , permission == "codebook_other_modify"   ~ "Codes and categories: Modify others"  
  , permission == "codebook_other_view"     ~ "Codes and categories: View others"  
  , permission == "annotation_modify"       ~ "Annotations: Modify own"
  , permission == "annotation_other_modify" ~ "Annotations: Modify others"      
  , permission == "annotation_other_view"   ~ "Annotations: View others"      
  , permission == "analysis_other_view"     ~ "Analysis: View others"    
  , permission == "report_other_view"       ~ "Report: View others"  
  , permission == "permissions_modify"      ~ "Permissions: Modify"  
)

}
