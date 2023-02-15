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

add_permissions_record <- function(pool, project_id, codes_df, user_id) {
  res <- DBI::dbWriteTable(pool, "codes", codes_df, append = TRUE, row.names = FALSE)
  if (res) {
    written_code_id <- dplyr::tbl(pool, "codes") %>%
      dplyr::filter(.data$code_name == !!codes_df$code_name &
        .data$project_id == project_id) %>%
      dplyr::pull(code_id)
    log_add_code_record(pool, project_id, codes_df %>%
      dplyr::mutate(code_id = written_code_id),
    user_id = user_id
    )
  } else {
    warning("code not added")
  }
}

# modify permissions for project
modify_permissions_record <- function(pool, project_id, permissions_df) {

      update_user_permissions_sql <- glue::glue_sql("UPDATE user_permissions
                 SET {`permissions_df$permission`} = {permissions_df$value}
                 WHERE user_id = {permissions_df$user_id}
                 AND project_id = {project_id};", .con = pool)

    purrr::map(update_user_permissions_sql, ~DBI::dbExecute(pool, .x))
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


purrr::map2(nested_df$user_id_copy, nested_df$data, 
.f = function(x,y) {
 box(
            tags$div(
              purrr::pmap(y,
.f = gen_permissions_ui, id = id
)
            ),
            id = x,
            title = x,
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