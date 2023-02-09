# read users from credentials DB
get_users <- function() {
  credentials_con <- DBI::dbConnect(
    RSQLite::SQLite(),
    golem::get_golem_options(which = "credentials_path")
  )

  on.exit(DBI::dbDisconnect(credentials_con))

  shinymanager::read_db_decrypt(credentials_con,
    passphrase = golem::get_golem_options(which = "credentials_pass")
  )[c("user", "user_id")]
}


# Generate user permissions UI -----
gen_permissions_ui <- function(user_id,
                         permission,
                         initial_value) {
    # permission_name <- switch(permission,
    # can_code = "Kódovat",
    # can_modify_codes = "ddd",
    # can_delete_codes = "df",
    # can_modify_documents = "sl",
    # can_delete_documents = "s",
    # can_manage = "d"
    # )
   
    checkboxGroupInput(
        inputId = as.character(user_id),
    label = as.character(user_id),
    choiceNames = unname(permission),
    choiceValues = unname(initial_value),
    inline = TRUE
    ) %>% tagAppendAttributes(
        `data-user_id` = user_id
    ) 

}
