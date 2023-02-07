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