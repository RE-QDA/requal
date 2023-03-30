credentials <- data.frame(
  user = c("admin", "martin", "michal", "nina", "radim", "test"),
  password = c("Jilska1", "test", "test", "test", "test", "test"),
  # password will automatically be hashed
  admin = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
  user_id = c(1,2,3,4,5,6),
  user_name = c("Administrátor", "Martin H", "Michal S", "Nina F", "Radim H", "Testér"),
  user_mail = "mail@example.com",
  project_admin = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
  stringsAsFactors = FALSE
)
# Init the database
shinymanager::create_db(
    credentials_data = credentials,
    sqlite_path = "requal_users.sqlite",
    passphrase = "test"
)
