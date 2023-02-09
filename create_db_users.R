credentials <- data.frame(
    user = c("admin", "test"),
    password = c("admin", "test"),
    # password will automatically be hashed
    admin = c(TRUE, FALSE),
    user_id = c(1,2),
    stringsAsFactors = FALSE
)

# Init the database
shinymanager::create_db(
    credentials_data = credentials,
    sqlite_path = "requal_users.sqlite",
    passphrase = "test"
)
