credentials <- data.frame(
    user = c("admin", "test"),
    password = c("admin", "test"),
    # password will automatically be hashed
    admin = c(TRUE, FALSE),
    user_id = c(1,2),
    user_name = c("Admin adminů", "Test Testerů"),
    user_mail = c("mail@example.com", "mail@example.com"),
    project_owner = c(TRUE, TRUE),
    stringsAsFactors = FALSE
)

# Init the database
shinymanager::create_db(
    credentials_data = credentials,
    sqlite_path = "requal_users.sqlite",
    passphrase = "test"
)
