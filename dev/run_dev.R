# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

(run_app(
  mode = "server",
  dbname = "requal",
  dbhost = "localhost",
  dbusername = "requal_admin",
  dbpassword = "test",
  credentials_path = "requal_users.sqlite",
  credentials_pass = "test",
  options = list("launch.browser")
))

# (run_app(
#  mode = "local",
#  options = list("launch.browser")
# ))

# (run_app(
#   mode = "local_test",
#   dbname = "tests/test.requal",
#   # dbhost = "localhost",
#   # dbusername = "requal_admin",
#   # dbpassword = "test",
#   # credentials_path = "requal_users.sqlite",
#   # credentials_pass = "test",
#   options = list("launch.browser")
# ))
