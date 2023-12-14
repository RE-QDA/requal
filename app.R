# For testing purposes only
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = TRUE)

file.copy(from = "tests/test_basic_backup.requal", 
          to = "tests/test_basic.requal",
          overwrite = TRUE)

requal::run_app(
    mode = "local_test",
    dbname = "tests/test_basic.requal",
    user = "test",
    options = list("launch.browser")
)

