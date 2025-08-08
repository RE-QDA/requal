library(shinytest2)


skip_on_check <- function() {
    if (identical(Sys.getenv("NOT_CRAN"), "true")) {
        skip("Skipping test during package checks")
    }
}

test_that("test_app works", {
    # Don't run these tests on the CRAN build servers
    skip_on_cran()
    skip_on_ci()
    skip_on_check()
    
    appdir <- system.file("test_app", package = "requal")
    test_app(appdir, check_setup = FALSE)
})