library(shinytest2)

test_that("test_app works", {
    # Don't run these tests on the CRAN build servers
    skip_on_cran()
    skip_on_ci()
    # skip_if_not(interactive())
    
    appdir <- system.file("test_app", package = "requal")
    test_app(appdir, check_setup = FALSE)
})