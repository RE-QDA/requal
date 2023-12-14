library(shinytest2)

test_that("test_app works", {
    # Don't run these tests on the CRAN build servers
    skip_on_cran()
    
    appdir <- system.file(package = "requal", "test_app")
    test_app(appdir)
})