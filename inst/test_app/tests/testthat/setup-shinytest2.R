# Load application support files into testing environment
# For testing in RStudio
# if(FALSE) shinytest2::load_app_env()
devtools::load_all(pkgload::pkg_path())

# For CMD check use
# shinytest2::load_app_env("inst/test_app")

