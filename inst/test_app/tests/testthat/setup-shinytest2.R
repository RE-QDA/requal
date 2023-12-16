# Load application support files into testing environment
if(FALSE) shinytest2::load_app_env()
devtools::load_all(pkgload::pkg_path())
