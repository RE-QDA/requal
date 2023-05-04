# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package( "rlang" )
usethis::use_package( "shinydashboardPlus" )
usethis::use_package( "shinyFiles" )
usethis::use_package( "shinyjs" )
usethis::use_package( "colourpicker")
usethis::use_package( "DBI" )
usethis::use_package( "RSQLite" )
usethis::use_package( "dplyr" )
usethis::use_package( "tibble" )
usethis::use_package( "magrittr" )
usethis::use_package( "dbplyr" )
usethis::use_package( "purrr" )
usethis::use_package( "stringr" )
usethis::use_package( "htmlwidgets" )
usethis::use_package( "sortable" )
usethis::use_package( "shinyWidgets" )
usethis::use_package( "RPostgreSQL" )
usethis::use_package( "shinymanager" )
usethis::use_package( "pool" )


## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "launchpad_loader" ) # Name of the module
golem::add_module( name = "launchpad_creator" ) # Name of the module

# Project menu
golem::add_module( name = "project" ) # Name of the module
golem::add_module( name = "doc_manager" ) # Name of the module
golem::add_module( name = "user_manager" ) # Name of the module



# Codebook menu
golem::add_module( name = "codebook" ) # Name of the module
golem::add_module( name = "categories" ) # Name of the module


# Coding workspace
golem::add_module( name = "document_code" ) # Name of the module


# Analysis
golem::add_module( name = "analysis" ) # Name of the module
golem::add_module( name = "download_handler" ) # Name of the module
golem::add_module( name = "download_html" ) # Name of the module


golem::add_module( name = "reporting" ) # Name of the module
golem::add_module( name = "settings" ) # Name of the module
golem::add_module( name = "about" ) # Name of the module
golem::add_module( name = "user" ) # Name of the module
golem::add_module( name = "memo" ) # Name of the module
golem::add_module( name = "agreement" )
golem::add_module( name = "browser" )
golem::add_module( name = "text_stats")
golem::add_module( name = "summary" )

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct( "helpers" ) 
golem::add_utils( "project", module = "project" )
golem::add_utils( "codebook", module = "codebook" )
golem::add_utils( "categories", module = "categories" )
golem::add_utils( "document_code", module = "document_code" )
golem::add_utils( "doc_manager", module = "doc_manager" )
golem::add_utils( "analysis", module = "analysis" )
golem::add_utils( "reporting", module = "reporting" )
golem::add_utils( "user", module = "user" )
golem::add_utils( "memo", module = "memo" )
golem::add_utils( "agreement", module = "agreement" )
golem::add_utils( "text_stats", module = "agreement" )
golem::add_utils( "browser", module = "browser" )
golem::add_utils( "user_manager", module = "user_manager" )



## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "document_code_js" )
golem::add_js_file( "check_categories" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
# usethis::use_data_raw( name = "my_dataset", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
# usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("requal")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
# covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
## 
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action() 
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release() 
usethis::use_github_action_check_standard() 
usethis::use_github_action_check_full() 
# Add action for PR
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis() 
usethis::use_travis_badge() 

# AppVeyor 
usethis::use_appveyor() 
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

