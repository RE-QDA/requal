
<!-- README.md is generated from README.Rmd. Please edit that file -->

# requal

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/RE-QDA/requal/workflows/R-CMD-check/badge.svg)](https://github.com/RE-QDA/requal/actions)
<!-- badges: end -->

The goal of `requal` is to provide reproducibility support for
qualitative coding.

## Installation

You can install the development version of `requal` from
[GitHub](https://github.com/) by following these steps:

1.  download the current release from [this
    link](https://github.com/RE-QDA/requal/releases).
2.  install `remotes` package by running `install.packages("remotes")`
    in R console.
3.  install `requal` package by running
    `remotes::install_local("PATH_TO/requal_0.0.0.9000.tar.gz")` where
    `PATH_TO` needs to be replaced by a path to the `.tar.gz` file (such
    as `~Downloads/requal_0.0.0.9000.tar.gz` or
    `C:/Users/michael.skvrnak/Downloads/requal_0.0.0.9000.tar.gz` on
    Windows)

## Running the app

The shiny application for coding documents can be run by the following
command.

``` r
requal::run_app()
```
