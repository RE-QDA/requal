
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ReQual

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/RE-QDA/requal/workflows/R-CMD-check/badge.svg)](https://github.com/RE-QDA/requal/actions)
<!-- badges: end -->

The goal of `requal` is to provide reproducibility support for
qualitative coding.

⚠️**The package is in development and in experimental stage.**⚠️

## Installation

### Prerequisites

To use `requal`, you need to have the [R
language](https://www.r-project.org) installed and running on your
machine. You can download and install R from
[here](https://cloud.r-project.org)). You will also need a web browser
with JavaScript support (i.e. any of the standard issue browsers like
Firefox, Chrome, or Safari).

### Package

You can install the development version of `requal` from
[GitHub](https://github.com/) by running this code in R:

    install.packages("devtools")
    devtools::install_github("RE-QDA/requal")

## Usage

The `requal` app can be launched from the R console with the following
command:

``` r
requal::run_app(options = list("launch.browser"))
```

On the first launch of the app, use the “Create” menu to set up a new
project by providing a name a selecting a folder for your project. The
app will create an SQLite database file with the `.requal` extension and
start up the project.

`requal` is a `shiny` application that runs in your browser and provides
basic functionality for annotating documents in plain text with
user-defined codes. The annotated text segments can be filtered and
exported. More functions are coming up.

## Acknowledgement

The application development has been supported by The Technology Agency
of the Czech Republic, project n. TL05000054.
