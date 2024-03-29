
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ReQual

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/RE-QDA/requal/workflows/R-CMD-check/badge.svg)](https://github.com/RE-QDA/requal/actions)
<!-- badges: end -->

The goal of `requal` is to provide reproducibility support for
qualitative coding.

<img src="https://owncloud.cesnet.cz/index.php/s/mQcxC998RC6qwVP/download" width="100%" />

## Installation

### Prerequisites

To use `requal`, you need to have the [R
language](https://www.r-project.org) installed and running on your
machine. You can download and install R from
[here](https://cloud.r-project.org)). You will also need a web browser
with JavaScript support (i.e. any of the standard issue browsers like
Firefox, Chrome, or Safari).

### Package

You can install `requal` from [GitHub](https://github.com/) by following
these steps (assuming you have R installed on your machine):

1.  Download the current release of `requal` from
    [Releases](https://github.com/RE-QDA/requal/releases).
2.  Install the `remotes` package by running
    `install.packages("remotes")` command in R console.
3.  Install the `requal` package by running
    `remotes::install_local(file.choose())` and selecting the downloaded
    release file on your hard-drive.

#### Development version

The development version contains the latest features, but might be less
stable than a [released
version](https://github.com/RE-QDA/requal/releases).

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
project by providing a name and selecting a folder for your project. The
app will create an SQLite database file with the `.requal` extension and
start up the project.

`requal` is a `shiny` application that runs in your browser and provides
basic functionality for annotating documents in plain text with
user-defined codes. The annotated text segments can be filtered and
exported. More functions are coming up.

## Acknowledgement

The application development has been supported by The Technology Agency
of the Czech Republic, project n. TL05000054.
