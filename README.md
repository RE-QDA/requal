
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

### Prerequisites

To use `requal`, you need to have the [R
language](https://www.r-project.org) installed and running on your
machine. You can download and install R from
[here](https://cloud.r-project.org)). You will also need a web browser
with JavaScript support (i.e. any of the standard issue browsers like
Firefox, Chrome, or Safari).

### Package

You can install the development version of `requal` from
[GitHub](https://github.com/) by following these steps (assuming you
have R installed on your machine :

1.  download the current release of \`requal\`\` from
    [Releases](https://github.com/RE-QDA/requal/releases).
2.  install `remotes` package by running `install.packages("remotes")`
    command in R console.
3.  install `requal` package by running
    `remotes::install_local(file.choose())` and selecting the downloaded
    `requal` release file on your hard-drive.

## Usage

The `requal` `shiny` application can be launched with the following
command:

``` r
requal::run_app(options = list("launch.browser"))
```

The application runs in your browser and provides basic functionality
for annotating texts with codes. The annotated text segments can be
filtered and exported. More functions are coming up.

## Acknowledgement

The application development has been supported by The Technology Agency
of the Czech Republic, project n. TL05000054.
