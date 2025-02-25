
<!-- README.md is generated from README.Rmd. Please edit that file -->

# requal

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/RE-QDA/requal/workflows/R-CMD-check/badge.svg)](https://github.com/RE-QDA/requal/actions)
[![R-CMD-check](https://github.com/RE-QDA/requal/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RE-QDA/requal/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `requal` is to provide reproducibility support for
qualitative coding.

<img src="https://owncloud.cesnet.cz/index.php/s/mQcxC998RC6qwVP/download" width="100%" />

## Installation

### Prerequisites

To use `requal`, you need to have the [R
language](https://www.r-project.org) installed and running on your
machine. You can download and install R from
[here](https://cran.r-project.org/). You will also need a web browser
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

### Collaborative usage

`requal` is designed with reproducibility, openness, and transparency in
mind, and these values are most evident when it’s used collaboratively.
You can achieve this by self-hosting `requal` on your own server. A
server instance of `requal` can support multiple users and offers
fine-grained permission settings, making it easier to form diverse teams
and collaborate in real time. With `requal`, you can discuss codes,
evaluate agreement on codes, and estimate the sensitivity of coding to
the attributes of your team members.

However, setting up a server instance is not a simple task. It requires
a basic level of system administration experience. If you need
assistance, the IT department at your institution should be able to help
set up an `requal` server instance by following these
[instructions](https://github.com/RE-QDA/requal/wiki/Server-setup). To
get a sense of how `requal` operates in server mode, take a look at the
public demo available on the [requal project
website](https://requal.fsv.cuni.cz/).

> NOTE: It is crucial to prioritize **data security** when hosting your
> `requal` instance publicly over the internet. At the very least,
> enabling the TLS protocol is a necessary precaution. However, be aware
> that this measure alone will not fully protect you from knowledgeable
> attackers. Consequently, it is important that you only analyze public,
> GDPR-compliant, and non-confidential data via `requal` in server mode.
> If you intend to use `requal` for collaborative work involving
> confidential or sensitive data, ensure this is done behind adequate
> firewalls or on internal networks established in secure locations.
> Always consult with your data stewards and/or research ethics
> committee to determine the appropriate level of security control for
> your dataset. In the future, we plan to provide support for data
> de-identification. This will allow you to perform de-identification
> locally and collaboratively analyze the de-identified dataset online.

## Acknowledgement

The application development has been supported by The Technology Agency
of the Czech Republic, project n. TL05000054, and CLS INFRA Fellowship
Programme.
