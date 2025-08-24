#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_about_ui <- function(id) {
  ns <- NS(id)
  # Citation info ----
  rql_citation <- utils::citation("requal")
  # Extract authors
  authors <- sapply(as.character(rql_citation$author), function(x) {
    strsplit(x, " ")
  }) # Split the author names into first and last names
  # Format the authors as "Last name, Initial."
  formatted_authors <- sapply(authors, function(x) {
    paste0(x[2], ", ", substr(x[1], 1, 1), ".")
  })
  # Combine the authors into a single string
  author_string <- paste(formatted_authors, collapse = ", ")
  year <- rql_citation$year
  title <- rql_citation$title
  version <- as.character(packageVersion("requal"))
  rql_url <- rql_citation$url
  # Construct the APA citation
  apa_citation <- paste0(
    author_string,
    " (",
    year,
    "). ",
    tags$i(title),
    ". (Version ",
    version,
    "). &lt;",
    rql_url,
    "&gt;"
  )
  # UI starts here ----
  tagList(
    h2(
      tags$a(
        href = "https://requal.app/",
        tags$img(src = "www/requal_logo.png", width = "5%"),
        target = "_blank"
      ),
      "requal CAQDAS"
    ),
    textOutput(ns("version_project")),
    textOutput(ns("version_package")),
    p(),
    p(
      "For help, consult ",
      a(
        "requal Wiki",
        href = "https://github.com/RE-QDA/requal/wiki",
        target = "_blank"
      ),
      ".",
      br(),
      "Leave feedback on our ",
      a(
        "GitHub page",
        href = "https://github.com/RE-QDA/requal/issues",
        target = "_blank"
      ),
      "."
    ),
    p(),
    p(
      "The development of this tool has been supported by:",
      tags$ul(
        tags$li(
          a(
            href = "https://www.tacr.cz/en/technology-agency-of-the-czech-republic",
            "The Technology Agency of the Czech Republic",
            target = "_blank"
          ),
          "project n. TL05000054"
        ),
        tags$li(
          a(
            href = "https://clsinfra.io/",
            "CLS INFRA Fellowship programme",
            target = "_blank"
          )
        )
      )
    ),
    p("To cite package", tags$code("requal"), "in publications use:"),
    HTML(apa_citation),
    p(),
    p("A BibTeX entry for LaTeX users:"),
    tags$pre(paste(utils::toBibtex(utils::citation("requal")), collapse = "\n"))
  )
}

#' about Server Functions
#'
#' @noRd
mod_about_server <- function(id, glob) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$version_project <- renderText({
      paste0(
        "The current project was created with requal version ",
        dplyr::tbl(glob$pool, "requal_version") %>%
          dplyr::filter(
            .data$project_id == local(as.numeric(glob$active_project))
          ) %>%
          dplyr::pull(version),
        "."
      )
    })

    output$version_package <- renderText({
      paste0(
        "The current version of requal package installed is ",
        packageVersion("requal"),
        "."
      )
    })
  })
}
