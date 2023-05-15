#' project UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_project_ui <- function(id) {
  ns <- NS(id)
        tagList(
            tabsetPanel(
              id = "project_tabs",
              tabPanel(
                "Project info",
                fluidRow(class = "module_content",
       h4("Active project"),
       tags$b(textOutput(ns("project_name"))),
       h4("Project description"),
       div(textOutput(ns("project_description"))),
       h4("Created at"),
       div(
        textOutput(ns("project_date"))
       )
                ),
                value = "project_info_tab"
              ),
              tabPanel(
                "Manage users",
                mod_user_manager_ui("user_manager_1"),
                value = "user_manager_tab"
              ),
              tabPanel(
                "About",
                mod_about_ui("about_ui_1"),
                value = "about_tab"
              )
            )
        )
}

#' project Server Functions
#'
#' @noRd
mod_project_server <- function(id, glob) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    loc <- reactiveValues()

    observeEvent(glob$active_project, {
      loc$project_df <- dplyr::tbl(pool, "projects") %>%
      dplyr::filter(project_id == local(as.integer(glob$active_project)))

      output$project_name <- renderText({names(glob$active_project)})
      output$project_description <- renderText({loc$project_df %>% dplyr::pull(project_description)})
      output$project_date <- renderText({format(loc$project_df %>% dplyr::pull(created_at),
        format = "%Y-%m-%d %H:%M:%S")})


    })
    return(NULL)
  })
}
