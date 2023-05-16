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
        tagList(
          fluidRow(
            class = "module_tools",
            div(mod_rql_button_ui(ns("project_edit_tool"),
              label = "Edit project",
              icon = "pencil",
              inputId = ns("project_edit_menu")
            )) %>% tagAppendAttributes(style = "padding-right: 25px;"),
            mod_rql_button_ui(ns("project_delete_tool"),
              label = "Delete project",
              icon = "trash",
              inputId = ns("project_delete_menu")
            )
          ),
          fluidRow(
            class = "module_content",
            h4("Active project"),
            div(tags$b(
              textOutput(ns("project_name"))
            )),
            h4("Project description"),
            div(
              textOutput(ns("project_description"))
            ),
            h4("Created at"),
            div(
              textOutput(ns("project_date"))
            )
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
    loc$edit_observer <- 0
      output$project_name <- renderText({
        loc$project_name
      })
      output$project_description <- renderText({
        loc$project_description
      })
      output$project_date <- renderText({
        loc$project_date
      })
    observeEvent(glob$active_project, {
      loc$project_df <- dplyr::tbl(pool, "projects") %>%
        dplyr::filter(project_id == local(as.integer(glob$active_project)))
    })

    observeEvent(loc$project_df, {
      loc$project_name <- loc$project_df %>% dplyr::pull(project_name)
      loc$project_description <- loc$project_df %>% dplyr::pull(project_description)
      loc$project_date <- format(loc$project_df %>% dplyr::pull(created_at),
        format = "%Y-%m-%d %H:%M:%S"
      )
    })

    # Project edit UI ----

    mod_rql_button_server(
      id = "project_edit_tool",
      custom_title = "Edit project",
      custom_tagList = tagList(
        textInput(ns("project_edit_name"), "Project name", value = loc$project_name),
        textAreaInput(ns("project_edit_description"), "Project description", value = loc$project_description),
        actionButton(ns("project_edit_save"), "Save")
      ),
      glob,
      permission = "project_owner"
    )

    observeEvent(input$project_edit_save, {
      active_project <- as.integer(local(glob$active_project))

      if (input$project_edit_name != loc$project_name) {
        res <- DBI::dbExecute(
        glob$pool,
        glue::glue_sql("UPDATE projects
                 SET project_name = {input$project_edit_name}
                 WHERE project_id = {active_project}", .con = glob$pool)
        )
        updateTextInput(session = session,
        "project_edit_name",
        value = input$project_edit_name
        )
      }
      if (input$project_edit_description != loc$project_description) {
        res <- DBI::dbExecute(
        glob$pool,
        glue::glue_sql("UPDATE projects
                 SET project_description = {input$project_edit_description}
                 WHERE project_id = {active_project}", .con = glob$pool)
        )
               updateTextInput(session = session,
        "project_edit_description",
        value = input$project_edit_description
        )
      }

      loc$project_df <- dplyr::tbl(pool, "projects") %>%
        dplyr::filter(project_id == local(as.integer(glob$active_project)))
      shinyjs::hide()
      showNotification("Changes to project were saved.")

    })

    # Project delete UI ----
    mod_rql_button_server(
      id = "project_delete_tool",
      custom_title = "Delete project",
      custom_tagList = tagList(
        actionButton(ns("project_edit_delete"), "Delete project", class = "btn-danger")
      ),
      glob,
      permission = "project_owner"
    )

    observeEvent(input$project_edit_delete, {
      showModal(
        modalDialog(
          title = "Are you sure?",
          paste(
            "You are about to delete the project",
            loc$project_name,
            "and lose all data associated with it!"
          ),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Dismiss"),
            actionButton(ns("confirmation"),
              "Yes, I am sure.",
              class = "btn-danger"
            )
          ),
          fade = TRUE
        )
      )
    })

    observeEvent(input$confirmation, {
      res <- DBI::dbExecute(
        glob$pool,
        glue::glue_sql("DELETE from projects
                   WHERE project_id IN ({glob$active_project})",
          .con = glob$pool
        )
      )
      session$reload()
    })

    return(NULL)
  })
}
