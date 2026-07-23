#' launchpad_import UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_launchpad_import_ui <- function(id) {
  ns <- NS(id)

  if (golem::get_golem_options(which = "mode") == "local") {
    import_UI_local(ns)
  } else if (
    golem::get_golem_options(which = "mode") %in% c("server", "local_test")
  ) {
    import_UI_server(ns)
  }
}

#' launchpad_import Server Functions
#'
#' @noRd
mod_launchpad_import_server <- function(id, glob) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # module reactive vals ----
    loc <- reactiveValues()
    loc$db_path <- NULL
    loc$active_project <- NULL
    loc$project_directory <- NULL

    ##################
    # Local setup ####
    ##################

    observeEvent(req(golem::get_golem_options(which = "mode") == "local"), {
      # handle import button ----
      observeEvent(input$project_import, {
        req(input$import_file)

        # parse QDPX first to get project name
        parsed <- tryCatch(
          parse_qdpx(input$import_file$datapath),
          error = function(e) {
            warn_user(paste("Error parsing QDPX file:", e$message))
            NULL
          }
        )
        req(!is.null(parsed))

        # create temp db path for import
        proj_name_clean <- gsub(
          "[^a-zA-Z0-9]+",
          "",
          iconv(parsed$project$project_name, to = "ASCII//TRANSLIT")
        )
        loc$db_path <- paste0(
          tempdir(),
          .Platform$file.sep,
          paste0(proj_name_clean, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".requal")
        )

        # Store project name for download handler
        glob$active_project_name <- parsed$project$project_name

        # Create temporary SQLite connection for import (does not affect glob$pool)
        import_con <- DBI::dbConnect(RSQLite::SQLite(), loc$db_path)
        on.exit(DBI::dbDisconnect(import_con), add = TRUE)

        # Enable foreign keys
        DBI::dbExecute(import_con, "PRAGMA foreign_keys = ON;")

        glob$user$user_id <- as.integer(1)

        # create project and import
        loc$active_project <- create_project_db(
          pool = import_con,
          project_name = parsed$project$project_name,
          project_description = parsed$project$project_description,
          user_id = glob$user$user_id
        )

        # import content
        import_result <- tryCatch(
          {
            import_project(
              content = parsed,
              user_id = glob$user$user_id,
              active_project = isolate(loc$active_project),
              pool = import_con
            )
            TRUE
          },
          error = function(e) {
            warn_user(paste("Error importing project:", e$message))
            FALSE
          }
        )

        if (isTRUE(import_result)) {
          # Clear the file input using shinyjs::reset()
          shinyjs::reset("import_file")

          # Signal that a project was imported (triggers selector update in loader)
          glob$project_imported <- glob$project_imported + 1

          # Show modal with download button to save the .requal file
          showModal(
            modalDialog(
              title = h4("Import Successful"),
              paste0(
                "Project '", parsed$project$project_name,
                "' has been imported successfully."
              ),
              p(
                strong("Important:"),
                "Download the .requal file to save it to your desired location."
              ),
              downloadButton(
                ns("download_project"),
                label = "Download .requal File",
                class = "btn-primary",
                onclick = sprintf("Shiny.setInputValue('%s_closed', 1, {priority: 'event'});", ns("download"))
              ),
              footer = tagList(
                actionButton(
                  ns("close_import_modal"),
                  "Close",
                  class = "btn-default"
                )
              )
            )
          )

          # Close modal when download button is clicked
          observeEvent(input[[paste0(ns("download"), "_closed")]], {
            removeModal()
          }, ignoreInit = TRUE)
        }
      })

      # Download .requal file for local mode ----
      output$download_project <- downloadHandler(
        filename = function() {
          if (!is.null(glob$active_project_name)) {
            proj_name_clean <- gsub(
              "[^a-zA-Z0-9]+",
              "",
              iconv(glob$active_project_name, to = "ASCII//TRANSLIT")
            )
            paste0(proj_name_clean, ".requal")
          } else {
            "project.requal"
          }
        },
        content = function(file) {
          if (!is.null(loc$db_path) && file.exists(loc$db_path)) {
            file.copy(loc$db_path, file)
          }
        }
      )

      # Close import modal ----
      observeEvent(input$close_import_modal, {
        removeModal()
      })
    })

    ###################
    # Server setup ####
    ###################

    observeEvent(
      req(
        golem::get_golem_options(which = "mode") %in% c("server", "local_test")
      ),
      {
        observeEvent(input$project_import, {
          req(input$import_file)

          # require project admin privileges
          if (!isTruthy(glob$user$project_admin)) {
            warn_user(
              "Only users with project administration privileges can import projects."
            )
            req(glob$user$project_admin)
          }

          # reuse global pool
          if (!isTruthy(glob$pool)) {
            glob$pool <- pool
          }

          # parse QDPX
          parsed <- tryCatch(
            parse_qdpx(input$import_file$datapath),
            error = function(e) {
              warn_user(paste("Error parsing QDPX file:", e$message))
              NULL
            }
          )
          req(!is.null(parsed))

          # create project in postgres
          loc$active_project <- tryCatch(
            create_project_db(
              pool = glob$pool,
              project_name = parsed$project$project_name,
              project_description = parsed$project$project_description,
              user_id = glob$user$user_id
            ),
            error = function(e) {
              warn_user(paste("Error creating project:", e$message))
              NULL
            }
          )
          req(!is.null(loc$active_project))

          # import content
          import_result <- tryCatch(
            {
              import_project(
                content = parsed,
                user_id = glob$user$user_id,
                active_project = isolate(loc$active_project),
                pool = glob$pool
              )
              TRUE
            },
            error = function(e) {
              warn_user(paste("Error importing project:", e$message))
              FALSE
            }
          )

          if (isTRUE(import_result)) {
            # Clear the file input using shinyjs::reset()
            shinyjs::reset("import_file")

            # Signal that a project was imported (triggers selector update in loader)
            glob$project_imported <- glob$project_imported + 1

            # Show modal informing user they can now load the project
            showModal(
              modalDialog(
                title = h4("Import Successful"),
                paste0(
                  "Project '", parsed$project$project_name,
                  "' has been imported successfully to the server."
                ),
                p(
                  "The project is now available in the system."
                ),
                tags$ul(
                  tags$li(
                    "Go to the ",
                    strong("Load"),
                    " tab to select this project."
                  ),
                  tags$li(
                    "The project selector will be updated with the newly imported project."
                  )
                ),
                footer = tagList(
                  actionButton(
                    ns("switch_to_load"),
                    "Go to Load Tab",
                    class = "btn-primary"
                  ),
                  actionButton(
                    ns("close_server_import_modal"),
                    "Close",
                    class = "btn-default"
                  )
                )
              )
            )
          }
        })

        # Switch to load tab when requested
        observeEvent(input$switch_to_load, {
          removeModal()
          shinydashboardPlus::updateControlbarMenu(
            "launchpad",
            selected = "Load",
            session = session$rootScope()
          )
        })

        # Close server import modal
        observeEvent(input$close_server_import_modal, {
          removeModal()
        })
      }
    )

    # Note: We no longer auto-set glob$active_project after import
    # Users must explicitly load the project from the Load tab

    return(NULL)
  })
}
