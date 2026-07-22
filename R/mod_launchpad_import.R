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
      # file system prep -----
      volumes <- c(Home = fs::path_home(), get_volume_paths())

      shinyFiles::shinyDirChoose(
        input,
        "sel_directory",
        roots = volumes,
        defaultRoot = "Home",
        session = session,
        restrictions = system.file(package = "base"),
        allowDirCreate = TRUE
      )

      output$project_path <- renderText({
        if (is.integer(input$sel_directory)) {
          "No destination directory has been selected."
        } else {
          loc$project_directory
        }
      })

      observeEvent(input$sel_directory, {
        loc$project_directory <- normalizePath(shinyFiles::parseDirPath(
          volumes,
          input$sel_directory
        ))
      })

      # handle import button ----
      observeEvent(input$project_import, {
        req(input$import_file)
        req(loc$project_directory)

        # parse QDPX first to get project name
        parsed <- tryCatch(
          parse_qdpx(input$import_file$datapath),
          error = function(e) {
            warn_user(paste("Error parsing QDPX file:", e$message))
            NULL
          }
        )
        req(!is.null(parsed))

        # build db path from parsed project name
        proj_name_clean <- gsub(
          "[^a-zA-Z0-9]+",
          "",
          iconv(parsed$project$project_name, to = "ASCII//TRANSLIT")
        )
        loc$db_path <- paste0(
          loc$project_directory,
          .Platform$file.sep,
          paste0(proj_name_clean, ".requal")
        )

        # check for existing file
        if (file.exists(loc$db_path)) {
          warn_user(
            paste(
              "Project file already exists at:",
              loc$db_path,
              "Choose a different folder or delete the existing file."
            )
          )
          return(NULL)
        }

        # create new SQLite pool
        glob$pool <- pool::dbPool(
          drv = RSQLite::SQLite(),
          dbname = loc$db_path,
          onCreate = function(con) {
            DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")
          }
        )
        glob$user$user_id <- as.integer(1)

        # create project and import
        loc$active_project <- create_project_db(
          pool = glob$pool,
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
                "To permanently save this project, download the .requal file to your desired location."
              ),
              downloadButton(
                ns("download_project"),
                label = "Download .requal File",
                class = "btn-primary"
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
        }
      })

      # Download .requal file for local mode ----
      output$download_project <- downloadHandler(
        filename = function() {
          if (!is.null(loc$db_path)) {
            basename(loc$db_path)
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
