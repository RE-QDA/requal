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
    import_UI_local(id)
  } else if (
    golem::get_golem_options(which = "mode") %in% c("server", "local_test")
  ) {
    import_UI_server(id)
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
    # Single source of truth for file state - maintained by file observer
    loc$file_input <- NULL  # NULL or file path; button checks this directly

    ##################
    # Local setup ####
    ##################

    # Render progress UI
    output$import_progress_ui <- renderUI({
      if (isTRUE(loc$importing)) {
        tagList(
          br(),
          div(class = "progress", style = "height: 20px;",
            div(id = ns("import_progress_bar"), class = "progress-bar progress-bar-striped active",
              role = "progressbar", style = "width: 0%; height: 100%;",
              `aria-valuenow` = "0", `aria-valuemin` = "0", `aria-valuemax` = "100"
            )
          ),
          p(id = ns("import_progress_text"), "Preparing import...")
        )
      } else {
        NULL
      }
    })

    # Observer: Maintain loc$file_input based on actual file input state
    # This is the single source of truth for the button handler
    observeEvent(input$import_file, {
      if (isTruthy(input$import_file)) {
        # File selected - store the path
        loc$file_input <- input$import_file$datapath
      } else {
        # File cleared (reset or JS rejection) - clear our stored value
        loc$file_input <- NULL
      }
    })

    # Observer to handle file size violation from JavaScript
    # When JS rejects a file, reset the UI and clear our stored file path
    observeEvent(input$file_too_large, {
      req(input$file_too_large)
      info <- input$file_too_large
      showModal(
        modalDialog(
          title = "File too large",
          div(
            p(paste("The selected file is too large.")),
            p(paste0("File size: ", info$sizeMb, " MB")),
            p(paste0("Maximum allowed size: ", info$maxMb, " MB")),
            p("Please select a smaller file.")
          ),
          footer = modalButton("Close"),
          easyClose = TRUE,
          size = "m"
        )
      )
      # Reset the file input UI
      shinyjs::reset("import_file")
      # Clear our stored file path (file observer will also fire but input is already NULL)
      loc$file_input <- NULL
      # Clear the custom input so it can be triggered again
      golem::invoke_js("Shiny.setInputValue", list(name = "file_too_large", value = NULL, priority = "event"))
    })

    observeEvent(req(golem::get_golem_options(which = "mode") == "local"), {
      # handle import button ----
      observeEvent(input$project_import, {
        # Check: is there a file in our stored state?
        if (!isTruthy(loc$file_input)) {
          showModal(
            modalDialog(
              title = "No file selected",
              p("Please select a file to import before clicking the Import button."),
              footer = modalButton("Close"),
              easyClose = TRUE
            )
          )
          return()
        }

        # File exists - capture path and clear state BEFORE processing
        file_path <- loc$file_input
        loc$file_input <- NULL  # Clear so next click requires new selection
        shinyjs::reset("import_file")  # Reset UI

        # Set importing flag to show progress UI
        loc$importing <- TRUE
        on.exit(loc$importing <- FALSE, add = TRUE)

        # Show progress modal
        showModal(modalDialog(
          title = "Importing Project",
          div(
            class = "progress", style = "height: 30px; margin-bottom: 15px;",
            div(id = ns("modal_progress_bar"), class = "progress-bar progress-bar-striped active",
              role = "progressbar", style = "width: 0%; height: 100%;",
              `aria-valuenow` = "0", `aria-valuemin` = "0", `aria-valuemax` = "100"
            )
          ),
          p(id = ns("modal_progress_text"), "Preparing..."),
          size = "m",
          easyClose = FALSE,
          footer = NULL
        ))

        # parse QDPX first to get project name (use captured file_path)
        parsed <- tryCatch(
          parse_qdpx(file_path),
          error = function(e) {
            removeModal()
            warn_user(paste("Error parsing QDPX file:", conditionMessage(e)))
            warn_user(paste("Error class:", class(e)[1]))
            NULL
          }
        )
        req(!is.null(parsed))
        removeModal()

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

        # Create temporary SQLite pool for import (does not affect glob$pool)
        import_pool <- pool::dbPool(
          drv = RSQLite::SQLite(),
          dbname = loc$db_path,
          onCreate = function(con) {
            DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")
          }
        )
        on.exit(pool::poolClose(import_pool), add = TRUE)

        # Use a local user_id for import (do not set glob$user$user_id to avoid triggering loader observers)
        local_user_id <- as.integer(1)

        # create project and import
        loc$active_project <- create_project_db(
          pool = import_pool,
          project_name = parsed$project$project_name,
          project_description = parsed$project$project_description,
          user_id = local_user_id
        )

        # import content
        import_result <- tryCatch(
          {
            import_project(
              content = parsed,
              user_id = local_user_id,
              active_project = isolate(loc$active_project),
              pool = import_pool
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
          # Check: is there a file in our stored state?
          if (!isTruthy(loc$file_input)) {
            showModal(
              modalDialog(
                title = "No file selected",
                p("Please select a file to import before clicking the Import button."),
                footer = modalButton("Close"),
                easyClose = TRUE
              )
            )
            return()
          }

          # File exists - capture path and clear state BEFORE processing
          file_path <- loc$file_input
          loc$file_input <- NULL
          shinyjs::reset("import_file")

          # require project admin privileges
          if (!isTruthy(glob$user$project_admin)) {
            warn_user(
              "Only users with project administration privileges can import projects."
            )
            req(glob$user$project_admin)
          }

          # Set importing flag to show progress UI
          loc$importing <- TRUE
          on.exit(loc$importing <- FALSE, add = TRUE)

          # Show progress modal
          showModal(modalDialog(
            title = "Importing Project",
            div(
              class = "progress", style = "height: 30px; margin-bottom: 15px;",
              div(id = ns("server_modal_progress_bar"), class = "progress-bar progress-bar-striped active",
                role = "progressbar", style = "width: 0%; height: 100%;",
                `aria-valuenow` = "0", `aria-valuemin` = "0", `aria-valuemax` = "100"
              )
            ),
            p(id = ns("server_modal_progress_text"), "Preparing..."),
            size = "m",
            easyClose = FALSE,
            footer = NULL
          ))

          # reuse global pool
          if (!isTruthy(glob$pool)) {
            glob$pool <- pool
          }

          # parse QDPX (use captured file_path)
          parsed <- tryCatch(
            parse_qdpx(file_path),
            error = function(e) {
              removeModal()
              warn_user(paste("Error parsing QDPX file:", e$message))
              NULL
            }
          )
          req(!is.null(parsed))
          removeModal()

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
