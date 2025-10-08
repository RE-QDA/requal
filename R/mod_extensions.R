#' extensions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_extensions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(
      "
      .description-text {
        overflow: hidden;
        max-height: 1.2em;
        line-height: 1.2em;
        white-space: nowrap;
        text-overflow: ellipsis;
        cursor: pointer;
        transition: all 0.3s ease;
      }
      .description-text:hover {
        background-color: #f8f9fa;
      }
      .description-text.expanded {
        max-height: none;
        white-space: normal;
        background-color: #f8f9fa;
        padding: 8px;
        border-radius: 4px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        z-index: 10;
        position: relative;
      }
    "
    ),
    tabsetPanel(
      type = "tabs",
      id = ns("extensions_tabset"),
      tabPanel(
        "Modules manager",
        id = ns("extensions_manager"),
        value = "extensions_manager",
        fluidRow(
          column(
            12,
            h4("Available Extensions"),
            div(
              id = ns("search_container"),
              class = "mb-3",
              textInput(
                ns("search_extensions"),
                label = NULL,
                placeholder = "Search extensions...",
                width = "300px"
              )
            ),
            uiOutput(ns("extensions_cards"))
          )
        )
      )
      # Extensions will be dynamically added here via updateTabsetPanel
    )
  )
}

#' extensions Server Functions
#'
#' @noRd
mod_extensions_server <- function(id, glob, api) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    observe(print(api))

    extensions_data <- reactiveValues(
      available = NULL,
      launched = c() # Track which extensions are launched
    )

    # Discover extensions on module load
    observe({
      if (is.null(extensions_data$available)) {
        extensions_data$available <- tools::dependsOnPkgs("requal")
      }

      # Show/hide search input based on available extensions
      if (length(extensions_data$available) > 0) {
        shinyjs::show("search_container")
      } else {
        shinyjs::hide("search_container")
      }
    })

    # Render extension cards with search filtering
    output$extensions_cards <- renderUI({
      if (is.null(extensions_data$available)) {
        return(div(
          style = "text-align: center; padding: 40px; color: #666;",
          icon(
            "spinner",
            class = "fa-spin",
            style = "font-size: 32px; margin-bottom: 10px;"
          ),
          br(),
          "Loading available extensions..."
        ))
      }

      if (length(extensions_data$available) == 0) {
        return(div(
          class = "text-center",
          style = "padding: 60px 20px;",
          div(
            icon(
              "puzzle-piece",
              style = "font-size: 64px; color: #ccc; margin-bottom: 20px;"
            )
          ),
          h4(
            "No Extensions Found",
            style = "color: #666; margin-bottom: 15px;"
          ),
          p(
            "No extension packages were found that depend on 'requal'.",
            style = "color: #888; margin-bottom: 25px; font-size: 16px;"
          ),
          p(
            "Visit ",
            tags$a(
              href = "https://www.requal.app",
              target = "_blank",
              "www.requal.app"
            ),
            " to learn more about available extensions and installation instructions.",
            style = "color: #666; margin-bottom: 20px;"
          )
        ))
      }

      # Get search term
      search_term <- input$search_extensions

      # Filter extensions based on search
      filtered_extensions <- extensions_data$available
      if (!is.null(search_term) && nchar(trimws(search_term)) > 0) {
        search_lower <- tolower(trimws(search_term))
        filtered_extensions <- Filter(
          function(ext) {
            pkg_info <- tryCatch(
              {
                desc <- packageDescription(ext)
                title <- if (!is.null(desc$Title) && desc$Title != "NA") {
                  desc$Title
                } else {
                  ext
                }
                # Search in package name and title
                grepl(search_lower, tolower(ext), fixed = TRUE) ||
                  grepl(search_lower, tolower(title), fixed = TRUE)
              },
              error = function(e) {
                # If error getting description, just search package name
                grepl(search_lower, tolower(ext), fixed = TRUE)
              }
            )
          },
          extensions_data$available
        )
      }

      # Show message if no results
      if (length(filtered_extensions) == 0) {
        return(div(
          style = "text-align: center; padding: 40px; color: #999;",
          icon("search", style = "font-size: 48px; margin-bottom: 10px;"),
          br(),
          paste("No extensions found matching '", search_term, "'")
        ))
      }

      # Create cards for filtered extensions
      cards <- lapply(filtered_extensions, function(ext) {
        # Get package description info
        pkg_info <- tryCatch(
          {
            desc <- packageDescription(ext)
            list(
              title = if (!is.null(desc$Title) && desc$Title != "NA") {
                desc$Title
              } else {
                ext
              },
              description = if (
                !is.null(desc$Description) && desc$Description != "NA"
              ) {
                desc$Description
              } else {
                "Extension package for requal"
              },
              author = if (!is.null(desc$Author) && desc$Author != "NA") {
                desc$Author
              } else {
                "Unknown"
              },
              version = if (!is.null(desc$Version) && desc$Version != "NA") {
                desc$Version
              } else {
                "Unknown"
              },
              license = if (!is.null(desc$License) && desc$License != "NA") {
                desc$License
              } else {
                "Unknown"
              }
            )
          },
          error = function(e) {
            list(
              title = ext,
              description = "Extension package for requal",
              author = "Unknown",
              version = "Unknown",
              license = "Unknown"
            )
          }
        )

        # Try to find package logo
        logo_element <- tryCatch(
          {
            pkg_path <- system.file(package = ext)

            # Recursively search for logo files
            logo_files <- c()

            # Search in man directory recursively
            man_dir <- file.path(pkg_path, "man")
            if (dir.exists(man_dir)) {
              logo_files <- c(
                logo_files,
                list.files(
                  man_dir,
                  pattern = "^logo\\.(png|jpg|jpeg|svg)$",
                  recursive = TRUE,
                  full.names = TRUE,
                  ignore.case = TRUE
                )
              )
            }

            # Search in inst directory recursively
            inst_dir <- file.path(pkg_path, "inst")
            if (dir.exists(inst_dir)) {
              logo_files <- c(
                logo_files,
                list.files(
                  inst_dir,
                  pattern = "^logo\\.(png|jpg|jpeg|svg)$",
                  recursive = TRUE,
                  full.names = TRUE,
                  ignore.case = TRUE
                )
              )
            }

            # Also check root directory
            root_logos <- list.files(
              pkg_path,
              pattern = "^logo\\.(png|jpg|jpeg|svg)$",
              full.names = TRUE,
              ignore.case = TRUE
            )
            logo_files <- c(logo_files, root_logos)

            # Use first found logo
            if (length(logo_files) > 0) {
              existing_logo <- logo_files[1]
              print(paste("Found logo for", ext, "at:", existing_logo))

              # Convert to base64 for embedding
              logo_data <- base64enc::base64encode(existing_logo)
              tags$img(
                src = paste0("data:image/png;base64,", logo_data),
                style = "width: 48px; height: 48px; object-fit: contain;",
                alt = paste("Logo for", ext)
              )
            } else {
              # Fallback to puzzle icon - same size as logos
              icon(
                "puzzle-piece",
                style = "font-size: 48px; width: 48px; height: 48px; color: #007bff;"
              )
            }
          },
          error = function(e) {
            print(paste("Error finding logo for", ext, ":", e$message))
            # Fallback to puzzle icon on any error - same size as logos
            icon(
              "puzzle-piece",
              style = "font-size: 48px; width: 48px; height: 48px; color: #007bff;"
            )
          }
        )

        # Check if extension is launched
        is_launched <- ext %in% extensions_data$launched

        div(
          class = "col-md-4 col-sm-6 mb-3",
          div(
            class = "card h-100",
            div(
              class = "card-body d-flex flex-column",
              div(
                class = "d-flex align-items-center mb-3",
                div(
                  class = "me-3",
                  logo_element
                ),
                h5(
                  class = "card-title mb-0",
                  pkg_info$title
                )
              ),
              h6(
                class = "card-subtitle mb-2 text-muted",
                paste("Package:", ext, "| License:", pkg_info$license)
              ),
              p(
                class = "card-text flex-fill description-text",
                onclick = "this.classList.toggle('expanded')",
                # Always show full text - CSS will handle truncation and expansion
                pkg_info$description
              ),
              p(
                class = "text-muted mb-2 small",
                paste(
                  "Version:",
                  pkg_info$version,
                  "| Author:",
                  if (nchar(pkg_info$author) > 30) {
                    paste0(substr(pkg_info$author, 1, 27), "...")
                  } else {
                    pkg_info$author
                  }
                )
              ),
              if (is_launched) {
                div(
                  class = "mt-auto",
                  div(
                    class = "btn-group w-100",
                    actionButton(
                      ns(paste0("goto_", ext)),
                      "Go to",
                      class = "btn-success btn-sm",
                      onclick = paste0(
                        "Shiny.setInputValue('",
                        ns("goto_extension"),
                        "', '",
                        ext,
                        "', {priority: 'event'});"
                      )
                    ),
                    actionButton(
                      ns(paste0("close_", ext)),
                      "Close",
                      class = "btn-outline-danger btn-sm",
                      onclick = paste0(
                        "Shiny.setInputValue('",
                        ns("close_extension"),
                        "', '",
                        ext,
                        "', {priority: 'event'});"
                      )
                    )
                  )
                )
              } else {
                actionButton(
                  ns(paste0("launch_", ext)),
                  "Launch",
                  class = "btn-primary btn-sm mt-auto",
                  onclick = paste0(
                    "Shiny.setInputValue('",
                    ns("launch_extension"),
                    "', '",
                    ext,
                    "', {priority: 'event'});"
                  )
                )
              }
            )
          )
        )
      })

      div(
        class = "row",
        cards
      )
    })

    # Handle launch extension events
    observeEvent(input$launch_extension, {
      ext <- input$launch_extension
      print(paste("Launching extension:", ext))

      extension_id <- paste0("ext_", ext, "_", as.integer(Sys.time()))

      tryCatch(
        {
          if (requireNamespace(ext, quietly = TRUE)) {
            # Using eval and parse for dynamic namespace calls
            ui_call <- paste0(ext, "::mod_ui")
            ui_func <- eval(parse(text = ui_call))

            if (is.function(ui_func)) {
              # Check for custom display name in DESCRIPTION
              display_name <- tryCatch(
                {
                  desc <- packageDescription(ext)
                  # Look for Config/requal/displayName field
                  config_field <- desc[["Config/requal/displayName"]]
                  if (!is.null(config_field) && config_field != "NA") {
                    config_field
                  } else {
                    ext # fallback to package name
                  }
                },
                error = function(e) {
                  ext # fallback to package name on error
                }
              )

              # Add the tab to the existing tabsetPanel
              appendTab(
                inputId = "extensions_tabset",
                tab = tabPanel(
                  title = tagList(icon("puzzle-piece"), display_name),
                  value = paste0(ext, "_tab"),
                  ui_func(ns(extension_id))
                ),
                session = session
              )

              # Add to launched extensions
              extensions_data$launched <- c(extensions_data$launched, ext)

              showNotification(
                paste("Loaded extension:", ext),
                type = "message"
              )

              # Initialize server function
              server_call <- paste0(ext, "::mod_server")
              server_func <- eval(parse(text = server_call))

              if (is.function(server_func)) {
                server_func(extension_id, api = api)
                print(paste("Server function initialized for", ext))
              }
            } else {
              print(paste("mod_ui is not a function in", ext))
              showNotification(
                paste("Error: mod_ui not found in", ext),
                type = "error"
              )
            }
          } else {
            print(paste("Could not load namespace for", ext))
            showNotification(
              paste("Could not load extension:", ext),
              type = "error"
            )
          }
        },
        error = function(e) {
          print(paste("Error loading extension", ext, ":", e$message))
          showNotification(
            paste("Error loading", ext, ":", e$message),
            type = "error"
          )
        }
      )
    })

    # Handle goto extension events
    observeEvent(input$goto_extension, {
      ext <- input$goto_extension
      updateTabsetPanel(
        session,
        "extensions_tabset",
        selected = paste0(ext, "_tab")
      )
    })

    # Handle close extension events
    observeEvent(input$close_extension, {
      ext <- input$close_extension
      print(paste("Closing extension:", ext))

      tryCatch(
        {
          # Remove the tab
          removeTab(
            inputId = "extensions_tabset",
            target = paste0(ext, "_tab"),
            session = session
          )

          # Remove from launched extensions
          extensions_data$launched <- extensions_data$launched[
            extensions_data$launched != ext
          ]

          showNotification(
            paste("Closed extension:", ext),
            type = "message"
          )

          # Switch back to modules manager tab
          updateTabsetPanel(
            session,
            "extensions_tabset",
            selected = "extensions_manager"
          )
        },
        error = function(e) {
          print(paste("Error closing extension", ext, ":", e$message))
          showNotification(
            paste("Error closing", ext, ":", e$message),
            type = "error"
          )
        }
      )
    })
  })
}
