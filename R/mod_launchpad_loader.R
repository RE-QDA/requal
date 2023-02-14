#' launchpad_loader UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_launchpad_loader_ui <- function(id) {
  ns <- NS(id)

  if (golem::get_golem_options(which = "mode") == "local") {
    loader_UI_local(ns)
  } else if (golem::get_golem_options(which = "mode") == "server") {
    loader_UI_server(ns)
  }
}
    
#' launchpad_loader Server Functions
#'
#' @noRd 
mod_launchpad_loader_server <- function(id, glob, setup) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # module reactive vals ----
    loc <- reactiveValues()
    loc$db_path <- NULL
    loc$active_project <- NULL
    loc$doc_list <- NULL
    loc$project <- ""

    ##################
    # Local setup ####
    ##################
    observeEvent(req(golem::get_golem_options(which = "mode") == "local"), {


      # file system prep ----
      volumes <- c(Home = fs::path_home(), get_volume_paths())


      shinyFiles::shinyFileChoose(
        input,
        "sel_file_load",
        roots = volumes,
        defaultRoot = "Home",
        session = session,
        restrictions = system.file(package = "base"),
        pattern = c("\\.requal")
      )

      observeEvent(input$sel_file_load, {
        loc$project_file_load <- normalizePath(shinyFiles::parseFilePaths(volumes, input$sel_file_load)$datapath)

        if (length(loc$project_file_load > 0)) {
          glob$pool <- pool::dbPool(
            drv = RSQLite::SQLite(),
            dbname = loc$project_file_load
          )

          reactive({
            onStop(function() {
              pool::poolClose(glob$pool)
            })
          })

        update_db_schema(glob$pool)


          updateSelectInput(session,
            "project_selector_load",
            choices = read_project_db(glob$pool,
              project_id = NULL
            )
          )
        }
      })


      output$project_path_load <- renderText({
        if (is.integer(input$sel_file_load)) {
          "No project file has been selected."
        } else {
          loc$project_file_load
        }
      })


      observeEvent(input$project_load, {
        if (!isTruthy(input$project_selector_load)) {
          warn_user("No project to load.")
        }

        req(input$project_selector_load)

        glob$user$user_id <- as.integer(1)

        loc$active_project <- isolate(
          read_project_db(
            pool = glob$pool,
            project_id = input$project_selector_load
          )
        )
      })
    })

    ###################
    # Server setup ####
    ###################

    observeEvent(req(golem::get_golem_options(which = "mode") == "server"), {

      # close start-up pool

      # pool::poolClose(pool)

      # create glob$pool if it was not launched previously
      if (is.null(glob$active_project)) {
        glob$pool <- pool::dbPool(
          drv = RPostgreSQL::PostgreSQL(),
          host = golem::get_golem_options(which = "dbhost"),
          port = golem::get_golem_options(which = "dbport"),
          dbname = golem::get_golem_options(which = "dbname"),
          user = golem::get_golem_options(which = "dbusername"),
          password = golem::get_golem_options(which = "dbpassword")
        )

        reactive({
          onStop(function() {
            pool::poolClose(glob$pool)
          })
        })

        update_db_schema(glob$pool)
      }
      
      observeEvent(glob$pool, {
        updateSelectInput(session,
                          "project_selector_load",
                          choices = read_project_db(glob$pool,
                                                    project_id = NULL
                          )
        )
      })

      observeEvent(input$project_load, {
        if (!isTruthy(input$project_selector_load)) {
          warn_user("No project to load.")
        }

        req(input$project_selector_load)

        # user control

        existing_user_id <- dplyr::tbl(glob$pool, "users") %>%
          dplyr::pull(user_id)

        if (!(glob$user$user_id %in% existing_user_id)) {
          # create user in db if an uknown user logs in
          users_df <- data.frame(
            user_id = glob$user$user_id,
            user_name = glob$user$name,
            user_mail = glob$user$mail
          )
          DBI::dbWriteTable(pool, "users", users_df,
            append = TRUE, row.names = FALSE
          )

          create_default_user(pool, input$project_selector_load, glob$user$user_id)
        }

        loc$active_project <- isolate(
          read_project_db(
            pool = glob$pool,
            project_id = input$project_selector_load
          )
        )
      })
    })

    # observe newly created projects
    observeEvent(glob$active_project, {
      updateSelectInput(session,
        "project_selector_load",
        choices = read_project_db(glob$pool,
          project_id = NULL
        )
      )
    })



    ####################
    # General setup ####
    ####################

    # set active project from load ----



    observeEvent(loc$active_project, {
      glob$active_project <- loc$active_project
    })
  })
}
