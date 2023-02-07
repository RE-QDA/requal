#' launchpad_creator UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_launchpad_creator_ui <- function(id) {
  ns <- NS(id)

  if (golem::get_golem_options(which = "mode") == "local") {
    creator_UI_local(ns)
  } else if (golem::get_golem_options(which = "mode") == "server") {
    creator_UI_server(ns)
  }
}

#' launchpad_creator Server Functions
#'
#' @noRd
mod_launchpad_creator_server <- function(id, glob, setup) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    # module reactive vals ----

    loc <- reactiveValues()
    loc$db_path <- NULL
    loc$active_project <- NULL
    loc$doc_list <- NULL


    ####################
    # General setup ####
    ####################



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
          "No project directory has been selected."
        } else {
          loc$project_directory
        }
      })

      observeEvent(input$sel_directory, {
        loc$project_directory <- normalizePath(shinyFiles::parseDirPath(volumes, input$sel_directory))
      })


      # set active project from create ----
      observeEvent(input$project_create, {
        req(input$project_name, input$sel_directory)

        loc$db_path <- paste0(
          loc$project_directory,
          .Platform$file.sep,
          paste0(gsub(
            "[^a-zA-Z0-9]+",
            "",
            iconv(input$project_name,
              to = "ASCII//TRANSLIT"
            )
          ), ".requal")
        )


        glob$pool <- pool::dbPool(
          drv = RSQLite::SQLite(),
          dbname = loc$db_path
        )


        glob$user$user_id <- as.integer(1)

        loc$active_project <- create_project_db(
          pool = glob$pool,
          project_name = input$project_name,
          project_description = input$project_description,
          user_id = glob$user$user_id
        )
      names(loc$active_project) <- input$project_name

      })
    })

    ###################
    # Server setup ####
    ###################

    observeEvent(req(golem::get_golem_options(which = "mode") == "server"), {
      observeEvent(input$project_create, {
        req(input$project_name)

        if (!isTruthy(glob$active_project)) {
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
        }
        # user control ----
        
        existing_user_id <- dplyr::tbl(glob$pool, "users") %>%
          dplyr::pull(user_id)

        if (glob$user$is_admin && !(glob$user$user_id %in% existing_user_id)) {
          # create user in db if an uknown admin logs in
          users_df <- data.frame(
            user_id = glob$user$user_id,
            user_name = glob$user$name,
            user_mail = glob$user$mail
          )
          DBI::dbWriteTable(glob$pool, "users", users_df,
            append = TRUE, row.names = FALSE
          )
        } else if (!glob$user$is_admin) {
          # abort project creation if user is not admin
          warn_user("Only administrators can create new projects.")
          req(glob$user$is_admin)
        } else {

          # if user control ok, create project

          loc$active_project <- create_project_db(
            pool = glob$pool,
            project_name = input$project_name,
            project_description = input$project_description,
            user_id = glob$user$user_id
          )
        }
      })
    })


    # pass active project details info to glob ----
    observeEvent(loc$active_project, {
      glob$active_project <- loc$active_project
    })
  })
}
