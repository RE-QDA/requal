utils::globalVariables(c("user_id_copy", "permission", "project_admin", 
                         "permissions_modify", "project_owner"))

#' user_manager UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_user_manager_ui <- function(id) {
  ns <- NS(id)
  tagList(
    if (golem::get_golem_options("mode") == "server") {
      fluidRow(class = "module_tools",
      mod_rql_button_ui(ns("add_user_ui"),
        label = "Add user",
        icon = "plus",
        inputId = ns("add_menu")
      ),
      mod_rql_button_ui(ns("remove_user_ui"),
        label = "Remove user",
        icon = "minus",
        inputId = ns("remove_menu")
      ),
      mod_rql_button_ui(ns("modify_permissions_ui"),
        label = "Modify permissions",
        icon = "lock",
        inputId = ns("modify_permissions")
      )
      ) 
    },
    fluidRow(class = "module_content",
      tags$h2("Project members"), tags$br(),
      DT::DTOutput(ns("assigned_users")),
      if (golem::get_golem_options("mode") == "local") {
        "User management is only enabled for the server version."
      }
  )
  )
}

#' user_manager Server Functions
#'
#' @noRd
mod_user_manager_server <- function(id, glob) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    loc <- reactiveValues()

    # initialize user management page on project load ----
    observeEvent(glob$active_project, {
      if (golem::get_golem_options("mode") == "server") {
        # on init, get a list of users in credentials
        loc$all_users <- get_users(
          credentials_path = golem::get_golem_options(which = "credentials_path"),
          credentials_pass = golem::get_golem_options(which = "credentials_pass")
        )

        loc$all_users_choices <- stats::setNames(loc$all_users$user_id, loc$all_users$user_login)

        loc$users_permissions_df <- get_user_permissions(
          glob$pool,
          glob$active_project
        )

        loc$project_members_ids <- loc$users_permissions_df %>%
          dplyr::mutate(user_id = stats::setNames(user_id, user_name)) %>%
          dplyr::pull(user_id)
      }
    })

    # render project members =======================================================
    output$assigned_users <- DT::renderDataTable(server = FALSE, {
      req(golem::get_golem_options("mode") == "server") 

        loc$users_display <- loc$users_permissions_df %>%
        transform_user_table()
          
  
      DT::datatable(
      loc$users_display,
      escape = FALSE, # To allow the rendering of HTML elements
      rownames = FALSE
      )
    })

    # change permissions =======================================================
    observeEvent(input$save_permissions, {
      check_modify_permission(
        glob$user$data$permissions_modify,
        "Missing permission to modify permissions."
      )
      req(input$members_permissions)
      
      modified_permissions_df <- loc$users_permissions_df %>%
      dplyr::filter(user_id %in% input$members_permissions) %>%
      dplyr::mutate(dplyr::across(dplyr::matches("modify|view"), .fns = function(x) {
        x  <- 0 # reset permissions
      })) %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(input$permissions_list), .fns = function(x) {
        x  <- 1 # apply new permissions
      }))

      # Enforce that project owner can always modify permission
      modified_permissions_df$permissions_modify[modified_permissions_df$project_owner == 1] <- 1
      
      modify_permissions_record(
        pool = glob$pool,
        project_id = glob$active_project,
        permissions_df = modified_permissions_df, 
        user_id = glob$user$user_id
      )
      loc$users_permissions_df <- get_user_permissions(
        glob$pool,
        glob$active_project
      )
      showNotification("Changes to permissions were saved.")
    })

    # add new users =======================================================
    observeEvent(input$assign, {
      check_modify_permission(
        glob$user$data$permissions_modify,
        "Missing permission to add users."
      )

      req(input$rql_users > 0)
      # run check on existing users
      existing_users <- dplyr::tbl(glob$pool, "users") %>%
        dplyr::pull(user_id)

      existing_users_check <- input$rql_users[!input$rql_users %in% existing_users]

      if (length(existing_users_check) > 0) {
        # create user in db if an uknown user is assigned
        users_df <- loc$all_users %>%
          dplyr::filter(user_id %in% as.integer(input$rql_users))
        purrr::map(users_df$user_id, .f = function(x) {
          users_df_filtered <- users_df %>%
            dplyr::filter(user_id == as.integer(x))
          DBI::dbWriteTable(glob$pool, "users", users_df_filtered,
            append = TRUE, row.names = FALSE
          )
        })
      }
      # add user to project
      add_permissions_record(
        pool = glob$pool,
        project_id = glob$active_project,
        permission_user_id = req(input$rql_users), 
        user_id = glob$user$user_id
      )
      # refresh users for current project
      loc$users_permissions_df <- get_user_permissions(
        glob$pool,
        glob$active_project
      )

      glob$users_observer <- glob$users_observer + 1
    })

    # remove users =======================================================
    observeEvent(input$remove_members, {
      check_modify_permission(
        glob$user$data$permissions_modify,
        "Missing permission to remove users."
      )
      owner_check <- loc$users_permissions_df %>%
        dplyr::filter(
          project_id %in% glob$active_project &
            user_id %in% req(input$members_to_remove)
        ) %>%
        dplyr::pull(project_owner)
      if (any(owner_check == 1)) warn_user("Project owners cannot be removed from project.")
      req(all(owner_check == 0))
      remove_permissions_record(
        pool = glob$pool,
        project_id = glob$active_project,
        permission_user_id = req(input$members_to_remove), 
        user_id = glob$user$user_id
      )

      # refresh users for current project
      loc$project_members_ids <- loc$project_members_ids[loc$project_members_ids != as.integer(input$members_to_remove)]

      loc$users_permissions_df <- get_user_permissions(
        glob$pool,
        glob$active_project
      )
      
      glob$users_observer <- glob$users_observer + 1

    })

    # update user selection inputs =======================================================
    observeEvent(c(loc$users_permissions_df, glob$active_project), {
      loc$users_to_add <- loc$all_users_choices[!loc$all_users_choices %in% as.integer(loc$users_permissions_df$user_id)]
      if (length(loc$users_to_add) < 1) {
        loc$users_to_add <- c("All registered users have been assigned." = 0)
      }
      # display users to add
      shinyWidgets::updatePickerInput(
        session = session,
        "rql_users",
        choices = loc$users_to_add
      )

      # display users to remove
      shinyWidgets::updatePickerInput(
        session = session,
        "members_to_remove",
        choices = stats::setNames(
          loc$users_permissions_df$user_id,
          loc$users_permissions_df$user_name
        )
      )

      # display users permissions
      shinyWidgets::updatePickerInput(
        session = session,
        "members_permissions",
        choices = stats::setNames(
          loc$users_permissions_df$user_id,
          loc$users_permissions_df$user_name
          )
      )
      

      
    })

    # Add user UI =======================================================
    mod_rql_button_server(
      id = "add_user_ui",
      custom_title = "Add users to project",
      custom_tagList = tagList(
            rql_picker_UI(ns("rql_users"),
                label = "Select users",
                none = "Users to add"),
            rql_button_UI(ns("assign"), 
            label = "Add users")
            ),
      glob,
      permission = "permissions_modify"
    )


    # Remove user UI =======================================================
    mod_rql_button_server(
      id = "remove_user_ui",
      custom_title = "Remove users from project",
      custom_tagList = tagList(
   rql_picker_UI(ns("members_to_remove"), "Select users:", none = "Users to remove"),
   rql_button_UI(ns("remove_members"), "Remove users")
            ),
      glob,
      permission = "permissions_modify"
    )

   
   # Modify permissions UI =======================================================
       mod_rql_button_server(
      id = "modify_permissions_ui",
      custom_title = "Modify user permissions for project",
      custom_tagList = tagList(
   rql_picker_UI(
      ns("members_permissions"), 
      "Select users:",
      none = "Users to change permissions"),
    checkboxGroupInput(
      ns("permissions_list"),
      label = NULL,
      choices = stats::setNames(
          permission(),
          translate_permissions(permission())
          )
    ),
    rql_button_UI(ns("save_permissions"), "Save permissions")
            ),
      glob,
      permission = "permissions_modify"
    )
  
    # hide save permission UI from users without sufficient permission ======
    observeEvent(loc$users_permissions_df, {
      loc$permissions_modify <- loc$users_permissions_df %>%
        dplyr::filter(user_id == glob$user$user_id) %>%
        dplyr::pull(permissions_modify)
    })
    observeEvent(
      {
        input$add_menu
        input$remove_menu
        input$modify_permissions
        loc$permissions_modify
      },
      {
        if (loc$permissions_modify == 0) {
          shinyjs::disable("assign")
          shinyjs::disable("remove_members")
          shinyjs::disable("save_permissions")
        } else {
          shinyjs::enable("assign")
          shinyjs::enable("remove_members")
          shinyjs::enable("save_permissions")
        }
      })
  })
}