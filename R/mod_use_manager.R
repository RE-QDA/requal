#' use_manager UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_use_manager_ui <- function(id) {
  ns <- NS(id)
  tagList(
    menu_btn(
      uiOutput(ns("add_user_ui")),
      label = "Add user",
      icon = "plus"
    ),
    menu_btn(
      uiOutput(ns("remove_user_ui")),
      label = "Remove user",
      icon = "minus"
    ),
    tags$h2("Project members"),
    uiOutput(ns("assigned_users")),
    actionButton(ns("save_permissions"), "Save")
  )
}

#' use_manager Server Functions
#'
#' @noRd
mod_use_manager_server <- function(id, glob) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    loc <- reactiveValues()

    observeEvent(glob$active_project, {

      # get permissions of users for current project
      loc$users_assigned_df <- dplyr::tbl(glob$pool, "user_permissions") %>%
        dplyr::filter(project_id == !!as.integer(glob$active_project)) %>%
        dplyr::collect()

      # on init, get a list of users in credentials
      all_users <- get_users(
        credentials_path = golem::get_golem_options(which = "credentials_path"),
        credentials_pass = golem::get_golem_options(which = "credentials_pass")
      ) %>%
        dplyr::filter(!user_id %in% loc$users_assigned_df$user_id)

      loc$all_users_choices <- all_users$user_id
      names(loc$all_users_choices) <- all_users$user


      # get details of users for current project
      loc$users_details_df <- dplyr::tbl(glob$pool, "users") %>%
        dplyr::filter(user_id %in% !!loc$users_assigned_df$user_id) %>%
        dplyr::collect()

      # join details and permissions of users for current project
      loc$users_permissions_df <- dplyr::inner_join(
        loc$users_assigned_df,
        loc$users_details_df,
        by = "user_id"
      )

      loc$project_members <- loc$users_permissions_df  |> 
      dplyr::filter(!permissions_modify) |> 
      dplyr::mutate(user_id = stats::setNames(user_id, user_name)) |> 
      dplyr::pull(user_id)


      # convert details and permissions into long format for dynamic UI
      loc$users_permissions_long <- loc$users_permissions_df |>
        dplyr::select(-permissions_modify) |> 
        dplyr::select(user_id, tidyselect::matches("view|modify")) |>
        tidyr::pivot_longer(
          -user_id,
          names_to = "permission",
          values_to = "value"
        )
    })

  # render project members ----
    output$assigned_users <- renderUI({

      # create nested df for nested UI
      users_permissions_nested <- loc$users_permissions_long |>
        dplyr::mutate(user_id_copy = user_id) |>
        tidyr::nest(data = -user_id_copy) |>
        dplyr::left_join(loc$users_details_df,
          by = c("user_id_copy" = "user_id")
        )

      # generated user boxes with nested permissions
      gen_users_permissions_ui(users_permissions_nested, id = id)
    })

  # change permissions ----
    observeEvent(input$save_permissions, {
      loc$users_permissions_long <- loc$users_permissions_long |>
        dplyr::mutate(value = purrr::map2_int(user_id, permission,
          .f = function(user_id, permission) {
            input[[paste(user_id, permission, sep = "_")]]
          }
        ))

      modify_permissions_record(
        pool = glob$pool,
        project_id = glob$active_project,
        permissions_df = loc$users_permissions_long
      )
      showNotification("Changes to permissions were saved.")
    })

  # add new users ----
    observeEvent(input$assign, {
      add_permissions_record(
        pool = glob$pool,
        project_id = glob$active_project,
        user_id = req(input$rql_users)
      )

      # refresh permissions of users for current project
      loc$users_assigned_df <- dplyr::tbl(glob$pool, "user_permissions") %>%
        dplyr::filter(project_id == !!as.integer(glob$active_project)) %>%
        dplyr::collect()

    })

      # remove users ----
    observeEvent(input$remove_members, {

      remove_permissions_record(
        pool = glob$pool,
        project_id = glob$active_project,
        user_id = req(input$members_to_remove)
      )

      # refresh permissions of users for current project
     loc$project_members <- loc$project_members[loc$project_members != as.integer(input$members_to_remove)]
     loc$users_permissions_df <- loc$users_permissions_df |> 
     dplyr::filter(!user_id %in% as.integer(input$members_to_remove))

    })

observeEvent({req(glob$active_project)
                  loc$all_users_choices
                  loc$project_members},{
      # display users to add
      updateSelectInput(
        session = session,
        "rql_users",
        choices = c("", loc$all_users_choices)
      )

      # display users to remove
      updateSelectInput(
        session = session,
        "members_to_remove",
        choices = c("", loc$project_members)
      )
})
  # Add user UI --------------
    output$add_user_ui <- renderUI({
      add_user_UI(id)
    })
    outputOptions(output, "add_user_ui", suspendWhenHidden = FALSE)
  

  # Remove user UI --------------
    output$remove_user_ui <- renderUI({
      remove_user_UI(id)
})
    outputOptions(output, "remove_user_ui", suspendWhenHidden = FALSE)
})
  }