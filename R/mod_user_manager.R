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
        menu_btn(
          uiOutput(ns("add_user_ui")),
          label = "Add user",
          icon = "plus",
          inputId = ns("add_menu")
        ),
        menu_btn(
          uiOutput(ns("remove_user_ui")),
          label = "Remove user",
          icon = "minus",
          inputId = ns("remove_menu")
        )
      ) 
    },
    fluidRow(class = "module_content",
      tags$h2("Project members"),
    DT::DTOutput(ns("assigned_users")),
    if (golem::get_golem_options("mode") == "server") {
      actionButton(ns("save_permissions"), "Save permissions")
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
      if (golem::get_golem_options("mode") == "server") {
        loc$users_display <- loc$users_permissions_df %>%
        dplyr::mutate(across(-c(user_id, created_at, user_mail, user_name, user_login), 
        .fn = function(x) {
          ifelse(x == 1, '<i class="fa fa-check"></i>', '<i class="fa fa-cross"></i>')
        })) %>%
        tidyr::unite("Data", tidyselect::starts_with("data")) %>%
        tidyr::unite("Attributes", tidyselect::starts_with("attributes")) %>%
        tidyr::unite("Codebook", tidyselect::starts_with("codebook")) %>%
        tidyr::unite("Annotation", tidyselect::starts_with("annotation")) %>%
        tidyr::unite("Report", tidyselect::starts_with("report")) %>%
        dplyr::select(
          "Login" = user_login,
          "Name"  = user_name,
          "Mail"  = user_mail,
          "Data",
          "Attributes"
          )
          
  
      DT::datatable(
      loc$users_display,
      escape = FALSE, # To allow the rendering of HTML elements
      options = list(
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE,
        columnDefs = list(
          list(targets = 1, render = DT::JS("function(data, type, row, meta) {
            return '<input type=\"checkbox\" class=\"row-checkbox\" value=\"' + meta.row + '\" ' + (data ? 'checked' : '') + '>';
          }")),
          list(targets = 2, className = "dt-center"),
          list(targets = 3, className = "dt-center")
        )
      ),
      callback = DT::JS("table.on('change', '.row-checkbox', function() {
        table.cell($(this).closest('td')).data(this.checked);
      });")
    )

    
        #gen_users_permissions_ui(users_permissions_nested, id = id, glob$user$data)
      } else {
        "Local version of reQual does not support multiple users."
      }
    })

    # change permissions =======================================================
    observeEvent(input$save_permissions, {
      check_modify_permission(
        glob$user$data$permissions_modify,
        "Missing permission to modify permissions."
      )
      loc$users_permissions_long <- loc$users_permissions_long %>%
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
      add_permissions_record(
        pool = glob$pool,
        project_id = glob$active_project,
        user_id = req(input$rql_users)
      )
      # refresh users for current project
      loc$users_permissions_df <- get_user_permissions(
        glob$pool,
        glob$active_project
      )
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
        dplyr::pull(project_admin)
      if (any(owner_check == 1)) warn_user("Project owners cannot be removed from project.")
      req(all(owner_check == 0))
      remove_permissions_record(
        pool = glob$pool,
        project_id = glob$active_project,
        user_id = req(input$members_to_remove)
      )

      # refresh users for current project
      loc$project_members_ids <- loc$project_members_ids[loc$project_members_ids != as.integer(input$members_to_remove)]

      loc$users_permissions_df <- get_user_permissions(
        glob$pool,
        glob$active_project
      )
    })

    # update user selection inputs =======================================================
    observeEvent(c(loc$users_permissions_df, glob$active_project), {
      loc$users_to_add <- loc$all_users_choices[!loc$all_users_choices %in% as.integer(loc$users_permissions_df$user_id)]
      if (length(loc$users_to_add) < 1) {
        loc$users_to_add <- c("All registered users have been assigned." = 0)
      }
      # display users to add
      updateSelectInput(
        session = session,
        "rql_users",
        choices = c("", loc$users_to_add)
      )

      # display users to remove
      updateSelectInput(
        session = session,
        "members_to_remove",
        choices = c("", stats::setNames(
          loc$users_permissions_df$user_id,
          loc$users_permissions_df$user_name
        ))
      )
    })

    # Add user UI =======================================================
    output$add_user_ui <- renderUI({
      add_user_UI(id)
    })
    outputOptions(output, "add_user_ui", suspendWhenHidden = FALSE)

    # Remove user UI =======================================================
    output$remove_user_ui <- renderUI({
      remove_user_UI(id)
    })
    outputOptions(output, "remove_user_ui", suspendWhenHidden = FALSE)


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