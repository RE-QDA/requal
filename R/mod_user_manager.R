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
    uiOutput(ns("user_management_buttons_ui")),
    tags$h2("Project members"),
    uiOutput(ns("assigned_users")),
    uiOutput(ns("user_save_permissions_ui"))
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
      
      if(golem::get_golem_options("mode") == "server"){
        # on init, get a list of users in credentials
        loc$all_users <- get_users(
          credentials_path = golem::get_golem_options(which = "credentials_path"),
          credentials_pass = golem::get_golem_options(which = "credentials_pass")
        ) 
        
        loc$all_users_choices <- loc$all_users$user_id
        names(loc$all_users_choices) <- loc$all_users$user_login
        
        loc$users_permissions_df <- get_user_permissions(
          glob$pool, 
          glob$active_project
        )
        
        loc$project_members_ids <- loc$users_permissions_df %>% 
          dplyr::filter(!permissions_modify) %>% 
          dplyr::mutate(user_id = stats::setNames(user_id, user_name)) %>% 
          dplyr::pull(user_id)
        
      }
      
      # add user permission to glob
      # glob$user$permissions <- loc$users_permissions_df %>% 
      #   dplyr::filter(user_id == glob$user$user_id) %>% 
      #   dplyr::select(dplyr::any_of(
      #     c("data_modify", "data_other_modify", "data_other_view", 
      #       "attributes_modify", "attributes_other_modify", "attributes_other_view", 
      #       "codebook_modify", "codebook_other_modify", 
      #       "annotation_modify", "annotation_other_modify", "annotation_other_view", 
      #       "analysis_other_view", "report_other_view", "permissions_modify"))) %>% 
      #   as.list()
    })
    
    output$user_management_buttons_ui <- renderUI({
      if(golem::get_golem_options("mode") == "server" && 
         glob$user$data$permissions_modify == 1){
        div(
          menu_btn(
            uiOutput(ns("add_user_ui")),
            label = "Add user",
            icon = "plus"
          ),
          menu_btn(
            uiOutput(ns("remove_user_ui")),
            label = "Remove user",
            icon = "minus"
          )
        ) %>% tagAppendAttributes(style = "display: -webkit-inline-box;") 
      }
    })
    
    output$user_save_permissions_ui <- renderUI({
      if(golem::get_golem_options("mode") == "server" && 
         glob$user$data$permissions_modify == 1){
        actionButton(ns("save_permissions"), "Save")
      }
    })
    
    # render project members ----
    output$assigned_users <- renderUI({
      if(golem::get_golem_options("mode") == "server"){
        
        loc$users_permissions_long <- loc$users_permissions_df %>%
          # dplyr::select(-permissions_modify) %>% 
          dplyr::select(user_id, tidyselect::matches("view|modify")) %>%
          tidyr::pivot_longer(
            -user_id,
            names_to = "permission",
            values_to = "value"
          ) 
        
        # create nested df for nested UI
        users_permissions_nested <- loc$users_permissions_long %>% 
          dplyr::mutate(user_id_copy = user_id) %>%
          tidyr::nest(data = -user_id_copy) %>%
          dplyr::inner_join(loc$users_permissions_df,
                            by = c("user_id_copy" = "user_id")
          ) %>% 
          dplyr::filter(!duplicated(user_id_copy))
        
        # generated user boxes with nested permissions
        # browser()
        gen_users_permissions_ui(users_permissions_nested, id = id, glob$user$data)
      }
    })
    
    # change permissions ----
    observeEvent(input$save_permissions, {
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
    
    # add new users ----
    observeEvent(input$assign, {
      
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
                            append = TRUE, row.names = FALSE)
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
    
    # remove users ----
    observeEvent(input$remove_members, {
      
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
    
    # update user selection inputs ----
    observeEvent(loc$users_permissions_df, {
      
      loc$users_to_add <- loc$all_users_choices[!loc$all_users_choices %in% as.integer(loc$users_permissions_df$user_id)]
      if (length(loc$users_to_add) < 1) {loc$users_to_add <- "All registered users have been assigned."}
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
        )
        )
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