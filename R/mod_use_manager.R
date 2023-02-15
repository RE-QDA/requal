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
    selectInput(ns("rql_users"),
                "Assign selected users to project",
                choices = "",
                multiple = TRUE
  ),
  actionButton(ns("assign"), "Assign"),
  tags$br(),
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
    
    # on init, get a list of users in credentials
    all_users <- get_users(
      credentials_path = golem::get_golem_options(which = "credentials_path"),
      credentials_pass = golem::get_golem_options(which = "credentials_pass")
    ) %>%
    dplyr::filter(user_id != glob$user$user_id)

    # display users in selection
    updateSelectInput( session = session,
          "rql_users",
          choices = c("", all_users$user))
    
    # get permissions of users for current project
    loc$users_assigned_df <- dplyr::tbl(glob$pool, "user_permissions") %>%
    dplyr::select(-permissions_modify) %>%
dplyr::filter(project_id == !!as.integer(glob$active_project)) %>%
dplyr::collect()

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

  # convert details and permissions into long format for dynamic UI
  loc$users_permissions_long <- loc$users_permissions_df |>
  dplyr::select(user_id, tidyselect::matches("view|modify")) |> 
  tidyr::pivot_longer(
    -user_id, 
    names_to = "permission",
    values_to = "value"
    )
})


output$assigned_users <- renderUI({

browser()
nested_df <- loc$users_permissions_long |> 
dplyr::mutate(user_id_copy = user_id) |> 
tidyr::nest(data = -user_id_copy)


 gen_users_permissions_ui(nested_df, id = id)



})

observeEvent(input$save_permissions, {
 
 loc$users_permissions_long <- loc$users_permissions_long |>
   dplyr::mutate(value = purrr::map2_int(user_id, permission,
     .f = function(user_id, permission) {
       input[[paste(user_id, permission, sep = "_")]]
     }
   ))

 
# add_permissions_record(
#           pool = glob$pool,
#           project_id = glob$active_project,
#           permissions_df = NULL, 
#           user_id = glob$user$user_id
#         )

modify_permissions_record(
          pool = glob$pool,
          project_id = glob$active_project,
          permissions_df = loc$users_permissions_long
        )
showNotification("Changes to user permissions were saved.")

})
    
  })
}