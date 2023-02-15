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
  uiOutput(ns("assigned_users"))
  )
}

#' use_manager Server Functions
#'
#' @noRd
mod_use_manager_server <- function(id, glob) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(glob$active_project, {
    
    all_users <- get_users() %>%
    dplyr::filter(user_id != glob$user$user_id)

    updateSelectInput( session = session,
          "rql_users",
          choices = c("", all_users$user))
})

observeEvent(input$assign, {
    validate(
      need(glob$user$permissions_modify, 'Insufficent privileges.'),
    )
output$assigned_users <- renderUI({

users_assigned_df <- dplyr::tbl(glob$pool, "user_permissions") %>%
dplyr::filter(project_id == !!as.integer(glob$active_project)) %>%
dplyr::collect()

users_details_df <- dplyr::tbl(glob$pool, "users") %>%
dplyr::filter(user_id %in% !!users_assigned_df$user_id) %>%
dplyr::collect()

users_permissions_df <- dplyr::inner_join(
  users_assigned_df,
  users_details_df,
  by = "user_id"
  )
browser()
  users_permissions_df2 <- users_permissions_df |>
  dplyr::select(user_id, tidyselect::starts_with("can_")) |> 
  tidyr::pivot_longer(
    -user_id, 
    names_to = "permission",
    values_to = "initial_value"
    )  |> 
    tidyr::nest(permission = permission, initial_value = initial_value) |> 
    dplyr::mutate(permission = purrr::map(permission, unlist),
    initial_value = purrr::map(initial_value, unlist))

purrr::pmap(users_permissions_df2 |> dplyr::group_by(user_id),
.f = gen_permissions_ui
)

})
})
    
  })
}