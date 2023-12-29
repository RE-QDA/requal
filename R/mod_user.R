#' user UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_user_ui <- function(id) {
  ns <- NS(id)
  tagList(
    userOutput(ns("user"))
  )
}

#' user Server Functions
#'
#' @noRd
mod_user_server <- function(id, glob) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    loc <- reactiveValues() 
    
    output$user <- renderUser({
      if (isTruthy(glob$active_project)) {
        
        # user data ----
        loc$user_data <- read_user_db(
          glob$pool,
          user_id = glob$user$user_id,
          active_project = glob$active_project
        )
        
        glob$user$data <- loc$user_data
        
        # user attributes ---- 
        loc$user_attributes <- read_user_attributes(glob$pool, project_id = glob$active_project) %>%
          dplyr::group_by(attribute_name) %>% 	
          dplyr::summarise(values = list(value))
        
        loc$permissions_list <- loc$user_data %>% 
          dplyr::select(dplyr::matches("view|modify")) %>% 
          tidyr::pivot_longer(dplyr::everything(),
                              names_to = "permissions",
                              values_to = "granted") %>% 
          dplyr::filter(.data$granted == 1) %>% 
          dplyr::pull(.data$permissions) 
        
        dashboardUser(
          name = loc$user_data$user_name,
          image = "www/user_logo.jpg", 
          title = loc$user_data$user_login,
          subtitle = ifelse(is.na(loc$user_data$user_mail), "No email address provided.", loc$user_data$user_mail), 
          footer =  actionButton(ns("edit_user"),
                                 "Edit"),
          fluidRow(
            dashboardUserItem(
              width = 12,
              tags$div(purrr::map(translate_permissions(loc$permissions_list), tags$p),
                       style = "text-align: left")
            )
          ))
      }
    })
    
    # edit user ----
    observeEvent(input$edit_user, {
      
      loc$user_attributes <- read_user_attributes(glob$pool, project_id = glob$active_project) %>% 
        dplyr::group_by(attribute_name) %>% 
        dplyr::summarise(values = list(value))
      
      loc$specific_user_attributes <- read_user_attributes_by_id(
        glob$pool, user_id = glob$user$user_id, project_id = glob$active_project)
      
      showModal(
        modalDialog(
          title = "User details",
          textInput(ns("user_name"), "User name",
                    value = loc$user_data$user_name
          ),
          textInput(ns("user_email"), "Email",
                    value = ifelse(is.na(loc$user_data$user_mail), "@", loc$user_data$user_mail)
          ),
          
          purrr::map2(loc$user_attributes$attribute_name, loc$user_attributes$values, function(x, y) {
            loc$values_df <- loc$specific_user_attributes %>% 
              dplyr::filter(.data$attribute_name == x)
            
            if(nrow(loc$values_df)){
              selected_value <- loc$values_df %>% dplyr::pull(value)
            }else{
              selected_value <- ""
            }
            generate_user_attribute_select_ui(ns(paste0("user_attribute_", x)), x, y, 
                                              selected_value)
          }),
          
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("save_close"), "Save & Close", class = "btn-success")
          )
        )
      )
    })
    
    observeEvent(input$save_close, {
      update_user_db(glob$pool,
                     user_id = glob$user$user_id,
                     input$user_name,
                     input$user_email
      )
      
      loc$user_attributes <- read_user_attributes(glob$pool, project_id = glob$active_project) %>%
        dplyr::group_by(attribute_name) %>% 	
        dplyr::summarise(values = list(value))
      
      if (nrow(loc$user_attributes) > 0) {
        loc$user_attr_values_df <- get_user_attributes_from_modal(input, loc$user_attributes$attribute_name)
        update_user_attributes(glob$pool, glob$active_project, user_id = glob$user$user_id, loc$user_attr_values_df)
      }
      
      loc$user_data <- read_user_db(glob$pool, user_id = glob$user$user_id, glob$active_project)
      removeModal()
      
      glob$user$data <- loc$user_data
    })
  })
}
