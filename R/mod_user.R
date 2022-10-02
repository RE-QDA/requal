#' user UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_user_ui <- function(id){
    ns <- NS(id)
    tagList(
        userOutput(ns("user"))
    )
}

#' user Server Functions
#'
#' @noRd 
mod_user_server <- function(id, glob){
    moduleServer(id, function(input, output, session){
        ns <- session$ns
        
        loc <- reactiveValues()
        
        output$user <- renderUser({
            
            if (isTruthy(glob$active_project)) {
                
        loc$user_data <- read_user_db(
            glob$pool,
            user_id = 1,
            active_project = glob$active_project
          )
        
        glob$user <- loc$user_data
                
                permissions_list <- loc$user_data %>% 
                    dplyr::select(dplyr::starts_with("can_")) %>% 
                    tidyr::pivot_longer(dplyr::everything(),
                                        names_to = "permissions",
                                        values_to = "granted") %>% 
                    dplyr::filter(.data$granted == 1) %>% 
                    dplyr::pull(.data$permissions) 
                
                dashboardUser(
                    name = loc$user_data$user_name,
                    image = "www/user_logo.jpg", 
                    title = ifelse(is.na(loc$user_data$user_mail), "@", loc$user_data$user_mail),
                    subtitle = paste0("Project:", glob$active_project), 
                    footer =  actionButton(ns("edit_user"),
                                           "Edit"),
                    fluidRow(
                        dashboardUserItem(
                            width = 12,
                            tags$div(purrr::map(permissions_list, tags$p),
                                     style = "text-align: left")
                        )
                    ))
            }
        })
        
        # edit user ----
        observeEvent(input$edit_user, {
            
            showModal(
                
                modalDialog(
                    title = "User details",
                    
                    textInput(ns("user_name"), "User name",
                              value = loc$user_data$user_name
                    ),
                    
                    textInput(ns("user_email"), "Email",
                              value = ifelse(is.na(loc$user_data$user_mail), "@", loc$user_data$user_mail)
                    ),
                    
                    footer = tagList(
                        modalButton("Cancel"),
                        actionButton(ns("save_close"), "Save & Close")
                    )
                    
                )
                
            )
        })
        
        observeEvent(input$save_close, {
            update_user_db(glob$pool, 
                           user_id = 1,
                           input$user_name,
                           input$user_email)
            
            loc$user_data <- read_user_db(glob$pool, user_id = 1, glob$active_project)
            removeModal()
            
            glob$user <- loc$user_data
        })
        
        
        
    })
}


