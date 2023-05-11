#' attributes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_attributes_ui <- function(id){
  ns <- NS(id)

  tabsetPanel(
      type = "tabs", id = ns("attributes_tabset"),
     tabPanel("Attributes", 
               id = ns("attributes_manager"), 
               value = "attributes_manager", 
               mod_attributes_manager_ui(ns("attributes_manager_1"))
  ),
      tabPanel("User attributes", 
               id = ns("user_attributes"), 
               value = "user_attributes", 
               mod_user_attributes_ui(ns("user_attributes_ui_1"))
  )
    
  
  )
}
    
#' attributes Server Functions
#'
#' @noRd 
mod_attributes_server <- function(id, glob){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
  hideTab("attributes_tabset", "attributes_manager") #TODO temporary, to remove when done
  mod_attributes_manager_server("attributes_manager_1", glob)
  mod_user_attributes_server("user_attributes_ui_1", glob)

  })
}
    
