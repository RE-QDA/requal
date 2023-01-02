#' reproducibility_user_attributes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_user_attributes_ui <- function(id){
  ns <- NS(id)
  tagList(
    textOutput(ns("user_message")),
    tableOutput(ns("user_attributes_table")),
    textInput(ns("attribute_name"), 
              label = "Attribute name",
              placeholder = "Write an attribute name"),
    textAreaInput(ns("attribute_values"), 
              label = "Attribute values",
              placeholder = "Write possible attribute values separated by comma"),
    actionButton(ns("add_attribute"), "Add user attribute")
    
  )
}
    
#' reproducibility_user_attributes Server Functions
#'
#' @noRd 
mod_user_attributes_server <- function(id, glob){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$add_attribute, {
      
      existing_attributes <- dplyr::tbl(glob$pool, "attributes") %>% 
        dplyr::filter(.data$attribute_object == "user") %>% 
        dplyr::collect()
      
      if(!input$attribute_name %in% existing_attributes$attribute_name){
        add_attribute(pool = glob$pool, input$attribute_name,
                      type = "category", object = "user")
        
        new_attribute_id <- dplyr::tbl(glob$pool, "attributes") %>% 
          dplyr::filter(.data$attribute_name == local(input$attribute_name)) %>% 
          dplyr::filter(.data$attribute_object == "user") %>% 
          dplyr::collect() %>% 
          dplyr::pull(attribute_id)
        
        add_attribute_values(pool = glob$pool, 
                             attribute_id = new_attribute_id, 
                             attribute_values = input$attribute_values)
        
        log_create_user_attribute(glob$pool, glob$active_project, user_id = 1, 
                                  attribute_data = list(
                                    attribute_name = input$attribute_name,
                                    attribute_id = new_attribute_id, 
                                    attribute_values = input$attribute_values)) 
      }
      
      shinyjs::reset("attribute_name")
      shinyjs::reset("attribute_values")
      
      user_attributes <- read_user_attributes(glob$pool) %>% 
        dplyr::group_by(attribute_name) %>%
        dplyr::summarise(values = paste0(value, collapse = ", "))
      
      output$user_attributes_table <- renderTable({user_attributes})
    })
    
    output$user_message <- renderText({
      "Here you can add user attributes for your users to select from." 
    })
    
    output$user_attributes_table <- renderTable({
      read_user_attributes(glob$pool) %>% 
        dplyr::group_by(attribute_name) %>%
        dplyr::summarise(values = paste0(value, collapse = ", "))
    })  
  })
}