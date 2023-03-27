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
    uiOutput(ns("attribute_name_ui")),
    uiOutput(ns("attribute_type_ui")),
    uiOutput(ns("attribute_values_ui")),
    uiOutput(ns("attribute_num_range_ui")),
    uiOutput(ns("add_attribute_ui"))
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
                      type = input$attribute_type, 
                      min = input$attribute_min, 
                      max = input$attribute_max,
                      object = "user")
        
        new_attribute_id <- dplyr::tbl(glob$pool, "attributes") %>% 
          dplyr::filter(.data$attribute_name == local(input$attribute_name)) %>% 
          dplyr::filter(.data$attribute_object == "user") %>% 
          dplyr::collect() %>% 
          dplyr::pull(attribute_id)
        
        if(input$attribute_type == "category"){
          add_attribute_values(pool = glob$pool, 
                               attribute_id = new_attribute_id, 
                               attribute_values = input$attribute_values) 
        }
        
        log_create_user_attribute(
          glob$pool, glob$active_project, 
          user_id = glob$user$user_id, 
          attribute_data = list(
            attribute_name = input$attribute_name,
            attribute_id = new_attribute_id, 
            attribute_values = input$attribute_values, 
            attribute_min = input$attribute_min, 
            attribute_max = input$attribute_max)
        ) 
      }
      
      shinyjs::reset("attribute_name")
      shinyjs::reset("attribute_values")
      
      user_attributes <- read_user_attributes(glob$pool) %>% 
        summarise_user_attributes()
      
      output$user_attributes_table <- renderTable({user_attributes})
    })
    
    output$user_message <- renderText({
      "Here you can add user attributes for your users to select from." 
    })
    
    output$attribute_name_ui <- renderUI({
      if(glob$user$data$attributes_modify == 1){
        textInput(ns("attribute_name"), 
                  label = "Attribute name",
                  placeholder = "Write an attribute name") 
      }
    })
    
    output$attribute_type_ui <- renderUI({
      if(glob$user$data$attributes_modify == 1){
        selectInput(ns("attribute_type"), 
                  label = "Attribute type",
                  choices = c("Categorical"="category", "Numeric"="numeric")) 
      }
    })
    
    output$attribute_values_ui <- renderUI({
      req(input$attribute_type)
      if(glob$user$data$attributes_modify == 1){
        if(input$attribute_type == "category"){
          textAreaInput(ns("attribute_values"), 
                        label = "Attribute values",
                        placeholder = "Write possible attribute values separated by comma")  
        }
      }
    })
    
    output$attribute_num_range_ui <- renderUI({
      req(input$attribute_type)
      if(glob$user$data$attributes_modify == 1){
        if(input$attribute_type == "numeric"){
          list(
            numericInput(ns("attribute_min"), "Minimum (optional):", 
                         value = NULL, min = -Inf, max = Inf),
            numericInput(ns("attribute_max"), "Maximum (optional):", 
                         value = NULL, min = -Inf, max = Inf) 
          )
        }
      }
    })
    
    output$add_attribute_ui <- renderUI({
      if(glob$user$data$attributes_modify == 1){
        actionButton(ns("add_attribute"), "Add user attribute")
      }
    })
    
    output$user_attributes_table <- renderTable({
      read_user_attributes(glob$pool) %>% 
        summarise_user_attributes()
    })  
  })
}