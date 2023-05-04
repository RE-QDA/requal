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
    uiOutput(ns("attribute_name_ui")),
    uiOutput(ns("attribute_values_ui")),
    uiOutput(ns("add_attribute_ui")),
    DT::dataTableOutput(ns("user_attributes_table")),
    plotOutput(ns("user_attributes_chart"))
  )
}
    
#' reproducibility_user_attributes Server Functions
#'
#' @noRd 
mod_user_attributes_server <- function(id, glob){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$user_attributes_table <- DT::renderDataTable({
      get_user_attributes_data_table(glob$pool, project_id = glob$active_project) %>% 
      DT::datatable(., escape = FALSE, 
                options = list(dom = 't', paging = FALSE, ordering = FALSE), 
                colnames = c("Attribute ID", "Attribute name", "Attribute values", "Actions"))
    })
    
    observeEvent(glob$active_project, {
      user_attributes <- get_user_attributes_data_table(glob$pool, project_id = glob$active_project)
      # re-render on project change
      output$user_attributes_table <- DT::renderDT({
        user_attributes }, 
        escape = FALSE, 
        options = list(dom = 't', paging = FALSE, ordering = FALSE), 
        colnames = c("Attribute ID", "Attribute name", "Attribute values", "Actions"))
      
      output$user_attributes_chart <- renderPlot({
        req(glob$active_project)
        
        user_attributes_summary <- get_user_attributes_summary(glob$pool, glob$active_project)
        
        unique_attributes <- unique(user_attributes_summary$attribute_name)
        n_attributes <- length(unique_attributes)
        
        if(n_attributes > 0){
          rows <- ceiling(sqrt(n_attributes))
          cols <- ceiling(n_attributes / rows)
          par(mfrow = c(rows, cols), oma = rep(0, 4), mar = c(0, 0, 2, 0))
          
          purrr::walk(unique_attributes, function(x) {
            tmp <- user_attributes_summary %>% 
              dplyr::filter(attribute_name == !!x) 
            
            pie(tmp$n, labels = tmp$attribute_value, main = paste0("Attribute: ", x))
          })  
        }
      })
    })
    
    observeEvent(input$add_attribute, {
      
      existing_attributes <- dplyr::tbl(glob$pool, "attributes") %>% 
        dplyr::filter(.data$attribute_object == "user" & 
                        .data$project_id == !!as.numeric(glob$active_project)) %>% 
        dplyr::collect()
      
      if(!input$attribute_name %in% existing_attributes$attribute_name){
        add_attribute(pool = glob$pool, input$attribute_name,
                      type = "category", object = "user", 
                      project_id = glob$active_project)
        
        new_attribute_id <- dplyr::tbl(glob$pool, "attributes") %>% 
          dplyr::filter(.data$attribute_name == local(input$attribute_name)) %>% 
          dplyr::filter(.data$attribute_object == "user") %>% 
          dplyr::collect() %>% 
          dplyr::pull(attribute_id)
        
        add_attribute_values(pool = glob$pool, 
                             attribute_id = new_attribute_id, 
                             attribute_values = input$attribute_values)
        
        log_create_user_attribute(glob$pool, glob$active_project, 
                                  user_id = glob$user$user_id, 
                                  attribute_data = list(
                                    attribute_name = input$attribute_name,
                                    attribute_id = new_attribute_id, 
                                    attribute_values = input$attribute_values)) 
      }
      
      shinyjs::reset("attribute_name")
      shinyjs::reset("attribute_values")
      
      user_attributes <- get_user_attributes_data_table(glob$pool, project_id = glob$active_project)
      
      output$user_attributes_table <- DT::renderDT({
        user_attributes}, escape = FALSE, 
        options = list(dom = 't', paging = FALSE, ordering = FALSE), 
        colnames = c("Attribute ID", "Attribute name", "Attribute values", "Actions"))
    })
    
    observeEvent(input$selected_attr, {
      user_attribute_name <- dplyr::tbl(glob$pool, "attributes") %>% 
        dplyr::filter(attribute_id == !!input$selected_attr) %>% 
        dplyr::collect() %>% 
        dplyr::pull(attribute_name)
      
      # TODO: check user permissions / attributes_other_modify
      # What permission should user have to delete user attribute?
      if(glob$user$data$attributes_modify == 1){
        showModal(
          modalDialog(
            title = "Delete user attribute",
            HTML(paste0("Are you sure that you want to delete user attribute: <b>", user_attribute_name, "</b>?")),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("delete_attr"), "Yes, delete", class = "btn-danger")
            ), 
            easyClose = TRUE
          )
        )
      }else{
        showModal(
          modalDialog(
            title = "Delete user attribute",
            paste0("You don't have necessary permissions for deleting user attributes."),
            footer = tagList(
              modalButton("Cancel")
            ), 
            easyClose = TRUE
          )
        )
      }
    })
    
    observeEvent(input$delete_attr, {
      delete_user_attribute(pool = glob$pool, project_id = glob$active_project, 
                            user_id = glob$user$user_id, input$selected_attr)
      
      user_attributes <- get_user_attributes_data_table(glob$pool, project_id = glob$active_project)
      output$user_attributes_table <- DT::renderDT({
        user_attributes}, escape = FALSE, 
        options = list(dom = 't', paging = FALSE, ordering = FALSE), 
        colnames = c("Attribute ID", "Attribute name", "Attribute values", "Actions"))
      
      removeModal()
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
    
    output$attribute_values_ui <- renderUI({
      if(glob$user$data$attributes_modify == 1){
        textAreaInput(ns("attribute_values"), 
                      label = "Attribute values",
                      placeholder = "Write possible attribute values separated by comma")
      }
    })
    
    output$add_attribute_ui <- renderUI({
      if(glob$user$data$attributes_modify == 1){
        actionButton(ns("add_attribute"), "Add user attribute")
      }
    })
    
  })
}