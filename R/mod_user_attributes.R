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
    DT::dataTableOutput(ns("user_attributes_table")),
    uiOutput(ns("attribute_name_ui")),
    uiOutput(ns("attribute_values_ui")),
    uiOutput(ns("add_attribute_ui")),
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
                options = list(dom = 't', paging = FALSE, ordering = FALSE))
    })
    
    observeEvent(input$delete_attribute_click, {
      browser()
      selectedRow <- as.numeric(strsplit(input$delete_attribute_click, "_")[[1]][2])
      # Check if the click event was on a button
      if (startsWith(input$mytable_cell_clicked$value, "<button")) {
        # Get the ID of the row that was clicked
        row_id <- as.numeric(str_extract(input$mytable_cell_clicked$id, "\\d+"))
        # Print a message to the console
        print(paste0("Button clicked on row ", row_id))
        # You can add code here to perform an action when the button is clicked
      }
    })
    
    observeEvent(glob$active_project, {
      user_attributes <- get_user_attributes_data_table(glob$pool, project_id = glob$active_project)
      # re-render on project change
      output$user_attributes_table <- DT::renderDT({
        user_attributes }, 
        escape = FALSE, 
        options = list(dom = 't', paging = FALSE, ordering = FALSE))
      
      output$user_attributes_chart <- renderPlot({
        req(glob$active_project)
        
        attr_user_map <- dplyr::tbl(glob$pool, "attributes_users_map") %>% 
          dplyr::filter(project_id == !!as.numeric(glob$active_project)) %>% 
          dplyr::collect()
        
        attribute_values <- dplyr::tbl(glob$pool, "attributes") %>% 
          dplyr::left_join(., dplyr::tbl(glob$pool, "attribute_values"), by = "attribute_id") %>% 
          dplyr::select(attribute_id, attribute_name, attribute_value_id, value) %>% 
          dplyr::collect()
        
        user_attributes <- attr_user_map %>% 
          dplyr::left_join(., attribute_values, by = c("attribute_id", "attribute_value_id")) %>% 
          dplyr::select(user_id, attribute_name, attribute_value = value)
        
        user_attributes_summary <- user_attributes %>% 
          dplyr::count(attribute_name, attribute_value) %>% 
          dplyr::group_by(attribute_name) %>% 
          dplyr::mutate(share = n / sum(n)) %>% 
          dplyr::ungroup() %>% 
          dplyr::filter(!is.na(attribute_name))
        
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
      browser()
      
      output$user_attributes_table <- DT::renderDT({
        user_attributes}, escape = FALSE, 
        options = list(dom = 't', paging = FALSE, ordering = FALSE))
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