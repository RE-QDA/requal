utils::globalVariables(c("share"))

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
    fluidRow(
      class = "module_tools",
      div(
      mod_rql_button_ui(ns("attribute_create_ui"),
        label = "Create attribute",
        icon = "plus"
      )) %>% tagAppendAttributes(style = "padding-right: 25px;"),
      mod_rql_button_ui(ns("attribute_delete_ui"),
        label = "Delete attribute",
        icon = "minus"
      )
    ),
    fluidRow(
      class = "module_content",
    textOutput(ns("user_message")),
    DT::dataTableOutput(ns("user_attributes_table")),
    actionButton(ns("show_graph"), label = "Show distribution", icon = icon("chart-pie", verify_fa = FALSE)),
    plotOutput(ns("user_attributes_chart"))
  )
  )
}
    
#' reproducibility_user_attributes Server Functions
#'
#' @noRd 
mod_user_attributes_server <- function(id, glob){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    loc <- reactiveValues()
     loc$change_observer <- 0

    # re-render attributes data on project change
    observeEvent(glob$active_project, {
      loc$user_attributes <- get_user_attributes_data_table(ns = ns, 
      glob$pool, 
      project_id = glob$active_project)
    })
    observeEvent(req(loc$change_observer > 0), {
      loc$user_attributes <- get_user_attributes_data_table(ns = ns, 
      glob$pool, 
      project_id = glob$active_project)
      shinyWidgets::updatePickerInput(session = session, "attribute_delete", choices = stats::setNames(
            loc$user_attributes$attribute_id,
            loc$user_attributes$attribute_name))
    })

    # attributes table
     output$user_attributes_table <- DT::renderDT({
        req(loc$user_attributes)}, escape = FALSE, 
        options = list(dom = 't', paging = FALSE, ordering = FALSE), 
        colnames = c("Attribute ID", "Attribute name", "Attribute values", "Actions"))
    
    # pie chart
    observeEvent(input$show_graph, {
        
        req(glob$active_project)
        output$user_attributes_chart <- renderPlot({
        
        user_attributes_summary <- get_user_attributes_summary(glob$pool, glob$active_project)
        
        unique_attributes <- unique(user_attributes_summary$attribute_name)
        n_attributes <- length(unique_attributes)
        
        if(n_attributes > 0){
          rows <- ceiling(sqrt(n_attributes))
          cols <- ceiling(n_attributes / rows)
          
          ggplot2::ggplot(user_attributes_summary, 
                          ggplot2::aes(x = "", y = share, fill = attribute_value)) + 
            ggplot2::geom_bar(stat = "identity") + 
            ggplot2::geom_text(ggplot2::aes(label = attribute_value), 
                               position = ggplot2::position_stack(vjust=0.5)) +
            ggplot2::coord_polar(theta = "y", start = 0) + 
            ggplot2::facet_wrap(ggplot2::vars(attribute_name), nrow = rows, ncol = cols) + 
            ggplot2::theme_void() + 
            ggplot2::theme(legend.position = "none")
            
          
        }
      })
    })
    
    # Add attribute event ----
    observeEvent(input$add_attribute, {
      
      existing_attributes <- dplyr::tbl(glob$pool, "attributes") %>% 
        dplyr::filter(.data$attribute_object == "user" & 
                        .data$project_id == !!as.numeric(glob$active_project)) %>% 
        dplyr::collect()
      if(!isTruthy(input$attribute_name) | !isTruthy(input$attribute_values)){
        warn_user("Attribute name and values cannot be empty.")
      }else if(input$attribute_name %in% existing_attributes$attribute_name){
        warn_user(paste0("Attribute ", input$attribute_name, " already exists. Choose a different name."))
      }else if(
        length(split_values(input$attribute_values)) != length(unique(split_values(input$attribute_values)))
        ){
        warn_user("Attribute values must be unique.")
      }else{
        add_attribute(pool = glob$pool, input$attribute_name,
                      type = "category", object = "user", 
                      project_id = glob$active_project, 
                      user_id = glob$user$user_id)
        
        new_attribute_id <- dplyr::tbl(glob$pool, "attributes") %>% 
          dplyr::filter(.data$project_id == !!as.numeric(glob$active_project), 
                        .data$attribute_name == local(input$attribute_name), 
                        .data$attribute_object == "user") %>% 
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
        
        shinyjs::reset("attribute_name")
        shinyjs::reset("attribute_values")
      }
      loc$change_observer <- loc$change_observer + 1

    })
    
    # Attribute table event ----
    observeEvent(input$selected_attr, {
      user_attribute_name <- dplyr::tbl(glob$pool, "attributes") %>% 
        dplyr::filter(attribute_id == !!input$selected_attr) %>% 
        dplyr::pull(attribute_name)
      
      # TODO: check user permissions / attributes_other_modify
      # What permission should user have to delete user attribute?
      if(glob$user$data$attributes_other_modify == 1){
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
    
   
    # Delete attribute event from table ----
    observeEvent(input$delete_attr, {
   
      delete_user_attribute(pool = glob$pool, project_id = glob$active_project, 
                            user_id = glob$user$user_id, input$selected_attr)
      
      loc$change_observer <- loc$change_observer + 1
      
      removeModal()
    })

    # Delete attribute event from tools ----
    observeEvent(input$attribute_delete_btn, {


      delete_user_attribute(pool = glob$pool, project_id = glob$active_project, 
                            user_id = glob$user$user_id, input$attribute_delete)
      
          loc$change_observer <- loc$change_observer + 1


    })
    
    output$user_message <- renderText({
      "Here you can add user attributes for your users to self-report." 
    })
    
    #---Create attribute UI --------------
    mod_rql_button_server(
      id = "attribute_create_ui",
      custom_title = "Create attribute",
      custom_tagList = tagList(
          textInput(ns("attribute_name"), 
                  label = "Attribute name",
                  placeholder = "Write an attribute name"),
          textAreaInput(ns("attribute_values"), 
                      label = "Attribute values",
                      placeholder = "Write possible attribute values separated by comma"),
           actionButton(ns("add_attribute"), "Add user attribute")
      ),
      glob,
      permission = "attributes_modify"
    )
    #---Delete attribute UI --------------
    mod_rql_button_server(
      id = "attribute_delete_ui",
      custom_title = "Delete attribute",
      custom_tagList = tagList(
        rql_picker_UI(
          ns("attribute_delete"),
          label = "Select attributes:",
          choices = stats::setNames(
            loc$user_attributes$attribute_id,
            loc$user_attributes$attribute_name),
          multiple = FALSE,
          none = "Attributes to delete"
        ),
        actionButton(ns("attribute_delete_btn"), "Delete user attribute")
      ),
      glob,
      permission = "attributes_modify"
    )
    
  })
}