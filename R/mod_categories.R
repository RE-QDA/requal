#' categories UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_categories_ui <- function(id){
  ns <- NS(id)

    fluidRow(
      
      column(width = 8,
             
    fluidRow(
      column(
        width = 6,
        tags$h4("Orphan codes"),
        
        uiOutput(
          ns("uncategorized")
          )
      ),
      column(width = 6,
             tags$h4("Categories"),
             tags$br(),
             
             uiOutput(
               ns("categories_ui")
             )
             
             )
    )),
 
  column(width = 4,
         uiOutput(
           ns("categories_manager")
         ),
         
         verbatimTextOutput(ns("category_list_1")),
         verbatimTextOutput(ns("category_list_2")),
         actionButton(ns("test"), "test")
  )
         
  )
  
}
    
#' categories Server Functions
#'
#' @noRd 
mod_categories_server <- function(id, project){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    observeEvent(input$test, {browser()})
    
    output$category_list_1 <-
      renderPrint(
        input$category_list_1 # This matches the input_id of the second rank list
      )
    
    output$category_list_2 <-
      renderPrint(
        input$category_list_2 # This matches the input_id of the second rank list
      )
    

    output$uncategorized <- renderUI({

      
    
      sortable::rank_list(
        input_id = ns("code_list"),
        text = NULL,
        labels = render_codes(active_project = project()$active_project,
                                project_db = project()$project_db),
        class = "uncategorized",
        options = sortable::sortable_options(
          group = list(
            name = "categories",
            pull = "clone",
            put = TRUE
          ),
            onAdd = htmlwidgets::JS("function (evt) { this.el.removeChild(evt.item); }")
        )
      )
    })
    
    # List existing categories in category boxes ----
    output$categories_ui <- renderUI({
      
      render_categories(id = id,
                        active_project = project()$active_project,
                   project_db = project()$project_db)
      
      
    })
    
    #---Generate categories UI (if there is an active project)--------------
    output$codes_manager <- renderUI({
      if (isTruthy(project()$active_project)) {
        
        codebook_manager_UI(id,
                            project_db = project()$project_db,
                            project_id = project()$active_project)
      }
    })
    
    #---Generate categories UI (if there is an active project)--------------
    # output$categories_manager <- renderUI({
    #   if (isTruthy(project()$active_project)) {
    #     
    #     codebook_manager_UI(id,
    #                         project_db = project()$project_db,
    #                         project_id = project()$active_project)
    #   }
    # })
    
  })
}
    

