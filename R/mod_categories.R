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

  tagList(
    fluidRow(
      
      column(width = 6
      
      ),
      column(width = 6,
             
             actionButton(ns("new_category"), "Create category"),
             actionButton(ns("delete_category"), "Delete category")
             
    )
      ),

    fluidRow(
      column(width = 6,
             tags$br(),
             
        uiOutput(
          ns("uncategorized")
          )
      ),
      column(width = 6,
             tags$br(),
             
             uiOutput(ns("categories_manager")),
             
             uiOutput(
               ns("categories_ui")
             ),
             actionButton(ns("test"), "test")
             
             
             )
    )
         
  )
  
}
    
#' categories Server Functions
#'
#' @noRd 
mod_categories_server <- function(id, project, user){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    observeEvent(input$test, {browser()})
    

    # List existing codes in code boxes --------
    
    output$uncategorized <- renderUI({

      sortable::rank_list(
        input_id = ns("code_list"),
        text = NULL,
        labels = render_codes(active_project = project()$active_project,
                                project_db = project()$project_db),
        class = "codes-rank-list",
        options = sortable::sortable_options(
          sort = TRUE,
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
    
    observeEvent(input$new_category, {
      
      
      output$categories_manager <- renderUI({
        create_new_category_UI(id)
      })
      
      
    })
  
    observe(print(input$edges_category))
    
  })
}
    

