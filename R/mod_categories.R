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
      tags$head(
        tags$style(HTML(".bucket-list-container {min-height: 90%;
                            border: none;
                            height:30vh;
                            overflow-y:scroll;}"))
      ),
      
      column(width = 8,
    fluidRow(
      column(
        width = 12,
        tags$h3("Uncategorized codes"),
        
        sortable::bucket_list(
          header = NULL,
          group_name = "bucket_list_group",
          orientation = "horizontal",
          sortable::add_rank_list(
            input_id = ns("rank_list_1"),
            text = "Assign codes to categories",
            labels = list(
              "one",
              "two",
              "three",
              htmltools::tags$div(
                htmltools::em("Complex"), " html tag without a name"
              ),
              "five" = htmltools::tags$div(
                htmltools::em("Complex"), " html tag with name: 'five'"
              )
            )
          ))
      )
    ),
  
  column(width = 4,
         uiOutput(
           ns("categories_manager")
         )
  )))
  
}
    
#' categories Server Functions
#'
#' @noRd 
mod_categories_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    output$results_1 <-
      renderPrint(
        input$rank_list_1 # This matches the input_id of the second rank list
      )
    
    output$results_2 <-
      renderPrint(
        input$rank_list_2 # This matches the input_id of the second rank list
      )
    
    output$results_3 <-
      renderPrint(
        input$rank_list_3 # This matches the input_id of the second rank list
      )

    
  })
}
    

