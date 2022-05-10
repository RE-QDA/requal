#' reproducibility UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_reproducibility_ui <- function(id){
  ns <- NS(id)
  tagList(
    "hello, reproducible world!", 
    actionButton(ns("test"), "test")
  )
}
    
#' reproducibility Server Functions
#'
#' @noRd 
mod_reproducibility_server <- function(id, project){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$test, {
      browser()
      
      segments <- load_all_segments_db(project_db = project()$project_db, 
                                       active_project = project()$active_project)
      # TODO: update function
      segments %>% 
        dplyr::count(user_id) %>% 
        dplyr::pull(n) %>% 
        mean()
    })
    
  })
}
    
## To be copied in the UI
# mod_reproducibility_ui("reproducibility_ui_1")
    
## To be copied in the server
# mod_reproducibility_server("reproducibility_ui_1")
