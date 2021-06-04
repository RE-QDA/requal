#' document_code UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_document_code_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    tags$head(
      tags$script(
        src = "www/document_code_js.js"
      )
    ),
    
    textOutput(ns("focal_text")),



    
    textOutput(ns("captured_range"))
  )
}
    
#' document_code Server Functions
#'
#' @noRd 
mod_document_code_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    lorem_ipsum_input <- "Lorem ipsum dolor sit amet, consectetur adipisici elit, sed eiusmod tempor incidunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquid ex ea commodi consequat. Quis aute iure reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint obcaecat cupiditat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
    
    output$focal_text <- renderText({lorem_ipsum_input})




    # observeEvent(input$tag_position, {
    # print(input$tag_position)
    # })
    
    output$captured_range <- renderText({input$tag_position})
 
  })
}
