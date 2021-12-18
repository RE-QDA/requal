#' code_button UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_code_button_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' code_button Server Functions
#'
#' @noRd 
mod_code_button_server <- function(id, project){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    golem::invoke_js("highlight", list("yellow"))
    
    startOff <- as.numeric(unlist(strsplit(input$tag_position, "-")))[1]+1
    endOff <- as.numeric(unlist(strsplit(input$tag_position, "-")))[2]
    segment <- substr(text(), startOff, endOff)
    
    write_segment_db(project$active_project,
                     project$project_db,
                     doc_id = input$doc_selector,
                     code_id = 2,
                     startOff, 
                     endOff
    )
 
  })
}
    
## To be copied in the UI
# mod_code_button_ui("code_button_ui_1")
    
## To be copied in the server
# mod_code_button_server("code_button_ui_1")
