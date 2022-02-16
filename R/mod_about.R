#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_about_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    h2(
      tags$a(href="https://github.com/RE-QDA/requal",
    tags$img(src="www/requal_logo.png", width = "5%"), target = "_blank"),
    "ReQual CAQDAS"
    ), 
    
    p(),
    
    p("The development of this tool has been supported by", 
      a(href = "https://www.tacr.cz/en/technology-agency-of-the-czech-republic", 
        "The Technology Agency of the Czech Republic",
        target = "_blank"), 
      "project n. TL05000054."
      )
 
  )
}
    
#' about Server Functions
#'
#' @noRd 
mod_about_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_about_ui("about_ui_1")
    
## To be copied in the server
# mod_about_server("about_ui_1")
