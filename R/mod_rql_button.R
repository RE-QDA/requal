#' rql_button UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_rql_button_ui <- function(id, label, icon, inputId = NULL){
  ns <- NS(id)
  tagList(
 
  shinyWidgets::dropdown(
   uiOutput(ns("rql_button")),
    label = NULL,
    style = "material-circle",
    tooltip = shinyWidgets::tooltipOptions(
      placement = "right",
      title = label,
      html = FALSE
    ),
    size = "md", 
    width = "370px",
    icon = icon(icon, verify_fa = FALSE) %>% tagAppendAttributes(style = "color: #3c8dbc"), 
    right = FALSE,
    inputId = inputId
  ) %>% tagAppendAttributes(style = "padding-right: 5px; padding-top: 10px; top: 1vh; position: relative; min-width: 50%;")

  )
}
    
#' rql_button Server Functions
#'
#' @noRd 
mod_rql_button_server <- function(id, custom_title, custom_tagList){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$rql_button <- renderUI({
    tags$div(
        h4(custom_title),
        custom_tagList
    ) %>% tagAppendAttributes(style = "text-align: left")
    })
    outputOptions(output, "rql_button", suspendWhenHidden = FALSE)
    
 
  })
}
    
## To be copied in the UI
# mod_rql_button_ui("rql_button_1")
    
## To be copied in the server
# mod_rql_button_server("rql_button_1")
