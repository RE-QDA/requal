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
    inputId = ifelse(is.null(inputId), paste0(id, "-rql_button_id"), inputId)
  ) %>% tagAppendAttributes(style = "padding-right: 5px; padding-top: 10px; top: 1vh; position: relative; min-width: 50%;")

  )
}
    
#' rql_button Server Functions
#'
#' @noRd 
mod_rql_button_server <- function(id, custom_title, custom_tagList, glob = NULL, permission = TRUE){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    

  output$rql_button <- renderUI({
    # We provide default permission as TRUE
    # this way we have to opt-in into permission checks
    # otherwise the module just creates menu buttons
    if (!is.logical(permission)) {
    req(glob$active_project)
    validate(
      need(glob$user$data[[permission]] == TRUE, 'Insufficent permission.')
    )
    }
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
