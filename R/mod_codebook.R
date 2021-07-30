#' codebook UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_codebook_ui <- function(id) {
  ns <- NS(id)
  
  project_codes <- list_db_codes()

  codes_ui <- purrr::pmap(project_codes, gen_codes_ui)
  
  tagList(
    "Management of codes. Including creation, deletion, merges.",
    fluidRow(column(width = 6,
                    codes_ui),
             column(width = 6,
                    
                    textAreaInput(ns("doc_text"), label = NULL, placeholder = "Paste a new document content here"),
                    actionButton(ns("code_add"), label = "Add document")
                    
                    
                    ))
    
  )
}

#' codebook Server Functions
#'
#' @noRd
mod_codebook_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
  })
}

## To be copied in the UI
# mod_codebook_ui("codebook_ui_1")

## To be copied in the server
# mod_codebook_server("codebook_ui_1")
