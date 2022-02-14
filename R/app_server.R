#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  
  project <- mod_launchpad_loader_server("launchpad_loader_ui_1")

  observeEvent(project$active_project, {
  updateControlbar("control_bar")
  })
  
  mod_project_server("project_ui_1")

  codebook <- mod_codebook_server("codebook_ui_1", project)

  mod_document_code_server("document_code_ui_1", 
                           project,
                           codebook)
  
  
  
}
