#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  
 project_observer <- reactiveVal()
  
 project_loader <- mod_launchpad_loader_server("launchpad_loader_ui_1")
    observeEvent(project_loader(), {
    project_observer(project_loader())

  })

  project_creator <- mod_launchpad_creator_server("launchpad_creator_ui_1")
    observeEvent(project_creator(), {
      project_observer(project_creator())
    })

  observeEvent(project_observer()$active_project, {
  updateControlbar("control_bar")
  })

  # documents  ----
  
  documents <- mod_project_server("mod_project_ui_1", project_observer)
  
  # codebook  ----
  
  codebook <- mod_codebook_server("codebook_ui_1", project_observer)
  
  # workdesk ----
 
  mod_document_code_server("document_code_ui_1", project_observer, codebook, documents)
  
  # analysis
  
  mod_analysis_server("analysis_ui_1", project_observer, codebook, documents)

  
}
