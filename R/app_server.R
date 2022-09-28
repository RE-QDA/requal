#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  glob <- reactiveValues()

  project_observer <- reactiveVal()

  project_loader <- mod_launchpad_loader_server("launchpad_loader_ui_1", glob)
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
  #
  #   mod_project_server("mod_project_ui_1", project_observer, user)
  #   documents <- mod_doc_manager_server("doc_manager_ui_1", project_observer, user)

  # codebook  ----

  # codebook <- mod_codebook_server("codebook_ui_1", project_observer, user)
  # category <- mod_categories_server("categories_ui_1",
  #                                   project_observer,
  #                                   user,
  #                                   codebook)

  # workdesk ----

  # segments_observer <- mod_document_code_server("document_code_ui_1", project_observer, user, codebook, documents)

  # analysis ----

  # mod_analysis_server("analysis_ui_1", project_observer, user, glob, codebook, category, documents, segments_observer)
  #
  #   mod_download_handler_server("download_handler_ui_1", glob)
  #   mod_download_html_server("download_html_ui_1", glob)
  # reporting

  # reporting <- mod_reporting_server("reporting_ui_1", project_observer, user)
  # mod_reproducibility_server("reproducibility_ui_1", project_observer)

  # about -----
  # mod_about_server("about_ui_1", project_observer, user)

  # user
  user <- mod_user_server("user_ui_1", glob, project_observer)

  # memo

  # mod_memo_server("memo_ui_1", project_observer, user)
}
