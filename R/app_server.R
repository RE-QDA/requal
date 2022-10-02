#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  setup <- reactiveValues()
  mode$mode <- golem::get_golem_options("mode")
  
  observeEvent(req(setup$mode == "server") {
      # call the server part
      # check_credentials returns a function to authenticate users
      setup$auth <- secure_server(
          check_credentials = check_credentials(credentials)
      )
      print(setup$auth)
  })
        
  glob <- reactiveValues()

  mod_launchpad_loader_server("launchpad_loader_ui_1", glob, mode)

  mod_launchpad_creator_server("launchpad_creator_ui_1", glob, mode)

  observeEvent(glob$active_project, {
    updateControlbar("control_bar")
  })

  # documents  ----
  mod_project_server("mod_project_ui_1", glob)
  # output: glob$documents
  mod_doc_manager_server("doc_manager_ui_1", glob)

  # codebook  ----
  # output: glob$codebook
  mod_codebook_server("codebook_ui_1", glob)
  # output: glob$category
  mod_categories_server("categories_ui_1", glob)
  # workdesk ----
  # output: glob$segments
  mod_document_code_server("document_code_ui_1", glob)

  # analysis ----
  mod_analysis_server("analysis_ui_1", glob)
  mod_download_handler_server("download_handler_ui_1", glob)
  mod_download_html_server("download_html_ui_1", glob)
  
  # reporting
  mod_reporting_server("reporting_ui_1", glob)
  mod_reproducibility_server("reproducibility_ui_1", glob)

  # about -----
  mod_about_server("about_ui_1", glob)

  # user
  mod_user_server("user_ui_1", glob)

  # memo
  mod_memo_server("memo_ui_1", glob)
}
