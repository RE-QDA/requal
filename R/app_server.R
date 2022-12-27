#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  setup <- reactiveValues()
  setup$mode <- ifelse(Sys.getenv("mode") == "server", "server", "local")

  # check_credentials returns a function to authenticate users
  auth <- shinymanager::secure_server(
      check_credentials = shinymanager::check_credentials(
          db = golem::get_golem_options(which = "credentials_path"),
          passphrase = golem::get_golem_options(which = "credentials_pass")),
      timeout = 60
  )  
  
  observeEvent(auth, {
      setup$auth <- reactiveValuesToList(auth)
      glob$user <- as.integer(setup$auth$user_id)
  })
        
  glob <- reactiveValues()

  mod_launchpad_loader_server("launchpad_loader_ui_1", glob, setup)

  mod_launchpad_creator_server("launchpad_creator_ui_1", glob, setup)

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
