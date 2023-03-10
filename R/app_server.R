#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  glob <- reactiveValues()

  # check_credentials returns a function to authenticate users
  auth <- shinymanager::secure_server(
      check_credentials = shinymanager::check_credentials(
          db = golem::get_golem_options(which = "credentials_path"),
          passphrase = golem::get_golem_options(which = "credentials_pass")),
      timeout = 60
  )  

  observeEvent(auth, {
      glob$user$user_login <- auth$user
      glob$user$user_id <- as.integer(auth$user_id)
      glob$user$is_admin <- as.logical(auth$admin)
      glob$user$name <- auth$user_name
      glob$user$mail <- auth$user_mail
      glob$user$project_owner <- as.logical(auth$project_owner)
  })
        

  mod_launchpad_loader_server("launchpad_loader_ui_1", glob)

  mod_launchpad_creator_server("launchpad_creator_ui_1", glob)

  observeEvent(glob$active_project, {
    updateControlbar("control_bar")
  })

  # documents  ----
  mod_project_server("mod_project_ui_1", glob)
  # output: glob$documents
  mod_doc_manager_server("doc_manager_ui_1", glob)
  # output: no output, permissions and membership written to DB
  # ---- to prevent manipulation via UI
  mod_use_manager_server("use_manager_1", glob)
  
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
  mod_user_attributes_server("user_attributes_ui_1", glob)
  mod_agreement_server("agreement_ui_1", glob)
  mod_browser_server("browser_ui_1", glob)
  mod_summary_server("summary_ui_1", glob)

  # about -----
  mod_about_server("about_ui_1", glob)

  # user
  mod_user_server("user_ui_1", glob)

  # memo
  mod_memo_server("memo_ui_1", glob)
}
