#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
    
    active_project <- reactiveVal()
    pool <- pool::dbPool(
        drv = RSQLite::SQLite(),
        dbname = golem::get_golem_options("db_path")
    )
    
    active_project(dplyr::tbl(pool, "projects") %>% 
        dplyr::pull(project_id) %>% head(1))

    glob  <- reactiveValues()

    project_observer <- reactiveVal()
 
 # project_loader <- mod_launchpad_loader_server("launchpad_loader_ui_1")
 #    observeEvent(project_loader(), {
 #    project_observer(project_loader())
 # 
 #  })
 # 
 #  project_creator <- mod_launchpad_creator_server("launchpad_creator_ui_1")
 #    observeEvent(project_creator(), {
 #      project_observer(project_creator())
 #    })

    observeEvent(active_project(), {
        updateControlbar("control_bar")
    })

    # documents  ----
    # mod_project_server("mod_project_ui_1", project_observer, user)
    # documents <- mod_doc_manager_server("doc_manager_ui_1", project_observer, user)

  # mod_project_server("mod_project_ui_1", project_observer, user)
  # documents <- mod_doc_manager_server("doc_manager_ui_1", project_observer, user)

  # codebook  ----

  # codebook <- mod_codebook_server("codebook_ui_1", project_observer, user)
  # category <- mod_categories_server("categories_ui_1",
  #                                   project_observer,
  #                                   user,
  #                                   codebook)

  # workdesk ----

  # segments_observer <- mod_document_code_server("document_code_ui_1", project_observer, user, codebook, documents)

  # analysis ----
  mod_analysis_server("analysis_ui_1", active_project, user, glob, codebook, category, documents, segments_observer)
  # segments_df <- mod_analysis_server("analysis_ui_1", active_project, user, codebook, category, documents, segments_observer)

  mod_download_handler_server("download_handler_ui_1", glob)
  mod_download_html_server("download_html_ui_1", glob)
  # reporting

  # reporting <- mod_reporting_server("reporting_ui_1", project_observer, user)
  # mod_reproducibility_server("reproducibility_ui_1", project_observer)

    # about -----
    mod_about_server("about_ui_1", pool, active_project, user)
    
    # user
    user <- mod_user_server("user_ui_1", pool, active_project)
    
    # memo
    mod_memo_server("memo_ui_1", pool, active_project, user)

}
