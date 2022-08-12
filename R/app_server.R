#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
    
    active_project <- reactiveVal()
    
    db_path <- golem::get_golem_options("db_path")
    
    glob  <- reactiveValues()
    
    pool <- pool::dbPool(
        drv = RSQLite::SQLite(),
        dbname = db_path
    )
    
    if (!DBI::dbExistsTable(pool, "projects")) {
        create_db_schema(pool)
    }

    project_loader <- mod_launchpad_loader_server("launchpad_loader_ui_1", pool)
    observeEvent(project_loader(), {
        active_project(project_loader())
    })
    
    project_creator <- mod_launchpad_creator_server("launchpad_creator_ui_1", pool)
    observeEvent(project_creator(), {
        active_project(project_creator())
    })

    observeEvent(active_project(), {
        updateControlbar("control_bar")
    })

    # documents  ----
    mod_project_server("mod_project_ui_1", pool, active_project, user)
    documents <- mod_doc_manager_server("doc_manager_ui_1", pool, active_project,
                                        user)

    # codebook  ----
    codebook <- mod_codebook_server("codebook_ui_1", pool, active_project, user)
    category <- mod_categories_server("categories_ui_1",
                                      pool, active_project,
                                      user, codebook)
    # workdesk ----
    segments_observer <- mod_document_code_server("document_code_ui_1",
                                                  pool, active_project,
                                                  user, codebook, documents)

    # analysis ----
    mod_analysis_server("analysis_ui_1", pool, active_project, user, glob,
                        codebook, category, documents, segments_observer)
    mod_download_handler_server("download_handler_ui_1", glob)
    mod_download_html_server("download_html_ui_1", glob)

    # reporting
    reporting <- mod_reporting_server("reporting_ui_1", pool, active_project, user)
    mod_reproducibility_server("reproducibility_ui_1", pool, active_project)

    # about -----
    mod_about_server("about_ui_1", pool, active_project, user)

    # user
    user <- mod_user_server("user_ui_1", pool, active_project)

    # memo
    mod_memo_server("memo_ui_1", pool, active_project, user)

}
