#' memo UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_memo_free_ui <- function(id) {
  ns <- NS(id)
    print(ns("text_memo_click"))

  div(
    tags$b("Free memo"),
    mod_memo_editor_ui(ns("memo_free_editor")),
    hr(),
    DT::dataTableOutput(ns("memo"))
  )
}

#' memo Server Functions
#'
#' @noRd
mod_memo_free_server <- function(id, glob) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    memo_list <- reactiveVal()
    loc <- reactiveValues()
    mod_memo_editor_server("memo_free_editor", glob, type = "free")

    ## Observe free_memo_edit_click ----
    observeEvent(req(input$text_memo_click), {
      golem::invoke_js("updateEditorInput", 
              list(ns_memo_id = paste0(ns("memo_free_editor"), "-memo_id"),
                    id = parse_memo_id(input$text_memo_click)))
      golem::invoke_js("resetMemoClick", 
              list(ns_text_memo_click = ns("text_memo_click")))
    })

    output$memo <- DT::renderDataTable({
      if (isTruthy(glob$active_project)) {
        visible_memos <- list_memo_records(glob$pool, glob$active_project)
        if(glob$user$data$memo_other_view == 0 && nrow(visible_memos) > 0){
          visible_memos <- visible_memos %>% 
            dplyr::filter(user_id == glob$user$user_id) 
        }
        
        memo_list(visible_memos) 
        
        DT::datatable(memo_list() %>%
          dplyr::arrange(dplyr::desc(memo_id)) %>%
          dplyr::mutate(memo_name = memo_link(memo_id, memo_name)) %>%
          dplyr::select(memo_name),
        options = memo_table_options(),
        class = "compact",
        escape = FALSE,
        rownames = FALSE,
        colnames = NULL,
        selection = "none"
        )
      }
    })


 
    
  })
}
