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
  div(
    tags$b("Free memo"),
    shinyjs::hidden(div(id = ns("free_memo_div"),
      mod_memo_editor_ui(ns("memo_free_editor"))
    )),
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
    loc <- reactiveValues()
    glob$free_memo_observer <- 0
    mod_memo_editor_server("memo_free_editor", glob, type = "free_memo")
    observeEvent(req(glob$active_project), {
    glob$free_memo_observer <- glob$free_memo_observer + 1
    shinyjs::show("free_memo_div")
    })

    output$memo <- DT::renderDataTable({loc$memos_table})

    ## Observe free_memo_edit_click ----
    observeEvent(req(input$text_memo_click), {
      req(glob$free_memo_observer > 0)
      golem::invoke_js("updateEditorInput", 
              list(ns_memo_id = paste0(ns("memo_free_editor"), "-memo_id"),
                    id = parse_memo_id(input$text_memo_click)))
      golem::invoke_js("resetMemoClick", 
              list(ns_text_memo_click = ns("text_memo_click")))
    })

    observeEvent(glob$free_memo_observer, {
      req(glob$free_memo_observer > 0)
      loc$visible_memos <- list_memo_records(glob$pool, glob$active_project)
        if(glob$user$data$memo_other_view == 0 && nrow(visible_memos) > 0){
          loc$visible_memos <- loc$visible_memos %>% 
            dplyr::filter(user_id == glob$user$user_id) 
        }
      memos_segments_map_vec <- dplyr::tbl(glob$pool, "memos_segments_map") %>%
          dplyr::pull(memo_id)

      loc$memos_table <- DT::datatable(loc$visible_memos %>%
          dplyr::filter(!memo_id %in% memos_segments_map_vec)  %>% # filter for free memos
          dplyr::arrange(dplyr::desc(memo_id)) %>%
          dplyr::mutate(memo_name = memo_link(ns("text_memo_click"), memo_id, memo_name)) %>%
          dplyr::select(memo_name),
        options = memo_table_options(),
        class = "compact free_memo_table",
        escape = FALSE,
        rownames = FALSE,
        colnames = NULL,
        selection = "none"
        )
      
    })
    
  })
}
