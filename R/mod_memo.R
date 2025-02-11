#' memo UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_memo_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
 div(
  style = "display: flex; align-items: flex-start; margin-left:: 30px;",
  div(
    style = "min-width: 40vh;",
    mod_memo_editor_ui(ns("memo_main_editor"))
  ),
  div(
    style = "margin-left: 10px;",  # Adjust the spacing between the editor and the thumbtack if needed
    actionButton(ns("pin"), "", icon = icon("thumbtack"), class = "pinned")
  )
),
    hr(),
    fluidRow(
      div(style = "margin-left: 30px; overflow-x: scroll",
    DT::dataTableOutput(ns("memo"))
      )
    )
    # downloadButton(ns("export_memo"), label = "Export memos") %>% 
    #     tagAppendAttributes(style = "display: inline-block; float: right", class = "scrollable80")
  )
}

#' memo Server Functions
#'
#' @noRd
mod_memo_server <- function(id, glob) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    loc <- reactiveValues()
    mod_memo_editor_server("memo_main_editor", glob, type = "free_memo")

    ## Observe free_memo_edit_click ----
    observeEvent(req(input$text_memo_click), {
      req(glob$free_memo_observer > 0)
      golem::invoke_js("updateEditorInput", 
              list(ns_memo_id = paste0(ns("memo_main_editor"), "-memo_id"),
                    id = parse_memo_id(input$text_memo_click)))
      golem::invoke_js("resetMemoClick", 
              list(ns_text_memo_click = ns("text_memo_click")))
    })

    output$new_memo_btn <- renderUI({
      req(glob$user$data)
      if(glob$user$data$memo_modify == 1){
        actionButton(ns("new_memo"), "New memo") 
      }
    })

    output$memo <- DT::renderDataTable({
      if (isTruthy(glob$active_project)) {
        memo_table <- list_memo_records(glob$pool, glob$active_project)
        if(glob$user$data$memo_other_view == 0 && nrow(memo_table) > 0){
          memo_table <- memo_table %>% 
            dplyr::filter(user_id == glob$user$user_id) 
        }
        req(nrow(memo_table) > 0)
        enriched_memo_table <- memo_table  |> 
          dplyr::left_join(
            memos_segments_map <- dplyr::tbl(pool, "memos_segments_map") |>
              dplyr::filter(memo_id %in% !!memo_table$memo_id) |>
              dplyr::collect()
            ) |> 
          dplyr::left_join(
            segment_df <- dplyr::tbl(pool, "segments") %>%
              dplyr::select(segment_id, doc_id) |> 
              dplyr::filter(.data$segment_id %in% !!memos_segments_map$segment_id) %>%
              dplyr::collect()
          ) |> 
         dplyr::left_join(
            documents_df <- dplyr::tbl(pool, "documents") %>%
              dplyr::select(doc_id, doc_name) %>%
              dplyr::filter(.data$doc_id %in% !!segment_df$doc_id) %>%
              dplyr::collect()
         ) |> 
          dplyr::mutate(
            memo_text = memo_name,
            memo_name = memo_link(ns("text_memo_click"), memo_id, memo_name)
            ) |> 
            dplyr::arrange(dplyr::desc(memo_id))
            
              DT::datatable(
        enriched_memo_table,
        filter = "top",
        escape = FALSE,
        extensions = c("Buttons"),
        options = dt_memo_options(),
        class = "display",
        selection = "none"
      )
      }
    })

    # pin ----
       observeEvent(input$pin, {
        
        insertUI("div.content-wrapper",  where = "afterBegin",
        div(id = "pinned_memo", 
            div(id = "pin_header", icon("thumbtack")),
            div("Memo content")
           )
        )
        golem::invoke_js("makeDraggable", list(id = "pinned_memo"))

      })

    # # Memo export ----
    # output$export_memo <- downloadHandler(
    #     filename = function() {
    #         "requal_memo_export.csv"
    #     },
    #     content = function(file) {
    #         memos <- export_memos(glob$pool, glob$active_project)
    #         utils::write.csv(memos, file)
    #     }
    # )
    
  })
}
