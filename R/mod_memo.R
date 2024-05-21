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
  div(
    tags$b("Memos"),
    uiOutput(ns("new_memo_btn")) %>% 
        tagAppendAttributes(style = "display: inline-block; float: right"),
    hr(),
    DT::dataTableOutput(ns("memo")), 
    hr(), 
    downloadButton(ns("export_memo"), label = "Export memos") %>% 
        tagAppendAttributes(style = "display: inline-block; float: right")
  ) %>% tagAppendAttributes(class = "scrollable80")
}

#' memo Server Functions
#'
#' @noRd
mod_memo_server <- function(id, glob) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    memo_list <- reactiveVal()
    loc <- reactiveValues()
    
    output$new_memo_btn <- renderUI({
      req(glob$user$data)
      if(glob$user$data$memo_modify == 1){
        actionButton(ns("new_memo"), "New memo") 
      }
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


    # new memo ----
    observeEvent(input$new_memo, {
      showModal(
        modalDialog(
          title = "New memo",
          textAreaInput(ns("memo_text"), "Text",
            value = "", width = "100%", height = "100%",
            placeholder = "First 50 characters of the first line will become a searchable title..."
          ) %>% tagAppendAttributes(style = "height: 50vh"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("save_close"), "Save & Close", class = "btn-success")
          )
        )
      )
    })

    observeEvent(input$save_close, {
      add_memo_record(
        pool = glob$pool,
        project = glob$active_project,
        text = req(input$memo_text),
        user_id = glob$user$user_id
      )
      
      visible_memos <- list_memo_records(glob$pool, glob$active_project)
      if(glob$user$data$memo_other_view == 0 && nrow(visible_memos) > 0){
        visible_memos <- visible_memos %>% 
          dplyr::filter(user_id == glob$user$user_id) 
      }
      memo_list(visible_memos)

      removeModal()
    })

    # Display selected memo -----
    observeEvent(input$selected_memo, {
      loc$memo <- read_memo_by_id(glob$pool, glob$active_project, input$selected_memo)
      can_modify <- find_memo_permission(loc$memo$user_id, glob$user)
      
      showModal(
        modalDialog(
          title = loc$memo$memo_name,
          textAreaInput(ns("displayed_memo_text"), "Text",
            value = loc$memo$memo_text,
            width = "100%", height = "100%",
            placeholder = "First 50 characters of the first line will become a searchable title..."
          ) %>% tagAppendAttributes(style = "height: 50vh"),
          footer = tagList(
            modalButton("Close"),
            if(can_modify){ 
              list(
                actionButton(ns("save_changes"), "Save & Close", class = "btn-success"), 
                actionButton(ns("delete_memo"), "Delete", class = "btn-danger")  
              )
            }
          )
        )
      )
    })

    observeEvent(input$save_changes, {
      update_memo_record(glob$pool, glob$project, 
                         input$selected_memo, req(input$displayed_memo_text),
        user_id = glob$user$user_id
      )
      
      visible_memos <- list_memo_records(glob$pool, glob$active_project)
      if(glob$user$data$memo_other_view == 0 && nrow(visible_memos) > 0){
        visible_memos <- visible_memos %>% 
          dplyr::filter(user_id == glob$user$user_id) 
      }
      memo_list(visible_memos)

      removeModal()
    })

    # Delete memo ----

    observeEvent(input$delete_memo, {
  
      showModal(
        modalDialog(
          title = "Are you sure?",
            tags$span("You are about to delete the memo and lose its content:",
            tags$b(loc$memo$memo_name)),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Dismiss"),
            actionButton(ns("delete_memo_confirmation"),
              "Yes, I am sure.",
              class = "btn-danger"
            )
          ),
          fade = TRUE
        )
      )
    })

    observeEvent(input$delete_memo_confirmation, {
       delete_memo_record(glob$pool, glob$active_project, input$selected_memo,
        user_id = glob$user$user_id
      )
      
      visible_memos <- list_memo_records(glob$pool, glob$active_project)
      if(glob$user$data$memo_other_view == 0 && nrow(visible_memos) > 0){
        visible_memos <- visible_memos %>% 
          dplyr::filter(user_id == glob$user$user_id) 
      }
      memo_list(visible_memos)

      removeModal()
  
    })

    # Memo export ----
    output$export_memo <- downloadHandler(
        filename = function() {
            "requal_memo_export.csv"
        },
        content = function(file) {
            memos <- export_memos(glob$pool, glob$active_project)
            utils::write.csv(memos, file)
        }
    )
    
  })
}
