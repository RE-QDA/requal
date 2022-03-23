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

 
    uiOutput(ns("memo")) 
     
  
}

#' memo Server Functions
#'
#' @noRd
mod_memo_server <- function(id, project) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    memo_list <- reactiveVal()
    observeEvent(project()$active_project, {
     memo_list(list_memo_records(project))
    })
   

    output$memo <- renderUI({
      
      if (isTruthy(project()$active_project)) {
        tagList(
        tags$b("Memos"),
        actionButton(ns("new_memo"), "New memo", style="display: inline-block; float: right"),
        hr(),
        render_memos(id, memo_list())
        )
      } else {
        "No active project."
      }
    })
    
    
    # new memo ----
    observeEvent(input$new_memo, {
      
      showModal(
        
        modalDialog(
          title = "New memo",
          
          textAreaInput(ns("memo_text"), "Text",
                    value = "", width = "100%"
          ),
          
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("save_close"), "Save & Close")
          )
          
        )
        
      )
    })
    
    observeEvent(input$save_close, {
      
      add_memo_record(project, input$memo_text)
      
      memo_list(list_memo_records(project))
               
      removeModal()
    })
    
    
    # Display selected memo -----
    observeEvent(input$selected_memo, {
      showModal(
    
        modalDialog(
          title = paste0(memo_list() %>% 
                           dplyr::filter(memo_id == as.integer(input$selected_memo)) %>% 
                           dplyr::pull(memo_name)),
          
          textAreaInput(ns("displayed_memo_text"), "Text",
                        value = read_memo_db(project, input$selected_memo), 
                        width = "100%"
          ),
          
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("save_changes"), "Save & Close"),
            actionButton(ns("delete_memo"), "Delete", class = "btn-danger")
          )
          
        )
        
      )
    })
    
    observeEvent(input$save_changes, {
      
      update_memo_record(project, input$selected_memo, input$displayed_memo_text)
      
      memo_list(list_memo_records(project))
      
      removeModal()
    })
    
    observeEvent(input$delete_memo, {
      
      delete_memo_record(project, input$selected_memo)
      
      memo_list(list_memo_records(project))
      
      removeModal()
    })
    
  
    

    
  }) 

}


