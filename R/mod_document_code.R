#' document_code UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_document_code_ui <- function(id){
  ns <- NS(id)
  fluidRow(column(width = 10,
    
    tags$head(
      tags$script(
        src = "www/document_code_js.js"
      )
    ),
    
    selectInput(ns("doc_selector"), label = "Documents", 
                choices = "",
                selected = ""),
    
    actionButton(ns("refresh"), label = "Refresh"),
    
    
    htmlOutput(ns("focal_text")),

    textOutput(ns("captured_range"))
  ),
  column(width = 2,
         
         
         selectInput(ns("code_id"), label = "Code", choices = ""),
         
         actionButton(ns("add_code"), label = "code")
         
         
        # uiOutput(ns("code_list"))
         
         )
  )
}
    
#' document_code Server Functions
#'
#' @noRd 
mod_document_code_server <- function(id, project){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Selection of documents to code
    
    doc_choices <- reactiveVal("")
    
    code_list <- reactiveValues()
    
    # Refresh
    
    observeEvent(input$refresh, {
      doc_choices <- read_doc_db(project$active_project, project$project_db)
      
      updateSelectInput(session = session, "doc_selector", choices = doc_choices)
      
      code_df <- list_db_codes(project$project_db, project$active_project)
      
      updateSelectInput(session = session, 
                        "code_id", 
                        choices = setNames(code_df$code_id, code_df$code_name)
                        )
      
      
      code_list$code_id <- code_df$code_id
      code_list$code_name <- code_df$code_name
      
            })
    
    # Displayed text
    text <- reactiveVal("")
    
    observeEvent(input$doc_selector, {
    if (isTruthy(input$doc_selector)) {
      display_text <- load_doc_to_display(project$active_project, 
                                          project$project_db, 
                                          input$doc_selector)
      text(display_text)
    } 
    })
    # Render selected text
    output$focal_text <- renderUI({
      
     HTML(text())

      
    })
    
    # Coding tools
    observeEvent(input[["add_code"]], {
      
      # browser()
      # golem::invoke_js("highlight", list("yellow"))
      
      startOff <- as.numeric(unlist(strsplit(input$tag_position, "-")))[1]+1
      endOff <- as.numeric(unlist(strsplit(input$tag_position, "-")))[2]
      doc <- load_doc_db(project$active_project,
                         project$project_db,
                         doc_id = input$doc_selector)
      segment <- substr(doc, startOff, endOff)
      
      write_segment_db(project$active_project,
                       project$project_db,
                       doc_id = input$doc_selector,
                       code_id = input$code_id,
                       segment, 
                       startOff, 
                       endOff
                       )
      
      display_text <- load_doc_to_display(project$active_project, 
                                          project$project_db, 
                                          input$doc_selector)
      text(display_text)
      
    })
    
    output$code_list <- renderUI({
      
      if (!is.null(code_list$code_id)) {
        print(code_list)

        purrr::map2(paste0("code_id_", code_list$code_id), 
                    code_list$code_name, 
                    ~actionButton(inputId = .x,
                                  label = .y)
                  )
      } else {""}
      
    })

    
    # Helper (to be commented out)
    
    output$captured_range <- renderText({input$tag_position})
 
  })
}
