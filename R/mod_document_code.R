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
    
    htmlOutput(ns("focal_text")),

    textOutput(ns("captured_range")),
    
    verbatimTextOutput(ns("printLabel"))
  ),
  column(width = 2,
        
         tags$b("Codes"),
         br(),
        uiOutput(ns("code_list")),   

         
         )
  )
}
    
#' document_code Server Functions
#'
#' @noRd 
mod_document_code_server <- function(id, project){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    

# Selection of documents to code ------------------------------------------

    
    doc_choices <- reactiveVal()
    
    code_list <- reactiveValues()
    

# Refresh list of documents when documents are added/removed --------

    
    observeEvent(project$doc_list, {
      
      if (isTruthy(project$active_project)) {
        
      doc_choices(list_db_documents(project_db = project$project_db,
                                    active_project = project$active_project))

      updateSelectInput(session = session, 
                        "doc_selector",
                        choices = c("", doc_choices())
                        )
      
        }
    })
            
  
    

# Displayed text ----------------------------------------------------------

    
    text <- reactiveVal("")

    observeEvent(input$doc_selector, {
    if (isTruthy(input$doc_selector)) {
      display_text <- load_doc_to_display(project$active_project, 
                                          project$project_db, 
                                          input$doc_selector, ns=NS(id))
      text(display_text)
    } 
    })
    # Render selected text
    output$focal_text <- renderUI({
      
     HTML(text())
      
    })
    

    

# List out available codes ------------------------------------------------

    code_df <-reactiveVal()
    
    
    output$code_list <- renderUI({
    
      if (isTruthy(project$active_project)) {
        
        code_df <- list_db_codes(project$project_db, project$active_project)

        purrr::map2(
          
        .x = code_df$code_id,
        .y = code_df$code_name, 
        ~ generate_coding_tools(ns = ns, code_id = .x, code_name = .y)
              )
        
        
      } else {
        
        ""
      }
    
    })
    
    
    # Coding tools ------------------------------------------------------------

    observeEvent(input$selected_code, {
      
      req(input$selected_code, input$tag_position)
      
      # browser()
      # golem::invoke_js("highlight", list("yellow"))
      
      startOff <- parse_tag_pos(input$tag_position, "start")
      endOff <- parse_tag_pos(input$tag_position, "end")
      
      if (endOff - startOff > 1 & endOff > startOff) {
        
        write_segment_db(project$active_project,
                         project$project_db,
                         doc_id = input$doc_selector,
                         code_id = input$selected_code,
                         startOff, 
                         endOff)
        
        display_text <- load_doc_to_display(project$active_project, 
                                            project$project_db, 
                                            input$doc_selector,ns=NS(id))
        text(display_text)
        
        
        }
    })
    
    
    
    
# # Pop-up menu ------
    
    observeEvent(input$marked_code, {
      
      if (grepl("\\+", input$marked_code)) {
      marked_code_parsed <- as.integer(unlist(strsplit(input$marked_code, split = "\\+")))
      } else {
      marked_code_parsed <- as.integer(input$marked_code)
      }
      
      #TODO make this work with segment ids
      marked_segments <- load_segment_codes_db(active_project = project$active_project,
                                               project_db = project$project_db,
                                               marked_codes = marked_code_parsed)
      
      #print(marked_segments)
      
      showModal(
            modalDialog(

            checkboxGroupInput(ns("codes_to_remove"),
                               label = "",
                               choices = marked_segments,
                               selected = TRUE),

            title = "Codes in this segment",
            "Remove selected codes from segment?",
            easyClose = TRUE,
            footer = tagList(
              modalButton("Dismiss"),
              actionButton(ns("remove_codes"), "Remove", class = "btn-danger")
              ),
            fade = TRUE
          )


          )
      
      })

    
    # Code removal ----------
    
    observeEvent(input$remove_codes, {
      
#      browser()
      
      delete_segment_codes_db(project_db =  project$project_db,
                              active_project = project$active_project, 
                              doc_id = input$doc_selector,
                              code_id = input$codes_to_remove,
                              startOff <- parse_tag_pos(input$tag_position, "start")
                              )
      
      removeModal()
      
      display_text <- load_doc_to_display(project$active_project, 
                                          project$project_db, 
                                          input$doc_selector,ns=NS(id))
      text(display_text)
      
      
    })
  
#  # Helper (to be commented out in prod): position counter ---------------

  
    
    output$captured_range <- renderText({input$tag_position})
 
  })
}
