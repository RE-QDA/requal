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
  fluidRow(
    
    tags$head(
      
      tags$script(
        src = "www/document_code_js.js"
      ),
      
      tags$style(HTML(
        paste0("#",ns("code_list")), ", ",
        paste0("#",ns("focal_text")), "{
                    border: none;
                    height:80vh;
                    overflow-y:scroll;
                    }
                    ",
        ".code-button {
                      overflow: hidden;
                      text-overflow: ellipsis;
                      }
                    ",
        ".code-button:hover {
                      overflow: visible;
                      }
                      "))),
    
    
    column(width = 10,
    

    

    

    selectInput(ns("doc_selector"), label = "Select a document to code", 
                choices = "",
                selected = ""),
    
    htmlOutput(ns("focal_text")),

    textOutput(ns("captured_range")),
    
    verbatimTextOutput(ns("printLabel"))
  ),
  column(width = 2,
        
         tags$b("Codes"),
         br(),
         actionButton(ns("remove_codes"), 
                      "Remove code",
                      class = "btn-danger",
                      width = "100%"),
         br(), br(),
         uiOutput(ns("code_list"))
         

         
         )
  )
}
    
#' document_code Server Functions
#'
#' @noRd 
mod_document_code_server <- function(id, project, codebook){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    


# Selection of documents to code ------------------------------------------

    
    doc_choices <- reactiveVal()
    

# Refresh list of documents when documents are added/removed --------

    
    observeEvent(documents$doc_list, {
      if (isTruthy(project()$active_project)) {

      doc_choices(list_db_documents(project_db = project()$project_db,
                                    active_project = project()$active_project))

      updateSelectInput(session = session,
                        "doc_selector",
                        choices = c("", doc_choices())
                        )

        }
    })
    # 


# Displayed text ----------------------------------------------------------

    
    text <- reactiveVal("")

    observeEvent(input$doc_selector, {
    if (isTruthy(input$doc_selector)) {
      display_text <- load_doc_to_display(project()$active_project, 
                                          project()$project_db, 
                                          input$doc_selector, 
                                          code_df$active_codebook,
                                          ns=NS(id))
      text(display_text)
    } 
    })
    # Render selected text
    output$focal_text <- renderUI({
      
     HTML(text())
      
    })
    



# List out available codes ------------------------------------------------

    code_df <- reactiveValues()
    
    output$code_list <- renderUI({
      
      if (isTruthy(project()$active_project)) {
        
        if (isTruthy(codebook$active_codebook)) {
          code_df$active_codebook <- codebook$active_codebook
        } else {
          code_df$active_codebook <- list_db_codes(
            project()$project_db,
            project()$active_project
          )
        }
        
        purrr::pmap(
          list(
          code_df$active_codebook$code_id,
          code_df$active_codebook$code_name,
          code_df$active_codebook$code_color
          ),
          ~ generate_coding_tools(ns = ns, 
                                  code_id = ..1, 
                                  code_name = ..2,
                                  code_color = ..3)
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
        
        write_segment_db(project()$active_project,
                         project()$project_db,
                         doc_id = input$doc_selector,
                         code_id = input$selected_code,
                         startOff, 
                         endOff)
        
        display_text <- load_doc_to_display(project()$active_project, 
                                            project()$project_db, 
                                            input$doc_selector,
                                            code_df$active_codebook,
                                            ns=NS(id))
        text(display_text)
        
        
        }
    })
    
    
    
    

    
# Code removal ----------
    
    observeEvent(input$remove_codes, {
      
      req(project()$active_project)
      # browser()
      marked_segments_df <- load_segment_codes_db(
        active_project = project()$active_project,
        project_db = project()$project_db,
        active_doc = input$doc_selector,
        marked_codes = parse_tag_pos(
          input$tag_position,
          "start"
        )
      )
      if (nrow(marked_segments_df) == 0) {
        NULL
      } else if (nrow(marked_segments_df) == 1) {
        delete_segment_codes_db(
          project_db = project()$project_db,
          active_project = project()$active_project,
          doc_id = input$doc_selector,
          segment_id = marked_segments_df$segment_id
        )

        display_text <- load_doc_to_display(project()$active_project,
          project()$project_db,
          input$doc_selector,
          code_df$active_codebook,
          ns = NS(id)
        )
        text(display_text)
      } else {
        showModal(
          modalDialog(

            checkboxGroupInput(ns("codes_to_remove"),
              label = "",
              choiceValues = marked_segments_df$segment_id,
              choiceNames = marked_segments_df$code_name,
              selected = FALSE
            ),
            title = "Codes in this segment",
            "Remove selected codes from segment?",
            easyClose = TRUE,
            footer = tagList(
              modalButton("Dismiss"),
              actionButton(ns("remove_codes_select"),
                "Remove selected",
                class = "btn-danger"
              )
            ),
            fade = TRUE
          )
        )
      }
    })
    
    observeEvent(input$remove_codes_select, {
      
      if (isTruthy(input$codes_to_remove)) {
        
        delete_segment_codes_db(project_db =  project()$project_db,
                                active_project = project()$active_project, 
                                doc_id = input$doc_selector,
                                segment_id = input$codes_to_remove
        )
        removeModal()
        
        
        
        display_text <- load_doc_to_display(project()$active_project, 
                                            project()$project_db, 
                                            input$doc_selector,
                                            code_df$active_codebook,
                                            ns=NS(id))
        text(display_text)
      }
      
    })
  
#  # Helper (to be commented out in prod): position counter ---------------

  
    
    output$captured_range <- renderText({input$tag_position})
 
  })
}
