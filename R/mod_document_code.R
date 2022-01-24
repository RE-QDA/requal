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
    
    shinyjs::useShinyjs(),
    
    selectInput(ns("doc_selector"), label = "Documents", 
                choices = "",
                selected = ""),
    
    htmlOutput(ns("focal_text")),

    textOutput(ns("captured_range"))
  ),
  column(width = 2,
        
         tags$b("Codes"),
         br(),
        uiOutput(ns("code_list"))
         
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

      updateSelectInput(session = session, "doc_selector", choices = c("", doc_choices()))

      # code_df <- list_db_codes(project$project_db, project$active_project)
      # 
      # updateSelectInput(session = session,
      #                   "code_id",
      #                   choices = stats::setNames(code_df$code_id, code_df$code_name)
      #                   )
      # 
      # 
      # code_list$code_id <- code_df$code_id
      # code_list$code_name <- code_df$code_name
      
        }
    })
            
  
    

# Displayed text ----------------------------------------------------------

    
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
    
# Coding tools ------------------------------------------------------------
    #TODO this observer should be auto-generated for each code
    
    observeEvent(input[["add_code"]], {


      
      # browser()
      # golem::invoke_js("highlight", list("yellow"))
      
      req(input$tag_position)
      
      startOff <- as.numeric(unlist(strsplit(input$tag_position, "-")))[1]+1
      endOff <- as.numeric(unlist(strsplit(input$tag_position, "-")))[2]
      
      if (endOff - startOff > 1 & endOff > startOff) {
      
      write_segment_db(project$active_project,
                       project$project_db,
                       doc_id = input$doc_selector,
                       code_id = input$code_id,
                       startOff, 
                       endOff)
      
      display_text <- load_doc_to_display(project$active_project, 
                                          project$project_db, 
                                          input$tag_position)
      text(display_text)
      }
    })
  

#  Mouse coding tools -----------------------------------------------------


    
    shinyjs::onevent("click",
            "focal_text",
            
            
          print(load_segment_codes_db(project$active_project, 
                                      project$project_db,
                                      input$doc_selector))
    
    )
    
    

# List out available codes ------------------------------------------------


    
    output$code_list <- renderUI({
    
      if (isTruthy(project$active_project)) {
        
#browser()
        code_df <- list_db_codes(project$project_db, project$active_project)

        purrr::map2(
          
        .x = code_df$code_id,
         .y = code_df$code_name, 
        ~generate_coding_tools("code_id", .x, .y)
              )
        
        
      } else {
        
        ""
        
      }
      
      # 
      # updateSelectInput(session = session,
      #                   "code_id",
      #                   choices = stats::setNames(code_df$code_id, code_df$code_name)
      #                   )
      # 
      # 
      # code_list$code_id <- code_df$code_id
      # code_list$code_name <- code_df$code_name
      # purrr::map(
      #         citations,
      #         ~stringr::str_replace_all(.x, "<.*?>", " ")
      #       )
      #       
      #       tagList(
      #         
      #         checkboxGroupInput(ns("publist"), 
      #                            label ="Most recent publications in ASEP.", 
      #                            width = "100%",
      #                            choiceNames = displayed_citations,
      #                            choiceValues = citations),
      #         
      #         actionButton(ns("add"),
      #                      label = "Add to report",                  icon = icon("check"),                  class = "btn-success"
      #         )
      #       )
      #     } else {
      #       
      #       paste0("No ASEP records found for author ", 
      # 
      # 
      # } else {""}
      
    })

    

#  # Helper (to be commented out in prod): position counter ---------------

  
    
    output$captured_range <- renderText({input$tag_position})
 
  })
}
