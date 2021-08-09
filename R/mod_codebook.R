#' codebook UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_codebook_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    "Management of codes. Including creation, deletion, merges.",
    fluidRow(column(width = 6,
                    
                  uiOutput(ns("codes_ui"))
                  
                  ),
             
             column(width = 6,
                    
                    textAreaInput(ns("doc_text"), label = NULL, placeholder = "Paste a new document content here"),
                    
                    actionButton(ns("code_add"), label = "Add document")
                    
                    
                    )
             )
    
  )
}

#' codebook Server Functions
#'
#' @noRd
mod_codebook_server <- function(id, project) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
   
    output$codes_ui <- renderUI({

      if (!is.null(project$active_project)) {

        project_codes <- list_db_codes(project_db =project$project_db,
                                                project_id = project$active_project)
        

        if (nrow(project_codes) == 0) {
          
          "No codes have been created."
          
        } else {

        purrr::pmap(project_codes, gen_codes_ui)
          
        }

      } else {"No active project."}
      
      })
    
  })
}

