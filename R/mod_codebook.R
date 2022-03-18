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
 
  tabsetPanel(
    type = "tabs", 
    id = ns("codebook_tabset"),
    
   
    tabPanel("Codebook",
      id = ns("codebook_tabset"),
      value = "codebook_tabset",
      tags$br(), 
      
      column(
        width = 6,
        uiOutput(
          ns("codes_ui")
          )
      ) %>% tagAppendAttributes(class = "scrollable90"),
      
      column(
        width = 6,
        uiOutput(
          ns("codes_manager")
        )
      )
    ),
    
    tabPanel("Categories",
      id = ns("categories"),
      value = "categories",
      
      mod_categories_ui("categories_ui_1")
      
    )
  )
)
  
}

#' codebook Server Functions
#'
#' @noRd
mod_codebook_server <- function(id, project, user) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns

    # codebook observer ----

    codebook <- reactiveVal()
    
    observeEvent(project()$active_project, {
      # update codebook return values
      
      codebook(
        list_db_codes(project()$project_db,
                      project()$active_project)
      )
      
    })

# List existing codes in codes boxes ----
    output$codes_ui <- renderUI({

      render_codes(active_project = project()$active_project,
                   project_db = project()$project_db)


    })
#---Generate codes UI (if there is an active project)--------------
    output$codes_manager <- renderUI({
      if (isTruthy(project()$active_project)) {

        codebook_manager_UI(id,
                            project_db = project()$project_db,
                            project_id = project()$active_project)
      }
    })

#---Create new code------------------------------------------------------



    observeEvent(input$code_add, {


      # check if code name is unique
      code_names <- list_db_codes(project_db = project()$project_db,
                                     project_id = project()$active_project)$code_name

      if (!input$code_name %in% code_names & input$code_name != "") {
      con <- DBI::dbConnect(RSQLite::SQLite(), project()$project_db)
      on.exit(DBI::dbDisconnect(con))

      codes_input_df <- data.frame(project_id = project()$active_project,
                                   code_name = input$code_name,
                                   code_description = input$code_desc,
                                   code_color = paste0("rgb(",
                                                       paste(
                                                         as.vector(
                                                           grDevices::col2rgb(
                                                             input$color_pick
                                                             )
                                                           ), collapse = ", "),
                                                       ")"
                                                       )
                                   )

      add_codes_record(con = con,
                       project_id = project()$active_project,
                       codes_df = codes_input_df)

      output$codes_ui <- renderUI({

        render_codes(active_project = project()$active_project,
                     project_db = project()$project_db)


      })

      # re-render manager UI

      output$codes_manager <- renderUI({
        if (isTruthy(project()$active_project)) {
          codebook_manager_UI(id,
                              project_db = project()$project_db,
                              project_id = project()$active_project)
        }
      })


      } else { showModal(modalDialog(title = "Warning",
                                     "Code names must be unique and non-empty."))
      }


      # update codebook return value
      codebook(
        list_db_codes(project()$project_db,
                      project()$active_project)
      )

    })

#---Delete existing code-----------------------------------------------------


    observeEvent(input$code_del_btn, {

      req(input$code_to_del)


      # delete code
      delete_db_codes(project_db = project()$project_db,
                      active_project = project()$active_project,
                      delete_code_id = input$code_to_del)
      
      # delete edges
      edge <- list()
      edge$code_id <- input$code_to_del
      delete_category_code_record(project_db = project()$project_db,
                                  active_project = project()$active_project,
                                  user = user,
                                  edge = edge)

      # re-render manager UI

      output$codes_manager <- renderUI({
        if (isTruthy(project()$active_project)) {
          codebook_manager_UI(id,
                              project_db = project()$project_db,
                              project_id = project()$active_project)
        }
      })

      # relist remaining codes
      output$codes_ui <- renderUI({

        render_codes(active_project = project()$active_project,
                     project_db = project()$project_db)
      })


      # update codebook return value
      codebook(
        list_db_codes(project()$project_db,
                      project()$active_project)
      )

    })

#---Merge codes-----------------------------------------------------
    
    observeEvent(input$code_merge, {
      
      if (isTruthy(input$merge_from) & isTruthy(input$merge_to)) {
        
      if (input$merge_from == input$merge_to) {
        warn_user("Cannot merge a code into itself.")
      } else {
        
        
        
        merge_codes(project()$project_db,
                    project()$active_project,
                    input$merge_from,
                    input$merge_to)
        
        # update codebook return value
        codebook(
          list_db_codes(project()$project_db,
                        project()$active_project)
        )
        
        # relist remaining codes
        output$codes_ui <- renderUI({
          
          render_codes(active_project = project()$active_project,
                       project_db = project()$project_db)
        })
        
        # re-render manager UI
        
        output$codes_manager <- renderUI({
          if (isTruthy(project()$active_project)) {
            codebook_manager_UI(id,
                                project_db = project()$project_db,
                                project_id = project()$active_project)
          }
        })
        
        
        
       } } else {NULL}
      
    })
    
# return active codebook details ----

       return(reactive(codebook()))
    
    
 # end of server module function   
     })
  }

