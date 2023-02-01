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
        fluidRow(
          column(
            width = 10,
            br(),
            uiOutput(
              ns("codes_ui")
            ) %>% tagAppendAttributes(class = "scrollable90")
          ),

          # menu
          menu_column(
            width = 2,
            menu_btn(
              uiOutput(ns("code_create_ui")),
              label = "Create code",
              icon = "plus"
            ),
            menu_btn(
              uiOutput(ns("code_merge_ui")),
              label =  "Merge codes",
              icon = "compress"
            ),
            menu_btn(
              uiOutput(ns("code_delete_ui")),
              label =  "Delete code",
              icon = "minus"
            )
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
mod_codebook_server <- function(id, glob) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

   # initialize codebook upon load
    observeEvent(glob$active_project, {
        glob$codebook <- list_db_codes(
            glob$pool,
            glob$active_project
        )
    })

    # List existing codes in codes boxes ----
    output$codes_ui <- renderUI({
      render_codes(
        active_project = glob$active_project,
        pool = glob$pool
      )
    })
    
    #---Generate codes UI (if there is an active project)--------------
    output$codes_manager <- renderUI({
      if (isTruthy(glob$active_project)) {
        codebook_manager_UI(id,
          pool = glob$pool,
          project_id = glob$active_project
        )
      }
    })

    #---Create code UI --------------
    output$code_create_ui <- renderUI({
      req(glob$active_project)
      create_code_UI(id)
    })
    outputOptions(output, "code_create_ui", suspendWhenHidden = FALSE)

    #---Create new code------------------------------------------------------
    observeEvent(input$code_add, {

      # check if code name is unique
      code_names <- list_db_codes(
        pool = glob$pool,
        project_id = glob$active_project
      )$code_name

      if (!input$code_name %in% code_names & input$code_name != "") {
        
        codes_input_df <- data.frame(
          project_id = glob$active_project,
          code_name = input$code_name,
          code_description = input$code_desc,
          code_color = paste0(
            "rgb(",
            paste(
              as.vector(
                grDevices::col2rgb(
                  input$color_pick
                )
              ),
              collapse = ", "
            ),
            ")"
          )
        )

        add_codes_record(
          pool = glob$pool,
          project_id = glob$active_project,
          codes_df = codes_input_df, 
          user_id = glob$user$user_id
        )

        output$codes_ui <- renderUI({
          render_codes(
            active_project = glob$active_project,
            pool = glob$pool
          )
        })

        # re-render manager UI
        output$code_create_ui <- renderUI({
          create_code_UI(id)
        })
        output$code_merge_ui <- renderUI({
          merge_code_UI(id, glob$pool, glob$active_project)
        })
        output$code_delete_ui <- renderUI({
          delete_code_UI(id, glob$pool, glob$active_project)
        })
      } else {
        showModal(modalDialog(
          title = "Warning",
          "Code names must be unique and non-empty."
        ))
      }

      # update codebook return value
      glob$codebook <- list_db_codes(
          glob$pool,
          glob$active_project
        )
    })

    #---Delete code UI --------------
    output$code_delete_ui <- renderUI({
      req(glob$active_project)
      delete_code_UI(id, glob$pool, glob$active_project)
    })
    outputOptions(output, "code_delete_ui", suspendWhenHidden = FALSE)

    #---Delete existing code-------------------------------------


    observeEvent(input$code_del_btn, {
      req(input$code_to_del)

      # delete edges - must precede deleting of codes
      edge <- list()
      edge$code_id <- input$code_to_del
      delete_category_code_record(
        pool = glob$pool,
        active_project = glob$active_project,
        user_id = glob$user$user_id,
        edge = edge
      )

      delete_codes_segment_db(
        pool = glob$pool,
        active_project = glob$active_project,
        user_id = glob$user$user_id,
        code_id = input$code_to_del
        )

      # delete code
      delete_db_codes(
        pool = glob$pool,
        active_project = glob$active_project,
        delete_code_id = input$code_to_del, 
        user_id = glob$user$user_id
      )

      # re-render UI

      output$code_merge_ui <- renderUI({
        merge_code_UI(id, glob$pool, glob$active_project)
      })
      output$code_delete_ui <- renderUI({
        delete_code_UI(id, glob$pool, glob$active_project)
      })

      # relist remaining codes
      output$codes_ui <- renderUI({
        render_codes(
          active_project = glob$active_project,
          pool = glob$pool
        )
      })

      # update codebook return value
      glob$codebook <- list_db_codes(
          glob$pool,
          glob$active_project
        )
    })

    #---Merge code UI --------------
    output$code_merge_ui <- renderUI({
      req(glob$active_project)
      merge_code_UI(id, glob$pool, glob$active_project)
    })
    outputOptions(output, "code_merge_ui", suspendWhenHidden = FALSE)

    #---Merge codes-----------------------------------------------------
    observeEvent(input$code_merge, {
      if (isTruthy(input$merge_from) & isTruthy(input$merge_to)) {
        if (input$merge_from == input$merge_to) {
          warn_user("Cannot merge a code into itself.")
        } else {
          merge_codes(
            glob$pool,
            glob$active_project,
            input$merge_from,
            input$merge_to, 
            user_id = glob$user$user_id
          )

          # update codebook return value
          glob$codebook <- list_db_codes(
              glob$pool,
              glob$active_project
            )

          # relist remaining codes
          output$codes_ui <- renderUI({
            render_codes(
              active_project = glob$active_project,
              pool = glob$pool
            )
          })

          # re-render UI
          output$code_merge_ui <- renderUI({
            req(glob$active_project)
            merge_code_UI(id, glob$pool, glob$active_project)
          })
          output$code_delete_ui <- renderUI({
            req(glob$active_project)
            delete_code_UI(id, glob$pool, glob$active_project)
          })
        }
      } else {
        NULL
      }
    })

    # returns glob$codebook ----

    # end of server module function
  })
}
