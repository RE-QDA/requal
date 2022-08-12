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
mod_codebook_server <- function(id, pool, project, user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # codebook observer ----
    codebook <- reactiveVal()

    observeEvent(project(), {
      # update codebook return values

      codebook(
        list_db_codes(
          pool,
          project()
        )
      )
    })

    # List existing codes in codes boxes ----
    output$codes_ui <- renderUI({
        render_codes(
            pool = pool,
            active_project = project()
        )
    })

    # #---Generate codes UI (if there is an active project)--------------
    output$codes_manager <- renderUI({
      if (isTruthy(project())) {
        codebook_manager_UI(
            id,
            pool,
            project_id = project()
        )
      }
    })

    #---Create code UI --------------
    output$code_create_ui <- renderUI({
      req(project())
      create_code_UI(id)
    })
    outputOptions(output, "code_create_ui", suspendWhenHidden = FALSE)

    #---Create new code------------------------------------------------------

    observeEvent(input$code_add, {

      # check if code name is unique
      code_names <- list_db_codes(
        pool,
        project_id = project()
      )$code_name

      if (!input$code_name %in% code_names & input$code_name != "") {

        codes_input_df <- data.frame(
          project_id = project(),
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
          pool,
          project_id = project(),
          codes_df = codes_input_df,
          user_id = user()$user_id
        )

        output$codes_ui <- renderUI({
          render_codes(
              pool = pool,
              active_project = project()
          )
        })

        # re-render manager UI
        output$code_create_ui <- renderUI({
          create_code_UI(id)
        })
        output$code_merge_ui <- renderUI({
          merge_code_UI(id, pool, project)
        })
        output$code_delete_ui <- renderUI({
          delete_code_UI(id, pool, project)
        })
      } else {
        showModal(modalDialog(
          title = "Warning",
          "Code names must be unique and non-empty."
        ))
      }


      # update codebook return value
      codebook(
        list_db_codes(
          pool,
          project()
        )
      )
    })

    #---Delete code UI --------------

    output$code_delete_ui <- renderUI({
      req(project())
      delete_code_UI(id, pool, project)
    })
    outputOptions(output, "code_delete_ui", suspendWhenHidden = FALSE)

    #---Delete existing code-------------------------------------
    observeEvent(input$code_del_btn, {
      req(input$code_to_del)

      # delete code
      delete_db_codes(
        pool,
        active_project = project(),
        delete_code_id = input$code_to_del,
        user_id = user()$user_id
      )

      # delete edges
      edge <- list()
      edge$code_id <- input$code_to_del
      delete_category_code_record(
        pool,
        active_project = project(),
        user_id = user()$user_id,
        edge = edge
      )

      # re-render UI

      output$code_merge_ui <- renderUI({
        merge_code_UI(id, pool, project)
      })
      output$code_delete_ui <- renderUI({
        delete_code_UI(id, pool, project)
      })

      # relist remaining codes
      output$codes_ui <- renderUI({
        render_codes(
          pool = pool,
          active_project = project()
        )
      })


      # update codebook return value
      codebook(
        list_db_codes(
            pool,
            project()
        )
      )
    })

    # #---Merge code UI --------------
    output$code_merge_ui <- renderUI({
      req(project())
      merge_code_UI(id, pool, project)
    })
    outputOptions(output, "code_merge_ui", suspendWhenHidden = FALSE)

    #---Merge codes-----------------------------------------------------

    observeEvent(input$code_merge, {
      if (isTruthy(input$merge_from) & isTruthy(input$merge_to)) {
        if (input$merge_from == input$merge_to) {
          warn_user("Cannot merge a code into itself.")
        } else {
          merge_codes(
            pool,
            project(),
            input$merge_from,
            input$merge_to,
            user_id = user()$user_id
          )

          # update codebook return value
          codebook(
            list_db_codes(
              pool,
              project()
            )
          )

          # relist remaining codes
          output$codes_ui <- renderUI({
            render_codes(
              pool = pool,
              active_project = project()
            )
          })

          # re-render UI

          output$code_merge_ui <- renderUI({
            merge_code_UI(id, pool, project)
          })
          output$code_delete_ui <- renderUI({
            delete_code_UI(id, pool, project)
          })
        }
      } else {
        NULL
      }
    })

    # return active codebook details ----

    return(reactive(codebook()))


    # end of server module function
  })
}
