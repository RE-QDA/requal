#' doc_manager UI Function
#'
#' @description This module accept document inputs and writes them to ReQual database.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_doc_manager_ui <- function(id) {
  ns <- NS(id)

  fluidRow(
    tags$style(".shiny-file-input-progress {display: none}"),
    column(
      width = 10,
      br(),
      htmlOutput(ns("project_name")),
      uiOutput(ns("project_active")),
      tags$div(
        tableOutput(ns("doc_list_table"))
      ) %>%
        tagAppendAttributes(class = "scrollable90")
    ),
    # menu
    
    menu_column(
      width = 2,
      menu_btn(
        uiOutput(ns("doc_create_ui")),
        label = "Create document",
        icon = "plus"
      ),
      menu_btn(
        uiOutput(ns("doc_upload_ui")),
        label =  "Upload file",
        icon = "upload"
      ),
      menu_btn(
        uiOutput(ns("doc_delete_ui")),
        label =  "Delete document",
        icon = "minus"
      )
    )
  )
}

#' doc_manager Server Functions
#'
#' @noRd
mod_doc_manager_server <- function(id, project, user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    doc_list <- reactiveVal()
    
    
    #---Create doc UI --------------
    
    output$doc_create_ui <- renderUI({
      create_doc_UI(id)
    })
    outputOptions(output, "doc_create_ui", suspendWhenHidden = FALSE)
    #---Upload doc UI --------------
    
    output$doc_upload_ui <- renderUI({
      upload_doc_UI(id)
    })
    outputOptions(output, "doc_upload_ui", suspendWhenHidden = FALSE)
    #---Delete doc UI --------------
    
    output$doc_delete_ui <- renderUI({
      req(project()$active_project)
      delete_doc_UI(id, project)
    })
    outputOptions(output, "doc_delete_ui", suspendWhenHidden = FALSE)
    
# list documents initially ----
    observeEvent(project()$active_project, {
      output$project_name <- renderText({
        paste(tags$b("Active project:"), names(project()$active_project))
      })

      doc_startup <- list_db_documents(
        project_db = project()$project_db,
        active_project = project()$active_project
      )
      doc_list(doc_startup)

      output$doc_list_table <- make_doc_table(
        project()$project_db,
        project()$active_project,
        doc_list()
      )

      updateSelectInput(
        session = session,
        "doc_delete_list",
        choices = doc_list()
      )
    })



    # observe documents actions ----

    # document input ----


    observeEvent(input$doc_add, {
      
      if ((!req(input$doc_name) %in% c("", names(doc_list()))) &
        isTruthy(input$doc_text)) {
        add_input_document(
          connection = project()$project_db,
          project = project()$active_project,
          doc_name = input$doc_name,
          doc_text = input$doc_text,
          doc_description = input$doc_description
        )

        output$doc_list_table <- make_doc_table(
          project()$project_db,
          project()$active_project,
          doc_list()
        )

        doc_list(list_db_documents(
          project_db = project()$project_db,
          active_project = project()$active_project
        ))

        updateTextInput(
          session = session,
          "doc_name", value = ""
        )
        updateTextAreaInput(
          session = session,
          "doc_description", value = ""
        )
        updateTextAreaInput(
          session = session,
          "doc_text", value = ""
        )
        updateSelectInput(
          session = session,
          "doc_delete_list",
          choices = doc_list()
        )
      } else {
        warn_user("Documents need to have a content and their names must be unique.")
      }
    })

    # document removal ----
    observeEvent(input$doc_remove, {
      req(input$doc_delete_list)

      delete_db_documents(
        project()$project_db,
        project()$active_project,
        input$doc_delete_list
      )


      # update reactive value containing project documents
      doc_list(list_db_documents(
        project_db = project()$project_db,
        active_project = project()$active_project
      ))

      output$doc_list_table <- make_doc_table(
        project()$project_db,
        project()$active_project,
        doc_list()
      )

      updateSelectInput(
        session = session,
        "doc_delete_list",
        choices = doc_list()
      )
    })

    # document upload ----

    observeEvent(input$doc_upload_add, {
      if (

        ((!input$doc_upload_name %in% c("", names(doc_list()))) & !is.null(input$doc_path)) |
          (!is.null(input$doc_path) && (!input$doc_path[["name"]] %in% names(doc_list())))) {
        if (isTruthy(input$doc_path[["datapath"]])) {
          doc_upload_text <- paste0(readLines(input$doc_path[["datapath"]]), collapse = "\n")

          if (input$encoding != "UTF-8") {
            doc_upload_text <- iconv(doc_upload_text,
              from = input$encoding,
              to = "UTF-8", sub = ""
            )
          }

          if (!isTruthy(input$doc_upload_name)) {
            doc_name_parsed <- input$doc_path[["name"]]
          } else {
            doc_name_parsed <- input$doc_upload_name
          }


          add_input_document(
            connection = project()$project_db,
            project = project()$active_project,
            doc_name = doc_name_parsed,
            doc_text = doc_upload_text,
            doc_description = input$doc_upload_description
          )

          output$doc_list_table <- make_doc_table(
            project()$project_db,
            project()$active_project,
            doc_list()
          )

          doc_list(list_db_documents(
            project_db = project()$project_db,
            active_project = project()$active_project
          ))

          updateSelectInput(
            session = session,
            "doc_delete_list",
            choices = doc_list()
          )
          updateSelectInput(
            session = session,
            "encoding",
            selected = "UTF-8"
          )
          updateTextInput(
            session = session,
            "doc_upload_name",
            value = ""
          )
          updateTextAreaInput(
            session = session,
            "doc_upload_description", value = ""
          )
          shinyjs::reset("doc_path")
        }
      } else {
        warn_user("Documents need to have a content and their names must be unique.")
        shinyjs::reset("doc_path")
      }
    })


    return(reactive(doc_list()))
  })
}
