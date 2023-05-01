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
  tagList(
    fluidRow(
      class = "module_tools",
      mod_rql_button_ui(ns("doc_create_ui"),
        label = "Create document",
        icon = "plus"
      ),
      mod_rql_button_ui(ns("doc_upload_ui"),
        label = "Upload file",
        icon = "upload"
      ),
      mod_rql_button_ui(ns("doc_delete_ui"),
        label = "Delete document",
        icon = "minus"
      ),
    ),
    fluidRow(
      class = "module_content",
      tags$style(".shiny-file-input-progress {display: none}"),
      tags$h2("List of project documents"),
      tags$div(
        tableOutput(ns("doc_list_table"))
      ) %>%
        tagAppendAttributes(class = "scrollable90")
    )
  )
}

#' doc_manager Server Functions
#'
#' @noRd
mod_doc_manager_server <- function(id, glob) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #---List active project  --------------
    output$no_active_project <- renderUI({
      if (is.null(glob$active_project)) {
        "No active project."
      }
    })


    #  # handle permissions
    #     if(glob$user$data$data_modify == 1){

    #     }


    #---Create doc UI --------------
    mod_rql_button_server(
      id = "doc_create_ui",
      custom_title = "Create document",
      custom_tagList = create_doc_UI(ns = ns),
      glob,
      permission = "data_modify"
    )

    #---Upload doc UI --------------
    mod_rql_button_server(
      id = "doc_upload_ui",
      custom_title = "Upload file",
      custom_tagList = upload_doc_UI(ns = ns),
      glob,
      permission = "data_modify"
    )

    #---Delete doc UI --------------
    mod_rql_button_server(
      id = "doc_delete_ui",
      custom_title = "Delete document",
      custom_tagList = delete_doc_UI(ns = ns, glob),
      glob,
      permission = "data_modify"
    )


    # list documents initially ----
    observeEvent(glob$active_project, {
      output$project_name <- renderText({
        paste(tags$b("Active project:"), names(glob$active_project))
      })

      glob$documents <- list_db_documents(
        pool = glob$pool,
        active_project = glob$active_project,
        user = glob$user
      )

      output$doc_list_table <- make_doc_table(
        glob,
        glob$documents
      )

      shinyWidgets::updatePickerInput(
        session = session,
        "doc_delete_list",
        choices = glob$documents
      )
    })

    # observe documents actions ----
    # document input ----
    observeEvent(input$doc_add, {
      if ((!req(input$doc_name) %in% c("", names(glob$documents))) &
        isTruthy(input$doc_text)) {
        add_input_document(
          pool = glob$pool,
          project = glob$active_project,
          doc_name = input$doc_name,
          doc_text = input$doc_text,
          doc_description = input$doc_description,
          user_id = glob$user$user_id
        )

        output$doc_list_table <- make_doc_table(
          glob,
          glob$documents
        )

        glob$documents <- list_db_documents(
          pool = glob$pool,
          active_project = glob$active_project,
          user = glob$user
        )

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
        shinyWidgets::updatePickerInput(
          session = session,
          "doc_delete_list",
          choices = glob$documents
        )
      } else {
        warn_user("Documents need to have a content and their names must be unique.")
      }
    })

    # document removal ----
    observeEvent(input$doc_remove, {
      req(input$doc_delete_list)

      delete_db_documents(
        glob$pool,
        glob$active_project,
        input$doc_delete_list,
        glob$user$user_id
      )

      # update reactive value containing project documents
      glob$documents <- list_db_documents(
        pool = glob$pool,
        active_project = glob$active_project,
        user = glob$user
      )

      output$doc_list_table <- make_doc_table(
        glob,
        glob$documents
      )

      updateSelectInput(
        session = session,
        "doc_delete_list",
        choices = glob$documents
      )
    })

    # document upload ----

    observeEvent(input$doc_upload_add, {
      if (

        ((!input$doc_upload_name %in% c("", names(glob$documents))) & !is.null(input$doc_path)) |
          (!is.null(input$doc_path) && (!input$doc_path[["name"]] %in% names(glob$documents)))) {
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
            pool = glob$pool,
            project = glob$active_project,
            doc_name = doc_name_parsed,
            doc_text = doc_upload_text,
            doc_description = input$doc_upload_description,
            user_id = glob$user$user_id
          )

          output$doc_list_table <- make_doc_table(
            glob,
            glob$documents
          )

          glob$documents <- list_db_documents(
            pool = glob$pool,
            active_project = glob$active_project,
            user = glob$user
          )

          updateSelectInput(
            session = session,
            "doc_delete_list",
            choices = glob$documents
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

    # returns glob$documents
  })
}
