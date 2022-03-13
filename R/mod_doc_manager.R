#' doc_manager UI Function
#'
#' @description This module accept document inputs and writes them to ReQual database.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_doc_manager_ui <- function(id){
  ns <- NS(id)

    fluidRow(
      tags$style(".shiny-file-input-progress {display: none}"),

      column(width = 6,
             br(),
             htmlOutput(ns("project_name")),

             uiOutput(ns("project_active")),

tags$div(      tableOutput(ns("doc_list_table")) ) %>%
  tagAppendAttributes(class = "scrollable90")


    ),

      column(width = 6,

            box(title = "Document input",
                collapsible = TRUE,
                width = NULL,


                textInput(ns("doc_name"), label = "Document name", placeholder = "Short name") %>%
                  tagAppendAttributes(class = "required"),

               textAreaInput(ns("doc_description"), label = "Document description", placeholder = "Description"),

                textAreaInput(ns("doc_text"), label = "Document content", placeholder = "Paste the new document content here") %>%
                 tagAppendAttributes(class = "required"),

               actionButton(ns("doc_add"), label = "Add document")

            ),

            box(title = "File upload",
                collapsible = TRUE,
                collapsed = TRUE,
                width = NULL,

                textInput(ns("doc_upload_name"), label = "Document name", placeholder = "Optional filename replacement"),
                textAreaInput(ns("doc_upload_description"), label = "Document description", placeholder = "Description"),

      # upload UI ----
      fileInput(ns("doc_path"), NULL,
                multiple = FALSE,
                buttonLabel = "Select file",
                accept = c("text/plain", ".txt")),
      selectInput(ns("encoding"),
                  "File encoding",
                  choices = support_encodings(),
                  selected = "UTF-8",
                  width = "40%"
      ),
      actionButton(ns("doc_upload_add"),
                   "Add document")

            ),
            box(title = "Delete documents",
                collapsible = TRUE,
                collapsed = TRUE,
                width = NULL,

        selectInput(ns("doc_delete_list"),
                    label = "Remove selected documents from project",
                    choices = "",
                    multiple = TRUE,
                    selected = NULL),

        actionButton(ns("doc_remove"),
                     "Remove",
                     class = "btn-danger")

            )

    ))


}

#' doc_manager Server Functions
#'
#' @noRd
mod_doc_manager_server <- function(id, project){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    doc_list <- reactiveVal()
    ## list documents initially ----
    observeEvent(project()$active_project, {

      output$project_name <- renderText({
        paste(tags$b("Active project:"), names(project()$active_project))
      })

      doc_startup <- list_db_documents(project_db = project()$project_db,
                                 active_project = project()$active_project)
      doc_list(doc_startup)

      output$doc_list_table <- make_doc_table(
        project()$project_db,
        project()$active_project,
        doc_list()
      )

      updateSelectInput(session = session,
                        "doc_delete_list",
                        choices = doc_list())

    })


    ## render existing documents ----

output$doc_list_table <- make_doc_table(
  project()$project_db,
  project()$active_project,
  doc_list()
)






 # observe documents actions ----

    # document input ----


observeEvent(input$doc_add, {

  if ((!input$doc_name %in% c("", names(doc_list()))) & 
      isTruthy(input$doc_text)) {

  add_input_document(connection = project()$project_db,
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

    doc_list(list_db_documents(project_db = project()$project_db,
                               active_project = project()$active_project))

    updateTextInput(session = session,
                    "doc_name", value = "")
    updateTextAreaInput(session = session,
                    "doc_description", value = "")
    updateTextAreaInput(session = session,
                    "doc_text", value = "")
    updateSelectInput(session = session,
                      "doc_delete_list",
                      choices = doc_list())

  } else { warn_user("Documents need to have a content and their names must be unique.")}

})

    # document removal ----
    observeEvent(input$doc_remove, {

     req(input$doc_delete_list)

        delete_db_documents(project()$project_db,
                            project()$active_project,
                            input$doc_delete_list)


      # update reactive value containing project documents
      doc_list(list_db_documents(project_db = project()$project_db,
                                 active_project = project()$active_project))

      output$doc_list_table <- make_doc_table(
        project()$project_db,
        project()$active_project,
        doc_list()
      )

      updateSelectInput(session = session,
                        "doc_delete_list",
                        choices = doc_list())

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
                                 to = "UTF-8", sub="")

      }

      if (!isTruthy(input$doc_upload_name)) {
        doc_name_parsed <- input$doc_path[["name"]]
      } else {
        doc_name_parsed <- input$doc_upload_name
      }


      add_input_document(connection = project()$project_db,
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

      doc_list(list_db_documents(project_db = project()$project_db,
                                 active_project = project()$active_project))

      updateSelectInput(session = session,
                        "doc_delete_list",
                        choices = doc_list())
      updateSelectInput(session = session,
                        "encoding",
                        selected = "UTF-8")
      updateTextInput(session = session,
                      "doc_upload_name",
                      value = "")
      updateTextAreaInput(session = session,
                           "doc_upload_description", value = "")
      shinyjs::reset("doc_path")


      }

      } else {warn_user("Documents need to have a content and their names must be unique.")
        shinyjs::reset("doc_path")}

    })


    return(reactive(doc_list()))

  })
}
