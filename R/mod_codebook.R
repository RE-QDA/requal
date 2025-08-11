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
      tabPanel(
        "Codes",
        id = ns("codebook_tabset"),
        value = "codebook_tabset",
        fluidRow(
          class = "module_tools",
          mod_rql_button_ui(
            ns("code_create_ui"),
            label = "Create code",
            icon = "plus"
          ),
          mod_rql_button_ui(
            ns("code_edit_ui"),
            label = "Edit codes",
            icon = "edit"
          ),
          mod_rql_button_ui(
            ns("code_merge_ui"),
            label = "Merge codes",
            icon = "compress"
          ),
          mod_rql_button_ui(
            ns("code_delete_ui"),
            label = "Delete code",
            icon = "minus"
          ),
          mod_rql_button_ui(
            ns("code_export_ui"),
            label = "Export codebook",
            icon = "download"
          ),
          mod_rql_button_ui(
            ns("code_import_ui"),
            label = "Import codebook",
            icon = "upload"
          )
        ),
        fluidRow(
          class = "module_content",
          column(
            width = 10,
            br(),
            uiOutput(
              ns("codes_ui")
            ) %>%
              tagAppendAttributes(class = "scrollable90")
          )
        )
      ),
      tabPanel(
        "Categories",
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
    loc <- reactiveValues()

    # initialize codebook upon load
    observeEvent(
      c(
        glob$active_project,
        input$code_add,
        input$code_edit_btn,
        input$code_merge,
        input$code_del_btn,
        glob$codebook_observer
      ),
      {
        #---Create code UI --------------
        mod_rql_button_server(
          id = "code_create_ui",
          custom_title = "Create code",
          custom_tagList = create_code_UI(ns),
          glob,
          permission = "codebook_modify"
        )
        #---Merge code UI --------------
        mod_rql_button_server(
          id = "code_merge_ui",
          custom_title = "Merge codes",
          custom_tagList = merge_code_UI(
            ns,
            glob$pool,
            glob$active_project,
            glob$user
          ),
          glob,
          permission = "codebook_modify"
        )
        #---Edit code UI --------------
        mod_rql_button_server(
          id = "code_edit_ui",
          custom_title = "Edit code",
          custom_tagList = edit_code_UI(
            ns,
            glob$pool,
            glob$active_project,
            glob$user
          ),
          glob,
          permission = "codebook_modify"
        )
        #---Delete code UI --------------
        mod_rql_button_server(
          id = "code_delete_ui",
          custom_title = "Delete code",
          custom_tagList = delete_code_UI(
            ns,
            glob$pool,
            glob$active_project,
            glob$user
          ),
          glob,
          permission = "codebook_modify"
        )

        #---Import codebook UI --------------
        mod_rql_button_server(
          id = "code_import_ui",
          custom_title = with_help(
            "Import codebook",
            help_item = "codebook_csv_import",
            visible = TRUE
          ),
          custom_tagList = mod_codebook_import_ui(ns("codebook_import_1")),
          glob,
          permission = "codebook_modify"
        )

        #---Export codebook UI --------------
        mod_rql_button_server(
          id = "code_export_ui",
          custom_title = "Export codebook",
          custom_tagList = downloadButton(ns("export_codebook"), label = "CSV"),
          glob,
          permission = TRUE
        )

        #---Codebook display --------------
        glob$codebook <- list_db_codes(
          glob$pool,
          glob$active_project,
          glob$user
        )
        output$codes_ui <- renderUI({
          render_codes(
            active_project = glob$active_project,
            pool = glob$pool,
            user = glob$user
          )
        })
      }
    )

    #---Create new code------------------------------------------------------
    observeEvent(input$code_add, {
      # check if code name is unique
      code_names <- list_db_codes(
        pool = glob$pool,
        project_id = glob$active_project,
        user = glob$user
      )$code_name

      if (!req(input$code_name) %in% code_names) {
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
          ),
          user_id = glob$user$user_id
        )

        add_codes_record(
          pool = glob$pool,
          project_id = glob$active_project,
          codes_df = codes_input_df,
          user_id = glob$user$user_id
        )
      } else {
        warn_user("Code names must be unique and non-empty.")
      }
    })

    #---Edit existing code-------------------------------------
    # To edit a code, first observe which code is being edited
    observeEvent(input$code_to_edit, {
      req(input$code_to_edit)
      updateTextInput(
        session = session,
        "edit_code_name",
        value = glob$codebook %>%
          dplyr::filter(code_id == input$code_to_edit) %>%
          dplyr::pull(code_name)
      )
      updateTextAreaInput(
        session = session,
        "edit_code_desc",
        value = glob$codebook %>%
          dplyr::filter(code_id == input$code_to_edit) %>%
          dplyr::pull(code_description)
      )
      colourpicker::updateColourInput(
        session = session,
        "edit_color_pick",
        value = glob$codebook %>%
          dplyr::filter(code_id == input$code_to_edit) %>%
          dplyr::pull(code_color)
      )
    })
    # Execute code edit
    observeEvent(input$code_edit_btn, {
      req(input$code_to_edit)

      # check if code name is unique
      code_names <- list_db_codes(
        pool = glob$pool,
        project_id = glob$active_project,
        user = glob$user
      ) %>%
        dplyr::filter(code_id != input$code_to_edit) %>% # exclude original name from comparison
        dplyr::pull(code_name)

      # code must have a name that does not exist yet (unless it stays the same as original)
      if (
        isTruthy(input$edit_code_name) && !input$edit_code_name %in% code_names
      ) {
        # edit code
        edit_db_codes(
          pool = glob$pool,
          active_project = glob$active_project,
          user_id = glob$user$user_id,
          edit_code_id = input$code_to_edit,
          edit_code_name = input$edit_code_name,
          edit_code_description = input$edit_code_desc,
          edit_code_color = paste0(
            "rgb(",
            paste(
              as.vector(
                grDevices::col2rgb(
                  input$edit_color_pick
                )
              ),
              collapse = ", "
            ),
            ")"
          )
        )
      } else {
        warn_user("Code names must be unique and non-empty.")
      }
    })

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

      # if a code gets deleted, the corresponding segments should be deleted too
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
    })

    #---Merge codes-----------------------------------------------------
    observeEvent(input$code_merge, {
      if (req(input$merge_from) == req(input$merge_to)) {
        warn_user("Cannot merge a code into itself.")
      } else {
        merge_codes(
          glob$pool,
          glob$active_project,
          req(input$merge_from),
          req(input$merge_to),
          user_id = glob$user$user_id
        )
      }
    })

    #---Import codebook-----------------------------------------------------
    mod_codebook_import_server("codebook_import_1", glob = glob)

    output$export_codebook <- downloadHandler(
      filename = function() {
        "requal_codebook.csv"
      },
      content = function(file) {
        codebook <- get_codebook_export_table(glob)
        utils::write.csv(codebook, file)
      }
    )

    # end of server module function
  })
}
