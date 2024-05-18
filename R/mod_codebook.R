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
      tabPanel("Codes",
        id = ns("codebook_tabset"),
        value = "codebook_tabset",
    fluidRow(
      class = "module_tools",
      mod_rql_button_ui(ns("code_create_ui"),
        label = "Create code",
        icon = "plus"
      ),
      mod_rql_button_ui(ns("code_merge_ui"),
        label = "Merge codes",
        icon = "compress"
      ),
      mod_rql_button_ui(ns("code_delete_ui"),
        label = "Delete code",
        icon = "minus"
      ),
    ),
        fluidRow(
          class = "module_content",
          column(
            width = 10,
            br(),
            uiOutput(
              ns("codes_ui")
            ) %>% tagAppendAttributes(class = "scrollable90")
          )
        )
      ),
      tabPanel("Categories",
        id = ns("categories"),
        value = "categories",
        mod_categories_ui("categories_ui_1") 
      )
    ), 
    downloadButton(ns("export_codebook"), label = "CSV")
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
   observeEvent(c(
      glob$active_project,
      input$code_add,
      input$code_merge,
      input$code_del_btn
    ), {
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
      custom_tagList = merge_code_UI(ns, glob$pool, glob$active_project, glob$user),
      glob,
      permission = "codebook_modify"
    )
    #---Delete code UI --------------
    mod_rql_button_server(
      id = "code_delete_ui",
      custom_title = "Delete code",
      custom_tagList = delete_code_UI(ns, glob$pool, glob$active_project, glob$user),
      glob,
      permission = "codebook_modify"
    )
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
})
  
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
