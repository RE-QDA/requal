#' categories UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_categories_ui <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      width = 5,
      tags$br(),
      uiOutput(ns("uncategorized")) %>% tagAppendAttributes(class = "scrollable90")
    ),
    column(
      width = 5,
      tags$br(),
      uiOutput(ns("categories_ui")) %>% tagAppendAttributes(class = "scrollable90")
    ),

    # menu
    menu_column(
      width = 2,
      menu_btn(
        uiOutput(ns("category_create")),
        label = "Create category",
        icon = "plus"),
      menu_btn(
        uiOutput(ns("category_delete")),
        label =  "Delete category",
        icon = "minus")
      )
  )
}

#' categories Server Functions
#'
#' @noRd
mod_categories_server <- function(id, glob) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # initialize categories upon load
    observeEvent(glob$active_project, {
        glob$category <- read_db_categories(
            pool = glob$pool,
            active_project = glob$active_project
        )
        })


    # List existing codes in code boxes --------
    output$uncategorized <- renderUI({
        render_codes_ui(id, glob$pool, glob$active_project)
    })
    
    # Re-render list of codes on code change
    observeEvent(glob$codebook, {
        render_codes_ui(id, glob$pool, glob$active_project)
    })

    # List existing categories in category boxes ----
    output$categories_ui <- renderUI({
      render_categories(
        id = id,
        active_project = glob$active_project,
        pool = glob$pool
      )
    })

    # Relist categories on codebook changes
    observeEvent(glob$codebook, {

      output$categories_ui <- renderUI({
        render_categories(
          id = id,
          active_project = glob$active_project,
          pool = glob$pool
        )
      })

    })

    #---Generate create categories UI --------------
    output$category_create <- renderUI({
      req(glob$active_project)
      create_new_category_UI(id)
    })
    outputOptions(output, "category_create", suspendWhenHidden = FALSE)

    # Create categories ------
    observeEvent(input$category_add, {
      # check if code name is unique
      category_names <- list_db_categories(
        id = id,
        pool = glob$pool,
        project_id = glob$active_project
      )$category_name

      if (!input$category_name %in% category_names & input$category_name != "") {
        
        categories_input_df <- data.frame(
          project_id = glob$active_project,
          category_name = input$category_name,
          category_description = input$category_desc
        )

        add_category_record(
          pool = glob$pool,
          project_id = glob$active_project,
          user_id = glob$user$user_id,
          categories_df = categories_input_df
        )
        
        # refresh listed categories
        output$categories_ui <- renderUI({
          render_categories(
            id = id,
            pool = glob$pool,
            active_project = glob$active_project
          )
        })

        # refresh create UI
        updateTextInput(
          session = session,
          inputId = "category_name",
          value = ""
        )
        updateTextAreaInput(
          session = session,
          inputId = "category_desc",
          value = ""
        )
        
        # refresh delete UI
        updateSelectInput(
          session = session,
          inputId = "categories_to_del",
          choices = c("", read_db_categories(
            pool = glob$pool,
            active_project = glob$active_project
          ))
        )
        
        # update return value
        glob$category <- read_db_categories(
          pool = glob$pool,
          active_project = glob$active_project
        )
      } else {
        warn_user("Category name must be unique.")
      }
    })

    # Delete categories ------
    # delete UI
    output$category_delete <- renderUI({
      req(glob$active_project)
      delete_category_UI(id,
        pool = glob$pool,
        active_project = glob$active_project
      )
    })
    outputOptions(output, "category_delete", suspendWhenHidden = FALSE)

    # delete action
    observeEvent(input$category_remove, {

      # remove from db
      delete_db_category(
        pool = glob$pool,
        active_project = glob$active_project,
        user_id = glob$user_id,
        delete_cat_id = input$categories_to_del
      )

      # remove from edges
      edge <- list()
      edge$category_id <- input$categories_to_del
      delete_category_code_record(
          pool = glob$pool,
          active_project = glob$active_project,
          user_id = glob$user$user_id,
          edge = edge)

      # refresh delete UI
      updateSelectInput(
        session = session,
        inputId = "categories_to_del",
        choices = c("", read_db_categories(
          pool = glob$pool,
          active_project = glob$active_project
        ))
      )

      # refresh listed categories
      output$categories_ui <- renderUI({
        render_categories(
          id = id,
          pool = glob$pool,
          active_project = glob$active_project
        )
      })

      # update return value
      glob$category <- read_db_categories(
        pool = glob$pool,
        active_project = glob$active_project
      )
    })

    # Create edge
    observeEvent(input$edges_category, {
        add_category_code_record(
            pool = glob$pool,
            active_project = glob$active_project,
            user_id = glob$user$user_id,
            edge = input$edges_category)
    })
    
    # Delete edge
    observeEvent(input$edges_category_delete, {
        delete_category_code_record(
            pool = glob$pool,
            active_project = glob$active_project,
            user_id = glob$user$user_id,
            edge = input$edges_category_delete)
    })

    # return active categories details in glob$category ----


  })
}
