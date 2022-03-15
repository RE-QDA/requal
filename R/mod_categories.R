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

  tagList(
    fluidRow(
      actionButton(ns("category_create"), "Create category"),
      actionButton(ns("category_delete"), "Delete category")
    ),
    fluidRow(
      column(
        width = 6,
        tags$br(),
        uiOutput(ns("categories_manager")),
        uiOutput(ns("uncategorized"))
      ),
      column(
        width = 6,
        tags$br(),
        uiOutput(
          ns("categories_ui")
        )
      )
    )
  )
}

#' categories Server Functions
#'
#' @noRd
mod_categories_server <- function(id, project, user, codebook) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # set up return value object
    
    category <- reactiveVal()

    # update return value
    observeEvent(project()$active_project, {
    category(read_db_categories(
      project_db = project()$project_db,
      active_project = project()$active_project))
      })

    # List existing codes in code boxes --------
    observeEvent(codebook(), {
      output$uncategorized <- renderUI({
        sortable::rank_list(
          input_id = ns("code_list"),
          text = NULL,
          labels = render_codes(
            active_project = project()$active_project,
            project_db = project()$project_db
          ),
          class = "codes-rank-list",
          options = sortable::sortable_options(
            sort = TRUE,
            group = list(
              name = "categories",
              pull = "clone",
              put = TRUE
            ),
            onAdd = htmlwidgets::JS("function (evt) {  this.el.removeChild(evt.item); }")
          )
        )
      })
    })

    # List existing categories in category boxes ----
    output$categories_ui <- renderUI({
      render_categories(
        id = id,
        active_project = project()$active_project,
        project_db = project()$project_db
      )
    })
    
    # Relist categories on codebook changes
    observeEvent(codebook(), {
      
      output$categories_ui <- renderUI({
        render_categories(
          id = id,
          active_project = project()$active_project,
          project_db = project()$project_db
        )
      })
      
    })


    #---Generate create categories UI --------------

    observeEvent(input$category_create, {
      output$categories_manager <- renderUI({
        create_new_category_UI(id)
      })
    })

    # Create categories ------
    observeEvent(input$category_add, {


      # check if code name is unique
      category_names <- list_db_categories(
        id = id,
        project_db = project()$project_db,
        project_id = project()$active_project
      )$category_name

      if (!input$category_name %in% category_names & input$category_name != "") {
        con <- DBI::dbConnect(RSQLite::SQLite(), project()$project_db)
        on.exit(DBI::dbDisconnect(con))

        categories_input_df <- data.frame(
          project_id = project()$active_project,
          category_name = input$category_name,
          category_description = input$category_desc
        )


        add_category_record(
          con = con,
          project_id = project()$active_project,
          user = user,
          categories_df = categories_input_df
        )

        output$categories_ui <- renderUI({
          render_categories(
            id = id,
            active_project = project()$active_project,
            project_db = project()$project_db
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
          inputId = "category_description",
          value = ""
        )
        
        # update return value
        category(read_db_categories(
          project_db = project()$project_db,
          active_project = project()$active_project
        ))
      } else {
        warn_user("Category name must be unique.")
      }
    })

    # Delete categories ------
    # delete UI
    observeEvent(input$category_delete, {
      output$categories_manager <- renderUI({
        delete_category_UI(id,
          project_db = project()$project_db,
          active_project = project()$active_project
        )
      })
    })
    
    # delete action
    observeEvent(input$category_remove, {

      # remove from db
      delete_db_category(
        project_db = project()$project_db,
        active_project = project()$active_project,
        user = user,
        delete_cat_id = input$categories_to_del
      )
      
      # remove from edges
      edge <- list()
      edge$category_id <- input$categories_to_del
      delete_db_edge(project_db = project()$project_db,
                     active_project = project()$active_project,
                     user = user,
                     edge = edge)

      # refresh delete UI
      updateSelectInput(
        session = session,
        inputId = "categories_to_del",
        choices = c("", read_db_categories(
          project_db = project()$project_db,
          active_project = project()$active_project
        ))
      )

      # refresh listed categories
      output$categories_ui <- renderUI({
        render_categories(
          id = id,
          active_project = project()$active_project,
          project_db = project()$project_db
        )
      })
      
      # update return value
      category(read_db_categories(
        project_db = project()$project_db,
        active_project = project()$active_project
      ))
    })

  # Create edge
    observeEvent(input$edges_category, {
    add_edge_record(project_db = project()$project_db,
                    active_project = project()$active_project,
                    user = user,
                    edge = input$edges_category)
  })
  # Delete edge
    observeEvent(input$edges_category_delete, {
    delete_db_edge(project_db = project()$project_db,
                               active_project = project()$active_project,
                               user = user,
                               edge = input$edges_category_delete) 
    })
    
    
    # return active categories details ----
    
    return(reactive(category()))
    
  })
}
