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
mod_categories_server <- function(id, pool, project, user, codebook) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # set up return value object

    category <- reactiveVal()

    # update return value
    observeEvent(project(), {
    category(read_db_categories(
      pool, 
      active_project = project()))
      })

    # List existing codes in code boxes --------
    observeEvent(codebook(), {
      output$uncategorized <- renderUI({
        sortable::rank_list(
          input_id = ns("code_list"),
          text = NULL,
          labels = render_codes(
              pool, active_project = project()
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
          pool, 
          active_project = project(),
      )
    })

    # Relist categories on codebook changes
    observeEvent(codebook(), {

      output$categories_ui <- renderUI({
        render_categories(
          id = id,
          pool, 
          active_project = project()
        )
      })

    })


    #---Generate create categories UI --------------


    output$category_create <- renderUI({
      req(project())
      create_new_category_UI(id)
    })
    outputOptions(output, "category_create", suspendWhenHidden = FALSE)

    # Create categories ------
    observeEvent(input$category_add, {


      # check if code name is unique
      category_names <- list_db_categories(
        id = id,
        pool, 
        project_id = project()
      )$category_name

      if (!input$category_name %in% category_names & input$category_name != "") {
        categories_input_df <- data.frame(
          project_id = project(),
          category_name = input$category_name,
          category_description = input$category_desc
        )


        add_category_record(
          pool, 
          project_id = project(),
          user_id = user()$user_id,
          categories_df = categories_input_df
        )
        
        # refresh listed categories

        output$categories_ui <- renderUI({
          render_categories(
            id = id,
            pool, 
            active_project = project()
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
            pool, 
            active_project = project()
          ))
        )
        

        # update return value
        category(read_db_categories(
          pool, 
          active_project = project()
        ))
      } else {
        warn_user("Category name must be unique.")
      }
    })

    # Delete categories ------
    # delete UI
    output$category_delete <- renderUI({
      req(project())
      delete_category_UI(id,
        pool, 
        active_project = project()
      )
    })
    outputOptions(output, "category_delete", suspendWhenHidden = FALSE)


    # delete action
    observeEvent(input$category_remove, {

      # remove from db
      delete_db_category(
        pool, 
        active_project = project(),
        user_id = user()$user_id,
        delete_cat_id = input$categories_to_del
      )

      # remove from edges
      edge <- list()
      edge$category_id <- input$categories_to_del
      delete_category_code_record(
          pool, 
          active_project = project(),
          user_id = user()$user_id,
          edge = edge)

      # refresh delete UI
      updateSelectInput(
        session = session,
        inputId = "categories_to_del",
        choices = c("", read_db_categories(
          pool,
          active_project = project()
        ))
      )

      # refresh listed categories
      output$categories_ui <- renderUI({
        render_categories(
          id = id,
          pool, 
          active_project = project()
        )
      })

      # update return value
      category(read_db_categories(
        pool,
        active_project = project()
      ))
    })

  # Create edge
    observeEvent(input$edges_category, {
    add_category_code_record(
        pool, 
        active_project = project(),
        user_id = user()$user_id,
        edge = input$edges_category)
  })
  # Delete edge
    observeEvent(input$edges_category_delete, {
    delete_category_code_record(
        pool, 
        active_project = project(),
        user_id = user()$user_id,
        edge = input$edges_category_delete)
    })


    # return active categories details ----

    return(reactive(category()))

  })
}
