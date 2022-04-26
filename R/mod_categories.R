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


    output$category_create <- renderUI({
      req(project()$active_project)
      create_new_category_UI(id)
    })
    outputOptions(output, "category_create", suspendWhenHidden = FALSE)

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
          user_id = user()$user_id,
          categories_df = categories_input_df
        )
        
        # refresh listed categories

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
        
        # refresh delete UI
        updateSelectInput(
          session = session,
          inputId = "categories_to_del",
          choices = c("", read_db_categories(
            project_db = project()$project_db,
            active_project = project()$active_project
          ))
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
    output$category_delete <- renderUI({
      req(project()$active_project)
      delete_category_UI(id,
        project_db = project()$project_db,
        active_project = project()$active_project
      )
    })
    outputOptions(output, "category_delete", suspendWhenHidden = FALSE)


    # delete action
    observeEvent(input$category_remove, {

      # remove from db
      delete_db_category(
        project_db = project()$project_db,
        active_project = project()$active_project,
        user_id = user()$user_id,
        delete_cat_id = input$categories_to_del
      )

      # remove from edges
      edge <- list()
      edge$category_id <- input$categories_to_del
      delete_category_code_record(project_db = project()$project_db,
                     active_project = project()$active_project,
                     user_id = user()$user_id,
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
    add_category_code_record(project_db = project()$project_db,
                    active_project = project()$active_project,
                    user_id = user()$user_id,
                    edge = input$edges_category)
  })
  # Delete edge
    observeEvent(input$edges_category_delete, {
    delete_category_code_record(project_db = project()$project_db,
                               active_project = project()$active_project,
                               user_id = user()$user_id,
                               edge = input$edges_category_delete)
    })


    # return active categories details ----

    return(reactive(category()))

  })
}
