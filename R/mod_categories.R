utils::globalVariables(c("category_id"))

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
      class = "module_tools",
      mod_rql_button_ui(
        ns("category_create"),
        label = "Create category",
        icon = "plus"
      ),
      mod_rql_button_ui(
        ns("category_delete"),
        label = "Delete category",
        icon = "minus"
      )
    ),
    fluidRow(
      class = "module_content",
      column(
        width = 5,
        tags$br(),
        actionButton(
          ns("toggle_all_codes"),
          label = NULL,
          icon = icon("expand-arrows-alt"),
          title = "Expand/Collapse",
          style = "color: silver; background: none; border: none; padding: 0; margin: 0;"
        ),
        uiOutput(ns("uncategorized")) %>%
          tagAppendAttributes(class = "scrollable90")
      ),
      column(
        width = 5,
        tags$br(),
        actionButton(
          ns("toggle_all_categories"),
          label = NULL,
          icon = icon("expand-arrows-alt"),
          title = "Expand/Collapse",
          style = "color: silver; background: none; border: none; padding: 0; margin: 0;"
        ),
        uiOutput(ns("categories_ui")) %>%
          tagAppendAttributes(class = "scrollable90")
      )
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
    observeEvent(
      c(
        glob$active_project,
        input$category_add,
        input$category_remove
      ),
      {
        #---Create category UI --------------
        mod_rql_button_server(
          id = "category_create",
          custom_title = "Create category",
          custom_tagList = create_new_category_UI(ns = ns),
          glob,
          permission = "codebook_modify"
        )
        #---Delete category UI --------------
        mod_rql_button_server(
          id = "category_delete",
          custom_title = "Delete category",
          custom_tagList = delete_category_UI(
            ns = ns,
            glob$pool,
            glob$active_project,
            glob$user
          ),
          glob,
          permission = "codebook_modify"
        )
        glob$category <- read_db_categories(
          pool = glob$pool,
          active_project = glob$active_project,
          user = NULL
        )
      }
    )

    # List existing codes in code boxes -------------------
    output$uncategorized <- renderUI({
      glob$codebook
      render_codes_ui(id, glob$pool, glob$active_project, glob$user)
    })

    # List existing categories in category boxes ----------
    output$categories_ui <- renderUI({
      render_categories(
        id = id,
        active_project = glob$active_project,
        pool = glob$pool,
        user = glob$user
      )
    })

    # Relist categories on codebook changes ---------------
    observeEvent(c(glob$codebook, glob$codebook_observer), {
      output$categories_ui <- renderUI({
        render_categories(
          id = id,
          active_project = glob$active_project,
          pool = glob$pool,
          user = glob$user
        )
      })
    })

    # Create categories ------
    observeEvent(input$category_add, {
      # check if code name is unique
      category_names <- list_db_categories(
        id = id,
        pool = glob$pool,
        project_id = glob$active_project
      )$category_name

      if (
        !input$category_name %in% category_names & input$category_name != ""
      ) {
        categories_input_df <- data.frame(
          project_id = glob$active_project,
          category_name = input$category_name,
          category_description = input$category_desc,
          user_id = glob$user$user_id
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
            active_project = glob$active_project,
            user = glob$user
          )
        })

        # update return value
        glob$category <- read_db_categories(
          pool = glob$pool,
          active_project = glob$active_project,
          user = glob$user
        )
      } else {
        warn_user("Category name must be unique.")
      }
    })

    # Delete categories ------
    observeEvent(input$category_remove, {
      # remove from edges
      edge <- list()
      edge$category_id <- input$categories_to_del
      delete_category_code_record(
        pool = glob$pool,
        active_project = glob$active_project,
        user_id = glob$user$user_id,
        edge = edge
      )

      # remove from db
      delete_db_category(
        pool = glob$pool,
        active_project = glob$active_project,
        user_id = glob$user_id,
        delete_cat_id = input$categories_to_del
      )

      # refresh listed categories
      output$categories_ui <- renderUI({
        render_categories(
          id = id,
          pool = glob$pool,
          active_project = glob$active_project,
          user = glob$user
        )
      })

      # update return value
      glob$category <- read_db_categories(
        pool = glob$pool,
        active_project = glob$active_project,
        user = glob$user
      )
    })

    # Create edge -----------------------------------------
    observeEvent(input$edges_category, {
      # check ownership
      owns_code <- glob$codebook %>%
        dplyr::filter(code_id == !!input$edges_category$code_id) %>%
        dplyr::pull(user_id) ==
        glob$user$user_id

      owns_category <- dplyr::tbl(glob$pool, "categories") %>%
        dplyr::filter(category_id == !!input$edges_category$category_id) %>%
        dplyr::collect() %>%
        dplyr::pull(user_id) ==
        glob$user$user_id

      # initialize as negative permission
      permission_check <- FALSE

      if (!is.null(glob$user) && glob$user$data$codebook_modify != 1) {
        # User has no permissions
        warn_user(
          "You don't have permissions for modifying codes and categories."
        )
      } else if (all(c(owns_code, owns_category))) {
        # Edge belongs to user
        permission_check <- TRUE
      } else if (
        !is.null(glob$user) && glob$user$data$codebook_other_modify == 1
      ) {
        # Edge belongs to others but user can modify all codes and categories
        permission_check <- TRUE
      } else {
        # User can edit own edges but this edge belongs to others
        warn_user(
          "You don't have permissions for modifying codes and categories created by others."
        )
      }

      if (permission_check) {
        add_category_code_record(
          pool = glob$pool,
          active_project = glob$active_project,
          user_id = glob$user$user_id,
          edge = input$edges_category
        )
      } else {
        # re-render categories
        output$categories_ui <- renderUI({
          render_categories(
            id = id,
            active_project = glob$active_project,
            pool = glob$pool,
            user = glob$user
          )
        })
      }
    })

    # Delete edge ----
    observeEvent(input$edges_category_delete, {
      # check ownership
      owns_code <- glob$codebook %>%
        dplyr::filter(code_id == !!input$edges_category_delete$code_id) %>%
        dplyr::pull(user_id) ==
        glob$user$user_id

      owns_category <- dplyr::tbl(glob$pool, "categories") %>%
        dplyr::filter(
          category_id == !!input$edges_category_delete$category_id
        ) %>%
        dplyr::collect() %>%
        dplyr::pull(user_id) ==
        glob$user$user_id

      # initialize as negative permission
      permission_check <- FALSE

      if (!is.null(glob$user) && glob$user$data$codebook_modify != 1) {
        # User has no permissions
        warn_user(
          "You don't have permissions for modifying codes and categories."
        )
      } else if (all(c(owns_code, owns_category))) {
        # Edge belongs to user
        permission_check <- TRUE
      } else if (
        !is.null(glob$user) && glob$user$data$codebook_other_modify == 1
      ) {
        # Edge belongs to others but user can modify all codes and categories
        permission_check <- TRUE
      } else {
        # User can edit own edges but this edge belongs to others
        warn_user(
          "You don't have permissions for modifying codes and categories created by others."
        )
      }

      if (permission_check) {
        delete_category_code_record(
          pool = glob$pool,
          active_project = glob$active_project,
          user_id = glob$user$user_id,
          edge = input$edges_category_delete
        )
      } else {
        # re-render categories
        output$categories_ui <- renderUI({
          render_categories(
            id = id,
            active_project = glob$active_project,
            pool = glob$pool,
            user = glob$user
          )
        })
      }
    })

    observeEvent(input$toggle_all_codes, {
      if (input$toggle_all_codes %% 2 == 1) {
        expand_codes_cat(session = session)
      } else {
        collapse_codes_cat(session = session)
      }
    })
    observeEvent(input$toggle_all_categories, {
      if (input$toggle_all_categories %% 2 == 1) {
        expand_categories(session)
      } else {
        collapse_categories(session)
      }
    })
    # return active categories details in glob$category ----
  })
}
