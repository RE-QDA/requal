# 1. BUILD HTML FUNCTION (same as before)
build_tree <- function(df, ns, parent_id = NA_integer_) {
  children <- if (is.na(parent_id)) {
    df[is.na(df$parent), , drop = FALSE]
  } else {
    df[!is.na(df$parent) & df$parent == parent_id, , drop = FALSE]
  }

  if (nrow(children) == 0) {
    return(NULL)
  }

  purrr::map(1:nrow(children), function(i) {
    child <- children[i, ]
    has_children <- any(!is.na(df$parent) & df$parent == child$id)
    custom_content <- if (
      !is.null(child$custom_tags) && length(child$custom_tags[[1]]) > 0
    ) {
      HTML(child$custom_tags[[1]])
    } else {
      NULL
    }

    # Include a caret placeholder for all nodes
    caret_icon <- '<div class="tree_toggle">
  <i class="fa fa-caret-right"></i>
</div>'

    subtree_class <- if (has_children) {
      paste("tree_subtree", ns("subtree"), "force-hidden")
    } else {
      paste("tree_subtree", ns("subtree"))
    }

    container <- div(
      class = paste("tree_container", ns("tree-container")),
      `data-id` = child$id,
      div(
        class = "tree_header",
        tags$button(
          class = "tree_toggle",
          HTML(caret_icon),
          style = if (has_children) "" else "visibility: hidden;"
        ),
        if (!is.null(custom_content)) {
          div(class = "tree_custom", custom_content)
        }
      ),
      div(
        class = subtree_class,
        build_tree(df, ns, parent_id = child$id)
      )
    )

    if (isTRUE(child$is_draggable)) {
      container <- tagAppendAttributes(container, draggable = "true")
    }

    container
  })
}
# 2. SHINY MODULE UI (CSS removed - put in your CSS file)
mod_tree_ui <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("tree_structure")) # This will be the main container
  )
}

# 3. SHINY MODULE SERVER (simplified - no constant re-initialization)
mod_tree_server <- function(id, tree_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    loc <- reactiveValues(reactive_tree = tree_data)

    output$tree_structure <- renderUI({
      isolate({
        build_tree(loc$reactive_tree, ns)
      })
    })

    observeEvent(input$tree_move, {
      cat(
        "Tree move event for",
        id,
        ":",
        input$tree_move$child_id,
        "->",
        input$tree_move$new_parent_id,
        "\n"
      )

      event_data <- input$tree_move
      current_tree <- loc$reactive_tree
      child_id <- as.integer(event_data$child_id)
      new_parent_id <- if (is.null(event_data$new_parent_id)) {
        NA_integer_
      } else {
        as.integer(event_data$new_parent_id)
      }

      if (
        !child_id %in% current_tree$id ||
          (!is.na(new_parent_id) && !new_parent_id %in% current_tree$id)
      ) {
        return()
      }

      if (!is.na(new_parent_id)) {
        ancestors <- c()
        current_parent <- new_parent_id
        while (!is.na(current_parent) && !(current_parent %in% ancestors)) {
          ancestors <- c(ancestors, current_parent)
          parent_row <- current_tree[current_tree$id == current_parent, ]
          current_parent <- if (nrow(parent_row) > 0) {
            parent_row$parent[1]
          } else {
            NA_integer_
          }
        }
        if (child_id %in% ancestors) return()
      }

      current_tree$parent[current_tree$id == child_id] <- new_parent_id
      loc$reactive_tree <- current_tree
    })

    return(list(
      tree_data = reactive(loc$reactive_tree),
      last_move = reactive(input$tree_move)
    ))
  })
}

# 4. USAGE - Multiple instances work automatically:
#
# UI:
# fluidPage(
#   mod_tree_ui("tree1"),      # Gets namespace "tree1"
#   mod_tree_ui("tree2"),      # Gets namespace "tree2"
#   mod_tree_ui("codebook")    # Gets namespace "codebook"
# )
#
# Server:
# function(input, output, session) {
#   mod_tree_server("tree1", tree_data_1)
#   mod_tree_server("tree2", tree_data_2)
#   mod_tree_server("codebook", codebook_data)
# }
