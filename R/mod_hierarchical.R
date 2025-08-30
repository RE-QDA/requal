library(shiny)
library(purrr)

# 1. THE DATA
# ===============================================
test_df <- data.frame(
  id = c(1, 2, 3, 4),
  name = c("A", "B", "C", "D"),
  parent = c(NA, NA, 2, 3),
  is_draggable = c(FALSE, FALSE, TRUE, TRUE),
  stringsAsFactors = FALSE
)

# Add custom tags as a list column
test_df$custom_tags <- list(
  list(
    p("This is item A with custom content", style = "color: blue;"),
    actionButton("btn_a", "Button A", class = "btn-sm")
  ),
  list(
    div(
      "Item B has a special div",
      style = "background: yellow; padding: 5px;"
    ),
    span("And a span", style = "font-style: italic;")
  ),
  list(
    h5("Item C Header"),
    p("Some description for C")
  ),
  list(
    strong("Item D is important!"),
    br(),
    em("With emphasis")
  )
)

# 2. JAVASCRIPT LOGIC
# ===============================================
create_js_code <- function(namespace) {
  paste0(
    '
function initializeDragAndDrop() {
    const containerSelector = ".',
    namespace,
    '-tree-container";
    const dropZoneSelector = ".',
    namespace,
    '-subtree";
    const rootSelector = "#',
    namespace,
    '-root";

    let draggedElement = null;
    let originalParent = null;

    const rootElement = document.getElementById("',
    namespace,
    '-root");
    if (!rootElement) {
        console.error("Root element not found:", "',
    namespace,
    '-root");
        return;
    }

    // --- DRAG START ---
    rootElement.addEventListener("dragstart", function(event) {
        const target = event.target.closest(containerSelector);
        if (target && target.getAttribute("draggable") === "true") {
            draggedElement = target;
            originalParent = target.parentElement.closest(containerSelector);
            target.style.opacity = "0.5";
            event.dataTransfer.effectAllowed = "move";
        }
    });

    // --- DRAG END ---
    rootElement.addEventListener("dragend", function(event) {
        if (event.target === draggedElement) {
            event.target.style.opacity = "";
            draggedElement = null;
            originalParent = null;
        }
    });

    // --- DRAG OVER ---
    rootElement.addEventListener("dragover", function(event) {
        event.preventDefault();
        event.dataTransfer.dropEffect = "move";
    });
    
    // --- VISUAL FEEDBACK ---
    rootElement.addEventListener("dragenter", function(event) {
        if (!draggedElement) return;
        
        const dropTarget = event.target.closest(dropZoneSelector + ", " + rootSelector);
        if (dropTarget && !draggedElement.contains(dropTarget)) {
            dropTarget.style.backgroundColor = "#f0f8ff";
            dropTarget.style.borderColor = "#007bff";
        }
    });
    
    rootElement.addEventListener("dragleave", function(event) {
        const dropTarget = event.target.closest(dropZoneSelector + ", " + rootSelector);
        if (dropTarget) {
            dropTarget.style.backgroundColor = "";
            dropTarget.style.borderColor = "";
        }
    });

    // --- DROP ---
    rootElement.addEventListener("drop", function(event) {
        event.preventDefault();
        if (!draggedElement) return;
        
        let newParentId = null;
        let targetContainer = null;
        
        // Try to find a container target first
        const containerTarget = event.target.closest(containerSelector);
        if (containerTarget && !draggedElement.contains(containerTarget)) {
            targetContainer = containerTarget;
            newParentId = containerTarget.dataset.id;
        }
        
        // Clear visual feedback
        const allTargets = rootElement.querySelectorAll(dropZoneSelector + ", " + rootSelector);
        allTargets.forEach(target => {
            target.style.backgroundColor = "";
            target.style.borderColor = "";
        });
        
        // IMPORTANT: Do NOT move the DOM element here - let Shiny handle UI updates
        // Just send the data change to Shiny and let it re-render
        
        // Clean up original parent toggle button if it will be empty
        if (originalParent) {
            const originalSubtree = originalParent.querySelector(".tree_subtree");
            if (originalSubtree && originalSubtree.children.length === 1) {
                // This will be the last child, so remove toggle button
                const toggleButton = originalParent.querySelector(".tree_toggle");
                if (toggleButton) {
                    toggleButton.remove();
                }
            }
        }

        // Send update to Shiny using properly namespaced input
        Shiny.setInputValue("',
    namespace,
    '-tree_move", {
            child_id: parseInt(draggedElement.dataset.id),
            new_parent_id: newParentId ? parseInt(newParentId) : null,
            timestamp: Date.now()
        }, { priority: "event" });
        
        console.log("Move event sent to Shiny:", {
            child: draggedElement.dataset.id,
            newParent: newParentId || "root"
        });
    });
    
    // --- TOGGLE FUNCTIONALITY ---
    rootElement.addEventListener("click", function(event) {
        const toggleButton = event.target.closest(".tree_toggle");
        if (toggleButton) {
            event.stopPropagation();
            toggleSubtree(toggleButton);
        }
    });
}

function addToggleButtonIfNeeded(container, namespace) {
    // This function is no longer needed since Shiny handles UI updates
    // Toggle buttons are created during HTML generation based on data
}

function cleanupEmptyParent(parent, namespace) {
    // This function is no longer needed since Shiny handles UI updates
    // Cleanup happens during HTML generation based on data
}

function toggleSubtree(button) {
    const container = button.closest(".tree_container");
    const subtree = container.querySelector(".tree_subtree");
    const icon = button.querySelector("i");
    
    if (subtree.style.display === "none") {
        subtree.style.display = "block";
        icon.classList.remove("fa-caret-right");
        icon.classList.add("fa-caret-down");
    } else {
        subtree.style.display = "none";
        icon.classList.remove("fa-caret-down");
        icon.classList.add("fa-caret-right");
    }
}
'
  )
}

# 3. BUILD HTML FUNCTION
# ===============================================
build_tree <- function(df, ns, parent_id = NA_integer_) {
  # Filter children
  if (is.na(parent_id)) {
    children <- df[is.na(df$parent), , drop = FALSE]
  } else {
    children <- df[!is.na(df$parent) & df$parent == parent_id, , drop = FALSE]
  }

  if (nrow(children) == 0) {
    return(NULL)
  }

  purrr::map(1:nrow(children), function(i) {
    child <- children[i, ]
    has_children <- any(!is.na(df$parent) & df$parent == child$id)

    # Custom content
    custom_content <- if (
      !is.null(child$custom_tags) && length(child$custom_tags[[1]]) > 0
    ) {
      child$custom_tags[[1]]
    } else {
      NULL
    }

    # Main container
    container <- div(
      class = paste("tree_container", ns("tree-container")),
      `data-id` = child$id,

      # Header with optional toggle button and name
      div(
        class = "tree_header",
        if (has_children) {
          tags$button(
            class = "tree_toggle",
            HTML('<i class="fa fa-caret-down"></i>')
          )
        },
        strong(child$name)
      ),

      # Custom content
      if (!is.null(custom_content)) {
        div(class = "tree_custom", custom_content)
      },

      # Subtree for children
      div(
        class = paste("tree_subtree", ns("subtree")),
        build_tree(df, ns, parent_id = child$id)
      )
    )

    # Make draggable if specified
    if (isTRUE(child$is_draggable)) {
      container <- tagAppendAttributes(container, draggable = "true")
    }

    container
  })
}

# 4. SHINY MODULE UI
# ===============================================
mod_tree_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$head(
      tags$link(
        rel = "stylesheet",
        href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"
      ),
      tags$style(HTML(paste0(
        "
        /* Core functionality styles */
        .tree_toggle {
          background: none;
          border: none;
          padding: 2px 4px;
          cursor: pointer;
          font-size: 12px;
          margin-right: 8px;
          color: #666;
        }
        .tree_toggle:hover {
          background-color: #f0f0f0;
          border-radius: 2px;
        }
        .tree_container[draggable='true'] {
          cursor: move;
        }
        .tree_container[draggable='true']:hover {
          opacity: 0.8;
        }
        .tree_subtree {
          border-left: 1px dashed silver;
          padding-left: 30px;
          margin-left: 15px;
          display: block;
          transition: opacity 0.3s ease;
        }
        .tree_header {
          display: flex;
          align-items: center;
          margin: 4px 0;
        }
        .tree_custom {
          margin: 8px 0;
        }
        /* Drop zone visual feedback */
        .tree_subtree {
          border: 1px solid transparent;
          border-radius: 2px;
          min-height: 20px;
        }
        #",
        id,
        "-root {
          border: 2px dashed transparent;
          border-radius: 4px;
          min-height: 60px;
          padding: 10px;
          transition: all 0.2s ease;
        }
      "
      )))
    ),

    h3("Draggable Tree Structure"),

    div(
      id = ns("root"),
      class = "tree_root",
      uiOutput(ns("tree_structure"))
    ),

    # Initialize JavaScript
    tags$script(HTML(create_js_code(ns(NULL)))),
    tags$script(HTML(paste0(
      'document.addEventListener("DOMContentLoaded", function() {
        initializeDragAndDrop();
      });'
    )))
  )
}

# 5. SHINY MODULE SERVER
# ===============================================
mod_tree_server <- function(id, tree_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive data
    reactive_tree <- reactiveVal(tree_data)

    # Render tree
    output$tree_structure <- renderUI({
      build_tree(reactive_tree(), ns)
    })

    # Handle move events with proper namespacing
    observeEvent(input$tree_move, {
      event_data <- input$tree_move

      cat("=== TREE MOVE EVENT ===\n")
      cat("Child ID:", event_data$child_id, "\n")
      cat(
        "New Parent ID:",
        if (is.null(event_data$new_parent_id)) {
          "ROOT"
        } else {
          event_data$new_parent_id
        },
        "\n"
      )
      cat("Timestamp:", event_data$timestamp, "\n\n")

      # Update data
      current_tree <- reactive_tree()
      child_id <- as.integer(event_data$child_id)
      new_parent_id <- if (is.null(event_data$new_parent_id)) {
        NA_integer_
      } else {
        as.integer(event_data$new_parent_id)
      }

      # Validate the move
      if (!child_id %in% current_tree$id) {
        cat("ERROR: Child ID not found in tree\n")
        return()
      }

      if (!is.na(new_parent_id) && !new_parent_id %in% current_tree$id) {
        cat("ERROR: New parent ID not found in tree\n")
        return()
      }

      # Prevent circular references
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

        if (child_id %in% ancestors) {
          cat("ERROR: Circular reference prevented\n")
          return()
        }
      }

      # Apply the change
      current_tree$parent[current_tree$id == child_id] <- new_parent_id
      reactive_tree(current_tree)

      cat("Tree updated successfully!\n\n")
    })

    # Return reactive data and move events for external use
    return(list(
      tree_data = reactive_tree,
      last_move = reactive({
        input$tree_move
      })
    ))
  })
}

# 6. DEMO APPLICATION
# ===============================================
ui <- fluidPage(
  title = "Production Tree Module",

  tags$h2("Production-Ready Draggable Tree"),
  tags$p(
    "Drag items to reorganize the tree structure. Items can be nested or moved to root level."
  ),

  fluidRow(
    column(8, mod_tree_ui("main_tree")),
    column(
      4,
      tags$h4("Tree State"),
      verbatimTextOutput("tree_state"),
      tags$h4("Last Move"),
      verbatimTextOutput("last_move_info")
    )
  )
)

server <- function(input, output, session) {
  # Initialize tree module
  tree_module <- mod_tree_server("main_tree", test_df)

  # Display current tree state
  output$tree_state <- renderPrint({
    tree_data <- tree_module$tree_data()
    tree_data[, c("id", "name", "parent")]
  })

  # Display last move information
  output$last_move_info <- renderPrint({
    move_event <- tree_module$last_move()
    if (!is.null(move_event)) {
      list(
        child_moved = move_event$child_id,
        new_parent = if (is.null(move_event$new_parent_id)) {
          "ROOT"
        } else {
          move_event$new_parent_id
        },
        timestamp = move_event$timestamp
      )
    } else {
      "No moves yet"
    }
  })
}

shinyApp(ui = ui, server = server)
