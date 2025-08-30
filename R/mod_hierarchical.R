library(shiny)
library(shinyjs)
library(purrr)

# 1. THE DATA
test_df <- data.frame(
  id = c(1, 2, 3, 4),
  name = c("A", "B", "C", "D"),
  parent = c(NA, NA, 2, 3),
  is_draggable = c(TRUE, TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE
)

test_df$custom_tags <- list(
  list(p("This is item A with custom content", style = "color: blue;")),
  list(div(
    "Item B has a special div",
    style = "background: yellow; padding: 5px;"
  )),
  list(p("Some description for C")),
  list(em("With emphasis"))
)

# 2. JAVASCRIPT LOGIC
create_js_code <- function(namespace) {
  paste0(
    'function initializeDragAndDrop() {
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

      rootElement.addEventListener("dragstart", function(event) {
          const target = event.target.closest(containerSelector);
          if (target && target.getAttribute("draggable") === "true") {
              draggedElement = target;
              originalParent = target.parentElement.closest(containerSelector);
              $(target).find(".tree_custom").addClass("dragging");
              event.dataTransfer.effectAllowed = "move";

              // Create a root drop overlay
              const rootDropOverlay = document.createElement("div");
              rootDropOverlay.id = "',
    namespace,
    '-root-drop-overlay";
              rootDropOverlay.textContent = "/";
              rootDropOverlay.style.position = "absolute";
              rootDropOverlay.style.top = "0";
              rootDropOverlay.style.left = "0";
              rootDropOverlay.style.width = "5%";
              rootDropOverlay.style.height = "100%";
              rootDropOverlay.style.backgroundColor = "rgba(0, 0, 0, 0.1)";
              rootDropOverlay.style.color = "#fff";
              rootDropOverlay.style.display = "flex";
              rootDropOverlay.style.alignItems = "center";
              rootDropOverlay.style.justifyContent = "center";
              rootDropOverlay.style.pointerEvents = "auto"; // Enable pointer events
              rootElement.style.position = "relative";
              rootElement.appendChild(rootDropOverlay);

              // Add event listener for drop on overlay
              rootDropOverlay.addEventListener("dragover", function(event) {
                  event.preventDefault();
                  event.dataTransfer.dropEffect = "move";
              });

              rootDropOverlay.addEventListener("drop", function(event) {
                  event.preventDefault();
                  if (!draggedElement) return;

                  // Move to root
                  $(".highlight-drop").removeClass("highlight-drop");

                  Shiny.setInputValue("',
    namespace,
    '-tree_move", {
                      child_id: parseInt(draggedElement.dataset.id),
                      new_parent_id: null,
                      timestamp: Date.now()
                  }, { priority: "event" });
              });
          }
      });

      rootElement.addEventListener("dragend", function(event) {
          if (event.target === draggedElement) {
              $(event.target).find(".tree_custom").removeClass("dragging");
              draggedElement = null;
              originalParent = null;

              // Remove the root drop overlay
              const rootDropOverlay = document.getElementById("',
    namespace,
    '-root-drop-overlay");
              if (rootDropOverlay) {
                  rootDropOverlay.remove();
              }
          }
      });

      rootElement.addEventListener("dragover", function(event) {
          event.preventDefault();
          event.dataTransfer.dropEffect = "move";
      });

      rootElement.addEventListener("dragenter", function(event) {
          if (!draggedElement) return;
          const dropTarget = event.target.closest(dropZoneSelector + ", " + rootSelector);
          if (dropTarget && !draggedElement.contains(dropTarget)) {
              $(dropTarget).addClass("highlight-drop");
          }
      });

      rootElement.addEventListener("dragleave", function(event) {
          const dropTarget = event.target.closest(dropZoneSelector + ", " + rootSelector);
          if (dropTarget) {
              $(dropTarget).removeClass("highlight-drop");
          }
      });

      rootElement.addEventListener("drop", function(event) {
          event.preventDefault();
          if (!draggedElement) return;

          let newParentId = null;
          const containerTarget = event.target.closest(containerSelector);

          // Determine if the drop target is a valid container
          if (containerTarget && !draggedElement.contains(containerTarget)) {
              newParentId = containerTarget.dataset.id;
          } else {
              // If the drop is outside any valid container, move to root
              newParentId = null;
          }

          $(".highlight-drop").removeClass("highlight-drop");

          Shiny.setInputValue("',
    namespace,
    '-tree_move", {
              child_id: parseInt(draggedElement.dataset.id),
              new_parent_id: newParentId,
              timestamp: Date.now()
          }, { priority: "event" });
      });

      rootElement.addEventListener("click", function(event) {
          const toggleButton = event.target.closest(".tree_toggle");
          if (toggleButton) {
              event.stopPropagation();
              toggleSubtree(toggleButton);
          }
      });
    }

    function toggleSubtree(button) {
      const container = button.closest(".tree_container");
      const subtree = container.querySelector(".tree_subtree");
      const icon = button.querySelector("i");

      $(subtree).toggle();
      $(icon).toggleClass("fa-caret-down fa-caret-right");
    }'
  )
}

# 3. BUILD HTML FUNCTION
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
      child$custom_tags[[1]]
    } else {
      NULL
    }

    container <- div(
      class = paste("tree_container", ns("tree-container")),
      `data-id` = child$id,
      div(
        class = "tree_header",
        if (has_children) {
          tags$button(
            class = "tree_toggle",
            HTML('<i class="fa fa-caret-down"></i>')
          )
        },
        if (!is.null(custom_content)) {
          div(class = "tree_custom", custom_content)
        }
      ),
      div(
        class = paste("tree_subtree", ns("subtree")),
        build_tree(df, ns, parent_id = child$id)
      )
    )

    if (isTRUE(child$is_draggable)) {
      container <- tagAppendAttributes(container, draggable = "true")
    }

    container
  })
}

# 4. SHINY MODULE UI
mod_tree_ui <- function(id) {
  ns <- NS(id)

  tagList(
    useShinyjs(),
    tags$head(
      tags$link(
        rel = "stylesheet",
        href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"
      ),
      tags$style(HTML(paste0(
        ".tree_toggle { background: none; border: none; padding: 2px 4px; cursor: pointer; font-size: 12px; margin-right: 8px; color: #666; }
         .tree_toggle:hover { background-color: #f0f0f0; border-radius: 2px; }
         .tree_container[draggable='true'] { cursor: default; }
         .tree_container[draggable='true'].dragging { cursor: move; }
         .tree_header { display: flex; align-items: center; }
         .tree_subtree { border-left: 1px dashed silver; padding-left: 30px; margin-left: 15px; display: block; transition: opacity 0.3s ease; }
         .tree_custom { margin-left: 8px; }
         .highlight-drop { background-color: rgba(0, 123, 255, 0.1); }
         .dragging { opacity: 0.5; }
         #",
        id,
        "-root { border: 2px dashed transparent; border-radius: 4px; min-height: 60px; padding: 10px; transition: all 0.2s ease; }
         #",
        id,
        "-root.highlight-drop { background-color: rgba(0, 123, 255, 0.1); }"
      )))
    ),
    h3("Draggable Tree Structure"),
    div(id = ns("root"), class = "tree_root", uiOutput(ns("tree_structure"))),
    tags$script(HTML(create_js_code(ns(NULL)))),
    tags$script(HTML(
      'document.addEventListener("DOMContentLoaded", function() { initializeDragAndDrop(); });'
    ))
  )
}

# 5. SHINY MODULE SERVER
mod_tree_server <- function(id, tree_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    loc <- reactiveValues(reactive_tree = tree_data)

    output$tree_structure <- renderUI({
      build_tree(loc$reactive_tree, ns)
    })

    observeEvent(input$tree_move, {
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

# 6. DEMO APPLICATION
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
  tree_module <- mod_tree_server("main_tree", test_df)

  output$tree_state <- renderPrint({
    tree_data <- tree_module$tree_data()
    tree_data[, c("id", "name", "parent")]
  })

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
