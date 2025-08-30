// Simple Global Tree Drag & Drop
// Save this as www/js/tree_drag_drop.js

$(document).ready(function() {
  
  // Global state storage to persist across Shiny updates
  window.treeStates = window.treeStates || {};
  
  function initializeTreeDragDrop(namespace) {
    console.log("Initializing tree drag drop for namespace:", namespace);
    
    const containerSelector = "." + namespace + "-tree-container";
    const subtreeSelector = "." + namespace + "-subtree";
    const rootSelector = "#" + namespace + "-root";
    
    let draggedElement = null;
    
    const rootElement = $(rootSelector);
    if (rootElement.length === 0) {
      console.error("Root element not found:", rootSelector);
      return;
    }
    
    // Initialize global state for this namespace if it doesn't exist
    if (!window.treeStates[namespace]) {
      window.treeStates[namespace] = {};
    }
    
    // Remove existing handlers first to prevent duplicates
    rootElement.off('.treeDragDrop');
    
    // Function to save current tree states to global storage
    function saveAllTreeStates() {
      rootElement.find(containerSelector).each(function() {
        const $container = $(this);
        const id = $container.data('id');
        const subtree = $container.find('.tree_subtree').first();
        
        if (id && subtree.length) {
          window.treeStates[namespace][id] = subtree.is(':visible');
        }
      });
      console.log("Saved tree states:", window.treeStates[namespace]);
    }
    
    // Function to restore tree states from global storage
    function restoreAllTreeStates() {
      console.log("Restoring tree states:", window.treeStates[namespace]);
      
      Object.keys(window.treeStates[namespace]).forEach(function(id) {
        const isExpanded = window.treeStates[namespace][id];
        const $container = rootElement.find(containerSelector + '[data-id="' + id + '"]');
        
        if ($container.length) {
          const subtree = $container.find('.tree_subtree').first();
          const icon = $container.find('.tree_toggle i').first();
          
          if (subtree.length && icon.length) {
            if (isExpanded) {
              subtree.css('display', 'block');
              icon.removeClass('fa-caret-right').addClass('fa-caret-down');
            } else {
              subtree.css('display', 'none');
              icon.removeClass('fa-caret-down').addClass('fa-caret-right');
            }
          }
        }
      });
    }
    
    // Save states initially and restore any existing states
    setTimeout(function() {
      restoreAllTreeStates();
      saveAllTreeStates();
    }, 50);
    
    // Drag start
    rootElement.on('dragstart.treeDragDrop', function(event) {
      const target = $(event.target).closest(containerSelector);
      if (target.length && target.attr("draggable") === "true") {
        draggedElement = target[0];
        target.find(".tree_custom").addClass("dragging");
        
        // Important: Set dataTransfer properly
        if (event.originalEvent && event.originalEvent.dataTransfer) {
          event.originalEvent.dataTransfer.effectAllowed = "move";
          event.originalEvent.dataTransfer.setData("text/plain", ""); // Required for some browsers
        }
        
        // Save current states before any changes
        saveAllTreeStates();
        
        // Create root drop overlay
        const overlay = $('<div>')
          .attr('id', namespace + '-root-drop-overlay')
          .text('ROOT')
          .css({
            position: 'absolute',
            top: '5px',
            left: '5px',
            width: '50px',
            height: '25px',
            backgroundColor: 'rgba(255, 0, 0, 0.3)',
            border: '2px dashed #ff0000',
            color: '#ff0000',
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
            fontSize: '10px',
            fontWeight: 'bold',
            zIndex: 1000,
            pointerEvents: 'auto',
            cursor: 'pointer'
          });
        
        rootElement.css('position', 'relative').append(overlay);
        
        // Overlay events
        overlay.on('dragover.overlayDrop', function(e) {
          e.preventDefault();
          e.stopPropagation();
          if (e.originalEvent && e.originalEvent.dataTransfer) {
            e.originalEvent.dataTransfer.dropEffect = "move";
          }
          $(this).css('backgroundColor', 'rgba(255, 0, 0, 0.5)');
        });
        
        overlay.on('dragleave.overlayDrop', function(e) {
          e.stopPropagation();
          $(this).css('backgroundColor', 'rgba(255, 0, 0, 0.3)');
        });
        
        overlay.on('drop.overlayDrop', function(e) {
          e.preventDefault();
          e.stopPropagation();
          console.log("Dropped on root");
          
          if (draggedElement) {
            Shiny.setInputValue(namespace + "-tree_move", {
              child_id: parseInt($(draggedElement).data('id')),
              new_parent_id: null,
              timestamp: Date.now()
            }, { priority: "event" });
            
            // Restore states after Shiny processes the change
            setTimeout(function() {
              restoreAllTreeStates();
            }, 100);
          }
        });
      }
    });
    
    // Drag end - cleanup
    rootElement.on('dragend.treeDragDrop', function(event) {
      if (event.target === draggedElement) {
        $(event.target).find(".tree_custom").removeClass("dragging");
        $(".highlight-drop").removeClass("highlight-drop");
        
        // Remove overlay and its events
        const overlay = $("#" + namespace + "-root-drop-overlay");
        if (overlay.length) {
          overlay.off('.overlayDrop').remove();
        }
        
        draggedElement = null;
      }
    });
    
    // Drag over - allow drop
    rootElement.on('dragover.treeDragDrop', function(event) {
      const target = $(event.target).closest(containerSelector);
      if (target.length && draggedElement && target[0] !== draggedElement && !$.contains(draggedElement, target[0])) {
        event.preventDefault();
        if (event.originalEvent && event.originalEvent.dataTransfer) {
          event.originalEvent.dataTransfer.dropEffect = "move";
        }
      }
    });
    
    // Drag enter - visual feedback
    rootElement.on('dragenter.treeDragDrop', function(event) {
      if (!draggedElement) return;
      
      const target = $(event.target).closest(containerSelector);
      if (target.length && target[0] !== draggedElement && !$.contains(draggedElement, target[0])) {
        $(".highlight-drop").removeClass("highlight-drop");
        target.addClass("highlight-drop");
      }
    });
    
    // Drag leave - remove visual feedback
    rootElement.on('dragleave.treeDragDrop', function(event) {
      const target = $(event.target).closest(containerSelector);
      if (target.length) {
        target.removeClass("highlight-drop");
      }
    });
    
    // Drop - handle the drop
    rootElement.on('drop.treeDragDrop', function(event) {
      const target = $(event.target).closest(containerSelector);
      if (target.length && draggedElement && target[0] !== draggedElement && !$.contains(draggedElement, target[0])) {
        event.preventDefault();
        event.stopPropagation();
        
        console.log("Dropped on container:", target.data('id'));
        
        $(".highlight-drop").removeClass("highlight-drop");
        
        Shiny.setInputValue(namespace + "-tree_move", {
          child_id: parseInt($(draggedElement).data('id')),
          new_parent_id: parseInt(target.data('id')),
          timestamp: Date.now()
        }, { priority: "event" });
        
        // Restore states after Shiny processes the change
        setTimeout(function() {
          restoreAllTreeStates();
        }, 100);
      }
    });
    
    // Click for toggle - separate from drag/drop
    rootElement.on('click.treeDragDrop', '.tree_toggle', function(event) {
      event.stopPropagation();
      event.preventDefault();
      
      const $this = $(this);
      const container = $this.closest('.tree_container');
      const subtree = container.find('.tree_subtree').first();
      const icon = $this.find('i');
      const containerId = container.data('id');
      
      // Toggle visibility and icon
      if (subtree.is(':visible')) {
        subtree.css('display', 'none');
        icon.removeClass('fa-caret-down').addClass('fa-caret-right');
        // Update global state
        if (containerId) {
          window.treeStates[namespace][containerId] = false;
        }
      } else {
        subtree.css('display', 'block');
        icon.removeClass('fa-caret-right').addClass('fa-caret-down');
        // Update global state
        if (containerId) {
          window.treeStates[namespace][containerId] = true;
        }
      }
      
      console.log("Updated state for", containerId, "to", window.treeStates[namespace][containerId]);
    });
    
    // Listen for any DOM mutations (when Shiny updates the tree)
    if (window.MutationObserver) {
      const observer = new MutationObserver(function(mutations) {
        let shouldRestore = false;
        mutations.forEach(function(mutation) {
          if (mutation.type === 'childList' && mutation.addedNodes.length > 0) {
            // Check if tree containers were added
            for (let i = 0; i < mutation.addedNodes.length; i++) {
              const node = mutation.addedNodes[i];
              if (node.nodeType === Node.ELEMENT_NODE) {
                const $node = $(node);
                if ($node.hasClass(namespace + '-tree-container') || 
                    $node.find(containerSelector).length > 0) {
                  shouldRestore = true;
                  break;
                }
              }
            }
          }
        });
        
        if (shouldRestore) {
          console.log("Tree DOM updated, restoring states...");
          setTimeout(function() {
            restoreAllTreeStates();
          }, 50);
        }
      });
      
      observer.observe(rootElement[0], {
        childList: true,
        subtree: true
      });
    }
    
    console.log("Tree drag drop initialized for:", namespace);
  }
  
  // Global function to initialize from R
  window.initTreeDragDrop = function(namespace) {
    // Small delay to ensure DOM is fully ready
    setTimeout(function() {
      initializeTreeDragDrop(namespace);
    }, 100);
  };
  
});