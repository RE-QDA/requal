$(document).ready(function() {
    let draggedElement = null;
    let currentNamespace = null;

    // Global drag event handlers
    document.addEventListener('dragstart', function(event) {
        const container = event.target.closest('[class*="-tree-container"]');
        if (container && container.getAttribute("draggable") === "true") {
            draggedElement = container;
            
            // Extract namespace from class name
            const classList = Array.from(container.classList);
            const namespaceClass = classList.find(cls => cls.endsWith('-tree-container'));
            if (namespaceClass) {
                currentNamespace = namespaceClass.replace('-tree-container', '');
            }
            
            const customEl = container.querySelector('.tree_custom');
            if (customEl) {
                customEl.classList.add('dragging');
            }
            event.target.style.opacity = 0.5;
            
            // Create root drop overlay
            if (currentNamespace) {
                createRootDropOverlay(currentNamespace);
            }
        }
    }, false);

    document.addEventListener('dragend', function(event) {
        if (draggedElement) {
            event.target.style.opacity = '';
            const customEl = draggedElement.querySelector('.tree_custom');
            if (customEl) {
                customEl.classList.remove('dragging');
            }
            
            // Remove overlay
            if (currentNamespace) {
                const overlay = document.getElementById(currentNamespace + '-root-drop-overlay');
                if (overlay) {
                    overlay.remove();
                }
            }
            
            // Clear highlights
            document.querySelectorAll('.highlight-drop').forEach(el => {
                el.classList.remove('highlight-drop');
            });
            
            draggedElement = null;
            currentNamespace = null;
        }
    }, false);

    document.addEventListener('dragover', function(event) {
        if (draggedElement) {
            event.preventDefault();
        }
    }, false);

    document.addEventListener('dragenter', function(event) {
        if (draggedElement && currentNamespace) {
            const target = event.target.closest('.' + currentNamespace + '-tree-container');
            if (target && target !== draggedElement && !draggedElement.contains(target)) {
                document.querySelectorAll('.highlight-drop').forEach(el => {
                    el.classList.remove('highlight-drop');
                });
                target.classList.add('highlight-drop');
            }
        }
    }, false);

    document.addEventListener('dragleave', function(event) {
        if (draggedElement) {
            const target = event.target.closest('.' + currentNamespace + '-tree-container');
            if (target) {
                target.classList.remove('highlight-drop');
            }
        }
    }, false);

    document.addEventListener('drop', function(event) {
    event.preventDefault();

    if (!draggedElement || !currentNamespace) return;

    const target = event.target.closest('.' + currentNamespace + '-tree-container');
    let newParentId = null;
    let targetSubtree = null;

    if (target && target !== draggedElement && !draggedElement.contains(target)) {
        // Dropping on another container
        targetSubtree = target.querySelector('.tree_subtree');
        newParentId = parseInt(target.dataset.id);
    } else {
        // Check if dropped on root overlay
        const overlay = event.target.closest('#' + currentNamespace + '-root-drop-overlay');
        if (overlay) {
            // Dropping on root
            targetSubtree = document.getElementById(currentNamespace + '-tree_structure');
            newParentId = null;
        }
    }

    if (targetSubtree) {
        // Just move the element - don't touch any states
        if (targetSubtree.firstElementChild) {
            targetSubtree.insertBefore(draggedElement, targetSubtree.firstElementChild);
        } else {
            targetSubtree.appendChild(draggedElement);
        }

        // Clear highlights
        document.querySelectorAll('.highlight-drop').forEach(el => {
            el.classList.remove('highlight-drop');
        });

        // Tell Shiny about the move (just data, no UI changes)
        const childId = parseInt(draggedElement.dataset.id);
        Shiny.setInputValue(currentNamespace + "-tree_move", {
            child_id: childId,
            new_parent_id: newParentId,
            timestamp: Date.now()
        }, { priority: "event" });
    }
}, false);

   function createRootDropOverlay(namespace) {
    const treeStructureElement = document.getElementById(namespace + '-tree_structure');
    if (!treeStructureElement) return;

    const existingOverlay = document.getElementById(namespace + '-root-drop-overlay');
    if (existingOverlay) {
        existingOverlay.remove();
    }

    const overlay = document.createElement('div');
    overlay.id = namespace + '-root-drop-overlay';
    overlay.textContent = 'ROOT';
    overlay.style.cssText = `
        position: absolute;
        top: 5px;
        left: 5px;
        width: 50px;
        height: 100%;
        background-color: rgba(0, 0, 0, 0.1);
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 10px;
        font-weight: bold;
        z-index: 1000;
        pointer-events: auto;
        cursor: pointer;
    `;

    treeStructureElement.style.position = 'relative';
    treeStructureElement.appendChild(overlay);

    overlay.addEventListener('dragover', function(e) {
        e.preventDefault();
        e.stopPropagation();
        this.style.backgroundColor = 'rgba(255, 0, 0, 0.5)';
    });

    overlay.addEventListener('dragleave', function(e) {
        e.stopPropagation();
        this.style.backgroundColor = 'rgba(255, 0, 0, 0.3)';
    });
}

    window.initTreeDragDrop = function(namespace) {
        console.log("Tree drag drop ready for namespace:", namespace);
    };
});

// Simple toggle functionality
document.addEventListener('click', function(event) {
    if (event.target.closest('.tree_toggle')) {
        event.stopPropagation();
        event.preventDefault();
        
        const toggleButton = event.target.closest('.tree_toggle');
        const container = toggleButton.closest('[class*="-tree-container"]');
        const subtree = container.querySelector('.tree_subtree');
        const icon = toggleButton.querySelector('i');
        
        if (subtree && icon) {
            // Simple toggle - nothing fancy
            if (subtree.classList.contains('force-hidden')) {
                subtree.classList.remove('force-hidden');
                icon.classList.remove('fa-caret-right');
                icon.classList.add('fa-caret-down');
            } else {
                subtree.classList.add('force-hidden');
                icon.classList.remove('fa-caret-down');
                icon.classList.add('fa-caret-right');
            }
        }
    }
}, false);