// $(document).ready(function() {
//   // Listen for messages of type 'toggleStyle'
//   Shiny.addCustomMessageHandler('toggleStyle', function(mode) {
//     // Loop over all elements with the class 'segment'
//     $('.segment').each(function() {
//       var highlightTag = $(this);
//       var currentBackgroundColor = highlightTag.css('background-color');
//       var currentTextDecorationColor = highlightTag.css('text-decoration-color');

//       // Determine the new mode and apply styles accordingly
//       if (mode === 'underline') {
//         // If switching to underline, use the current background color for the underline
//         highlightTag.css('background-color', 'transparent'); // Clear background color
//         highlightTag.css('text-decoration', 'underline'); // Apply underline
//         highlightTag.css('text-decoration-color', currentBackgroundColor); // Use the current background color for underline
//       } else if (mode === 'background') {
//         // If switching to background, use the current underline color for the background
//         highlightTag.css('background-color', currentTextDecorationColor); // Apply underline color as background color
//         highlightTag.css('text-decoration', ''); // Remove underline
//         highlightTag.css('text-decoration-color', ''); // Remove underline color
//       }
//     });
//   });
// });
// function applyBackgroundColor(element) {
//   const color = element.getAttribute('data-color');
//   if (color) {
//     element.style.setProperty('--highlight-color', color);
//   }
// }

// $( document ).ready(function() {
//   window.addEventListener("load", function() {
//     const articleElement = document.getElementById('article');
//     if (articleElement) {
//       articleElement.querySelectorAll('.segment-code.background').forEach(applyBackgroundColor);
//       const observer = new MutationObserver((mutationsList) => {
//         for (const mutation of mutationsList) {
//           if (mutation.type === 'childList') {
//             mutation.addedNodes.forEach(node => {
//               if (node.nodeType === Node.ELEMENT_NODE && node.matches('.segment-code.background')) {
//                 applyBackgroundColor(node);
//               }
//               if (node.nodeType === Node.ELEMENT_NODE) {
//                 node.querySelectorAll('.segment-code.background').forEach(applyBackgroundColor);
//               }
//             });
//           }
//         }
//       });
//       observer.observe(articleElement, { childList: true, subtree: true });
//     }
//   });
// });

$(document).ready(function() {
    var dragged;

    document.addEventListener('dragstart', function(event) {
        // Store a reference to the dragged element
        dragged = event.target.closest('.code_container'); // Ensure we get the entire container
        console.log('Dragging:', dragged);
        event.target.style.opacity = 0.5;
    }, false);

    document.addEventListener('dragend', function(event) {
        event.target.style.opacity = '';
    }, false);

    document.addEventListener('dragover', function(event) {
        event.preventDefault();
    }, false);

    document.addEventListener('dragenter', function(event) {
        // Highlight potential drop target when the draggable element enters it
        if (event.target.classList.contains('subcodes')) {
            event.target.style.background = '#f0f0f0';
        }
    }, false);

    document.addEventListener('dragleave', function(event) {
        // Reset background of potential drop target when the draggable element leaves it
        if (event.target.classList.contains('subcodes')) {
            event.target.style.background = '';
        }
    }, false);

    document.addEventListener('drop', function(event) {
        event.preventDefault();
        
        var targetContainer = event.target.closest('.code_container');
        var parentId = 0; // Default to 0 for outside drop

        if (targetContainer) {
            var subcodesDiv = targetContainer.querySelector('.subcodes');

            if (subcodesDiv) {
                console.log('Dropped on:', subcodesDiv);
                subcodesDiv.style.background = '';
                subcodesDiv.appendChild(dragged);
                parentId = targetContainer.id; // Set parentId to the target container's ID
            }
        } else {
            var fallbackContainer = document.getElementById('codebook_ui_1-codes_ui');
            if (fallbackContainer) {
                console.log('Dropped outside valid target, moving to:', fallbackContainer);
                fallbackContainer.insertBefore(dragged, fallbackContainer.firstChild);
            }
        }

        // Set the input value in Shiny
        var childId = extractNumberFromId(dragged.id); // Get the ID of the dragged element
        if (parentId !== 0) {
            parentId = extractNumberFromId(parentId)
        }
        Shiny.setInputValue('drop_event', { parent: parentId, child: childId }, { priority: 'event' });
    }, false);
});

function extractNumberFromId(id) {
    // Use a regular expression to match one or more digits
    var match = id.match(/\d+/);
    return match ? match[0] : null; // Return the first match or null if no match is found
}

function toggleSubcodes(button) {
  // Find the code_container element as the parent
  const parentElement = button.closest('.code_container');
  // Find the direct subcodes element within the parent
  const subcodesElement = parentElement.querySelector(':scope > .subcodes');

  // Toggle the display of the subcodes element
      if (subcodesElement.style.display === 'block' || subcodesElement.style.display === '') {
          subcodesElement.style.display = 'none';
          console.log('button')
          // Set icon to caret-right when hiding
          button.querySelector('i').classList.remove('fa-caret-down');
          button.querySelector('i').classList.add('fa-caret-right');

      } else {
            subcodesElement.style.display = 'block';
            // Set icon to caret-down when showing
            button.querySelector('i').classList.remove('fa-caret-right');
            button.querySelector('i').classList.add('fa-caret-down');console.log('right')

      }
 
}