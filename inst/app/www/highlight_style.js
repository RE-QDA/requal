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

// $(document).ready(function() {
//     var dragged;
//     var originalContainer;

//     document.addEventListener('dragstart', function(event) {
//         dragged = event.target.closest('.code_container'); 
//         originalContainer = dragged.parentElement.closest('.code_container'); // Store the original container
//         event.target.style.opacity = 0.5;
//     }, false);

//     document.addEventListener('dragend', function(event) {
//         event.target.style.opacity = '';
//     }, false);

//     document.addEventListener('dragover', function(event) {
//         event.preventDefault();
//     }, false);

//     document.addEventListener('dragenter', function(event) {
//         if (event.target.classList.contains('subcodes')) {
//             event.target.style.background = '#f0f0f0';
//         }
//     }, false);

//     document.addEventListener('dragleave', function(event) {
//         if (event.target.classList.contains('subcodes')) {
//             event.target.style.background = '';
//         }
//     }, false);

//     document.addEventListener('drop', function(event) {
//         event.preventDefault();

//         var targetContainer = event.target.closest('.code_container');
//         var parentId = 0;

//         if (targetContainer) {
//             var subcodesDiv = targetContainer.querySelector('.subcodes');

//             if (subcodesDiv.firstChild) {
//                 subcodesDiv.insertBefore(dragged, subcodesDiv.firstChild);
//             } else {
//                 subcodesDiv.appendChild(dragged);
//             }

//             var codeContainer = targetContainer.closest('.code_container');
//             if (codeContainer) {
//                 var idMatch = codeContainer.id.match(/^code_container_(.+)$/);
//                 if (idMatch) {
//                     codeId = idMatch[1];
//                 }
//             }

//             if (codeId) {
//                 var parentButtonId = 'btn_code_id_' + codeId;
//                 if (!document.getElementById(parentButtonId)) {
//                     var toggleButton = document.createElement('button');
//                     toggleButton.id = parentButtonId;
//                     toggleButton.className = 'btn_code_id';
//                     toggleButton.innerHTML = '<i class="fas fa-caret-down btn_code_id"></i>';
//                     var boxWrap = targetContainer.querySelector('.box_wrap');
//                     if (boxWrap) {
//                         boxWrap.insertBefore(toggleButton, boxWrap.firstChild);
//                     }
//                 }
//             }
//         } else {
//             var fallbackContainer = document.getElementById('codebook_ui_1-codes_ui');
//             if (fallbackContainer) {
//                 console.log('Dropped outside valid target, moving to:', fallbackContainer);
//                 fallbackContainer.insertBefore(dragged, fallbackContainer.firstChild);
//             }
//         }

//         // Check if the original container's subcodes div is empty
//         if (originalContainer) {
//             var originalSubcodesDiv = originalContainer.querySelector('.subcodes');
//             if (originalSubcodesDiv && !originalSubcodesDiv.firstChild) {
//                 var originalToggleButton = originalContainer.querySelector('.btn_code_id');
//                 if (originalToggleButton) {
//                     originalToggleButton.remove();
//                 }
//             }
//         }

//         var childId = extractNumberFromId(dragged.id);
//         if (parentId !== 0) {
//             parentId = extractNumberFromId(parentId);
//         }
//         Shiny.setInputValue('drop_event', { parent: parentId, child: childId }, { priority: 'event' });
//     }, false);
// });

// function extractNumberFromId(id) {
//     // Use a regular expression to match one or more digits
//     var match = id.match(/\d+/);
//     return match ? match[0] : null; // Return the first match or null if no match is found
// }

// function toggleSubcodes(button) {
//   // Find the code_container element as the parent
//   const parentElement = button.closest('.code_container');
//   // Find the direct subcodes element within the parent
//   const subcodesElement = parentElement.querySelector(':scope > .subcodes');

//   // Toggle the display of the subcodes element
//       if (subcodesElement.style.display === 'block' || subcodesElement.style.display === '') {
//           subcodesElement.style.display = 'none';
//           console.log('button')
//           // Set icon to caret-right when hiding
//           button.querySelector('i').classList.remove('fa-caret-down');
//           button.querySelector('i').classList.add('fa-caret-right');

//       } else {
//             subcodesElement.style.display = 'block';
//             // Set icon to caret-down when showing
//             button.querySelector('i').classList.remove('fa-caret-right');
//             button.querySelector('i').classList.add('fa-caret-down');console.log('right')

//       }
 
// }