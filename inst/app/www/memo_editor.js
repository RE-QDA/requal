$(document).ready(function() {
  // Function to set up the observer for the iframe
  function setupIframeObserver() {
    // Check if the second iframe is available
    var iframes = document.getElementsByTagName('iframe');
    if (iframes.length > 1) {
      var iframe = iframes[1]; // Get the second iframe

      // Ensure the iframe is loaded before accessing its content
      iframe.onload = function() {
        var segmentMemoElement = iframe.contentDocument.getElementById('segmentMemoInput');

        if (segmentMemoElement) {
          var updateShinyInput = function() {
            var segmentMemoValue = segmentMemoElement.dataset.memo_text || '';
            Shiny.setInputValue('document_code_ui_1-memo_segment_1-memo_editor_1-memo_text_input', segmentMemoValue);
            console.log('Updated Shiny input:', segmentMemoValue);
          };

          // Define a callback function to execute when mutations are observed
          var observerCallback = function(mutationsList, observer) {
            updateShinyInput();
          };

          // Create an observer instance linked to the callback function
          var memoObserver = new MutationObserver(observerCallback);

          // Start observing the target nodes for configured mutations
          memoObserver.observe(segmentMemoElement, {
            attributes: true,
            attributeFilter: ['data-memo_text']
          });

          // Initial setInputValue call to handle the current state
          updateShinyInput();
        } else {
          console.error('Element with id \"segmentMemoInput\" not found in iframe.');
        }
      };
    }
  }

  // Options for the observer (which mutations to observe)
  var config = { childList: true, subtree: true };

  // Callback function to execute when mutations are observed
  var callback = function(mutationsList, observer) {
    for (var mutation of mutationsList) {
      if (mutation.type === 'childList') {
        // Attempt to set up the iframe observer whenever children change
        setupIframeObserver();
      }
    }
  };

  // Create an observer instance linked to the callback function
  var observer = new MutationObserver(callback);

  // Start observing the document body for configured mutations
  observer.observe(document.body, config);

  console.log('Persistent MutationObserver is set up to detect the second iframe.');
});

Shiny.addCustomMessageHandler('resetSegmentMemoInput', function(message) {
  console.log('resetSegmentMemoInput called')
  var iframes = document.getElementsByTagName('iframe');
  if (iframes.length > 1) {
    var iframe = iframes[1]; // Get the second iframe

    // Ensure the iframe is loaded before accessing its content
    if (iframe.contentDocument) {
      var segmentMemoElement = iframe.contentDocument.getElementById('segmentMemoInput');
      
      if (segmentMemoElement) {
        // Reset the value of segmentMemoInput
        segmentMemoElement.value = ''; // Assuming it's an input or textarea
        segmentMemoElement.dataset.memo_text = ''; // Resetting the data attribute if needed

        // Optionally, trigger any events if needed
        var event = new Event('input', { bubbles: true });
        segmentMemoElement.dispatchEvent(event);

        console.log('segmentMemoInput has been reset.');
      } else {
        console.error('Element with id \"segmentMemoInput\" not found in iframe.');
      }
    } else {
      console.error('Iframe content is not accessible.');
    }
  } else {
    console.error('Second iframe not found.');
  }
});



// Add event listeners to the text_memo_btn class
document.addEventListener('click', (e) => {
  // Restrict scope to elements within an article
    // Click on text memo icon
    if (e.target.classList.contains('text_memo_btn')) {
      handleSingleClick(e.target);
  }
});
// Function to handle single click
function handleSingleClick(target) {
  Shiny.setInputValue('document_code_ui_1-memo_segment_1-text_memo_click', target.id);
}

// Refresh memo id
Shiny.addCustomMessageHandler('resetMemoClick', function(message) {
  Shiny.setInputValue(message.ns_text_memo_click, null);
});

// Refresh memo id
Shiny.addCustomMessageHandler('updateEditorInput', function(message) {
  Shiny.setInputValue(message.ns_memo_id, message.id);
});


$(document).ready(function() {
const freeMemoElement = document.querySelector('.free_memo');

// Add a click event listener to the element
freeMemoElement.addEventListener('click', function() {
    // Toggle the 'free_memo_show' class on click
    freeMemoElement.classList.toggle('free_memo_show');
});
});

// Pin memo
Shiny.addCustomMessageHandler("makeDraggable", function(message) {
 // Create a MutationObserver to watch for the element
 const observer = new MutationObserver(function(mutationsList, observer) {
  // Check if the element is now in the DOM
  var element = document.getElementById(message.id);
  if (element) {
    resizeElement(element);
    dragElement(element);
    observer.disconnect(); // Stop observing once the element is found and processed
  }
});
// Start observing the document body for added nodes
observer.observe(document.body, { childList: true, subtree: true });
});

function dragElement(elmnt) {
  var pos1 = 0, pos2 = 0, pos3 = 0, pos4 = 0;
  if (document.getElementById(elmnt.id + "pin_header")) {
    /* if present, the header is where you move the DIV from:*/
    document.getElementById(elmnt.id + "pin_header").onmousedown = dragMouseDown;
  } else {
    /* otherwise, move the DIV from anywhere inside the DIV:*/
    elmnt.onmousedown = dragMouseDown;
  }

  function dragMouseDown(e) {
    e.preventDefault();
    // get the mouse cursor position at startup:
    pos3 = e.clientX;
    pos4 = e.clientY;
    document.onmouseup = closeDragElement;
    // call a function whenever the cursor moves:
    document.onmousemove = elementDrag;
  }

  function elementDrag(e) {
    e.preventDefault();
    // calculate the new cursor position:
    pos1 = pos3 - e.clientX;
    pos2 = pos4 - e.clientY;
    pos3 = e.clientX;
    pos4 = e.clientY;
    // set the element's new position:
    elmnt.style.top = (elmnt.offsetTop - pos2) + "px";
    elmnt.style.left = (elmnt.offsetLeft - pos1) + "px";
  }

  function closeDragElement() {
    /* stop moving when mouse button is released:*/
    document.onmouseup = null;
    document.onmousemove = null;
  }
}

function resizeElement(elmnt) {
  var startX, startY, startWidth, startHeight;
  var resizer = document.getElementById('resize_handle');
  
  resizer.onmousedown = function(e) {
    e.stopPropagation();  // Prevent the drag event from starting
    initResize(e);
  };

  function initResize(e) {
    startX = e.clientX;
    startY = e.clientY;
    startWidth = parseInt(document.defaultView.getComputedStyle(elmnt).width, 10);
    startHeight = parseInt(document.defaultView.getComputedStyle(elmnt).height, 10);
    document.documentElement.addEventListener('mousemove', doResize, false);
    document.documentElement.addEventListener('mouseup', stopResize, false);
  }

  function doResize(e) {
    elmnt.style.width = (startWidth + e.clientX - startX) + 'px';
    elmnt.style.height = (startHeight + e.clientY - startY) + 'px';
  }

  function stopResize(e) {
    document.documentElement.removeEventListener('mousemove', doResize, false);
    document.documentElement.removeEventListener('mouseup', stopResize, false);
  }
}

Shiny.addCustomMessageHandler("removeAllPinnedMemos", function(message) {
  var elements = document.getElementsByClassName(message.class);
  while(elements.length > 0){
      elements[0].parentNode.removeChild(elements[0]);
  }
});
// let maxZ = 2;
// // Function to handle double click
// function handleDoubleClick(target) {
//   target.style.zIndex = ++maxZ;
//   target.classList.toggle('show_memo');
// }
// document.addEventListener('dblclick', (e) => {
//   // Restrict scope to elements within an article
//   if (e.target.closest('article')) {
//     // Double click on text memo icon
//     if (e.target.classList.contains('text_memo_btn')) {
//       handleDoubleClick(e.target);
//     }
//   }
// });

Shiny.addCustomMessageHandler("getMemoParagraph", function(message) {
  // Get all paragraph elements with the class 'docpar'
  var paragraphs = document.querySelectorAll('.docpar');
  var target = parseInt(message.startOff, 10);
  // Iterate over each paragraph to find the one with the matching startOff
  paragraphs.forEach(function(paragraph) {
      // Get the data-startend attribute and split it into start and end
      var startEnd = paragraph.getAttribute('data-startend').split(' ');
      var start = parseInt(startEnd[0], 10);
      var end = parseInt(startEnd[1], 10);
      // Check if message.startOff is between start and end
      if (target >= start && target <= end) {
          // Construct the new ID by appending "info_" to the paragraph's ID
          var newId = "info_" + paragraph.id;
          // Set the Shiny input 'active_memo_par' to the paragraph's ID
          Shiny.setInputValue('document_code_ui_1-memo_segment_1-memo_editor_1-active_memo_par', newId);
      }
  });
});