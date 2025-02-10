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