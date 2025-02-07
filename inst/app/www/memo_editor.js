Shiny.addCustomMessageHandler('initializeIframeHandler', function(message) {
  var iframe = document.getElementsByTagName('iframe')[1];

  // Ensure the iframe is loaded before accessing its content
  iframe.onload = function() {
    var segmentMemoElement = iframe.contentDocument.getElementById('segmentMemoInput');

    if (segmentMemoElement) {
      var updateShinyInput = function() {
        var segmentMemoValue = segmentMemoElement.dataset.memo_text || '';
        Shiny.setInputValue('document_code_ui_1-memo_segment_1-memo_editor_1-memo_text_external', segmentMemoValue);
        console.log(segmentMemoValue);
      };

      // Define a callback function to execute when mutations are observed
      var observerCallback = function(mutationsList, observer) {
        updateShinyInput();
      };

      // Create an observer instance linked to the callback function
      var observer = new MutationObserver(observerCallback);

      // Start observing the target nodes for configured mutations
      observer.observe(segmentMemoElement, {
        attributes: true,
        attributeFilter: ['data-memo_text']
      });

      // Initial setInputValue call to handle the current state
      updateShinyInput();
    } else {
      console.error('Element with id "segmentMemoInput" not found in iframe.');
    }
  };
});

// Refresh memo iframe
Shiny.addCustomMessageHandler('refreshMemoIframe', function(message) {
  var iframe = document.getElementsByTagName('iframe')[1];
  iframe.src = iframe.src;
  console.log('refresh');
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
  Shiny.setInputValue('document_code_ui_1-text_memo_click', target.id);
}

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