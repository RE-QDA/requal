// Function to replace the text of segmentMemoInput
function replaceSegmentMemoText(newText) {
  var iframe = document.getElementsByTagName('iframe')[1];

  // Ensure the iframe is loaded before accessing its content
  iframe.onload = function() {
    var segmentMemoElement = iframe.contentDocument.getElementById('segmentMemoInput');

    if (segmentMemoElement) {
      // Replace the text content or value of the element
      segmentMemoElement.textContent = newText; // Use textContent for plain text
      // segmentMemoElement.value = newText; // Use value if it's an input element
      segmentMemoElement.dataset.memo_text = newText; // Update data attribute if needed

      // Optionally, trigger the observer manually if needed
      var event = new Event('change');
      segmentMemoElement.dispatchEvent(event);

      console.log('Text replaced with:', newText);
    } else {
      console.error('Element with id "segmentMemoInput" not found in iframe.');
    }
  };
}

// Obtain information from memo iframe and send to Shiny
$(document).ready(function() {
  var iframe = document.getElementsByTagName('iframe')[1];

  // Ensure the iframe is loaded before accessing its content
  iframe.onload = function() {
    var segmentMemoElement = iframe.contentDocument.getElementById('segmentMemoInput');

    if (segmentMemoElement) {
      var updateShinyInput = function() {
        var segmentMemoValue = segmentMemoElement.dataset.memo_text || '';
        Shiny.setInputValue('memo_segment_1-memo_editor_1-memo', segmentMemoValue);
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
  Shiny.setInputValue('memo_segment_1-memo_editor_1-memo_text', '');
});

// Replace text in iframe when a custom message is received
Shiny.addCustomMessageHandler('replaceSegmentMemoText', function(message) {
  replaceSegmentMemoText(message.memo_text);
});

let maxZ = 2;

// Function to handle single click
function handleSingleClick(target) {
  Shiny.setInputValue('document_code_ui_1-text_memo_click', target.id + ' ' + Math.random());
  target.classList.remove('show_memo');
}

// Function to handle double click
function handleDoubleClick(target) {
  target.style.zIndex = ++maxZ;
  target.classList.toggle('show_memo');
}

// Add event listeners to the document
document.addEventListener('click', (e) => {
  // Restrict scope to elements within an article
  if (e.target.closest('article')) {
    // Click on text memo icon
    if (e.target.classList.contains('text_memo_btn')) {
      handleSingleClick(e.target);
    }
  }
});

document.addEventListener('dblclick', (e) => {
  // Restrict scope to elements within an article
  if (e.target.closest('article')) {
    // Double click on text memo icon
    if (e.target.classList.contains('text_memo_btn')) {
      handleDoubleClick(e.target);
    }
  }
});