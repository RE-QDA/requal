// Function to calculate selection positions
function getCaretCharacterOffsetWithin(element) {
  var caretOffset = 0;
  var doc = element.ownerDocument || element.document;
  var win = doc.defaultView || doc.parentWindow;
  var sel;
  //var sel = sel.replace(/[\t\n\r ]+/g, "");
  if (typeof win.getSelection != "undefined") {
      sel = win.getSelection();
      // https://stackoverflow.com/questions/7224368/how-do-i-remove-siblings-from-the-dom
    //   var previous = sel.previousSibling;
      
      // iterate until we find an element node or there is no previous sibling
      // while(previous && previous.nodeType !== 1) {
        //   previous = previous.previousSibling;
       // }

       // if there is a sibling, remove it
      // if(previous) {
        //   previous.parentNode.removeChild(previous);
      // }
      if (sel.rangeCount > 0) {
          var range = win.getSelection().getRangeAt(0);
          var preCaretRange = range.cloneRange();
          preCaretRange.selectNodeContents(element);
          preCaretRange.setEnd(range.endContainer, range.endOffset);
          caretOffset = preCaretRange.toString().length;
       }
  } else if ( (sel = doc.selection) && sel.type != "Control") {
      var textRange = sel.createRange();
      var preCaretTextRange = doc.body.createTextRange();
      preCaretTextRange.moveToElementText(element);
      preCaretTextRange.setEndPoint("EndToEnd", textRange);
      caretOffset = preCaretTextRange.text.length;
  }
  return caretOffset;
}

// Function to send calculated positions to Shiny
$( document ).ready(function() {
document.addEventListener('mouseup', function () {
    var sel = window.getSelection();
    // if(window.getSelection().baseNode.parentNode.id != "document_code_ui_1-focal_text") return;
    
    if(sel.rangeCount > 0){
      var range = sel.getRangeAt(0);
      var el = document.getElementById("document_code_ui_1-focal_text")
    
      var endOffset = getCaretCharacterOffsetWithin(el);
      var startOffset_js = endOffset - range.toString().length;
      var startOffset = startOffset_js+1;
      var text_length = $('#document_code_ui_1-focal_text').text().length;

      if (endOffset == 0) {
        var endOffset = endOffset+1;
      } 
      if (startOffset > endOffset) {
        var endOffset = startOffset; 
      } 
      if (startOffset < 1) {
        var startOffset = 1;
      } 
      if (endOffset > text_length) {
        var endOffset = text_length;
      } 
      
      var tag_position_value = startOffset.toString() + '-' + endOffset.toString();
        console.log("tag_position" + tag_position_value)
      Shiny.setInputValue('document_code_ui_1-tag_position', tag_position_value);
    }
}, false);
})
// Obtain information from iframe and send to Shiny
$(document).ready(function() {
  var iframe = document.getElementsByTagName('iframe')[0];

  // Ensure the iframe is loaded before accessing its content
  iframe.onload = function() {
    var res = iframe.contentDocument.getElementById('quickCodeInput');

    if (res) {
      // Define a callback function to execute when mutations are observed
      var observerCallback = function(mutationsList, observer) {
        for (var mutation of mutationsList) {
          if (mutation.type === 'attributes' && mutation.attributeName === 'data-quickode') {
            var quickodeValue = res.dataset.quickode;
            Shiny.setInputValue('document_code_ui_1-quickcode', quickodeValue);
          }
        }
      };

      // Create an observer instance linked to the callback function
      var observer = new MutationObserver(observerCallback);

      // Start observing the target node for configured mutations
      observer.observe(res, {
        attributes: true, // Observe attribute changes
        attributeFilter: ['data-quickode'] // Only observe changes to the 'data-quickode' attribute
      });

      // Initial setInputValue call to handle the current state
      var initialQuickodeValue = res.dataset.quickode;
      Shiny.setInputValue('document_code_ui_1-quickcode', initialQuickodeValue);
    } else {
      console.error('Element with id "quickCodeInput" not found in iframe.');
    }
  };
});

// Refresh iframe
Shiny.addCustomMessageHandler('refreshIframe', function(message) {
  var iframe = document.getElementsByTagName('iframe')[0];
  iframe.src = iframe.src;
  Shiny.setInputValue('document_code_ui_1-quickcode', '');
});

// Obtain information from memo iframe and send to Shiny
$(document).ready(function() {
  var iframe = document.getElementsByTagName('iframe')[1];

  // Ensure the iframe is loaded before accessing its content
  iframe.onload = function() {
    var segmentMemoElement = iframe.contentDocument.getElementById('segmentMemoInput');
    var segmentTypeElement = iframe.contentDocument.getElementById('segmentTypeInput');

    if (segmentMemoElement && segmentTypeElement) {
      // Function to update Shiny input with both values
      var updateShinyInput = function() {
        var segmentMemoValue = segmentMemoElement.dataset.memo_text || '';
        var segmentTypeValue = segmentTypeElement.dataset.memo_type || '';
        
        Shiny.setInputValue('document_code_ui_1-segment_memo', {
          text: segmentMemoValue,
          type: segmentTypeValue
        });
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

      observer.observe(segmentTypeElement, {
        attributes: true,
        attributeFilter: ['data-memo_type']
      });

      // Initial setInputValue call to handle the current state
      updateShinyInput();
    } else {
      console.error('Element with id "segmentMemoInput" or "segmentTypeInput" not found in iframe.');
    }
  };
});

// Functions for scrolling
$(document).ready(function() {
  Shiny.addCustomMessageHandler('scrollToSegment', function(arg) {

    let attempts = 0;
    const maxAttempts = 20; // Limit the number of attempts

    function attemptScroll() {
      let el = findScrollElement(arg);
      if (el) {
        console.log("Element found, scrolling into view:", el);
        el.scrollIntoView({ behavior: 'smooth', block: 'start' });
        clearInterval(scrollInterval); // Stop further attempts once the element is found and scrolled to
      } else {
        attempts++;
        console.log("Element not found, retrying... Attempt:", attempts);
        if (attempts >= maxAttempts) {
          console.log("Max attempts reached, stopping search.");
          clearInterval(scrollInterval);
        }
      }
    }
    // Set an interval to repeatedly attempt to find and scroll to the element
    const scrollInterval = setInterval(attemptScroll, 500); // Attempt every 500 milliseconds
  });
});

function findScrollElement(arg) {
  let targetStart = parseInt(arg.target_start, 10);
  let segments = document.querySelectorAll('article .segment');

  let matchingElement = Array.from(segments).find(el => {
    let startEnd = el.dataset.startend.split(' ');
    let startValue = parseInt(startEnd[0], 10);
    return startValue === targetStart;
  });

  if (matchingElement) {
    return matchingElement;
  } else {
    console.log("No matching segment found.");
    return null;
  }
}

// Helper for clearing content
Shiny.addCustomMessageHandler('clearContent', function(message) {
  let par = document.getElementById(message.id);
  par.innerHTML = '';
});

Shiny.addCustomMessageHandler('updateParagraphContent', function(message) {
  const container = document.getElementById(message.id);
  const fragment = document.createDocumentFragment();
  // Create new content
  const newElement = document.createElement('div');
  newElement.innerHTML = message.data;
  fragment.appendChild(newElement);
  // Replace existing content
  container.innerHTML = ''; // Clear existing content
  container.appendChild(fragment); // Add new content
});
    