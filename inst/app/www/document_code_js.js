function getCaretCharacterOffsetWithin(element) {
  var caretOffset = 0;
  var doc = element.ownerDocument || element.document;
  var win = doc.defaultView || doc.parentWindow;
  var sel;

  if (typeof win.getSelection != "undefined") {
      sel = win.getSelection();
      if (sel.rangeCount > 0) {
          var range = sel.getRangeAt(0);
          var preCaretRange = range.cloneRange();
          preCaretRange.selectNodeContents(element);
          preCaretRange.setEnd(range.endContainer, range.endOffset);

          // Traverse the nodes within the range and calculate offset
          var walker = doc.createTreeWalker(element, NodeFilter.SHOW_TEXT, {
              acceptNode: function(node) {
                  // Exclude text nodes that are within elements with the class 'exclude-from-caret'
                  return node.parentNode.closest('.text_memo_extra') ? NodeFilter.FILTER_REJECT : NodeFilter.FILTER_ACCEPT;
              }
          });

          while (walker.nextNode()) {
              var node = walker.currentNode;
              if (node === range.endContainer) {
                  caretOffset += range.endOffset;
                  break;
              } else {
                  caretOffset += node.textContent.length;
              }
          }
      }
  } else if ((sel = doc.selection) && sel.type != "Control") {
      var textRange = sel.createRange();
      var preCaretTextRange = doc.body.createTextRange();
      preCaretTextRange.moveToElementText(element);
      preCaretTextRange.setEndPoint("EndToEnd", textRange);
      caretOffset = preCaretTextRange.text.length;
  }
  return caretOffset;
}

$(document).ready(function() {
document.addEventListener('mouseup', function() {
  var sel = window.getSelection();
  
  if (sel.rangeCount > 0) {
    var range = sel.getRangeAt(0);
    var el = document.getElementById("document_code_ui_1-focal_text");

    // Calculate the caret offset excluding elements with the class 'exclude-from-caret'
    var endOffset = getCaretCharacterOffsetWithin(el);
    var startOffset_js = endOffset - range.toString().length;
    var startOffset = startOffset_js + 1;
    var text_length = $(el).text().length;

    if (endOffset == 0) {
      endOffset = endOffset + 1;
    }
    if (startOffset > endOffset) {
      endOffset = startOffset;
    }
    if (startOffset < 1) {
      startOffset = 1;
    }
    if (endOffset > text_length) {
      endOffset = text_length;
    }

    var tag_position_value = startOffset.toString() + '-' + endOffset.toString();
    console.log("tag_position" + tag_position_value);
    Shiny.setInputValue('document_code_ui_1-tag_position', tag_position_value);
  }
}, false);
});
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
    