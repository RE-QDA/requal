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

Shiny.addCustomMessageHandler('getIframeContent', function(message) {
  var iframe = document.getElementsByTagName('iframe')[0];
  var res = iframe.contentDocument.getElementById('quickCodeInput');
  var quickodeValue = res.dataset.quickode;
  Shiny.setInputValue('document_code_ui_1-quickcode', quickodeValue);
});

// Refresh iframe

Shiny.addCustomMessageHandler('refreshIframe', function(message) {
  var iframe = document.getElementsByTagName('iframe')[0];
  iframe.src = iframe.src;
});


// Functions for scrolling
// TODO


function findScrollElement(message) {
  let targetStart = parseInt(message, 10);
  console.log("Target Start:", targetStart);
  let segments = document.querySelectorAll('article .segment');
  let segmentStartValues = Array.from(segments).map(el => parseInt(el.dataset.segment_start, 10));
  let index = segmentStartValues.findIndex(value => value => targetStart);
  return segments[index]; // This could be undefined if no matching segment is found
}

$(document).ready(function() {
  Shiny.addCustomMessageHandler('scrollToSegment', function(message) {
    const observer = new MutationObserver((mutationsList, observer) => {
      let el = findScrollElement(message);
      if (el) {
        console.log("Scrolling to element:", el);
        scrollToElementWithinContainer(el);
        observer.disconnect(); // Stop observing once the element is found and scrolled to
      }
    });

    // Start observing the #article element for changes
    const articleElement = document.getElementById('article');
    if (articleElement) {
      observer.observe(articleElement, { childList: true, subtree: true });
    } else {
      console.log("No #article element found to observe.");
    }
  });

  function findScrollElement(message) {
    // Implement your logic to find the scroll element based on the message
    // Example: return document.getElementById(message.id);
    return document.querySelector(message.selector); // Adjust based on your message structure
  }

  function scrollToElementWithinContainer(el) {
    // Implement your logic to scroll to the element within its container
    el.scrollIntoView({ behavior: 'smooth', block: 'center' });
  }
});


function updateParagraphContent(message) {

  // Locate the paragraph using the par_id
  console.log(1)
  console.log(data)
  const paragraph = document.querySelector(`#${data.par_id}`);

  if (paragraph) {
    // Replace the content of the paragraph based on the provided data
    // Example: You might want to update the text or other attributes
    paragraph.textContent = `Updated content for ${data.par_id}`;
  }
}



Shiny.addCustomMessageHandler('clearContent', function(message) {
  let par = document.getElementById(message.id);
  par.innerHTML = '';
});

Shiny.addCustomMessageHandler('updateParagraphContent', function(message) {
  const container = document.getElementById(message.id);
  const fragment = document.createDocumentFragment();

// Create new content
const newElement = document.createElement('span');
newElement.innerHTML = message.data;
fragment.appendChild(newElement);

// Replace existing content
container.innerHTML = ''; // Clear existing content
container.appendChild(fragment); // Add new content

});
    
