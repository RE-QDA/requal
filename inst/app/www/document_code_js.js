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


Shiny.addCustomMessageHandler('getIframeContent', function(message) {
  var iframe = document.getElementsByTagName('iframe')[0];
  var res = iframe.contentDocument.getElementById('quickCodeInput');
  var quickodeValue = res.dataset.quickode;
  Shiny.setInputValue('document_code_ui_1-quickcode', quickodeValue);
});

Shiny.addCustomMessageHandler('refreshIframe', function(message) {
  var iframe = document.getElementsByTagName('iframe')[0];
  iframe.src = iframe.src;
});

function findScrollElement(message) {
  let targetStart = parseInt(message, 10);
  console.log("Target Start:", targetStart);
  let segments = document.querySelectorAll('article .segment');
  let segmentStartValues = Array.from(segments).map(el => parseInt(el.dataset.segment_start, 10));
  let index = segmentStartValues.findIndex(value => value => targetStart);
  return segments[index]; // This could be undefined if no matching segment is found
}

$( document ).ready(function() {

Shiny.addCustomMessageHandler('scrollToSegment', function(message) {
  let el = findScrollElement(message);
  if (el) { // Check if the element exists before trying to scroll into view
      console.log("Scrolling to element:", el);
      scrollToElementWithinContainer(el);
  } else {
      console.log("No element found to scroll to for message:", message);
  }
});
});

function scrollToElementWithinContainer(targetSelected) {
  let container = document.querySelector('#document_code_ui_1-focal_text');
  let target = targetSelected;

  if (container && target) {
      let targetPosition = target.getBoundingClientRect().top;
      let containerPosition = container.getBoundingClientRect().top;
      let scrollPosition = targetPosition - containerPosition + container.scrollTop;
      
      container.scrollTo({ top: scrollPosition, behavior: 'smooth' });
  }
}