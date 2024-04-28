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

Shiny.addCustomMessageHandler('wrapTextWithBold', function(message) {
  const startOffset = message.startOffset;
  const endOffset = message.endOffset;
  const newId = message.newId;
  wrapTextWithBold(startOffset, endOffset, newId);
});

function wrapTextWithBold(startOffset, endOffset, newId) {
  const container = document.querySelector('#article');
  if (!container) {
    console.error("Container not found");
    return;
  }

  let globalOffset = 0;

  Array.from(container.childNodes).forEach((p) => {
    if (p.nodeType === Node.ELEMENT_NODE && p.tagName.toLowerCase() === 'p') {
      // Create a copy of the text content without <b> tags and a list of <b> tag positions
      let plainText = '';
      let bTagPositions = [];
      Array.from(p.childNodes).forEach(child => {
        if (child.nodeType === Node.TEXT_NODE) {
          plainText += child.textContent;
        } else if (child.nodeType === Node.ELEMENT_NODE && child.tagName.toLowerCase() === 'b') {
          bTagPositions.push({start: plainText.length, end: plainText.length + child.textContent.length, id: child.id});
          plainText += child.textContent;
        }
      });

      // Calculate the positions of the new <b> tag
      if (globalOffset <= endOffset && globalOffset + plainText.length >= startOffset) {
        const paragraphStart = Math.max(startOffset - globalOffset, 0);
        const paragraphEnd = Math.min(endOffset - globalOffset, plainText.length);
        const newBTagPosition = {start: paragraphStart, end: paragraphEnd, id: newId};
        bTagPositions.push(newBTagPosition);
      }

      // Sort the list of <b> tag positions
      bTagPositions.sort((a, b) => a.start - b.start);

      // Merge overlapping <b> tag positions
      let mergedBTagPositions = [];
      if (bTagPositions.length > 0) {
        let currentBTagPosition = bTagPositions[0];
        for (let i = 1; i < bTagPositions.length; i++) {
          if (bTagPositions[i].start <= currentBTagPosition.end) {
            // Overlapping <b> tags, merge them into a single tag
            if (bTagPositions[i].id !== currentBTagPosition.id) {
              currentBTagPosition.end = bTagPositions[i].start;
              mergedBTagPositions.push(currentBTagPosition);
              currentBTagPosition = {start: bTagPositions[i].start, end: bTagPositions[i].end, id: currentBTagPosition.id + '+' + bTagPositions[i].id};
            } else {
              currentBTagPosition.end = Math.max(currentBTagPosition.end, bTagPositions[i].end);
            }
          } else {
            // Non-overlapping <b> tag, add the current one to the list and start a new one
            mergedBTagPositions.push(currentBTagPosition);
            currentBTagPosition = bTagPositions[i];
          }
        }
        mergedBTagPositions.push(currentBTagPosition);
      }

      // Reconstitute the paragraph with the <b> tags
      p.innerHTML = '';
      let currentOffset = 0;
      mergedBTagPositions.forEach((position) => {
        const before = document.createTextNode(plainText.slice(currentOffset, position.start));
        const middle = document.createElement('b');
        middle.id = position.id;
        middle.textContent = plainText.slice(position.start, position.end);
        p.appendChild(before);
        p.appendChild(middle);
        currentOffset = position.end;
      });
      const after = document.createTextNode(plainText.slice(currentOffset));
      p.appendChild(after);

      globalOffset += plainText.length;
    }
  });
}
// // Auxiliary function to add highlight in the browser
// $( document ).ready(function() {
//   Shiny.addCustomMessageHandler('highlight', function(message) {
//     var startOff = message.startOff;
//     var endOff = message.endOff;
//     console.log(startOff)
//     console.log(endOff)
//     wrapSelection(startOff, endOff);
//   })
// });

// function wrapSelection(startOffset, endOffset) {
//   let currentOffset = 0;

//   function traverse(node) {
//       if (node.nodeType === Node.TEXT_NODE) {
//           const textLength = node.textContent.length;

//           // If the startOffset is within this text node
//           if (startOffset >= currentOffset && startOffset < currentOffset + textLength) {
//               const start = startOffset - currentOffset;
//               const before = node.textContent.slice(0, start);
//               const after = node.textContent.slice(start);
//               const newNode = document.createElement('b');
//               newNode.textContent = after;
//               node.textContent = before;
//               node.parentNode.insertBefore(newNode, node.nextSibling);
//           }

//           // If the endOffset is within this text node
//           if (endOffset > currentOffset && endOffset <= currentOffset + textLength) {
//               const end = endOffset - currentOffset;
//               const boldNode = node.nextSibling;
//               const before = boldNode.textContent.slice(0, end);
//               const after = boldNode.textContent.slice(end);
//               const newNode = document.createTextNode(after);
//               boldNode.textContent = before;
//               boldNode.parentNode.insertBefore(newNode, boldNode.nextSibling);
//           }

//           currentOffset += textLength;
//       } else {
//           for (let child of node.childNodes) {
//               traverse(child);
//           }
//       }
//   }

//   traverse(document.querySelector('article'));
// }

// <article id="article">
// <p id = "1">Text <b id="1">of</b> paragraph 1</p>
// <p id = "2">Text of paragraph 2</p>
// <p id = "3">Text of paragraph 3</p>
// <p id = "4">Text of paragraph 4</p>
// </article>

// <article id="article">
// <p id = "1"><b id = "2">Text </b><b id="1+2">of</b><b id="2"> paragraph 1</b></p>
// <p id = "2"><b id = "2">Text of paragraph 2</b></p>
// <p id = "3"><b id = "2">Text</b> of paragraph 3</p>
// <p id = "4">Text of paragraph 4</p>
// </article>