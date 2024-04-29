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
  const color = message.color;
  const title = message.title;
  wrapTextWithBold(startOffset, endOffset, newId, color, title);
});


function wrapTextWithBold(startOffset, endOffset, newId, color, title) {
  const container = document.querySelector('#article');
  if (!container) {
    console.error("Container not found");
    return;
  }

  let globalOffset = 0;

  Array.from(container.childNodes).forEach((p) => {
    if (p.nodeType === Node.ELEMENT_NODE && p.tagName.toLowerCase() === 'p') {
      let plainText = '';
      let bTagPositions = [];
      Array.from(p.childNodes).forEach(child => {
        if (child.nodeType === Node.ELEMENT_NODE && child.tagName.toLowerCase() === 'b') {
          bTagPositions.push({pos: plainText.length, type: 'start', id: child.id, color: child.getAttribute('data-color'), title: child.title});
          plainText += child.textContent;
          bTagPositions.push({pos: plainText.length, type: 'end', id: child.id});
        } else if (child.nodeType === Node.TEXT_NODE) {
          plainText += child.textContent;
        }
      });

      if (globalOffset <= endOffset && globalOffset + plainText.length >= startOffset) {
        const paragraphStart = Math.max(startOffset - globalOffset, 0);
        const paragraphEnd = Math.min(endOffset - globalOffset, plainText.length);
        bTagPositions.push({pos: paragraphStart, type: 'start', id: newId, color: color, title: title});
        bTagPositions.push({pos: paragraphEnd, type: 'end', id: newId});
      }

      bTagPositions.sort((a, b) => a.pos - b.pos || (a.type === 'end' ? -1 : 1));

      let finalBTagPositions = [];
      let openTags = [];
      let openColors = [];
      let openTitles = [];
      for (let i = 0; i < bTagPositions.length - 1; i++) {
        if (bTagPositions[i].type === 'start') {
          openTags.push(bTagPositions[i].id);
          openColors.push(bTagPositions[i].color);
          openTitles.push(bTagPositions[i].title);
        } else {
          const index = openTags.indexOf(bTagPositions[i].id);
          openTags.splice(index, 1);
          openColors.splice(index, 1);
          openTitles.splice(index, 1);
        }
        if (bTagPositions[i].pos !== bTagPositions[i+1].pos && openTags.length > 0) {
          let avgColor = "rgb(0,0,0)";
          if (openColors.length > 0) {
            let r = 0, g = 0, b = 0;
            openColors.forEach(color => {
              let match = color.match(/\d+/g);
              r += parseInt(match[0]);
              g += parseInt(match[1]);
              b += parseInt(match[2]);
            });
            r = Math.round(r / openColors.length);
            g = Math.round(g / openColors.length);
            b = Math.round(b / openColors.length);
            avgColor = `rgb(${r},${g},${b})`;
          }
          finalBTagPositions.push({start: bTagPositions[i].pos, end: bTagPositions[i+1].pos, id: openTags.join('+'), color: avgColor, title: openTitles.join(' | ')});
        }
      }

      p.innerHTML = '';
      let currentOffset = 0;
      finalBTagPositions.forEach((position) => {
        if (position.start !== position.end) {
          const before = document.createTextNode(plainText.slice(currentOffset, position.start));
          const middle = document.createElement('b');
          middle.id = position.id;
          middle.title = position.title;
          middle.setAttribute('data-color', position.color);
          middle.classList.add('segment'); 
          middle.textContent = plainText.slice(position.start, position.end);
          middle.setAttribute('onclick', "Shiny.setInputValue('document_code_ui_1-clicked_title', this.title, {priority: 'event'});");
          p.appendChild(before);
          p.appendChild(middle);
          currentOffset = position.end;
        }
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