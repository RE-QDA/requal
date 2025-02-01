// $(document).ready(function() {
//   Shiny.addCustomMessageHandler('highlightSegments', function(message) {
//     const processSegments = (elements, styleType) => {
//       elements.forEach(segment_coded => {
//         const colors = segment_coded.getAttribute('data-color').split(' | ');
//         const color = averageColor(colors);
//         // console.log('Style Type:', styleType);
//         // console.log('Colors:', colors);
//         // console.log('Computed Color:', color);

//         if (styleType === 'underline') {
//           segment_coded.style.textDecoration = `underline ${color}`;
//           segment_coded.style.backgroundColor = ''; // Clear background if previously set
//         } else {
//           segment_coded.style.backgroundColor = color;
//           segment_coded.style.textDecoration = ''; // Clear underline if previously set
//         }
//       });
//     };

//     const ids = Array.isArray(message.ids) ? message.ids : [message.ids]; // Ensure ids is an array

//     const targetNode = document.getElementById("article");

//     const observer = new MutationObserver((mutationsList, observer) => {
//       ids.forEach(id => {
//         const container = document.getElementById(id);
//         if (container) {
//           const segments = container.querySelectorAll('.segment-coded');
//           if (segments.length > 0) {
//             // console.log(`Segments found in #${id}:`, segments.length);
//             processSegments(segments, message.styleType || 'background');
//             observer.disconnect(); // Stop observing once the elements are processed
//           }
//         } else {
//           console.log(`Container with ID #${id} not found.`);
//         }
//       });
//     });

//     // Start observing the document body for changes
//     observer.observe(targetNode, { childList: true, attributes: true, subtree: true });
//   });
// });

function averageColor(colors) {
  const rgbColors = colors.map(color => {
    const matches = color.match(/\d+/g);
    return matches ? matches.map(Number) : [0, 0, 0];
  });

  const avgColor = rgbColors.reduce((acc, color) => {
    return acc.map((value, index) => value + color[index]);
  }, [0, 0, 0]).map(value => Math.round(value / colors.length));

  return `rgb(${avgColor.join(',')})`;
}


function averageColor(colors) {
  const rgbColors = colors.map(color => {
      const matches = color.match(/\d+/g);
      // console.log(matches);
      return matches ? matches.map(Number) : [0, 0, 0];
  });

  const avgColor = rgbColors.reduce((acc, color) => {
      return acc.map((value, index) => value + color[index]);
  }, [0, 0, 0]).map(value => Math.round(value / colors.length));

  return `rgb(${avgColor.join(',')})`;
}



// Add code
//function (startOffset, endOffset, newId, color, title) {

Shiny.addCustomMessageHandler('AddCode', function(message) {
  const startOffset = message.startOffset;
  const endOffset = message.endOffset;
  const newId = message.newId;
  const color = message.color;
  const title = message.title;
  AddCode(startOffset, endOffset, newId, color, title);
});

function AddCode(startOffset, endOffset, newId, color, title) {
  // Convert R's 1-based indexing to JavaScript's 0-based indexing
  startOffset = startOffset - 1;
  endOffset = endOffset - 1;

  const paragraphs = document.querySelectorAll('.docpar');

  paragraphs.forEach((paragraph) => {
    const [paraStart, paraEnd] = paragraph.getAttribute('data-startend').split(' ').map(num => Number(num) - 1); // Convert to 0-based
    
    if (endOffset >= paraStart && startOffset <= paraEnd) {
      // Step 1: Collect all text and existing styles
      let fullText = '';
      const charStyles = new Array(paraEnd - paraStart + 1).fill().map(() => ({
        colors: new Set(),
        titles: new Set(),
        codes: new Set()
      }));

      // First collect the full text content
      const textNodes = [];
      let currentPos = 0;
      
      paragraph.childNodes.forEach(node => {
        if (node.nodeType === Node.TEXT_NODE || (node.nodeType === Node.ELEMENT_NODE && node.nodeName === 'SPAN' && !node.classList.contains('br'))) {
          const text = node.textContent;
          if (node.nodeType === Node.ELEMENT_NODE) {
            const [spanStart, spanEnd] = node.getAttribute('data-startend').split(' ').map(num => Number(num) - 1);
            const relativeStart = spanStart - paraStart;
            
            // Fill any gaps
            while (currentPos < relativeStart) {
              fullText += ' ';
              currentPos++;
            }
            
            fullText += text;
            currentPos = spanEnd - paraStart;

            // Add existing styles
            if (node.getAttribute('data-color')) {
              const colors = node.getAttribute('data-color').split(' | ');
              const titles = node.title ? node.title.split(' | ') : [];
              const codes = node.getAttribute('data-codes') ? node.getAttribute('data-codes').split(' | ') : [];
              
              for (let i = relativeStart; i < spanEnd - paraStart; i++) {
                colors.forEach(c => charStyles[i].colors.add(c));
                titles.forEach(t => charStyles[i].titles.add(t));
                codes.forEach(c => charStyles[i].codes.add(c));
              }
            }
          } else {
            fullText += text;
            currentPos += text.length;
          }
        }
      });

      // Add new highlight
      const relativeStart = Math.max(0, startOffset - paraStart);
      const relativeEnd = Math.min(charStyles.length, endOffset - paraStart + 1);
      
      for (let i = relativeStart; i < relativeEnd; i++) {
        charStyles[i].colors.add(color);
        charStyles[i].titles.add(title);
        charStyles[i].codes.add(newId);
      }

      // Step 2: Create segments based on style changes
      const segments = [];
      let currentSegment = null;

      for (let i = 0; i < charStyles.length; i++) {
        const styleKey = JSON.stringify({
          colors: Array.from(charStyles[i].colors).sort(),
          titles: Array.from(charStyles[i].titles).sort(),
          codes: Array.from(charStyles[i].codes).sort()
        });

        if (!currentSegment || currentSegment.styleKey !== styleKey) {
          if (currentSegment) {
            segments.push(currentSegment);
          }
          currentSegment = {
            start: i,
            end: i + 1,
            styleKey,
            colors: Array.from(charStyles[i].colors),
            titles: Array.from(charStyles[i].titles),
            codes: Array.from(charStyles[i].codes)
          };
        } else {
          currentSegment.end = i + 1;
        }
      }
      if (currentSegment) {
        segments.push(currentSegment);
      }

      // Step 3: Rebuild the paragraph
      paragraph.innerHTML = '';
      
      segments.forEach(segment => {
        const span = document.createElement('span');
        // Convert back to 1-based indexing for data-startend
        span.setAttribute('data-startend', 
          `${segment.start + paraStart + 1} ${segment.end + paraStart + 1}`
        );
        span.textContent = fullText.slice(segment.start, segment.end);

        if (segment.colors.length > 0) {
          span.className = 'segment code';
          span.setAttribute('data-color', segment.colors.join(' | '));
          // Use the most recently added color (last in sorted array)
          span.style.backgroundColor = segment.colors[segment.colors.length - 1];
          span.title = segment.titles.join(' | ');
          span.setAttribute('data-codes', segment.codes.join(' | '));
          span.setAttribute('onclick', 
            "Shiny.setInputValue('document_code_ui_1-clicked_title', this.title, {priority: 'event'});"
          );
        }

        paragraph.appendChild(span);
      });

      // Restore break span
      const brSpan = document.createElement('span');
      brSpan.className = 'br';
      brSpan.textContent = '​';
      paragraph.appendChild(brSpan);
    }
  });
}


Shiny.addCustomMessageHandler('insertNote', function(message) {
  var target = document.getElementById(message.span_id);
  console.log(target)
  if (target) {
    // Create a temporary div to parse the HTML string
    var tempDiv = document.createElement('span');
    tempDiv.innerHTML = message.memo_html;
    // Append the parsed HTML to the target element
    console.log(tempDiv)

    while (tempDiv.firstChild) {
      target.appendChild(tempDiv.firstChild);
    }
  }
});

document.addEventListener('click', (e) => {
  if (e.target.classList.contains('text_memo')) { // Check if the clicked element is the parent
    const childToRemove = e.target.querySelector('.text_memo_extra'); // Find the specific child
    if (childToRemove) {
      childToRemove.remove(); // Remove the child element
    } else {
      Shiny.setInputValue('document_code_ui_1-text_memo_click', e.target.id + ' ' + Math.random());
    }
  }
});
