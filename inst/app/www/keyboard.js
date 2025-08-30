$(document).ready(function() {
  // Use the Shiny-namespaced ID of your editable div
  const editableDiv = document.getElementById("document_code_ui_1-focal_text");

  // 1. PREVENT THE USER FROM TYPING OR DELETING TEXT
  editableDiv.addEventListener('beforeinput', function(event) {
    // A list of input types we want to allow (if any)
    const allowedInputs = [/* e.g., 'insertLineBreak' if you want to allow Enter */];
    
    if (!allowedInputs.includes(event.inputType)) {
      event.preventDefault(); // Block the change
    }
  });

  // 2. LISTEN FOR A KEYBOARD SHORTCUT TO APPLY A CODE
  editableDiv.addEventListener('keydown', function(event) {
    // Let's use Ctrl+M as the shortcut to "Mark" or "Code" a selection
    if (event.ctrlKey && event.key === 'm') {
      event.preventDefault(); // Stop the browser's default action for this shortcut

      const selection = window.getSelection();
      if (selection.rangeCount > 0 && !selection.isCollapsed) {
        
        // 3. WRAP THE HIGHLIGHTED TEXT IN A SPAN
        const range = selection.getRangeAt(0);
        const highlightSpan = document.createElement('span');
        highlightSpan.className = 'coded-text'; // Use CSS to style this class
        
        // This is the most efficient way to wrap a selection
        // without destroying the surrounding text.
        range.surroundContents(highlightSpan);

        // Clear the selection after coding
        selection.removeAllRanges();

        // OPTIONAL: Send the newly coded text back to Shiny
        const selectedText = highlightSpan.textContent;
        Shiny.setInputValue("coded_selection", { text: selectedText });
      }
    }
  });
});



//   contenteditable = "true",
//   spellcheck = "false",
  
//     /* This removes the blue ring that appears on click */
//     [contenteditable='true']:focus {
//       outline: none;
//     }
 

$(document).ready(function() {
  const editableDiv = document.getElementById("document_code_ui_1-focal_text");
  const customCursor = document.getElementById("custom-cursor");

  // This function reads the real cursor's position and moves our fake one.
  const updateCursorPosition = () => {
    const selection = window.getSelection();
    if (selection.rangeCount > 0) {
      const range = selection.getRangeAt(0);
      const isCollapsed = range.collapsed; // True when it's a cursor, false for a selection

      if (isCollapsed) {
        // We have a blinking cursor, so let's position our custom one.
        const startNode = range.startContainer;
        
        // Only show the cursor if it's inside a text node
        if (startNode.nodeType === Node.TEXT_NODE) {
            // Create a new temporary range that covers just one character
            // to get its exact dimensions and position.
            const tempRange = document.createRange();
            
            // If the cursor is at the very end, get the position of the last character
            const offset = Math.min(range.startOffset, startNode.textContent.length - 1);
            tempRange.setStart(startNode, offset);
            tempRange.setEnd(startNode, offset + 1);

            const rect = tempRange.getBoundingClientRect();
            const wrapperRect = editableDiv.getBoundingClientRect();

            // Position the custom cursor relative to the wrapper
            customCursor.style.left = (rect.left - wrapperRect.left) + 'px';
            customCursor.style.top = (rect.top - wrapperRect.top) + 'px';
            customCursor.style.width = rect.width + 'px';
            customCursor.style.height = rect.height + 'px';
            customCursor.style.display = 'block';

        } else {
             customCursor.style.display = 'none';
        }

      } else {
        // The user is making a selection, not a single cursor, so hide our custom cursor.
        customCursor.style.display = 'none';
      }
    }
  };

  // Run the update function whenever the user clicks or presses a key.
  editableDiv.addEventListener('click', updateCursorPosition);
  editableDiv.addEventListener('keydown', () => {
    // We use a small delay because the browser needs a moment
    // to move the real cursor after a keydown event.
    setTimeout(updateCursorPosition, 1);
  });
  
  // Also update when the selection changes for any other reason
  document.addEventListener('selectionchange', updateCursorPosition);
});