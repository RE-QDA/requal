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
 