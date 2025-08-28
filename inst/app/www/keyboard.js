$(document).ready(function() {
    document.getElementById("document_code_ui_1-focal_text").addEventListener('click', function(event) {
        const parentElement = event.target;

        // Remove any existing highlight
        const existingHighlight = parentElement.querySelector('.cursor_highlight');
        if (existingHighlight) {
            const originalText = existingHighlight.textContent;
            const beforeText = existingHighlight.previousSibling ? existingHighlight.previousSibling.textContent : '';
            const afterText = existingHighlight.nextSibling ? existingHighlight.nextSibling.textContent : '';

            // Restore the original text
            parentElement.textContent = beforeText + originalText + afterText;
        }

        const textNode = parentElement.firstChild; // Assuming textNode is the firstChild
        const range = document.createRange();
        const selection = window.getSelection();

        // Clear any existing selections
        selection.removeAllRanges();

        // Iterate through each character to find the clicked position
        let clickedIndex = -1;
        for (let i = 0; i < textNode.length; i++) {
            range.setStart(textNode, i);
            range.setEnd(textNode, i + 1);
            const rect = range.getBoundingClientRect();

            if (event.clientX >= rect.left && event.clientX <= rect.right) {
                clickedIndex = i;
                break;
            }
        }

        if (clickedIndex !== -1) {
            // Wrap the clicked character in a span
            const char = textNode.nodeValue[clickedIndex];
            const before = textNode.nodeValue.slice(0, clickedIndex);
            const after = textNode.nodeValue.slice(clickedIndex + 1);

            const span = document.createElement('span');
            span.className = 'cursor_highlight';
            span.textContent = char;

            const newTextBefore = document.createTextNode(before);
            const newTextAfter = document.createTextNode(after);

            parentElement.textContent = ''; // Clear the existing text
            parentElement.appendChild(newTextBefore);
            parentElement.appendChild(span);
            parentElement.appendChild(newTextAfter);
        }
    });
});