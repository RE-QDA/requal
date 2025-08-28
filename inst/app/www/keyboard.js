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

$(document).ready(function() {
    let currentNode = document.getElementById("document_code_ui_1-focal_text").firstChild;
    let currentIndex = 0;

    const updateHighlight = (node, index) => {
        if (!node || index < 0 || index >= node.length) return;

        const char = node.nodeValue[index];
        const before = node.nodeValue.slice(0, index);
        const after = node.nodeValue.slice(index + 1);

        const span = document.createElement('span');
        span.className = 'cursor_highlight';
        span.textContent = char;

        const newTextBefore = document.createTextNode(before);
        const newTextAfter = document.createTextNode(after);

        const parent = node.parentNode;
        parent.textContent = ''; // Clear the existing text
        parent.appendChild(newTextBefore);
        parent.appendChild(span);
        parent.appendChild(newTextAfter);
    };

    const moveCursor = (direction) => {
        if (!currentNode) return;

        const range = document.createRange();
        range.setStart(currentNode, currentIndex);
        range.setEnd(currentNode, currentIndex + 1);
        const rect = range.getBoundingClientRect();

        let newNode = currentNode;
        let newIndex = currentIndex;

        if (direction === 'ArrowLeft' && currentIndex > 0) {
            newIndex--;
        } else if (direction === 'ArrowRight' && currentIndex < currentNode.length - 1) {
            newIndex++;
        } else if (direction === 'ArrowUp' || direction === 'ArrowDown') {
            const yOffset = direction === 'ArrowUp' ? -rect.height : rect.height;
            const targetY = rect.top + yOffset;

            // Find the closest character in the new line
            let closestDistance = Infinity;
            let closestNode = null;
            let closestIndex = 0;

            const checkNode = (node) => {
                for (let i = 0; i < node.length; i++) {
                    range.setStart(node, i);
                    range.setEnd(node, i + 1);
                    const charRect = range.getBoundingClientRect();
                    const distance = Math.abs(charRect.top - targetY);

                    if (distance < closestDistance) {
                        closestDistance = distance;
                        closestNode = node;
                        closestIndex = i;
                    }
                }
            };

            // Traverse the DOM to find the closest node and index
            const traverseNodes = (node) => {
                if (node.nodeType === Node.TEXT_NODE) {
                    checkNode(node);
                } else if (node.nodeType === Node.ELEMENT_NODE) {
                    node.childNodes.forEach(traverseNodes);
                }
            };

            traverseNodes(document.getElementById("document_code_ui_1-focal_text"));
            newNode = closestNode;
            newIndex = closestIndex;
        }

        if (newNode && newIndex !== currentIndex) {
            currentNode = newNode;
            currentIndex = newIndex;
            updateHighlight(currentNode, currentIndex);
        }
    };

    document.addEventListener('keydown', function(event) {
        const validKeys = ['ArrowLeft', 'ArrowRight', 'ArrowUp', 'ArrowDown'];
        if (validKeys.includes(event.key)) {
            moveCursor(event.key);
        }
    });
});