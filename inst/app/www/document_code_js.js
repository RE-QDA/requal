// Function to calculate selection positions
function getCaretCharacterOffsetWithin(element) {
    console.log('Function called with element:', element);

    var caretOffset = 0;
    var doc = element.ownerDocument || element.document;
    var win = doc.defaultView || doc.parentWindow;
    var sel;

    if (typeof win.getSelection != "undefined") {
        sel = win.getSelection();
        if (sel.rangeCount > 0) {
            var range = win.getSelection().getRangeAt(0);
            var preCaretRange = range.cloneRange();
            preCaretRange.selectNodeContents(element);

            // Only include text nodes within a <span class="text"> in the range
            var treeWalker = document.createTreeWalker(element, NodeFilter.SHOW_TEXT, null, false);
            var node, textNodes = [];
            while(node = treeWalker.nextNode()) {
                if(node.parentNode.className === "text") {
                    textNodes.push(node);
                }
            }
            console.log('Text nodes:', textNodes);

            if(textNodes.length > 0) {
                preCaretRange.setStart(textNodes[0], 0);
                preCaretRange.setEnd(textNodes[textNodes.length - 1], textNodes[textNodes.length - 1].length);

                preCaretRange.setEnd(range.endContainer, range.endOffset);
                caretOffset = preCaretRange.toString().length;
            }
        }
    } else if ( (sel = doc.selection) && sel.type != "Control") {
        var textRange = sel.createRange();
        var preCaretTextRange = doc.body.createTextRange();
        preCaretTextRange.moveToElementText(element);
        preCaretTextRange.setEndPoint("EndToEnd", textRange);
        caretOffset = preCaretTextRange.text.length;
    }
    console.log('Caret offset:', caretOffset);
    return caretOffset;
}

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
