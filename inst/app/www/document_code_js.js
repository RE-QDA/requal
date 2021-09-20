function getCaretCharacterOffsetWithin(element) {
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

$( document ).ready(function() {

document.addEventListener('mouseup', function () {
    var sel = window.getSelection();
    // if(window.getSelection().baseNode.parentNode.id != "document_code_ui_1-focal_text") return;
    
    if(sel.rangeCount > 0){
      var range = sel.getRangeAt(0);
      var el = document.getElementById("document_code_ui_1-focal_text")
    
      var endOffset = getCaretCharacterOffsetWithin(el);
      var startOffset = endOffset - range.toString().length;
    
      var tag_position_value = startOffset.toString() + '-' + endOffset.toString();
        
      Shiny.setInputValue('document_code_ui_1-tag_position', tag_position_value);
    }

}, false);
})



$( document ).ready(function() {
  Shiny.addCustomMessageHandler('highlight', function(arg_color) {
  
        var selection = window.getSelection().getRangeAt(0);
        if(window.getSelection().baseNode.parentNode.id != "document_code_ui_1-focal_text") return;
        var selectedText = selection.extractContents();
        var span = document.createElement("span");
        span.style.background = arg_color;
        span.appendChild(selectedText);
        selection.insertNode(span);  
  })
});

