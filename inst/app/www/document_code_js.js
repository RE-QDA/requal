$( document ).ready(function() {

document.addEventListener('mouseup', function () {
    if (typeof window.getSelection != 'undefined') {
        var sel = window.getSelection();
        if(window.getSelection().baseNode.parentNode.id != "document_code_ui_1-focal_text") return;
        var range = sel.getRangeAt(0);

        var startOffset = range.startOffset;
        var endOffset = startOffset + range.toString().length;
      
        var tag_position_value = startOffset.toString() + '-' + endOffset.toString();
        
        Shiny.setInputValue('document_code_ui_1-tag_position', tag_position_value);

    }
}, false);

});

