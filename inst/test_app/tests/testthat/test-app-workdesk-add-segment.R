library(shinytest2)

test_that("{shinytest2} test creating segment", {
    
    app <- AppDriver$new(name = "workdesk2", seed = 123, 
                         height = 789, width = 1139, 
                         variant = platform_variant())
    app$click("launchpad_loader_ui_1-project_load")
    app$wait_for_idle()
    
    app$click("doc_create_btn")
    app$set_inputs(`doc_manager_ui_1-doc_name` = "Dok1")
    app$set_inputs(
        `doc_manager_ui_1-doc_text` = "Lorem ipsum dolor sit amet")
    app$click("doc_manager_ui_1-doc_add")
    
    app$set_inputs(tab_menu = "Codebook")
    
    app$click("code_create_btn")
    app$set_inputs(`codebook_ui_1-code_name` = "Class")
    app$click("codebook_ui_1-code_add")
    
    app$set_inputs(tab_menu = "Document")
    app$set_inputs(`document_code_ui_1-doc_selector` = "1")
    app$wait_for_idle()
    
    app$run_js(
        '
    var el = document.getElementById("document_code_ui_1-focal_text");
    var textNode = el.childNodes[0].childNodes[0].childNodes[0];
    
    var range = document.createRange();
    range.setStart(textNode, 6);
    range.setEnd(textNode, 11);
    
    var selection = window.getSelection();
    selection.removeAllRanges();
    selection.addRange(range);
    '
    )
    
    # Javascript from document_code_js.js
    # IRL triggered by mouse event
    app$run_js('
    var sel = window.getSelection();
    
    if(sel.rangeCount > 0){
      var range = sel.getRangeAt(0);
      var el = document.getElementById("document_code_ui_1-focal_text")
    
      var endOffset = getCaretCharacterOffsetWithin(el);
      var startOffset_js = endOffset - range.toString().length;
      var startOffset = startOffset_js+1;
      var text_length = $("#document_code_ui_1-focal_text").text().length;

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
      
      var tag_position_value = startOffset.toString() + "-" + endOffset.toString();
        
      Shiny.setInputValue("document_code_ui_1-tag_position", tag_position_value);
    }
    ')
    
    app$click("document_code_ui_1-1")
    
    # app$expect_screenshot()
    app$expect_values(output = "document_code_ui_1-focal_text")
    
})