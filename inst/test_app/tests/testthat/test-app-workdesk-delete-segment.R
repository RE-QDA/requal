library(shinytest2)

test_that("{shinytest2} test deleting segment", {
    
    app <- AppDriver$new(name = "workdesk2", seed = 123, 
                         height = 789, width = 1139, 
                         variant = platform_variant())
    app$click("launchpad_loader_ui_1-project_load")
    app$wait_for_idle()
    
    app$set_inputs(tab_menu = "Data")
    
    app$click("data_1-doc_manager_ui_1-doc_create_ui-rql_button_id")
    app$set_inputs(`data_1-doc_manager_ui_1-doc_name` = "dok1")
    app$set_inputs(`data_1-doc_manager_ui_1-doc_text` = "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Integer tempor. Praesent in mauris eu tortor porttitor accumsan. Aenean fermentum risus id tortor. Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Aenean vel massa quis mauris vehicula lacinia. Aliquam erat volutpat. Nullam justo enim, consectetuer nec, ullamcorper ac, vestibulum in, elit. Integer imperdiet lectus quis justo. Sed elit dui, pellentesque a, faucibus vel, interdum nec, diam. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos hymenaeos. Aenean id metus id velit ullamcorper pulvinar. Suspendisse nisl. Et harum quidem rerum facilis est et expedita distinctio. In dapibus augue non sapien. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Duis condimentum augue id magna semper rutrum.")
    app$click("data_1-doc_manager_ui_1-doc_add")
    app$click("data_1-doc_manager_ui_1-doc_create_ui-rql_button_id")
    
    app$set_inputs(tab_menu = "Codebook")
    
    app$click("codebook_ui_1-code_create_ui-rql_button_id")
    app$set_inputs(`codebook_ui_1-code_name` = "Class")
    app$click("codebook_ui_1-code_add")
    app$click("codebook_ui_1-code_create_ui-rql_button_id")
    
    app$set_inputs(tab_menu = "Annotate")
    
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
    
    app$expect_values(output = "document_code_ui_1-focal_text")
    
    # Select segment to delete
    app$run_js(
        '
    var el = document.getElementById("document_code_ui_1-focal_text");
    var textNode = el.childNodes[0].childNodes[0].childNodes[1].childNodes[0];
    
    var range = document.createRange();
    range.setStart(textNode, 2);
    range.setEnd(textNode, 3);
    
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
    
    app$click("document_code_ui_1-remove_codes")
    
    app$expect_values(output = "document_code_ui_1-focal_text")
    
})