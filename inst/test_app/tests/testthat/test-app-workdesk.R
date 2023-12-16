library(shinytest2)

test_that("{shinytest2} test delete code", {
    
    app <- AppDriver$new(name = "workdesk", seed = 123, 
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
    
    app$expect_screenshot()
    
})