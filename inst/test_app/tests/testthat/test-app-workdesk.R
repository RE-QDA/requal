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
        `doc_manager_ui_1-doc_text` = "The history of all hitherto existing society(2) 
        is the history of class struggles.
        Freeman and slave, patrician and plebeian, lord and serf, guild-master(3) 
        and journeyman, in a word, oppressor and oppressed, stood in constant 
        opposition to one another, carried on an uninterrupted, now hidden, 
        now open fight, a fight that each time ended, either in a revolutionary 
        reconstitution of society at large, or in the common ruin of the 
        contending classes.")
    app$click("doc_manager_ui_1-doc_add")
    
    app$set_inputs(tab_menu = "Codebook")
    
    app$click("code_create_btn")
    app$set_inputs(`codebook_ui_1-code_name` = "Class")
    app$click("codebook_ui_1-code_add")
    
    app$set_inputs(tab_menu = "Document")
    app$set_inputs(`document_code_ui_1-doc_selector` = "1")
    app$wait_for_idle()
    
    # TODO:
    # Update output value
    # app$click("document_code_ui_1-1")
    # # Update output value
    # app$click("document_code_ui_1-1")
    
    app$expect_screenshot()
    
})