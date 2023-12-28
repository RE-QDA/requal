library(rvest)
library(shinytest2)

test_that("{shinytest2} testing create codebook", {
    app <- AppDriver$new(name = "createcode", seed = 123, 
                         height = 789, width = 1139)
    
    app$click("launchpad_loader_ui_1-project_load")
    app$wait_for_idle()
    
    app$set_inputs(tab_menu = "Codebook")
    
    # Codes
    # Creating code
    app$click("code_create_btn")
    app$set_inputs(`codebook_ui_1-code_name` = "Code1")
    app$set_inputs(`codebook_ui_1-code_desc` = "Code description")
    app$click("codebook_ui_1-code_add")
    app$click("code_create_btn")

    app$expect_values(output = "codebook_ui_1-codes_ui")
    
    # Categories
    # Create category
    
    app$set_inputs(`codebook_ui_1-codebook_tabset` = "categories")
    
    app$click("ctg_create_btn")
    app$set_inputs(`categories_ui_1-category_name` = "Category 1")
    app$set_inputs(`categories_ui_1-category_desc` = "Description of a category")
    app$click("categories_ui_1-category_add")
    app$click("ctg_create_btn")
    
    app$expect_values(output = "categories_ui_1-categories_ui")
    
    
})