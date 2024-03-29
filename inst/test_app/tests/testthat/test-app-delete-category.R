library(shinytest2)

test_that("{shinytest2} test delete category", {
    
    app <- AppDriver$new(name = "delete_category", seed = 123, 
                         height = 789, width = 1139,
                         variant = platform_variant())
    
    app$click("launchpad_loader_ui_1-project_load")
    app$wait_for_idle()
    
    app$set_inputs(tab_menu = "Codebook")
    app$set_inputs(`codebook_ui_1-codebook_tabset` = "categories")
    
    app$click("categories_ui_1-category_create-rql_button_id")
    app$set_inputs(`categories_ui_1-category_name` = "Kat1")
    app$click("categories_ui_1-category_add")
    
    app$set_inputs(`categories_ui_1-category_name` = "Kat2")
    app$click("categories_ui_1-category_add")
    
    app$click("categories_ui_1-category_delete-rql_button_id")
    app$set_inputs(`categories_ui_1-categories_to_del` = "2")
    app$click("categories_ui_1-category_remove")
    
    app$expect_values(output = "categories_ui_1-categories_ui")
    
})




# Update output value