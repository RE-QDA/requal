library(shinytest2)

test_that("{shinytest2} test delete category", {
    
    app <- AppDriver$new(name = "delete_category", seed = 123, 
                         height = 789, width = 1139,
                         variant = platform_variant())
    
    app$click("launchpad_loader_ui_1-project_load")
    app$wait_for_idle()
    
    app$click("codebook_ui_1-code_add")
    app$set_inputs(tab_menu = "Codebook")
    # Update output value
    app$set_inputs(`codebook_ui_1-codebook_tabset` = "categories")
    # Update output value
    app$set_inputs(`categories_ui_1-code_list` = "No codes have been created.", allow_no_input_binding_ = TRUE)
    app$click("categories_ui_1-category_create-rql_button_id")
    # app$set_inputs(`document_code_ui_1-tag_position` = "1-0", allow_no_input_binding_ = TRUE)
    app$set_inputs(`categories_ui_1-category_name` = "Kat1")
    app$click("categories_ui_1-category_add")
    app$set_inputs(`categories_ui_1-category_list_1` = character(0), allow_no_input_binding_ = TRUE)
    # Update output value
    app$set_inputs(`categories_ui_1-category_name` = "Kat2")
    app$click("categories_ui_1-category_add")
    app$set_inputs(`categories_ui_1-category_list_2` = character(0), allow_no_input_binding_ = TRUE)
    # Update output value
    app$click("categories_ui_1-category_delete-rql_button_id")
    app$set_inputs(`categories_ui_1-categories_to_del` = "2")
    app$click("categories_ui_1-category_remove")
    
    vals <- app$get_values()
    expect_equal(
        vals$output$`categories_ui_1-categories_ui`$html %>%
            rvest::read_html() %>% rvest::html_nodes("h3") %>% length(),
        1
    )
    
})
