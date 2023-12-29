library(rvest)
library(shinytest2)

test_that("{shinytest2} test", {
    app <- AppDriver$new(name = "requaltest", seed = 123, height = 789, width = 1139)
    
    app$click("launchpad_loader_ui_1-project_load")
    
    app$wait_for_idle()
    
    app$set_inputs(tab_menu = "Data")
    
    app$click("data_1-doc_manager_ui_1-doc_create_ui-rql_button_id")
    app$set_inputs(`data_1-doc_manager_ui_1-doc_name` = "dok1")
    app$set_inputs(`data_1-doc_manager_ui_1-doc_text` = "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Integer tempor. Praesent in mauris eu tortor porttitor accumsan. Aenean fermentum risus id tortor. Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Aenean vel massa quis mauris vehicula lacinia. Aliquam erat volutpat. Nullam justo enim, consectetuer nec, ullamcorper ac, vestibulum in, elit. Integer imperdiet lectus quis justo. Sed elit dui, pellentesque a, faucibus vel, interdum nec, diam. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos hymenaeos. Aenean id metus id velit ullamcorper pulvinar. Suspendisse nisl. Et harum quidem rerum facilis est et expedita distinctio. In dapibus augue non sapien. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Duis condimentum augue id magna semper rutrum.")
    app$click("data_1-doc_manager_ui_1-doc_add")
    app$click("data_1-doc_manager_ui_1-doc_create_ui-rql_button_id")
    
    vals <- app$get_values()
    docs_length <- read_html(vals$output$`data_1-doc_manager_ui_1-doc_list_table`) %>% 
        html_nodes("tr") %>% 
        length()
    
    docs_name <- read_html(vals$output$`data_1-doc_manager_ui_1-doc_list_table`) %>% 
        html_nodes("tr") %>% 
        html_nodes("td") %>% 
        `[`(., 1) %>% 
        html_text() %>% 
        trimws()
    
    expect_equal(docs_length, 1)
    expect_equal(docs_name, "dok1")
    # The tables includes time of document creation so we cannot use snapshots
    # app$expect_values()
})