library(rvest)
library(shinytest2)

test_that("{shinytest2} test", {
    app <- AppDriver$new(name = "requaltest", seed = 123, height = 789, width = 1139)
    
    app$click("launchpad_loader_ui_1-project_load")
    
    app$wait_for_idle()
    
    app$click(selector = ".fa-sticky-note")
    
    app$wait_for_idle()
    
    app$click("memo_ui_1-new_memo")
    app$set_inputs(`memo_ui_1-memo_text` = "Memo")
    app$click("memo_ui_1-save_close")
    
    app$click(selector = ".fa-sticky-note")
    
    app$wait_for_idle()
    
    vals <- app$get_values()
    
    expect_equal(vals$input$`memo_ui_1-memo_rows_all`, 1)
})
