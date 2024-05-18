library(rvest)
library(shinytest2)

test_that("{shinytest2} test", {
    app <- AppDriver$new(name = "requaltest", seed = 123, height = 789, width = 1139)
    
    app$click("launchpad_loader_ui_1-project_load")
    
    app$wait_for_idle()
    
    app$click(selector = ".fa-note-sticky")
    
    app$wait_for_idle()
    
    app$click("memo_ui_1-new_memo")
    app$set_inputs(`memo_ui_1-memo_text` = "Memo")
    app$click("memo_ui_1-save_close")
    
    app$click(selector = ".fa-note-sticky")
    
    app$wait_for_idle()
    
    app$click(selector = ".memo_name")
    app$wait_for_idle()
    
    app$click("memo_ui_1-delete_memo")
    
    app$click("memo_ui_1-delete_memo_confirmation")
    
    app$click(selector = ".fa-note-sticky")
    app$wait_for_idle()
    
    vals <- app$get_values()
    
    expect_null(vals$input$`memo_ui_1-memo_rows_all`)
})
