library(rvest)
library(shinytest2)

test_that("{shinytest2} test", {
    app <- AppDriver$new(name = "requaltest", seed = 123, height = 789, width = 1139)

    app$click("launchpad_loader_ui_1-project_load")

    app$wait_for_idle()

    # Open the Memos tab
    app$set_inputs(tab_menu = "Memos")
    app$wait_for_idle()

    # Create two memos
    app$set_inputs(`memo_ui_1-memo_main_editor-memo_text_input` = "Memo one")
    app$click("memo_ui_1-memo_main_editor-create_new")
    app$wait_for_idle()

    app$set_inputs(`memo_ui_1-memo_main_editor-memo_text_input` = "Memo two")
    app$click("memo_ui_1-memo_main_editor-create_new")
    app$wait_for_idle()

    expect_length(app$get_values()$input$`memo_ui_1-memo_rows_all`, 2)

    # Open the first memo and delete it
    app$run_js("document.querySelector('#memo_ui_1-memo .memo_name').click()")
    app$wait_for_idle()

    app$click("memo_ui_1-memo_main_editor-delete_memo")
    app$wait_for_idle()

    vals <- app$get_values()

    expect_equal(vals$input$`memo_ui_1-memo_rows_all`, 1)
})
