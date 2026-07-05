library(rvest)
library(shinytest2)

test_that("{shinytest2} test", {
    skip_on_cran()
    skip_on_ci()

    appdir <- system.file("test_app", package = "requal")
    app <- AppDriver$new(appdir, name = "requaltest", seed = 123, height = 789, width = 1139)

    app$click("launchpad_loader_ui_1-project_load")

    app$wait_for_idle()

    # Open the Memos tab
    app$set_inputs(tab_menu = "Memos")
    app$wait_for_idle()

    # Compose and create a new free memo
    app$set_inputs(`memo_ui_1-memo_main_editor-memo_text_input` = "Memo")
    app$click("memo_ui_1-memo_main_editor-create_new")

    app$wait_for_idle()

    vals <- app$get_values()

    expect_equal(vals$input$`memo_ui_1-memo_rows_all`, 1)
})
