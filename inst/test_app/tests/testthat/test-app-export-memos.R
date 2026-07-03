library(rvest)
library(shinytest2)

test_that("{shinytest2} test", {
    app <- AppDriver$new(name = "requaltest", seed = 123, height = 789, width = 1139)

    app$click("launchpad_loader_ui_1-project_load")
    app$wait_for_idle()

    # Open the Memos tab
    app$set_inputs(tab_menu = "Memos")
    app$wait_for_idle()

    # Create a memo so the table (and its export button) is rendered
    app$set_inputs(`memo_ui_1-memo_main_editor-memo_text_input` = "Memo")
    app$click("memo_ui_1-memo_main_editor-create_new")
    app$wait_for_idle()

    # Memo export is a client-side DataTables CSV button; confirm it is available
    csv_button_count <- app$get_js(
        "document.querySelectorAll('#memo_ui_1-memo .buttons-csv, #memo_ui_1-memo .dt-button').length"
    )
    expect_gt(csv_button_count, 0)
})
