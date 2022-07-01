app <- ShinyDriver$new("../../")
app$snapshotInit("select-project")

app$setInputs(`launchpad_loader_ui_1-project_selector_load` = "1")
app$setInputs(`launchpad_loader_ui_1-project_load` = "click")
app$snapshot()
