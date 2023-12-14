library(shinytest2)

test_that("{shinytest2} test merge codes", {
  app <- AppDriver$new(name = "merging-codes", seed = 123, 
                       height = 857, width = 1211)
  app$click("launchpad_loader_ui_1-project_load")
  app$wait_for_idle()
  
  app$set_inputs(tab_menu = "Codebook")
  
  app$click("code_create_btn")
  app$set_inputs(`codebook_ui_1-code_name` = "Code 1")
  app$set_inputs(`codebook_ui_1-code_desc` = "1")
  app$click("codebook_ui_1-code_add")
  
  app$set_inputs(`codebook_ui_1-code_name` = "Code 2")
  app$set_inputs(`codebook_ui_1-code_desc` = "2")
  app$click("codebook_ui_1-code_add")
  app$click("code_create_btn")
  
  app$click("code_merge_btn")
  app$set_inputs(`codebook_ui_1-merge_from` = "2")
  app$set_inputs(`codebook_ui_1-merge_to` = "1")
  app$click("codebook_ui_1-code_merge")
  
  app$expect_values(output = "codebook_ui_1-codes_ui")
})
