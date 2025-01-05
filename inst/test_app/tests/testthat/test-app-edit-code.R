library(shinytest2)

test_that("{shinytest2} testing editing code", {
  app <- AppDriver$new(name = "edit_code", seed = 123, 
                       height = 789, width = 1139)
  
  app$click("launchpad_loader_ui_1-project_load")
  app$wait_for_idle()
  
  app$set_inputs(tab_menu = "Codebook")
  
  app$click("codebook_ui_1-code_create_ui-rql_button_id")
  app$set_inputs(`codebook_ui_1-code_name` = "Code1")
  app$set_inputs(`codebook_ui_1-code_desc` = "Code description")
  app$click("codebook_ui_1-code_add")
  app$click("codebook_ui_1-code_create_ui-rql_button_id")
  
  # Codes
  app$expect_values(output = "codebook_ui_1-codes_ui")
  
  app$click("codebook_ui_1-code_edit_ui-rql_button_id")
  app$set_inputs(`codebook_ui_1-code_to_edit` = "1")
  
  app$set_inputs(`codebook_ui_1-edit_code_name` = "kód")
  app$set_inputs(`codebook_ui_1-edit_code_desc` = "popis kódu")
  
  app$click("codebook_ui_1-code_edit_btn")
  
  app$expect_values(output = "codebook_ui_1-codes_ui")
  
})
