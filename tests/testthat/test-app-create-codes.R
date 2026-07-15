library(rvest)
library(shinytest2)

test_that("{shinytest2} testing create codebook", {
  skip_on_cran()
  skip_on_ci()

  appdir <- system.file("test_app", package = "requal")
  app <- AppDriver$new(
    appdir,
    name = "createcode",
    seed = 123,
    height = 789,
    width = 1139
  )

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

  # Categories
  # Create category

  app$set_inputs(`codebook_ui_1-codebook_tabset` = "categories")

  app$click("categories_ui_1-category_create-rql_button_id")
  app$set_inputs(`categories_ui_1-category_name` = "Category 1")
  app$set_inputs(`categories_ui_1-category_desc` = "Description of a category")
  app$click("categories_ui_1-category_add")
  app$click("categories_ui_1-category_create-rql_button_id")

  app$expect_values(output = "categories_ui_1-categories_ui")

  # Open the export dropdown so the download link becomes active.
  # Shiny keeps download links inside hidden containers disabled (no href)
  # until the container is shown.
  app$set_inputs(`codebook_ui_1-codebook_tabset` = "codebook_tabset")
  app$click("codebook_ui_1-code_export_ui-rql_button_id")
  app$wait_for_idle()

  # The download filename embeds a timestamp, which is not stable across runs,
  # so assert on the filename pattern rather than snapshotting the exact name.
  download_path <- app$get_download(
    output = "codebook_ui_1-export_codebook"
  )
  expect_match(
    basename(download_path),
    "^requal_codebook-\\d{4}-\\d{2}-\\d{2}-\\d{6}\\.csv$"
  )
})
