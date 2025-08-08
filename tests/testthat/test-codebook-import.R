test_that("Test parsing color to RGB works", {
    expect_equal(convert_to_rgb("#FF0000"), "rgb(255, 0, 0)")
    expect_equal(convert_to_rgb("#F00"), "rgb(255, 0, 0)")
    expect_equal(convert_to_rgb("red"), "rgb(255, 0, 0)")
    expect_equal(convert_to_rgb("gred"), NA_character_)
})

color_df <- data.frame(
  color = c("#FF0000", "#F00", "red", "gred")
)

output_color_df <- data.frame(
  color = c("rgb(255, 0, 0)", "rgb(255, 0, 0)", "rgb(255, 0, 0)", "rgb(255, 255, 0)")
)

test_that("Test parsing color to RGB works with data frame", {
    expect_equal(
      process_color_column(color_df, "color"), 
      output_color_df  
    )
})