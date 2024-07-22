library(shinytest2)

test_that("{shinytest2} recording: test1", {
  skip_if(is_checking() || !interactive())
  app <- AppDriver$new(variant = platform_variant(), name = "test1", height = 911, 
      width = 1619)
  app$set_inputs(label_points = "Bar-tailed Godwit")
  app$expect_screenshot()
})
