#' @import shinytest2

test_that("{shinytest2} recording: app1", {
  app <- AppDriver$new(variant = platform_variant(), name = "app1", height = 911, 
      width = 1619)
  app$set_inputs(label_points = "Bar-tailed Godwit")
  app$expect_screenshot()
})
