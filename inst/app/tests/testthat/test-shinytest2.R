library(shinytest2)

test_that("{shinytest2} recording: app-test1", {
  app <- AppDriver$new(name = "neal-start", height = 569, width = 979)
  app$set_inputs(`side-panel` = FALSE)
  app$wait_for_value(input = "start_labelling", timeout = 5000)
  app$click("start_labelling")
  app$wait_for_value(input = "end_labelling", timeout = 5000)
  app$click("end_labelling")
  app$expect_values()
})
