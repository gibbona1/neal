library(shinytest2)

test_that("{shinytest2} recording: neal-start", {
  app <- AppDriver$new(name = "neal-start", height = 569, width = 979)
  app$set_inputs(`side-panel` = FALSE)
  app$wait_for_value(input = "start_labelling", timeout = 5000)
  app$click("start_labelling")
  app$wait_for_value(input = "end_labelling", timeout = 5000)
  app$click("end_labelling")
  app$expect_values()
})


test_that("{shinytest2} recording: neal-labelling", {
  app <- AppDriver$new(name = "neal-labelling", height = 569, width = 979)
  app$set_inputs(`side-panel` = FALSE)
  app$wait_for_value(input = "start_labelling", timeout = 2000)
  app$click("start_labelling")
  app$click("save_points")
  app$click("remove_points")
  app$wait_for_value(input = "end_labelling", timeout = 2000)
  app$click("end_labelling")
  app$expect_values()
  app$expect_values()
})


test_that("{shinytest2} recording: neal-categories", {
  app <- AppDriver$new(name = "neal-categories", height = 569, width = 979)
  app$wait_for_value(input = "start_labelling", timeout = 2000)
  app$click("start_labelling")
  app$set_inputs(otherCategory = "abc")
  app$click("addCategory")
  app$click("remCategory")
  app$set_inputs(`side-panel` = TRUE)
  app$set_inputs(sidebarItemExpanded = "Configuration")
  app$set_inputs(species_list = "Carnsore")
  app$set_inputs(bto_codes = TRUE)
  app$set_inputs(sidebarItemExpanded = character(0))
  app$wait_for_value(input = "end_labelling", timeout = 2000)
  app$click("end_labelling")
  app$expect_values()
})
