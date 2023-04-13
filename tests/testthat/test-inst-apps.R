# File: tests/testthat/test-inst-apps.R
library(shinytest2)

test_that("sample_app works", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()
  
  appdir <- system.file(package = "neal", "app")
  test_app(appdir)
})