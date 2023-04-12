run_NEAL_app <- function() {
  if (interactive()) {
    runApp(appDir = system.file("app", package = "neal"))
  } else {
    shinyAppDir(appDir = system.file("app", package = "neal"))
  }
}