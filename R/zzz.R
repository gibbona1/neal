.onLoad <- function(libname, pkgname) {
  # shinyBS only registers its "sbs" www resource path in .onAttach(), which
  # never fires when shinyBS is loaded as an Imports dependency (rather than
  # library()'d directly). Register it here so its JS/CSS actually get served.
  shiny::addResourcePath("sbs", system.file("www", package = "shinyBS"))
}
