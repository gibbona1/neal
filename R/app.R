library(ggplot2)
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyFiles)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyThings)
library(keys)
library(imola)
library(tuneR)
library(seewave) # for spectrogram
#library(plotly)
#library(oce)
library(viridis)
#library(grid)
#library(gridExtra)
#library(cowplot) # to get legend
#library(reactlog) # view all connections for reactive objects
library(profvis) # for checking code performance
library(dplyr)
library(stringr)
library(janitor)
library(DT)
library(here)
library(data.table)

#change max supported audio file size to 30MB
options(shiny.maxRequestSize = 30 * 1024 ^ 2)

bto_df <- read.csv(here("inst", "data", "bto_codes.csv"), fileEncoding = "UTF-8-BOM")

ldir   <- "labels" #label directory

empty_lab_df <- read.csv(here("inst", ldir, "labels_tmp.csv"))[FALSE, ]

#Some taken from https://www.audubon.org/news/a-beginners-guide-common-bird-sounds-and-what-they-mean
call_types <- c("song", "call", "subsong", "alarm call", "begging call", "contact call", "flight call",
                "flock", "juvenile call", "mimicry", "nocturnal call", "whisper song")
misc_categories <- c("Human", "Bird - Cannot Identify", "Anthropogenic Noise", "Weather Noise",
                     "Insect Noise", "Other Noise")
playback_vals <- c(0.1, 0.25, 0.5, 1, 2, 5, 10)
names(playback_vals) <- paste0(playback_vals, "x")

hotkeys <- c(
  "shift+space",
  "shift+enter",
  "shift+backspace",
  "shift+left",
  "shift+right",
  "shift+n", "shift+w",
  paste(1:9)
)

.is_null <- function(x) return(is.null(x) | x %in% c("", "<NULL>"))

get_species_list <- function(nrows = -1){
  return(read.csv(here("inst", "data", "species_list.csv"), 
                  fileEncoding = "UTF-8-BOM", check.names = FALSE, nrows=nrows))
}

parse_span <- function(x, col) tags$span(x, style=paste0("color: ", col, ";"))

btn_row_style  <- "display: inline-block;
                   width: 100%;
                   height: 100%;
                   text-align: center; 
                   vertical-align: center; 
                   horizontal-align: center;"
btn_sel_style  <- "display:inline-block; 
                   text-align: left; 
                   padding-left: 1%; 
                   width: 100%;"
file_btn_style <- "padding:1%; width:100%;"
header_btn_style <- "padding: 0%;
                     vertical-align: center;"
plot_z_style <- "
#specplot_freq {
  position: absolute;
  z-index: 1;
}
#specplot_time {
  position: absolute;
  z-index: 2;
}
#specplot_front {
  position: absolute;
  z-index: 3;
}"

jsCode <- "shinyjs.audiotoggle = function() {
  var audio = document.getElementById('my_audio_player');
  if (audio.paused){ //check audio is playing
    audio.play();
  } else {
    audio.pause();
  }
}"


#profvis(runApp(), prof_output = file.path(getwd(),"profiling"))

#auth0::use_auth0(overwrite = TRUE)
#usethis::edit_r_environ()
#options(shiny.port = 8080)
#auth0::shinyAppAuth0(ui_func(), server)
#shinyApp(ui_func(), server)

# tell shiny to log all reactivity
#reactlog::reactlog_enable()

# run a shiny app
#shinyApp(ui_func(), server)

# once app has closed, display reactlog from shiny
#shiny::reactlogShow()
