library(ggplot2)
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyFiles)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(imola)
library(tuneR)
library(seewave) # for spectrogram
#library(plotly)
#library(oce)
library(viridis)
#library(grid)
#library(gridExtra)
#library(cowplot) # to get legend
library(profvis) # for checking code performance
library(dplyr)
library(stringr)

source('plot_helpers.R')

#TODO: navbarPage() to have distinct pages: label, verify/check, run model
#from https://shiny.rstudio.com/articles/layout-guide.html
#TODO: Color/highlight plot as it plays e.g. blue over red in oscillogram (have time tracker)
#TODO: Save list of extra species as a column in species list (or append to current list) 
#TODO: Colour border of label radio buttons same as bounding boxes (ggplot override aes) (notification label colour too)
#TODO: Label hover click option instead
#TODO: Show details of saved labels in list in a sidebar
#TODO: On clicking label in sidebar, highlights or zooms to the label (option to play it)
#TODO: Unit tests (especially for plots)
#TODO: Check soundgen pitch app https://github.com/tatters/soundgen
#TODO: Example sound files in right sidebar (https://birdwatchireland.ie/our-work/surveys-research/research-surveys/countryside-bird-survey/cbs-bird-songs-and-calls/)

#change max supported audio file size to 30MB
options(shiny.maxRequestSize = 30*1024^2)

file_list <- list.files('www/')

species_list <- read.csv("species_list.csv", fileEncoding = 'UTF-8-BOM')

.is_null <- function(x) return(is.null(x) | x == "<NULL>")

ui_func <- function() {
    sidebar <- {dashboardSidebar(
      sidebarMenu(
      menuItem("Configuration", tabName = "config_menu", icon = icon("bars"),
        #File/Folder selection
        shinyDirButton('folder',
                       label    = 'Folder select',
                       title    = 'Please select a folder'),
        verbatimTextOutput("folder", placeholder = TRUE),
        selectInput(
          "file1",
          "Select File:",
          choices = c("<NULL>", file_list),
          width   = '100%'
          ),
        selectInput("species_list",
                    "Species List:",
                    choices = colnames(species_list),
                    width   = '100%')
      ),
      menuItem("Sound Settings", tabName = "sound_menu", icon = icon("music"),
        selectInput(
          "noisereduction",
          "Spectrogram Noise reduction:",
          choices  = c("None", "Rows", "Columns"),
          selected = "None",
          width    = '100%'
        ),
        sliderInput(
                 "db_gain",
                 "dB Gain:",
                 min   = -96,
                 max   = 96,
                 value = 0,
                 ticks = FALSE
               )
      ),
      menuItem("Spectrogram Settings", tabName = "spec_menu", icon = icon("chart-area"),
        selectInput("freq_min", "minimum frequency in filter", choices = c(0, 2^(3:7)), selected = 0),
        selectInput("freq_max", "maximum frequency in filter", choices = 2^(4:9), selected = 32),
        selectInput(
          "palette_selected",
          "Spectrogram colour palette:",
          choices = palette_list,
          width   = '100%'
        ),
        sliderInput(
                 "db_contrast",
                 "Contrast:",
                 min   = 0,
                 max   = 96,
                 value = 0,
                 ticks = FALSE
               ),
        checkboxInput("palette_invert", "Invert color palette"),
        actionButton("savespec", "Save Spectrogram"),
        checkboxInput("include_hover", "Include spectrogram hover tooltip", value = FALSE),
        checkboxInput("spec_labs", "Show spectrogram labels"),
        uiOutput("spec_collapse")
      ),
      menuItem("FFT Settings", tabName = "fft_menu", icon = icon("barcode"),
        numericInput('window_width', 'Window Size (number of points)', value = 256),
        numericInput('fft_overlap', 'FFT Overlap (%)', value = 75, min = 1, max = 99, step = 1)
      ),
      menuItem("Oscillogram Settings", tabName = "osc_menu", icon = icon('chart-line'),
        actionButton("saveosc", "Save Oscilloogram"),
        checkboxInput("include_hover_osc", "Include oscillogram hover tooltip", value = FALSE),
        checkboxInput("osc_labs", "Show oscillogram labels"),
        uiOutput("osc_collapse")
      ),
      menuItem("Other Settings", tabName = "other_menu", icon = icon("cog"),
        numericInput('label_columns', 'Number of Columns', value = 5, min = 1, max = 9, step = 1)
      )
      ),
      #Options for sidebar
      collapsed = TRUE)}
    
    body <- {dashboardBody(
      theme = "blue_gradient",
      useShinyjs(),
      tags$head(tags$style(HTML(".content {padding-top: 0;}"))),
      htmlOutput("file1"),
      #Spectrogram Plot
      fluidRow({
        div(
          plotOutput(
            "specplot",
            height   = 250,
            click    = "specplot_click",
            dblclick = "specplot_dblclick",
            hover    = hoverOpts(
              id        = "specplot_hover",
              delay     = 10,
              delayType = "debounce"
              ),
            brush    = brushOpts(
              id         = "specplot_brush",
              resetOnNew = TRUE)
            ),
          plotOutput(
            "specplot_blank",
            height   = 25,
            ),
          tags$head(tags$style('
          #hover_info {
            position: absolute;
            width: 300px;
            z-index: 100;
           }
        ')),
        tags$script('
          $(document).ready(function(){
            // id of the plot
            $("#specplot").mousemove(function(e){ 
      
              // ID of uiOutput
              $("#hover_info").show();         
              $("#hover_info").css({             
                top: (e.pageY + 5) + "px",             
                left: (e.pageX + 5) + "px"         
              });     
            });     
          });
        '),
          uiOutput("hover_info")
          )
      }),
      #Oscillogram Plot
      fluidRow({
        div(
          plotOutput(
            "oscplot",
            height   = 110,
            click    = "oscplot_click",
            dblclick = "oscplot_dblclick",
            hover    = hoverOpts(
              id        = "oscplot_hover",
              delay     = 50,
              delayType = "debounce"
              ),
            brush    = brushOpts(
              id         = "oscplot_brush",
              direction  = "x",
              resetOnNew = TRUE
              )
            ),
          plotOutput(
            "oscplot_blank",
            height   = 25,
            ),
          tags$head(tags$style('
          #hover_info_osc {
            position: absolute;
            width: 300px;
            z-index: 100;
           }
        ')),
        tags$script('
          $(document).ready(function(){
            // id of the plot
            $("#oscplot").mousemove(function(e){ 
      
              // ID of uiOutput
              $("#hover_info_osc").show();         
              $("#hover_info_osc").css({             
                top: (e.pageY - 15) + "px",             
                left: (e.pageX + 5) + "px"
              });     
            });     
          });
        '),
          uiOutput("hover_info_osc")
          )
        }),
      #One row of audio settings
      fluidRow({
        div(
        column(4,{
          uiOutput("freq_ui")
               }),
        column(4,{
               div(
                 HTML("<b>Play audio:<b/>"),
                 #style = "color: black;",
                 uiOutput('my_audio'),
                 tags$script('
                 var id = setInterval(audio_pos, 100);
                 function audio_pos() {
                  var audio = document.getElementById("my_audio_player");
                  var curtime = audio.currentTime;
                  console.log(audio);
                  Shiny.onInputChange("get_time", curtime);
                 };'),
                 #actionButton("get_time", "Get Time", onclick = js),
                 verbatimTextOutput("audio_time")
                 )
          }),
        column(2,{
                 selectInput(
                   "playbackrate",
                   "Playback Speed:",
                   choices  = paste0(c(0.1, 0.25, 0.5, 1, 2, 5, 10), "x"),
                   selected = "1x",
                   width    = '100%'
                 )
                 }),
        column(2,{
            fixedRow(style = "display:inline-block;width:100%;text-align: center;  vertical-align:center; horizontal-align:center",
              div(
                tipify(actionButton("prev_file", "", icon = icon("arrow-left"), style='padding:1%; font-size:90%'),  "Previous File"),
                disabled(tipify(
                  actionButton("prev_section", "", icon = icon("chevron-left"), style='padding:1%; font-size:90%'),  "previous section"
                )),
                disabled(tipify(
                  actionButton("next_section", "", icon = icon("chevron-right"), style='padding:1%; font-size:90%'), "next section"
                )),
                tipify(actionButton("next_file", "", icon = icon("arrow-right"), style='padding:1%; font-size:90%'), "Next File")
              ),
              fixedRow(style = "display:inline-block;width:100%;text-align: center;  vertical-align:center; horizontal-align:center",
                actionButton("plt_reset", "Reset Plot")
                )
              )
            })
        )
        }),
      #Labelling
      fluidPage({
        div(uiOutput("label_ui"),
            column(6, 
              textInput("otherCategory", "Type in additional category:", width = "100%"),
              #br(),
              fixedRow(style = "display:inline-block; text-align: center; padding-left: 1%; width: 100%;",
                actionButton("addCategory", HTML("<b>Add category</b>"), style = "width: 60%;"),
                actionButton("remCategory", HTML("<b>Remove category</b>"), style = "width: 60%;"),
                actionButton("resetCategory", HTML("<b>Reset categories</b>"), style = "width: 60%;")
                )
              ),
            column(6,
            fluidRow(style = "display:inline-block; text-align: left; padding-left: 1%; width: 100%;",
                     selectInput(
                       inputId    = "call_type", 
                       label      = "Call Type:", 
                       width      = '100%',
                       multiple   = TRUE,
                       choices    = c("<NULL>", "song", "alarm call", "flight call", "flock"), 
                       selected   = "<NULL>")
                     ),
            #br(),
            #TODO: Other info to label/record -
            ## altitude of recorder (check if in metadata)
            fluidRow(style = "display:inline-block; text-align: center; padding-left: 1%; width: 100%;",
              actionButton("save_points", HTML("<b>Save Selection</b>"), style = "width: 60%;"),
              actionButton("remove_points", HTML("<b>Delete Selection</b>"), style = "width: 60%;"),
              actionButton("undo_delete_lab", HTML("<b>Undo Deletion</b>"), style = "width: 60%;")
              )
            )
          )
        })
    )}
  ui <- dashboardPage(
    dashboardHeader(title = "Audio Labeler App"),
    sidebar,
    body
  )
  return(ui)
}

server <- function(input, output, session) {
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyDirChoose(
    input          = input,
    id             = 'folder',
    roots          = volumes,
    filetypes      = c('wav'),
    allowDirCreate = FALSE
  )
  
  global <- reactiveValues(datapath = getwd())
  
  mydir <- reactive(input$folder)
  
  output$folder <- renderText({global$datapath})
  
  output$files <- renderPrint(list.files(global$datapath))
  
  observeEvent(ignoreNULL  = TRUE, eventExpr = {input$folder},
               handlerExpr = {
                 if (!"path" %in% names(dir())) return()
                 home <- normalizePath("~")
                 global$datapath <-
                   file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
               })
  
  ranges_spec  <- reactiveValues(x = NULL, y = NULL)
  ranges_osc   <- reactiveValues(x = NULL)
  length_ylabs <- reactiveValues(osc  = 4,    spec = 0)
  deleted_lab  <- reactiveValues(rows = NULL, data = NULL)
  plots_open   <- reactiveValues(osc  = TRUE, spec = TRUE)
  
  length_b10 <- function(x){
    return(x %>% 
             range %>% 
             as.integer %>% 
             as.character %>% 
             str_length %>% 
             max)
  }
  
  blank_plot <- function(label){
    p <- ggplot() + 
      geom_text(aes(x = 0, y = 0), label = label, colour = "white") +
      theme_void() + 
      theme(
        plot.background = element_rect(fill = rgb(0.1529412, 0.1686275, 0.1882353)),
        legend.position = "none")
    return(p)
  }
  
  frange_check <- function(vec, freq_range){
    return((vec[1] > freq_range[1]) | (vec[2] < freq_range[2]))
  }
  
  plot_collapse_button <- function(name, id, top_pad = 0){
    if(plots_open[[id]]){
      button_id    <- paste0("collapse_", id)
      button_icon  <- "chevron-up"
      button_hover <- paste("Collapse", name)
    } else {
      button_id    <- paste0("open_", id)
      button_icon  <- "chevron-down"
      button_hover <- paste("Open", name)
    }
    actionButton(button_id, button_hover, icon = icon(button_icon))
  }
  
  get_entries <- function(x) return(x[x != ""])
  
  categories   <- reactiveValues(
    base = get_entries(species_list[,1]), 
    misc = c("Noise", "Other"),
    xtra = "abc"
  )
  
  observeEvent(input$species_list, {
    categories$base <- get_entries(species_list[,input$species_list])
  })
  
  class_label <- reactive({
    cats <- categories
    c(cats$base, cats$misc, cats$xtra)
  })
  
  output$label_ui <- renderUI({
    extra_cols   <- c('red','darkred')
    cextra <- categories$xtra
    my_gradients <- colorRampPalette(c('darkred','red'))(length(cextra))
    
    get_jq_lines <- function(val, cols){
      x <- paste0("var ", c("originalBorder", "originalColor", "originalBackground" ), " = [];", collapse= " ")
      #x <- "var originalBorder = []; var originalColor = []; var originalBackground = [];"
      button_val_id <- paste0("'input[type=radio][name=label_points][value=", val, "]'")
      
      css_line <- function(css_class, css_val = NULL, func = "css", sq = "'"){
        x <- paste0("$(this).parent().", func, "(", sq, css_class, sq)
        if(!is.null(css_val))
          x <- paste0(x, ",'", css_val, "'")
        x <- paste0(x, ")")
        if(func == "css")
          x <- paste0(x, ";")
        return(x)
      }
        x <- paste0(x, 
        "$(", button_val_id, ").parent().css({",
          "'color':            'black',",
          "'background-color': '", cols[1], "',",
          "'border-color':     '", cols[2], "'});")
        x <- paste0(x, "$(", button_val_id, ").hover(",
        "function(){",
            "originalBorder[", css_line(button_val_id, func = "index", sq=""), "]     = ", css_line('border-color'),
            "originalBackground[", css_line(button_val_id, func = "index", sq=""), "] = ", css_line('background-color'),
            "originalColor[", css_line(button_val_id, func = "index", sq=""), "]      = ", css_line('color'),
            css_line('border-color', 'darkblue'),
            css_line('background-color', 'blue'),
            css_line('color', 'white'),
          "},")
        x <- paste0(x, 
        "function(){",
        css_line('border-color',     paste0("originalBorder[", css_line(button_val_id, func = "index", sq=""), "]")), 
        css_line('background-color', paste0("originalBackground[", css_line(button_val_id, func = "index", sq=""), "]")), 
        css_line('color',            paste0("originalColor[", css_line(button_val_id, func = "index", sq=""), "]")),
          "});")
        x <- gsub("'original", "original", x)
        x <- gsub(")]')", ")])", x)
      return(x)
    }
    btn_col_script <- NULL
    if(!is.null(cextra)){
      for(k in 1:length(cextra))
        btn_col_script <- paste0(btn_col_script, get_jq_lines(cextra[k], extra_cols))
    }
    #print(btn_col_script)
    div(
      radioGroupButtons(
        inputId    = "label_points", 
        label      = paste("Class Label Selection:", input$species_list), 
        individual = TRUE,
        width      = '100%',
        status     = "primary",
        choices    = class_label(), 
        #selected     = "",
      ),
      tags$style(paste0(".btn-group-container-sw {
                          display: grid;
                          grid-template-columns: ",
                          paste(rep("1fr", input$label_columns), collapse=" "), 
                        ";}
                        .radiobtn {
                          width: 100%;
                        }")),
      #tags$script(btn_col_script),
      )
    })
  
  output$freq_ui <- renderUI({
    freq_range <- as.numeric(c(input$freq_min, input$freq_max))
    div(
      sliderInput(
        "frequency_range",
        "Audio Frequency Range:",
        min   = freq_range[1],
        max   = freq_range[2],
        step  = 0.2,
        value = freq_range,
        ticks = FALSE
      ))
  })
  
  audioInput <- reactive({
    if(.is_null(input$file1))     
      return(NULL) 
    tmp_audio <- readWave(file.path(getwd(), "www", input$file1))
    
    tmp_audio@samp.rate <- tmp_audio@samp.rate * as.numeric(gsub("x", "", input$playbackrate))
    
    #setWavPlayer("C:/Program Files/Windows Media Player/wmplayer.exe")
    if(!is.null(ranges_osc$x) | !is.null(ranges_spec$x)){
      #time crop
      if(!is.null(ranges_osc$x))
        tc <- ranges_osc$x
      if(!is.null(ranges_spec$x))
        tc <- ranges_spec$x
      tmp_audio <- extractWave(tmp_audio, from = tc[1], to = tc[2], xunit = "time")
    }
    
    #based on torchaudio::functional_gain
    audio_gain <- function (waveform, gain_db = 0){
      if(gain_db == 0)
        return(waveform)
      ratio <- 10^(gain_db/20)
      return(waveform * ratio)
    }
    submean <- function(x) x - mean(x)
    tmp_audio@left <- submean(tmp_audio@left)
    tmp_audio      <- audio_gain(tmp_audio, input$db_gain)
    #anything outside the 16-bit range [-32768, 32767] will be rounded
    tmp_audio@left[tmp_audio@left >  32767] <-  32767
    tmp_audio@left[tmp_audio@left < -32768] <- -32768
    tmp_audio@left <- as.integer(tmp_audio@left)
    
    writeWave(tmp_audio, 'www/tmp.wav')
    return(tmp_audio)
  })
  
  cleanInput <- reactive({
    tmp_audio <- audioInput()
    if(is.null(tmp_audio))
      return(NULL)
    
    frange <- input$frequency_range
    
    tmp_spec <- spectro(tmp_audio,
                        f        = tmp_audio@samp.rate, 
                        wl       = input$window_width, 
                        ovlp     = input$fft_overlap, 
                        plot     = FALSE)
    
    if(frange_check(frange, range(tmp_spec$freq))){
      complex_spec <- spectro(tmp_audio,
                              f        = tmp_audio@samp.rate, 
                              wl       = input$window_width, 
                              ovlp     = input$fft_overlap,
                              complex  = TRUE,
                              plot     = FALSE,
                              norm     = FALSE,
                              dB       = NULL)
      #Put zeros outside frequency range and reconstruct audio file from complex spec
      out_freq <- complex_spec$freq < frange[1] | complex_spec$freq > frange[2]
      complex_spec$amp[out_freq,] <- 0
      audio_inv <- istft(complex_spec$amp,
                         f    = tmp_audio@samp.rate,
                         wl   = input$window_width, 
                         ovlp = input$fft_overlap,
                         out  = "Wave")
      tmp_audio <- normalize(audio_inv, unit = "16")
    } else 
      return(NULL)
    writeWave(tmp_audio, 'www/tmp_clean.wav')
    return(tmp_audio)
  })
  
  output$file1 <- renderUI({
    if(.is_null(input$file1))
      return(NULL)
    else
      return(HTML(paste0('<b>', input$file1, '</b>')))
    })
  
  specData <- reactive({
    if(.is_null(input$file1))     
      return(data.frame(time        = 1,
                        frequency   = 1:10,
                        amplitude   = rep(-96,10),
                        freq_select = 1))
    tmp_audio <- audioInput()
    
    noisered <- switch(input$noisereduction,
                       None    = NULL,
                       Rows    = 1,
                       Columns = 2)
    
    spec <- spectro(tmp_audio,
                    f        = tmp_audio@samp.rate, 
                    wl       = input$window_width, 
                    ovlp     = input$fft_overlap, 
                    plot     = FALSE,
                    noisereduction = noisered)
    
    spec$amp <- spec$amp + input$db_gain
    spec$amp <- spec$amp + input$db_contrast
    
    df   <- data.frame(time      = rep(spec$time, each  = nrow(spec$amp)), 
                       frequency = rep(spec$freq, times = ncol(spec$amp)), 
                       amplitude = as.vector(spec$amp))
    if(!is.null(ranges_osc$x))
      df$time <- df$time + ranges_osc$x[1]
    else if(!is.null(ranges_spec$x))
      df$time <- df$time + ranges_spec$x[1]
    
    df$freq_select <- 1
    
    frange <- input$frequency_range
    if(frange_check(frange, range(df$frequency)))
      df$freq_select[df$frequency < frange[1] | df$frequency > frange[2]] <- 0.4
    
    write.csv(df, 'tmp_spec.csv', row.names = FALSE)
    return(df)
  })
  
  oscData <- reactive({
    if(.is_null(input$file1))     
      return(NULL)
    if(is.null(cleanInput()))
      tmp_audio <- audioInput()
    else
      tmp_audio <- cleanInput()
    df2 <- data.frame(time      = seq(0, length(tmp_audio@left)/tmp_audio@samp.rate, length.out = length(tmp_audio)),
                      amplitude = tmp_audio@left - mean(tmp_audio@left))
    if(!is.null(ranges_osc$x))
      df2$time <- df2$time + ranges_osc$x[1]
    else if(!is.null(ranges_spec$x))
      df2$time <- df2$time + ranges_spec$x[1]
    
    #spacing for "custom" y axis margin
    strlen_osc_y  <- length_b10(df2$amplitude)
    spec_freq <- input$frequency_range
    if(!is.null(ranges_spec$y))
      spec_freq <- c(max(spec_freq[1], ranges_spec$y[1]), min(spec_freq[2], ranges_spec$y[2]))
    strlen_spec_y <- length_b10(pretty(spec_freq, 5))
    if(strlen_spec_y + 3 <= strlen_osc_y){
      length_ylabs$osc  <- 0
      length_ylabs$spec <- strlen_osc_y - strlen_spec_y - 3
    } else {
      length_ylabs$osc  <- strlen_spec_y + 3 - strlen_osc_y
      length_ylabs$spec <- 0
    }
    return(df2)
  })
  
  output$specplot <- renderPlot({
    p <- plot_spectrogram(specData(), input, length_ylabs)
    
    if(!.is_null(input$file1)){
      if(!is.null(ranges_spec$y))
        p <- p + coord_cartesian(ylim = ranges_spec$y, expand = FALSE)
      spec_name <- paste0(gsub('.wav', '', input$file1), '_spec.png')
    } else
      spec_name <- 'blank_spec.png'
    observeEvent(input$savespec, {
      file_nm <- file.path(getwd(), "images", spec_name)
      width   <- session$clientData$output_specplot_width
      height  <- session$clientData$output_specplot_height
      # For high-res displays, this will be greater than 1
      pixelratio <- session$clientData$pixelratio
      ggsave(file_nm, p,
             height = height*pixelratio, 
             width  = width*pixelratio,
             units  = "px")
      showNotification(HTML(paste0("Spectrogram image <b>", spec_name, "</b> saved to <b>images</b>.")), 
                       #TODO: clickable link to images folder
                       #action = a(href = file.path("file://", getwd(), "images"), "Go to folder", target = "_blank"),
                       type = "message")
    })
    return(p)
  })
      
  output$specplot_blank <- renderPlot({
    if(plots_open$spec)
      return(NULL)
    else 
      blank_plot(label = "Spectrogram (hidden)")
    })
  
  output$oscplot <- renderPlot({
    p <- plot_oscillogram(oscData(), input, length_ylabs)
    
    if(!.is_null(input$file1)){
      if(!is.null(ranges_spec$x))
        p <- p + coord_cartesian(xlim = ranges_spec$x, expand = TRUE)
     else if(!is.null(ranges_osc$x))
        p <- p + coord_cartesian(xlim = ranges_osc$x, expand = TRUE)
     osc_name <- paste0(gsub('.wav', '', input$file1), '_osc.png')
    } else 
      osc_name <- 'blank_osc.png'
    file_nm   <- file.path(getwd(), "images", osc_name)
  
    width  <- session$clientData$output_oscplot_width
    height <- session$clientData$output_oscplot_height
    # For high-res displays, this will be greater than 1
    pixelratio <- session$clientData$pixelratio
    observeEvent(input$saveosc, {
      ggsave(file_nm, p,
             height = height*pixelratio, 
             width  = width*pixelratio,
             units  = "px")
      showNotification(HTML(paste0("Oscillogram image <b>", osc_name, "</b> saved to <b>images</b>.")), 
                       #TODO: clickable link to images folder
                       #action = a(href = file.path("file://", getwd(), "images"), "Go to folder", target = "_blank"),
                       type = "message")
    })
    return(p)
  })
  
  output$oscplot_blank <- renderPlot({
    if(plots_open$osc)
      return(NULL)
    else 
      blank_plot(label = "Oscillogram (hidden)")
  })
  
  output$hover_info <- renderUI({
    if(.is_null(input$file1) | !input$include_hover)
      return(NULL)
    hover <- input$specplot_hover
    point <- nearPoints(specData(), hover, threshold = 5, maxpoints = 1, addDist = TRUE, xvar="time", yvar="frequency")
    if(nrow(point) == 0) 
      return(NULL)
    point <- round(point, 3)
    
    in_label_box <- function(df, point){
      pb_rate <- as.numeric(gsub("x", "", input$playbackrate))
      return(point$time      >= df$start_time / pb_rate & 
             point$time      <= df$end_time   / pb_rate &
             point$frequency >= df$start_freq * pb_rate &
             point$frequency <= df$end_freq   * pb_rate)
    }
    lab_df <- read.csv("tmp_labels.csv")
    lab_df <- lab_df[lab_df$file_name == input$file1 & in_label_box(lab_df, point),]
    if(nrow(lab_df) == 0 | !input$spec_labs)
      species_in_hover <- ''
    else{
      lab_df <- lab_df[1,]
      species_in_hover <- paste0("<br/><b> Species: </b>", lab_df$class_label,
                                 "<br/><b> Call type: </b>", lab_df$call_type)
    }
    
    # create style property for tooltip
    # background color is set so tooltip is a almost transparent
    # z-index is the stack index and is set to 100 so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; ",
                    "background-color: rgba(120, 120, 120, 0.25); ",
                    "color: rgb(245, 245, 245); padding: 1%;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Time: </b>", point$time, " seconds<br/>", 
                    "<b> Frequency: </b>", point$frequency, " kHz<br/>",
                    "<b> Amplitude: </b>", point$amplitude, " dB",
                    species_in_hover)))
    )
  })
  
  output$spec_collapse <- renderUI({
    plot_collapse_button("Spectrogram", 'spec')
    })
  
  observeEvent(input$collapse_spec, {
    plots_open$spec <- FALSE
  })
  
  observeEvent(input$open_spec, {
    plots_open$spec <- TRUE
  })
  
  output$hover_info_osc <- renderUI({
    if(.is_null(input$file1) | !input$include_hover_osc)
      return(NULL)
    hover <- input$oscplot_hover
    point <- nearPoints(oscData(), hover, threshold = 5, maxpoints = 1, addDist = TRUE, xvar="time")
    if(nrow(point) == 0) 
      return(NULL)
    point <- round(point, 3)
    
    in_label_box <- function(df, point){
      pb_rate <- as.numeric(gsub("x", "", input$playbackrate))
      return(point$time      >= df$start_time / pb_rate & 
             point$time      <= df$end_time   / pb_rate)
    }
    lab_df <- read.csv("tmp_labels.csv")
    lab_df <- lab_df[lab_df$file_name == input$file1 & in_label_box(lab_df, point),]
    
    if(nrow(lab_df) == 0 | !input$osc_labs)
      species_in_hover <- ''
    else{
      lab_df <- lab_df[1,]
      species_in_hover <- paste0("<br/><b> Species: </b>", lab_df$class_label,
                                 "<br/><b> Call type: </b>", lab_df$call_type)
    }
    
    # create style property for tooltip
    # background color is set so tooltip is a almost transparent
    # z-index is the stack index and is set to 100 so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; ",
                    "background-color: rgba(120, 120, 120, 0.25); ",
                    "color: rgb(245, 245, 245); padding: 1%;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Time: </b>", point$time, " seconds<br/>", 
                    "<b> Amplitude: </b>", point$amplitude,
                    species_in_hover)))
    )
  })
  
  output$osc_collapse <- renderUI({
    plot_collapse_button("Oscillogram", 'osc')
  })
  
  observeEvent(input$collapse_osc, {
    plots_open$osc <- FALSE
  })
  
  observeEvent(input$open_osc, {
    plots_open$osc <- TRUE
  })
  
  observeEvent(input$specplot_dblclick, {
    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    brush <- input$specplot_brush
    if (!is.null(brush)) {
      ranges_spec$x <- c(brush$xmin, brush$xmax)
      ranges_spec$y <- c(brush$ymin, brush$ymax)
      showNotification("Double click either plot to reset zoom", type = "default") 
    } else {
      ranges_spec$x <- NULL
      ranges_spec$y <- NULL
    }
  })
  
  observeEvent(input$oscplot_dblclick, {
    brush <- input$oscplot_brush
    if (!is.null(brush)){
      ranges_osc$x <- c(brush$xmin, brush$xmax)
      showNotification("Double click either plot to reset zoom", type = "default") 
    } else
      ranges_osc$x <- NULL
  })
  
  observeEvent(input$plt_reset, {
    ranges_spec$x <- NULL
    ranges_spec$y <- NULL
    ranges_osc$x  <- NULL
  })
  
  observeEvent(input$addCategory, {
    if(gsub(" ", "", input$otherCategory) == "")
      showNotification("Need category name to add", type = "error")
    else if(input$otherCategory %in% class_label())
      showNotification("Category already present", type = "error")
    else
      categories$xtra <- c(categories$xtra, input$otherCategory)
  })
  
  observeEvent(input$remCategory, {
    if(is.null(input$label_points))
      showNotification("Need a category selected to remove it", type = "error")
    else if(input$label_points %in% c(categories$base, categories$misc))
      showNotification("Cannot remove main or misc category!", type = "error")
    else if(input$label_points %in% categories$xtra){
      categories$xtra <- categories$xtra[-which(categories$xtra == input$label_points)]
      showNotification(HTML(paste0("Label <b>", input$label_points, "</b> removed")), type = "warning")
    }
  })
  
  observeEvent(input$resetCategory, {
    categories$xtra <- NULL
    showNotification("Extra labels reset to null", type = "warning")
  })
  
  observeEvent(input$save_points, {
    #get x and y coordinates with max and min of brushedPoints()
    res <- brushedPoints(specData(), input$specplot_brush,
                         xvar = 'time', yvar = 'frequency')
    bb_iou <- function(boxA, boxB){
      # intersection_over_union
      # boxes have column start_time, end_time, start_freq, end_freq
      # determine the (x, y)-coordinates of the intersection rectangle
      xA <- max(boxA[,1], boxB[,1])
      yA <- max(boxA[,3], boxB[,3])
      xB <- min(boxA[,2], boxB[,2])
      yB <- min(boxA[,4], boxB[,4])
      #apply(rbind(boxA,boxB),2,max)
      # compute the area of intersection rectangle
      interArea <- max(0, xB - xA) * max(0, yB - yA)
      # compute the area of both boxA and boxB
      box_area <- function(box)
        return((box[,2] - box[,1]) * (box[,4] - box[,3]))
      boxAArea <- box_area(boxA)
      boxBArea <- box_area(boxB)
      # compute the intersection over union
      iou <- interArea / (boxAArea + boxBArea - interArea)
      return(iou)
    }
    if(!is.null(input$specplot_brush)) {
      if(is.null(input$call_type))
        call_type <- "<NULL>"
      else
        call_type <- paste(input$call_type, collapse='; ')
      lab_df <- data.frame(date_time   = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                           file_name   = input$file1,
                           start_time  = min(res$time),
                           end_time    = max(res$time),
                           start_freq  = min(res$frequency),
                           end_freq    = max(res$frequency),
                           class_label = input$label_points,
                           call_type   = call_type,
                           labeler     = Sys.info()[["user"]])
      
      file_name <- "tmp_labels.csv"
      
        if(file.exists(file_name))
          write.table(lab_df, file_name, append = TRUE,  col.names = FALSE, sep=",", row.names = FALSE)
        else
          write.table(lab_df, file_name, append = FALSE,  col.names = TRUE, sep=",", row.names = FALSE)
        showNotification(HTML(paste0("Label <b>", input$label_points, "</b> successfully saved!")), type = "message")
    } else {
      showNotification("Label not saved, nothing selected!", type = "error")
    }
  })
  
  observeEvent(input$remove_points, {
    #get x and y coordinates with max and min of brushedPoints()
    res <- brushedPoints(specData(), input$specplot_brush,
                         xvar = 'time', yvar = 'frequency')
    file_name <- "tmp_labels.csv"
    bb_iou <- function(boxA, boxB){
      # intersection_over_union
      # boxes have column start_time, end_time, start_freq, end_freq
      # determine the (x, y)-coordinates of the intersection rectangle
      xA <- max(boxA[,1], boxB[,1])
      yA <- max(boxA[,3], boxB[,3])
      xB <- min(boxA[,2], boxB[,2])
      yB <- min(boxA[,4], boxB[,4])
      #apply(rbind(boxA,boxB),2,max)
      # compute the area of intersection rectangle
      interArea <- max(0, xB - xA) * max(0, yB - yA)
      # compute the area of both boxA and boxB
      box_area <- function(box)
        return((box[,2] - box[,1]) * (box[,4] - box[,3]))
      boxAArea <- box_area(boxA)
      boxBArea <- box_area(boxB)
      # compute the intersection over union
      iou <- interArea / (boxAArea + boxBArea - interArea)
      return(iou)
    }
    if(!is.null(input$specplot_brush)) {
      lab_df <- data.frame(date_time   = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                           file_name   = input$file1,
                           start_time  = min(res$time),
                           end_time    = max(res$time),
                           start_freq  = min(res$frequency),
                           end_freq    = max(res$frequency),
                           class_label = input$label_points,
                           labeler     = Sys.info()[["user"]])
      full_df    <- read.csv(file_name)
      full_df_rm <- c()
      for(idx in 1:nrow(full_df)){
        bb_cols  <- c('start_time', 'end_time', 'start_freq', 'end_freq')
        check_df <- full_df[idx,]
        row_iou  <- bb_iou(lab_df[,bb_cols], check_df[,bb_cols])
        if(row_iou > 0.6){
          full_df_rm <- c(full_df_rm, idx)
        }
      }
      if(!is.null(full_df_rm)){
        deleted_lab$rows <- full_df_rm
        deleted_lab$data <- full_df[full_df_rm,]
        full_df <- full_df[-full_df_rm,]
        write.table(full_df, file_name, append = FALSE,  col.names = TRUE, sep=",", row.names = FALSE)
        showNotification("Label removed, click Undo to bring back", type = "message")
      }
    } else {
      showNotification("Label not removed, nothing selected!", type = "error")
    }
  })
  
  observeEvent(input$undo_delete_lab, {
    file_name <- 'tmp_labels.csv'
    full_df   <- read.csv(file_name)
    rownums   <- deleted_lab$rows
    del_df    <- deleted_lab$data
    if(!is.null(del_df)){
      insertRow <- function(existingDF, newrow, idx) {
        if(idx <= nrow(existingDF))
          existingDF[seq(idx+1,nrow(existingDF)+1),] <- existingDF[seq(idx,nrow(existingDF)),]
        existingDF[idx,] <- newrow
        return(existingDF)
      }
      for(row in 1:length(rownums))
        full_df <- insertRow(full_df, del_df[row,], rownums[row])
      deleted_lab$rows <- NULL
      deleted_lab$data <- NULL
      write.table(full_df, file_name, append = FALSE,  col.names = TRUE, sep=",", row.names = FALSE)
      showNotification("Label recovered", type = "message")
    } else {
      showNotification("Nothing undone, no deletions detected!", type = "error")
    }
  })
  
  output$my_audio <- renderUI({
    if(!is.null(cleanInput()))
      file_name <- 'www/tmp_clean.wav'
    else
      file_name <- 'www/tmp.wav'
    
    audio_style <- HTML("
    filter: sepia(50%);
    background-color: red;
    color: green;")
    if(.is_null(input$file1))     
      return(tags$audio(id       = 'my_audio_player',
                        src      = "", 
                        type     = "audio/wav", 
                        controls = NA, 
                        autoplay = NA,
                        style    = audio_style
      ))
    tags$audio(id       = 'my_audio_player',
               src      = markdown:::.b64EncodeFile(file_name), 
               type     = "audio/wav", 
               controls = "controls",#HTML('controlsList: nodownload'),
               #TODO: HTML styling (background colour, no download button, playback speed,...)
               style    = audio_style)
    })
  
  output$audio_time <- renderPrint({
    if(.is_null(input$file1))
      return("<NULL>")
    else {
      input$get_time
    }
  })
  
  observe({
    shinyjs::toggle(id  = "specplot",
              anim      = TRUE,
              animType  = "fade",
              time      = 0.5,
              condition = plots_open$spec)
    shinyjs::toggle(id  = "specplot_blank",
                    condition = !plots_open$spec)
  })
  
  observe({
    shinyjs::toggle(id  = "oscplot",
              anim      = TRUE,
              animType  = "fade",
              time      = 0.5,
              condition = plots_open$osc)
    shinyjs::toggle(id  = "oscplot_blank",
                    condition = !plots_open$osc)
  })
  
  # move to previous file (resetting zoom)
  observeEvent(input$prev_file, {
    if(.is_null(input$file1))
      updateSelectInput(inputId  = "file1",
                        selected = file_list[length(file_list)])
    else {
      idx <- which(input$file1 == file_list) - 1
      if(idx == 0)
        idx <- length(file_list)
      ranges_spec$x <- NULL
      ranges_spec$y <- NULL
      ranges_osc$x  <- NULL
      updateSelectInput(inputId  = "file1",
                        selected = file_list[idx])
    }
  })
  
  # move to next file (resetting zoom)
  observeEvent(input$next_file, {
    if(.is_null(input$file1))
      updateSelectInput(inputId  = "file1",
                        selected = file_list[1])
    else {
      idx <- which(input$file1 == file_list) + 1
      if(idx > length(file_list))
        idx <- 1
      ranges_spec$x <- NULL
      ranges_spec$y <- NULL
      ranges_osc$x  <- NULL
      updateSelectInput(inputId  = "file1",
                        selected = file_list[idx])
    }
  })
}

#profvis(runApp(), prof_output = file.path(getwd(),'profiling'))

shinyApp(ui_func(), server)


#library(reactlog)

# tell shiny to log all reactivity
#reactlog_enable()

# run a shiny app
#shinyApp(ui_func(), server)

# once app has closed, display reactlog from shiny
#shiny::reactlogShow()
