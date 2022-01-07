library(ggplot2)
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyFiles)
library(tuneR)
library(seewave) # for spectrogram
#library(plotly)
#library(oce)
library(viridis)
library(grid)
#library(gridExtra)
library(cowplot) # to get legend
library(profvis) # for checking code performance

source('plot_helpers.R')
source('spectrogram_params.R')

#change file size to 30MB
options(shiny.maxRequestSize = 30*1024^2)

classes <- c(read.csv("species_list.csv")[,1], "Noise", "Other")

file_list <- list.files('www/')

classes <- sort(classes)

.is_null <- function(x) return(is.null(x) | x == "<NULL>")

ui_func <- function(){
  ui <- fluidPage(
    fluidRow(
      plotOutput("specplot",
                 height   = 250,
                 click    = "specplot_click",
                 dblclick = "specplot_dblclick",
                 hover    = "specplot_hover", #TODO: hover tooltip
                 brush    = brushOpts(
                   id         = "specplot_brush",
                   resetOnNew = TRUE)
                 ),
    ),
    fluidRow(
      plotOutput("oscplot",
                 height   = 110,
                 click    = "oscplot_click",
                 dblclick = "oscplot_dblclick",
                 hover    = "oscplot_hover",
                 brush    = brushOpts(
                   id         = "oscplot_brush",
                   direction  = "x",
                   resetOnNew = TRUE)
                 ),
    ),
    fluidRow(
      column(5,
      sliderInput("db_gain", "dB Gain:",
                  min = -96, max = 96, value = 0,
                  ticks = FALSE),
      ),
      column(5,
      sliderInput("db_contrast", "Contrast:",
                  min = 0, max = 96, value = 0,
                  ticks = FALSE)
      ),
      column(2,
             br(),
             fixedRow(tags$div(style="display: inline-block; vertical-align:center; horizontal-align:center", class = "row-fluid",
                               tipify(actionButton("prev_file", "", icon = icon("arrow-left")),  "Prev"),
                               disabled(tipify(actionButton("prev_section", "", icon = icon("chevron-left")),  "prev section")),
                               disabled(tipify(actionButton("next_section", "", icon = icon("chevron-right")), "next section")),
                               tipify(actionButton("next_file", "", icon = icon("arrow-right")), "Next"))
             ),
             div(style="display:inline-block;width:100%;text-align: center;  vertical-align:center; horizontal-align:center",
               actionButton("plt_reset", "Reset Plot"))
             )
    ),
    fluidRow(
      column(5,
             div(h4("Select a folder"), style = "color: black;",
                 shinyDirButton('folder', 
                                label    = 'Folder select', 
                                title    = 'Please select a folder')
                 ),
             verbatimTextOutput("folder", placeholder = TRUE), br(),
             selectInput("file1", "Select File:", 
                         choices = c("<NULL>", file_list),
                         width   = '100%')
      ),
      column(3,
             div(h4("Labeling"), style = "color: black;",
             radioButtons("label_points", "Label Selection:", 
                          choices = classes
                          ),
             textInput("otherCategory", "Type in additional category"),
             fixedRow(
             actionButton("addCategory", "Add category"),
             actionButton("resetCategory", "Reset categories")
             ),
             #TODO: "other" button should come with textInput box, and can't submit unless it is filled (use shinyvalidate)
             #TODO: Other info to label/record - 
             ## type of sound e.g. alarm call, flight call, flock
             ## naming groups: Order, Family, Genus, Species, Subspecies
             ## altitude of recorder (check if in metadata)
             disabled(actionButton("save_points", HTML("<b>Save Selection</b>")))
             )
      ),
      column(4,
             div(h4("Play audio"), style = "color: black;",
             uiOutput('my_audio')
             ),
             selectInput("noisereduction", "Spectrogram Noise reduction:", 
                         choices  = c("None", "Rows", "Columns"),
                         selected = "None",
                         width    = '100%'),
             selectInput("palette_selected", "Spectrogram colour palette:", 
                         choices = palette_list,
                         width   = '100%')
      )
    )
  )
return(ui)
}

server <- function(input, output) {
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
  
  ranges_spec <- reactiveValues(x = NULL, y = NULL)
  ranges_osc  <- reactiveValues(x = NULL)
  
  audio_path <- reactive({
    if(!is.null(ranges_spec$x)){}#do nothing
    if(!is.null(ranges_osc$x)){} #do nothing
    if(input$db_gain !=0){}     #do nothing
    return('www/tmp.wav')
    })
  
  audioInput <- reactive({
    if(.is_null(input$file1))     
      return(NULL) 
    
    tmp_audio <- readWave(file.path(getwd(), "www", input$file1))
    #setWavPlayer("C:/Program Files/Windows Media Player/wmplayer.exe")
    if(!is.null(ranges_osc$x) | !is.null(ranges_spec$x)){
      #time crop
      if(!is.null(ranges_osc$x))
        tc <- ranges_osc$x
      if(!is.null(ranges_spec$x))
        tc <- ranges_spec$x
      tmp_audio <- extractWave(tmp_audio, from = tc[1], to = tc[2], xunit = "time")
    }
    #TODO: Only play zoomed spectrogram/oscillogram times/frequencies
    
    #based on torchaudio::functional_gain
    audio_gain <- function (waveform, gain_db = 0){
      ratio <- 10^(gain_db/20)
      return(waveform * ratio)
    }
    submean <- function(x) x - mean(x)
    tmp_audio@left <- submean(tmp_audio@left)
    tmp_audio      <- audio_gain(tmp_audio, input$db_gain)
    #anything outside the range [-32768, 32767] will be rounded
    tmp_audio@left[tmp_audio@left > 32767]  <- 32767
    tmp_audio@left[tmp_audio@left < -32768] <- -32768
    tmp_audio@left <- as.integer(tmp_audio@left)
    #don't need this if we can fix y axis alignment problem
    #tmp_audio <- normalize(tmp_audio, "16")
    writeWave(tmp_audio, audio_path())
    
    return(tmp_audio)
  })
  
  specData <- reactive({
    if(.is_null(input$file1))     
      return(NULL)
    tmp_audio <- audioInput()
    
    noisered <- switch(input$noisereduction,
                       None    = NULL,
                       Rows    = 1,
                       Columns = 2)
    
    spec <- spectro(tmp_audio,
                    f        = tmp_audio@samp.rate, 
                    wl       = params$window_width, 
                    ovlp     = params$fft_overlap, 
                    fastdisp = TRUE,
                    plot     = FALSE,
                    db       = NULL,
                    noisereduction = noisered)
    
    spec$amp <- spec$amp + input$db_gain
    spec$amp <- spec$amp + input$db_contrast
    
    df   <- data.frame(time      = rep(spec$time, each  = nrow(spec$amp)), 
                       frequency = rep(spec$freq, times = ncol(spec$amp)), 
                       amplitude = as.vector(spec$amp))
    #browser()
    if(!is.null(ranges_osc$x))
      df$time <- df$time + ranges_osc$x[1]
    if(!is.null(ranges_spec$x))
      df$time <- df$time + ranges_spec$x[1]
    
    #spec_zoomed <- spec
    
    #if(!is.null(ranges_spec$x) || !is.null(ranges_osc$x)){
    #  complex_spec <- spectro(tmp_audio,
    #                          f        = tmp_audio@samp.rate, 
    #                          wl       = params$window_width, 
    #                          ovlp     = params$fft_overlap, 
    #                          #fastdisp = TRUE,
    #                          plot     = FALSE,
    #                          db       = NULL,
    #                          complex  = FALSE,
    #                          noisereduction = noisered)
    #  #get a (complex) zero matrix and paste in the zoomed area we're keeping
      #can chop off time axis
    #}
    #in_range <- function(vec, range2) return(vec >= range2[1] & vec <= range2[2])
    #if(!is.null(ranges_spec$x))
    #  df[!(in_range(df$time, ranges_spec$x) & in_range(df$frequency, ranges_spec$y)),3] <- min(df$amplitude)
    #else if(!is.null(ranges_osc$x))
    #  df[!in_range(df$time, ranges_osc$x),3] <- min(df$amplitude)
    
    write.csv(df, 'tmp_spec.csv', row.names = FALSE)
    return(df)
  })
  
  oscData <- reactive(({
    if(.is_null(input$file1))     
      return(NULL)
    tmp_audio <- audioInput()
    df2 <- data.frame(time      = seq(0, length(tmp_audio@left)/tmp_audio@samp.rate, length.out = length(tmp_audio)),
                      amplitude = tmp_audio@left - mean(tmp_audio@left))
    if(!is.null(ranges_osc$x))
      df2$time <- df2$time + ranges_osc$x[1]
    if(!is.null(ranges_spec$x))
      df2$time <- df2$time + ranges_spec$x[1]
    return(df2)
  }))
  
  output$specplot <- renderPlot({
    if(.is_null(input$file1)){
      df <- data.frame(time      = 1,
                       frequency = 1:10,
                       amplitude = rep(-96,10))
      return(plot_spectrogram(df, input))
    }     
    
    p <- plot_spectrogram(specData(), input)
    if(!is.null(ranges_spec$y))
      p <- p + coord_cartesian(ylim = ranges_spec$y, expand = FALSE)
    #else if(!is.null(ranges_osc$x))
    #  p <- p + coord_cartesian(xlim = ranges_osc$x, expand = FALSE)
    #if(!is.null(input$specplot_brush)){
    #  res <- brushedPoints(df, input$specplot_brush,
    #                       xvar = 'time', yvar = 'frequency')
    #  spec_plot <- spec_plot + geom_raster(data = res, fill = 'green')
    #}
    return(p)
  })
  
  output$oscplot <- renderPlot({
    if(.is_null(input$file1))     
      return(plot_oscillogram(NULL))
    
    p <- plot_oscillogram(oscData())
    if(!is.null(ranges_spec$x))
      p <- p + coord_cartesian(xlim = ranges_spec$x, expand = TRUE)
    else if(!is.null(ranges_osc$x))
      p <- p + coord_cartesian(xlim = ranges_osc$x, expand = TRUE)
    return(p)
  })
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$specplot_dblclick, {
    brush <- input$specplot_brush
    if (!is.null(brush)) {
      ranges_spec$x <- c(brush$xmin, brush$xmax)
      ranges_spec$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges_spec$x <- NULL
      ranges_spec$y <- NULL
    }
  })
  
  observeEvent(input$oscplot_dblclick, {
    brush <- input$oscplot_brush
    if (!is.null(brush))
      ranges_osc$x <- c(brush$xmin, brush$xmax)
    else
      ranges_osc$x <- NULL
  })
  
  observeEvent(input$plt_reset, {
    ranges_spec$x <- NULL
    ranges_spec$y <- NULL
    ranges_osc$x  <- NULL
  })
  
  observeEvent(input$specplot_brush, {
    enable("save_points")
  })
  
  observeEvent(input$addCategory, {
    if(input$label_points == "Other"){
      updatedValues <- c(classes, input$otherCategory)
      updateRadioButtons(inputId = "label_points", choices = updatedValues)
    }
  })
  
  observeEvent(input$resetCategory, {
    updateRadioButtons(inputId = "label_points", choices = classes)
  })
  
  observeEvent(input$save_points, {
    #get x and y coordinates with max and min of brushedPoints()
    res <- brushedPoints(specData(), input$specplot_brush,
                         xvar = 'time', yvar = 'frequency')
    if (!is.null(input$specplot_brush)) {
      lab_df <- data.frame(date_time   = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                           file_name   = input$file1,
                           start_time  = min(res$time),
                           end_time    = max(res$time),
                           start_freq  = min(res$frequency),
                           end_freq    = max(res$frequency),
                           class_label = input$label_points,
                           labeler    = Sys.info()[["user"]])
      file_name <- "tmp_labels.csv"
      if(file.exists(file_name))
        write.table(lab_df, file_name, append = TRUE,  col.names = FALSE, sep=",", row.names = FALSE)
      else
        write.table(lab_df, file_name, append = FALSE,  col.names = TRUE, sep=",", row.names = FALSE)
    }
  })
  
  output$my_audio <- renderUI({
    file_name   <- audio_path()
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
  
  # move to previous file (resetting zoom)
  observeEvent(input$prev_file, {
    idx <- which(input$file1 == file_list) - 1
    if(idx == 0)
      idx <- length(file_list)
    ranges_spec$x <- NULL
    ranges_spec$y <- NULL
    ranges_osc$x  <- NULL
    updateSelectInput(inputId  = "file1",
                      choices  = file_list,
                      selected = file_list[idx])
  })
  
  # move to next file (resetting zoom)
  observeEvent(input$next_file, {
    idx <- which(input$file1 == file_list) + 1
    if(idx > length(file_list))
      idx <- 1
    ranges_spec$x <- NULL
    ranges_spec$y <- NULL
    ranges_osc$x  <- NULL
    updateSelectInput(inputId  = "file1",
                      choices  = file_list,
                      selected = file_list[idx])
  })
}

#profvis(runApp(), prof_output = file.path(getwd(),'profiling'))

shinyApp(ui_func(), server)
