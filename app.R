library(ggplot2)
library(shiny)
library(shinyjs)
#library(shinyFiles)
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

classes <- c("Eurasian Magpie",
             "European Goldfinch",
             "House Sparrow",
             "Noise",
             "Other Bird")

classes <- sort(classes)

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
      column(6,
      sliderInput("db_gain", "dB Gain:",
                  min = -96, max = 96, value = 0,
                  ticks = FALSE),
      ),
      column(6,
      sliderInput("db_contrast", "Contrast:",
                  min = 0, max = 96, value = 0,
                  ticks = FALSE)
      )
    ),
    fluidRow(
      #TODO: folder input and move between files in it
      column(3,
             div(h4("Select a song"), style = "color: black;",
                 fileInput("file1", "Choose File", 
                           multiple = F, 
                           accept   = "audio/*"))
      ),
      column(3,
             div(h4("Labelling"), style = "color: black;",
             radioButtons("label_points", "Label Selection:", 
                          choices = classes,
                          #TODO: get classes from other file
                          ),
             #TODO: Other info to label/record - 
             ## type of call e.g. alarm call, flight call, flock
             ## naming groups: Order, Family, Genus, Species, Subspecies
             ## name of labeler
             ## altitude of recorder (check if in metadata)
             disabled(actionButton("save_points", "Save Selection"))
             )
      ),
      column(6,
             div(h4("Play audio"), style = "color: black;",
             uiOutput('my_audio')
             )
      )
    )
  )
return(ui)
}

server <- function(input, output) {
  audioInput <- reactive({
    if(is.null(input$file1))     
      return(NULL) 
    
    tmp_audio <- readWave(input$file1$datapath)
    #setWavPlayer("C:/Program Files/Windows Media Player/wmplayer.exe")
    writeWave(tmp_audio, 'www/tmp.wav')
    
    #originally tried method from torchaudio::functional_gain
    #but wasn't making a difference
    audio_gain <- function (waveform, gain_db = 0) {
      return(waveform + gain_db)
    }
    submean <- function(x) x - mean(x)
    tmp_audio@left <- submean(tmp_audio@left)
    tmp_audio <- audio_gain(tmp_audio, input$db_gain)
    tmp_audio <- normalize(tmp_audio, "1")
    return(tmp_audio)
  })
  
  specData <- reactive({
    if(is.null(input$file1))     
      return(NULL)
    tmp_audio <- audioInput()
    spec <- spectro(tmp_audio,
                    f        = tmp_audio@samp.rate, 
                    wl       = params$window_width, 
                    ovlp     = params$fft_overlap, 
                    fastdisp = TRUE,
                    plot     = FALSE,
                    db       = NULL)
    
    spec$amp <- spec$amp + input$db_gain
    spec$amp <- spec$amp + input$db_contrast
    #TODO: try to add optional noise reduction
    df   <- data.frame(time      = rep(spec$time, each  = nrow(spec$amp)), 
                       frequency = rep(spec$freq, times = ncol(spec$amp)), 
                       amplitude = as.vector(spec$amp))
    
    write.csv(df, 'tmp_spec.csv', row.names = FALSE)
    return(df)
  })
  
  oscData <- reactive(({
    if(is.null(input$file1))     
      return(NULL)
    tmp_audio <- audioInput()
    df2 <- data.frame(time      = seq(0, length(tmp_audio@left)/tmp_audio@samp.rate, length.out = length(tmp_audio)),
                      amplitude = tmp_audio@left - mean(tmp_audio@left))
    return(df2)
  }))
  
  p1_widths   <- reactiveVal(value = NULL)
  ranges_spec <- reactiveValues(x = NULL, y = NULL)
  ranges_osc  <- reactiveValues(x = NULL)
  
  output$specplot <- renderPlot({
    if(is.null(input$file1)){
      df <- data.frame(time      = 1,
                       frequency = 1:10,
                       amplitude = rep(-96,10))
      return(plot_spectrogram(df, input))
    }     
    
    p <- plot_spectrogram(specData(), input)
    if(!is.null(ranges_spec$x))
      p <- p + coord_cartesian(xlim = ranges_spec$x, ylim = ranges_spec$y, expand = FALSE)
    else if(!is.null(ranges_osc$x))
      p <- p + coord_cartesian(xlim = ranges_osc$x, expand = FALSE)
    return(p)
  })
  
  output$oscplot <- renderPlot({
    if(is.null(input$file1))     
      return(plot_oscillogram(NULL))
    
    p <- plot_oscillogram(oscData())
    if(!is.null(ranges_spec$x))
      p <- p + coord_cartesian(xlim = ranges_spec$x, expand = FALSE)
    else if(!is.null(ranges_osc$x))
      p <- p + coord_cartesian(xlim = ranges_osc$x, expand = FALSE)
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
  
  observeEvent(input$specplot_brush, {
    enable("save_points")
  })
  
  observeEvent(input$save_points, {
    #get x and y coordinates with max and min of brushedPoints()
    
    res <- brushedPoints(specData(), input$specplot_brush,
                         xvar = 'time', yvar = 'frequency')
    if (!is.null(input$specplot_brush)) {
      lab_df <- data.frame(date_time   = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                           file_name   = input$file1$name,
                           start_time  = min(res$time),
                           end_time    = max(res$time),
                           start_freq  = min(res$frequency),
                           end_freq    = max(res$frequency),
                           class_label = input$label_points)
      file_name <- "tmp_labels.csv"
      if(file.exists(file_name))
        write.table(lab_df, file_name, append = TRUE,  col.names = FALSE, sep=",", row.names = FALSE)
      else
        write.table(lab_df, file_name, append = FALSE,  col.names = TRUE, sep=",", row.names = FALSE)
    }
  })
  
  output$my_audio <- renderUI({
    if(is.null(input$file1))     
      return(tags$audio(id       = 'my_audio_player',
                        src      = "", 
                        type     = "audio/wav", 
                        controls = NA, 
                        autoplay = NA
      ))
    tranquil <- "#E0FEFE"
    tags$audio(id       = 'my_audio_player',
               src      = markdown:::.b64EncodeFile('www/tmp.wav'), 
               type     = "audio/wav", 
               controls = 'controls',
               #TODO: HTML styling (background colour, no download button, playback speed,...)
               style    = HTML("audio {
               background-color: #95B9C7;
                               }")
               )
    })
}

#profvis(runApp(), prof_output = file.path(getwd(),'profiling'))

shinyApp(ui_func(), server)
