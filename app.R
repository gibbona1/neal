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
library(profvis) #for checking code performance

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
    plotOutput("plot1",
               height = 250, #width = 800,
               click  = "plot1_click",
               hover  = "plot1_hover",
               brush  = "plot1_brush"),
  ),
  fluidRow(
    plotOutput("plot2",
               height = 110, #width = 800,
               click  = "plot2_click",
               hover  = hoverOpts(id = "plot_hover"),
               brush  = brushOpts(
                 id = "plot2_brush"
               )),
  ),
  fluidRow(
    column(3,
           div(h4("Select a song"), style = "color: black;",
               fileInput("file1","Choose File", 
                         multiple = F, 
                         accept   = "audio/*"))
    ),
    column(3,
           div(h4("Labelling"), style = "color: black;",
           radioButtons("label_points", "Label Selection:", 
                        choices = classes,
                        #TODO: get classes from other file
                        #inline = TRUE
                        ),
           #br(),
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
                    plot     = FALSE)
    #try to add noisereduction (optional possibly?)
    df   <- data.frame(time      = rep(spec$time, each  = nrow(spec$amp)), 
                       frequency = rep(spec$freq, times = ncol(spec$amp)), 
                       amplitude = as.vector(spec$amp))
    
    write.csv(df, 'tmp_spec.csv')
    return(df)
  })
  
  oscData <- reactive(({
    if(is.null(input$file1))     
      return(NULL)
    tmp_audio <- audioInput()
    df2 <- data.frame(time    = seq(0, length(tmp_audio@left)/tmp_audio@samp.rate, length.out = length(tmp_audio)),
                      amplitude = tmp_audio@left - mean(tmp_audio@left))
    return(df2)
  }))
  
  p1_widths <- reactiveVal(value = NULL)
  
  output$plot1 <- renderPlot({
    if(is.null(input$file1))     
      return(NULL)
    
    p1 <- plot_spectrogram(specData(), input)
    
    #p1_widths(p1$widths)
    #grid.draw(p1)
    p1
  })
  
  output$plot2 <- renderPlot({
    if(is.null(input$file1))     
      return(NULL)
    
    p2 <- plot_oscillogram(oscData())
    #p2$widths <- p1_widths()
    #grid.draw(p2)
    p2
  })
  
  observeEvent(input$plot1_brush, {
    enable("save_points")
  })
  
  observeEvent(input$save_points, {
    #get x and y cooridnates with max and min of brushedPoints()
    
    res <- brushedPoints(specData(), input$plot1_brush, #allRows = TRUE,
                         xvar = 'time', yvar = 'frequency')
    if (!is.null(input$plot1_brush)) {
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
                        src      = NA, 
                        type     = "audio/wav", 
                        #autoplay = NA,
      ))
    tranquil <- "#E0FEFE"
    tags$audio(id       = 'my_audio_player',
               src      = markdown:::.b64EncodeFile('www/tmp.wav'), 
               type     = "audio/wav", 
               #autoplay = NA, 
               controls = NA,#'controls',
               #controlsList="nodownload",
               style    = HTML("background-color: #007db5;") #width: 300px;
               )
    })
}


#profvis(runApp(), prof_output = file.path(getwd(),'profiling'))

shinyApp(ui_func(), server)
