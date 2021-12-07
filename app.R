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
               hover  = hoverOpts(id = "plot1_hover"),
               brush  = brushOpts(
                 id   = "plot1_brush", 
                 clip = FALSE
               )),
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
           radioButtons("label_points", "Label Selection:", 
                        choices = classes,
                        #TODO: get classes from other file
                        inline = TRUE),
           br(),
           disabled(actionButton("save_points", "Save Selection"))
    ),
    column(6,
           uiOutput('my_audio')
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
    setWavPlayer("C:/Program Files/Windows Media Player/wmplayer.exe")
    writeWave(tmp_audio, 'www/tmp.wav')
    return(tmp_audio)
  })
  
  specData <- reactive({
    if(is.null(input$file1))     
      return(NULL)
    tmp_audio <- audioInput()
    spec <- spectro(tmp_audio, tmp_audio@samp.rate, 
                    wl       = params$window_width, 
                    ovlp     = params$fft_overlap, 
                    fastdisp = TRUE,
                    plot     = FALSE)
    #try to add noisereduction (optional possibly?)
    df   <- data.frame(time      = rep(spec$time, each  = nrow(spec$amp)), 
                       frequency = rep(spec$freq, times = ncol(spec$amp)), 
                       amplitude = as.vector(spec$amp))
    
    write.csv(df, 'tmp_df.csv')
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
    
    p1_widths(p1$widths)
    grid.draw(p1)
  })
  
  output$plot2 <- renderPlot({
    if(is.null(input$file1))     
      return(NULL)
    
    p2 <- plot_oscillogram(oscData())
    p2$widths <- p1_widths()
    grid.draw(p2)
  })
  
  observeEvent(input$plot1_brush, {
    enable("save_points")
  })
  
  observeEvent(input$save_points, {
    brush <- input$plot1_brush
    
    max_time = 1
    max_freq = 1
    
    #get x and y cooridnates with max and min of brushedPoints()
    
    res <- brushedPoints(specData(), input$plot1_brush, #allRows = TRUE,
                         xvar = 'time', yvar = 'frequency')
    
    #sel_rows <- specData()[res$selected_,]
    #browser()
    if (!is.null(brush)) {
      lab_df <- data.frame(date_time   = format(Sys.time(), "%d-%b-%Y %H:%M:%s"),
                           file_name   = input$file1$name,
                           start_time  = min(res$time),#max_time*brush$xmin, 
                           end_time    = max(res$time),#max_time*brush$xmax, 
                           start_freq  = min(res$frequency),#max_freq*brush$ymin, 
                           end_freq    = max(res$frequency),#max_freq*brush$ymax,
                           class_label = input$label_points)
      file_name <- "tmp.csv"
      if(file.exists(file_name))
        write.table(lab_df, file_name, append = TRUE,  col.names = FALSE, sep=",", row.names = FALSE)
      else
        write.table(lab_df, file_name, append = FALSE,  col.names = TRUE, sep=",", row.names = FALSE)
      
    }
  })
  
  output$my_audio <- renderUI({
    if(is.null(input$file1))     
      return(NULL)
    tranquil <- "#E0FEFE"
    tags$audio(id       = 'my_audio_player',
               src      = markdown:::.b64EncodeFile('www/tmp.wav'), 
               type     = "audio/wav", 
               autoplay = NA, 
               controls = 'controls',
               style    = HTML("color: tranquil;
                                download: none;
                                color: yellow;
                                width: 250px")
               )
    })
}


shinyApp(ui_func(), server)
