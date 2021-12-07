library(ggplot2)
library(shiny)
library(shinyjs)
#library(shinyFiles)
#library(sound)
library(tuneR)
library(seewave) # for spectrogram
#library(reticulate)
#library(phonTools)
#library(seewave)
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
ui_func <- function(){
ui <- fluidPage(
  fluidRow(
    plotOutput("plot1",
               height = 250, #width = 800,
               click  = "plot1_click",
               hover  = hoverOpts(id = "plot_hover"),
               brush  = brushOpts(
                 id = "plot1_brush"
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
           div(h4("Play"), style = "color: black;",
               actionButton("play","Play"))
           #uiOutput('my_audio')
    ),
    column(3,
           radioButtons("label_points", "Label Selection:", 
                        choices = c('birdA', 'birdB', 'noise'),
                        #TODO: get classes from other file
                        inline = TRUE)
    ),
    column(3,
           disabled(actionButton("save_points", "Save Selection"))
    )
  )
)
return(ui)
}

server <- function(input, output) {
  # For storing which rows have been excluded
  #asp_ratio <- 3
  
  #df <- data.frame(time = c(1), frequency = c(1), amplitude = c(1))
  
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
    #browser()
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
    
    p1 <- plot_spectrogram(specData())
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
  
  #  gA <- ggplot_gtable(ggplot_build(spec_plot))
  #  gB <- ggplot_gtable(ggplot_build(osc_plot))
  #  maxWidth  <- grid::unit.pmax(gA$widths, gB$widths)
  #  gA$widths <- as.list(maxWidth)
  #  gB$widths <- as.list(maxWidth)
  #  layo <- rbind(c(1,1,1),
  #                c(1,1,1),
  #                c(1,1,1),
  #                c(2,2,2),
  #                c(2,2,2))
  #  
  #  grid.newpage()
  #  grid.arrange(gA, gB, layout_matrix = layo)
  #  #return(spec_plot)
  #  
  #})#, height = 300, width = 900)
  
  #observeEvent(input$plot1_click, {
  #  res <- nearPoints(df, input$plot1_click, 
  #                    allRows = TRUE)
  #  
  #  vals$keeprows <- xor(vals$keeprows, res$selected_)
  #})
  
  # Toggle points that are brushed, when button is clicked
  #observeEvent(input$exclude_toggle, {
  #  res <- brushedPoints(df, input$plot1_brush, allRows = TRUE)
  #  
  #  vals$keeprows <- xor(vals$keeprows, res$selected_)
  #})
  
  # Reset all points
  #observeEvent(input$exclude_reset, {
  #  vals$keeprows <- rep(TRUE, nrow(df))
  #})
  
  observeEvent(input$plot1_brush, {
    enable("save_points")
  })
  
  observeEvent(input$save_points, {
    brush <- input$plot1_brush
    #browser()
    max_time = 1
    max_freq = 1
    #browser()
    #get x and y cooridnates with max and min of nearPoints()
    
    res <- brushedPoints(specData(), input$plot1_brush, allRows = TRUE,
                         xvar = 'time', yvar = 'frequency')
    #browser()
    sel_rows <- res[res$selected_]
    if (!is.null(brush)) {
      lab_df <- data.frame(date_time   = format(Sys.time(), "%d-%b-%Y %H:%M"),
                           file_name   = input$file1$name,
                           start_time  = min(sel_rows$time),#max_time*brush$xmin, 
                           end_time    = max(sel_rows$time),#max_time*brush$xmax, 
                           start_freq  = min(sel_rows$freqiency),#max_freq*brush$ymin, 
                           end_freq    = max(sel_rows$freqiency),#max_freq*brush$ymax,
                           class_label = input$save_points)
      file_name <- "tmp.csv"
      if(file.exists(file_name))
        write.table(lab_df, file_name, append = TRUE,  col.names = FALSE, sep=",", row.names = FALSE)
      else
        write.table(lab_df, file_name, append = FALSE,  col.names = TRUE, sep=",", row.names = FALSE)
      
    }
  })
  
  observeEvent(input$play, {
    insertUI(selector = "#play",
             ui       = tags$audio(src      = input$file1$datapath, 
                                   type     = "audio/wav", 
                                   autoplay = NA, 
                                   controls = NA))#"controls", 
                                   #style    = "display:none;"))
  })
}

shinyApp(ui_func(), server)
