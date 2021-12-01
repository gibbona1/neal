library(ggplot2)
library(shiny)
library(shinyjs)
library(shinyFiles)
#install.packages("shinyFiles")
#library(sound)
library(tuneR)
library(seewave)
library(reticulate)
#library(phonTools)
#library(seewave)
library(plotly)
library(oce)
#install.packages('viridis')
library(viridis)
library(grid)
library(gridExtra)
library(cowplot)
#install.packages('cowplot')
source('plot_helpers.R')
source('spectrogram_params.R')

mydir <- getwd()
#setwd('C:/Users/Anthony/OneDrive - Maynooth University/Documents/GitHub/batdetect/bat_train')
#data_set_params <- import('data_set_params')
#params <- data_set_params$DataSetParams()
#params$window_width


#tf <- import('tensorflow')
#np <- import('numpy')

#setwd('C:/Users/Anthony/OneDrive - Maynooth University/
#Documents/GitHub/batdetect/bat_train')
#helper_fns <- import('helper_fns')
#setwd(mydir)


#wavfile <- import('scipy.io.wavfile')

folder_path <- 'www'
#folder_path <- 'C:\\Users\\Anthony\\OneDrive - Maynooth University\\
#November_test_audio_work\\November_test_samples\\Aoibheann'

files       <- list.files(folder_path)

tmp_file    <- files[1]
tmp_path    <- paste0(folder_path, '\\', tmp_file)

tmp_audio <- readWave(tmp_path)
tmp_audio

# ie 1024/44100.0 about 23 msecs.

1024/24000

spec <- spectro(tmp_audio, tmp_audio@samp.rate, 
                #wl       = params$window_width, 
                ovlp     = params$fft_overlap, 
                fastdisp = TRUE,
                plot     = FALSE)
#params$window_width

#tmp_audio@samp.rate

#tmp_list <- wavfile$read(tmp_path)

#sampling_rate <- tmp_list[[1]]
#audio_samples <- tmp_list[[2]]

#plot(spectrogram)
#change file size to 30MB
options(shiny.maxRequestSize=30*1024^2)

ui <- fluidPage(
  fluidRow(
    plotOutput("plot1",
               #height = 200,
               click  = "plot1_click",
               brush  = brushOpts(
                 id = "plot1_brush"
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
                        inline = TRUE)
    ),
    column(3,
           disabled(actionButton("save_points", "Save Selection"))
    )
  )
)

server <- function(input, output) {
  # For storing which rows have been excluded
  #asp_ratio <- 3
  
  #df <- data.frame(time = c(1), frequency = c(1), amplitude = c(1))
  
  output$plot1 <- renderPlot({
    if(is.null(input$file1))     
      return(NULL) 
    
    #tmp_list = wavfile$read(input$file1$datapath)
    #sampling_rate = tmp_list[[1]]
    #audio_samples = tmp_list[[2]]
    #wavfile$write('tmp_wav', sampling_rate, audio_samples)
    
    tmp_audio <- readWave(input$file1$datapath)
    
    setWavPlayer("C:/Program Files/Windows Media Player/wmplayer.exe")
    writeWave(tmp_audio, 'tmp.wav')#, extensible=FALSE)
    
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
    
    vals <- reactiveValues(
      keeprows = rep(TRUE, nrow(df))
    )
    
    write.csv(df, 'tmp_df.csv')
    #spec_dims <- dim(spec$amp)
    #asp_ratio <- spec_dims[2]/spec_dims[1]
    
    spec_plot <- ggplot(df, aes_string(x = 'time',
                                       y = 'frequency', 
                                       z = 'amplitude')) + 
      geom_raster(aes(fill = amplitude), 
                  #alpha = (0.5 + 0.5*vals$keeprows), 
                  interpolate = TRUE) +
      xlab("Time (s)") + 
      ylab("Frequency (kHz)") + 
      scale_fill_viridis("Amplitude\n(dB)\n") +
      hot_theme_grid
    
    df2 <- data.frame(time    = seq(0, length(tmp_audio@left)/tmp_audio@samp.rate, length.out = length(tmp_audio)),
                      amplitude = tmp_audio@left - mean(tmp_audio@left))
    
    osc_plot <- ggplot(df2)+
      geom_line(mapping = aes(x=time, y=amplitude), color="red")+ 
      #scale_x_continuous(labels=s_formatter, expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))+
      xlab("Time (s)") + 
      geom_hline(yintercept = 0, color="white", linetype = "dotted")+
      oscillo_theme_dark
    
    spec_legend <- get_legend(spec_plot)
    osc_legend  <- get_legend(osc_plot)
    
    spec_plot <- spec_plot + theme(legend.position='none')
    osc_plot  <- osc_plot  + theme(legend.position='none')
    
    #plot_grid(plot_grid(spec_plot, spec_legend, align="h", rel_widths = c(1,0.1)),
    #          plot_grid(osc_plot,  osc_legend, rel_widths = c(1,0.1)), nrow=2, rel_heights = c(1,0.4))
    
    gA=ggplot_gtable(ggplot_build(spec_plot))
    gB=ggplot_gtable(ggplot_build(osc_plot))
    maxWidth = grid::unit.pmax(gA$widths, gB$widths)
    gA$widths <- as.list(maxWidth)
    gB$widths <- as.list(maxWidth)
    layo <- rbind(c(1,1,1),
                  c(1,1,1),
                  c(1,1,1),
                  c(2,2,2),
                  c(2,2,2))
    
    grid.newpage()
    grid.arrange(gA, gB, layout_matrix = layo)
    
  }, height = 300, width = 900)
  
  # Toggle points that are clicked
  #browser
  #df <- read.csv('tmp_df.csv')
  
  #observeEvent(input$plot1_click, {
  #  res <- nearPoints(df, input$plot1_click, 
  #                    allRows = TRUE)
  #  
  #  vals$keeprows <- xor(vals$keeprows, res$selected_)
  #})
  
  # Toggle points that are brushed, when button is clicked
  observeEvent(input$exclude_toggle, {
    res <- brushedPoints(df, input$plot1_brush, allRows = TRUE)
    
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Reset all points
  observeEvent(input$exclude_reset, {
    vals$keeprows <- rep(TRUE, nrow(df))
  })
  
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
    if (!is.null(brush)) {
      lab_df <- data.frame(date_time   = format(Sys.time(), "%d-%b-%Y %H:%M"),
                           file_name   = input$file1$name,
                           start_time  = max_time*brush$xmin, 
                           end_time    = max_time*brush$xmax, 
                           start_freq  = max_freq*brush$ymin, 
                           end_freq    = max_freq*brush$ymax,
                           class_label = input$save_points)
      file_name <- "tmp.csv"
      if(file.exists(file_name))
        write.table(lab_df, file_name, append = TRUE,  col.names = FALSE, sep=",", row.names = FALSE)
      else
        write.table(lab_df, file_name, append = FALSE,  col.names = TRUE, sep=",", row.names = FALSE)
      
    }
  })
  
  #data <- reactive({
  #  brushedPoints(df, input$plot1_brush, xvar = "time", yvar = "frequency")
  #})
  
  #output$info <- renderPrint({data()})
  
  #observeEvent(input$save_points, {
  #  write.csv(data(), 'brushed_data.csv', row.names = FALSE)
  #})
  
  eventReactive(input$play, {
    insertUI(selector = "#play",
             ui       = tags$audio(src      = markdown:::.b64EncodeFile('tmp.wav'), 
                                   type     = "audio/wav", 
                                   autoplay = TRUE, 
                                   controls = "controls", 
                                   style    = "display:none;"))
  })
}

shinyApp(ui, server)
