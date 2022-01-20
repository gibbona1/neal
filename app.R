library(ggplot2)
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyFiles)
library(shinythemes)
library(tuneR)
library(seewave) # for spectrogram
#library(plotly)
#library(oce)
library(viridis)
library(grid)
#library(gridExtra)
library(cowplot) # to get legend
library(profvis) # for checking code performance
library(dplyr)
library(stringr)

source('plot_helpers.R')
source('spectrogram_params.R')

#TODO: navbarPage() to have distinct pages: label, verify/check, run model
#from https://shiny.rstudio.com/articles/layout-guide.html
#TODO: collapsible sidebar panel to clear up the app
#TODO: color/highlight plot as it plays e.g. blue over red in oscillogram
#TODO: put fft/spectrogram parameters (and any others) in sidebar when made
#TODO: Option for Action Buttons instead of radio buttons for labeling which can be pressed and unpressed if labels want to be removed
#TODO: Seperate button to remove label and Button to undo it
#TODO: Reactive object of classes to add more than one species, option to save list
#TODO: Colour buttons (action or radio) same as bounding boxes (ggplot override aes) (notification label colour too)
#TODO: Label hover click option instead
#TODO: show details of saved labels in list in a sidebar
#TODO: on clicking label in sidebar, highlights or zooms to the label (option to play it)

#change file size to 30MB
options(shiny.maxRequestSize = 30*1024^2)

classes <- c(read.csv("species_list.csv")[,1], "Noise", "Other")

file_list <- list.files('www/')

classes <- sort(classes)

.is_null <- function(x) return(is.null(x) | x == "<NULL>")

ui_func <- function() {
  ui <- fluidPage(
    theme = shinytheme("slate"),
    useShinyjs(),
    htmlOutput("file1"),
    fluidRow(
      div(
        style = "position:relative",
        plotOutput(
          "specplot",
          height   = 250,
          click    = "specplot_click",
          dblclick = "specplot_dblclick",
          hover    = hoverOpts(
            id        = "specplot_hover",
            delay     = 50,
            delayType = "debounce"
          ),
          brush    = brushOpts(
            id         = "specplot_brush",
            resetOnNew = TRUE)
        ),
        uiOutput("hover_info")
      )),
      fluidRow(
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
        uiOutput("hover_info_osc")
      ),
      fluidRow(
        column(
          5,
          sliderInput(
            "db_gain",
            "dB Gain:",
            min = -96,
            max = 96,
            value = 0,
            ticks = FALSE
          ),
        ),
        column(
          5,
          sliderInput(
            "db_contrast",
            "Contrast:",
            min = 0,
            max = 96,
            value = 0,
            ticks = FALSE
          )
        ),
        column(
          2,
          br(),
          fixedRow(
            tags$div(
              style = "display: inline-block; vertical-align:center; horizontal-align:center",
              class = "row-fluid",
              tipify(actionButton("prev_file", "", icon = icon("arrow-left")),  "Prev"),
              disabled(tipify(
                actionButton("prev_section", "", icon = icon("chevron-left")),  "prev section"
              )),
              disabled(tipify(
                actionButton("next_section", "", icon = icon("chevron-right")), "next section"
              )),
              tipify(actionButton("next_file", "", icon = icon("arrow-right")), "Next")
            )
          ),
          div(style = "display:inline-block;width:100%;text-align: center;  vertical-align:center; horizontal-align:center",
              actionButton("plt_reset", "Reset Plot"))
        )
      ),
      fluidRow(
        column(
          5,
          div(
            h4("Select a folder"),
            shinyDirButton('folder',
                           label    = 'Folder select',
                           title    = 'Please select a folder')
          ),
          verbatimTextOutput("folder", placeholder = TRUE),
          br(),
          selectInput(
            "file1",
            "Select File:",
            choices = c("<NULL>", file_list),
            width   = '100%'
          )
        ),
        column(3,
               div(
                 h4("Labeling"),
                 radioButtons("label_points", "Label Selection:",
                              choices = classes),
                 textInput("otherCategory", "Type in additional category"),
                 fixedRow(
                   actionButton("addCategory", "Add category"),
                   actionButton("resetCategory", "Reset categories")
                 ),
                 #TODO: Other info to label/record -
                 ## type of sound e.g. alarm call, flight call, flock
                 ## naming groups: Order, Family, Genus, Species, Subspecies
                 ## altitude of recorder (check if in metadata)
                 actionButton(
                   "save_points", HTML("<b>Save Selection</b>")
                 ),
                 actionButton(
                   "remove_points", HTML("<b>Delete Selection</b>")
                 ),
                 actionButton(
                   "undo_delete_lab", HTML("<b>Undo Deletion</b>")
                 )
               )),
        column(
          4,
          div(
            h4("Play audio"),
            #style = "color: black;",
            uiOutput('my_audio'),
            selectInput(
              "playbackrate",
              "Playback Speed:",
              choices  = paste0(c(0.1, 0.25, 0.5, 1, 2, 5, 10), "x"),
              selected = "1x",
              width    = '100%'
            )
          ),
          selectInput(
            "noisereduction",
            "Spectrogram Noise reduction:",
            choices  = c("None", "Rows", "Columns"),
            selected = "None",
            width    = '100%'
          ),
          selectInput(
            "palette_selected",
            "Spectrogram colour palette:",
            choices = palette_list,
            width   = '100%'
          ),
          checkboxInput("palette_invert", "Invert color palette"),
          checkboxInput("toggle_spec", "Show Spectrogram", value = TRUE),
          checkboxInput("toggle_osc",  "Show Oscillogram", value = TRUE),
          actionButton("savespec", "Save Spectrogram"),
          actionButton("saveosc", "Save Oscilloogram"),
          checkboxInput("include_hover", "Include spectrogram hover tooltip", value = TRUE),
          checkboxInput("include_hover_osc", "Include oscillogram hover tooltip", value = FALSE),
          checkboxInput("spec_labs", "Show spectrogram labels"),
          checkboxInput("osc_labs", "Show oscillogram labels")
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
  
  ranges_spec  <- reactiveValues(x = NULL, y = NULL)
  ranges_osc   <- reactiveValues(x = NULL)
  length_ylabs <- reactiveValues(osc = 4, spec = 0)
  deleted_lab  <- reactiveValues(rows = NULL, data = NULL)
  
  length_b10 <- function(x){
    return(x %>% 
             range %>% 
             as.integer %>% 
             as.character %>% 
             str_length %>% 
             max)
  }
  
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
    #TODO: Only play zoomed spectrogram frequencies 
    #requires working on complex spectrogram before getting modulus
    
    #based on torchaudio::functional_gain
    audio_gain <- function (waveform, gain_db = 0){
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
      return(data.frame(time      = 1,
                        frequency = 1:10,
                        amplitude = rep(-96,10)))
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
  
  oscData <- reactive({
    if(.is_null(input$file1))     
      return(NULL)
    tmp_audio <- audioInput()
    df2 <- data.frame(time      = seq(0, length(tmp_audio@left)/tmp_audio@samp.rate, length.out = length(tmp_audio)),
                      amplitude = tmp_audio@left - mean(tmp_audio@left))
    if(!is.null(ranges_osc$x))
      df2$time <- df2$time + ranges_osc$x[1]
    if(!is.null(ranges_spec$x))
      df2$time <- df2$time + ranges_spec$x[1]
    
    #spacing for "custom" y axis margin
    strlen_osc_y  <- length_b10(df2$amplitude)
    
    spec_freq <- specData()$frequency
    if(!is.null(ranges_spec$y))
      spec_freq <- spec_freq[spec_freq >= ranges_spec$y[1] & spec_freq <= ranges_spec$y[2]]
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
    if(.is_null(input$file1)){
      return(plot_spectrogram(specData(), input, length_ylabs))
    }     
    
    withProgress(message = 'Creating Spectrogram', value = 0.1, {
      p <- plot_spectrogram(specData(), input, length_ylabs)
      incProgress(0.5)
      
      if(!is.null(ranges_spec$y))
        p <- p + coord_cartesian(ylim = ranges_spec$y, expand = FALSE)
      observeEvent(input$savespec, {
        saveplt <- p + 
          theme_void() + 
          theme(legend.position = 'none')
        spec_name <- paste0(gsub('.wav', '', input$file1), '_spec.png')
        file_nm   <- file.path(getwd(), "images", spec_name)
        #ggsave(file_nm, plot = saveplt)
        png(file_nm)
        print(saveplt)
        dev.off()
        showNotification(HTML(paste0("Spectrogram image <b>", spec_name, "</b> saved to <b>images</b>.")), 
                         #TODO: clickable link to images folder
                         #action = a(href = file.path("file://", getwd(), "images"), "Go to folder", target = "_blank"),
                         type = "message")
      })
      incProgress(0.3)
    })
      return(p)
  })
  
  output$oscplot <- renderPlot({
    if(.is_null(input$file1))
      return(plot_oscillogram(NULL, input, length_ylabs))
    
    p <- plot_oscillogram(oscData(), input, length_ylabs)
    if(!is.null(ranges_spec$x))
      p <- p + coord_cartesian(xlim = ranges_spec$x, expand = TRUE)
    else if(!is.null(ranges_osc$x))
      p <- p + coord_cartesian(xlim = ranges_osc$x, expand = TRUE)
    observeEvent(input$saveosc, {
      saveplt <- p + 
        theme_void() + 
        theme(legend.position = 'none',
              plot.background = element_rect(fill = "black"))
      spec_name <- paste0(gsub('.wav', '', input$file1), '_osc.png')
      file_nm <- file.path(getwd(), "images", spec_name)
      #ggsave(file_nm, plot = saveplt)
      png(file_nm)
      print(saveplt)
      dev.off()
      showNotification(HTML(paste0("Oscillogram image <b>", spec_name, "</b> saved to <b>images</b>.")), 
                       #TODO: clickable link to images folder
                       #action = a(href = file.path("file://", getwd(), "images"), "Go to folder", target = "_blank"),
                       type = "message")
    })
    return(p)
  })
  
  output$hover_info <- renderUI({
    if(.is_null(input$file1) | !input$include_hover)
      return(NULL)
    hover <- input$specplot_hover
    point <- nearPoints(specData(), hover, threshold = 5, maxpoints = 1, addDist = TRUE, xvar="time", yvar="frequency")
    if(nrow(point) == 0) 
      return(NULL)
    point <- round(point, 3)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct  <- (hover$domain$top - hover$y)  / (hover$domain$top - hover$domain$bottom)
    
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
      species_in_hover <- paste0("<br/><b> Species: </b>", lab_df$class_label)
    }
    
    # create style property for tooltip
    # background color is set so tooltip is a almost transparent
    # z-index is the stack index and is set to 100 so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; ",
                    "background-color: rgba(120, 120, 120, 0.15); ",
                    "color: rgb(245, 245, 245); padding: 1%;",
                    "left:", 100*left_pct+2, "%; top:", 100*top_pct+2, "%;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Time: </b>", point$time, " seconds<br/>", 
                    "<b> Frequency: </b>", point$frequency, " kHz<br/>",
                    "<b> Amplitude: </b>", point$amplitude, " dB",
                    species_in_hover)))
    )
  })
  
  output$hover_info_osc <- renderUI({
    if(.is_null(input$file1) | !input$include_hover_osc)
      return(NULL)
    hover <- input$oscplot_hover
    point <- nearPoints(oscData(), hover, threshold = 5, maxpoints = 1, addDist = TRUE, xvar="time")
    if(nrow(point) == 0) 
      return(NULL)
    point <- round(point, 3)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct  <- (hover$domain$top - hover$y)  / (hover$domain$top - hover$domain$bottom)
    
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
      species_in_hover <- paste0("<br/><b> Species: </b>", lab_df$class_label)
    }
    
    # create style property for tooltip
    # background color is set so tooltip is a almost transparent
    # z-index is the stack index and is set to 100 so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; ",
                    "background-color: rgba(120, 120, 120, 0.15); ",
                    "color: rgb(245, 245, 245); padding: 1%;",
                    "left:", 100*left_pct+2, "%; top:", 100*top_pct+2, "%;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Time: </b>", point$time, " seconds<br/>", 
                    "<b> Amplitude: </b>", point$amplitude,
                    species_in_hover)))
    )
  })
  
  observeEvent(input$specplot_dblclick, {
    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
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
  
  observeEvent(input$addCategory, {
    if(input$label_points == "Other"){
      updatedValues <- c(classes, input$otherCategory)
      updateRadioButtons(inputId = "label_points", choices = updatedValues, selected = input$otherCategory)
    }
  })
  
  observeEvent(input$resetCategory, {
    updateRadioButtons(inputId = "label_points", choices = classes)
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
      lab_df <- data.frame(date_time   = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                           file_name   = input$file1,
                           start_time  = min(res$time),
                           end_time    = max(res$time),
                           start_freq  = min(res$frequency),
                           end_freq    = max(res$frequency),
                           class_label = input$label_points,
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
    file_name   <-  'www/tmp.wav'
    if(!is.null(audioInput()))
      writeWave(audioInput(), file_name)
    
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
  
  observe({
    shinyjs::toggle(id  = "specplot",
              anim      = TRUE,
              animType  = "fade",
              time      = 0.5,
              condition = input$toggle_spec)
  })
  
  observe({
    shinyjs::toggle(id  = "oscplot",
              anim      = TRUE,
              animType  = "fade",
              time      = 0.5,
              condition = input$toggle_osc)
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
