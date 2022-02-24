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
#TODO: Save list of added species as col in species csv (or append to a column) 
#TODO: (notification with label colour)
#TODO: Label hover click option instead
#TODO: Show details of saved labels in list in a sidebar
#TODO: On clicking label in sidebar, highlights the label (option to play it)
#TODO: Unit tests (especially for plots)
#TODO: Check soundgen pitch app https://github.com/tatters/soundgen
#TODO: Example sound files in right sidebar 
# (https://birdwatchireland.ie/our-work/surveys-research/research-surveys/
#countryside-bird-survey/cbs-bird-songs-and-calls/)
#TODO: Mel scale
#TODO: gridExtra blank plot with correct axes, paste spec as image (not raster)

#change max supported audio file size to 30MB
options(shiny.maxRequestSize = 30*1024^2)

file_list <- list.files('www/')
file_list <- file_list[!stringr::str_starts(file_list, "tmp")]

species_list <- read.csv("species_list.csv", fileEncoding = 'UTF-8-BOM')
call_types   <- c("song", "alarm call", "flight call", "flock")

playback_vals <- c(0.1, 0.25, 0.5, 1, 2, 5, 10)
names(playback_vals) <- paste0(playback_vals, "x")

.is_null <- function(x) return(is.null(x) | x == "<NULL>")

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
file_btn_style <- 'padding:1%; width:100%;'

ui_func <- function() {
    header <- {dashboardHeader(
      title = "Audio Labeler App",
      tags$li(class = "dropdown",
              tags$li(class = "dropdown", uiOutput("start_ui"), 
                      style = "padding-top: 5%; 
                               padding-bottom: 5%; 
                               vertical-align: center;")
              ),
      dropdownMenu(
        type = "notifications", 
        icon = icon("question-circle"),
        badgeStatus = NULL,
        headerText  = "Links:",
        
        notificationItem("GitHub", icon = icon("github"), status = "info",
                         href = "https://github.com/gibbona1/audio_labeler")
      )
      )}
    
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
      menuItem("Spectrogram Settings", 
               tabName = "spec_menu", 
               icon    = icon("chart-area"),
        selectInput("freq_min", "minimum frequency in filter", 
                    choices = c(0, 2^(3:7)), selected = 0),
        selectInput("freq_max", "maximum frequency in filter", 
                    choices = 2^(4:9), selected = 32),
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
        checkboxInput("include_hover", "Include spectrogram hover tooltip", 
                      value = TRUE),
        checkboxInput("spec_labs", "Show spectrogram labels", value = TRUE),
        uiOutput("spec_collapse")
      ),
      menuItem("FFT Settings", tabName = "fft_menu", icon = icon("barcode"),
        numericInput('window_width', 'Window Size (number of points)', 
                     value = 256),
        numericInput('fft_overlap', 'FFT Overlap (%)', 
                     value = 75, min = 0, max = 99, step = 1),
        numericInput('window_width_disp', 'Window Size for display spectrogram', 
                     value = 1024),
        numericInput('fft_overlap_disp', 'FFT Overlap for display spectrogram', 
                     value = 15, min = 0, max = 99, step = 1)
      ),
      menuItem("Oscillogram Settings", 
               tabName = "osc_menu", icon = icon('chart-line'),
        actionButton("saveosc", "Save Oscilloogram"),
        checkboxInput("include_hover_osc", "Include oscillogram hover tooltip", 
                      value = FALSE),
        checkboxInput("osc_labs", "Show oscillogram labels"),
        checkboxInput("include_osc", "Show Oscillogram", value = FALSE),
        uiOutput("osc_collapse")
      ),
      menuItem("Other Settings", tabName = "other_menu", icon = icon("cog"),
        numericInput('label_columns', 'Number of Columns', 
                     value = 5, min = 1, max = 9, step = 1)
      )
      ),
      #Options for sidebar
      collapsed = TRUE)}
    
    body <- {dashboardBody(
      theme = "blue_gradient",
      useShinyjs(),
      tags$style(".content-wrapper{margin-left: 0px;}"),
      tags$head(tags$style(HTML(".content {padding-top: 0;}"))),
      htmlOutput("file1"),
      #Spectrogram Plot
      fluidRow({
        div(
          plotOutput(
            "specplot",
            height   = 300,
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
          #plotOutput(
          #  "specplot_blank",
          #  height   = 25,
          #  ),
          tags$head(tags$style('
          #hover_info {
            position: absolute;
            width: 200px;
            height: 30px;
            z-index: 100;
           }
        ')),
        tags$script(HTML('
          $(document).ready(function(){
            // id of the plot
            $("#specplot").mousemove(function(e){ 
              var hover     = $("#hover_info");
              var winwidth  = $( window ).width();
              var winheight = $( window ).height();
              
              var body = document.body,
                  html = document.documentElement;

              var height = Math.max( body.scrollHeight, 
                                     body.offsetHeight,
                                     html.clientHeight, 
                                     html.scrollHeight, 
                                     html.offsetHeight );
                                     
              hover.attr("style", "");
              //stop hover info going off edge of screen
              if(e.pageX + hover.width() <= $( window ).width()) {
                hover.css({"left": (e.pageX + 5) + "px"});
              } 
              else {
                hover.css({"right": (winwidth 
                                     - hover.width()/4
                                     - e.pageX - 5) + "px"});
              }
              if(e.pageY + hover.height() <= $(this).height()) {
                hover.css({"top": (e.pageY + 5) + "px"});
              } 
              else {
                hover.css({"bottom": (height 
                                      + $(this).offset().top
                                      - hover.height()
                                      - e.pageY - 5) + "px"});
              }
              hover.show();
            });     
          });
        ')),
          uiOutput("hover_info")
          )
      }),
      #Oscillogram Plot
      fluidRow({
        uiOutput("oscplot_ui")
        }),
      #One row of audio settings
      fluidRow({
        div(
        column(4,{
          uiOutput("freq_ui")
               }),
        column(4,{
               div(
                 uiOutput('audio_title'),
                 uiOutput('my_audio')#,
                 #tags$script('
                 #var id = setInterval(audio_pos, 100);
                 #function audio_pos() {
                 #var audio = document.getElementById("my_audio_player");
                 #var curtime = audio.currentTime;
                 #console.log(audio);
                 #Shiny.onInputChange("get_time", curtime);
                 #};'),
                 #actionButton("get_time", "Get Time", onclick = js),
                 #verbatimTextOutput("audio_time")
                 )
          }),
        column(2,{
                 selectInput(
                   "playbackrate",
                   "Playback Speed:",
                   choices  = playback_vals,
                   selected = 1,
                   width    = '100%'
                 )
                 }),
        column(2,{
          fixedRow(style = btn_row_style,
             div(column(3, style = "padding:0px;",
                        tipify(
                          actionButton("prev_file", "", 
                                       icon  = icon("arrow-left"), 
                                       style = file_btn_style),
                          "Previous File"
                        ),
             ), 
             column(3, style = "padding:0px;",
                    tipify(
                      actionButton("prev_section", "", 
                                   icon  = icon("chevron-left"), 
                                   style = file_btn_style),
                      "previous section"
                    )
             ), 
             column(3, style = "padding:0px;",
                    tipify(
                      actionButton("next_section", "", 
                                   icon  = icon("chevron-right"), 
                                   style = file_btn_style),
                      "next section"
                    )
             ), 
             column(3, style = "padding:0px;",
                    tipify(actionButton("next_file", "", 
                                        icon  = icon("arrow-right"), 
                                        style = file_btn_style), 
                           "Next File")
             )
             ),
             fluidRow(style = btn_row_style,
                     actionButton("plt_reset", "Reset Plot", 
                                  style = file_btn_style)
            )
          )
            })
        )
        }),
      #Labelling
      fluidPage({
        div(uiOutput("label_ui"),
            column(6, 
              fluidRow(style = btn_sel_style,
              textInput("otherCategory", "Type in additional category:", 
                        width = "100%")
              ),
              #br(),
              fixedRow(style = btn_row_style,
                actionButton("addCategory", 
                             HTML("<b>Add category</b>"), 
                             style = "width: 60%;"),
                actionButton("remCategory", 
                             HTML("<b>Remove category</b>"), 
                             style = "width: 60%;"),
                actionButton("resetCategory", 
                             HTML("<b>Reset categories</b>"), 
                             style = "width: 60%;")
                )
              ),
            column(6,
            fluidRow(style = btn_sel_style,
                     selectInput(
                       inputId    = "call_type", 
                       label      = "Call Type:", 
                       width      = '100%',
                       multiple   = TRUE,
                       choices    = c("<NULL>", call_types), 
                       selected   = "<NULL>")
                     ),
            #br(),
            #TODO: Other info to label/record -
            ## altitude of recorder (check if in metadata)
            fluidRow(style = btn_row_style,
              actionButton("save_points", 
                           HTML("<b>Save Selection</b>"), 
                           style = "width: 60%;"),
              actionButton("remove_points", 
                           HTML("<b>Delete Selection</b>"), 
                           style = "width: 60%;"),
              actionButton("undo_delete_lab", 
                           HTML("<b>Undo Deletion</b>"), 
                           style = "width: 60%;")
              )
            )
          )
        }),
      #more
      fluidRow({
        div(
          column(6, 
                 textAreaInput("notes", "Additional Notes:", width = "100%")
          ),
          column(4, offset = 2,
                 sliderInput("label_confidence", "Label Confidence:", 
                             width = "100%",
                             min   = 0,
                             max   = 1,
                             step  = 0.05,
                             value = 1,
                             ticks = FALSE),
                 div(div(style='float:left;', 'low'), 
                     div(style='float:right;', 'high')),
          )
        )
      })
    )}
  ui <- dashboardPage(
    header,
    sidebar,
    body
  )
  return(ui)
}

server <- function(input, output, session) {
  volumes <- c(Home = fs::path_home(), 
               "R Installation" = R.home(), 
               getVolumes()())
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
  
  observeEvent(eventExpr = {input$folder}, handlerExpr = {
                 if (!"path" %in% names(dir())) return()
                 home <- normalizePath("~")
                 global$datapath <-
                   file.path(home, paste(unlist(dir()$path[-1]), 
                                         collapse = .Platform$file.sep))
               })
  
  ranges_spec    <- reactiveValues(x = NULL, y = NULL)
  ranges_osc     <- reactiveValues(x = NULL)
  dc_ranges_spec <- reactiveValues(x = NULL, y = NULL)
  dc_ranges_osc  <- reactiveValues(x = NULL)
  length_ylabs   <- reactiveValues(osc  = 4,    spec = 0)
  deleted_lab    <- reactiveValues(rows = NULL, data = NULL)
  plots_open     <- reactiveValues(osc  = TRUE, spec = TRUE)
  x_coords       <- reactiveVal(NULL)
  segment_num    <- reactiveVal(1)
  segment_total  <- reactiveVal(1)
  segment_end_s  <- reactiveVal(1)
  segment_start  <- reactiveVal(0)
  
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
        plot.background = element_rect(fill = rgb(0.15, 0.17, 0.19)),
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
  
  reset_ranges <- function(x_list){
    for(nm in names(x_list))
      x_list[[nm]] <- NULL
  }
  
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
  
  cat_colours <- function(x){
    cat_df <- data.frame(type = c(rep("green", length(categories$base)),
                                  rep("orange", length(categories$misc)),
                                  rep("red", length(categories$xtra))),
                         category = class_label())
    merge_df <- merge(x, cat_df, by.x = "class_label", by.y = "category", sort = FALSE)
    return(merge_df$type)
  }
  
  categories <- reactiveValues(
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
  
  labs_filename <- reactive({"tmp_labels.csv"})
  
  fullData   <- reactiveVal(NULL)
  
  write_labs <- function(lab_df, 
                         fname     = labs_filename(), 
                         append    = TRUE, 
                         col.names = FALSE){
    write.table(lab_df, fname, 
                append    = append,  
                col.names = col.names, 
                sep       = ",", 
                row.names = FALSE)
  }
  
  observeEvent(input$file1, {
    if(.is_null(input$file1)){
      disable("prev_file")
      disable("next_file")
      disable("prev_section")
      disable("next_section")
    } else {
      idx <- which(input$file1 == file_list)
      if(idx == 1)
        disable("prev_file")
      else
        enable("prev_file")
      if(idx == length(file_list))
        disable("next_file")
      else
        enable("next_file")
    }
    lab_file <- labs_filename()
    if(file.exists(lab_file))
      fullData(read.csv(lab_file))
    else
      fullData(NULL)
  })
  
  labelsData <- reactive({
    lab_df <- fullData()
    lab_df <- lab_df[lab_df$file_name == input$file1,]
    if(is.null(lab_df))
      return(NULL)
    else if(nrow(lab_df)==0)
      return(NULL)
    else
      return(lab_df)
  })
  
  output$start_ui <- renderUI({
    if(.is_null(input$file1))
      actionButton("start_labelling", "Start Labelling!",
                                class = "btn-success")
    else
      actionButton("end_labelling", "End Labelling",
                   class = "btn-danger")
      
  })
  
  observeEvent(input$start_labelling, {
    updateSelectInput(inputId  = "file1",
                      selected = file_list[1])
  })
  
  observeEvent(input$end_labelling, {
    updateSelectInput(inputId  = "file1",
                      selected = "<NULL>")
  })
  
  output$label_ui <- renderUI({
    base_cols    <- c('darkgreen', 'green')
    misc_cols    <- c('yellow', 'orange')
    extra_cols   <- c('darkred', 'red')
    cbase  <- categories$base
    cmisc  <- categories$misc
    cextra <- categories$xtra
    #my_gradients <- colorRampPalette(c('darkred','red'))(length(cextra))
    
    get_jq_lines <- function(val, cols){
      x <- paste0("var ", 
                  c("originalBorder", "originalColor", "originalBackground" ), 
                  " = [];",  
                  collapse= " ")
      button_val_id <- paste0("'input[type=radio][name=label_points][value=\"", 
                              val, "\"]'")
      
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
          "'color':            'white',",
          #"'background-color': '", cols[1], "',",
          "'border-color':     '", cols[2], "'});")
        x <- paste0(x, "$(", button_val_id, ").hover(",
        "function(){",
            "originalBorder[", 
              css_line(button_val_id, func = "index", sq=""), 
              "]     = ", 
              css_line('border-color'),
            #"originalBackground[", 
            #  css_line(button_val_id, func = "index", sq=""), 
            #  "] = ", 
            #  css_line('background-color'),
            "originalColor[", 
              css_line(button_val_id, func = "index", sq=""), 
              "]      = ", 
              css_line('color'),
            css_line('border-color', 'darkblue'),
            #css_line('background-color', 'blue'),
            css_line('color', 'white'),
          "},")
        x <- paste0(x, 
        "function(){",
        css_line('border-color',     
                 paste0("originalBorder[", 
                        css_line(button_val_id, func = "index", sq=""), 
                        "]")), 
        #css_line('background-color', 
        #         paste0("originalBackground[", 
        #                css_line(button_val_id, func = "index", sq=""), 
        #                "]")), 
        css_line('color',            
                 paste0("originalColor[", 
                        css_line(button_val_id, func = "index", sq=""), 
                        "]")),
          "});")
        x <- gsub("'original", "original", x)
        x <- gsub(")]')", ")])", x)
      return(x)
    }
    btn_col_js <- NULL
    add_colour_js <- function(x, labs, cols){
      if(!is.null(labs)){
        for(lab in labs)
          x <- paste0(x, get_jq_lines(lab, cols))
      }
      return(x)
    }
    btn_col_js <- add_colour_js(btn_col_js, cbase, base_cols)
    btn_col_js <- add_colour_js(btn_col_js, cmisc, misc_cols)
    btn_col_js <- add_colour_js(btn_col_js, cextra, extra_cols)
    #print(btn_col_js)
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
      #might need to be wrapped in HTML
      tags$script(btn_col_js),
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
    
    #setWavPlayer("C:/Program Files/Windows Media Player/wmplayer.exe")
    
    #based on torchaudio::functional_gain
    audio_gain     <- function (waveform, gain_db = 0){
      if(gain_db == 0)
        return(waveform)
      ratio <- 10^(gain_db/20)
      return(waveform * ratio)
    }
    submean        <- function(x) x - mean(x)
    tmp_audio@left <- submean(tmp_audio@left)
    tmp_audio      <- audio_gain(tmp_audio, input$db_gain)
    #anything outside the 16-bit range [-32768, 32767] will be rounded
    tmp_audio@left[tmp_audio@left >  32767] <-  32767
    tmp_audio@left[tmp_audio@left < -32768] <- -32768
    tmp_audio@left <- as.integer(tmp_audio@left)
    
    len_s <- length(tmp_audio)/tmp_audio@samp.rate
    segment_end_s(len_s)
    if(len_s > 15){
      time_seq  <- seq(from = 0, to = len_s, by = 15)
      tc        <- time_seq[segment_num()]
      segment_start(tc)
      segment_total(length(time_seq))
      tmp_audio <- extractWave(tmp_audio, from = tc, to = tc+15, xunit = "time")
      x_coords(c(tc, tc + 15))
      if(length(tmp_audio) < 15*tmp_audio@samp.rate)
        tmp_audio@left <- c(tmp_audio@left, rep(1, 15*tmp_audio@samp.rate-length(tmp_audio)))
      if(segment_num() == 1)
        disable("prev_section")
      else 
        enable("prev_section")
      if(segment_num() == length(time_seq))
        disable("next_section")
      else
        enable("next_section")
    } else {
      disable("prev_section")
      disable("next_section")
      x_coords(NULL)
    }
    
    writeWave(tmp_audio, 'www/tmp.wav')
    return(tmp_audio)
  })
  
  cleanInput <- reactive({
    tmp_audio <- audioInput()
    if(is.null(tmp_audio))
      return(NULL)
    
    if(!is.null(ranges_osc$x) | !is.null(ranges_spec$x)){
      #time crop
      if(!is.null(ranges_osc$x))
        tc <- ranges_osc$x
      if(!is.null(ranges_spec$x))
        tc <- ranges_spec$x
      tmp_audio <- extractWave(tmp_audio, from = tc[1], to = tc[2], 
                               xunit = "time")
    } else if(!is.null(dc_ranges_osc$x) | !is.null(dc_ranges_spec$x)){
      #time crop
      if(!is.null(dc_ranges_osc$x))
        tc <- dc_ranges_osc$x
      if(!is.null(dc_ranges_spec$x))
        tc <- dc_ranges_spec$x
      tmp_audio <- extractWave(tmp_audio, from = tc[1], to = tc[2], 
                               xunit = "time")
    }

    if(!is.null(ranges_spec$y))
      frange <- ranges_spec$y
    else if(!is.null(dc_ranges_spec$y))
      frange <- dc_ranges_spec$y
    else
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
      #Put zeros outside frequency range, rebuild audio file using complex_spec
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
    txt <- paste0('<b>', input$file1, '</b>')
    if(segment_total() > 1){
      if(segment_num() == segment_total())
        seg_colour <- 'green'
      else
        seg_colour <- 'red'
      txt <- paste(txt, '<span style="color: ', seg_colour, ';"><b>(', 
              segment_num(), '/', segment_total(), ')</b></span>')
    }
    return(HTML(txt))
    })
  
  specData <- reactive({
    if(.is_null(input$file1))     
      return(data.frame(time        = 0,
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
                    wl       = input$window_width_disp, 
                    ovlp     = input$fft_overlap_disp, 
                    plot     = FALSE,
                    noisereduction = noisered)
    
    spec$amp <- spec$amp + input$db_gain
    spec$amp <- spec$amp + input$db_contrast
    
    df   <- data.frame(time      = rep(spec$time, each  = nrow(spec$amp)), 
                       frequency = rep(spec$freq, times = ncol(spec$amp)), 
                       amplitude = as.vector(spec$amp))
    
    df$time <- df$time + segment_start()
    
    #if(!is.null(dc_ranges_osc$x))
    #  df$time <- df$time + dc_ranges_osc$x[1]
    #else if(!is.null(dc_ranges_spec$x))
    #  df$time <- df$time + dc_ranges_spec$x[1]
    
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
    
    time_seq <- function(x)
      return(seq(0, length(x@left)/x@samp.rate, length.out = length(x)))
    df2 <- data.frame(time      = time_seq(tmp_audio),
                      amplitude = tmp_audio@left)
    if(!is.null(dc_ranges_osc$x))
      df2$time <- df2$time + dc_ranges_osc$x[1]
    else if(!is.null(dc_ranges_spec$x))
      df2$time <- df2$time + dc_ranges_spec$x[1]
    
    #spacing for "custom" y axis margin
    strlen_osc_y  <- length_b10(df2$amplitude)
    spec_freq <- input$frequency_range
    if(!is.null(ranges_spec$y))
      spec_freq <- c(max(spec_freq[1], ranges_spec$y[1]), 
                     min(spec_freq[2], ranges_spec$y[2]))
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
  
  specPlot <- reactive({
    p <- plot_spectrogram(specData(), input, length_ylabs, dc_ranges_spec)
    
    #if(!is.null(input$file1))
    #  spec_name_raw <- paste0(gsub('.wav', '', input$file1), '_spec_raw.png')
    #else
    #  spec_name_raw <- 'blank_spec_raw.png'
    #file_nm <- file.path(getwd(), "images", spec_name_raw)
    #
    #width   <- session$clientData$output_specplot_width
    #height  <- session$clientData$output_specplot_height
    # For high-res displays, this will be greater than 1
    #pixelratio <- session$clientData$pixelratio
    #ggsave(file_nm, p+theme_void()+theme(legend.position = "none"),
    #       height = height*pixelratio, 
    #       width  = width*pixelratio,
    #       units  = "px")
    if(!is.null(x_coords()))
      p <- p + coord_cartesian(xlim = x_coords(),
                               expand = FALSE)
    if(!is.null(dc_ranges_spec$y))
      p <- p + coord_cartesian(ylim = dc_ranges_spec$y,
                               xlim = dc_ranges_spec$x,
                               expand = FALSE, default = TRUE)
    
    return(p)
  })
  
  output$specplot <- renderPlot({
    spec_plot <- specPlot()
    lab_df    <- labelsData()
    if(is.null(lab_df))
      return(spec_plot)
    else
      if(input$spec_labs){
        spec_plot <- spec_plot +
          geom_rect(data = lab_df, 
                    mapping = aes(xmin = start_time,
                                  xmax = end_time,
                                  ymin = start_freq, 
                                  ymax = end_freq),
                    colour = cat_colours(lab_df),
                    fill   = "lightgrey",
                    alpha  = 0.15) +
          geom_label(data = lab_df,
                     aes(x     = start_time,
                         y     = end_freq,
                         label = class_label),
                     colour = cat_colours(lab_df),
                     label.r = unit(0, units="lines"),
                     label.size = 0.5,
                     hjust  = 0,
                     vjust  = 0
                     ) +
          geom_label(data = lab_df,
                    aes(x     = start_time,
                        y     = end_freq,
                        label = class_label),
                    label.r = unit(0, units="lines"),
                    label.size = 0,
                    hjust  = 0,
                    vjust  = 0,
                    alpha  = 0,
                    colour = "black")
      }
    return(spec_plot)
  })
  
  observeEvent(input$savespec, {
    if(!.is_null(input$file1))
      spec_name <- paste0(gsub('.wav', '', input$file1), '_spec.png')
    else
      spec_name <- 'blank_spec.png'
    file_nm <- file.path(getwd(), "images", spec_name)
    width   <- session$clientData$output_specplot_width
    height  <- session$clientData$output_specplot_height
    # For high-res displays, this will be greater than 1
    pixelratio <- session$clientData$pixelratio
    ggsave(file_nm, p,
           height = height*pixelratio, 
           width  = width*pixelratio,
           units  = "px")
    showNotification(HTML(paste0("Spectrogram image <b>", 
                                 spec_name, 
                                 "</b> saved to <b>images</b>.")), 
                     #TODO: clickable link to images folder
                     #action = a(href = file.path("file://", getwd(), "images"), 
                     #          "Go to folder", target = "_blank"),
                     type = "message")
  })
      
  output$specplot_blank <- renderPlot({
    if(plots_open$spec)
      return(NULL)
    else 
      blank_plot(label = "Spectrogram (hidden)")
    })
  
  output$oscplot_ui <- renderUI({
    if(input$include_osc)
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
    else
      return(NULL)
  })
  
  output$oscplot <- renderPlot({
    p <- plot_oscillogram(oscData(), input, length_ylabs)
    
    if(!.is_null(input$file1)){
      if(!is.null(dc_ranges_spec$x))
        p <- p + coord_cartesian(xlim = dc_ranges_spec$x, expand = FALSE)
      else if(!is.null(dc_ranges_osc$x))
        p <- p + coord_cartesian(xlim = dc_ranges_osc$x, expand = FALSE)
     osc_name <- paste0(gsub('.wav', '', input$file1), '_osc.png')
    } else 
      osc_name <- 'blank_osc.png'
    observeEvent(input$saveosc, {
      file_nm <- file.path(getwd(), "images", osc_name)
      width   <- session$clientData$output_oscplot_width
      height  <- session$clientData$output_oscplot_height
      # For high-res displays, this will be greater than 1
      pixelratio <- session$clientData$pixelratio
      ggsave(file_nm, p,
             height = height*pixelratio, 
             width  = width*pixelratio,
             units  = "px")
      showNotification(HTML(paste0("Oscillogram image <b>", 
                                   osc_name, 
                                   "</b> saved to <b>images</b>.")), 
                       #TODO: clickable link to images folder
                       #action = a(href = file.path("file://", getwd(), "images"), 
                       #          "Go to folder", target = "_blank"),
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
    point <- nearPoints(specData(), hover, 
                        threshold = 5, 
                        maxpoints = 1, 
                        addDist   = TRUE, 
                        xvar      = "time", 
                        yvar      = "frequency")
    if(nrow(point) == 0) 
      return(NULL)
    if(point$time > segment_end_s())
      return(NULL)
    point <- round(point, 3)
    
    in_label_box <- function(df, point){
      return(point$time      >= df$start_time & 
             point$time      <= df$end_time   &
             point$frequency >= df$start_freq &
             point$frequency <= df$end_freq   )
    }
    lab_df <- labelsData()
    lab_df <- lab_df[in_label_box(lab_df, point),]
    
    #if(is.null(lab_df))
    species_in_hover <- ''
    #else if(nrow(lab_df) == 0)
    #  species_in_hover <- ''
    #else{
    #  lab_df <- lab_df[1,]
    #  species_in_hover <- paste0("<br/><b> Species: </b>", lab_df$class_label,
    #                             "<br/><b> Call type: </b>", lab_df$call_type)
    #}
    
    # create style property for tooltip
    # background color is set so tooltip is a almost transparent
    # z-index is the stack index and is set to 100 so the tooltip will be on top
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
    point <- nearPoints(oscData(), hover, 
                        threshold = 5, 
                        maxpoints = 1, 
                        addDist   = TRUE, 
                        xvar      = "time")
    if(nrow(point) == 0) 
      return(NULL)
    point <- round(point, 3)
    
    in_label_box <- function(df, point){
      return(point$time      >= df$start_time & 
             point$time      <= df$end_time)
    }
    lab_df <- labelsData()
    if(is.null(lab_df))
      species_in_hover <- ''
    else{
      lab_df <- lab_df[1,]
      species_in_hover <- paste0("<br/><b> Species: </b>", lab_df$class_label,
                                 "<br/><b> Call type: </b>", lab_df$call_type)
    }
    
    # create style property for tooltip
    # background color is set so tooltip is a almost transparent
    # z-index is the stack index and is set to 100 so the tooltip will be on top
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
  
  observeEvent(input$specplot_brush, {
    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    brush <- input$specplot_brush
    if (!is.null(brush)) {
      ranges_spec$x <- c(brush$xmin, brush$xmax)
      ranges_spec$y <- c(brush$ymin, brush$ymax)
      #showNotification("Click play to hear selected times/frequencies", 
      #                  type = "warning") 
    } else {
      reset_ranges(ranges_spec)
    }
  })
  
  observeEvent(input$specplot_click, {
    reset_ranges(ranges_spec)
  })
  
  observeEvent(input$specplot_dblclick, {
    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    #dc_ranges_spec <- ranges_spec
    brush <- input$specplot_brush
    if (!is.null(brush)) {
      dc_ranges_spec$x <- c(brush$xmin, brush$xmax)
      dc_ranges_spec$y <- c(brush$ymin, brush$ymax)
      showNotification("Double click plot to reset zoom", type = "default") 
      #showNotification("Click play to hear selected times/frequencies", 
      #                  type = "warning") 
    } else {
      reset_ranges(dc_ranges_spec)
      reset_ranges(ranges_spec)
    }
  })
  
  observeEvent(input$oscplot_dblclick, {
    brush <- input$oscplot_brush
    if (!is.null(brush)){
      ranges_osc$x <- c(brush$xmin, brush$xmax)
      showNotification("Double click plot to reset zoom", type = "default") 
    } else
      reset_ranges(ranges_osc)
  })
  
  observeEvent(input$plt_reset, {
    reset_ranges(ranges_spec)
    reset_ranges(ranges_osc)
    reset_ranges(dc_ranges_spec)
    reset_ranges(dc_ranges_osc)
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
      lab_rem_idx <- which(categories$xtra == input$label_points)
      categories$xtra <- categories$xtra[-lab_rem_idx]
      showNotification(HTML(paste0("Label <b>", 
                                   input$label_points, 
                                   "</b> removed")), 
                       type = "warning")
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
    if(!is.null(input$specplot_brush)) {
      if(is.null(input$call_type))
        call_type <- "<NULL>"
      else
        call_type <- paste(input$call_type[!.is_null(input$call_type)], 
                           collapse='; ')
      lab_df <- data.frame(date_time   = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                           file_name   = input$file1,
                           start_time  = min(res$time),
                           end_time    = max(res$time),
                           start_freq  = min(res$frequency),
                           end_freq    = max(res$frequency),
                           class_label = input$label_points,
                           call_type   = call_type,
                           confidence  = input$label_confidence,
                           notes       = input$notes,
                           labeler     = Sys.info()[["user"]])
      
      full_df <- fullData()
      if(!is.null(full_df)){
        write_labs(lab_df)
        fullData(rbind(full_df, lab_df))
      } else {
        write_labs(lab_df, append = FALSE, col.names = TRUE)
        fullData(lab_df)
      }
      showNotification(HTML(paste0('Label ', '<span style="color: ', 
                                   cat_colours(lab_df), ';"><b>', 
                                   input$label_points, 
                                   '</b></span> successfully saved!')), 
                       type = "message")
    } else {
      showNotification("Label not saved, nothing selected!", type = "error")
    }
  })
  
  observeEvent(input$remove_points, {
    #get x and y coordinates with max and min of brushedPoints()
    res <- brushedPoints(specData(), input$specplot_brush,
                         xvar = 'time', yvar = 'frequency')
    if(!is.null(input$specplot_brush)) {
      lab_df <- data.frame(start_time  = min(res$time),
                           end_time    = max(res$time),
                           start_freq  = min(res$frequency),
                           end_freq    = max(res$frequency))
      full_df    <- fullData()
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
        fullData(full_df)
        write_labs(full_df, append = FALSE, col.names = TRUE)
        showNotification("Label removed, click Undo to bring back", 
                         type = "message")
      }
    } else {
      showNotification("Label not removed, nothing selected!", type = "error")
    }
  })
  
  observeEvent(input$undo_delete_lab, {
    full_df   <- fullData()
    rownums   <- deleted_lab$rows
    del_df    <- deleted_lab$data
    if(!is.null(del_df)){
      insertRow <- function(old_df, newrow, idx) {
        if(idx <= nrow(old_df))
          old_df[seq(idx+1,nrow(old_df)+1),] <- old_df[seq(idx,nrow(old_df)),]
        old_df[idx,] <- newrow
        return(old_df)
      }
      for(row in 1:length(rownums))
        full_df <- insertRow(full_df, del_df[row,], rownums[row])
      deleted_lab$rows <- NULL
      deleted_lab$data <- NULL
      write_labs(full_df, append = FALSE, col.names = TRUE)
      fullData(full_df)
      showNotification("Label recovered", type = "message")
    } else {
      showNotification("Nothing undone, no deletions detected!", type = "error")
    }
  })
  
  output$my_audio <- renderUI({
    audio_style <- "width: 100%;"
    if(!is.null(cleanInput())){
      file_name   <- 'www/tmp_clean.wav'
      audio_style <- paste(audio_style, "filter: sepia(50%);")
    } else {
      file_name <- 'www/tmp.wav'
    }
    
    audio_style <- HTML(audio_style)
    if(.is_null(input$file1))     
      return(tags$audio(id       = 'my_audio_player',
                        src      = "", 
                        type     = "audio/wav", 
                        controls = NA, 
                        autoplay = NA,
                        style    = audio_style
      ))
    pb <- input$playbackrate
    div(
      tags$audio(id       = 'my_audio_player',
                 src      = markdown:::.b64EncodeFile(file_name), 
                 type     = "audio/wav", 
                 controls = "controls",#HTML('controlsList: nodownload'),
                 #TODO: HTML styling (background colour, no download button,...)
                 style    = audio_style),
      tags$script(paste0('var audio = document.getElementById("my_audio_player");
                audio.playbackRate = ', pb, ';'))
    )
    })
  
  output$audio_title <- renderUI({
    base <- "<b>Play audio:<b/>"
    if(!is.null(cleanInput()))
      base <- paste(base, '<span style="color: red;">(selected)</span>')
    HTML(base)
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
    idx <- which(input$file1 == file_list) - 1
    if(idx == 0)
      showNotification("Cannot go to previous file, at beginning of folder", 
                       type = "error")
    else {
      updateSelectInput(inputId  = "file1",
                        selected = file_list[idx])
    }
    reset_ranges(ranges_spec)
    reset_ranges(ranges_osc)
    reset_ranges(dc_ranges_spec)
    reset_ranges(dc_ranges_osc)
    segment_num(1)
    segment_total(1)
    segment_end_s(1)
    segment_start(0)
  })
  
  # move to next file (resetting zoom)
  observeEvent(input$next_file, {
    idx <- which(input$file1 == file_list) + 1
    if(idx > length(file_list))
      showNotification("Cannot go to next file, at end of folder", 
                       type = "error")
    else {
      updateSelectInput(inputId  = "file1",
                        selected = file_list[idx])
    }
    reset_ranges(ranges_spec)
    reset_ranges(ranges_osc)
    reset_ranges(dc_ranges_spec)
    reset_ranges(dc_ranges_osc)
    segment_num(1)
    segment_total(1)
    segment_end_s(1)
    segment_start(0)
  })
  
  # Previous (15 second) segment
  observeEvent(input$prev_section, {
    idx <- segment_num() - 1
    
    if(idx < 1)
      showNotification("Cannot go to previous segment, at beginning of file", 
                       type = "error")
    else {
      segment_num(idx)
      reset_ranges(ranges_spec)
      reset_ranges(ranges_osc)
      reset_ranges(dc_ranges_spec)
      reset_ranges(dc_ranges_osc)
    }
  })
  
  # Next (15 second) segment
  observeEvent(input$next_section, {
    idx <- segment_num() + 1
    
    if(idx > segment_total())
      showNotification("Cannot go to next segment, at end of file", 
                       type = "error")
      else {
        segment_num(idx)
        reset_ranges(ranges_spec)
        reset_ranges(ranges_osc)
        reset_ranges(dc_ranges_spec)
        reset_ranges(dc_ranges_osc)
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
