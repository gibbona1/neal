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

source('plot_helpers.R')
source('audio_meta.R')

#change max supported audio file size to 30MB
options(shiny.maxRequestSize = 30*1024^2)

species_list <- read.csv("species_list.csv", fileEncoding = 'UTF-8-BOM')
#Some taken from https://www.audubon.org/news/a-beginners-guide-common-bird-sounds-and-what-they-mean
call_types <- c("song", "call", "subsong", "alarm call", "begging call", "contact call", "flight call", "flock", "juvenile call", "nocturnal call", "whisper song")
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

jsCode <- 'shinyjs.audiotoggle = function() {
  var audio = document.getElementById("my_audio_player");
  if(audio.paused){ //check audio is playing
    audio.play();
  } else {
    audio.pause();
  }
}'

ui_func <- function() {
  header <- {dashboardHeader(
    title = "Audio Labeler App",
    dropdownMenu(
      tags$li(class = "dropdown",
              uiOutput("start_ui")
      ),
      tags$li(class = "dropdown",
              auth0::logoutButton()
      ),
      type = "notifications", 
      icon = icon("user"),
      badgeStatus = NULL,
      headerText  = uiOutput("user_ui")
    ),
    dropdownMenu(tags$li(class = "dropdown",
                         HTML("<kbd>&#8679;</kbd>+<kbd>&#9166;</kbd> to Save Selection <br/>"),
                         HTML("<kbd>&#8679;</kbd>+<kbd>&#9003;</kbd> to Delete Selection <br/>"),
                         HTML("<kbd>&#8679;</kbd>+<kbd>&#8592;</kbd> to move to previous file <br/>"),
                         HTML("<kbd>&#8679;</kbd>+<kbd>&#8594;</kbd> to move to next file <br/>"),
                         HTML("<kbd>&#8679;</kbd>+<kbd>Space</kbd> to pause/play audio<br/>"),
                         HTML("<kbd>&#9166;</kbd> in additional category textbox to add one"),
                         HTML("<kbd>&#9003;</kbd> in category list to delete one")
    ),
    type = "notifications", 
    icon = icon("keyboard"),
    badgeStatus = NULL,
    headerText  = "Hotkeys"
    ),
    dropdownMenu(
      type = "notifications", 
      icon = icon("question-circle"),
      badgeStatus = NULL,
      headerText  = "Links:",
      tags$li(
        a(href = "https://github.com/gibbona1/audio_labeler",
          target = "_blank",
          tagAppendAttributes(icon("github"), class = "text-info"),
          "GitHub")
      ),
      tags$li(
        a(href = "https://github.com/gibbona1/audio_labeler/tree/master/instruction_doc",
          target = "_blank",
          tagAppendAttributes(icon("file"), class = "text-info"),
          "Instructions")
      ),
      tags$li(
        a(href = "https://www.bto.org/sites/default/files/u16/downloads/forms_instructions/bto_bird_species_codes.pdf",
          target = "_blank",
          tagAppendAttributes(icon("crow"), class = "text-info"),
          "BTO Species Codes")
      )
    )
  )}
  
  sidebar <- {dashboardSidebar(
    shinyjs::useShinyjs(),
    sidebarMenu(
      menuItem("Configuration", tabName = "config_menu", icon = icon("bars"),
               #File/Folder selection
               shinyDirButton('folder',
                              label = 'Folder select',
                              title = 'Please select a folder',
                              icon  = icon('folder')),
               verbatimTextOutput("folder", placeholder = TRUE),
               selectInput("species_list",
                           "Species List:",
                           choices = colnames(species_list),
                           width   = '100%'),
               checkboxInput("bto_codes", "Display as BTO codes", value = FALSE),
               actionButton('inputLoad', 'Load Settings')
      ),
      menuItem("Sound Settings", tabName = "sound_menu", icon = icon("music"),
               sliderInput(
                 "db_gain",
                 "dB Gain:",
                 min   = -96,
                 max   = 96,
                 value = 0,
                 ticks = FALSE
               ),
               numericInput('t_step', 'Audio length (in window)', 
                            value = 15, min = 10, max = 60, step = 1)
      ),
      menuItem("Spectrogram Settings", 
               tabName = "spec_menu", 
               icon    = icon("chart-area"),
               sliderInput("spec_height", 
                           "Plot Height", 
                           min   = 300,
                           max   = 800,
                           value = 300,
                           ticks = FALSE,
                           step  = 100),
               selectInput("freq_min", "minimum frequency in filter", 
                           choices = c(0, 2^(3:7)), selected = 0),
               selectInput("freq_max", "maximum frequency in filter", 
                           choices = 2^(4:9), selected = 32),
               selectInput(
                 "noisereduction",
                 "Noise reduction:",
                 choices  = c("None", "Rows", "Columns"),
                 selected = "None",
                 width    = '100%'
               ),
               selectInput(
                 "spec_db",
                 "dB type",
                 choices = c('max0', 'A', 'B', 'C', 'D'),
                 selected = 'max0',
                 width = '100%'
               ),
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
               actionButton("savespec", "Download Spectrogram", icon = icon('save')),
               checkboxInput("include_hover", "Include spectrogram hover tooltip", 
                             value = FALSE),
               checkboxInput("spec_time", "Vertical line guide for audio current time", 
                             value = FALSE),
               checkboxInput("include_guides", "Selected time/frequency guidelines",
                             value = FALSE),
               checkboxInput("spec_labs", "Show spectrogram labels", value = TRUE),
               downloadButton("downloadSpec", "Download Spec as CSV"),
               checkboxInput("clean_zero", "Zero Audio outside Selected", value = TRUE),
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
               actionButton("saveosc", "Download Oscillogram", icon = icon('save')),
               checkboxInput("include_hover_osc", "Include oscillogram hover tooltip", 
                             value = FALSE),
               checkboxInput("osc_labs", "Show oscillogram labels"),
               checkboxInput("include_osc", "Show Oscillogram", value = FALSE),
               uiOutput("osc_collapse")
      ),
      menuItem("Data Settings", tabName = "data_menu", icon = icon("database"),
               #checkboxInput("hide_other_labels", "Hide other users' labels", value = TRUE),
               downloadButton("downloadData", "Download Labels")
      ),
      menuItem("Other Settings", tabName = "other_menu", icon = icon("cog"),
               numericInput('label_columns', 'Number of Columns', 
                            value = 7, min = 1, max = 15, step = 1),
               actionButton("reset_sidebar", "Reset Sidebar"),
               actionButton("reset_body", "Reset Body"),
               checkboxInput("fileEditTab", "Label Edit Table", value = FALSE),
               checkboxInput("fileSummaryTab", "File Summary Table", value = FALSE),
               checkboxInput("summaryTabGroup", "Subgroup by class", value = TRUE),
               # Add the Undo/Redo buttons to the UI
               h5("Undo/Redo label save or delete"),
               undoHistoryUI("lab_hist", back_text = "Undo", fwd_text = "Redo")#,
               #undoHistoryUI_debug("lab_hist")
      )
    ),
    #Options for sidebar
    collapsed = TRUE,
    minified  = FALSE,
    id = "side-panel")}
  
  body <- {dashboardBody(
    shinyjs::useShinyjs(),
    extendShinyjs(text = jsCode, functions = c("audiotoggle")),
    useKeys(),
    uiOutput('loadScript'),
    keysInput("keys", hotkeys),
    id    = "main-panel",
    theme = "blue_gradient",
    tags$style(".content-wrapper{margin-left: 0px;}"),
    tags$head(tags$style(HTML(".content {padding-top: 0;}"))),
    fluidRow({
    div(
      column(2,
        HTML('<b>Filename: </b>')
      ),
      column(8,
        selectInput(
          "file1",
          label   = NULL,
          choices = c(""),
          width   = '100%'
        ),
        tags$head(tags$style(HTML('
                            #file1+ div>.selectize-input{
                            font-size: 14px; line-height: 14x; margin-bottom: -20px; 
                            min-height: 0px; 
                            }
                            #file1+ div>.selectize-dropdown{
                            font-size: 14px; margin-top: 20px; 
                            }
                            ')))
      ),
      column(2,
        uiOutput("segmentNumText")
      )
      )
    }),
    #Spectrogram Plot
    fluidRow({
      uiOutput('specplot_ui')
    }),
    #Oscillogram Plot
    #fluidRow({
    #  uiOutput("oscplot_ui")
    #  }),
    #One row of audio settings
    fluidRow({
      div(
        column(4, style='padding:0px;',
               #br(),
               actionButton("save_points", 
                            HTML("<b>Save Selection</b>"), 
                            icon  = icon("vector-square"),
                            style = "width: 100%; background-color: #fff491;"),
               uiOutput("meta_text")
        ),
        column(3, {
          div(
            uiOutput('audio_title'),
            uiOutput('my_audio'),
            tags$script('
                 var id = setInterval(audio_pos, 100);
                 function audio_pos() {
                 var audio = document.getElementById("my_audio_player");
                 var curtime = audio.currentTime;
                 console.log(audio);
                 Shiny.onInputChange("get_time", curtime);
                 };')
            #actionButton("get_time", "Get Time", onclick = js),
            #verbatimTextOutput("audio_time")
          )
        }),
        column(3, {
          fixedRow(style = btn_row_style,
                   div(
                     column(4, 
                            uiOutput("downloadAudio_ui")
                            ),
                   column(8, 
                          selectInput(
                            "playbackrate",
                            "Playback Speed:",
                            choices  = playback_vals,
                            selected = 1,
                            width    = '100%'
                          )
                   )))
        }),
        column(2,{
          fixedRow(style = btn_row_style,
                   div(column(6, style = "padding:0px;",
                              tipify(
                                actionButton("prev_file", "", 
                                             icon  = icon("arrow-left"), 
                                             style = file_btn_style),
                                "Previous File"
                              ),
                   ), 
                   #column(3, style = "padding:0px;",
                   #        tipify(
                   #          actionButton("prev_section", "", 
                   #                       icon  = icon("chevron-left"), 
                   #                       style = file_btn_style),
                   #          "previous section"
                   #        )
                   # ), 
                   #column(3, style = "padding:0px;",
                   #        tipify(
                   #          actionButton("next_section", "", 
                   #                       icon  = icon("chevron-right"), 
                   #                       style = file_btn_style),
                   #          "next section"
                   #        )
                   # ), 
                   column(6, style = "padding:0px;",
                          tipify(actionButton("next_file", "", 
                                              icon  = icon("arrow-right"), 
                                              style = file_btn_style), 
                                 "Next File")
                   )
                   ),
                   fluidRow(style = btn_row_style,
                            actionButton("plt_reset", "Reset Zoom", 
                                         style = file_btn_style)
                   )
          )
        })
      )
    }),
    #Label Buttons UI 
    fluidRow({
      div(style = btn_sel_style,
        uiOutput("label_ui"), #backspace to delete
        tags$script('document.getElementById("label_ui").addEventListener("keyup", 
        function(event) {
          event.preventDefault();
          if (event.keyCode === 8) {
            document.getElementById("remCategory").click();
          }
        });'),
        column(6,
        textInput("otherCategory", "Type in additional category:", 
                  width = "100%"), #enter to add category
        tags$script('document.getElementById("otherCategory").addEventListener("keyup", 
        function(event) {
          event.preventDefault();
          if (event.keyCode === 13) {
            document.getElementById("addCategory").click();
          }
        });')
        ),
        column(6,
        fluidRow(style = btn_sel_style,
                 selectInput(
                   inputId    = "call_type", 
                   label      = "Call Type:", 
                   width      = '100%',
                   multiple   = TRUE,
                   choices    = call_types, 
                   selected   = NULL)
                 )
          )
      )
    }),
    #Label buttons
    fluidRow({
      div(
       #br(),
       #TODO: Other info to label/record -
       ## altitude of recorder (check if in metadata)
       column(6,
              fixedRow(#style = btn_row_style,
                actionButton("addCategory", 
                             HTML("<b>Add category</b>"), 
                             icon  = icon("plus-square"),
                             style = "width: 100%; text-align:left;"),
                actionButton("remCategory", 
                             HTML("<b>Remove category</b>"), 
                             icon  = icon("minus-square"),
                             style = "width: 100%; text-align:left;"),
                actionButton("resetCategory", 
                             HTML("<b>Reset categories</b>"), 
                             icon  = icon("list"),
                             style = "width: 100%; text-align:left;")
              )),
       column(6,
              fluidRow(#style = btn_row_style,
                disabled(actionButton("save_extra", 
                                      HTML("<b>Save to List</b>"), 
                                      icon  = icon("archive"),
                                      style = "width: 100%; text-align:left;")),
                actionButton("remove_points", 
                             HTML("<b>Delete Selection</b>"), 
                             icon  = icon("trash-alt"),
                             style = "width: 100%; text-align:left"),
                actionButton("undo_delete_lab", 
                             HTML("<b>Undo Deletion</b>"), 
                             icon  = icon("undo-alt"),
                             style = "width: 100%; text-align:left;")
              )
       )
      )
    }),
    #notes, frequency filtering and label confidence
    fluidRow({
      div(
        column(4, 
               textAreaInput("notes", "Additional Notes:", width = "100%", resize = "vertical")
        ),
        column(4,
               uiOutput("freq_ui")
        ),
        column(4,
               sliderInput("label_confidence", "Label Confidence:",
                           min   = 0,
                           max   = 1,
                           step  = 0.05,
                           value = 1,
                           ticks = FALSE,
                           width = "100%"),
               div(div(style='float:left;', 'low'), 
                   div(style='float:right;', 'high')),
        )
      )
    }),
    #label summary and edit
    fluidRow({
      uiOutput("fileLabInfo")
    }),
    #file summary
    fluidRow({
      uiOutput("fileLabSummary")
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
  
  lab_nickname <- function(){
    auth0_session <- session$userData$auth0_info
    if(is.null(auth0_session))
      nickname <- Sys.info()[["user"]]
    else 
      nickname <- auth0_session$nickname
    if(!(nickname %in% list.files('www/')))
      nickname <- 'tmp'
    return(nickname)
  }
  
  filename_pre <- function(x, df)
    return(paste0('(', nrow(df[df$file_name == x,]), ') ', x))
  
  file_list <- reactive({
    filenames <- list.files(file.path('www', lab_nickname()))
    filenames <- filenames[!stringr::str_starts(filenames, "tmp")]
    full_df <- fullData()
    if(!is.null(full_df))
      names(filenames) <- sapply(filenames, function(x) filename_pre(x,full_df))
    return(filenames)
  })
  
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
  specplot_range <- reactiveValues(x = NULL, y = NULL)
  audio_clean    <- reactiveValues(select = FALSE, noisered = FALSE)
  x_coords       <- reactiveVal(NULL)
  segment_num    <- reactiveVal(1)
  segment_total  <- reactiveVal(1)
  segment_end_s  <- reactiveVal(1)
  segment_start  <- reactiveVal(0)
  
  extractWave_t <- function(x,tc){
    return(extractWave(x, from = tc[1], to = tc[2], xunit = "time"))
  }
  
  btw <- function(x, left, right){
    return(x >= left & x <= right)
  }
  
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
  
  trim_start <- function(y){
    start_list <- c("Common", "Eurasian", "European", "Northern", "Greater", "Great")
    for(st in start_list)
      y <- gsub(paste0("^", st, " "), "", y)
    return(y)
  }
  
  get_entries <- function(x){
    x <- x[x != ""]
    x <- trim_start(x)
    x <- as.vector(sort(x))
    return(x)
  }
  
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
    if(is.null(x))
      return(NULL)
    other_c <- unique(unlist(species_list))
    other_c <- other_c[other_c != "" & !(other_c %in% class_label())]
    x_cl    <- as.vector(x$class_label)
    other_c <- union(other_c, x_cl[!x_cl %in% class_label()])
    cat_df  <- data.frame(typecol = c(rep("green", length(categories$base)),
                                      rep("orange", length(categories$misc)),
                                      rep("cyan", length(categories$xtra)),
                                      rep("grey", length(other_c))),
                          category = c(class_label(), other_c))
    merge_df <- merge(x, cat_df, by.x = "class_label", by.y = "category", sort = FALSE)
    return(merge_df)
  }
  
  noise_reduce <- function(x){
    return(switch(x,
                  None    = NULL,
                  Rows    = 1,
                  Columns = 2))
  }
  
  geometric_median <- function(X, eps=1e-5, numIter = 200){
    mX <- matrix(c(as.vector(Re(X)), 
                   as.vector(Im(X))), 
                 ncol=2, byrow = FALSE)
    y  <- colMeans(mX)
    itr <- 1
    while(TRUE){
      D <- rdist::cdist(mX, matrix(y, nrow=1))
      nonzeros <- (D != 0)[, 1]
      Dinv  <- 1 / D[nonzeros]
      Dinvs <- sum(Dinv)
      W     <- Dinv / Dinvs
      WT    <- colSums(W * mX[nonzeros,])
      
      num_zeros <- length(X) - sum(nonzeros)
      if(num_zeros == 0)
        y1 <- WT
      else if(num_zeros == length(X))
        return(y)
      else {
        R <- (WT - y) * Dinvs
        r <- norm(R, type = "F")
        if(r == 0)
          rinv <- 0
        else
          rinv <- num_zeros/r
        y1 <-  max(0, 1-rinv)*WT + min(1, rinv)*y
      }
      euclidean <- function(a, b) sqrt(sum((a - b)^2))
      if(euclidean(y, y1) < eps | itr > numIter)
        return(y)
      y <- y1
      itr <- itr + 1
    }
  }
  
  categories <- reactiveValues(
    base = get_entries(species_list[,1]), 
    misc = c("Human", "Bird - Cannot Identify", "Anthropogenic Noise", "Geophonic Noise", "Insect Noise", "Other Noise"),
    xtra = c()
  )
  
  observeEvent(input$species_list, {
    categories$base <- get_entries(species_list[,input$species_list])
  })
  
  observeEvent(input$bto_codes, {
    x <- get_entries(species_list[,input$species_list])
    if(input$bto_codes){
      bto_df <- read.csv("bto_codes.csv", fileEncoding = 'UTF-8-BOM')
      bto_df$species_name <- trim_start(bto_df$species_name)
      x_bto <- as.vector(merge(data.frame(species_name = x), bto_df)$bto_code)
      x <- c(x_bto, x[!x %in% bto_df$species_name])
    }
    categories$base <- x
  })
  
  class_label <- reactive({
    cats <- categories
    c(cats$base, cats$misc, cats$xtra)
  })
  
  labs_filename <- reactive({paste0("labels/tmp_labels_",lab_nickname(),".csv")})
  
  conf_filename <- reactive({paste0("saved_configs/input_",lab_nickname(),".RDS")})
  
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
      #updateSelectInput(inputId  = "file1", 
      #                  choices  = file_list())
    } else {
      idx <- which(input$file1 == file_list())
      if(idx == 1)
        disable("prev_file")
      else
        enable("prev_file")
      if(idx == length(file_list()))
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
    lab_df <- cat_colours(lab_df)
    if(is.null(lab_df))
      return(NULL)
    else if(nrow(lab_df)==0)
      return(NULL)
    else
      return(lab_df)
  })
  
  labeler <- reactive({
    auth0_session <- session$userData$auth0_info
    if(is.null(auth0_session))
      return(Sys.info()[["user"]])
    else 
      return(auth0_session$name)
  })
  
  undo_app_state <- undoHistory(
    id = "lab_hist",
    value = reactive({
      # Value must be a reactive, but can be any structure you want
      fullData()
    })
  )
  
  # Use an observer to receive updates from undoHistory() and update the app.
  observe({
    req(!is.null(undo_app_state())) #<< Need to update app whenever not NULL
    
    # Manually update app UI and reactive values
    fullData(undo_app_state())
  })
  
  # only admin can download all labels
  # other users download only theirs
  saveData <- reactive({
    if(labeler() == "anthony.gibbons.2022@mumail.ie"){
      df <- data.frame()
      for(labfile in list.files('labels')){
        fname <- file.path('labels',labfile)
        if(file.exists(fname))
          df <- rbind(df,read.csv(fname))
      }
      return(df)
    } else
      return(fullData())
  })
  
  output$fileLabInfo <- renderUI({
    if(.is_null(input$file1) | !input$fileEditTab)
      return(NULL)
    lab_df <- labelsData()
    if(is.null(lab_df))
      return(NULL)
    if(nrow(lab_df)==0)
      return(NULL)
    panel_name <- paste0("Label Info", 
                         ifelse(is.null(lab_df), " ", 
                                paste0(' (',nrow(lab_df),')')))
    bsCollapse(id = "fileLabInfo",
               open = panel_name,
               bsCollapsePanel(panel_name,
                               tags$style("width: 100%"),
                               tableOutput("labTable"),
                               style = "info")
    )
  })
  
  output$fileLabSummary <- renderUI({
    if(!input$fileSummaryTab)
      return(NULL)
    df <- fullData()
    if(is.null(df))
      return(NULL)
    if(nrow(df)==0)
      return(NULL)
    
    panel_name <- paste("File Summary", 
                        ifelse(is.null(df), "", 
                               paste0('(',length(unique(df$file_name)),')')))
    bsCollapse(id = "fileLabSummary",
               open = panel_name,
               bsCollapsePanel(panel_name,
                               tags$style("width: 100%"),
                               DT::dataTableOutput("labSummaryTable"),
                               style = "info")
    )
  })
  
  input_names <- function(x){
    lab_df <- labelsData()
    if(is.null(lab_df))
      return(NULL)
    paste0('tab_',which(input$file1 == file_list()), x, 
           format(as.POSIXct(lab_df$date_time), "%Y%m%d_%H%M%S"))
  }
  
  output$labTable <- renderTable({
    lab_df <- labelsData()
    #columns in right order
    lab_df <- lab_df[,colnames(fullData())]
    lab_df <- lab_df[,1:10]
    lab_df <- lab_df[,colnames(lab_df)!="file_name"]
    tab_num_input <- function(x, val, name, minv = NULL, maxv = NULL, step = NULL){
      res <- paste0("<input id='", input_names(name), "' ",
                    "class='shiny-bound-input' type='number' value='", val, "' ")
      if(!is.null(minv))
        res <- paste0(res, "min='", minv, "' ")
      if(!is.null(maxv))
        res <- paste0(res, "max='", maxv, "' ")
      if(!is.null(step))
        res <- paste0(res, "step='", step, "' ")
      res <- paste0(res, "style='width: 100%;'>")
      res
    }
    tab_class_input <- function(x, sel, choices, name){
      res <- paste0("<select id='", input_names(name), "'>")
      optlist <- rep("", length(sel))
      for(i in 1:length(sel)){
        sel_str <- ifelse(choices == sel[i], " selected", "")
        optlist[i] <- paste0("<option value='", choices, "'", sel_str, ">", choices, "</option>", collapse = "")
      }
      res <- paste0(res, optlist, "</select>")
      return(res)
    }
    #ranges in frequencies and times from spectrogram plot
    fs <- specplot_range$y
    ts <- specplot_range$x
    time1 <- tab_num_input(lab_df, lab_df$start_time, "start_time", ts[1], lab_df$end_time, diff(ts)/100)
    time2 <- tab_num_input(lab_df, lab_df$end_time,   "end_time",   lab_df$start_time, ts[2], diff(ts)/100)
    freq1 <- tab_num_input(lab_df, lab_df$start_freq, "start_freq", fs[1], lab_df$end_freq, diff(fs)/100)
    freq2 <- tab_num_input(lab_df, lab_df$end_freq,   "end_freq",   lab_df$start_freq, fs[2], diff(fs)/100)
    
    lab_df$start_time <- time1
    lab_df$end_time   <- time2
    lab_df$start_freq <- freq1
    lab_df$end_freq   <- freq2
    
    conf <- tab_num_input(lab_df, lab_df$confidence, "confidence", 0, 1, 0.1)
    lab_df$confidence <- conf
    
    choices <- union(class_label(), fullData()$class_label)
    
    class1 <- tab_class_input(lab_df, lab_df$class_label, choices, 'class_label')
    lab_df$class_label <- class1
    
    return(lab_df)
  },
  striped  = TRUE,
  bordered = TRUE,
  sanitize.text.function = function(x) x)
  
  summary_df <- reactive({
    df <- fullData()
    df <- df[df$file_name %in% file_list() & 
               df$labeler == labeler(),]
    
    if(input$summaryTabGroup)
      sum_df <- df %>%
        tabyl(class_label, file_name) %>%
        #adorn_totals("row") %>%
        tidyr::gather(file_name, n, 2:ncol(.), convert = TRUE) %>%
        select(file_name, class_label, n)
    else
      sum_df <- df %>%
        tabyl(file_name) %>%
        #adorn_totals("row") %>%
        select(file_name, n)
    
    #sum_df$num_labels <- as.integer(sum_df$num_labels)
    sum_df$n <- as.integer(sum_df$n)
    sum_df <- sum_df %>%
      rename(num_labels = n)
    
    create_btns <- function(x) {
      x %>%
        purrr::map_chr(~
                         paste0(
                           '<div class = "btn-group">
                   <button class="btn btn-default action-button btn-info action_button" id="edit_',
                           .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-edit"></i></button>
                   <button class="btn btn-default action-button btn-danger action_button" id="delete_',
                           .x, '" type="button" onclick=get_id(this.id)><i class="fa fa-trash-alt"></i></button></div>'
                         ))
    }
    
    dtShinyInput <- function(FUN, len, id, ...) {
      inputs <- character(len)
      for (i in seq_len(len))
        inputs[i] <- as.character(FUN(paste0(id, i), ...))
      return(inputs)
    }
    
    sum_df$Actions <- dtShinyInput(actionButton, nrow(sum_df), 'button_', label = "Go to File", 
                                 onclick = 'Shiny.onInputChange("dt_select_button", this.id)')
    
    return(sum_df)
  })
  
  output$labSummaryTable <- renderDataTable({
    summary_df()
  }, filter = "top", server = FALSE, escape = FALSE, selection = 'none')
  
  observeEvent(input$dt_select_button, {
    selectedRow <- as.numeric(strsplit(input$dt_select_button, "_")[[1]][2])
    updateSelectInput(inputId = "file1", selected = summary_df()[selectedRow,1])
    segment_num(1)
    segment_total(1)
    segment_end_s(1)
    segment_start(0)
  })
  
  input_list <- function(x){
    a <- list()
    for(name in x)
      a <- append(a,input[[name]])
    return(a)
  }
  
  t1Inputs <- reactive({
    x <- input_names("start_time")
    return(input_list(x))
  }) %>% throttle(100)
  
  t2Inputs <- reactive({
    x <- input_names("end_time")
    return(input_list(x))
  }) %>% throttle(100)
  
  f1Inputs <- reactive({
    x <- input_names("start_freq")
    return(input_list(x))
  }) %>% throttle(100)
  
  f2Inputs <- reactive({
    x <- input_names("end_freq")
    return(input_list(x))
  }) %>% throttle(100)
  
  confInputs <- reactive({
    x <- input_names("confidence")
    return(input_list(x))
  }) %>% throttle(100)
  
  classInputs <- reactive({
    x <- input_names("class_label")
    return(input_list(x))
  }) %>% throttle(100)
  
  overwriteLabData <- function(vals, col_name){
    if(is.null(vals))
      return(NULL)
    full_df <- fullData()
    lab_df  <- labelsData()
    #columns in right order
    lab_df  <- lab_df[,colnames(full_df)]
    
    #which inputs (in display table) differ from saved data
    #there should be at most one changed
    diff_idx    <- which(vals!=lab_df[,col_name][1:length(vals)])
    if(length(diff_idx)>0){
      changed_row <- lab_df[diff_idx,]
      #change to the new edited value
      changed_row[,col_name] <- vals[diff_idx]
      changed_idx <- which(full_df$date_time == changed_row$date_time &
                           full_df$file_name == changed_row$file_name)
      #change value in full data and overwrite
      full_df[changed_idx,] <- changed_row
      write_labs(full_df, append = FALSE, col.names = TRUE)
      fullData(full_df)
    }
  }
  
  #If any of the inputs in the label info table change
  #edit the values in saved data
  observeEvent(t1Inputs(), {
    overwriteLabData(unlist(t1Inputs()), 'start_time')
  })
  
  observeEvent(t2Inputs(), {
    overwriteLabData(unlist(t2Inputs()), 'end_time')
  })
  
  observeEvent(f1Inputs(), {
    overwriteLabData(unlist(f1Inputs()), 'start_freq')
  })
  
  observeEvent(f2Inputs(), {
    overwriteLabData(unlist(f2Inputs()), 'end_freq')
  })
  
  observeEvent(confInputs(), {
    overwriteLabData(unlist(confInputs()), 'confidence')
  })
  
  observeEvent(classInputs(), {
    overwriteLabData(unlist(classInputs()), 'class_label')
  })
  
  output$start_ui <- renderUI({
    if(.is_null(input$file1))
      actionButton("start_labelling", "Start Labelling!",
                   class = "btn-success")
    else
      actionButton("end_labelling", "End Labelling",
                   class = "btn-danger")
    
  })
  
  output$user_ui <- renderUI({
    labeler()
  })
  
  output$downloadAudio_ui <- renderUI({
    div(
      HTML("<b>Download:</b>"), 
      downloadButton("downloadWav", label=NULL, style = "width:100%;")
    )
  })
  
  observeEvent(input$start_labelling, {
    full_df <- fullData()
    filenms <- file_list()
    updateSelectInput(inputId  = "file1",
                      choices  = filenms)
  })
  
  observeEvent(input$end_labelling, {
    updateSelectInput(inputId  = "file1",
                      selected = "",
                      choices  = c(""))
  })
  
  output$label_ui <- renderUI({
    base_cols  <- c('darkgreen', 'limegreen')
    misc_cols  <- c('yellow', 'orange')
    extra_cols <- c('cyan4', 'cyan')
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
                        #  display: flex;
                        #  flex-flow: row wrap;
                        #  flex: auto;
                        #}"
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
        ticks = FALSE,
        width = "100%"
      ))
  })
  
  audioInput <- reactive({
    if(.is_null(input$file1))     
      return(NULL) 
    tmp_audio <- readWave(file.path(getwd(), "www", lab_nickname(), input$file1))
    
    #based on torchaudio::functional_gain
    audio_gain     <- function (waveform, gain_db = 0){
      if(gain_db == 0)
        return(waveform)
      return(waveform * 10^(gain_db/20))
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
    t_step <- input$t_step
    if(len_s > t_step){
      time_seq  <- seq(from = 0, to = len_s, by = t_step)
      tc        <- time_seq[segment_num()]
      segment_start(tc)
      segment_total(length(time_seq))
      tmp_audio <- extractWave_t(tmp_audio, c(tc,tc+t_step))
      x_coords(c(tc, tc + t_step))
      if(length(tmp_audio) < t_step*tmp_audio@samp.rate)
        tmp_audio@left <- c(tmp_audio@left, rep(1, t_step*tmp_audio@samp.rate-length(tmp_audio)))
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
    writeWave(tmp_audio, file.path('www', lab_nickname(), 'tmp.wav'))
    return(tmp_audio)
  })
  
  cleanInput <- reactive({
    tmp_audio <- audioInput()
    if(is.null(tmp_audio) | is.null(input$frequency_range))
      return(NULL)
    
    if(!is.null(ranges_osc$x) | !is.null(ranges_spec$x)){
      #time crop
      if(!is.null(ranges_osc$x))
        tc <- ranges_osc$x
      if(!is.null(ranges_spec$x))
        tc <- ranges_spec$x
      tc <- sort(tc) - segment_start()
      tmp_audio <- extractWave_t(tmp_audio, tc)
    } else if(!is.null(dc_ranges_osc$x) | !is.null(dc_ranges_spec$x)){
      #time crop
      if(!is.null(dc_ranges_osc$x))
        tc <- dc_ranges_osc$x
      if(!is.null(dc_ranges_spec$x))
        tc <- dc_ranges_spec$x
      tc <- sort(tc) - segment_start()
      tmp_audio <- extractWave_t(tmp_audio, tc)
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
    
    if(!frange_check(frange, range(tmp_spec$freq)) & 
       (input$noisereduction == 'None'))
      return(NULL)
    
    if(input$noisereduction != 'None'){
      audio_clean$noisered <- TRUE
      complex_spec <- spectro(tmp_audio,
                              f        = tmp_audio@samp.rate, 
                              wl       = input$window_width, 
                              ovlp     = input$fft_overlap,
                              complex  = TRUE,
                              plot     = FALSE,
                              norm     = FALSE,
                              dB       = NULL)
      
      noisered  <- noise_reduce(input$noisereduction)
      #noise is the geometric median of each row/column
      specnoise <- t(apply(complex_spec$amp, MARGIN=noisered, geometric_median))
      specnoise_complex <- complex(real=specnoise[,1], imaginary=specnoise[,2])
      if(input$noisereduction == "Rows")
        noise_mat <- matrix(rep(specnoise_complex, ncol(complex_spec$amp)),
                            ncol = ncol(complex_spec$amp))
      else
        noise_mat <- matrix(rep(specnoise_complex, nrow(complex_spec$amp)),
                            nrow = nrow(complex_spec$amp), byrow = TRUE)
      
      complex_spec$amp <- complex_spec$amp - noise_mat
      
      audio_inv <- istft(complex_spec$amp,
                         f    = tmp_audio@samp.rate,
                         wl   = input$window_width, 
                         ovlp = input$fft_overlap,
                         out  = "Wave")
      tmp_audio <- normalize(audio_inv, unit = "16")
    } else
      audio_clean$noisered <- FALSE
    
    if(frange_check(frange, range(tmp_spec$freq))){
      audio_clean$select <- TRUE
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
      if(input$clean_zero)
        complex_spec$amp[out_freq,] <- 0
      else
        complex_spec$amp[out_freq,] <- complex_spec$amp[out_freq,]/100
      audio_inv <- istft(complex_spec$amp,
                         f    = tmp_audio@samp.rate,
                         wl   = input$window_width, 
                         ovlp = input$fft_overlap,
                         out  = "Wave")
      tmp_audio <- normalize(audio_inv, unit = "16")
    } else
      audio_clean$select <- FALSE
    writeWave(tmp_audio, file.path('www', lab_nickname(), 'tmp_clean.wav'))
    return(tmp_audio)
  })
  
  gettime <- reactive({list(x=input$get_time)})
  
  gettime_t <- gettime %>% throttle(50)
  
  output$segmentNumText <- renderUI({
    if(.is_null(input$file1))
      return(NULL)
    txt <- ""
    if(segment_total() > 1){
      if(segment_num() == segment_total())
        seg_colour <- 'green'
      else
        seg_colour <- 'red'
      txt <- paste(txt, 'Segment: <span style="color: ', seg_colour, ';"><b>(', 
                   segment_num(), '/', segment_total(), ')</b></span>')
    }
    return(HTML(txt))
  })
  
  specData <- reactive({
    if(.is_null(input$file1))     
      return(data.frame(time        = 0,
                        frequency   = 0,
                        amplitude   = -Inf,
                        freq_select = 1))
    tmp_audio <- audioInput()
    
    noisered <- noise_reduce(input$noisereduction)
    
    #wl <- as.integer(length(tmp_audio)/100) 
    #wl <- input$window_width_disp
    #if(!is.null(dc_ranges_spec$x)){
    #  xr <- dc_ranges_spec$x
    #  wl <- as.integer(tmp_audio@samp.rate*(xr[2]-xr[1])/100)
    #wl <- as.integer(wl/10) 
    #}
    #wl <- wl - wl %% 2
    
    spec <- spectro(tmp_audio,
                    f        = tmp_audio@samp.rate, 
                    wl       = input$window_width_disp, 
                    ovlp     = input$fft_overlap_disp, 
                    plot     = FALSE,
                    noisereduction = noisered,
                    dB = input$spec_db)
    
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
    df <- specData()
    
    y_breaks <- pretty(df$frequency, 5)
    if(!is.null(dc_ranges_spec$y))
      y_breaks <- pretty(dc_ranges_spec$y, 5)
    
    x_breaks <- pretty(df$time, 5)
    if(!is.null(dc_ranges_spec$x))
      x_breaks <- pretty(dc_ranges_spec$x, 5)
    
    p <- plot_spectrogram(df, input, length_ylabs, dc_ranges_spec, 
                          specplot_range, x_breaks, y_breaks)
    
    gb   <- ggplot_build(p)
    specplot_range$x <- gb$layout$panel_params[[1]]$x.range
    specplot_range$y <- gb$layout$panel_params[[1]]$y.range
    
    #if(!is.null(input$file1))
    #  spec_name_raw <- gsub('.wav', '_spec_raw.png', input$file1)
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
  
  specPlotFront <- reactive({
    df <- specData()
    
    y_breaks <- pretty(df$frequency, 5)
    if(!is.null(dc_ranges_spec$y))
      y_breaks <- pretty(dc_ranges_spec$y, 5)
    
    x_breaks <- pretty(df$time, 5)
    if(!is.null(dc_ranges_spec$x))
      x_breaks <- pretty(dc_ranges_spec$x, 5)
    
    p <- plot_spec_front(input, length_ylabs, specplot_range, x_breaks, y_breaks)
    
    if(!is.null(x_coords()))
      p <- p + coord_cartesian(xlim = x_coords(),
                               expand = FALSE)
    if(!is.null(dc_ranges_spec$y))
      p <- p + coord_cartesian(ylim = dc_ranges_spec$y,
                               xlim = dc_ranges_spec$x,
                               expand = FALSE, default = TRUE)
    
    return(p)
  })
  
  output$specplot_ui <- renderUI({
    div(
      tags$head(tags$style(HTML(plot_z_style))),
      plotOutput(
        "specplot_front",
        height   = input$spec_height,
        width    = "100%",
        click    = "specplot_click",
        dblclick = "specplot_dblclick",
        brush    = brushOpts(
          id         = "specplot_brush",
          resetOnNew = TRUE),
        hover    = hoverOpts(
          id        = "specplot_hover",
          delay     = 10,
          delayType = "debounce"
        )
      ),
      plotOutput(
        "specplot_time",
        height = input$spec_height,
        width  = "100%"
      ),
      plotOutput(
        "specplot_freq",
        height = input$spec_height,
        width  = "100%"
      ),
      plotOutput(
        "specplot",
        height = input$spec_height,
        width  = "100%"
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
              $("#specplot_front").mousemove(function(e){ 
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
  })
  
  output$specplot <- renderPlot({
    return(specPlot())
  })
  
  output$specplot_freq <- renderPlot({
    if(.is_null(input$file1) | !input$spec_time)
      return(NULL)
    spec_plot <- specPlotFront()
    
    df <- specData()
    frange <- input$frequency_range
    
    if(!is.null(frange))
      if(frange_check(frange, range(df$frequency)))
        spec_plot <- spec_plot + 
      geom_rect(aes(xmin = -Inf, 
                    xmax = Inf, 
                    ymin = frange[2], 
                    ymax = Inf),
                fill = 'black', alpha = .5) +
      geom_rect(aes(xmin = -Inf, 
                    xmax = Inf, 
                    ymin = -Inf, 
                    ymax = frange[1]),
                fill = 'black', alpha = .5)
    return(spec_plot)
  }, bg="transparent")
  
  output$specplot_time <- renderPlot({
    if(.is_null(input$file1) | !input$spec_time)
      return(NULL)
    spec_plot <- specPlotFront()
    if(!is.null(ranges_spec$y)){
      if(input$include_guides)
        spec_plot <- spec_plot + 
          geom_hline(aes(yintercept = ranges_spec$y[1]), colour="yellow", linetype="dashed", alpha=0.4) +
          geom_hline(aes(yintercept = ranges_spec$y[2]), colour="yellow", linetype="dashed", alpha=0.4) +
          geom_vline(aes(xintercept = ranges_spec$x[1]), colour="yellow", linetype="dashed", alpha=0.4) +
          geom_vline(aes(xintercept = ranges_spec$x[2]), colour="yellow", linetype="dashed", alpha=0.4)
      spec_plot <- spec_plot + 
        geom_segment(aes(x=ranges_spec$x[1]+gettime_t()$x, 
                         xend=ranges_spec$x[1]+gettime_t()$x,
                         y=ranges_spec$y[1], yend=ranges_spec$y[2]),
                     colour = "yellow")
    } else if(!is.null(dc_ranges_spec$y))
      spec_plot <- spec_plot + geom_vline(aes(xintercept=dc_ranges_spec$x[1]+gettime_t()$x), colour = "yellow")
    else
      spec_plot <- spec_plot + geom_vline(aes(xintercept=gettime_t()$x), colour = "yellow")
    return(spec_plot)
  }, bg="transparent")
  
  output$specplot_front <- renderPlot({
    if(.is_null(input$file1))
      return(NULL)
    spec_plot <- specPlotFront()
    lab_df    <- labelsData()
    if(is.null(lab_df))
      return(spec_plot)
    else if(input$spec_labs){
      #if(input$hide_other_labels)
      #  lab_df <- lab_df[lab_df$labeler == labeler(),]
      lab_df <- lab_df %>% 
        filter(between(start_time, segment_start(), segment_start()+input$t_step))
      if(!is.null(dc_ranges_spec$x)){
        lab_df <- lab_df[lab_df$end_time >= dc_ranges_spec$x[1] & lab_df$start_freq < dc_ranges_spec$y[2],]
        lab_df$start_time_crop <- sapply(lab_df$start_time, function(x) max(x, dc_ranges_spec$x[1]))
        lab_df$end_freq_crop   <- sapply(lab_df$end_freq, function(y) min(y, dc_ranges_spec$y[2]))
        hj <- 0
        vj <- 1
      } else {
        lab_df$start_time_crop <- lab_df$start_time
        lab_df$end_freq_crop   <- lab_df$end_freq
        hj <- 0
        vj <- 0
      }
      spec_plot <- spec_plot +
        geom_rect(data = lab_df, 
                  mapping = aes(xmin = start_time,
                                xmax = end_time,
                                ymin = start_freq, 
                                ymax = end_freq),
                  colour = as.vector(lab_df$typecol),
                  fill   = "lightgrey",
                  alpha  = 0.15) +
        geom_label(data = lab_df,
                   aes(x     = start_time_crop,
                       y     = end_freq_crop,
                       label = class_label),
                   colour  = as.vector(lab_df$typecol),
                   label.r = unit(0, units="lines"),
                   label.size = 0.5,
                   hjust  = hj,
                   vjust  = vj
        ) +
        geom_label(data = lab_df,
                   aes(x     = start_time_crop,
                       y     = end_freq_crop,
                       label = class_label),
                   label.r = unit(0, units="lines"),
                   label.size = 0,
                   hjust  = hj,
                   vjust  = vj,
                   alpha  = 0,
                   colour = "black")
    }
    return(spec_plot)
  }, bg="transparent")
  
  observeEvent(input$savespec, {
    if(!.is_null(input$file1))
      spec_name <- gsub('.wav', '_spec.png', input$file1)
    else
      spec_name <- 'blank_spec.png'
    file_nm <- file.path(getwd(), "images", spec_name)
    width   <- session$clientData$output_specplot_width
    height  <- session$clientData$output_specplot_height
    # For high-res displays, this will be greater than 1
    pixelratio <- session$clientData$pixelratio
    ggsave(file_nm, specPlot(),
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
      osc_name <- gsub('.wav', '_osc.png', input$file1)
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
      return(btw(point$time, df$start_time, df$end_time) &
               btw(point$frequency, df$start_freq, df$end_freq))
    }
    #lab_df <- labelsData()
    #lab_df <- lab_df[in_label_box(lab_df, point),]
    
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
  
  observeEvent(input$keys, {
    if(input$keys == "shift+space")
      js$audiotoggle()
    else if(input$keys == "shift+enter")
      click("save_points")
    else if(input$keys == "shift+backspace")
      click("remove_points")
    else if(input$keys == "shift+left") 
      click("prev_file")
    else if(input$keys == "shift+right")
      click("next_file")
    else if(input$keys == "shift+n")
      click("start_labelling")
    else if(input$keys == "shift+w")
      click("end_labelling")
    else if(input$keys %in% paste(1:9)) 
      updateRadioGroupButtons(inputId = "label_points", 
                              selected = class_label()[min(length(class_label()),as.integer(input$keys))])
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
      return(btw(point$time, df$start_time, df$end_time))
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
    } else
      reset_ranges(ranges_spec)
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
    if(!is.null(ranges_spec$x)) {
      if(is.null(input$call_type))
        call_type <- ""
      else
        call_type <- paste(input$call_type[!.is_null(input$call_type)], 
                           collapse = '; ')
      lab_df <- data.frame(date_time   = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                           file_name   = input$file1,
                           start_time  = ranges_spec$x[1],
                           end_time    = ranges_spec$x[2],
                           start_freq  = ranges_spec$y[1],
                           end_freq    = ranges_spec$y[2],
                           class_label = input$label_points,
                           call_type   = call_type,
                           confidence  = input$label_confidence,
                           notes       = input$notes,
                           labeler     = labeler())
      typecol <- as.vector(cat_colours(lab_df)$typecol)
      
      full_df <- fullData()
      if(!is.null(full_df)){
        inp_list  <- names(input)
        tab_check <- paste0("tab_", which(file_list() == input$file1), "class_label")
        inp_list  <- inp_list[startsWith(inp_list, tab_check)]
        inp_ids   <- sapply(inp_list, function(x) str_sub(x, start = str_length(tab_check)+1))
        lab_df$id <- 1
        write_labs(lab_df)
        fullData(rbind(full_df, lab_df))
        filenames <- file_list()
        names(filenames) <- sapply(filenames, function(x) filename_pre(x,fullData()))
        updateSelectInput(inputId  = "file1",
                          choices  = filenames,
                          selected = input$file1)
      } else {
        lab_df$id <- 1
        write_labs(lab_df, append = FALSE, col.names = TRUE)
        fullData(lab_df)
      }
      showNotification(HTML(paste0('Label ', '<span style="color: ', 
                                   typecol, ';"><b>', 
                                   input$label_points, 
                                   '</b></span> successfully saved!')), 
                       type = "message")
      updateTextAreaInput(inputId = "notes", value="")
    } else
      showNotification("Label not saved, nothing selected!", type = "error")
  })
  
  observeEvent(input$remove_points, {
    if(!is.null(ranges_spec$x)) {
      lab_df <- data.frame(start_time = ranges_spec$x[1],
                           end_time   = ranges_spec$x[2],
                           start_freq = ranges_spec$y[1],
                           end_freq   = ranges_spec$y[2])
      full_df    <- fullData()
      full_df_rm <- c()
      for(idx in 1:nrow(full_df)){
        bb_cols  <- c('start_time', 'end_time', 'start_freq', 'end_freq')
        check_df <- full_df[idx,]
        row_iou  <- bb_iou(lab_df[,bb_cols], check_df[,bb_cols])
        if(row_iou > 0.6)
          full_df_rm <- c(full_df_rm, idx)
      }
      if(!is.null(full_df_rm)){
        deleted_lab$rows <- full_df_rm
        deleted_lab$data <- full_df[full_df_rm,]
        full_df <- full_df[-full_df_rm,]
        fullData(full_df)
        write_labs(full_df, append = FALSE, col.names = TRUE)
        showNotification("Label removed, click Undo to bring back", 
                         type = "message")
        filenames <- file_list()
        names(filenames) <- sapply(filenames, function(x) filename_pre(x,fullData()))
        updateSelectInput(inputId  = "file1",
                          choices  = filenames,
                          selected = input$file1)
      }
    } else
      showNotification("Label not removed, nothing selected!", type = "error")
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
      deleted_lab$rows <- deleted_lab$data <- NULL
      write_labs(full_df, append = FALSE, col.names = TRUE)
      fullData(full_df)
      showNotification("Label recovered", type = "message")
    } else
      showNotification("Nothing undone, no deletions detected!", type = "error")
  })
  
  output$my_audio <- renderUI({
    audio_style <- "width: 100%;"
    if(!is.null(cleanInput())){
      file_name   <- file.path('www', lab_nickname(), 'tmp_clean.wav')
      audio_style <- paste(audio_style, "filter: sepia(50%);")
    } else
      file_name <- file.path('www', lab_nickname(), 'tmp.wav')
    
    audio_style <- HTML(audio_style)
    if(.is_null(input$file1))     
      return(tags$audio(id       = 'my_audio_player',
                        src      = "", 
                        type     = "audio/wav", 
                        controls = NA, 
                        style    = audio_style
      ))
    pb <- input$playbackrate
    pb_script <- NULL
    if(pb != 1){
      pb_script <- paste0(
        'var audio = document.getElementById("my_audio_player");
       audio.playbackRate = ', pb, ';')
    }
    div(
      tags$audio(id       = 'my_audio_player',
                 src      = markdown:::.b64EncodeFile(file_name), 
                 type     = "audio/wav", 
                 controls = "controls",#HTML('controlsList: nodownload'),
                 #TODO: HTML styling (background colour, no download button,...)
                 style    = audio_style),
      tags$script(pb_script)
    )
  })
  
  output$audio_title <- renderUI({
    base <- "<b>Play audio:<b/>"
    if(!is.null(cleanInput())){
      if(audio_clean$select)
        base <- paste(base, '<span style="color: red;">(selected)</span>')
      if(audio_clean$noisered)
        base <- paste(base, '<span style="color: red;">(noise reduction)</span>')
    }
    HTML(base)
  })
  
  output$meta_text <- renderUI({
    if(!is.null(audioInput())){
      #latlong <- c(52.208330, -6.594489)
      dt <- get_audio_dt(input$file1)
      df <- get_audio_recdf(input$file1)
      latlong <- NULL
      if(!is.null(df))
        latlong <- c(df$lat, df$long)
      wf_address <- ifelse(!is.null(df), 
                           paste0(df$wind_farm_name, ", Co.", df$wind_farm_county),
                           "")
      wf_latlong <- ifelse(!is.null(latlong), 
                           paste(dd2dms(latlong[1], "lat"), dd2dms(latlong[2], "long")),
                           "")
      wf_link <- ""
      if(!is.null(latlong))
        wf_link <- get_gmap_link(latlong)
      wf_d2c <- ""
      if(!is.null(df))
        wf_d2c <- m2km(df$dist_to_coastline)
      div(#HTML(base),
        tags$style(".panel-heading{font-size: 75%; padding: 0%;}"),
        tags$style("#collapseExample{font-size: 85%; padding: 0%;}"),
        bsCollapse(id = "collapseExample",
                   bsCollapsePanel("Meta Information", 
                                   #HTML("<b>filename: </b>"), input$file1, br(),
                                   HTML("<b>Time recorded: </b>"), as.character(dt), br(),
                                   HTML("<b>Location: </b>"), wf_address, br(),
                                   HTML("<b>Coordinates: </b>"), 
                                   wf_latlong, wf_link, br(),
                                   HTML("<b>Distance to coastline: </b>"),
                                   wf_d2c,
                                   #paste(dd2dms(latlong[1], "lat"), dd2dms(latlong[2], "long")),
                                   #get_gmap_link(latlong),
                                   style = "info")
        ))
    } else
      return(NULL)
  })
  
  output$audio_time <- renderText({
    if(.is_null(input$file1))
      return(NULL)
    else
      return(input$get_time)
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
  
  output$downloadSpec <- downloadHandler(
    filename = function() {
      paste0("tmp_spec-", Sys.Date(), ".csv")
    },
    content  = function(file) {
      write.csv(specData(), file, row.names = FALSE)
    }
  )
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("data-", Sys.Date(), ".csv")
    },
    content  = function(file) {
      write.csv(saveData(), file)
    }
  )
  
  output$downloadWav <- downloadHandler(
    filename = function() {
      input$file1
    },
    content  = function(file) {
      savewav(audioInput(), filename = file)
    }
  )
  
  # move to previous file (resetting zoom)
  observeEvent(input$prev_file, {
    idx <- which(input$file1 == file_list()) - 1
    if(idx == 0)
      showNotification("Cannot go to previous file, at beginning of folder", 
                       type = "error")
    else {
      updateSelectInput(inputId  = "file1",
                        selected = file_list()[idx])
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
    idx <- which(input$file1 == file_list()) + 1
    if(idx > length(file_list()))
      showNotification("Cannot go to next file, at end of folder", 
                       type = "error")
    else {
      updateSelectInput(inputId  = "file1",
                        selected = file_list()[idx])
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
  
  observeEvent(input$reset_sidebar, {
    shinyjs::reset("side-panel")
  })
  
  observeEvent(input$reset_body, {
    shinyjs::reset("main-panel")
  })
  
  output$loadScript <- renderUI({
    #JS code to change input val for single input
    inp_script <- function(name){
      return(paste0("Shiny.addCustomMessageHandler('",
      name,"', function(value) { ",
      "Shiny.setInputValue('", name,"', value);"#,
      #"$('#",name,"').val(value);});"
      ))
    }
    tags$script(paste0(sapply(names(input), inp_script), collapse = " "))
  })
  
  observeEvent(input$inputLoad, {
    #print(input$inputLoad)
    fname <- conf_filename()
    if(file.exists(fname)){
      inputa <- readRDS(fname)
      skips <- c("folder", "prev_file", "next_file",
                 "lab_hist-history_back", "lab_hist-history_forward",
                 "reset_sidebar", # this caused the delay in reloading last file
                 "sidebarItemExpanded",  "side-panel", 
                 "inputLoad", "._auth0logout_",
                 "start_labelling", "end_labelling", "savespec", 
                 "addCategory", "remCategory", "resetCategory", 
                 "save_points", "remove_points", "undo_delete_lab")
      for(nm in names(inputa)){
        if(nm %in%skips)
          next
        #session$sendCustomMessage(nm, inputa[[nm]])
        session$sendInputMessage(nm, list(value = inputa[[nm]]))
      }
      showNotification("Previous Settings loaded", type = "message", duration = NULL)
    } else
      showNotification("No previous settings to load", type = "warning", duration = NULL)
  })
  
  session$onSessionEnded(function() {
    isolate(saveRDS(input, file = conf_filename()))
  })
}

#profvis(runApp(), prof_output = file.path(getwd(),'profiling'))

#auth0::use_auth0(overwrite = TRUE)
#usethis::edit_r_environ()
#options(shiny.port = 8080)
#auth0::shinyAppAuth0(ui_func(), server)
shinyApp(ui_func(), server)

# tell shiny to log all reactivity
#reactlog::reactlog_enable()

# run a shiny app
#shinyApp(ui_func(), server)

# once app has closed, display reactlog from shiny
#shiny::reactlogShow()
