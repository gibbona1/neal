library(ggplot2)
library(png)
library(grid)

#theming based on: https://rug.mnhn.fr/seewave/spec.html

osc_theme <- theme(panel.grid.major.y = element_line(color = "black",    linetype = "dotted"),
                   panel.grid.major.x = element_line(color = "darkgrey", linetype = "dashed"),
                   panel.grid.minor.x = element_blank(),
                   panel.grid.minor.y = element_blank(),
                   panel.background   = element_rect(fill = "transparent"),
                   panel.border       = element_rect(linetype = "solid", fill = NA, color = "grey"),
                   axis.line          = element_blank(),
                   legend.position    = 'none',
                   plot.background    = element_rect(fill = "black"),
                   plot.margin        = unit(c(0.2, 0.9, 0.1, 0), "lines"),
                   axis.title         = element_blank(),
                   axis.text.x        = element_text(size = 16, color = "grey", family = "mono"),
                   axis.text.y        = element_text(size = 14, color = "grey", family = "mono"),
                   axis.ticks         = element_line(color = "grey"))
#TODO: minor ticks on x axis

spec_theme <- theme(panel.grid.major.y = element_line(color = "black", linetype = "dotted"),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor   = element_blank(),
                    panel.background   = element_rect(fill = "transparent"),
                    panel.border       = element_rect(linetype = "solid", fill = NA, color = "grey"),
                    axis.line          = element_blank(),
                    legend.position    = 'none',
                    plot.background    = element_rect(fill = "black"),
                    plot.margin        = margin(0.6,0.9,0.25,0, "lines"),
                    axis.title.y       = element_blank(),
                    axis.title.x       = element_text(size = 10, color = "lightgrey", family = "mono"), 
                    axis.text.y        = element_text(size = 14, color = "grey", family = "mono"),
                    axis.text.x        = element_text(size = 14, color = "grey", family = "mono"),
                    axis.ticks         = element_line(color = "grey"))
#debug colour to check alignment with main plot
plt_col <- NA#"dodgerblue"
spec_theme_front <- theme(panel.grid.major.y = element_line(color = plt_col, linetype = "dotted"),
                          panel.grid.major.x = element_blank(),
                          panel.grid.minor   = element_blank(),
                          panel.background   = element_rect(fill = NA),
                          panel.border       = element_rect(linetype = "solid", fill = NA, color = plt_col),
                          axis.line          = element_blank(),
                          legend.position    = 'none',
                          plot.background    = element_rect(fill='transparent', color=plt_col),
                          plot.margin        = margin(0.6,0.9,0.25,0, "lines"),
                          axis.title.y       = element_blank(),
                          axis.title.x       = element_text(size = 10, color = plt_col, family = "mono"), 
                          axis.text.y        = element_text(size = 14, color = plt_col, family = "mono"),
                          axis.text.x        = element_text(size = 14, color = plt_col, family = "mono"),
                          axis.ticks         = element_line(color = plt_col))

virpluscols <- c("#000000", "#440154FF", "#3B528BFF", "#21908CFF", "#5DC863FF", "#FDE725FF", "#ff0000")

palette_list <- c("viridisplus", "greyscale", "magma", "inferno", "plasma", 
                  "viridis", "cividis", "rocket", "mako", "turbo")

plot_oscillogram <- function(df, input, length_ylabs){
  lim16    <- 2^15-1
  osc_plot <- ggplot(df)
  if(!is.null(df)){
    osc_plot <- osc_plot + 
      #stat_summary_bin(
      #  aes(x = time, y = amplitude), colour = NA, fill = "red",
      #  geom="bar", fun=mean, bins=500)
      geom_line(aes(x = time, y = amplitude), colour = "red")
    y_breaks <- pretty(df$amplitude, 3)
  } else {
    y_breaks <- -1:1
  }
  y_labels <- paste0(paste0(rep(" ", length_ylabs$osc), collapse=''), y_breaks)
  
  osc_plot <- osc_plot + 
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(breaks = y_breaks,
                       expand = c(0.1,0.1),
                       labels = y_labels
                       ) +
    xlab("Time (s)") + 
    ylab("Amplitude") + 
    geom_hline(yintercept = 0, colour = "white", linetype = "dotted") +
    osc_theme
  
  lab_file <- "tmp_labels.csv"
  if(file.exists(lab_file)){
    lab_df <- read.csv(lab_file)
    lab_df <- lab_df[lab_df$file_name == input$file1,]
    if(nrow(lab_df)==0)
      return(osc_plot)
    if(input$osc_labs){
      osc_plot <- osc_plot +
        geom_rect(data = lab_df, 
                aes(xmin = start_time,
                    xmax = end_time,
                    ymin = -Inf, 
                    ymax = Inf),
        colour = "red",
        fill   = "lightgrey",
        alpha  = 0.15)
    }
  }
  return(osc_plot)
}

plot_spectrogram <- function(df, input, length_ylabs, dc_ranges_spec, specplot_range, x_breaks, y_breaks){
  palette_cols <- function(pal_name, n=6){
    if(pal_name == "viridisplus")
      return(virpluscols)
    else if(pal_name == "greyscale")
      return(grey.colors(n, start = 0, end = 1))
    else
      return(viridis(n, option = pal_name))
  }
  
  sel_col <- palette_cols(input$palette_selected)
  if(input$palette_invert)
    sel_col <- rev(sel_col)
  
  spec_plot <- ggplot(df)
  
  if(!is.null(input$file1) & input$file1 != '<NULL>'){
    spec_name_raw <- paste0(gsub('.wav', '', input$file1), '_spec_raw.png')
    file_nm <- file.path(getwd(), "images", spec_name_raw)
    
    #img_path <- 'images/SMU05115_20211104_064902_start_39_16_spec.png'
    #img <- readPNG(file_nm)
    #g <- rasterGrob(img, interpolate=TRUE,
    #                x=unit(0, "npc"),
    #                y=unit(0, "npc"), #
    #                hjust=0,
    #                width=unit(1,"npc"), 
    #                height=unit(1,"npc"))
    #spec_plot <- spec_plot + annotation_custom(g)
  }
  
  spec_plot <- spec_plot + 
    geom_raster(aes(x    = time,
                    y    = frequency, 
                    fill = amplitude),
                interpolate = TRUE
                ) +
    xlab("Time (s)") + 
    ylab("Frequency (kHz)") + 
    scale_fill_gradientn(name     = "Amplitude\n(dB)\n",
                         colours  = sel_col,
                         #limits   = c(-96,96), 
                         na.value = sel_col[1]) +
    scale_x_continuous(expand = c(0, 0),
                       breaks = x_breaks) +
    scale_y_continuous(expand = c(0, 0), 
                       breaks = y_breaks,
                       labels = paste0(paste0(rep(" ", length_ylabs$spec), collapse=''), 
                                       y_breaks, "kHz")
                       ) +
    spec_theme
  return(spec_plot)
}

plot_spec_front  <- function(input, length_ylabs, specplot_range, x_breaks, y_breaks){
  spec_plot <- ggplot(NULL) + 
    xlab("Time (s)") + 
    ylab("Frequency (kHz)") + 
    scale_x_continuous(expand = c(0, 0),
                       breaks = x_breaks,
                       limits = specplot_range$x) +
    scale_y_continuous(expand = c(0, 0), 
                       breaks = y_breaks,
                       limits = specplot_range$y,
                       labels = paste0(paste0(rep(" ", length_ylabs$spec), collapse=''), 
                                       y_breaks, "kHz")
    ) +
    spec_theme_front
  return(spec_plot)
}