library(ggplot2)

#theming based on: https://rug.mnhn.fr/seewave/spec.html

oscillo_theme_dark <- theme(panel.grid.major.y = element_line(color = "black", linetype = "dotted"),
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
                            axis.text.x        = element_text(size = 16, color = "grey"),
                            axis.text.y        = element_text(size = 14, color = "grey", family = "mono"),
                            axis.ticks         = element_line(color = "grey"))
#TODO: minor ticks on x axis

hot_theme_grid <- theme(panel.grid.major.y = element_line(color = "black", linetype = "dotted"),
                        panel.grid.major.x = element_blank(),
                        panel.grid.minor   = element_blank(),
                        panel.background   = element_rect(fill = "transparent"),
                        panel.border       = element_rect(linetype = "solid", fill = NA, color = "grey"),
                        axis.line          = element_blank(),
                        legend.position    = 'none',
                        plot.background    = element_rect(fill = "black"),
                        plot.margin        = margin(0.2,0.9,0.25,0, "lines"),
                        axis.title         = element_blank(),
                        axis.text.y        = element_text(size = 14, color = "grey", family = "mono"),
                        axis.text.x        = element_blank(),
                        axis.ticks         = element_line(color = "grey"),
                        axis.ticks.x       = element_blank())

virpluscols <- c("#000000", "#440154FF", "#3B528BFF", "#21908CFF", "#5DC863FF", "#FDE725FF", "#ff0000")

palette_list <- c("viridisplus", "greyscale", "magma", "inferno", "plasma", 
                  "viridis", "cividis", "rocket", "mako", "turbo")

plot_oscillogram <- function(df, input, length_ylabs){
  lim16    <- 2^15-1
  osc_plot <- ggplot(df)
  if(!is.null(df)){
    osc_plot <- osc_plot + 
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
    oscillo_theme_dark
  
  lab_df <- read.csv("tmp_labels.csv")
  lab_df <- lab_df[lab_df$file_name == input$file1,]
  if(nrow(lab_df)==0)
    return(osc_plot)
  if(input$osc_labs){
    pb_rate <- as.numeric(gsub("x", "", input$playbackrate))
    osc_plot <- osc_plot +
      geom_rect(data = lab_df, 
              aes(xmin = start_time / pb_rate,
                  xmax = end_time   / pb_rate,
                  ymin = -Inf, 
                  ymax = Inf),
      colour = "red",
      fill   = "lightgrey",
      alpha  = 0.15)
  }
  return(osc_plot)
}

plot_spectrogram <- function(df, input, length_ylabs){
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
  
  y_breaks <- pretty(df$frequency, 5)
  
  spec_plot <- ggplot(df) + 
    geom_raster(aes(x    = time,
                    y    = frequency, 
                    fill = amplitude),
                alpha       = df$freq_select,
                interpolate = TRUE
                ) +
    xlab("Time (s)") + 
    ylab("Frequency (kHz)") + 
    scale_fill_gradientn(name     = "Amplitude\n(dB)\n",
                         colours  = sel_col,
                         limits   = c(-96,96), 
                         na.value = sel_col[1]) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), 
                       breaks = y_breaks,
                       labels = paste0(paste0(rep(" ", length_ylabs$spec), collapse=''), 
                                       y_breaks, "kHz")
                       ) +
    hot_theme_grid
    
  lab_df <- read.csv("tmp_labels.csv")
  lab_df <- lab_df[lab_df$file_name == input$file1,]
  if(nrow(lab_df)==0)
    return(spec_plot)
  if(input$spec_labs){
    pb_rate <- as.numeric(gsub("x", "", input$playbackrate))
    spec_plot <- spec_plot +
      geom_rect(data = lab_df, 
                mapping = aes(xmin = start_time / pb_rate,
                              xmax = end_time   / pb_rate,
                              ymin = start_freq * pb_rate, 
                              ymax = end_freq   * pb_rate),
                colour = "green",
                fill   = "lightgrey",
                alpha  = 0.15)
  }
  return(spec_plot)
}