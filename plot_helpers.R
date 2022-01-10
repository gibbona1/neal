library(ggplot2)
library(gridExtra)

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
                            axis.text.y        = element_text(size = 14, color = "grey"),
                            axis.ticks         = element_line(color = "grey"))
#TODO: minor ticks on x axis
#TODO: check lineup of left y axes - not exact and will be noticeable if oscillogram not normalized 
# or spectrogram frequency goes into triple digits)

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
                        axis.text.y        = element_text(size = 14, color = "grey"),
                        axis.text.x        = element_blank(),
                        axis.ticks         = element_line(color = "grey"),
                        axis.ticks.x       = element_blank())

virpluscols <- c("#000000", "#440154FF", "#3B528BFF", "#21908CFF", "#5DC863FF", "#FDE725FF", "#ff0000")

palette_list <- c("viridisplus", "magma", "inferno", "plasma", 
                  "viridis", "cividis", "rocket", "mako", "turbo")

plot_oscillogram <- function(df){
  lim16    <- 2^15-1
  osc_plot <- ggplot(df)
  if(!is.null(df))
    osc_plot <- osc_plot + 
      geom_line(aes(x = time, y = amplitude), color = "red")
  osc_plot <- osc_plot + 
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(breaks = c(-lim16,0,lim16),
                       expand = c(0.1,0.1),
                       limits = c(-lim16,lim16))+
    xlab("Time (s)") + 
    ylab("Amplitude") + 
    geom_hline(yintercept = 0, color = "white", linetype = "dotted") +
    oscillo_theme_dark
  return(osc_plot)
}

plot_spectrogram <- function(df, input){
  palette_cols <- function(pal_name, n=6){
    if(pal_name == "viridisplus")
      return(virpluscols)
    else
      return(viridis(n, option = pal_name))
  }
  
  sel_col <- palette_cols(input$palette_selected)
  if(input$palette_invert)
    sel_col <- rev(sel_col)
  
  spec_plot <- ggplot(df, aes_string(x = 'time',
                                     y = 'frequency', 
                                     z = 'amplitude')) + 
    geom_raster(aes(fill = amplitude), 
                #TODO: keep a box over each label, even load any in the csv that exist for this file
                #alpha = (0.5 + 0.5*vals$keeprows), 
                interpolate = TRUE
                ) +
    xlab("Time (s)") + 
    ylab("Frequency (kHz)") + 
    scale_fill_gradientn(name   = "Amplitude\n(dB)\n",
                         colors = sel_col,
                         limits = c(-96,96), 
                         na.value = sel_col[1]) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), 
                       breaks = pretty(df$frequency, 5),
                       labels = paste0(" ", pretty(df$frequency, 5), "kHz")
                       ) +
    hot_theme_grid
    browser()
  return(spec_plot)
}