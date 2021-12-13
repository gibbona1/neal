library(ggplot2)
library(gridExtra)

#theming based on: https://rug.mnhn.fr/seewave/spec.html

oscillo_theme_dark <- theme(panel.grid.major.y = element_line(color = "black", linetype = "dotted"),
                            panel.grid.major.x = element_line(color = "darkgrey", linetype = "dashed"),
                            panel.grid.minor.x = element_blank(),
                            panel.grid.minor.y = element_blank(),
                            panel.background   = element_rect(fill="transparent"),
                            panel.border       = element_rect(linetype = "solid", fill = NA, color = "grey"),
                            axis.line          = element_blank(),
                            legend.position    = 'none',
                            plot.background    = element_rect(fill="black"),
                            plot.margin        = unit(c(0.2,0.9,0.1,0.5), "lines"),
                            axis.title.y       = element_text(size=16, color = "grey",
                                                              margin = margin(0,30,0,0)), # element_blank(),
                            axis.title.x       = element_text(size=16, color = "grey",
                                                              margin = margin(0,0,0.2,0)),
                            axis.text          = element_text(size=16, color = "grey"),
                            axis.ticks         = element_line(color="grey"))
#TODO: minor ticks on x axis
#TODO: check lineup of left y axes with no axis text

hot_theme_grid <- theme(panel.grid.major.y   = element_line(color="black", linetype = "dotted"),
                        panel.grid.major.x   = element_blank(),
                        panel.grid.minor     = element_blank(),
                        panel.background     = element_rect(fill="transparent"),
                        panel.border         = element_rect(linetype = "solid", fill = NA, color = "grey"),
                        axis.line            = element_blank(),
                        legend.position      = 'none',
                        plot.background      = element_rect(fill="black"),
                        plot.margin          = margin(0.2,0.9,0.25,0.5, "lines"),
                        axis.title.x         = element_blank(),
                        axis.title.y         = element_text(size=16, color = "grey",
                                                            margin = margin(0,30,0,0)),
                        axis.text            = element_text(size=16, color = "grey"),
                        axis.text.x          = element_blank(),
                        axis.ticks           = element_line(color="grey"),
                        axis.ticks.x         = element_blank())

virpluscols <- c("#000000", "#440154FF", "#3B528BFF", "#21908CFF", "#5DC863FF", "#FDE725FF", "#ff0000")

plot_oscillogram <- function(df){
  osc_plot <- ggplot(df)
  if(!is.null(df))
    osc_plot <- osc_plot + 
      geom_line(aes(x = time, y = amplitude), color = "red")
  osc_plot <- osc_plot + 
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(breaks = -1:1, 
                       expand = c(0.1,0.1),
                       limits = c(-1,1))+
    xlab("Time (s)") + 
    ylab("Amplitude") + 
    geom_hline(yintercept = 0, color = "white", linetype = "dotted") +
    oscillo_theme_dark
  return(osc_plot)
}

plot_spectrogram <- function(df, input){
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
                         colors = virpluscols,
                         limits = c(-96,96), 
                         na.value = "black") +
    #TODO: options for palettes (e.g. viridis, viridisplus, RdYlBu, magma, inferno)
    #na.value would then be the min value of the palette
    #TODO: invert palette
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), 
                       breaks = pretty(df$frequency, 5)
                       ) +
    hot_theme_grid
  
  if(!is.null(input$plot1_brush)){
    res <- brushedPoints(df, input$plot1_brush,
                         xvar = 'time', yvar = 'frequency')
    spec_plot <- spec_plot + geom_raster(data = res, fill = 'green')
  }
  return(spec_plot)
}