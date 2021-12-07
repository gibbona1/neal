library(ggplot2)
library(gridExtra)

oscillo_theme_dark <- theme(panel.grid.major.y = element_line(color="black", linetype = "dotted"),
                            panel.grid.major.x = element_blank(),
                            panel.grid.minor   = element_blank(),
                            panel.background   = element_rect(fill="transparent"),
                            panel.border       = element_rect(linetype = "solid", fill = NA, color = "grey"),
                            axis.line          = element_blank(),
                            legend.position    = "none",
                            plot.background    = element_rect(fill="black"),
                            plot.margin        = unit(c(0.3,0.2,0.2,0.1), "lines"),
                            axis.title.y       = element_text(size=16, color = "grey",
                                                              margin = margin(0,30,0,0)), # element_blank(),
                            axis.title.x       = element_text(size=14, color = "grey"),
                            #axis.text.y        = element_text(margin = margin(0,10,0,0)),
                            axis.text          = element_text(size=14, color = "grey"),
                            axis.ticks         = element_line(color="grey"))

hot_theme_grid <- theme(panel.grid.major.y   = element_line(color="black", linetype = "dotted"),
                        panel.grid.major.x   = element_blank(),
                        panel.grid.minor     = element_blank(),
                        panel.background     = element_rect(fill="transparent"),
                        panel.border         = element_rect(linetype = "solid", fill = NA, color = "grey"),
                        axis.line            = element_blank(),
                        legend.position      = "right",
                        legend.justification = "right",
                        legend.background    = element_rect(fill="black"),
                        legend.key.width     = unit(50, "native"),
                        legend.title         = element_text(size=16, color="grey"),
                        legend.text          = element_text(size=16, color="grey"),
                        plot.background      = element_rect(fill="black"),
                        plot.margin          = margin(0.2,0.2,0.3,0.1, "lines"),
                        axis.title.x         = element_blank(),
                        axis.title.y         = element_text(size=16, color = "grey",
                                                            margin = margin(0,30,0,0)),
                        axis.text            = element_text(size=16, color = "grey"),
                        axis.text.x          = element_blank(),
                        #axis.text.y          = element_text(margin = margin(0,10,0,0)),
                        axis.ticks           = element_line(color="grey"),
                        axis.ticks.x         = element_blank())

plot_oscillogram <- function(df){
  osc_plot <- ggplot(df)+
    geom_line(mapping = aes(x=time, y=amplitude), color="red")+ 
    #scale_x_continuous(labels=s_formatter, expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    xlab("Time (s)") + 
    ylab("Amplitude") + 
    geom_hline(yintercept = 0, color="white", linetype = "dotted") +
    scale_x_continuous(expand = c(0, 0)) +
    coord_cartesian(xlim = c(0, 15)) +
    oscillo_theme_dark
  
  osc_legend  <- get_legend(osc_plot)
  osc_plot    <- osc_plot  + theme(legend.position='none')
  p <- ggplot_gtable(ggplot_build(osc_plot))
  
  #browser()
  #fixed_width <- unit(4, 'cm')
  #p$widths[2:3] <- fixed_width
  #p1_widths(p$widths)
  return(p)
}

plot_spectrogram <- function(df){
  spec_plot <- ggplot(df, aes_string(x = 'time',
                                     y = 'frequency', 
                                     z = 'amplitude')) + 
    geom_raster(aes(fill = amplitude), 
                #alpha = (0.5 + 0.5*vals$keeprows), 
                interpolate = TRUE) +
    xlab("Time (s)") + 
    ylab("Frequency (kHz)") + 
    scale_fill_viridis("Amplitude\n(dB)\n") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_cartesian(xlim = c(0, 15)) +
    hot_theme_grid
  spec_legend <- get_legend(spec_plot)
  spec_plot   <- spec_plot + theme(legend.position='none')
  p <- ggplot_gtable(ggplot_build(spec_plot))
  
  #browser()
  #fixed_width <- unit(1, 'cm')
  #p$widths[2] <- fixed_width
  
  return(p)
}