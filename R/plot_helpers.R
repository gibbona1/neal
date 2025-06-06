#' @import ggplot2
#' @import dplyr
#' @importFrom graphics axis

#theming based on: https://rug.mnhn.fr/seewave/spec.html

osc_theme <- theme(panel.grid.major.y = element_line(color = "black",    linetype = "dotted"),
                   panel.grid.major.x = element_line(color = "darkgrey", linetype = "dashed"),
                   panel.grid.minor.x = element_blank(),
                   panel.grid.minor.y = element_blank(),
                   panel.background   = element_rect(fill = "transparent"),
                   panel.border       = element_rect(linetype = "solid", fill = NA, color = "grey"),
                   axis.line          = element_blank(),
                   legend.position    = "none",
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
                    legend.position    = "none",
                    plot.background    = element_rect(fill = "black"),
                    plot.margin        = margin(0.6, 0.9, 0.25, 0, "lines"),
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
                          legend.position    = "none",
                          plot.background    = element_rect(fill = "transparent", color = plt_col),
                          plot.margin        = margin(0.6, 0.9, 0.25, 0, "lines"),
                          axis.title.y       = element_blank(),
                          axis.title.x       = element_text(size = 10, color = plt_col, family = "mono"),
                          axis.text.y        = element_text(size = 14, color = plt_col, family = "mono"),
                          axis.text.x        = element_text(size = 14, color = plt_col, family = "mono"),
                          axis.ticks         = element_line(color = plt_col))

virpluscols <- c("#000000", "#440154FF", "#3B528BFF", "#21908CFF", "#5DC863FF", "#FDE725FF", "#ff0000")

palette_list <- c("viridisplus", "greyscale", "magma", "inferno", "plasma",
                  "viridis", "cividis", "rocket", "mako", "turbo")

.is_null <- function(x) return(is.null(x) | x %in% c("", "<NULL>"))

plot_oscillogram <- function(df, input, length_ylabs) {
  osc_plot <- ggplot(df)
  if (!is.null(df)) {
    osc_plot <- osc_plot +
      #stat_summary_bin(
      #  aes(x = time, y = amplitude), colour = NA, fill = "red",
      #  geom="bar", fun=mean, bins=500)
      geom_line(aes(x = df$time, y = df$amplitude), colour = "red")
    y_breaks <- pretty(df$amplitude, 3)
  } else {
    y_breaks <- -1:1
  }
  y_labels <- paste0(paste0(rep(" ", length_ylabs$osc), collapse = ""), y_breaks)

  osc_plot <- osc_plot +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(breaks = y_breaks,
                       expand = c(0.1, 0.1),
                       labels = y_labels
                       ) +
    xlab("Time (s)") +
    ylab("Amplitude") +
    geom_hline(yintercept = 0, colour = "white", linetype = "dotted") +
    osc_theme

  return(osc_plot)
}

palette_cols <- function(pal_name, n = 6) {
  if (pal_name == "viridisplus")
    return(virpluscols)
  else if (pal_name == "greyscale")
    return(grDevices::grey.colors(n, start = 0, end = 1))
  else
    return(viridis::viridis(n, option = pal_name))
}

plot_spectrogram <- function(df, canvas, input, length_ylabs, dc_ranges_spec, specplot_range, x_breaks, y_breaks) {
  sel_col <- palette_cols(input$palette_selected)
  if (input$palette_invert)
    sel_col <- rev(sel_col)

  if (!.is_null(input$file1)) {
    #get file extension (everything after last .)
    ext <- str_extract(input$file1, "\\.[^.]+$")
    spec_name_raw <- paste0(gsub(ext, "", input$file1), "_spec_raw.png")
    file_nm <- file.path(getwd(), "images", spec_name_raw)
  }
  
  spec_plot <- canvas +
    geom_raster(interpolate = as.logical(input$spec_interpolate)
                ) +
    xlab("Time (s)") +
    ylab("Frequency (kHz)") +
    scale_fill_gradientn(name     = "Amplitude\n(dB)\n",
                         colours  = sel_col,
                         limits   = c(-100, -20),
                         na.value = sel_col[1]) +
    scale_x_continuous(expand = c(0, 0),
                       breaks = x_breaks) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = y_breaks,
                       labels = paste0(paste0(rep(" ", length_ylabs$spec), collapse = ""),
                                       y_breaks, "kHz")
                       ) 
  return(spec_plot)
}

plot_spectrogram_base <- function(df, input, length_ylabs, dc_ranges_spec, specplot_range) {
  if (.is_null(input$file1))
    return(NULL)

  y_breaks <- pretty(df$frequency, 5)
  if (!is.null(dc_ranges_spec$y))
    y_breaks <- pretty(dc_ranges_spec$y, 5)

  x_breaks <- pretty(df$time, 5)
  if (!is.null(dc_ranges_spec$x))
    x_breaks <- pretty(dc_ranges_spec$x, 5)

  sel_col <- palette_cols(input$palette_selected)
  if (input$palette_invert)
    sel_col <- rev(sel_col)

  tmp_raster <- raster::rasterFromXYZ(df)
  #pmar <- par("mar")
  #poma <- par("oma")
  #par(oma=c(0,0,poma[3],poma[4]),
  #    mar=c(1.5,1.5,0,0),
  #    bg = "black") #b,l,t,r
  plot(raster::extent(tmp_raster), col = NA,
       family = "mono", xaxs = "i", yaxs = "i",
       xaxt = "n", yaxt = "n",
       col.axis = "grey", col.lab = "grey",
       xlab = "Time (s)", ylab = NA #"Frequency (kHz)"
       )
  plot(tmp_raster, add = TRUE,
       col = sel_col, colNA = sel_col[1],
       xlim = range(x_breaks),
       col.axis = "grey", col.lab = "grey",
       legend = FALSE,
       breaks = seq(-100, -20, length(sel_col)),
       interpolate = TRUE)
  axis(side = 1, at = x_breaks)
  axis(side = 2, at = y_breaks,
       las = 2,
       labels = paste0(paste0(rep(" ", length_ylabs$spec), collapse = ""),
                      y_breaks, "kHz")
  )
}

plot_spec_front  <- function(input, length_ylabs, specplot_range, x_breaks, y_breaks) {
  spec_plot <- ggplot(NULL) +
    xlab("Time (s)") +
    ylab("Frequency (kHz)") +
    scale_x_continuous(expand = c(0, 0),
                       breaks = x_breaks,
                       limits = specplot_range$x) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = y_breaks,
                       limits = specplot_range$y,
                       labels = paste0(paste0(rep(" ", length_ylabs$spec), collapse = ""),
                                       y_breaks, "kHz")
    ) +
    spec_theme_front
  return(spec_plot)
}