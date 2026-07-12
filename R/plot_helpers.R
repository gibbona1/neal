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

# Max-pool `mat` along `margin` (1 = rows/frequency, 2 = columns/time) by
# collapsing every `factor` consecutive slices into one via max - not
# mean/stride, so a short transient call sitting in an otherwise-quiet bin
# doesn't get blurred away or skipped by the decimation (important for
# bioacoustic clicks/chirps). Vectorised as `factor` pmax() passes over the
# whole matrix rather than one pass per output group/pixel: for a spectrogram-
# sized matrix, looping per-group (thousands of tiny `apply()` calls) is
# *slower* than just colouring the full-resolution matrix directly, which
# defeats the point - this stays fast regardless of matrix size because the
# loop only ever runs `factor` times (typically 2-5).
pool_max <- function(mat, factor, margin) {
  n      <- dim(mat)[margin]
  factor <- max(1, round(factor))
  if (factor <= 1)
    return(mat)

  n_groups <- ceiling(n / factor)
  n_pad    <- n_groups * factor
  if (n_pad > n) {
    pad <- if (margin == 1)
      matrix(-Inf, nrow = n_pad - n, ncol = ncol(mat))
    else
      matrix(-Inf, nrow = nrow(mat), ncol = n_pad - n)
    mat <- if (margin == 1) rbind(mat, pad) else cbind(mat, pad)
  }

  result <- NULL
  for (j in seq_len(factor)) {
    idx    <- seq(j, n_pad, by = factor)
    slice  <- if (margin == 1) mat[idx, , drop = FALSE] else mat[, idx, drop = FALSE]
    result <- if (is.null(result)) slice else pmax(result, slice)
  }
  result
}

# Skip building/colouring more raster resolution than could ever be seen:
# only pools a dimension once it's well beyond (> factor x) the panel's
# actual pixel size, and leaves dimensions alone when the pixel size isn't
# known yet (e.g. before the first client render).
downsample_spec_amp <- function(amp, width_px, height_px, factor = 2) {
  if (!is.null(width_px) && ncol(amp) > factor * width_px)
    amp <- pool_max(amp, ceiling(ncol(amp) / width_px), margin = 2)
  if (!is.null(height_px) && nrow(amp) > factor * height_px)
    amp <- pool_max(amp, ceiling(nrow(amp) / height_px), margin = 1)
  amp
}

# Map an amplitude matrix straight to a colour raster image via a
# pre-computed lookup table, instead of handing ~n_freq*n_time individual
# observations to ggplot as data (geom_raster + aes(fill=) + a trained
# continuous scale is dramatically slower than one vectorised LUT lookup for
# spectrogram-sized matrices - see plot_spectrogram()).
# scales::gradient_n_pal() is the same palette function scale_fill_gradientn()
# uses internally, so the colours are identical to the previous approach.
spec_to_raster <- function(amp, sel_col, limits = c(-100, -20), n_colours = 256) {
  lut      <- scales::gradient_n_pal(sel_col)(seq(0, 1, length.out = n_colours))
  rescaled <- (amp - limits[1]) / (limits[2] - limits[1])
  rescaled <- pmin(pmax(rescaled, 0), 1)
  idx      <- round(rescaled * (n_colours - 1)) + 1
  col_mat  <- matrix(lut[idx], nrow = nrow(amp))
  # raster row 1 is drawn at the top; spec$amp row 1 is the lowest frequency,
  # so flip rows to put the highest frequency at the top like the y axis does
  grDevices::as.raster(col_mat[nrow(col_mat):1, , drop = FALSE])
}

plot_spectrogram <- function(spec, input, length_ylabs, x_breaks, y_breaks,
                             width_px = NULL, height_px = NULL) {
  sel_col <- palette_cols(input$palette_selected)
  if (input$palette_invert)
    sel_col <- rev(sel_col)

  amp <- downsample_spec_amp(spec$amp, width_px, height_px)
  img <- spec_to_raster(amp, sel_col)

  time_range <- range(spec$time)
  freq_range <- range(spec$freq)

  spec_plot <- ggplot() +
    annotation_raster(img,
                      xmin = time_range[1], xmax = time_range[2],
                      ymin = freq_range[1], ymax = freq_range[2],
                      interpolate = as.logical(input$spec_interpolate)) +
    xlab("Time (s)") +
    ylab("Frequency (kHz)") +
    scale_x_continuous(expand = c(0, 0),
                       breaks = x_breaks,
                       limits = time_range) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = y_breaks,
                       limits = freq_range,
                       labels = paste0(paste0(rep(" ", length_ylabs$spec), collapse = ""),
                                       y_breaks, "kHz")
                       ) +
    spec_theme
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
