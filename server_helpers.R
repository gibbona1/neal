extractWave_t <- function(x, tc) {
  return(extractWave(x, from = tc[1], to = tc[2], xunit = "time"))
}

btw <- function(x, left, right) {
  return(x >= left & x <= right)
}

length_b10 <- function(x) {
  return(x %>%
           range %>%
           as.integer %>%
           as.character %>%
           str_length %>%
           max)
}

blank_plot <- function(label) {
  p <- ggplot() +
    geom_text(aes(x = 0, y = 0), label = label, colour = "white") +
    theme_void() +
    theme(
      plot.background = element_rect(fill = rgb(0.15, 0.17, 0.19)),
      legend.position = "none")
  return(p)
}

frange_check <- function(vec, freq_range) {
  return((vec[1] > freq_range[1]) | (vec[2] < freq_range[2]))
}

trim_start <- function(y) {
  start_list <- c("Common", "Eurasian", "European", "Northern", "Greater", "Great")
  for (st in start_list)
    y <- gsub(paste0("^", st, " "), "", y)
  return(y)
}

get_entries <- function(x) {
  x <- x[x != ""]
  x <- trim_start(x)
  x <- as.vector(sort(x))
  return(x)
}

bb_iou <- function(boxA, boxB) {
  # intersection_over_union
  # boxes have column start_time, end_time, start_freq, end_freq
  # determine the (x, y)-coordinates of the intersection rectangle
  xA <- max(boxA[, 1], boxB[, 1])
  yA <- max(boxA[, 3], boxB[, 3])
  xB <- min(boxA[, 2], boxB[, 2])
  yB <- min(boxA[, 4], boxB[, 4])
  #apply(rbind(boxA,boxB),2,max)
  # compute the area of intersection rectangle
  interArea <- max(0, xB - xA) * max(0, yB - yA)
  # compute the area of both boxA and boxB
  box_area <- function(box) {
    return((box[, 2] - box[, 1]) * (box[, 4] - box[, 3]))
  }
  boxAArea <- box_area(boxA)
  boxBArea <- box_area(boxB)
  # compute the intersection over union
  iou <- interArea / (boxAArea + boxBArea - interArea)
  return(iou)
}

noise_reduce <- function(x) {
  return(switch(x,
                None    = NULL,
                Rows    = 1,
                Columns = 2))
}

geometric_median <- function(X, eps = 1e-5, numIter = 200) {
  mX <- matrix(c(as.vector(Re(X)),
                 as.vector(Im(X))),
               ncol = 2, byrow = FALSE)
  y  <- colMeans(mX)
  itr <- 1
  while (TRUE) {
    D <- rdist::cdist(mX, matrix(y, nrow = 1))
    nonzeros <- (D != 0)[, 1]
    Dinv  <- 1 / D[nonzeros]
    Dinvs <- sum(Dinv)
    W     <- Dinv / Dinvs
    WT    <- colSums(W * mX[nonzeros, ])
    
    num_zeros <- length(X) - sum(nonzeros)
    if (num_zeros == 0) {
      y1 <- WT
    } else if (num_zeros == length(X)) {
      return(y)
    } else {
      R <- (WT - y) * Dinvs
      r <- norm(R, type = "F")
      if (r == 0)
        rinv <- 0
      else
        rinv <- num_zeros / r
      y1 <-  max(0, 1 - rinv) * WT + min(1, rinv) * y
    }
    euclidean <- function(a, b) sqrt(sum((a - b)^2))
    if (euclidean(y, y1) < eps | itr > numIter)
      return(y)
    y <- y1
    itr <- itr + 1
  }
}

get_jq_lines <- function(val, cols) {
  x <- paste0("var ",
              c("originalBorder", "originalColor", "originalBackground"),
              " = [];",
              collapse = " ")
  button_val_id <- paste0("'input[type=radio][name=label_points][value=\"",
                          val, "\"]'")
  
  css_line <- function(css_class, css_val = NULL, func = "css", sq = "'") {
    x <- paste0("$(this).parent().", func, "(", sq, css_class, sq)
    if (!is.null(css_val))
      x <- paste0(x, ",'", css_val, "'")
    x <- paste0(x, ")")
    if (func == "css")
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
              css_line(button_val_id, func = "index", sq = ""),
              "]     = ",
              css_line("border-color"),
              #"originalBackground[",
              #  css_line(button_val_id, func = "index", sq=""),
              #  "] = ",
              #  css_line("background-color"),
              "originalColor[",
              css_line(button_val_id, func = "index", sq = ""),
              "]      = ",
              css_line("color"),
              css_line("border-color", "darkblue"),
              #css_line("background-color", "blue"),
              css_line("color", "white"),
              "},")
  x <- paste0(x,
              "function(){",
              css_line("border-color",
                       paste0("originalBorder[",
                              css_line(button_val_id, func = "index", sq = ""),
                              "]")),
              #css_line("background-color",
              #         paste0("originalBackground[",
              #                css_line(button_val_id, func = "index", sq=""),
              #                "]")),
              css_line("color",
                       paste0("originalColor[",
                              css_line(button_val_id, func = "index", sq = ""),
                              "]")),
              "});")
  x <- gsub("'original", "original", x)
  x <- gsub(")]')", ")])", x)
  return(x)
}

add_colour_js <- function(x, labs, cols) {
  if (!is.null(labs)) {
    for (lab in labs)
      x <- paste0(x, get_jq_lines(lab, cols))
  }
  return(x)
}

#JS code to change input val for single input
inp_script <- function(name) {
  return(paste0("Shiny.addCustomMessageHandler('",
                name, "', function(value) { ",
                "Shiny.setInputValue('", name, "', value);",
                #"$('#",name,"').val(value);
                "});"
  ))
}

