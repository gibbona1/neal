#audio meta extraction

get_audio_dt <- function(x) {
  dt_str   <- stringr::str_extract(x, regex("[0-9]{8}_[0-9]{6}"))
  add_str  <- stringr::str_extract(x, regex("start_[0-9]{1,2}_[0-9]{1,2}"))
  add_str  <- str_replace(add_str, "start_", "")
  add_time <- as.numeric(str_replace(add_str, "_", "."))
  dt_time  <- strptime(dt_str, format = "%Y%m%d_%H%M%S")
  if(!is.na(add_time))
    dt_time <- dt_time + add_time
  return(dt_time)
}

get_audio_recdf <- function(x) {
  name_str <- stringr::str_extract(x, regex("[0-9]{0,9}_{0,1}[A-Z,0-9]{1,}_*"))
  pre_str  <- stringr::str_extract(x, regex("[0-9]{0,9}_*"))
  if(pre_str != "")
    str_start <- nchar(pre_str)+1
  else
    str_start <- 1
  name_str <- substr(name_str, str_start, nchar(name_str) - 1)

  loc_df <- read.csv("location_list.csv", fileEncoding = "UTF-8-BOM")
  if (name_str %in% loc_df$recorder_name)
    return(loc_df[loc_df$recorder_name == name_str, ])
  return(NULL)
}

get_gmap_link <- function(latlong) {
  href <- "http://maps.google.com/maps?t=k&q=loc:"
  return(tags$a(href = paste0(href, latlong[1], "+", latlong[2]), "Google Maps Link",
                target = "_blank"))
}

dd2dms <- function(x, c = "lat"){
  #https://gis.stackexchange.com/questions/10703/seeking-tool-to-convert-dd-to-dms/10729
  y <- abs(x)     # Work with positive values only.
  d <- floor(y)   # Whole degrees.  Floor() is ok too.
  z <- 60 * (y - d) # The fractional degrees, converted to minutes.
  m <- floor(z)   # Whole minutes.
  s <- 60 * (z - m) # The fractional minutes, converted to seconds.

  if (c == "lat")
    if (x < 0)
      hemisphere <- "S"
    else
      hemisphere <- "N"
  else #c is a longitude
    if (x < 0)
      hemisphere <- "W"
    else
      hemisphere <- "E"
  res <- paste0(d, "\u00B0", m, "\'", round(s, 1), '"', hemisphere)
  return(res)
}

m2km <- function(x) {
  x_m <- format(round(as.numeric(1000 * x), 1), nsmall = 1, big.mark = ",")
  return(paste(x_m, "m"))
}