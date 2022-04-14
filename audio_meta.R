#audio meta extraction

get_audio_dt <- function(x){
  dt_str  <- stringr::str_extract(x, regex("[0-9]{8}_[0-9]{6}"))
  dt_time <- strptime(dt_str, format = "%Y%m%d_%H%M%S")
  return(dt_time)
}

get_gmap_link <- function(latlong){
  href <- "http://maps.google.com/maps?t=k&q=loc:"
  return(tags$a(href=paste0(href, latlong[1], "+", latlong[2]), "Google Maps Link"))
}

dd2dms <- function(x, c = 'lat'){
  #https://gis.stackexchange.com/questions/10703/seeking-tool-to-convert-dd-to-dms/10729
  
  y <- abs(x)     # Work with positive values only.
  d <- floor(y)   # Whole degrees.  Floor() is ok too.
  z <- 60*(y - d) # The fractional degrees, converted to minutes.
  m <- floor(z)   # Whole minutes.
  s <- 60*(z - m) # The fractional minutes, converted to seconds.
  
  if(c == "lat")
    if(sign(x) == -1)
      hemisphere <- "S" 
    else 
      hemisphere <- "N"
  else #c is a longitude
    if(sign(x) == -1) 
      hemisphere = "W" 
    else 
      hemisphere = "E"
  res <- paste0(d, '\u00B0', m,'\'', s,'"', hemisphere)
  return(res)
}