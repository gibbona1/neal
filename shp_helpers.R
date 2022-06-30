#install.packages('rgdal')
library(rgeos)
library(maptools)

epsg.29902 <- "+proj=lcc +lat_0=53.5 +lon_0=-8 +k=1.000035 +x_0=200000 +y_0=250000 +a=6377340.189 +b=6356034.447938534 +units=m +no_defs "
wgs.84     <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
latlong    <- c(52.181183,-6.368361)

CRS(wgs.84)
coast <- readShapeLines("shp_data/ne_10m_coastline.shp", CRS(wgs.84))
MAD   <- readWKT(paste0("POINT(", latlong[1], " ", latlong[2],")"),p4s=CRS(wgs.84))
gDistance(MAD,coast)            # WGS-84 (long/lat) projection, units in "degrees"
# [1] 3.021808

coast.proj <- spTransform(coast,CRS(epsg.29902))
MAD.proj   <- spTransform(MAD,CRS(epsg.29902))
gDistance(MAD.proj,coast.proj)

tmp_df <- read.csv("location_list.csv")

for(i in 1:nrow(tmp_df)){
  row <- tmp_df[i,]
  print(row[1])
  MAD   <- readWKT(paste0("POINT(", row$lat, " ", row$long,")"),p4s=CRS(wgs.84))
  print(gDistance(MAD,coast))
}
