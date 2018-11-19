# Week 6 GIS Workshop - Point Patter Analysis and Spatial Autocorrelation

# first library a few packages that we will use during the practical
library(spatstat)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojsonio)
library(tmaptools)



# Get London boundaries as SP object
EW <- geojson_read("http://geoportal.statistics.gov.uk/datasets/8edafbe3276d4b56aec60991cbddda50_2.geojson", what = "sp")
BoroughMap <- EW[grep("E09", EW@data$lad15cd),]

# Check only LDN areas selected
length(BoroughMap@polygons)

qtm(BoroughMap)

# Find out current projection
# WGS 84
summary(BoroughMap)

# transform to espg 27700
BNG <- "+init=epsg:27700"
BoroughMapBNG <- spTransform(BoroughMap, BNG)
summary(BoroughMapBNG)

# Now read in Blue Plaques
BluePlaques <- read_shape(file.path(getwd(),"Data", "BluePlaques.shp"), as.sf = FALSE)
summary(BluePlaques)

WGS <- "+init=epsg:4326"
BluePlaquesBNG <- spTransform(BluePlaques, BNG)

# Clean the BP data: remove dups and points outside London
BluePlaquesBNG <- remove.duplicates(BluePlaquesBNG)
BluePlaquesBNGSub <- BluePlaquesBNG[BoroughMapBNG,]
BluePlaquesBNGSub@bbox <- BluePlaquesBNGSub@bbox[c(1,2),]

# Now plot the Blue Plaques
tmap_mode('view')
tm_shape(BoroughMapBNG) + tm_polygons(col = NA, alpha = 0.5) + tm_shape(BluePlaquesBNGSub) + tm_dots(col = 'blue')



