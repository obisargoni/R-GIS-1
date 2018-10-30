# lets o some mapping!

# install.packages('maptools')
# install.packages(c("classint", "OpenStreetMap", "tmap",
#                    "RColorBrewer", "Sp", "rgeos", "tmap", 
#                    "tmaptools", "sf", "downloader", "rgdal", "geojsonio"))
# install.packages('rJava')
library(maptools)
library(RColorBrewer)
library(classInt)
library(OpenStreetMap)
library(sp)
library(rgeos)
library(tmap)
library(tmaptools)
library(sf)
library(rgdal)
library(geojsonio)
library(tidyverse)

# read in some data
LdnData <- read_csv("https://files.datapress.com/london/dataset/ward-profiles-and-atlas/2015-09-24T14:21:24/ward-profiles-excel-version.csv", na = 'n/a')
LdnData <- edit(LdnData)
LdnData <- LdnData[grep("E09", LdnData[,'New code']),]
EW <- geojson_read("http://geoportal.statistics.gov.uk/datasets/8edafbe3276d4b56aec60991cbddda50_2.geojson", what = "sp")
class(EW)

# get london LAs polygons
LdnPolys <- EW[grep("E09",EW@data$lad15cd),]
qtm(LdnPolys)

# read shape file from local drive
BoroughMapSF <- read_shape("C:\\Users\\Obi\\Desktop\\CASA\\R-GIS-1\\Boundaries\\Local_Authority_Districts_December_2011_GB_BGC.shp", as.sf = TRUE)
BoroughMapSF <- BoroughMapSF[grep("E09",BoroughMapSF$lad11cd),]
BoroughMapSP <- LdnPolys

qtm(BoroughMapSF)

library(methods)
class(BoroughMapSF)
class(BoroughMapSP)

# converting between spatial polygons simple feature objects
newSF <- st_as_sf(BoroughMapSP)
class(newSF)

newSP <- as(newSF,'Spatial')
class(newSP)

# join attribute data to boundaries
# can be a pain with SP object
BoroughMapSP@data <- data.frame(BoroughMapSP@data, LdnData[match(BoroughMapSP@data$lad15cd, LdnData[,'New code'] ), ])
head(BoroughMapSP@data)

head(LdnData)

# join the SF data
BoroughDataMap <- append_data(BoroughMapSF, LdnData, key.shp = 'lad11cd', key.data = 'New code', ignore.duplicates = TRUE)

# or can do left join style
BoroughDataMap2 <-  BoroughMapSF %>% left_join(LdnData, by = c('lad11cd' = 'New code'))

# create a chloropleth quickly using tmap
library(tmap)
library(tmaptools)
tmap_mode('plot')

colnames(BoroughDataMap)

qtm(BoroughDataMap, fill = "Rate of JobSeekers Allowance (JSA) Claimants - 2015")

# add base map and some other stuff
london_osm <- read_osm(BoroughDataMap, type = 'esri', zoom = NULL)
qtm(london_osm) + tm_shape(BoroughDataMap) + 
  tm_polygons("Rate of JobSeekers Allowance (JSA) Claimants - 2015",
              style = 'jenks',
              palette = 'YlOrBr',
              midpoint = NA,
              title = 'Rate per 1,000 people',
              alpha = 0.5) +
  tm_compass(position  = c('left','bottom'), type = 'arrow') +
  tm_scale_bar(position = c('left', 'bottom')) +
  tm_layout(title = 'Job seekers\' Allowance Claimants', legend.position = c('right','bottom'))
