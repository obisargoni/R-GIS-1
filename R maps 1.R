# lets o some mapping!

 # install.packages('maptools')
 # install.packages(c("classint", "OpenStreetMap", "tmap",
 #                   "RColorBrewer", "tmap", 
 #                  "tmaptools", "downloader", "geojsonio"))
 # install.packages('rJava')
 # install.packages('rgdal')
 #  install.packages('sf')
 # install.packages('rgeos')
 # install.packages('rJava')
library(sf)
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
library(rJava)

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
BoroughMapSF <- read_shape(file.path("./Boundaries/Local_Authority_Districts_December_2011_GB_BGC.shp"), as.sf = TRUE)
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

# Multiple plots
tm_shape(BoroughDataMap) + tm_polygons(c("Rates of ambulance call outs for alcohol related illness - 2014",
                                         "Rate of JobSeekers Allowance (JSA) Claimants - 2015"),
                                       style = c('jenks', 'pretty'),
                                       palette = list('YlOrBr','Purples'),
                                       midpoint = FALSE,
                                       title = c("Rate of Alcohol Related Ambulance Callouts - 2014", 
                                             "Rate of JSA Claimants - 2015"))

# Check out the available palettes
install.packages('shinyjs')
library(shinyjs)
tmaptools::palette_explorer()

# Set tmap_mode to view to allow interactive maps
tmap_mode('view')

inter_map <- tm_shape(BoroughDataMap) + tm_polygons("General Fertility Rate - 2013" ,
                                       style = 'pretty',
                                       palette = get_brewer_pal('PuRd', n=7),
                                       midpoint = FALSE,
                                       title = 'London Fertility Rate by LA',
                                       popup.vars = c("General Fertility Rate - 2013", 'lad11cd'))
inter_map                                      

# Make maps with ggplot2
library(ggplot2)

ggplot1 <-ggplot() + geom_sf(mapping = aes(geometry = geometry), data = BoroughDataMap) + theme_minimal()
ggplot1



# clean multipyte with gsub
bdm_names <-  names(BoroughDataMap) 
bdm_names_clean <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", bdm_names)
names(BoroughDataMap) <-make.names(bdm_names_clean, unique = TRUE)

# To add colour, pass the column to use to the fill argument of the geom_sf function
ggplot1 <-ggplot() + geom_sf(mapping = aes(geometry = geometry),
                             fill = BoroughDataMap$Average.Public.Transport.Accessibility.score...2014, data = BoroughDataMap) + theme_minimal()
ggplot1

# correct the palette and add a legend - NOT WORKING
#pal <- scale_fill_continuous(low = "white", high = "orange", name = "Accessibility")
pal <- scale_color_brewer(palette="Dark2")
labels <- labs(list(title = 'Average Public Transport Accessibility 2014', x = 'Longitude', y = 'Latitude'))
ggplot2 <-  ggplot() + 
            geom_sf(mapping = aes(geometry = geometry),
            fill = BoroughDataMap$Average.Public.Transport.Accessibility.score...2014, data = BoroughDataMap) + 
            pal + 
            labels
ggplot2
