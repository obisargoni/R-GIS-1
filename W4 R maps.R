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

# Set working directory to project files dir
# setwd("./") didn't work

# read in some data
LdnData <- read_csv("https://files.datapress.com/london/dataset/ward-profiles-and-atlas/2015-09-24T14:21:24/ward-profiles-excel-version.csv", na = 'n/a')
LdnData <- edit(LdnData)
LdnData <- LdnData[grep("E09", LdnData[,'New code']),]
EW <- geojson_read("http://geoportal.statistics.gov.uk/datasets/8edafbe3276d4b56aec60991cbddda50_2.geojson", what = "sp")
class(EW)

# get london LAs polygons
LdnPolys <- EW[grep("E09",EW@data$lad15cd),]
qtm(LdnPolys)

dir.exists(file.path(getwd(),"Boundaries"))

# read shape file from local drive
BoroughMapSF <- read_shape(file.path(getwd(),"Boundaries", "Local_Authority_Districts_December_2011_GB_BGC.shp"), as.sf = TRUE)
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
pal <- scale_fill_continuous(low = "white", high = "orange", name = "Accessibility")
#pal <- scale_color_brewer(palette="Dark2")
labels <- labs(list(title = 'Average Public Transport Accessibility 2014', x = 'Longitude', y = 'Latitude'))
ggplot2 <-  ggplot() + 
            geom_sf(mapping = aes(geometry = geometry),
            fill = BoroughDataMap$Average.Public.Transport.Accessibility.score...2014, data = BoroughDataMap) + 
            pal + 
            labels
pal
labels
ggplot2

# Check coordinate reference system
print(BoroughMapSP)
print(BoroughMapSF)

# assigning CRS when reading the data
BoroughMapSP <- read_shape(file.path("./Boundaries","Local_Authority_Districts_December_2011_GB_BGC.shp"), 
                          #current.projection = 27700, # not needed since shape file comes with projection
                          as.sf = FALSE
                          )
# This changes it to a SF object - is a wrapper for st_transform which works on SF
BoroughMapSP <- set_projection(BoroughMapSP, get_proj4(27700))
print(BoroughMapSP)

# Again, setting the CRS when reading data doesn't reproject
BoroughMapSF <- st_read(file.path("./Boundaries","Local_Authority_Districts_December_2011_GB_BGC.shp")) %>% st_set_crs(27700)
print(BoroughMapSF)

# Can also assign CRS once shape reading - again this doesn't reproject
proj4string(BoroughMapSP) <- CRS("+init=epsg:27700")

# To change CRS need to re-project data
CRS("+init=epsg:4326")
CRS("+proj=longlat +datum=WGS84")
BoroughMapSPWGS84 <- spTransform(BoroughMapSP, CRS("+init=epsg:4326"))
print(BoroughMapSPWGS84)

# Transform back
BoroughMapSPBNG <- spTransform(BoroughMapSP, CRS("+init=epsg:27700"))
print(BoroughMapSP)
print(BoroughMapSPBNG)


# Now that maps can be re-projected allows us to work with various basemaps
#install.packages('ggmap')
library(ggmap)

# use the WGS84 projection, joined wit London data
BoroughDataMapWGS84 <- append_data(BoroughMapSPWGS84, LdnData, key.shp = 'lad11cd', key.data = 'New code', ignore.duplicates = TRUE)

bdm_names <-  names(BoroughDataMapWGS84@data) 
bdm_names_clean <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", bdm_names)
names(BoroughDataMapWGS84@data) <-make.names(bdm_names_clean, unique = TRUE)


ldnbox1 = c(left = -0.5103766, bottom = 51.28676, right = 0.3340146, top = 51.69187)
ldnbox2 <- as.vector((st_bbox(BoroughMapSPWGS84)))

map <- get_stamenmap(ldnbox2, zoom= 10, maptype = 'toner-lite')
colnames(BoroughDataMapWGS84@data)
BDMWGS84 <- st_as_sf(BoroughDataMapWGS84)

# cRSs dont match up
ggmap(map) +geom_sf(mapping = aes(geometry = geometry), 
                    fill = BDMWGS84$Crime.rate...2014.15, 
                     data = BDMWGS84,
                     inherit.aes = FALSE,
                     alpha = 0.7) + theme_minimal()


