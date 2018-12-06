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

# To test for Complete Spatial Randomness wantto compare distribution of points to 
# a poisson distribution - Ripley's K does this

# Takes a long time to compute for this many points across London, need to restrict analysis

# Select individual Borough
Lambeth <- BoroughMapBNG[BoroughMapBNG@data$lad15nm=="Lambeth",]
qtm(Lambeth)

# Plot just the BP points in Lambeth
BluePlaquesLambeth = BluePlaquesBNGSub[Lambeth,]
BluePlaquesLambeth@bbox <- BluePlaquesLambeth@bbox[c(1,2),]
tm_shape(Lambeth) + tm_polygons(col = NA, alpha = 0.5) + tm_shape(BluePlaquesLambeth) + tm_dots(col = 'blue')

# Can now carry out analysis in spatstat. Need to create and 'observation window'
LambethWindow <- as.owin(Lambeth)
plot(LambethWindow)

# Need to create point patter object to perform analysis with
BluePlaquesLambeth.ppp <- ppp(x=BluePlaquesLambeth@coords[,1],
                              y=BluePlaquesLambeth@coords[,2],
                              window=LambethWindow)

# Have a look at the pp object
plot(BluePlaquesLambeth.ppp, cex=0.5, main='Blue Plaques Harrow')

# Can now produce KDE plot using density() function
plot(density(BluePlaquesLambeth.ppp, sigma=500))
plot(density(BluePlaquesLambeth.ppp, sigma=1000))
plot(density(BluePlaquesLambeth.ppp, sigma=100))

# Analysing Point Data
# 1. Quadrat Analysis
plot(BluePlaquesLambeth.ppp,pch=16,cex=0.5, main="Blue Plaques in Harrow")
#now count the points in that fall in a 6 x 6 grid overlaid across the window
plot(quadratcount(BluePlaquesLambeth.ppp, nx = 5, ny = 5),add=T,col="red")

# Run the quadcount and save result in a dataframe
Qcount <- data.frame(quadratcount(BluePlaquesLambeth.ppp, nx=6, ny=6))
class(Qcount)

# table() takes data of cases and returns contingency table - grouped counts by input variables
QCountTable <- data.frame(table(Qcount$Freq, exclude = NULL))
class(QCountTable)

# counts are data type 'factor' - need to convert to numeric
class(QCountTable[,1])
counts_vect <- as.numeric(QCountTable[,1])
QCountTable[,1] <- counts_vect

# Now that have freq table, calculated expected values if dist were random
QCountTable$total <- QCountTable$Var1*QCountTable$Freq
# Get sum of frequency and total in order to calculate the mean
sums <- colSums(QCountTable[,-1])

lambda <- sums[2]/sums[1]

# using average, caclulate probability of each quarant containing each number of plaques
QCountTable$Pr <- ((lambda^QCountTable$Var1)*exp(-lambda))/factorial(QCountTable$Var1)
QCountTable$Expected <- round(QCountTable$Pr*sums[1],0)
QCountTable

# plot frequency distributions to compare
plot(c(1,5),c(0,14), type='n', 
     xlab = 'Num Blue Plaques (red=observed, blue =expected',
     ylab='Frequency')
points(QCountTable$Freq, col="Red", lwd=3, type='o')
points(QCountTable$Expected, col='Blue', lwd=3, type='o')

# Frequency distributions are similar at points and disimilar at other points
# Need to perform Chi-squared test to actually check

# quardat.test performs chi squared test to compare observed
# and expected frequencies for each quadrat (rather that bins)
# How is the expected number calculated?
teststats <- quadrat.test(BluePlaquesLambeth.ppp, nx=3, ny=3)
teststats

# 2. Ripley's K
# Problem with Quadrant analysis is that results are dependent on the size of quadrant
# Ripley's K is a meausre of density of points modified by how
# spread out points are for a giver circle of radius r

K <- Kest(BluePlaquesLambeth.ppp, correction = 'border')
plot(K)
# distribution shows taht at distances below ~800m plaques are clustered

# 3. DBSCAN
# Can tell us where in the spatial area of interest clusters are occuring

library(raster)
library(fpc)
library(plyr)
library(OpenStreetMap)

crs(Lambeth)

# DBSCAN requires two parameters:
# - epsilon: radius to search for clusters within
# - MinPts: minimum number of points to consider a cluster

# From Ripleys K analysis found that largest point of
# K (ie most dense clustering) occured around 600m
# Set e = 600m, minpts = 4

# Get coordinates of BluePlaque points
BluePlaquesLambethPts <- as.data.frame(BluePlaquesLambeth@coords[,1:2])
db <- fpc::dbscan(BluePlaquesLambethPts, eps = 600, MinPts = 4)
plot(db, BluePlaquesLambeth, main='DBSCAN Output', frame = F)
plot(Lambeth, add=T)

# Now make nicer plot
library(ggplot2)
db
# Add cluster membership infor back into BluePlaues df
BluePlaquesLambethPts$cluster <- db$cluster

# Create some convext hull ploygons to wrap around clusters
# ddply is a group by and apply type function
chulls <- ddply(BluePlaquesLambethPts, .(cluster), function(df) df[chull(df[,1], df[,2]),])

# remove points that aren't in a cluster
chulls <- subset(chulls, cluster>0)

# now create a plot
dbplot <- ggplot(data=BluePlaquesLambethPts, aes(coords.x1,coords.x2, colour=cluster, fill=cluster))
#add in the points
dbplot <- dbplot+geom_point()
# add convex hulls
dbplot <- dbplot + geom_polygon(data=chulls, aes(coords.x1,coords.x2, group=cluster), alpha=0.5)
dbplot + theme_bw() + coord_equal()

# Now add a base map
latlong <- "+init=epsg:4326"
LambethWGS <- spTransform(Lambeth, CRS(latlong))

# Get basemap from bounding box
basemap <- openmap(c(LambethWGS@bbox[1,2],LambethWGS@bbox[1,1]),c(LambethWGS@bbox[2,2],LambethWGS@bbox[2,1]), zoom=NULL, 'stamen-toner')
basemap_bng <- openproj(basemap, projection = BNG)

# did not work - some issue with projection
autoplot(basemap_bng) + geom_point(data=BluePlaquesLambethPts, aes(coords.x1,coords.x2, colour=cluster, fill=cluster)) + geom_polygon(data = chulls, aes(coords.x1,coords.x2, group=cluster, fill=cluster), alpha = 0.5)
