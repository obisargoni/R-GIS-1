####################
#
# Week 8 Practical
#
####################

library(tidyverse)
library(downloader)
library(rgdal)
library(sf)
library(ggplot2)
library(reshape2)
library(plotly)
library(highcharter)

getwd()

LondonWards <- readOGR(file.path(getwd(), 'Boundaries'),layer =  'NewLondonWard')
class(LondonWards)

# Cnovert from sp objcet to sf object
LondonWardsSF <- st_as_sf(LondonWards)

# Read in additional data on housing tenure
TenurData <- read_csv("https://www.dropbox.com/s/qay9q1jwpffxcqj/LondonAdditionalDataFixed.csv?raw=1")

# Merge these two together
LondonWardsSF <- merge(LondonWardsSF, TenurData, by.x = 'WD11CD',by.y = 'Wardcode')

# Now make some histograms from the data
histplot1 <- ggplot(data = LondonWardsSF, aes(x = PctDepChil)) + geom_histogram(binwidth = 5, colour = 'black', fill = 'white')
histplot1

# Make a histogram and add lines at the mean and median points
histplot2 <- ggplot(data = LondonWardsSF, aes(x=AvgPubTran)) + geom_histogram(binwidth = 0.5, colour = 'black', fill = 'white') + geom_vline(xintercept = mean(LondonWardsSF$AvgPubTran), colour = 'red', size = 0.5) + geom_vline(xintercept = median(LondonWardsSF$AvgPubTran), colour = 'blue', size = 1.0)
histplot2

# Add some KDE plots
histplot3 <- ggplot(data = LondonWardsSF, aes(x=PctDepChil, y = ..density..)) + geom_histogram(binwidth = 5, colour = 'black', fill = 'white') + geom_vline(xintercept = median(LondonWardsSF$PctDepChil), colour = 'red', size = 1) + geom_density(colour = 'blue', size = 1, adjust = 1)
histplot3

# Multifaceted plots
# First identify and remove non numerical fields

dfTypes <- as.data.frame(cbind(lapply(LondonWardsSF, class)))
dfTypes['WD11CD',]
# Add numeric index to the df
dfTypes <- cbind(dfTypes, seq.int(nrow(dfTypes)))
# Select only the fields we want
LondonWardsSFSub <- LondonWardsSF[,c(1:73, 83:86)]

# Create some subsets for plotting the data, also need to ensure that the geometry column is NULL
LondonWardsSFSub1 <- st_set_geometry(LondonWardsSFSub[,c(1:3,9:27)],NULL)
LondonWardsSFSub2 <- st_set_geometry(LondonWardsSFSub[,c(1:3,28:50)],NULL)
LondonWardsSFSub3 <- st_set_geometry(LondonWardsSFSub[,c(1:3,51:73)],NULL)

LondonMelt2 <- melt(LondonWardsSFSub2, id.vars = 1:3)

# Now plot the data
attach(LondonMelt2)
multihist2 <- ggplot(LondonMelt2, aes(x=value)) + geom_histogram(aes(y = ..density..)) + geom_density(colour = 'red', size = 1, adjust = 1)
multihist2 <- multihist2 + facet_wrap(~ variable, scales = 'free')
multihist2

LondonMelt3 <-melt(LondonWardsSFSub3, id.vars = c(1:3))
multihist3 <- ggplot(LondonMelt3, aes(x=value)) + geom_histogram(aes(y = ..density..)) + geom_density(colour = 'red', size = 1, adjust = 1)
multihist3 <- multihist3 + facet_wrap(~variable, scale = 'free')
multihist3

LondonMelt1 <-melt(LondonWardsSFSub1, id.vars = c(1:3))
multihist1 <- ggplot(LondonMelt1, aes(x=value)) + geom_histogram(aes(y = ..density..)) + geom_density(colour = 'red', size = 1, adjust = 1)
multihist1 <- multihist1 + facet_wrap(~variable, scale = 'free')
multihist1

# Can very simply plot histograms of transformed variables
multihist4 <- ggplot(LondonMelt1, aes(x = log10(value))) + geom_histogram(aes(y = ..density..)) + geom_density(colour = 'red', size = 1, adjust = 1)
multihist4 <- multihist4 + facet_wrap(~variable, scale = 'free')
multihist4
help("facet_wrap")


# Creating a histogram based on centroids of wards
wardpoints <- ggplot(LondonWardsSFSub, aes(x = x.y, y = y.y)) + geom_point(colour = 'blue', size = 0.5) + coord_equal()
wardpoints

wardbins <- ggplot(LondonWardsSFSub, aes(x=x.y, y = y.y)) + stat_bin_2d(bins = 15) + coord_equal()
wardbins

# Or can add KDE to plot of points
wardpoints  <- wardpoints + stat_density_2d(aes(fill = ..level..), geom = 'polygon')
wardpoints

#############################
#
# Task 2: Writing functions
#
#############################

# cngvar here is confusing? Some sort of dummy variable but don' tunderstand why it works
# after cngvar is initialised it becomes a string variable, a sort of list, with each element either high, low or medium
recode <- function(variable, high, medium, low){
  cngvar <- NULL
  cngvar[variable <= high] <- 'high'
  cngvar[variable <= medium] <- 'medium'
  cngvar[variable <= low] <- 'low'
  return(cngvar)
  }

summary(LondonWardsSF$AvgGCSE2011)
LndGCSERecode <- recode(LondonWardsSF$AvgGCSE2011, 409.1, 358.3, 332.2)
LndGCSERecode

summary(LondonWardsSF$UnauthAbse)
LndUnauthAbsRecode <- recode(LondonWardsSF$UnauthAbse, 2.4675, 1.4105, 0.8215)

# Add the recoded variables back into the dataframe
LondonWardsSF$GCSERecode <- LndGCSERecode
LondonWardsSF$UnauthAbseRecode <- LndUnauthAbsRecode

LondonWardsSF$GCSERecode
LondonWardsSF$UnauthAbseRecode

# Functions to calculate location quotients (quotient is the ratio of the local distribution to the global distribituion)
LQ1 <- function(pctVariable){
  pctVariable / mean(pctVariable)
}

# Use this if percentage hasn't already been calculated
LQ2 <- function(variable, rowtotal){
  localprop <- variable / rowtotal
  globabprop <- sum(variable) / sum(rowtotal)
  return(localprop/globabprop)
}


LondonWardsSF$PctOwnedLQ <- LQ1(LondonWardsSF$PctOwned20)
LondonWardsSF$PctSocialRLQ <- LQ1(LondonWardsSF$PctSocialR)
LondonWardsSF$PctPrivateLQ <- LQ1(LondonWardsSF$PctPrivate)
LondonWardsSF$PctSharedO <- LQ1(LondonWardsSF$PctSharedO)
LondonWardsSF$PctRentFreLQ <- LQ1(LondonWardsSF$PctRentFre)

LondonWardsSF$PctRentFreLQ

# Creating plots using functions

vars <- readline("Enter vars to map")

# Function to get character string from terminal input
GetInputStrList <- function(sep = ' '){
  InputString <- readline()
  StringList <- unlist(strsplit(InputString, sep))
  return(as.list(StringList))
}

# Function to calculate local quotients of prcnt variables given df and list of variables
AddLocalQuotients <- function(SFdataframe, varsList){
  attach(SFdataframe)
  # Iterate over variables and create quotients
  for (i in 1:length(varsList)) {
    pctVariableName <- varsList[[i]]
    colvect <- which(colnames(SFdataframe)==pctVariableName)
    v <- SFdataframe[,colvect]
    SFdataframe[,paste('LQ_', pctVariableName, sep = '')] <- LQ1(v[[pctVariableName]])
  }
  detach(SFdataframe)
  return(SFdataframe)
}

PlotVariables <- function(SFdataframe, varslist){
  print(paste("Plotting ", length(varslist), " variables"))
  for (i in 1:length(varslist)) {
    print(paste("Plotting ", varslist[[i]]))
    
    #LQMapperPlot <- ggplot(SFdataframe) + geom_polygon(aes(fill = varslist[[i]]))
    #LQMapperPlot
    #ggplot2::ggsave(LQMapperPlot, filename = paste(varslist[[i]], '.png', sep = ''))
    
    LQMapperPlot <- tm_shape(SFdataframe) + tm_polygons(varslist[[i]], 
                                                         style="jenks",
                                                         palette="Spectral",
                                                         midpoint=1,
                                                         title=varslist[[i]],
                                                         alpha = 0.5)
     
    tmap_save(LQMapperPlot, filename = paste(varslist[[i]], '.png', sep = ''))
    
  }
  return(SFdataframe)
}

help(geom_polygon)
# Test out these functions
colnames(LondonWardsSF)

vars_to_mapLQs <- GetInputStrList()
df_with_LQ_vars <- AddLocalQuotients(LondonWardsSF, vars_to_mapLQs)

LQ_vars <- paste('LQ_',vars_to_mapLQs, sep = '')

library(tmap)
PlotVariables(df_with_LQ_vars, LQ_vars) # Works!
colnames(df_with_LQ_vars)


################################
#
# Task 4 - Basic Geodemographic Classification
#
################################

# Remove geographic element of the LondonWardsSF data object
LondonWardsDF <- st_set_geometry(LondonWardsSF, NULL)
cbind(lapply(LondonWardsDF, class))

# Create a new data frame containing only the data to be used for the clustering
ClusterDF <- LondonWardsDF[,c('PctOwned20', 'PctNoEngli')]

# Check that variables are approximately normally distributed
pctownedPlot <- ggplot(data = ClusterDF, aes(x = PctOwned20)) + geom_histogram(colour = 'grey')
pctownedPlot

pctnoengPlot <- ggplot(data = LondonWardsDF, aes(x = PctNoEngli)) + geom_histogram(colour = 'blue')
pctnoengPlot

# Q-Q plot can also be used to check normality
qqnorm(ClusterDF$PctOwned20, pch = 19) + qqline(ClusterDF$PctOwned20)
qqnorm(ClusterDF$PctNoEngli, phc = 19) + qqline(ClusterDF$PctNoEngli)


# Run statistical test to check how normally distributed data is
# Despite looking reasonably normaly distributed the variables fail the S-W test
# This may be because of the large sample size
shapiro.test(ClusterDF$PctOwned20)
shapiro.test(ClusterDF$PctNoEngli)

# Continue with cluster analysis anyway

# Find three clusters
ClusterFit <- kmeans(ClusterDF, 3, nstart = 25)


# Get cluster means
ClusterMeans <- aggregate(ClusterDF, by = list(ClusterFit$cluster), FUN = mean)
ClusterMeans                   

# Plot the means. Need to convert the clusters into a categorical variable for colouring using 'factor'
ClusterPlot <- ggplot(ClusterDF, aes(x = PctOwned20, y = PctNoEngli)) + geom_point(colour = factor(ClusterFit$cluster)) + geom_point(data = ClusterMeans[,2:3], aes(x=PctOwned20, y=PctNoEngli), size = 7, shape = 18) + theme(legend.position = 'none')
ClusterPlot 

# Assign the clusters back into the dataframe
ClusterDF$cluster <- ClusterFit$cluster
LondonWardsSF$cluster <- ClusterDF$cluster

# Now plot wards and colour by cluster
WardClusterPlot <- ggplot(LondonWardsSF) + geom_sf(mapping = aes(fill = cluster)) + scale_fill_continuous(breaks = c(1,2,3))
WardClusterPlot

# Save the London Wards data with additional varliables
colnames(LondonWardsSF)
help("writeOGR")
writeOGR(as_Spatial(LondonWardsSF), dsn = file.path(getwd(), 'Data'), layer = 'NewLondonWardsUpdated.shp', driver = 'ESRI Shapefile', verbose = TRUE)
