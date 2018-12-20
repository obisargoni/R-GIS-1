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
# Select the numeric fields
unique(dfTypes$V1)
numeric_fields <- dfTypes[dfTypes$V1 %in% c('factor', 'numeric', 
                                            'integer'),2]



numeric_fields
class(seq.int(nrow(dfTypes)))

help("seq.int")
