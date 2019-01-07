# Week 10 R tutorial

library(ggplot2)
library(gstat)
library(sp)
library(maptools)
library(sf)

# Read in the 2013 nitrosdioxide data
AirPolData <- read.csv(file.path(getwd(), 'Data', '4.1. Concentrations LAEI 2013 Update', '2013', 'CSV', 'PostLAEI2013_2013_NO2.csv'))

# Check the fields
cbind(colnames(AirPolData),lapply(colnames(AirPolData), class))

# Make the coordinate fields numeric
AirPolData$x <- as.numeric(AirPolData$x)
AirPolData$y <- as.numeric(AirPolData$y)
cbind(colnames(AirPolData), lapply(colnames(AirPolData),class)) # this says x and y are still chatacter but hey lets continue

# Convert data frame to a spatial points data frame
help("coordinates")
coordinates(AirPolData) <- ~x+y
plot(AirPolData)

# Next will create a grid from which to interpolate the data

# First get the range of the data points
x.range <- as.integer(range(AirPolData@coords[,1]))
x.range
y.range <- as.integer(range(AirPolData@coords[,2]))
y.range

# Now create the grid
Grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 150), y = seq(from = y.range[1], to = y.range[2], by = 150))
head(Grd)

# Convert grid to a 'spatial pixel class' - the sp object for grids 
coordinates(Grd) <-  ~ x+y
gridded(Grd) <-  TRUE
help(gridded)

plot(Grd, cex=0.01)
points(AirPolData, pch = 1, col = 'red', cex = 0.05)
title("Iterpolation Grid and Sample Points")

# Now can carry out some interpolation analysis

# 1st Method: inverse weighting to generate a surface
?idw

AirPol_idw <- idw(formula = Total ~ 1, locations = AirPolData, newdata = Grd)
