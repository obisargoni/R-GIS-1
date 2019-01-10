# Week 10 R tutorial

library(ggplot2)
library(gstat)
library(sp)
library(maptools)
library(sf)

# Read in the 2013 nitrosdioxide data
AirPolData <- read.csv(file.path(getwd(), 'Data', '4.1. Concentrations LAEI 2013 Update', '2013', 'CSV', 'PostLAEI2013_2013_NO2.csv'))
length(AirPolData)

# Creat a smaller dataset so that interpolation runs more quickly
AirPolData <- AirPolData[sample(nrow(AirPolData), 5000),]

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

AirPol_idw <- idw(formula = conct ~ 1, locations = AirPolData, newdata = Grd)
AirPol_idw.output <- as.data.frame(AirPol_idw)
head(AirPol_idw.output) # This dataframe contains the interpolated values for each grid section

# Nename the dataframe columns
names(AirPol_idw.output)[1:3] <- c('x','y','conct_interp')
head(AirPol_idw.output)

# Now plot
plot(AirPol_idw) # Can see the road network!!!

# Plot a different way
plt_AirPol <- ggplot(data = AirPol_idw.output, aes(x=x, y=y)) + geom_tile(aes(fill = conct_interp)) + scale_fill_gradient(low = '#FEEBE2', high = '#7A0177')
plt_AirPol

####################
#
# Interpolation by Kriging
#
####################

# First need to construct a semivariogram model in order to describe the spatial autocorrelation of the data

?zerodist

# Remove Air Quality measurements that have the same coordinates
class(zerodist(AirPolData))

# Create empirical semivariogram 
variogmcloud <- variogram(conct ~ 1, locations = AirPolData, data = AirPolData, cloud = TRUE)
plot(variogmcloud)

# Create a semi variogram by binning values in the variogram and plotting the 'lags' of each bin - half the mean value (why this?)
semivariogm <- variogram(conct ~ 1, locations = AirPolData, data = AirPolData)
plot(semivariogm)
semivariogm

# From this can get the sill, range and nugget; measures of the data's spatial autocorrelation
class(semivariogm)

# Fit a polynomial to the data
?poly
model_sv <- lm(semivariogm$gamma ~ poly(semivariogm$dist, 3))
summary(model_sv) # The model coefficient for the intercept does not agree with the semivariogram plot

sv_nugget <-  model_sv$coefficients['(Intercept)']

# Tried getting the derivative of the model equation in order to find the sill argument but doesn't seem
# to be a quick way to fin the x values for which the model equation gradient (ie the derivative) isclose to zero but this is complicated
model_expr <- expression(79.4 + 86*x -14*x^2 + -10*x^3)
deriv_model <- deriv(model_expr, 'x')
class(deriv_model)

sv_sill <-  100
sv_range <- 20000
sv_nugget <- 30

vgm()
# Now build a model semivariogram using the sill range and nugget
model.variogm <- vgm(psil = sv_sill, model = 'Gau', nugget = sv_nugget, range = sv_range)

# Fit this model to a sample variogram to check the fit
fit.vgm <-fit.variogram(semivariogm, model.variogm)
plot(semivariogm, fit.vgm) # Looks OK

# Could also try some alternatives models instead of 'Gau', eg 'Exp'. Tried a few, Gau was best

# Now that a model semivariogram has been obtained, the charateristic spatial autocorrelation of the data is known. This
# permits Kriging interpolation
AirPolKrig <- krige(formula = conct ~ 1, locations = AirPolData, newdata = Grd, model = model.variogm)
krig.output <- as.data.frame(AirPolKrig)

head(krig.output)
names(krig.output)[3] <- 'conct_krig'
head(krig.output)

# Now plot the interpolated data
AirPolKrig_plot <- ggplot(data = krig.output, aes(x=x, y=y)) + geom_tile(aes(fill = conct_krig)) + scale_fill_gradient(low = '#FEEBE2', high = '#7A0177')
AirPolKrig_plot # Plot does not look ok! Must have made a mistake somewhere.