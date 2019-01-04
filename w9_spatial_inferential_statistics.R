# Week 9 practical - Spatial Inferential Statistics

library(tidyverse)
library(downloader)
library(rgdal)
library(sf)
library(ggplot2)
library(reshape2)
library(plotly)
library(highcharter)

rm(LondonWards)

LondonWards <- readOGR(file.path(getwd(), 'Data'), layer = 'NewLondonWardsUpdated.shp')

# Check SP object has recoded variables
LondonWards[, grepl('UnthAb', colnames(LondonWards@data))]
LondonWards@data$UnthAbR

# Convert to simple features object
LondonWardsSF <- st_as_sf(LondonWards)

# Chi squared test - non parametric
LdnChisq <- chisq.test(LondonWardsSF$GCSERcd, LondonWardsSF$UnthAbR)
LdnChisq$p.value

# Linear regression model - parametric

# First check variable are normally distributed
qplot(LondonWardsSF$AvGCSE201, geom = 'histogram', bins = 40)
qplot(LondonWardsSF$UnthAbs, geom = 'histogram', bins = 40) #slightly skewed

# Variables seem sufficiently normally distributed
# Check for correlation
qplot(LondonWardsSF$UnthAbs, LondonWardsSF$AvGCSE201, geom = 'point') + stat_smooth(method = 'lm')

# Now fit the regresion model
library(broom)
LinRegMdl <- lm(AvGCSE201 ~ UnthAbs, data = LondonWardsSF)
# Write the model results to a data frame (actually a tibble)
LinRegResults <- tidy(LinRegMdl)
class(LinRegResults)
LinRegResults
summary(LinRegMdl)

# Examine some diagnostic plots to see if there are any paatters in the residuals (I think we would expect these to be normally distributed)
plot(LinRegMdl)

# Plotting the residuals on a map to check for spatial clustering
# This asks whether the deviations from the linear regression model are spatially clustered
library(tmap)
LondonWardsSF$LinRegResid <- LinRegMdl$residuals
tmap_mode('view')
qtm(LondonWardsSF, fill = 'LinRegResid') # Slghtly positive and slightly negative rsiduals seems to cluster

# Can test statistically for spatial autocorrelation (clustering) using Morans I
library(spdep)
library(sp)

LondonWards <-  as_Spatial(LondonWardsSF)
# Calcualte centroids of all the Wards
CentCoords <- coordinates(LondonWards)
plot(CentCoords)

# Now create neighbourghs list
WardsNeigh <- poly2nb(LondonWards, queen = TRUE)
# Create spatial weights matrix
WardsSpWeights <- nb2listw(WardsNeigh, style = 'C')

# Now rin Morans I test - statistically significant but quite weak spatial clustering of residuals
moran.test(LondonWards@data$LinRegResid, WardsSpWeights)

# Residuals are the deviation of Wards from the regression model. Wards with non-zero residuals have GCSE scores that
# are not fully explained by the regression modeling.
# This could be due to random sampling errors, but if the wards' residuals were spatially correlated this would suggest
# the presence of underline geographic variables also contributing to GCSE performance.

# Introducing dummy variables to the regression model

# Can investigate the effect of categorical variables on GCSE results.
# For example, whether Wards are in inner or outer London
p <- ggplot(data = LondonWardsSF, aes(x= UnthAbs, y = AvGCSE201)) + geom_point(aes(colour = InnrOtr))
p # outer London wards seem to bshow greater correlation

# First convert the categorical variable to a factor
LondonWardsSF$InnrOtr <- as.factor(LondonWardsSF$InnrOtr)
levels(LondonWardsSF$InnrOtr)

# Create model
LinRegMld_dummy <- lm(AvGCSE201 ~ UnthAbs + InnrOtr, data = LondonWardsSF)
summary(LinRegMld_dummy)

# Categorical variables are included in the model by comparing one group (the control) to another.
# To see which group is used as the control, check out the contrasts matrix
contrasts(LondonWardsSF$InnrOtr)

# The control group can be switched by 'releveling' the factor variable
LondonWardsSF$InnrOtr <- relevel(LondonWardsSF$InnrOtr, ref = 'Outer')
contrasts(LondonWardsSF$InnrOtr)

LinRegMld_dummy2 <- lm(AvGCSE201 ~ UnthAbs + InnrOtr, data = LondonWardsSF)
summary(LinRegMld_dummy2)

# Add an additional scale variable into the regression model
# Check if employment is normally distributed
p <- ggplot(data = LondonWardsSF, aes(x = Emplymn)) + geom_histogram(bins = 40)
p # Looks good

# Additional variables need to be independent from other variables, therfore check for correlation
library(corrplot)
LondonWardsDF <- st_set_geometry(LondonWardsSF, NULL)
cormat <- cor(LondonWardsDF[,8:72], use = 'complete.obs', method = 'pearson')
corrplot(cormat) # Plot shows that Employment is highly correlated with some variable - but is it highly correlated with UnthAbs

# Find the column IDs of the variables we are including in the plot
corcols <- c(grep('UnthAbs', colnames(LondonWardsDF)),grep('Emplymn', colnames(LondonWardsDF)),grep('AvGCSE201', colnames(LondonWardsDF)))
corcols  
cormat <- cor(LondonWardsDF[,corcols], use = 'complete.obs', method = 'pearson')
corrplot(cormat) # Can view correlations between specific variables

# Now build regression model
LinRegMld_dummy3 <- lm(AvGCSE201 ~ UnthAbs + InnrOtr + Emplymn, data = LondonWardsSF)
summary(LinRegMld_dummy3)

# R-squared score of model has improved therefore this model fits the data better


# A follow up task is to build an optimum model of GCSE scores using as few variables as possible.
# Then map the residuals and test for spatial autocorrelation. If not autocorrelation suggest that model 
# is capturing all significant geographical variables

###############################
#
# Task 3 - Geographically Weighted Regression Models
#
###############################

# Adams final model from Task 2
LinRegMdlFinal <- lm(AvGCSE201~UnthAbs + Emplymn + CrsPHH2, data = LondonWardsSF)
summary(LinRegMdlFinal)
plot(LinRegMdlFinal)

# Get residuals and plot them for each ward
LondonWardsSF$LinRegResid_Final <- LinRegMdlFinal$residuals
qtm(LondonWardsSF, fill = 'LinRegResid_Final')

# Check for spatial autocorrelation
LondonWards <- as_Spatial(LondonWardsSF)
WardsNeighList <- poly2nb(LondonWards, queen = TRUE)
WardsWeihts <- nb2listw(WardsNeighList, style = 'C')
moran.test(LondonWards@data$LinRegResid_Final, WardsWeihts)

# Moran's I suggests there is still a little spatial autocorrelation
# Conduct Geographically Weighted Regression analysis to see how the regression model varies over the geographical space
library(spgwr)

# GWR performs a regression analysis for each data point, including all data points that are located within
# some distance to the target data point.
# This allows the variation of the regression model with geography to be examined.

# The window that determines which data points are included is set thrugh an optimisation procedure
# Also known as kernel bandwidth
GWRBandwidth <- gwr.sel(AvGCSE201 ~ UnthAbs + Emplymn + CrsPHH2, data = LondonWards, coords = cbind(x,y), adapt = T)

# Now run the model
gwr.model <- gwr(AvGCSE201 ~ UnthAbs + Emplymn + CrsPHH2, data = LondonWards, adapt = GWRBandwidth, hatmatrix = TRUE, se.fit = TRUE)
gwr.model

# Can now plot the coefficients for different variables
GWRResults <- as.data.frame(gwr.model$SDF)
head(GWRResults)

# Add these coefficients back into the LondonWards spatial dataframe
LondonWards@data$coefUnthAbs <- GWRResults$UnthAbs
LondonWards@data$coefEmplymn <- GWRResults$Emplymn
LondonWards@data$coefCrsPHH2 <- GWRResults$CrsPHH2

tm_shape(LondonWards) + tm_polygons(col = 'coefUnthAbs', palette = 'RdBu')
tm_shape(LondonWards) + tm_polygons(col = 'coefEmplymn', palette = 'RdBu')
tm_shape(LondonWards) + tm_polygons(col = 'coefCrsPHH2', palette = 'RdBu')

# Now loom at the statistical significance of the coefficients using the standard erroe
# 2 standard errors equarte to around the 95% confidence interval
# Therefore if a coefficient is more that 2 se from zero it can be said to be approximately significant

sigtestUnthAbs <- abs(LondonWards@data$coefUnthAbs) - 2*gwr.model$SDF@data$UnthAbs_se
LondonWards@data$sigtestUnthAbs <-  sigtestUnthAbs

tm_shape(LondonWards) + tm_polygons(col = 'sigtestUnthAbs', palette = 'RdBu')

# Can condluce that much of the geographical variation of the UnthAb variable is not significant.
