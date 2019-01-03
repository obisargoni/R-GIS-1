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
LondonWards[, grepl('GCSE', colnames(LondonWards@data))]
