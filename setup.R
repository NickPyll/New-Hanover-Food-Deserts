# load libraries
library(googleway)
library(tidyverse)
library(purrr)
library(ggmap)
library(ggplot2)
library(RColorBrewer)
library(zoo)
library(plotly)
library(tibble)
library(rgdal)
library(tigris)
library(magrittr)
library(sf)

# create basemap for New Hanover
nh_base <- get_map(location = c(left = -78.1, bottom = 33.9, right = -77.6, top = 34.41), 
                   zoom = 13, maptype = 'toner', source = 'stamen')

nb.cols <- 10
mycolors <- colorRampPalette(brewer.pal(9, "Reds"))(nb.cols)

# load functions
source('functions.R') 
