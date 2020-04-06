# load libraries
library(googleway)
library(purrr)
library(tidyverse)
library(zoo)
library(timetk)
library(forecast)
library(sweep)
library(tidyquant)
library(tigris)
library(magrittr)
library(plotly)
library(ggmap)
library(ggplot2)
library(RColorBrewer)
library(tibble)
library(sf)
library(rgdal)
library(scales)
library(janitor)
library(corrplot)
library(glue)
library(httr)

# load functions
source('functions.R') 

# create basemap for New Hanover County
nh_base <- get_map(location = c(left = -78.1, bottom = 33.9, right = -77.6, top = 34.41), 
                   zoom = 13, maptype = 'toner', source = 'stamen')

# create 10 color version of 'Reds' palette
mycolors <- colorRampPalette(brewer.pal(9, "Reds"))(10)
