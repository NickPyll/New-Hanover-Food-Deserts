source('setup.R')
source('google_API_data.R')
source('zillow_API_data.R')
source('data.R')

heatmap_helper(grocery.coords, 'grocery')

pg <- plot_ly(x.zillow, x = ~date, y = ~median, color = ~RegionName, type = 'scatter', mode = 'lines')
pg
