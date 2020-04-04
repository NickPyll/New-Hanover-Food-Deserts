source('setup.R') # Load Libraries and Functions

source('data.R') # Load RDS files (previously compiled by various other scripts)

heatmap_helper(grocery.coords, 'grocery') # heat map for grocery

zip_gradient_helper(zips_sf, grocery.coords, 'income') # heat map for income by zip

pg <- plot_ly(zillow_home_value, x = ~date, y = ~median, color = ~RegionName, type = 'scatter', mode = 'lines')
pg


