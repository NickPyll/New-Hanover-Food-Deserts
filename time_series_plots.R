# Time Series plots...
home_value_trends_zip <- 
  plot_ly(zillow_home_value, 
          x = ~date, y = ~median, 
          color = ~RegionName, type = 'scatter', 
          mode = 'lines', colors = 'Set1') %>%
  layout(xaxis = list(title = "Date"),
         yaxis = list(title = "Median Home Value")) %>%
  layout(
    xaxis = list(titlefont = list(size = 18), 
                 tickfont = list(size = 15)), 
    yaxis = list(titlefont = list(size = 18),
                 tickfont = list(size = 15),
                 tickformat = '$,'))
home_value_trends_zip

home_value_diff_zip <- 
  plot_ly(zillow_home_value_monthly_diff, 
          x = ~date, y = ~percent_diff, 
          color = ~RegionName, type = 'scatter', mode = 'lines', colors = 'Set1') %>%
  layout(xaxis = list(title = "Date"),
         yaxis = list(title = "Median Home Value"))
home_value_diff_zip

