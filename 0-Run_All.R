source('1-Setup Environment.R') # Load Libraries and Functions

source('2-Load Data.R') # Load RDS files (previously compiled by various other scripts)

# Maps
heatmap_helper(grocery.coords, 'grocery') # heat map for grocery
zip_gradient_helper(zips_sf, grocery.coords, 'income') # heat map for income by zip

# Time Series Plots
source('time_series_plots.R')


ggplot(zillow_value_vs_growth, 
       aes(x = percent_diff/100, y = zhvi)) + 
  geom_point(aes(color = RegionName, 
                 size = pct_low_income), alpha = 0.8) +
  geom_label(zillow_value_vs_growth %>% 
               filter(percent_diff > 1), 
             mapping = aes(label = RegionName),
             nudge_y = -90000, size = 6) +
  geom_label(zillow_value_vs_growth %>% 
               filter(zhvi > 750000), 
             mapping = aes(label = RegionName),
             nudge_y = -50000, size = 6) +
  scale_color_brewer(palette = 'Paired') +
  scale_size(range = c(5, 22))  +
  theme_bw() +
  scale_y_continuous(labels = dollar, 
                     limits = c(0, 1000000)) +
  scale_x_continuous(labels = percent, 
                     limits = c(.0025, .0150)) +
  xlab('Projected Monthly Growth of Housing Prices') +
  ylab('Median Home Value') +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18)) 
  