#### Maps ####

heatmap_helper(grocery.coords, 'grocery') # heat map for grocery
zip_gradient_helper(zips_sf, grocery.coords, 'income') # heat map for income by zip

# Create heatmaps for mobility
walkscore %>%
  rename(
    Walk = ws,
    Bike = bs,
    Transit = ts) %>%
  pivot_longer(cols = c("Walk", "Bike", "Transit"), 
               names_to = "score_type", values_to = "Score") %>%
  ggplot() + 
  geom_sf(data = nh, colour = NA, fill = 'gray96') +
  geom_tile(aes(x = snapped_lon, y = snapped_lat, fill = Score, 
                interpolate = TRUE, na.remove = FALSE)) +
  scale_fill_gradientn(colours = brewer.pal(7, "Oranges")) + 
  geom_sf(data = data.shape, color = 'black', alpha = 0) +
  facet_grid(cols = vars(score_type)) +
  theme(
    legend.key.size = unit(1, "cm"),
    legend.key = element_blank(), 
    legend.title = element_text(size = 16), 
    legend.text = element_text(size = 14),
    strip.background = element_rect(colour = "grey", fill = "white"),   
    strip.text = element_text(size = 20),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) 

#### Time Series ####

# plot monthly median home value by zip
home_value_trends_zip <- 
  plot_ly(zillow_home_value, 
          x = ~date, y = ~median, 
          color = ~RegionName, type = 'scatter', 
          mode = 'lines', colors = 'Paired') %>%
  layout(xaxis = list(title = "Date"),
         yaxis = list(title = "Median Home Value")) %>%
  layout(
    xaxis = list(titlefont = list(size = 18), 
                 tickfont = list(size = 15)), 
    yaxis = list(titlefont = list(size = 18),
                 tickfont = list(size = 15),
                 tickformat = '$,'))
home_value_trends_zip

# plot monthly change in median home value by zip
home_value_diff_zip <- 
  plot_ly(zillow_home_value_monthly_diff, 
          x = ~date, y = ~percent_diff, 
          color = ~RegionName, type = 'scatter', mode = 'lines', colors = 'Paired') %>%
  layout(xaxis = list(title = "Date"),
         yaxis = list(title = "Median Home Value")) %>%
  layout(
    xaxis = list(titlefont = list(size = 18), 
                 tickfont = list(size = 15)), 
    yaxis = list(titlefont = list(size = 18),
                 tickfont = list(size = 15),
                 tickformat = '$,'))
home_value_diff_zip

#### Bubble Plot ####

# plot median housing price against rate of price growth
# size of dot is percent of residents at low income
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

#### County Data ####

x.county_rank <- 
  county_metric %>%
  select(contains('_rank'))
x.county_rank_cor <- cor(x.county_rank)
colnames(x.county_rank_cor) <- c('Poverty %', 'Poverty % (u17)', 'Household Income', 'Population Growth', 'Education (18-24)', 'Unemployed %')
rownames(x.county_rank_cor) <- c('Poverty %', 'Poverty % (u17)', 'Household Income', 'Population Growth', 'Education (18-24)', 'Unemployed %')
corrplot(x.county_rank_cor, type = 'lower')

x.county_raw <- 
  county_metric %>%
  select(!contains('_rank'), -county)
x.county_raw_cor <- cor(x.county_raw)
colnames(x.county_raw_cor) <- c('Poverty %', 'Poverty % (u17)', 'Household Income', 'Population Growth', 'Education (18-24)', 'Unemployed %')
rownames(x.county_raw_cor) <- c('Poverty %', 'Poverty % (u17)', 'Household Income', 'Population Growth', 'Education (18-24)', 'Unemployed %')
corrplot(x.county_raw_cor, type = 'lower', diag = FALSE)

