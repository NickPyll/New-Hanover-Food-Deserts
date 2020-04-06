#### Grocery Data ####

x.fd_sf <- st_read("/Users/pylypiw/Documents/New Hanover Raw Data/zow2iDfy9w_poly.shp") 

# Run Once, but then output to .RDS to stop continuous calls to API
x.grocery <- google_places(location = c(34.255050, -77.870416), # lat long for Wilmington
                           place_type = "grocery_or_supermarket",
                           radius = 50000, # meters, 50k is the max. It's roughly 31 mile radius
                           key = GOOGLE_API_KEY)

# creates dataframe with name and address
x.grocery <- data.frame(
  place_id = as.character(x.grocery$results$place_id),
  name = x.grocery$results$name,
  address = x.grocery$results$vicinity,
  stringsAsFactors = FALSE
)

# use another google function to grab lat long
x.grocery <- 
  map_dfr(.x = x.grocery %>% 
            pull(place_id), 
          ~grab_latlong(place_id = .x, 
                        key = GOOGLE_API_KEY)) %>%
  left_join(x.grocery,
            by = "place_id")

# Harris Teeter and Food Lion, both big players in the area, were not showing up in Google API
# this can be updated if necessary
x.grocery_manual <-
  tribble(
    ~place_id, ~lat, ~long, ~name, ~address,
    "1", 34.130987, -77.901760, "Harris Teeter", "",
    "2", 34.198755, -77.885117, "Harris Teeter", "",
    "3", 34.220191, -77.898163, "Harris Teeter", "",
    "4", 34.223739, -77.814392, "Harris Teeter", "",
    "5", 34.240326, -77.825335, "Harris Teeter", "",
    "6", 34.249494, -77.870529, "Harris Teeter", "",
    "7", 34.265764, -77.826999, "Harris Teeter", "",
    "8", 34.261088, -77.825083, "Harris Teeter", "",
    "9", 34.223271, -78.017590, "Harris Teeter", "",
    "10", 34.242222, -77.975593, "Food Lion", "",
    "11", 34.268565, -78.032107, "Food Lion", "",
    "12", 34.298729, -77.922586, "Food Lion", "",
    "13", 34.316061, -77.876833, "Food Lion", "",
    "14", 34.305764, -77.794465, "Food Lion", "",
    "15", 34.265380, -77.826882, "Food Lion", "",
    "16", 34.234410, -77.827141, "Food Lion", "",
    "17", 34.243809, -77.888121, "Food Lion", "",
    "18", 34.229283, -77.922229, "Food Lion", "",
    "19", 34.200865, -77.913702, "Food Lion", "",
    "20", 34.177182, -77.922937, "Food Lion", "",
    "21", 34.181027, -77.893109, "Food Lion", "",
    "22", 34.110584, -77.901203, "Food Lion", "",
    "23", 34.055658, -77.901726, "Food Lion", ""
  )

# add manual data to API results
x.grocery.coords <- 
  x.grocery %>%
  bind_rows(x.grocery_manual) %>%
  # Google API picking up a few stores way outside the county
  filter(place_id != 'ChIJxUwcx0cHqokRNOTuLZSVoUs') %>%
  filter(place_id != 'ChIJ6f-GJ2A-qokRyvHkCb87800') %>%
  dplyr::select(lat, long)

#### Zillow Data ####

# monthly median home values by zip code
x.zhvi <- 
  read_csv("/Users/pylypiw/Documents/New Hanover Raw Data/Zip_Zhvi_AllHomes.csv") %>%
  filter(CountyName == 'New Hanover County') %>%
  gather(date, median, 8:294, na.rm = TRUE) %>%
  mutate(date = as.Date(as.yearmon(date)))

# create range assumptions
x.zhvi_full <-
  x.zhvi %>%
  filter(date >= '2019-01-01') %>%
  group_by(RegionName) %>%
  summarize(zhvi = mean(median)) %>%
  ungroup() 

# monthly lag diff in median home values by zip code
x.zhvi_lag <- 
  x.zhvi %>%
  group_by(RegionName) %>%
  mutate(lag_median = dplyr::lag(median, n = 1, default = NA)) %>%
  mutate(percent_diff = 100*(median - lag_median)/lag_median) %>%
  select(RegionName, date, percent_diff) %>%
  ungroup()

# manipulate data for multiple time series
# nest by group
x.zhvi_lag_nest <- 
  x.zhvi_lag %>%
  filter(date > as.Date('2013-01-01')) %>%
  group_by(RegionName) %>%
  nest()

# create ts object
x.zhvi_lag_nest_ts <- 
  x.zhvi_lag_nest %>%
  mutate(data.ts = map(.x = data, 
                       .f = tk_ts, 
                       select = -date, 
                       start = 1999,
                       freq = 12))

# model ets 
x.zhvi_lag_fit <- x.zhvi_lag_nest_ts %>%
  mutate(fit.ets = map(data.ts, ets))

# view model results
x.zhvi_lag_fit %>%
  mutate(tidy = map(fit.ets, sw_tidy)) %>%
  unnest(tidy) %>%
  spread(key = RegionName, value = estimate)

# view model results
x.zhvi_lag_fit %>%
  mutate(glance = map(fit.ets, sw_glance)) %>%
  unnest(glance)

# calculate residuals
x.model_residual_ets <- x.zhvi_lag_fit %>%
  mutate(augment = map(fit.ets, sw_augment, timetk_idx = FALSE, rename_index = 'date')) %>%
  unnest(augment)

# plot residuals
x.model_residual_ets %>%
  ggplot(aes(x = as.Date(date), y = .resid, group = RegionName)) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_line(color = palette_light()[[2]]) +
  geom_smooth(method = "loess") +
  labs(title = "Monthly Change in Median Home Value by Zip Code",
       subtitle = "ETS Model Residuals", x = "") + 
  theme_tq() +
  facet_wrap(~RegionName, scale = "free_y", ncol = 3) +
  scale_x_date(date_labels = "%Y")

# view seasonal decomposition
x.zhvi_lag_fit %>%
  mutate(decomp = map(fit.ets, sw_tidy_decomp, timetk_idx = TRUE, rename_index = "date")) %>%
  unnest(decomp)

# forecast
x.zhvi_lag_forecast <- x.zhvi_lag_fit %>%
  mutate(fcast.ets = map(fit.ets, forecast, h = 24))

# tidy forecasted values
x.zhvi_lag_forecast %<>%
  mutate(sweep = map(fcast.ets, sw_sweep, fitted = FALSE, timetk_idx = TRUE, rename_index = "date")) %>%
  unnest(sweep)

# plot results
x.zhvi_lag_forecast %>%
  ggplot(aes(x = as.Date(date), y = percent_diff, color = key, group = RegionName)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_line() +
  labs(title = "Monthly Change in Median Home Value by Zip Code",
       subtitle = "ETS Model Forecasts", 
       x = "Date", y = "Monthly Percent Change") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_tq() +
  scale_fill_tq() +
  facet_wrap(~RegionName, ncol = 3) +
  theme_tq() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### IRS Data ####

# pull zip codes from zillow data
wilmington_zips <- unique(x.zhvi$RegionName)

# read IRS data in
x.zip_data_IRS <- 
  read_csv("/Users/pylypiw/Documents/New Hanover Raw Data/17zpallagi.csv") %>%
  filter(zipcode %in% wilmington_zips) 

x.zip_agg <-
  x.zip_data_IRS %>%
  mutate(agi_n = agi_stub*N1) %>%
  group_by(zipcode) %>%
  # calculate weighted average of income
  summarize(weighted_sum = sum(agi_n),
            n_sum = sum(N1)) %>%
  ungroup() %>%
  mutate(avg_agi_stub = weighted_sum/n_sum)

# translate to shape file
x.zips_sf <- zctas(cb = T, starts_with = "28", class = "sf") %>%
  select(zip = ZCTA5CE10, geometry) %>%
  filter(zip %in% wilmington_zips)

# combine shape with income values
x.zips_sf %<>%
  inner_join(x.zip_agg %>%
               mutate(zip = zipcode),
             by = 'zip')

x.zhvi_growth <-
  x.zhvi_full %>%
  select(RegionName, zhvi) %>%
  inner_join(x.zhvi_lag_forecast %>%
               group_by(RegionName) %>%
               slice(which.max(date)) %>%
               select(RegionName, percent_diff),
             by = 'RegionName') %>%
  inner_join(x.zip_data_IRS %>%
               mutate(RegionName = zipcode,
                      income = if_else(agi_stub < 3, 'low', 'high')) %>%
               group_by(RegionName, income) %>%
               summarize(population = sum(N1)) %>%
               ungroup() %>%
               spread(income, population) %>%
               mutate(pct_low_income = low / (low + high)),
             by = 'RegionName')

#### County Data ####

# census.gov
x.poverty_data <- read_csv("/Users/pylypiw/Documents/New Hanover Raw Data/est18-nc_clean.csv")
x.population_data <- read_csv("/Users/pylypiw/Documents/New Hanover Raw Data/co-est2019-annres-37_clean.csv") %>%
  clean_names()

# American community survey
x.education_data <- read_csv("/Users/pylypiw/Documents/New Hanover Raw Data/ACSST1Y2018.S1501_data_with_overlays_2020-04-05T121851_clean.csv") %>%
  clean_names()

# us bureau of labor statistics
x.unemployment_data <- read_csv("/Users/pylypiw/Documents/New Hanover Raw Data/laucnty18_clean.csv") %>%
  clean_names() %>%
  filter(state_fips == 37)

# combine county data
x.county <-
  x.poverty_data %>%
  filter(county != 'North Carolina') %>%
  select(county, poverty_pop_pct, poverty_pop_u17_pct, median_household) %>%
  inner_join(x.population_data %>%
               filter(geographic_area != 'North Carolina') %>%
               mutate(county = str_remove(geographic_area, "."),
                      county = str_remove(county, ", North Carolina")) %>%
               gather('year', 'population', x2010:x2019) %>%
               mutate(year = as.numeric(str_remove(year, "x"))) %>%
               group_by(county) %>%
               mutate(lag_population = dplyr::lag(population, n = 1, default = NA)) %>%
               mutate(percent_diff = 100*(population - lag_population)/lag_population) %>%
               summarize(mean_annual_population_growth = mean(percent_diff, na.rm = TRUE)) %>%
               ungroup() %>%
               mutate(pop_growth_rank = rank(-mean_annual_population_growth)),
             by = 'county') %>%
  left_join(x.education_data %>%
              mutate(county = str_remove(geographic_area_name, ", North Carolina"),
                     pct_18_24_less_hs = pop_18_24_less_hs / pop_18_24,
                     pct_18_24_hs_equiv = pop_18_24_hs_equiv / pop_18_24,
                     pct_18_24_some_college_assdeg = pop_18_24_some_college_assdeg / pop_18_24,
                     pct_18_24_bach = pop_18_24_bach / pop_18_24,
                     educated_18_24 = 
                       rescale(
                         (1*pop_18_24_less_hs +
                            2*pop_18_24_hs_equiv +
                            3*pop_18_24_some_college_assdeg +
                            4*pop_18_24_bach)/pop_18_24)) %>%
              select(county, educated_18_24),
            by = 'county') %>%
  inner_join(x.unemployment_data %>%
               mutate(county = str_remove(county_name_state_abbreviation, ", NC"),
                      unemployed_pct = unemployed/labor_force) %>%
               select(county, unemployed_pct),
             by = 'county') %>%
  mutate(poverty_pop_pct_rank = rank(poverty_pop_pct),
         poverty_pop_u17_pct_rank = rank(poverty_pop_u17_pct), 
         median_household_rank = rank(-median_household),
         educated_18_24_rank = rank(-educated_18_24),
         educated_18_24_rank = if_else(educated_18_24_rank > 41, 42, educated_18_24_rank),
         unemployed_pct_rank = rank(unemployed_pct)) %>%
  replace_na(list(educated_18_24 = 0)) %>%
  select(county,
         poverty_pop_pct, poverty_pop_pct_rank,
         poverty_pop_u17_pct, poverty_pop_u17_pct_rank,
         median_household, median_household_rank,
         mean_annual_population_growth, pop_growth_rank,
         educated_18_24, educated_18_24_rank,
         unemployed_pct, unemployed_pct_rank)

#### Walkscore Data #### 
# Commented out to restrict going over call limit
# x.nh <- st_read("/Users/pylypiw/Documents/New Hanover Raw Data/wFP0KoyUoJ_poly.shp") 

# x.nh_grid_df <- nh %>% 
#   st_make_grid(cellsize = 0.01, what = "centers") %>%
#   st_coordinates() %>% 
#   as.data.frame() %>%
#   mutate(pt_id = row_number()) %>%
#   rename(grid_lon = X) %>%
#   rename(grid_lat = Y)

# x.lats <- x.nh_grid_df %>% pull(grid_lat)
# x.lons <- x.nh_grid_df %>% pull(grid_lon)
# x.nh_ws <- getwalkscore_bulk(lat_list = lats, lon_list = lons)

#### Save RDS ####
saveRDS(x.grocery.coords, file = "grocery.coords.rds") 
saveRDS(x.zhvi, file = "zillow_home_value.rds")   
saveRDS(x.zhvi_lag, file = "zillow_home_value_monthly_diff.rds")   
saveRDS(x.zips_sf, file = "zips_sf.rds") 
saveRDS(x.zhvi_growth, file = "zillow_value_vs_growth.rds")   
saveRDS(x.county, file = "county_metrics.rds")   
saveRDS(x.nh_ws, file = "nh_walkscoredata.rds")  
saveRDS(x.nh, file = "nh_basic_map.rds")  
saveRDS(x.fd_sf, file = "food_desert_shapes.rds")

# remove unnecessary objects
rm(list = ls(pattern = "^x"))
