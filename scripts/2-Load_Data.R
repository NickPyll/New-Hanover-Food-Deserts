grocery.coords <- readRDS("./data/grocery.coords.rds")
zillow_home_value <- readRDS("./data/zillow_home_value.rds")
zillow_home_value_monthly_diff <- readRDS("./data/zillow_home_value_monthly_diff.rds")
zillow_home_value_monthly_diff_wide <- readRDS("./data/zillow_home_value_monthly_diff_wide.rds")
zillow_value_vs_growth <- readRDS("./data/zillow_value_vs_growth.rds")
county_metric <- readRDS("./data/county_metrics.rds")
walkscore <- readRDS("./data/nh_walkscoredata.rds")
nh_basic_map <- readRDS("./data/nh_basic_map.rds")  
nh_base <- readRDS("./data/nh_base_ggmap.rds") 
zips_sf <- readRDS("./data/zips_sf.rds")
food.desert_sf <- readRDS("./data/food_desert_shapes.rds") %>% st_set_crs(4269)
