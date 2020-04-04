# Zillow Data
zhvi <- 
  read_csv("Zip_Zhvi_AllHomes.csv") %>%
  filter(CountyName == 'New Hanover County') %>%
  gather(date, median, 8:294, na.rm = TRUE) %>%
  mutate(date = as.Date(as.yearmon(date)))

saveRDS(zhvi, file = "zillow_home_value.rds")   