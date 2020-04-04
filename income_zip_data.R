# pull zip codes from zillow data
wilmington_zips <- unique(zillow_home_value$RegionName)

# read IRS data in
zip_data_IRS <- read_csv("17zpallagi.csv") %>%
  filter(zipcode %in% wilmington_zips) %>%
  mutate(agi_n = agi_stub*N1) %>%
  group_by(zipcode) %>%
  # calculate weighted average of income
  summarize(weighted_sum = sum(agi_n),
            n_sum = sum(N1)) %>%
  ungroup() %>%
  mutate(avg_agi_stub = weighted_sum/n_sum)

# translate to shape file
zips_sf <- zctas(cb = T, starts_with = "28", class = "sf") %>%
  select(zip = ZCTA5CE10, geometry) %>%
  filter(zip %in% wilmington_zips)

# combine shape with income values
zips_sf %<>%
  inner_join(zip_data_IRS %>%
               mutate(zip = zipcode),
             by = 'zip')

# save as RDS
saveRDS(zips_sf, file = "zips_sf.rds") 