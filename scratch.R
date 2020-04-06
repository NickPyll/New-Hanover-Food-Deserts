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

x.county <-
  x.poverty_data %>%
  filter(county != 'North Carolina') %>%
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
                     educated_18_24 = 
                       rescale(
                         (1*pop_18_24_less_hs +
                          2*pop_18_24_hs_equiv +
                          3*pop_18_24_some_college_assdeg +
                          4*pop_18_24_bach)/pop_18_24)),
            by = 'county')


df <- 
  x.unemployment_data %>%
  mutate(county = str_remove(county_name_state_abbreviation, ", North Carolina"),
         unemployed_pct = unemployed/labor_force)
         
  
  
  
  
  
  
  