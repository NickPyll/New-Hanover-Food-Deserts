# Zillow Data
x.zhvi <- 
  read_csv("Zip_Zhvi_AllHomes.csv") %>%
  filter(CountyName == 'New Hanover County') %>%
  gather(date, median, 8:294, na.rm = TRUE) %>%
  mutate(date = as.Date(as.yearmon(date)))

x.metro_zri <- 
  read_csv("Metro_Zri_AllHomesPlusMultifamily_Summary.csv") %>%
  filter(is.na(State) | State == 'NC')
  