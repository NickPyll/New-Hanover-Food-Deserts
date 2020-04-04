# Create RDS file for grocery data

#### Grocery Data ####
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
grocery.coords <- 
  x.grocery %>%
  bind_rows(x.grocery_manual) %>%
  # Google API picking up a few stores way outside the county
  filter(place_id != 'ChIJxUwcx0cHqokRNOTuLZSVoUs') %>%
  filter(place_id != 'ChIJ6f-GJ2A-qokRyvHkCb87800') %>%
  dplyr::select(lat, long)

# save as RDS
saveRDS(grocery.coords, file = "grocery.coords.rds") 