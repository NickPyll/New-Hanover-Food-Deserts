# Google API data

#### Grocery Data ####
x.grocery <- google_places(location = c(34.255050, -77.870416), # lat long for Wilmington
                           place_type = "grocery_or_supermarket",
                           radius = 50000, # meters, 50k is the max. It's roughly 31 mile radius
                           key = api_key)

# creates dataframe with name and address
x.grocery_df <- data.frame(
  place_id = as.character(x.grocery$results$place_id),
  name = x.grocery$results$name,
  address = x.grocery$results$vicinity,
  stringsAsFactors=FALSE
)

# use another google function to grab lat long
x.grocery_detail <- 
  map_dfr(.x = x.grocery_df %>% 
            pull(place_id), 
          ~grab_latlong(place_id = .x, 
                        key = api_key)) %>%
  left_join(x.grocery_df,
            by = "place_id")

# subset 
grocery.coords <- 
  x.grocery_detail %>%
  dplyr::select(lat, long)

#### Grocery Data ####
x.grocery <- google_places(location = c(34.255050, -77.870416), # lat long for Wilmington
                           place_type = "grocery_or_supermarket",
                           radius = 50000, # meters, 50k is the max. It's roughly 31 mile radius
                           key = GOOGLE_API_KEY)

# creates dataframe with name and address
x.grocery_df <- data.frame(
  place_id = as.character(x.grocery$results$place_id),
  name = x.grocery$results$name,
  address = x.grocery$results$vicinity,
  stringsAsFactors = FALSE
)

# use another google function to grab lat long
x.grocery_detail <- 
  map_dfr(.x = x.grocery_df %>% 
            pull(place_id), 
          ~grab_latlong(place_id = .x, 
                        key = GOOGLE_API_KEY)) %>%
  left_join(x.grocery_df,
            by = "place_id")

# subset 
grocery.coords <- 
  x.grocery_detail %>%
  dplyr::select(lat, long)