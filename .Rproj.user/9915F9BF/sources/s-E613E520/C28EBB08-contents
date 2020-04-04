library(googleway)
library(tidyverse)
library(purrr)

api_key <- "AIzaSyCluNKTUCAsjIRLeOZwkM9FG23owzg_jNw"

# Function to grab lat and long
grab_latlon<- function(place_id, key){
  
  place_details <- google_place_details(place_id = place_id , key = key)
  
  p_df <- data.frame(
    place_id = place_id,
    lat = place_details$result$geometry$location$lat,
    lon = place_details$result$geometry$location$lng,
    stringsAsFactors=FALSE
  )
  
  return(p_df)
}

#### Grocery Data ####

grocery <- google_places(location = c(34.255050, -77.870416), # lat lon for wilmington
                         place_type = "grocery_or_supermarket",
                         radius = 50000, # meters, 50k is the max. It's roughly 31 mile radius
                         key = api_key)

grocery_df <- data.frame(
  place_id = as.character(grocery$results$place_id),
  name = grocery$results$name,
  address = grocery$results$vicinity,
  stringsAsFactors=FALSE
)


grocery_detail <- map_dfr(.x = grocery_df %>% pull(place_id), ~grab_latlon(place_id = .x, key = api_key)) %>%
  left_join(grocery_df, by = "place_id")
