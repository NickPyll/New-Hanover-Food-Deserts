# Function to grab lat and long
grab_latlong <- function(place_id, key){
  
  place_details <- google_place_details(place_id = place_id , key = key)
  
  p_df <- data.frame(
    place_id = place_id,
    lat = place_details$result$geometry$location$lat,
    long = place_details$result$geometry$location$lng,
    stringsAsFactors=FALSE
  )
  
  return(p_df)
}

# Function to plot single heatmap
heatmap_helper <- function(df, category){
  
  coords.map <- 
    ggmap(
      get_stamenmap(
        # base coordinates for wilmington
        c(left = -78.04, bottom = 33.92, right = -77.72, top = 34.39), 
        zoom = 12, maptype = "terrain"), extent = "device", legend = "none") + 
    # density based heatmap
    stat_density2d(data = df,
                   aes(x = long, y = lat, fill = ..level.., alpha = ..level..), 
                   geom = "polygon") + 
    scale_fill_gradientn(colours = brewer.pal(7, "Blues")) + 
    # Add points
    geom_point(data = df, aes(x = long, y = lat), fill = "red", shape = 23, alpha = 0.4) + 
    theme_bw()

  ggsave(filename = paste0("./", category, "coords.png"))
  
}

