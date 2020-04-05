# create basemap for New Hanover
nh_base <- get_map(location = c(left = -78.1, bottom = 33.9, right = -77.6, top = 34.41), 
                   zoom = 13, maptype = 'toner', source = 'stamen')

nb.cols <- 10
mycolors <- colorRampPalette(brewer.pal(9, "Reds"))(nb.cols)

# Function to grab lat and long
grab_latlong <- function(place_id, key){
  
  place_details <- google_place_details(place_id = place_id , key = key)
  
  p_df <- data.frame(
    place_id = place_id,
    lat = place_details$result$geometry$location$lat,
    long = place_details$result$geometry$location$lng,
    stringsAsFactors = FALSE
  )
  
  return(p_df)
}

# Function to plot single heatmap
heatmap_helper <- function(df, category){
  
  coords.map <- 
    ggmap(nh_base) + 
    # density based heatmap
    stat_density2d(data = df,
                   aes(x = long, y = lat, 
                       fill = ..level.., alpha = ..level..), 
                   geom = "polygon") + 
    scale_fill_gradientn(colours = brewer.pal(7, "Blues")) + 
    # Add points
    geom_point(data = df, aes(x = long, y = lat), 
               fill = "red", shape = 23, alpha = 0.4) + 
    guides(fill = FALSE, alpha = FALSE) +
    labs(y = "Latitude", x = "Longitude") 
  
  ggsave(filename = paste0("./", category, "coords.png"))
  
}

# Function to plot zip gradient
zip_gradient_helper <- function(df, df2, category){

  ggmap(nh_base) +
    geom_sf(data = df, aes(fill = as.factor(avg_agi_stub)), 
            inherit.aes = FALSE, lwd = .2) +
    scale_fill_manual(values = mycolors) +
    coord_sf(crs = st_crs(4326)) +
    geom_point(data = df2, aes(x = long, y = lat), fill = "red", shape = 23) + 
    guides(fill = FALSE, alpha = FALSE) +
    labs(y = "Latitude", x = "Longitude")
  
  ggsave(filename = paste0("./", category, "coords.png"))
  
}


