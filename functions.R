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
heatmap_helper <- function(df, basemap, category){
  
  coords.map <- 
    ggmap(basemap) + 
    # density based heatmap
    stat_density2d(data = df,
                   aes(x = long, y = lat, 
                       fill = ..level.., alpha = ..level..), 
                   geom = "polygon") + 
    scale_fill_gradientn(colours = brewer.pal(7, "Blues")) + 
    # Add points
    geom_point(data = df, aes(x = long, y = lat), 
               fill = "red", shape = 23, alpha = 0.4) + 
    geom_sf(data = food.desert_sf, color = 'black', alpha = 0, inherit.aes = FALSE) +
    guides(fill = FALSE, alpha = FALSE) +
    labs(y = "Latitude", x = "Longitude") 
  
  ggsave(filename = paste0("./", category, "coords.png"))
  
}

# create 10 color version of 'Reds' palette
mycolors <- colorRampPalette(brewer.pal(9, "Reds"))(10)

# Function to plot zip gradient
zip_gradient_helper <- function(df, df2, df3, basemap,category){

  ggmap(basemap) +
    geom_sf(data = df, aes(fill = as.factor(avg_agi_stub)), 
            inherit.aes = FALSE, lwd = .2) +
    geom_sf(data = df3, color = 'black', size = 1, fill = NA, inherit.aes = FALSE) +
    scale_fill_manual(values = mycolors) +
    coord_sf(crs = st_crs(4326)) +
    geom_point(data = df2, aes(x = long, y = lat), fill = "red", shape = 23) + 
    guides(fill = FALSE, alpha = FALSE) +
    labs(y = "Latitude", x = "Longitude")
  
  ggsave(filename = paste0("./", category, "coords.png"))
  
}

# Function to calculate walkscore
getwalkscore <- function(lat, lon){
  
  # add code to check if environmental variable exists
  ws_key <- WALKSCORE_API_KEY
  link <- glue("http://api.walkscore.com/score?format=json&lat={lat}&lon={lon}&transit=1&bike=1&wsapikey={ws_key}&research=yes")
  resp <- httr::GET(link)

  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  resp_list <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  
  if(resp_list$status == 1){
    
    ws_list <- list(
      "ws" = resp_list$walkscore,
      "ws_desc" = resp_list$description,
      "bs" = resp_list$bike$score,
      "bs_desc" = resp_list$bike$description,
      "ts" = resp_list$transit$score,
      "ts_desc" = resp_list$transit$description,
      "snapped_lat" = resp_list$snapped_lat,
      "snapped_lon" = resp_list$snapped_lon
    )
    
    ws_list <- map(.x = ws_list, ~ifelse(is.null(.x), NA, .x))
    
  } else {
    
    error_msgs <- list(
      "1"  = "Your query was successful. I don't know why you're getting this error message... \n",
      "2"  = "Your query was not successful. Score is being calculated and is not currently available.\n",
      "30" = "Your query was not successful. Invalid latitude/longitude.\n",
      "31" = "Your query was not successful. Walk Score API internal error.\n",
      "40" = "Your query was not successful. Your WSAPIKEY is invalid.\n",
      "41" = "Your query was not successful. Your daily API quota has been exceeded.\n",
      "42" = "Your query was not successful. Your IP address has been blocked.\n"
    )
    
    warning(error_msgs[[as.character(resp_list$status)]])
    score_names <- list("ws", "ws_desc", "bs", "bs_desc", "ts", "ts_desc", "snapped_lat", "snapped_lon")
    ws_list <- map(score_names, ~NA) %>% set_names(score_names)
    
  }

  return(ws_list)
  
}

# Function to bulk score
getwalkscore_bulk <- function(lat_list, lon_list){
  
  ws_df <- map2_dfr(
    .x = lat_list,
    .y = lon_list,
    ~(getwalkscore(lat = .x, lon = .y)) %>% as.data.frame
  )
  
  return(ws_df)
  
}
