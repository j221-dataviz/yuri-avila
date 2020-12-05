# load required packages
library(tidyverse)
library(leaflet)
library(sf)
library(RJSONIO)

# load data
private <- read_csv("data/private.csv")
public <- read_csv("data/public.csv")
terminals <- read_csv("data/terminals.csv")
metro <- read_csv("data/metro.csv")

private <- private %>%
  filter(!is.na(address))

# process metro data
metro <- metro %>%
  separate(`Geo Point`, into = c("latitude","longitude"), sep = ",")

# geocoding
# enter your Bing Maps key
# instruction to get a key at https://github.com/paldhous/refine-geocoder
bingmapsKey <- "AgMAoU0H8oZK0QtLrUilQ4BjxHZfGjzLra3qADXFzmuKvjuTGwQwQI3noUWtP1gJ"

# geocoding function
bingGeocode <- function(address,bingmapsKey) {
  url <- URLencode(paste0("http://dev.virtualearth.net/REST/v1/Locations?q=",address,"&maxResults=1&key=",bingmapsKey))
  json <- fromJSON(url) 
  latitude <- json$resourceSets[[1]]$resources[[1]]$point$coordinates[[1]]
  longitude <- json$resourceSets[[1]]$resources[[1]]$point$coordinates[[2]]
  type <- json$resourceSets[[1]]$resources[[1]]$entityType[[1]]
  confidence <- json$resourceSets[[1]]$resources[[1]]$confidence[[1]]
  data.frame(address,latitude,longitude,type,confidence)
}  

#####
# private clinics

# create empty list to hold geocoded data
bing_list <- c()

# loop through addresses to geocode. This will take a while, I would not run again.
# error handling returns address alone if geocoding fails
n <- 0
for (address in private$address) {
  possibleError <- tryCatch(
    tmp <- bingGeocode(address,bingmapsKey),
    error=function(e) e
  )
  if(inherits(possibleError, "error")) {
    tmp <- data.frame(address)
  }
  bing_list[[length(bing_list)+1]] <- tmp
  n <- n+1
  print(paste0("addresses geocoded by Bing Maps=",n))
}
# combine list into a single data frame
bing_df <- bind_rows(bing_list) %>%
  select(2:5)

private <- bind_cols(private,bing_df)


#####
# terminals 

# create empty list to hold geocoded data
bing_list <- c()

# loop through addresses to geocode. This will take a while, I would not run again.
# error handling returns address alone if geocoding fails
n <- 0
for (address in terminals$address) {
  possibleError <- tryCatch(
    tmp <- bingGeocode(address,bingmapsKey),
    error=function(e) e
  )
  if(inherits(possibleError, "error")) {
    tmp <- data.frame(address)
  }
  bing_list[[length(bing_list)+1]] <- tmp
  n <- n+1
  print(paste0("addresses geocoded by Bing Maps=",n))
}
# combine list into a single data frame
bing_df <- bind_rows(bing_list) %>%
  select(2:5)

terminals <- bind_cols(terminals,bing_df)

# save geocoded data
write_csv(terminals, "data/terminals_geocode.csv", na = "")
write_csv(private, "data/private_geocode.csv", na = "")

# will need to check failed or inaccurate locations manual

# read back in
terminals <- read_csv("data/terminals_geocode_edit.csv")
private <- read_csv("data/private_geocode_edit.csv")

#########
# create buffers around metro stations and terminals

terminals <- st_as_sf(terminals, coords = c("longitude","latitude"), crs = 6362)
metro <- st_as_sf(metro, coords = c("longitude","latitude"), crs = 6362)

# 1km around metro
# 3km around terminals

test <- st_buffer(terminals, 1000)

ggplot(test) + geom_sf()


# leaflet
leaflet() %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addPolygons(data = test)
  
  
  addMarkers(data = public,
             ~long,~lat,
             popup = paste0("<b>",public$Hospital,"</b><br>Abortions: ", public$`Abortions 2007-2020`),
             group = "public") %>%
  addMarkers(data = private_edit,
             ~longitude,~latitude,
             group = "private") %>%
  addLayersControl(overlayGroups = c("public","private"), 
                   options = layersControlOptions(collapsed = FALSE))


