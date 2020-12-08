# set working directory to directory with this script
setwd("~/Documents/Semester 3/Data Viz/Final Project/Maps")

# load required packages, first three should already be installed from classes
library(tidyverse)
library(leaflet)
library(RJSONIO) # you will need to install
library(rgdal) # you will need to install, and also install raster package
library(htmlwidgets) # to save the leaflet map
library(htmltools) # to add some custom code into map, you will need to install

# load data
private <- read_csv("data/private.csv")
public <- read_csv("data/public.csv")
terminals <- read_csv("data/terminals.csv")
metro <- read_csv("data/metro.csv")

# process private data, adding city and country if not present!
private <- private %>%
  filter(!is.na(address)) %>%
  select(1,2)

# process metro data
metro <- metro %>%
  separate(`Geo Point`, into = c("latitude","longitude"), sep = ",") %>%
  mutate(latitude = as.double(latitude),
         longitude = as.double(longitude),
         # create column combining name and line for later use
         name_line = paste0(Nombre,",",Línea))

# geocoding
# enter your Bing Maps key!
# instruction to get a key at https://github.com/paldhous/refine-geocoder
bingmapsKey <- "ArVWzTXFsIhidyLoL1kcdEo-L5lfZrZITqeCRf_Gs67mdbQalF9oQoEv89w-Bpqo"

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

# loop through addresses to geocode. This will take a while.
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
# combine with original data
private <- bind_cols(private,bing_df)

#####
# terminals 

# create empty list to hold geocoded data
bing_list <- c()

# loop through addresses to geocode. 
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

# combine with original data
terminals <- bind_cols(terminals,bing_df)

# clean up environment
rm(bing_list,bing_df,address,tmp,n,possibleError)

# save geocoded data
write_csv(terminals, "data/terminals_geocode.csv", na = "")
write_csv(private, "data/private_geocode.csv", na = "")

# will need to check failed or inaccurate locations manually

errors_locations <- private_distances %>%
filter(confidence == "Medium")
view(errors_locations)
  
  

# read back in
terminals <- read_csv("data/terminals_geocode_edit.csv")
private <- read_csv("data/private_geocode_edit.csv")


# convert clinics and metro stations to spatial points data frames
# use epsg:3857 projection so units are in meters
private <- as.data.frame(private) %>%
  filter(!is.na(latitude))
xy <- private %>%
  select(longitude,latitude)
private <- SpatialPointsDataFrame(coords = xy, 
                                data = private, 
                                proj4string = CRS("+init=epsg:3857"))
# check projection
raster::crs(private)

# and for the metro stations
metro <- as.data.frame(metro) %>%
  filter(!is.na(latitude))
xy <- metro %>%
  select(longitude,latitude)
metro <- SpatialPointsDataFrame(coords = xy, 
                                data = metro, 
                                proj4string = CRS("+init=epsg:3857"))

raster::crs(metro)

# clean up environment
rm(xy)

# calculate matrix of distances in km 
# between the clinics and the metro stations
# note division by 1000 to convert meters to km
# this is where raster package is used
distances <- raster::pointDistance(private, metro, lonlat = TRUE)/1000

# convert to data frame, find the nearest metro station to each private clinic and straight line distance in km
private_distances <- as.data.frame(t(distances))
names(private_distances) <- private@data$clinic_private
private_distances <- private_distances %>%
  mutate(name_line = metro@data$name_line) %>%
  gather(private_clinic,distance,-name_line) %>%
  group_by(private_clinic) %>%
  filter(distance == min(distance)) %>%
  ungroup() %>%
  separate(name_line, into = c("name","line"), sep = ",") %>%
  bind_cols(private@data)

# need this bit for now to filter out the incorrectly geocoded data 
private_distances <- private_distances %>%
  filter(distance < 10)


#####################################
# make map

# set color palette
pal <- colorNumeric("viridis", private_distances$distance, reverse = TRUE)

# leaflet
clinic_map <- leaflet(private_distances) %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  setView(lng = -99.15, lat = 19.4, zoom = 11) %>%
  addCircleMarkers(data = public,
             ~longitude,~latitude,
             radius = sqrt(public$abortions)/15, 
             color = "#000000",
             weight = 0.2,
             fillColor ="#ffffff",
             fillOpacity = 0.5,
             popup = paste0("<b>",public$hospital,"</b><br>Abortos 2007-2019: ", prettyNum(public$abortions, big.mark = " ")),
             group = "Hospitales públicos") %>%
  addCircleMarkers(data = private_distances,
             ~longitude,~latitude,
             color = "#000000",
             weight = 0.2,
             fillColor = ~pal(distance),
             radius = 10,
             fillOpacity = 0.9,
             popup = paste0("<b>",private_distances$private_clinic,
             "<br>Estación de metro más cercana:</b> ", 
             private_distances$name,", ",private_distances$line,"
             <br><b>Distancia: </b>", round(private_distances$distance,2), " km"),
             group = "Clínicas privadas")%>%
  addLegend(
    "bottomright",
    pal = pal,
    values = ~private_distances$distance,
    title = "Distancia<br>a la estación<br>de metro (km)",
    opacity = 0.9
  ) %>%
  addLayersControl(overlayGroups = c("Hospitales públicos","Clínicas privadas"), 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  # this sets min zoom to zoom level on initial map render, prevents unwanted zoom out on scroll when embedded
  onRender("
    function(el, x) {
      var myMap = this
      this.options.minZoom = this.getZoom()
    }
  ")


# save as web page
saveWidget(clinic_map, "clinics.html", selfcontained = TRUE, background = "black")
