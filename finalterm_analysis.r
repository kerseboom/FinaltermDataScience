#### HEAD ####
if (!require(install.load)) {
  install.packages("install.load")
}
library(install.load)
install_load(
  "tidyverse",
  "sf",
  "shiny",
  "leaflet",
  "osmdata",
  "readxl",
  "archive",
  "matsim"
)

USETHISCRS <- 4326 #constant crs

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set wd to the directory the file was opened in


#### DOWNLOADING / LOADING ####
if (!file.exists("berlin-v5.5.3-10pct.output_trips.csv.gz")) { #if the matsim output trips aren't available, download 
  
  download.file(url = "https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/scenarios/countries/de/berlin/berlin-v5.5-10pct/output-berlinv5.5/berlin-v5.5.3-10pct.output_trips.csv.gz",
                destfile = "berlin-v5.5.3-10pct.output_trips.csv.gz",
                mode = "wb")
  
  unzip("berlin-v5.5.3-10pct.output_trips.csv.gz", exdir = getwd()) #and unzip them

  }

berlin_10pct_output_trips <-  readTripsTable("berlin-v5.5.3-10pct.output_trips.csv.gz") #load matsim output trips

districts <- st_read("berlin_districts/bezirksgrenzen.shp") #load districts
districts <- st_as_sf(districts) %>% st_transform(USETHISCRS)

trips <- berlin_10pct_output_trips %>%  #select the needed variables
  select(dep_time, trav_time, wait_time, traveled_distance, main_mode, start_activity_type, end_activity_type, start_x, start_y, end_x, end_y)



#### DATA MANIPULATOIN ####
start_sf <- st_as_sf(trips, coords = c("start_x", "start_y"), crs=31468) %>% #build a start df with the coords
  st_transform(USETHISCRS) 

start_sf <- st_join(start_sf, districts) #join the simple feature object with the district shapefile

trips$start_district_name <- start_sf$Gemeinde_n #add the district id to the dataframe

end_sf <- st_as_sf(trips, coords = c("end_x", "end_y"), crs = 31468) %>% #repeat the process for end point
  st_transform(USETHISCRS)

end_sf <- st_join(end_sf, districts)

trips$end_district_name <- end_sf$Gemeinde_n



tvz_matrix <- trips %>% #build a start-end-matrix
  group_by(start_district_name, end_district_name) %>%
  filter(!any(is.na(c(start_district_name, end_district_name)))) %>%
  summarize(count = n()) %>%
  ungroup()

tvz_matrix_joined <- tvz_matrix %>% #add the destination geometry
  left_join(districts, 
            by=c("end_district_name"="Gemeinde_n")) %>% 
  select(start_district_name, end_district_name, count, geometry) %>% 
  st_as_sf() %>% 
  st_transform(USETHISCRS)

tvz_matrix_filtered <- tvz_matrix_joined[0,]
trips_filtered <- trips

#### LEAFLET MAP ####
districts <- st_as_sf(districts) %>% st_transform(USETHISCRS)

map <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap.DE) %>%
  addPolygons(data = districts,           # districts Hintergrund (nur Borders)
              layerId = ~Gemeinde_n,
              color = "#000000",
              weight = 2,
              fillOpacity = 0,
              noClip = T
  ) %>% 
  addPolygons(data = tvz_matrix_filtered,
              layerId = ~end_district_name,
              color = "red",
              weight = 2,
              fillOpacity = 0.75,
              noClip = T
  )


#### SAVE DATA ####

saveRDS(map, file = "LEAFLETMAP.rds")# map
saveRDS(trips, file = "TRIPS.rds")# trips
saveRDS(trips_filtered, file = "TRIPS_FILTERED.rds")# trips_filtered
saveRDS(tvz_matrix_joined, file = "TVZ_MATRIX_JOINED.rds")# start-ziel-matrix
saveRDS(tvz_matrix_filtered, file = "TVZ_MATRIX_FILTERED.rds")# start-ziel-matrix


