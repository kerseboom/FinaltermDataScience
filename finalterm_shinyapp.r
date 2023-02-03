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

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set wd to the directory the file was opened in

USETHISCRS <- 4326 #constant crs


#### DOWNLOADING / LOADING ####
if (!file.exists("data/berlin-v5.5.3-10pct.output_trips.csv.gz")) { #if the matsim output trips aren't available, download 
  download.file(url = "https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/scenarios/countries/de/berlin/berlin-v5.5-10pct/output-berlinv5.5/berlin-v5.5.3-10pct.output_trips.csv.gz",
                destfile = "data/berlin-v5.5.3-10pct.output_trips.csv.gz",
                mode = "wb")
}

berlin_10pct_output_trips <-  readTripsTable("data/berlin-v5.5.3-10pct.output_trips.csv.gz") #load matsim output trips

trips <- berlin_10pct_output_trips %>%  #select the needed variables
  select(dep_time, trav_time, wait_time, traveled_distance, main_mode, start_activity_type, end_activity_type, start_x, start_y, end_x, end_y)


if (!file.exists("data/berlin_districts/bezirksgrenzen.shp")) { #if district shape files aren't available, download
  download.file(url = "https://tsb-opendata.s3.eu-central-1.amazonaws.com/bezirksgrenzen/bezirksgrenzen.shp.zip",
                destfile = "data/berlin_districts.zip",
                mode = "wb")
  
  unzip("data/berlin_districts.zip", exdir = "data/berlin_districts")
}

districts <- st_read("data/berlin_districts/bezirksgrenzen.shp")
districts <- st_as_sf(districts) %>% st_transform(USETHISCRS)


if (!file.exists("data/district_population.xlsx")) { # if district population data aren't available, download
  download.file(url = "https://download.statistik-berlin-brandenburg.de/31b0e1f55fbede61/0a9d61e4323b/SB_A01-05-00_2022h01_BE.xlsx",
                destfile = "data/district_population.xlsx",
                mode = "wb")
}

pop_data <- read_excel("data/district_population.xlsx", range = "T5!A7:B19")
pop_data <- pop_data %>% rename(Gemeinde_n = ...1,
                                pop = Bezirke)


#### DATA MANIPULATOIN ####
start_sf <- st_as_sf(trips, coords = c("start_x", "start_y"), crs=31468) %>% #build a start df with the coords
  st_transform(USETHISCRS) 

start_sf <- st_join(start_sf, districts) #join the simple feature object with the district shapefile

trips$start_district_name <- start_sf$Gemeinde_n #add the district id to the dataframe

end_sf <- st_as_sf(trips, coords = c("end_x", "end_y"), crs = 31468) %>% #repeat the process for end point
  st_transform(USETHISCRS)

end_sf <- st_join(end_sf, districts)

trips$end_district_name <- end_sf$Gemeinde_n

trips <- trips %>% drop_na(start_district_name, #retain only trips within Berlin
                           end_district_name)


tvz_matrix <- trips %>% #build a start-end-matrix
  group_by(start_district_name, end_district_name) %>%
  summarize(count = n()) %>%
  ungroup()

tvz_matrix <- tvz_matrix %>% # computing sum of trips per district, mobility rate
  left_join(pop_data,
            by=c("start_district_name"="Gemeinde_n")) %>%
  select(start_district_name, end_district_name, count, pop) %>%
  group_by(start_district_name) %>%
  mutate(totalCount = sum(count),
         rate = sum(count) / pop)

tvz_matrix_joined <- tvz_matrix %>% #add the destination geometry
  left_join(districts, 
            by=c("end_district_name"="Gemeinde_n")) %>% 
  select(start_district_name, end_district_name, count, pop, totalCount, rate, geometry) %>% 
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

              


#### MODALSPLIT PLOT ####

plot_modalsplit <- plotModalSplitPieChart(trips)


#### WAITTIME PLOT ####

plot_waittime <- plotAverageTravelWait(trips)


#### SHINY UI ####
ui <- fluidPage(
  titlePanel("DataScience finalterm"),
  sidebarLayout(
    position = "right",
    sidebarPanel(plotOutput(outputId = "plot_modalsplit"),
                 verbatimTextOutput("info")),
    mainPanel(leafletOutput("map"),
              verbatimTextOutput("legend")))
  )

#### SHINY SERVER ####
server <- function(input, output) {
  
  output$map <- renderLeaflet(map) # render leaflet map
  
  output$plot_modalsplit <-  renderPlot({
  
    if(is_null(input$map_shape_click$id)) {
      
      plotModalSplitPieChart(trips)
    
      } else {
      
        plotModalSplitPieChart(trips %>% filter(start_district_name == input$map_shape_click$id))
    
        }
    })
  
  output$info <- renderText({
    paste0("selected district: ", input$map_shape_click$id)
  })

  observeEvent(input$map_shape_click, {  # observe polygon clicks
    
    polygon_name <- input$map_shape_click$id  # get the clicked polygon's id
    
    print(paste0("Origin polygon is: ", polygon_name))

    tvz_matrix_filtered <- tvz_matrix_joined[tvz_matrix_joined$start_district_name == polygon_name, ] # filter data for map based on polygon name
    
    tvz_matrix_filtered <- tvz_matrix_filtered %>% 
      mutate(labels = paste0(round(count), " Wege"))
    
    bins <- seq(min(tvz_matrix_filtered$count, tvz_matrix_filtered$count),    # create bins for colorpal
                max(tvz_matrix_filtered$count, tvz_matrix_filtered$count),
                by = (max(tvz_matrix_filtered$count, tvz_matrix_filtered$count) -
                        min(tvz_matrix_filtered$count, tvz_matrix_filtered$count)) / 7)
    
    pal <- colorBin("Purples",                                       # custom bin pal
                    domain = tvz_matrix_filtered$count,
                    bins = bins)
    
    palQ <- colorQuantile(palette = "YlOrRd",                          # quantile pal
                          domain = tvz_matrix_filtered$count[tvz_matrix_filtered$count != 0])
    
      leafletProxy("map") %>%                                         # update the app with the filtered data
      clearShapes() %>%                                             # remove existing selection
      addPolygons(data = districts,                                       # add polygons according to selection
                  layerId = ~Gemeinde_n,
                  color = "#000000",
                  weight = 2,
                  fillOpacity = 0,
                  noClip = T
      ) %>% 
      addPolygons(data = tvz_matrix_filtered,
                  layerId = ~end_district_name,
                  fillColor = ~palQ(count),
                  highlightOptions = highlightOptions(
                    color = "red",
                    bringToFront = T),
                  label = ~labels,
                  color = "black",
                  weight = 1,
                  fillOpacity = 0.5,
                  noClip = T
        )%>%
        addLegend(
        position = "bottomleft",
        values = tvz_matrix_filtered$count,
        title = "Legende Bruder",
        pal=palQ
        )
  })
}

#### RUN SHINY APP ####
shinyApp(ui, server)



