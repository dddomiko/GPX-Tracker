################################################################################
#
# GPX-Tracker
#
# author:      Dominik Koch
# version:     0.2
# created at:  05.01.2016
# last update: 31.01.2016
#
# sources:      
#   http://mhermans.net/hiking-gpx-r-leaflet.html
#   http://www.r-bloggers.com/stay-on-track-plotting-gps-tracks-with-r/
#
################################################################################

# TODO: Replace the shifting via lag function
#       add tableoverview of all tracks
#       add information avg_speed
#       add css
#       add elevation plot (interactive)
#       add speed plot (interactive)
#       Ziel: 1000 km Radln, xx km laufen ....
#       add break markers

library(shiny)
library(shinydashboard)

library(leaflet)       # interactive Javascript maps
library(lubridate)     # datetime operatings
library(ggplot2)       # general plotting
library(rgdal)         # importing GPX files
library(sp)            # spatial operations
library(dplyr)         # data munging operations
library(DT)

### Import data ----------------------------------------------------------------

idcounter <- 1

importGPX <- function(file){
  # Import the GPX file
  trackpoints <- readOGR(file, layer = "track_points", verbose = FALSE)
  track       <- readOGR(file, layer = "tracks",       verbose = FALSE)
  # Metadata
  name = track@data$name
  type = track@data$type
  date = min(as.Date(ymd_hms(trackpoints@data$time)))
  distance <- sum(spDists(trackpoints, segments = TRUE))
  # Get all elevations, times and coordinates
  elevations <- lowess(trackpoints@data$ele, f = 0.01)$y 
  times      <- ymd_hms(trackpoints@data$time)
  lat <- trackpoints@coords[,2]
  lon <- trackpoints@coords[,1]
  # Put everything in a dataframe and get rid of old variables
  geodf <- data.frame(id = idcounter,
                      lat = lat, 
                      lon = lon, 
                      ele = elevations, 
                      time = times)
  
  metadf <- data.frame(id = idcounter,
                       name = name,
                       date = date,
                       type = type,
                       dist = distance)
  
  return(list(geodf=geodf, metadf=metadf))
}

for(filename in dir("data")){
  if (!exists("GPX")){
    tmp <- importGPX(file = file.path("data",filename))
    GPX <- tmp$geodf
    GPX.meta <- tmp$metadf
    assign("idcounter", idcounter + 1 )
  } else {
    tmp <- importGPX(file = file.path("data",filename))
    GPX <- rbind(GPX,tmp$geodf)
    GPX.meta <- rbind(GPX.meta,tmp$metadf)
    assign("idcounter", idcounter + 1 )
  }
}

# Define maps Markers
# https://mapicons.mapsmarker.com
iconStart <- makeIcon("www/cycling.png", 
                      "www/cycling.png", 45, 45,
                      iconAnchorX = 22.5,
                      iconAnchorY = 45)

iconFinish <- makeIcon("www/finish.png", 
                       "www/finish.png", 45, 45,
                       iconAnchorX = 22.5,
                       iconAnchorY = 45)

iconRestaurant <- makeIcon("www/restaurant.png", 
                           "www/restaurant.png", 45, 45,
                           iconAnchorX = 22.5,
                           iconAnchorY = 45)

### ui -------------------------------------------------------------------------

ui <- dashboardPage(skin = "yellow",
  dashboardHeader(title = "GPX-Tracker"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("globe")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "map",
              fluidRow(
                box(width = 12, title = "Tolle Karte",
                    leafletOutput("gpx_map", height = 500)
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "dashboard",
              textOutput("tmp1")
              #print("input$tableId_rows_selected")
      ),
      # DELETE tab content
      tabItem(tabName = "widgets",
              dataTableOutput('DT')
      )
    )
  )
)

### server  --------------------------------------------------------------------

server <- function(input, output) { 
  

  ## Leaflet Map ---------------------------------------------------------------
  output$gpx_map <- renderLeaflet({
    
    # Subsetting data based on DataTable selection
    if(length(input$DT_rows_selected) > 0){
      GPX.meta.subset <- GPX.meta[GPX.meta$id %in% input$DT_rows_selected,]
      GPX.subset <- GPX[GPX$id %in% input$DT_rows_selected,]
    } else {
      GPX.meta.subset <- GPX.meta
      GPX.subset <- GPX
    }
    
    # Calculate Marker position
    if(length(input$DT_rows_selected) == 1){
      markers <- GPX.subset[c(1,nrow(GPX.subset)),]
    }
    
    # Generate one color per track
    track.colors <- colorFactor(rainbow(n=nrow(GPX.meta.subset)), GPX.meta.subset$id)
    
    # Map setup
    m <- leaflet() %>% addTiles()
    
    # Tracks
    for(i in GPX.meta.subset$id){
      m <- m %>% addPolylines(data = GPX.subset[GPX.subset$id == i,],
                              ~lon, ~lat, color = track.colors(i), group = "Tracks") 
    }
    
    # Layer controls
    m <- m %>%  addLayersControl(position = "bottomleft", 
                                 baseGroups = c("Road map", "Topographical", "Satellite", "Watercolor"), 
                                 overlayGroups = c("Tracks", "Markers"), 
                                 options = layersControlOptions(collapsed = TRUE))
    
    # Legend
#     m <- m %>% addLegend(position = "bottomright", 
#                          opacity = 0.4, 
#                          colors = c("blue","red"),
#                          labels = c("ourtward journey","return journey"),
#                          title = "Neubiberg - Langbuergner See")
    
    # Tiles
    m <- m %>% 
      addProviderTiles("OpenStreetMap.Mapnik",    group = "Road map") %>% 
      addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>% 
      addProviderTiles("Esri.WorldImagery",       group = "Satellite") %>% 
      addProviderTiles("Stamen.Watercolor",       group = "Watercolor")
    
    # Markers
    if(length(input$DT_rows_selected) == 1){
      m <- m %>%   
        addMarkers(data = markers[1,], 
                   lng = ~ markers$lon[1], 
                   lat = ~ markers$lat[1], 
                   popup = markers$time[1],
                   icon = iconStart, group = "Markers") %>%
        addMarkers(data = markers[2,], 
                   lng = ~ markers$lon[2], 
                   lat = ~ markers$lat[2], 
                   popup = markers$time[2],
                   icon = iconFinish, group = "Markers")
    }
    
    m
  })
  
  ## Data table ----------------------------------------------------------------
  output$DT = renderDataTable(
    GPX.meta, options = list(lengthChange = FALSE), rownames = FALSE,
    selection = 'single',
    server = FALSE
  )
  
  ## TMP - delete --------------------------------------------------------------
  output$tmp1 <- renderText({
    s <- input$DT_rows_selected
    paste(s)
  })
  
}


shinyApp(ui, server)

