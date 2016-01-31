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
    
    if(length(input$DT_rows_selected)>0){
      GPX.meta.subset <- GPX.meta[GPX.meta$id %in% input$DT_rows_selected,]
      GPX.subset <- GPX[GPX$id %in% input$DT_rows_selected,]
    } else {
      GPX.meta.subset <- GPX.meta
      GPX.subset <- GPX
    }
    
    
    track.colors <- colorFactor(rainbow(n=nrow(GPX.meta.subset)), GPX.meta.subset$id)
    
    m <- leaflet() %>% addTiles()
    for(i in GPX.meta.subset$id){
      m <- m %>% addPolylines(data = GPX.subset[GPX.subset$id == i,],~lon, ~lat, color = track.colors(i)) 
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

