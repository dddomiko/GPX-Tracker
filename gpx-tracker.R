################################################################################
#
# GPX-Tracker
#
# author:      Dominik Koch
# version:     0.1
# created at:  05.01.2016
# last update: 10.01.2016
#
# sources:      
#   http://mhermans.net/hiking-gpx-r-leaflet.html
#   http://www.r-bloggers.com/stay-on-track-plotting-gps-tracks-with-r/
#
################################################################################

# TODO: Replace the shifting via lag function
#       add tableoverview of all tracks
#       select tracks via table
#       add information about total distance, avg_speed
#       add css
#       add elevation plot
#       add speed plot
#       ShinyDashboard

### Miscellaneous --------------------------------------------------------------

library(leaflet)       # interactive Javascript maps
#library(sp)            # spatial operations
library(lubridate)     # datetime operatings
library(ggplot2)       # general plotting
library(rgdal)         # importing GPX files

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
  # Get all elevations, times and coordinates
  elevations <- trackpoints@data$ele
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
                       type = type)
  
  return(list(geodf=geodf, metadf=metadf))
}

for(filename in dir("data")){
  if (!exists("GPX")){
    GPX <- importGPX(file = file.path("data",filename))$geodf
    GPX.meta <- importGPX(file = file.path("data",filename))$metadf
    idcounter <<- idcounter + 1
  } else {
    GPX <- rbind(GPX,importGPX(file = file.path("data",filename))$geodf)
    GPX.meta <- rbind(GPX.meta,importGPX(file = file.path("data",filename))$metadf)
    idcounter <<- idcounter + 1
  }
}

### Preprocess data ------------------------------------------------------------


# Calculate total distance
#kpi.distance <- spDists(trackpoints, segments = TRUE)

# # Calculate distance and speed between successive positions
# 
# shift.vec <- function(vec, shift) {
#   if (length(vec) <= abs(shift)) {
#     rep(NA, length(vec))
#   } else {
#     if (shift >= 0) {
#       c(rep(NA, shift), vec[1:(length(vec) - shift)])
#     } else {
#       c(vec[(abs(shift) + 1):length(vec)], rep(NA, abs(shift)))
#     }
#   }
# } 
# 
# # Shift vectors for lat and lon so that each row also contains the next position.
# geodf$lat.p1 <- shift.vec(geodf$lat, -1)
# geodf$lon.p1 <- shift.vec(geodf$lon, -1)
# 
# # Calculate distances (in metres) using the function pointDistance from the 'raster' package.
# # Parameter 'lonlat' has to be TRUE!
# geodf$dist.to.prev <- apply(geodf, 1, FUN = function(row) {
#   pointDistance(as.numeric(c(row["lon.p1"], row["lat.p1"])), 
#                 as.numeric(c(row["lon"], row["lat"])), 
#                 lonlat = TRUE)
# })
# 
# # Transform the column 'time' so that R knows how to interpret it.
# geodf$time <- strptime(geodf$time, format = "%Y-%m-%dT%H:%M:%OS")
# 
# # Shift the time vector, too.
# geodf$time.p1 <- shift.vec(geodf$time, -1)
# # Calculate the number of seconds between two positions.
# geodf$time.diff.to.prev <- as.numeric(difftime(geodf$time.p1, geodf$time))
# 
# # Calculate metres per seconds, kilometres per hour and two LOWESS smoothers to get rid of some
# # noise.
# geodf$speed.m.per.sec <- geodf$dist.to.prev/geodf$time.diff.to.prev
# geodf$speed.km.per.h <- geodf$speed.m.per.sec * 3.6
# geodf$speed.km.per.h <- ifelse(is.na(geodf$speed.km.per.h), 0, geodf$speed.km.per.h)
# geodf$lowess.speed <- lowess(geodf$speed.km.per.h, f = 0.01)$y
# geodf$lowess.ele <- lowess(geodf$ele, f = 0.01)$y 

### Descriptive Analysis -------------------------------------------------------

# p <- ggplot(as.data.frame(trackpoints), aes(x = time, y = ele))
# p + geom_point() + labs(x = "Time", y = "Elevations (meters)")
# 
# trackpoints$ele_new <- lowess(trackpoints$ele, f = 0.01)$y
# 
# p <- ggplot(data = as.data.frame(trackpoints), aes(x = time, y = ele_new))
# p + geom_point() + labs(x = "Time", y = "Elevations (meters)")
# 
# 
# ### Smooth data ----------------------------------------------------------------
# 
# # Plot elevations and smoother
# plot(geodf$ele, type = "l", bty = "n", xaxt = "n", ylab = "Elevation", xlab = "", col = "grey40")
# lines(geodf$lowess.ele, col = "red", lwd = 3)
# legend(x = "bottomright", legend = c("GPS elevation", "LOWESS elevation"), col = c("grey40", "red"), 
#        lwd = c(1, 3), bty = "n")
# 
# # Plot speeds and smoother
# plot(geodf$speed.km.per.h, type = "l", bty = "n", xaxt = "n", ylab = "Speed (km/h)", xlab = "", col = "grey40")
# lines(geodf$lowess.speed, col = "blue", lwd = 3)
# legend(x = "bottom", legend = c("GPS speed", "LOWESS speed"),
#        col = c("grey40", "blue"), lwd = c(1, 3), bty = "n")
# abline(h = mean(geodf$speed.km.per.h), lty = 2, col = "blue")
# 
# # Plot the track without any map, the shape of the track is already visible.
# plot(rev(geodf$lon), rev(geodf$lat), type = "l", col = "red", lwd = 3, bty = "n", ylab = "Latitude", 
#      xlab = "Longitude") 

### interactive track Plot -----------------------------------------------------

library(rgdal)
GPXfile <- "data\\Chiemsee_20150822.gpx"
track <- readOGR(GPXfile, layer = "tracks", verbose = FALSE)
leaflet() %>% addTiles() %>% addPolylines(data = track)

library(dplyr)

categories = LETTERS[1:(idcounter-1)]
RdYlBu = colorFactor("RdYlBu", domain = categories)
# m %>% addCircleMarkers(~lng, ~lat, radius = ~size,
#                        color = ~RdYlBu(category), fillOpacity = 0.5)
GPX$category <- LETTERS[GPX$id]

#GPX %>% group_by(id) %>% leaflet() %>% addTiles() %>% addPolylines(~lon, ~lat, color = ~RdYlBu(category))
#leaflet(GPX) %>% addTiles() %>% addCircleMarkers(~lon, ~lat, color = ~RdYlBu(category), radius = .1)



### ----------------------------------------------------------------------------

m <- leaflet() %>% addTiles()
m <- m %>% addPolylines(data = GPX[GPX$id == 1,],~lon, ~lat, color = "blue")
m <- m %>% addPolylines(data = GPX[GPX$id == 2,],~lon, ~lat, color = "red", group = "Photo markers")

### ----------------------------------------------------------------------------


leaflet() %>% addTiles() %>% 
  addPolylines(data = GPX[GPX$id == 1,],~lon, ~lat, color = "blue") %>% 
  addPolylines(data = GPX[GPX$id == 2,],~lon, ~lat, color = "red", group = "Photo markers") %>% 
  
  addLayersControl(position = "bottomleft", 
                   baseGroups = c("Road map", "Topographical", "Satellite", "Watercolor"), 
                   overlayGroups = c("Hiking routes", "Photo markers"), 
                   options = layersControlOptions(collapsed = FALSE))

markers <- trackpoints[c(1,nrow(trackpoints)),]


# https://mapicons.mapsmarker.com
iconStart <- makeIcon("images/cycling.png", 
                      "images/cycling.png", 45, 45,
                      iconAnchorX = 22.5,
                      iconAnchorY = 45)

iconFinish <- makeIcon("images/finish.png", 
                       "images/finish.png", 45, 45,
                       iconAnchorX = 22.5,
                       iconAnchorY = 45)

iconRestaurant <- makeIcon("images/restaurant.png", 
                           "images/restaurant.png", 45, 45,
                           iconAnchorX = 22.5,
                           iconAnchorY = 45)

m <- leaflet() %>% 
  # Add tiles
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>% 
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>% 
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>% 
  addProviderTiles("Stamen.Watercolor", group = "Watercolor") %>% 
  
  
  addLegend(position = "bottomright", 
            opacity = 0.4, 
            colors = c("blue","red"),
            labels = c("ourtward journey","return journey"),
            title = "Neubiberg - Langbuergner See") %>% 
  
  # Layers control
  addLayersControl(position = "bottomleft", 
                   baseGroups = c("Road map", "Topographical", "Satellite", "Watercolor"), 
                   overlayGroups = c("Hiking routes", "Photo markers"), 
                   options = layersControlOptions(collapsed = FALSE)) %>% 
  
  addPolylines(data = track, color = "blue", group = "Hiking routes") %>%
  addMarkers(data = markers[1,], 
             lng = ~ markers$coords.x1[1], 
             lat = ~ markers$coords.x2[1], 
             popup = markers$time[1],
             icon = iconStart, group = "Photo markers") %>%
  addMarkers(data = markers[2,], 
             lng = ~ markers$coords.x1[2], 
             lat = ~ markers$coords.x2[2], 
             popup = markers$time[2],
             icon = iconFinish, group = "Photo markers")

m


