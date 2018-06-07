library("maptools")
library("leaflet")
library("plotKML")
library("sf")
library("sp")
library("rgdal")
library("leaflet.extras")
library("dplyr")
library("tidyr")


#http://gsif.isric.org/doku.php/wiki:soil_webmaps
#https://seethedatablog.wordpress.com/2016/11/01/r-extract-coordinates-from-kmz-files/
#https://stackoverflow.com/questions/7907217/creating-x-and-y-distance-coordinates-for-r-from-a-kml-file


coords = getKMLcoordinates('markers.kml')
coords2<- as.data.frame(coords)


x <- c(coords2$X1,coords2$X1.1,coords2$X1.2,coords2$X1.3,coords2$X1.4,coords2$X1.5,coords2$X1.6)
y <-c (coords2$X2,coords2$X2.1,coords2$X2.2,coords2$X2.3,coords2$X2.4,coords2$X2.5,coords2$X2.6)

df<- cbind (x,y)
colnames(df)<- c("X", "Y")

PRO <- sp::CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
df2 <- SpatialPoints(df, CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'))




leaflet() %>%setView(-83.10, 29.25, zoom=15) %>% addProviderTiles("Esri.WorldImagery", group = "Esri WorldImagery")  %>%
  addMarkers(df2, lng= df2$X, lat= df2$Y)
    #markerType = 'circleMarker',
    #stroke=FALSE, fillColor='black'
    
  
head(coords)
class(coords3)
