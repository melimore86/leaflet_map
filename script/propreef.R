---
  output:
  html_document:
  fig_width: 10
fig_height: 8
---
  
  ```{r r_packages, include=FALSE}
library(leaflet)
library(htmlwidgets)
```


```{r r_uploads,include= FALSE}
library(rgdal)

propreef <- readOGR(dsn=path.expand("T:/Oyster Project/mels_git/mapping/lc_project.gdb"), layer = "lc_reef_elements")

oysterbeds1982<- readOGR(dsn= path.expand("T:/Oyster Project/mels_git/mapping/shapefile"), layer="LC_1982")

oysterbeds1995<- readOGR(dsn= path.expand("T:/Oyster Project/mels_git/mapping/shapefile"), layer="LC_1995")

oysterbeds2001<- readOGR(dsn= path.expand("T:/Oyster Project/mels_git/mapping/shapefile"), layer="LC_2001")

oysterbeds2010<- readOGR(dsn= path.expand("T:/Oyster Project/mels_git/mapping/shapefile"), layer="LC_2010")

PRO <- sp::CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')

propreef2<- sp::spTransform(propreef,PRO)
oysterbeds1982p <- sp::spTransform(oysterbeds1982,PRO)
oysterbeds1995p <- sp::spTransform(oysterbeds1995,PRO)
oysterbeds2001p <- sp::spTransform(oysterbeds2001,PRO)
oysterbeds2010p <- sp::spTransform(oysterbeds2010,PRO)



```
```{r oyster_icon, include=FALSE}

divericon <- makeIcon(
  iconUrl = "T:/Oyster Project/mels_git/mapping/icon/diver3.png",
  iconWidth = 12, iconHeight = 65)

staricon <- makeIcon(
  iconUrl = "T:/Oyster Project/mels_git/mapping/icon/star.png",
  iconWidth = 12, iconHeight = 50)

```

```{r r_map, include=FALSE}
intermap<-leaflet() %>%  addProviderTiles("OpenStreetMap.HOT", group = "OpenStreetMap.HOT") %>% addProviderTiles("Esri.NatGeoWorldMap", group = "Esri.NatGeoWorldMap") %>%
  addProviderTiles("Esri.WorldImagery", group = "Esri WorldImagery")  %>% 
  #addProviderTiles("CartoDB.Positron",group = "CartoDB Positron")%>%  
  addPolygons(data = oysterbeds1982p,stroke = FALSE, fillOpacity = .8, smoothFactor = 0.5, group= "1982 oyster distribution", color= "#33FF00")%>%
  addPolygons(data = oysterbeds1995p,stroke = FALSE, fillOpacity = 0.8, smoothFactor = 0.5, group= "1995 oyster distribution", color= "#3399FF")%>%
  addPolygons(data = oysterbeds2001p,stroke = FALSE, fillOpacity = 0.6, smoothFactor = 0.5, group= "2001 oyster distribution", color= "#9900FF")%>%
  addPolygons(data = oysterbeds2010p,stroke = FALSE, fillOpacity = 0.6, smoothFactor = 0.5, group= "2010 oyster distribution", color= "#FF33CC")%>%
  addPolygons(data = propreef2,stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5, group= "Proposed Reef", color="black", label = paste("Element ID ", propreef2$element_id),
              labelOptions = labelOptions(noHide = F, direction= "top"))%>%
  #addCircleMarkers(data=physical2017,lng=lon,lat=lat,label = ~as.character(Station_1), group="FWC 2017 Sites",color="#009E73")%>%
  setView(-83.10, 29.25, 14) %>%
  addMarkers(
    lng = -83.115749973803759, lat = 29.266459979116917,
    label = "Site 1, Serial # V5602",
    labelOptions = labelOptions(noHide = F, direction= "right"),
    icon=divericon,
    popup='<a href="http://rpubs.com/oysterproject/site1measurements/">Sensor Measurements</a>') %>%
  addMarkers(
    lng = -83.095912020653486, lat = 29.24560303799808,
    label = "Site 2, Serial # S9059",
    labelOptions = labelOptions(noHide = F, direction= "right"),
    icon=staricon,
    popup='<a href="http://rpubs.com/oysterproject/site2measurements/">Sensor Measurements</a>') %>%
  addMarkers(
    lng = -83.090120041742921, lat = 29.231049958616495,
    label = "Site 3, Serial # V6916",
    labelOptions = labelOptions(noHide = F, direction= "bottom"),
    icon=divericon,
    popup='<a href="http://rpubs.com/oysterproject/site3measurements/">Sensor Measurements</a>') %>%
  addMarkers(
    lng = -83.092115018516779, lat = 29.230171032249928,
    label = "Site 4, Serial # S9058",
    labelOptions = labelOptions(noHide = F, direction= "left"),
    icon=staricon,
    popup='<a href="http://rpubs.com/oysterproject/site4measurements/">Sensor Measurements</a>') %>%
  addMarkers(
    lng = -83.101499984040856, lat = 29.246092038229108,
    label = "Site 5, Serial # S9060",
    labelOptions = labelOptions(noHide = F, direction= "left"),
    icon=staricon,
    popup='<a href="http://rpubs.com/oysterproject/site5measurements/">Sensor Measurements</a>') %>%
  addMarkers(
    lng = -83.118119034916162, lat = 29.265770986676216,
    label = "Site 6, Serial # S9061",
    labelOptions = labelOptions(noHide = F, direction= "left"),
    icon=staricon,
    popup='<a href="http://rpubs.com/oysterproject/site6measurements/">Sensor Measurements</a>')%>%
  addMarkers(
    lng = -83.098221989348531, lat = 29.267726987600327,
    label = "Site 7, Serial # S9035 ",
    labelOptions = labelOptions(noHide = F, direction= "top"),
    icon=staricon,
    popup='<a href="http://rpubs.com/oysterproject/site7measurements/">Sensor Measurements</a>') %>%
  addMarkers(
    lng = -83.080270970240235, lat = 29.257425041869283,
    label = "Site 8, Serial # S9062",
    labelOptions = labelOptions(noHide = F, direction= "top"),
    icon=staricon,
    popup='<a href="http://rpubs.com/oysterproject/site8measurements/">Sensor Measurements</a>') %>%
  addMarkers(
    lng = -83.082710020244122, lat = 29.232152011245489,
    label = "Site 9, Serial # S9036",
    labelOptions = labelOptions(noHide = F, direction= "right"),
    icon=staricon,
    popup='<a href="http://rpubs.com/oysterproject/site9measurements/">Sensor Measurements</a>') %>%
  addProviderTiles("OpenWeatherMap.Clouds", group = "clouds2", options = tileOptions(opacity = 0.6)) %>%
  addTiles(urlTemplate = "http://{s}.tile.openweathermap.org/map/temp/{z}/{x}/{y}.png", attribution = "Weather data © OpenWeatherMap", group = "temp", options = tileOptions(opacity = 0.6)) %>% 
  addTiles(urlTemplate = "http://{s}.tile.openweathermap.org/map/precipitation/{z}/{x}/{y}.png", attribution = "Weather data © OpenWeatherMap", group = "precipitation", 
           options = tileOptions(opacity = 0.6)) %>% addTiles(urlTemplate = "http://{s}.tile.openweathermap.org/map/snow/{z}/{x}/{y}.png", attribution = "Weather data © OpenWeatherMap", group = "snow", options = tileOptions(opacity = 0.6)) %>% 
  addTiles(urlTemplate = "http://{s}.tile.openweathermap.org/map/wind/{z}/{x}/{y}.png", 
           attribution = "Weather data © OpenWeatherMap", group = "wind", 
           options = tileOptions(opacity = 0.6)) %>% 
  addLayersControl(baseGroups = c("OpenStreetMap.HOT","Esri.NatGeoWorldMap","Esri WorldImagery"), overlayGroups = c("1982 oyster distribution","1995 oyster distribution","2001 oyster distribution","2010 oyster distribution","Proposed Reef"), options = layersControlOptions(collapsed = TRUE)) %>% 
  hideGroup(c("2010 oyster distribution","2001 oyster distribution","1995 oyster distribution","1982 oyster distribution","Proposed Reef"))

```

```{r intermap, echo=FALSE}
intermap
saveWidget(intermap, file="intermap.html")
```



