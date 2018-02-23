#code to read FWC SAS files with water quality data



#Note from Caleb
#I have attached a file that shows the structure of our water quality data.  
#I also attached our physical data, since you will need to merge the two together by Reference.  
#The physical data will provide location (GPS and Zone B), date, and time.

install.packages("sas7bdat")

require(sas7bdat)
setwd("C:/Users/melimore86/Desktop/FWC")

hydro=read.sas7bdat("ckm_2017_hydrolab.sas7bdat")
physical=read.sas7bdat("ckm_2017_physical.sas7bdat")

require("ggplot2")
require("zoo")
require("lubridate")

lon<-(physical$Longitude)
lat<-(physical$Latitude)
dffwc <- as.data.frame(cbind(lon,lat))

physical$newDate <- as.POSIXct(as.Date(physical$date,origin= "1899-12-30"))

#Using `zoo` to add the date to the column
physical_zoo <- zoo(physical[, 2:5],order.by= physical$newDate)

require("sf")
require("ggplot2")

worldmap <- getMap(resolution = "high")

map1<- sf::st_as_sf(worldmap )


physical$date

ggplot(data=physical) +
  geom_point( aes(y= Salinity, x= newDate, fill= "Salinity"), color= "#999999") +
  geom_point(data= LC_WQ1, aes(x= newDate, y= Temperature, fill= "Temperature"), color= "#000000") +
  geom_point(data= dis, aes(x= newDate, y=Values/1000, fill= "River Discharge"), color="cornflowerblue", size=2, show.legend = TRUE, pch=15, alpha=0.4) +
  guides(fill= guide_legend(show= TRUE, title="", override.aes= list(colour= c("cornflowerblue", "#000000", "#999999"), size=10)))


require("leaflet")

leaflet() %>% addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>% addProviderTiles("CartoDB.Positron",group = "CartoDB Positron") %>% addProviderTiles("Esri.WorldImagery", group = "Esri WorldImagery")  %>% 
  addProviderTiles("OpenTopoMap", group = "OpenTopoMap") %>%  
  addPolygons(data = propreef,stroke = FALSE, fillOpacity = 0.9, smoothFactor = 0.5, group= "Proposed Reef", color="darkblue")%>%
  addPolygons(data = oysterbeds,stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, group= "2001 oyster distribution", color= "red")%>% addCircleMarkers(data=physical,lng=lon,lat=lat,label = ~as.character(Station_1), group="FWC Sites 1957")%>%
  setView(-83.08, 29.25, 1)
