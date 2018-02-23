
require(sas7bdat)
setwd("C:/Users/melimore86/Desktop/FWC")

#hydro2017=read.sas7bdat("ckm_2017_hydrolab.sas7bdat")
#physical2017=read.sas7bdat("ckm_2017_physical.sas7bdat")
hydro=read.sas7bdat("ckm_hydrolab.sas7bdat")
physical=read.sas7bdat("ckm_physical.sas7bdat")

write.csv(hydro, "hydro.csv")

write.csv(physical, "physical.csv")


require(dplyr)
#
hydrophysical2017 <- left_join(hydro2017, physical2017, by = c('Reference'))
hydrophysical <- left_join(hydro, physical, by = c('Reference'))
hydrophysicalall <- full_join(hydrophysical, hydrophysical2017, by = c('Reference'))


require(reshape)
hydrophysicalall$Reference2 <- substr(hydrophysicalall$Reference, start=4, stop=11)

hydrophysicalall$NEWDATE2 <- as.POSIXct(as.Date(hydrophysicalall$NEWDATE,origin= "1899-12-30"))
hydrophysicalall$NEWDATE <- lubridate::ymd(hydrophysicalall$Reference2)

#NEWDATE is lubridate time
#NEWDATAE2 is POSIXct

library(dplyr)
library(lubridate)

aggregate(cbind(count = Reference)~year(NEWDATE) +~ month(NEWDATE),
          data = hydrophysicalall , 
          FUN = function(x){NROW(x)})

hydrophysicalall_dt<-hydrophysicalall%>%
  count(month=floor_date(NEWDATE, "month"))
print(hydrophysicalall_dt)

write.csv(hydrophysicalall_dt, "hydrophysicalall_dt.csv")


#hydrosaldatessal<-
aggregate(Reference ~ month(NEWDATE) + year(NEWDATE)+ Salinity.x,dat= hydrophysicalall, FUN =length)
write.csv(hydrosaldatessal, "hydrosaldatessal.csv")

lon<-(hydrophysicalall$Longitude.x)
lat<-(hydrophysicalall$Latitude.x)
dffwc <- as.data.frame(cbind(lon,lat,hydrophysicalall$NEWDATE))

require(ggplot2)
require(ggthemes)
library("scales")
require(rworldmap)
require(rworldxtra)

worldmap <- getMap(resolution = "high")

map1<- sf::st_as_sf(worldmap )


ggplot(data=hydrophysicalall, aes(x=NEWDATE))+
  
  geom_sf(data=map1, fill="antiquewhite1") +
  
  coord_sf(xlim = c(-83.20, -83.0), ylim = c(29.20, 29.40), expand=TRUE ) +
  
  geom_point(data= dffwc, aes(x = lon, y = lat, fill = "darkred"), size = 3.2, shape = 21, inherit.aes = TRUE) +
  
  guides(fill=FALSE, alpha=FALSE, size=FALSE) +
  
  xlab("Longitude")+ ylab("Latitude") +
  
  scale_x_datetime(
    breaks = date_breaks("week") ,
    labels = date_format("%m/%d"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2016-06-01"),
      as.POSIXct("2016-06-15")))+
  
  theme_classic() + 
  
  theme(legend.position = "none", panel.grid.major = element_line(colour = gray(.5), linetype = "dashed", size = 0.5),panel.background = element_rect(fill = "aliceblue"), panel.border=element_rect(fill=NA))



ggplot(data=hydrophysicalall, aes( x=NEWDATE2))+ 
  geom_point(aes(y= Reference), color= "#000000")+
  labs(x= "Date", y= "Salinity") +
  scale_x_datetime(
    breaks = date_breaks("week") ,
    labels = date_format("%m/%d"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2016-06-01"),
      as.POSIXct("2016-06-15")))+
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70) 


hydrophysicalall_dt%>%
  mutate(monthly=wday(NEWDATE, label=TRUE))

library(tidyverse)
hydrophysicalall %>% 
  mutate(Date = dmy_hms(NEWDATE)) %>% 
  count(Date1 = as.Date(Date), Month = month(Date)) %>%
  group_by(Date1)
  arrange(Date1, Month)


