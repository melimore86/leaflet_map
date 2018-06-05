library(rnoaa)
library(plyr)
library(ncdf4)
library(lubridate)
library(scales)
library(ggplot2)
library(tidyr)

#ncdc_stations(datasetid='CDRF1', locationid='CDRF1', stationid='CDRF1')
#buoys(dataset='cwind')
#wind<-buoy(dataset='cwind',buoyid='CDRF1',  year=2017, datatype='c')




LC_WQ1 <- read.csv("data/LC_WQ1_All_Days_R.csv", header= T)

#Naming columns, using the Diver sensors, collects date, pressure, temp, conductivity
colnames(LC_WQ1) <- c("oldDate", "Pressure", "Temperature", "Conductivity")
head(LC_WQ1)

#Changing the format of the dates to be able to plot against time
LC_WQ1$Date <- as.POSIXct(as.Date(LC_WQ1$oldDate,origin= "1899-12-30"))
LC_WQ1$Station<- "1"


LC_WQ3 <- read.csv("data/LC_WQ3_All_Days_R.csv", header= T)

#Naming columns, using the Diver sensors, collects date, pressure, temp, conductivity
colnames(LC_WQ3) <- c("oldDate", "Pressure", "Temperature", "Conductivity")
head(LC_WQ3)

#Changing the format of the dates to be able to plot against time
LC_WQ3$Date <- as.POSIXct(as.Date(LC_WQ3$oldDate,origin= "1899-12-30"))
LC_WQ3$Station<- "3"

#The timestamp is off approximately for 4 hours and 3 minutes, adding that time to match Diver, will check other sensors for this issue as well, adding 4 hours 
#LC_WQ1$Date<- LC_WQ1$Date + 4*60*60

#Getting the barometric pressure, it's in the co-ops search 

aug17wind<-coops_search(begin_date = 20170801, end_date = 20170831, station_name = 8727520,
              product= "air_pressure")
aug17wind<- data.frame(aug17wind$data)

sept17wind<-coops_search(begin_date = 20170901, end_date = 20170930, station_name = 8727520,
                        product= "air_pressure")
sept17wind<- data.frame(sept17wind$data)

oct17wind<-coops_search(begin_date = 20171001, end_date = 20171031, station_name = 8727520,
                         product= "air_pressure")
oct17wind<- data.frame(oct17wind$data)

nov17wind<-coops_search(begin_date = 20171101, end_date = 20171130, station_name = 8727520,
                        product= "air_pressure")
nov17wind<- data.frame(nov17wind$data)

dec17wind<-coops_search(begin_date = 20171201, end_date = 20171231, station_name = 8727520,
                        product= "air_pressure")
dec17wind<- data.frame(dec17wind$data)

jan18wind<-coops_search(begin_date = 20180101, end_date = 20180131, station_name = 8727520,
                        product= "air_pressure")
jan18wind<- data.frame(jan18wind$data)

feb18wind<-coops_search(begin_date = 20180201, end_date = 20180228, station_name = 8727520,
                        product= "air_pressure")
feb18wind<- data.frame(feb18wind$data)

mar18wind<-coops_search(begin_date = 20180201, end_date = 20180228, station_name = 8727520,
                        product= "air_pressure")
mar18wind<- data.frame(mar18wind$data)


aug17_mar18<- rbind(aug17wind,sept17wind,oct17wind,nov17wind,dec17wind,jan18wind,feb18wind, mar18wind)


colnames(aug17_mar18) <- c("oldDate", "Pressure", "F")


aug17_mar18$Date <- as.POSIXct(aug17_mar18$oldDate,origin= "1899-12-30",format="%Y%m%d%H%M%S")

aug17_mar18$Air<- (aug17_mar18$Pressure * 1.01972)

aug17_mar18$Station<- "R"

diver_data<- rbind (LC_WQ1, LC_WQ3)


diver_rnoaa <- merge(aug17_mar18,diver_data,by=c("Date")) 

diver_rnoaa$WC<- (9806.65*((diver_rnoaa$Pressure.y - diver_rnoaa$Air)/(9.81*1000)))

diver_rnoaa$WC_m<- (diver_rnoaa$WC/100)

## Getting water_levels from rnoaa package

aug17wl<-coops_search(begin_date = 20170801, end_date = 20170831, station_name = 8727520,
                      product= "water_level",datum="STND")
aug17wl<- data.frame(aug17wl$data)

sept17wl<-coops_search(begin_date = 20170901, end_date = 20170930, station_name = 8727520,
                       product= "water_level",datum="STND")
sept17wl<- data.frame(sept17wl$data)

oct17wl<-coops_search(begin_date = 20171001, end_date = 20171031, station_name = 8727520,
                      product= "water_level",datum="STND")
oct17wl<- data.frame(oct17wl$data)

nov17wl<-coops_search(begin_date = 20171101, end_date = 20171130, station_name = 8727520,
                      product= "water_level",datum="STND")
nov17wl<- data.frame(nov17wl$data)

dec17wl<-coops_search(begin_date = 20171201, end_date = 20171231, station_name = 8727520,
                      product= "water_level",datum="STND")
dec17wl<- data.frame(dec17wl$data)

jan18wl<-coops_search(begin_date = 20180101, end_date = 20180131, station_name = 8727520,
                      product= "water_level",datum="STND")
jan18wl<- data.frame(jan18wl$data)

feb18wl<-coops_search(begin_date = 20180201, end_date = 20180228, station_name = 8727520,
                      product= "water_level",datum="STND")
feb18wl<- data.frame(feb18wl$data)

mar18wl<-coops_search(begin_date = 20180201, end_date = 20180228, station_name = 8727520,
                      product= "water_level",datum="STND")
mar18wl<- data.frame(mar18wl$data)

aug17wl_mar18wl<- rbind(aug17wl,sept17wl,oct17wl,nov17wl,dec17wl,jan18wl,feb18wl, mar18wl)

  
colnames(aug17wl_mar18wl) <- c("oldDate", "Water_Level", "S", "F", "Q")


aug17wl_mar18wl$Date <- as.POSIXct(aug17wl_mar18wl$oldDate,origin= "1899-12-30",format="%Y%m%d%H%M%S")



aug17wl_mar18wl$Month <- format(aug17wl_mar18wl$Date, '%Y/%m')
diver_rnoaa$Month<-format(diver_rnoaa$Date, '%Y/%m')


windows()

depth_tide<-
ggplot(data=diver_rnoaa, aes( x= Date, y=WC_m)) +
  labs(x="Date", y="Water Depth (meters)", color="") +
  geom_line(data= aug17wl_mar18wl, aes(x= Date, y=Water_Level/3, color= "Tide Level"), fill= "cornflowerblue", alpha=0.6, size= 1.3) +
  scale_y_continuous(sec.axis = sec_axis(~.*3, name = "Tide Level (meters)"), limits=c(-0.5,2.5)) +
  geom_line(size=1.1, aes(color=factor(Station.y))) +
  scale_color_manual(values=c("#0072B2","#E69F00","cornflowerblue" )) +
  scale_x_datetime(
  breaks = date_breaks("1 week") ,
   labels = date_format("%m/%d"),
  #labels = date_format("%m/%d-%H:%M"),
    expand = c(0, 0)) +
  #limits = c(
    #as.POSIXct("2017-09-08"),
    #as.POSIXct("2017-09-17"))) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.title=element_text(size=12,face="bold"),
        axis.text.x = element_text(size=8,angle = 90)) + 
  facet_wrap (~Month, scales = 'free_x', ncol = 2)

ggsave(file="depth_tide.png", dpi= 300,width = 10, height = 10)


