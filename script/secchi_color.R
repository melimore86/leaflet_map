library(ggplot2)
library(scales)
library(lubridate)
library(grid)
library(gridExtra)
library(lattice)
library(colorspace)
library(plotly)
library(zoo)
library(ggthemes)
library(waterData)
library(hydroTSM)


labresults <- read.csv("data/2017_lakewatch_labresults.csv", header= TRUE)

colnames(labresults) <- c("County", "Name", "oldDate", "Month", "Day","Year","Station", "Phosphorus", 
                          "Nitrogen", "Chlorophyll", "Secchi", "Secchi 2", "Color", "SpecificConductancemicro", "SpecificConductancemilli")

labresults$Date <- as.POSIXct(as.Date(labresults$oldDate, origin= "1899-12-30"))

labresults$Date <- as.Date(labresults$oldDate, "%m/%d/%Y")
labresults$Conductivity<-NA

labresults$Conductivity<-paste(labresults$SpecificConductancemilli,labresults$SpecificConductancemicro)

labresults$Conductivity<-as.numeric(gsub('NA','',labresults$Conductivity))

labresults$Secchi<- (as.numeric(labresults$Secchi/3.28))


site1<-subset(labresults,Station=="1")
site2<-subset(labresults,Station=="2")
site3<-subset(labresults,Station=="3")
site4<-subset(labresults,Station=="4")
site5<-subset(labresults,Station=="5")
site6<-subset(labresults,Station=="6")

site1$Latitude<- (site1$Latitude= "29.26772")
site2$Latitude<- (site2$Latitude= "29.25742")
site3$Latitude<- (site3$Latitude= "29.23215")
site4$Latitude<- (site4$Latitude= "29.26645")
site5$Latitude<- (site5$Latitude= "29.24560")
site6$Latitude<- (site6$Latitude= "29.23104")

all_lab<- rbind(site1,site2,site3,site4,site5,site6)


Secchi <- data.frame('Secchi', all_lab$Station, all_lab$Secchi, all_lab$Date, all_lab$Latitude)

Color <- data.frame('Color', all_lab$Station, all_lab$Color, all_lab$Date, all_lab$Latitude)

colnames(Color) <- c('Variable','Station','Value','Date', 'Latitude')
colnames(Secchi) <- c('Variable','Station','Value','Date', 'Latitude')

Secchi$Date <- as.POSIXct(as.Date(Secchi$Date, origin= "1899-12-30"))
Color$Date <- as.POSIXct(as.Date(Color$Date, origin= "1899-12-30"))



#station to analyze
station = '02323500'   
#get site name to use in plot titles and such
stinfo  = siteInfo(station)

#read entire time series
dis   = importDVs(staid=station,code='00060',stat='00003', sdate= "1950-01-01")

#get some date components
dis$year    = as.numeric(strftime(dis$dates,format="%Y"))
dis$month   = as.numeric(strftime(dis$dates,format="%m")) 

colnames(dis) <- c("StaID", "Discharge", "newDate", "QualCode", "Year", "Month")


#Changing the format of the dates to be able to plot against time
dis$Date <- as.Date(dis$newDate,origin= "1899-12-30")


ggthemes_data$colorblind  <- ggthemes_data$colorblind
assignInNamespace("ggthemes_data", ggthemes_data, ns="ggthemes")

windows()

#secchi_lat<-
ggplot(data= Secchi, aes(x = Latitude, y= Value, color= Date, shape=factor(Station))) +
  labs(y="Secchi (meters)", x="Latitude", shape= "Site") +
  geom_jitter(size=6, stroke = 2, alpha=.9) +
  scale_color_gradient(low = "deepskyblue", high = "black",
                       trans = time_trans()) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(file="secchi_lat.png")

windows()
#sechi_date<-
ggplot(data= Secchi, aes(x = Date, y= Value) +
  labs(y="Secchi (meters)", x="Date", shape= "Site") +
  geom_jitter(size=5, stroke = 2, alpha=.8, shape= 16) +
  geom_jitter(data= Color, size=5, stroke = 2, alpha=.8, shape ) +
  #scale_colour_manual(name = "Site",
                      labels = c("1","2", "3", "4", "5", "6"),
                      values = c("#999999", "#E69F00", "#56B4E9", "#009E73","#0072B2", "#D55E00")) +   
  #scale_shape_manual(name = "Site",
                     #labels = c("1","2", "3", "4", "5", "6"),
                     #values = c(15, 16, 17, 18,19, 8)) +
  #scale_color_gradient(low = "deepskyblue", high = "black",
                       #trans = time_trans()) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(file="sechi_date.png")


#color_lat<-
  ggplot(data= Color, aes(x = Latitude, y= Value ,color= Date, shape=factor(Station))) +
  labs(y="Color (Pt-Co Units)", x="Latitude", shape= "Site") +
  geom_jitter(size=6, stroke = 2, alpha=.9) +
  scale_color_gradient(low = "deepskyblue", high = "black",
                       trans = time_trans()) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(file="color_lat.png")

#color_date<-
  ggplot(data= Color, aes(x = Date, y= Value,shape=factor(Station))) +
  labs(y="Color (Pt-Co Units)", x="Date", shape= "Site") +
  geom_jitter(size=6, stroke = 2, alpha=.7) +
  #scale_color_gradient(low = "deepskyblue", high = "black",
                       #trans = time_trans()) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(file="color_date.png")

