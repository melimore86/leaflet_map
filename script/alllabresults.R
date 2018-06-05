#WATER QUALITY GRAPHS 2017 USING GGPLOT2

#required packages
library(ggplot2)
library(waterData)
library(hydroTSM)
library(scales)
library(lubridate)
library(plotly)

setwd("C:/Users/melimore86/Desktop")

labresults <- read.csv("cedarkeylabresults.csv", header= TRUE)

colnames(labresults) <- c("County", "Name", "oldDate", "Month", "Day","Year","Station", "Phosphorus", 
    "Nitrogen", "Chlorophyll", "Secchi", "Secchi 2", "Color", "SpecificConductancemicro", "SpecificConductancemilli")

labresults$Date <- as.POSIXct(as.Date(labresults$oldDate, origin= "1899-12-30"))
labresults$Conductivity<-NA

labresults$Conductivity<-paste(labresults$SpecificConductancemilli,labresults$SpecificConductancemicro)

labresults$Conductivity<-as.numeric(gsub('NA','',labresults$Conductivity))


Phosphorus <- data.frame('Phosphorus', labresults$Station, labresults$Phosphorus, labresults$Date)
Nitrogen <- data.frame('Nitrogen', labresults$Station, labresults$Nitrogen, labresults$Date)  
Chlorophyll <- data.frame('Chlorophyll', labresults$Station, labresults$Chlorophyll, labresults$Date)  
Color <- data.frame('Color', labresults$Station, labresults$Color, labresults$Date)
Conductivity <- data.frame('Conductivity', labresults$Station, labresults$Conductivity, labresults$Date)


colnames(Phosphorus)  <- c('Variable','Station','Value','Date')
colnames(Nitrogen)  <- c('Variable','Station','Value','Date')
colnames(Chlorophyll) <- c('Variable','Station','Value','Date')
colnames(Color) <- c('Variable','Station','Value','Date')
colnames(Conductivity) <- c('Variable','Station','Value','Date')


Phosphorus$Date <- as.POSIXct(as.Date(Phosphorus$Date, origin= "1899-12-30"))
Nitrogen$Date <- as.POSIXct(as.Date(Nitrogen$Date, origin= "1899-12-30"))
Chlorophyll$Date <- as.POSIXct(as.Date(Chlorophyll$Date, origin= "1899-12-30"))
Color$Date <- as.POSIXct(as.Date(Color$Date, origin= "1899-12-30"))
Conductivity$Date <- as.POSIXct(as.Date(Conductivity$Date, origin= "1899-12-30"))

Phosphorus$Station<-paste("Site",Phosphorus$Station )
Nitrogen$Station<-paste("Site",Nitrogen$Station )
Chlorophyll$Station<-paste("Site",Chlorophyll$Station )
Color$Station<-paste("Site",Color$Station )
Conductivity$Station<-paste("Site",Conductivity$Station )


###RIVER DISCHARGE
#now create the river discharge dataframe which will ultimately be graphed in the background of 
#the variable being graphed

#station to analyze
station = '02323500'   
#get site name to use in plot titles and such
stinfo  = siteInfo(station)

#read entire time series
dis   = importDVs(staid=station,code='00060',stat='00003', sdate= "1950-01-01")

#get some date components
dis$year    = as.numeric(strftime(dis$dates,format="%Y"))
dis$month   = as.numeric(strftime(dis$dates,format="%m")) 

#Naming columns, using the Diver sensors, collects date, pressure, temp, conductivity
colnames(dis) <- c("StaID", "Discharge", "oldDate", "QualCode", "Year", "Month")

#Changing the format of the dates to be able to plot against time
dis$Date <- as.POSIXct(as.Date(dis$oldDate,origin= "1899-12-30"))


####PLOTTING
phosall<-ggplot(data = Phosphorus, aes(x = Date)) +
  labs(title ="Phosphorous Measurements", y="Phosphorus (µg/L)& Discharge/160", size = 12) +
  geom_line(data= dis, aes(y=Discharge/160) , color= "cornflowerblue", size=1.5)  +
  geom_point(aes(y = Value),size=1.5, color = "black") +
  scale_y_continuous(limits=c(0,125)) +
  scale_x_datetime(
    breaks = date_breaks("month") ,
    labels = date_format("%m/%y"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2016-12-15"),
      as.POSIXct("2017-12-30"))) +
  theme(axis.text=element_text(size=08),axis.title=element_text(size=08,face="bold"),
        plot.title =element_text(size=08, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, linetype="solid")) +
  facet_wrap(~ Station, ncol = 2, nrow = 3)
  
phosall<-ggplotly(phosall)


#############
#Nitrogen

nitroall<-ggplot(data = Nitrogen, aes(x = Date)) +
  labs(title ="Nitrogen Measurements", y="Nitrogen (µg/L) & Discharge/13", size = 12) +
  geom_line(data= dis, aes(y=Discharge/13) , color= "cornflowerblue", size=1.5)  +
  geom_point(aes(y = Value),size=1.5, color = "black") +
  scale_y_continuous(limits=c(0,1500)) +
  scale_x_datetime(
    breaks = date_breaks("month") ,
    labels = date_format("%m/%y"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2016-12-15"),
      as.POSIXct("2017-12-30"))) +
  theme(axis.text=element_text(size=07),axis.title=element_text(size=07,face="bold"),
        plot.title =element_text(size=08, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, linetype="solid")) +
  facet_wrap(~ Station, ncol = 2, nrow = 3)

nitroall<-ggplotly(nitroall)

###########
#Chlorophyll

chloroall<-ggplot(data = Chlorophyll, aes(x = Date)) +
  labs(title ="Chlorophyll Measurements", y="Chlorophyll (µg/L) & Discharge/13", size = 12) +
  geom_line(data= dis, aes(y=Discharge/250) , color= "cornflowerblue", size=1.5)  +
  geom_point(aes(y = Value),size=1.5, color = "black") +
  scale_y_continuous(limits=c(0,80)) +
  scale_x_datetime(
    breaks = date_breaks("month") ,
    labels = date_format("%m/%y"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2016-12-15"),
      as.POSIXct("2017-12-30"))) +
  theme(axis.text=element_text(size=07),axis.title=element_text(size=07,face="bold"),
        plot.title =element_text(size=08, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, linetype="solid")) +
  facet_wrap(~ Station, ncol = 2, nrow = 3)

chloroall<-ggplotly(chloroall)

############
#Color

colorall<-ggplot(data = Color, aes(x = Date)) +
  labs(title ="Color Measurements", y="Color (Pt-Co Units) & Discharge/160", size = 12) +
  geom_line(data= dis, aes(y=Discharge/160) , color= "cornflowerblue", size=1.5)  +
  geom_point(aes(y = Value),size=1.5, color = "black") +
  scale_y_continuous(limits=c(0,125)) +
  scale_x_datetime(
    breaks = date_breaks("month") ,
    labels = date_format("%m/%y"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2016-12-15"),
      as.POSIXct("2017-12-30"))) +
  theme(axis.text=element_text(size=07),axis.title=element_text(size=07,face="bold"),
        plot.title =element_text(size=08, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, linetype="solid")) +
  facet_wrap(~ Station, ncol = 2, nrow = 3)

colorall<-ggplotly(colorall)




#######
#Conductivity

condall<-ggplot(data = Conductivity, aes(x = Date)) +
  labs(title ="Conductivity Measurements", y="Conductivity mS/cm @ 25° C) & Discharge/500", size = 12) +
  geom_line(data= dis, aes(y=Discharge/500) , color= "cornflowerblue", size=1.5)  +
  geom_point(aes(y = Value),size=1.5, color = "black") +
  scale_y_continuous(limits=c(0,40)) +
  scale_x_datetime(
    breaks = date_breaks("month") ,
    labels = date_format("%m/%y"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2016-12-15"),
      as.POSIXct("2017-12-30"))) +
  theme(axis.text=element_text(size=07),axis.title=element_text(size=07,face="bold"),
        plot.title =element_text(size=08, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, linetype="solid")) +
  facet_wrap(~ Station, ncol = 2, nrow = 3)

condall<-ggplotly(condall)
