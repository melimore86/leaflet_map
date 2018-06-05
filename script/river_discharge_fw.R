library(waterData)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(scales)
library(dplyr)

#station to analyze
station = '02323500'   

#get site name to use in plot titles and such
stinfo  = siteInfo(station)

#read entire time series
dis   = importDVs(staid=station,code='00060',stat='00003', sdate= "1950-01-01") 

dis$year    = as.numeric(strftime(dis$dates,format="%Y"))
#dis$month   = as.numeric(strftime(dis$dates,format="%m")) 

#Naming columns, using the Diver sensors, collects date, pressure, temp, conductivity
colnames(dis) <- c("StaID", "Discharge", "oldDate", "QualCode", "Year")


#Changing the format of the dates to be able to plot against time
dis$Date <- as.Date(dis$oldDate)

dis$Month <- month(dis$Date, label=TRUE)
dis$Month2 <- month(dis$Date, label=FALSE)

summary(dis$Discharge)

#Boxplot of every year per month
#river<-
  ggplot(data=dis, aes(x= Month, y=Discharge/1000)) + 
  geom_boxplot(position = 'identity') +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x="Month", y="River Discharge (1,000 cfs)") +
  facet_wrap(~Year, ncol=8) 
ggsave('river.png', height = 12, width = 15, dpi=300)

  
#Linear river discharge  
  
  river_linear<- 
    ggplot(data=dis, aes(x= Date, y=Discharge/1000)) + 
    geom_line() + 
      scale_x_date(
        breaks = date_breaks("1 month") ,
        labels = date_format("%b")) +
      facet_wrap(~year(Date), ncol=8, scale = "free_x") +
    theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x="Month", y="River Discharge (1,000 cfs)") 
  ggsave('river_linear.png', height = 12, width = 15, dpi=300)
  
  
  class(dis$Date)
  
  
dis$Date2 <- as.POSIXct(as.Date(dis$Date, origin= "1899-12-30"))
  
  
 #Average per year and then line per year 
#river_average<-
  ggplot(data=dis, aes(x= Date, y=Discharge)) + 
  geom_line(color= "black") +
  stat_summary(fun.y ="mean", geom="line", color= "red", size= 3, alpha= 0.6) +
  #facet_wrap(~year(Date), ncol=8, scale = "free_x") +
  scale_x_date(
    breaks = date_breaks("1 month") ,
    labels = date_format("%b"),
    limits = c(
      as.Date("1950-01-01"),
      as.Date("1950-12-31"))) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),axis.text.x = element_text(angle = 90, hjust = 1))
 
  #Stacking in ggplot
  
  
  ggplot(data=dis, aes(x= Date, y=Discharge, color=Year)) + 
    stat_summary(fun.y ="mean", geom="line", position= "stack",color= "red", size= 3, alpha= 0.6) 
  
  
  
  
  
  #Average by monthly mean
  #station to analyze
  station = '02323500'   
  
  #get site name to use in plot titles and such
  stinfo  = siteInfo(station)
  
  
  dis   = importDVs(staid=station,code='00060',stat='00003', sdate= "1950-01-01") 
  #ok looks like temp code is wrong for this station, maybe rainfall is available?
  
  #get some date components
  dis$year    = as.numeric(strftime(dis$dates,format="%Y"))
  dis$month   = as.numeric(strftime(dis$dates,format="%m")) 
  
  #make dataset from epochs, 
  disE  = dis[dis$dates>='1950-01-01' & dis$dates<='2017-10-01',]  
  
  #get monthly sum, mean, sd, and var
  #discharge
  disE.mo  = aggregate(val~month+year,data=disE,FUN = function(x) c(mean(x,na.rm=T),sd(x,na.rm=T),var(x,na.rm=T),sum(x)))



library(dplyr)
dis %>% group_by(dis$Month) %>% summarise(mean(Discharge))

library("xts")
# Create xts object
na.omit(dis)
dis.xts <- as.xts(dis[,8], order.by=as.Date(dis[,8], format='%d.%m.%Y %H:%M'))

# Daily mean
d.mean <- apply.daily(df.xts, mean)

# Daily median
d.median <- apply.daily(df.xts, median)

# Daily min
d.min <- apply.daily(df.xts, min)

# Daily max
d.max <- apply.daily(df.xts, max)
