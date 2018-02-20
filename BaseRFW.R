library("cowplot")
library("devtools")
library("ggplot2")
library("ggpubr")
library("grid")
library("gridExtra")
library("lattice")
library("marelac")
library("scales")
library("zoo")
library("ggpubr")



setwd("C:/Users/melimore86/Desktop")
labresults <- read.csv("cedarkeylabresults.csv", header= T)

#Creating new column names
colnames(labresults) <- c("County", "Name", "Date", "Month", "Day","Year","Station", "Phosphorus", "Nitrogen", "Chlorophyll", "Secchi", "Secchi 2", "Color", "SpecificConductancemicro", "SpecificConductancemilli")

labresults$newDate <- as.POSIXct(as.Date(labresults$Date, origin= "1899-12-30"))

labresults$newDate <- as.POSIXct(as.Date(labresults$Date, "%m/%d/%Y"))

#create new column for the micro and milli siemens in Specific Conductance
#1 millisiemens [mS] = 1000 microsiemens [??S, uS]

labresults$allconduct<-NA

labresults$allconduct<-paste(labresults$SpecificConductancemilli,labresults$SpecificConductancemicro)

labresults$allconduct<-as.numeric(gsub('NA','',labresults$allconduct))


site1<-subset(labresults,Station=="1")
site2<-subset(labresults,Station=="2")
site3<-subset(labresults,Station=="3")
site4<-subset(labresults,Station=="4")
site5<-subset(labresults,Station=="5")
site6<-subset(labresults,Station=="6")


# River Disharge
library("waterData")
library("hydroTSM")

#station to analyze
station = '02323500'   
#get site name to use in plot titles and such
stinfo  = siteInfo(station)

#read entire time series
dis   = importDVs(staid=station,code='00060',stat='00003', sdate= "1950-01-01")

#get some date components
dis$year    = as.numeric(strftime(dis$dates,format="%Y"))
dis$month   = as.numeric(strftime(dis$dates,format="%m")) 

#make dataset from epochs, 
disE  = dis[dis$dates>='1950-01-01' & dis$dates<='2017-10-01',]  

#get monthly sum, mean, sd, and var
#discharge
disE.mo  = aggregate(val~month+year,data=disE,FUN = function(x) c(mean(x,na.rm=T),sd(x,na.rm=T),var(x,na.rm=T),sum(x)))
disE.mo  = do.call('data.frame',disE.mo)
names(disE.mo)[3:6] = c('avg','sd','var','sumflow') 
disE.mo$yrmo = disE.mo$year+(disE.mo$month-0.5)/12       


#get yearly mean, sd, and var
#discharge
disE.yr  = aggregate(val~year,data=disE,FUN = function(x) c(mean(x,na.rm=T),sd(x,na.rm=T),var(x,na.rm=T)))
disE.yr  = do.call('data.frame',disE.yr)
names(disE.yr)[2:4] = c('avg','sd','var')                      


#make some time series objects
disE.zoo    = zoo(disE$val,disE$dates)  

disE.mo.ts  = ts(disE.mo$avg,start=c(1950,1),end=c(2017,10),frequency=12)
disE.mo.sum.ts  = ts(disE.mo$sumflow,start=c(1950,1),end=c(2017,10),frequency=12)
disE.yr.ts  = ts(disE.yr$avg,start=1950,end=2017,frequency=1)

#Naming columns, using the Diver sensors, collects date, pressure, temp, conductivity
colnames(dis) <- c("StaID", "Values", "Date", "QualCode", "Year", "Month")
head(dis)

#Changing the format of the dates to be able to plot against time
dis$newDate <- as.POSIXct(as.Date(dis$Date,origin= "1899-12-30"))

#Using `zoo` to add the date to the column
dis_zoo <- zoo(dis[, 2:5],order.by= dis$newDate)


##Facet wrapping 


windows()
ggplot(data= labresults, aes(x= newDate, y= Phosphorus, group= Station)) +
  ggtitle("Phosphorus per Site") +
  labs(x= "Date", y= "Phosphorus") +
  geom_point() +
  facet_wrap(~Station) +
  expand_limits(y=0) +
  #geom_ribbon(data= dis, aes(x= newDate, y=Values/500, ymin=0, ymax=Values/500, fill= "blue"), fill= "cornflowerblue", alpha=0.5) +
  #scale_y_continuous(sec.axis = sec_axis(~.*500, name = "River Discharge"), limits=c(0,40)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text.x = element_text(angle = 90, hjust = 1))


windows()
ggplot(data= labresults, aes(x= newDate, y= Nitrogen, group= Station)) +
  ggtitle("Nitrogen per Site") +
  labs(x= "Date", y= "Nitrogen") +
  geom_point() +
  facet_wrap(~Station) +
  expand_limits(y=0) +
  #geom_ribbon(data= dis, aes(x= newDate, y=Values/500, ymin=0, ymax=Values/500, fill= "blue"), fill= "cornflowerblue", alpha=0.5) +
  #scale_y_continuous(sec.axis = sec_axis(~.*500, name = "River Discharge"), limits=c(0,40)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text.x = element_text(angle = 90, hjust = 1))

windows()
ggplot(data= labresults, aes(x= newDate, y= Color, group= Station)) +
  ggtitle("Color per Site") +
  labs(x= "Date", y= "Color") +
  geom_point() +
  facet_wrap(~Station) +
  expand_limits(y=0) +
  #geom_ribbon(data= dis, aes(x= newDate, y=Values/500, ymin=0, ymax=Values/500, fill= "blue"), fill= "cornflowerblue", alpha=0.5) +
  #scale_y_continuous(sec.axis = sec_axis(~.*500, name = "River Discharge"), limits=c(0,40)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text.x = element_text(angle = 90, hjust = 1))

windows()
ggplot() +
  geom_ribbon(data= dis, aes(x= newDate, y=Values/300, ymin=0, ymax=Values/300, fill= "blue"), fill= "cornflowerblue", alpha=0.5) +
  geom_point(data=site1, aes(x= newDate, y= Phosphorus, color= "Phosphorus"), color="black", size=3)+
  ggtitle("Secchi per Site") +
  labs(x= "Date", y= "Secchi") +
  scale_y_continuous(sec.axis = sec_axis(~.*300, name = "River Discharge"), limits=c(0,115)) +
  scale_x_datetime(
    breaks = date_breaks("4 weeks") ,
    labels = date_format("%m/%d/%y"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2016-12-01"),
      as.POSIXct("2018-01-31"))) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text.x = element_text(angle = 90, hjust = 1))


  
  
  
  
  
    breaks = date_breaks("1 month") ,
    labels = date_format("%m/%d/%y"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2017-08-10"),
      as.POSIXct("2018-01-30"))) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1,size=12),
        aspect.ratio = 0.70)
