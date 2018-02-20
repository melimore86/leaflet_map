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

setwd("C:/Users/melimore86/Desktop/Deployment/Lone Cabbage WQ Data/LC_WQ1/CSV")
LC_WQ1 <- read.csv("LC_WQ1_All_Days_R.csv", header= T)

setwd("C:/Users/melimore86/Desktop/Deployment/Lone Cabbage WQ Data/LC_WQ2/CSV")
LC_WQ2 <- read.csv("LC_WQ2_All_Days_R.csv", header= T)

setwd("C:/Users/melimore86/Desktop/Deployment/Lone Cabbage WQ Data/LC_WQ3/CSV")
LC_WQ3 <- read.csv("LC_WQ3_All_Days_R.csv", header= T)

setwd("C:/Users/melimore86/Desktop/Deployment/Lone Cabbage WQ Data/LC_WQ4/CSV")
LC_WQ4 <- read.csv("LC_WQ4_All_Days_R.csv", header= T)

setwd("C:/Users/melimore86/Desktop/Deployment/Lone Cabbage WQ Data/LC_WQ5/CSV")
LC_WQ5 <- read.csv("LC_WQ5_All_Days_R.csv", header= T)

setwd("C:/Users/melimore86/Desktop/Deployment/Lone Cabbage WQ Data/LC_WQ6/CSV")
LC_WQ6 <- read.csv("LC_WQ6_All_Days_R.csv", header= T)

setwd("C:/Users/melimore86/Desktop/Deployment/Lone Cabbage WQ Data/LC_WQ7/CSV")
LC_WQ7 <- read.csv("LC_WQ7_All_Days_R.csv", header= T)

setwd("C:/Users/melimore86/Desktop/Deployment/Lone Cabbage WQ Data/LC_WQ8/CSV")
LC_WQ8 <- read.csv("LC_WQ8_All_Days_R.csv", header= T)

setwd("C:/Users/melimore86/Desktop/Deployment/Lone Cabbage WQ Data/LC_WQ9/CSV")
LC_WQ9 <- read.csv("LC_WQ9_All_Days_R.csv", header= T)


colnames(LC_WQ1) <- c("DateTime_Serial", "Pressure", "Temperature", "Conductivity")
colnames(LC_WQ3) <- c("DateTime_Serial", "Pressure", "Temperature", "Conductivity")
colnames(LC_WQ2) <- c("DateTime_Serial", "Temperature", "Salinity", "Conductivity", "Pressure")
colnames(LC_WQ4) <- c("DateTime_Serial", "Temperature", "Salinity", "Conductivity", "Pressure")
colnames(LC_WQ5) <- c("DateTime_Serial", "Temperature", "Salinity", "Conductivity", "Pressure")
colnames(LC_WQ6) <- c("DateTime_Serial", "Temperature", "Salinity", "Conductivity", "Pressure")
colnames(LC_WQ7) <- c("DateTime_Serial", "Temperature", "Salinity", "Conductivity", "Pressure")
colnames(LC_WQ8) <- c("DateTime_Serial", "Temperature", "Salinity", "Conductivity", "Pressure")
colnames(LC_WQ9) <- c("DateTime_Serial", "Temperature", "Salinity", "Conductivity", "Pressure")

LC_WQ1$newDate <- as.POSIXct(as.Date(LC_WQ1$DateTime_Serial,origin= "1899-12-30"))
LC_WQ2$newDate <- as.POSIXct(as.Date(LC_WQ2$DateTime_Serial,origin= "1899-12-30"))
LC_WQ3$newDate <- as.POSIXct(as.Date(LC_WQ3$DateTime_Serial,origin= "1899-12-30"))
LC_WQ4$newDate <- as.POSIXct(as.Date(LC_WQ4$DateTime_Serial,origin= "1899-12-30"))
LC_WQ5$newDate <- as.POSIXct(as.Date(LC_WQ5$DateTime_Serial,origin= "1899-12-30"))
LC_WQ6$newDate <- as.POSIXct(as.Date(LC_WQ6$DateTime_Serial,origin= "1899-12-30"))
LC_WQ7$newDate <- as.POSIXct(as.Date(LC_WQ7$DateTime_Serial,origin= "1899-12-30"))
LC_WQ8$newDate <- as.POSIXct(as.Date(LC_WQ8$DateTime_Serial,origin= "1899-12-30"))
LC_WQ9$newDate <- as.POSIXct(as.Date(LC_WQ9$DateTime_Serial,origin= "1899-12-30"))


library(marelac)

standard= 42.914

LC_WQ1$Salinity <- convert_RtoS(LC_WQ1$Conductivity/standard, 
                                t= LC_WQ1$Temperature, p= 0)
LC_WQ2$Salinity <- convert_RtoS(LC_WQ2$Conductivity/standard, 
                                t= LC_WQ2$Temperature, p=0)
LC_WQ3$Salinity <- convert_RtoS(LC_WQ3$Conductivity/standard, 
                                t= LC_WQ3$Temperature, p= 0)
LC_WQ4$Salinity <- convert_RtoS(LC_WQ4$Conductivity/standard, 
                                t= LC_WQ4$Temperature, p=0)
LC_WQ5$Salinity <- convert_RtoS(LC_WQ5$Conductivity/standard, 
                                t= LC_WQ5$Temperature, p=0)
LC_WQ6$Salinity <- convert_RtoS(LC_WQ6$Conductivity/standard, 
                                t= LC_WQ6$Temperature, p=0)
LC_WQ7$Salinity <- convert_RtoS(LC_WQ7$Conductivity/standard, 
                                t= LC_WQ7$Temperature, p=0)
LC_WQ8$Salinity <- convert_RtoS(LC_WQ8$Conductivity/standard, 
                                t= LC_WQ8$Temperature, p=0)
LC_WQ9$Salinity <- convert_RtoS(LC_WQ9$Conductivity/standard, 
                                t= LC_WQ9$Temperature, p=0)



LC_WQ1$site<-1
LC_WQ2$site<-2
LC_WQ3$site<-3
LC_WQ4$site<-4
LC_WQ5$site<-5
LC_WQ6$site<-6
LC_WQ7$site<-7
LC_WQ8$site<-8
LC_WQ9$site<-9


#total <- merge(data frameA,data frameB,by=c("ID","Country")) 
allsals<- rbind(LC_WQ6, LC_WQ1,LC_WQ7,LC_WQ5,LC_WQ2,LC_WQ8,LC_WQ4,LC_WQ3,LC_WQ9)



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


windows()
ggplot(data= allsals, aes(x= newDate, group= site)) +
  ggtitle("Salinity per Site") +
  labs(x= "Date", y= "Temp(C) & Salinity (ppt)") +
  geom_point(aes(y=Temperature, color="Temperature"),size=1, col="red") +
  geom_point(aes(y= Salinity,color="Salinity"),size=1, col="black") +
  facet_wrap(~site,scales="free_y") +
  expand_limits(y=0) +
  scale_x_datetime(
    breaks = date_breaks("3 weeks") ,
    labels = date_format("%m/%d/%y"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2017-08-01"),
      as.POSIXct("2018-02-10"))) +
  #geom_ribbon(data= dis, aes(x= newDate, y=Values/500, ymin=0, ymax=Values/500, fill= "blue"), fill= "cornflowerblue", alpha=0.5) +
  #scale_y_continuous(sec.axis = sec_axis(~.*500, name = "River Discharge"), limits=c(0,40)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text.x = element_text(angle = 90, hjust = 1),strip.text.x = element_text(size=15,face="bold"))


