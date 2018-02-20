setwd("C:/Users/melimore86/Desktop/WQ Sonde Trial/Back_dock")

rm(list=ls())

#Reading in dataset for DIVER (this is the type of sensor for WQ 1 and WQ 2)
DIVER_ALL <- read.csv("diver_back_dock.csv", header=T)
#Naming columns 
colnames(DIVER_ALL)<-c("Date", "Time","Pressure", "Temperature", "Spec.conduct")
head(DIVER_ALL)

#next line very important. The Diver type of sensor records in Conductivity or specific conductivity. By default
#it should be conductivity.  When I ran these trials it recorded in spec.conductivity, so I had to convert
#point is you have to look to see on the data file is it conductivity or specific conductivity

#so have to convert to conductivity

DIVER_ALL$Conductivity = DIVER_ALL$Spec.conduct *(1 + 0.0191 * (DIVER_ALL$Temperature - 25))

#Conductivity = Spec.cond *(1 + 0.0191 * (temp(C) - 25)), to convert from specific conductivity to regular conductivity
#(e.g., https://www.solinst.com/products/dataloggers-and-telemetry/3001-levelogger-series/operating-instructions/user-guide/1-introduction/1-2-4-conductivity.php)


#Reading in Star-oddi (this is the type for WQ2 Station)
#note Star-Oddi is reading conductivity so no need for conversion
STAR_ALL<-read.csv("star_back_dock.csv", header=T)
colnames(STAR_ALL)<-c("DateTime_serial", "Temp_Cel", "Sal_psu", "Conductivity", "Sound", "DateTime")

head(STAR_ALL)
#STAR_ALL= filter(STAR_ALL, Conductivity > 20)

#Reading refractometer
#this was a one time thing to provide some spot checks

REFRAC_ALL <- read.csv("refractometer_back_dock.csv", header=T)
head(REFRAC_ALL)

#r way of changing serial dates, seems to want to start at 7pm where excel says midnight...? based on plots excel version seems correct (given matching of temp)
STAR_ALL$newDate <- as.POSIXct(as.Date(STAR_ALL$DateTime_serial,origin="1899-12-30"))

#Changing to proper Datetime format in r
DIVER_ALL$DateTime<-as.POSIXct(paste(DIVER_ALL$Date, DIVER_ALL$Time), format="%m/%d/%Y %H:%M")
STAR_ALL$DateTime<-as.POSIXct(STAR_ALL$DateTime, format="%m/%d/%y %H:%M")
REFRAC_ALL$DateTime<-as.POSIXct(paste(REFRAC_ALL$Date, REFRAC_ALL$Time), format="%m/%d/%Y %H:%M")

##This refrac_all dateTime is not working above. unsure why

#Making them time series objects
library(zoo)
library(dplyr)
DIVER_zoo<-zoo(DIVER_ALL[,3:6],order.by=DIVER_ALL$DateTime)
head(DIVER_zoo)

STAR_zoo<-zoo(STAR_ALL[,2:4], order.by=STAR_ALL$DateTime)
head(STAR_zoo)


REFRAC_zoo<-zoo(REFRAC_ALL[,2:3],order.by=REFRAC_ALL$DateTime)
head(REFRAC_zoo)




# #Salinity, #Note stardog is in psu and refrac is in ppt
plot(STAR_zoo$Sal_psu,ylab="Salinity (psu)", lwd=2, xlab="Date", ylim=c(10:37))
points(REFRAC_zoo$Sal_ppt, col="blue",pch=5, lwd=2)


#Conductivity
plot(STAR_zoo$Conductivity, lwd=2, ylab="Conductivity (mS/cm)", xlab="Date",ylim=c(20,60))
lines(DIVER_zoo$Conductivity, lwd=2, col="red")



plot(STAR_zoo$Conductivity, DIVER_zoo$Conductivity, ylim=c(35,55), xlim=c(35,55))



#Temperature
plot(STAR_zoo$Temp_Cel, lwd=2, ylab="Temperature (C)", xlab="Date", ylim=c(20:35))
lines(DIVER_zoo$Temperature, lwd=2, col="red")

#below is how you would do this, but not updated for this example


##ok revise this to put the three together

# #All together now, rename tiff file name for where you want plot to go 
# tiff(filename="C:/Users/billpine/Google Drive/GEBF Lone Cabbage Oyster/WQ/WQ Sonde Trial/Back_dock/plot3.tiff", height = 22.7, width = 17.3, units = 'cm', compression = "lzw", res = 500)
# par(mfrow=c(3,1), oma = c(4,2,0,0.5) + 0.1, mar = c(1,4,1,1) + 0.1, xpd=F)

# #Salinity
# plot(YSI_zoo$Sal,xaxt="n",xlab="", ylab="Salinity (ppt)", lwd=2, ylim=c(10,30), cex.lab=1.2, font.lab=2)
# lines(stardog_zoo$Sal_psu, col="red", lwd=2)
# points(DACS_zoo$Bot_sal, col="blue",pch=5, lwd=2)
# plot(YSI_zoo$Cond, lwd=2, ylab="Conductivity (mS/cm)", xaxt="n",xlab="", ylim=c(20,50),cex.lab=1.2,font.lab=2)
# lines(stardog_zoo$Conductivity, lwd=2, col="red") 
# legend("topright", c("YSI_ALL", "Star Dog"), lwd=2, col=c("black", "red"))
# plot(YSI_zoo$Temp, xlab="Date", ylab="Temperature (Fahrenheit)", lwd=2, ylim=c(65,85),cex.lab=1.2,font.lab=2)
# lines(stardog_zoo$Temp_Cel*(9/5)+32, col="red", lwd=2)
# mtext("Date", side=1, line=3)
# dev.off()

