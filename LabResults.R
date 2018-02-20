library(grid)
library(gridExtra)
library(lattice)
library(marelac)
library(scales)
library(zoo)


setwd("C:/Users/melimore86/Desktop")
labresults <- read.csv("cedarkeylabresults.csv", header= T)

#Creating new column names
colnames(labresults) <- c("County", "Name", "Date", "Month", "Day","Year","Station", "Phosphorus", "Nitrogen", "Chlorophyll", "Secchi", "Secchi 2", "Color", "SpecificConductancemicro", "SpecificConductancemilli")

labresults$newDate <- as.Date(labresults$Date, origin= "1899-12-30")

labresults$newDate <- as.Date(labresults$Date, "%m/%d/%Y")

#create new column for the micro and milli siemens in Specific Conductance
#1 millisiemens [mS] = 1000 microsiemens [??S, uS]

labresults$allconduct<-NA

labresults$allconduct<-paste(labresults$SpecificConductancemilli,labresults$SpecificConductancemicro)

labresults$allconduct<-as.numeric(gsub('NA','',labresults$allconduct))

#Subsetting the data into 
site1<-subset(labresults,Station=="1")
site2<-subset(labresults,Station=="2")
site3<-subset(labresults,Station=="3")
site4<-subset(labresults,Station=="4")
site5<-subset(labresults,Station=="5")
site6<-subset(labresults,Station=="6")


site1_zoo <- zoo(site1[, 2:5],order.by= site1$newDate)
site2_zoo <- zoo(site2[, 2:5],order.by= site2$newDate)
site3_zoo <- zoo(site3[, 2:5],order.by= site3$newDate)
site4_zoo <- zoo(site4[, 2:5],order.by= site4$newDate)
site5_zoo <- zoo(site5[, 2:5],order.by= site5$newDate)
site6_zoo <- zoo(site6[, 2:5],order.by= site6$newDate)

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



##Testing with river discharge



xx <- c(dis$newDate, rev(dis$newDate))
yy <- c(rep(0, nrow(dis)), rev(dis$Values))


windows()
plot(site6$newDate, site6$Phosphorus, pch=19, ylab="Phosphorus (µg/L)", xlab="Date", main="Site 6 Phosphorus", ylim=c(0,100),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)


#Phosphorus
windows()
par(mfrow=c(3,2))
plot(site6$newDate, site6$Phosphorus, pch=19, ylab="Phosphorus (µg/L)", xlab="Date", main="Site 6 Phosphorus", ylim=c(0,100),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)
plot(site1$newDate, site6$Phosphorus, pch=19, ylab="Phosphorus (µg/L)", xlab="Date", main="Site 6 Phosphorus", ylim=c(0,100),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)
plot(site5$newDate, site6$Phosphorus, pch=19, ylab="Phosphorus (µg/L)", xlab="Date", main="Site 6 Phosphorus", ylim=c(0,100),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)
plot(site2$newDate, site6$Phosphorus, pch=19, ylab="Phosphorus (µg/L)", xlab="Date", main="Site 6 Phosphorus", ylim=c(0,100),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)
plot(site4$newDate, site6$Phosphorus, pch=19, ylab="Phosphorus (µg/L)", xlab="Date", main="Site 6 Phosphorus", ylim=c(0,100),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)
plot(site3$newDate, site6$Phosphorus, pch=19, ylab="Phosphorus (µg/L)", xlab="Date", main="Site 6 Phosphorus", ylim=c(0,100),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)


#Nitrogen


windows()
par(mfrow=c(3,2))
plot(site6$newDate, site6$Nitrogen, pch=19, ylab="Nitrogen (µg/L)", xlab="Date", main="Site 6 Nitrogen", ylim=c(0,1300),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)

plot(site1$newDate, site1$Nitrogen, pch=19, ylab="Nitrogen (µg/L)", xlab="Date", main="Site 1 Nitrogen", ylim=c(0,1300),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)

plot(site5$newDate, site5$Nitrogen, pch=19, ylab="Nitrogen (µg/L)", xlab="Date", main="Site 5 Nitrogen", ylim=c(0,1300),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)

plot(site2$newDate, site2$Nitrogen, pch=19, ylab="Nitrogen (µg/L)", xlab="Date", main="Site 2 Nitrogen", ylim=c(0,1300),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)

plot(site4$newDate, site4$Nitrogen, pch=19, ylab="Nitrogen (µg/L)", xlab="Date", main="Site 4 Nitrogen", ylim=c(0,1300),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)

plot(site3$newDate, site3$Nitrogen, pch=19, ylab="Nitrogen (µg/L)", xlab="Date", main="Site 3 Nitrogen", ylim=c(0,1300),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)



windows()
par(mfrow=c(3,2))
plot(site6$newDate, site6$Nitrogen, pch=19, ylab="Nitrogen (µg/L)", xlab="Date", main="Site 6 Nitrogen",ylim=c(0,1300))
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,30000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")),col="lightblue", type="l")
polygon(xx,yy,col="lightblue")
axis(4, at=pretty(c(0,30000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6 )
plot(site1$newDate, site1$Nitrogen, pch=19, ylab="Nitrogen  (µg/L)", xlab="Date", main="Site 1 Nitrogen",ylim=c(0,1300))
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,30000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")),col="lightblue", type="l")
polygon(xx,yy,col="lightblue")
axis(4, at=pretty(c(0,30000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6 )
plot(site5$newDate, site5$Nitrogen, pch=19, ylab="Nitrogen (µg/L)", xlab="Date", main="Site 5 Nitrogen",ylim=c(0,1300))
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,30000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")),col="lightblue", type="l")
polygon(xx,yy,col="lightblue")
axis(4, at=pretty(c(0,30000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6 )
plot(site2$newDate, site2$Nitrogen, pch=19, ylab="Nitrogen (µg/L)", xlab="Date", main="Site 2 Nitrogen",ylim=c(0,1300))
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,30000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")),col="lightblue", type="l")
polygon(xx,yy,col="lightblue")
axis(4, at=pretty(c(0,30000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6 )
plot(site4$newDate, site4$Nitrogen, pch=19, ylab="Nitrogen (µg/L)", xlab="Date", main="Site 4 Nitrogen",ylim=c(0,1300))
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,30000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")),col="lightblue", type="l")
polygon(xx,yy,col="lightblue")
axis(4, at=pretty(c(0,30000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6 )
plot(site3$newDate, site3$Nitrogen, pch=19, ylab="Nitrogen (µg/L)", xlab="Date", main="Site 3 Nitrogen",ylim=c(0,1300))
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,30000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")),col="lightblue", type="l")
polygon(xx,yy,col="lightblue")
axis(4, at=pretty(c(0,30000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6 )


#Chlorophyll
windows()
par(mfrow=c(3,2))
plot(site6$newDate, site6$Chlorophyll, pch=19, ylab="Chlorophyll (µg/L)", xlab="Date", main="Site 6 Chlorophyll", ylim=c(0,65),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)

plot(site1$newDate, site1$Chlorophyll, pch=19, ylab="Chlorophyll (µg/L)", xlab="Date", main="Site 1 Chlorophyll", ylim=c(0,65),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)

plot(site5$newDate, site5$Chlorophyll, pch=19, ylab="Chlorophyll (µg/L)", xlab="Date", main="Site 5 Chlorophyll", ylim=c(0,65),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)

plot(site2$newDate, site2$Chlorophyll, pch=19, ylab="Chlorophyll (µg/L)", xlab="Date", main="Site 2 Chlorophyll", ylim=c(0,65),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)

plot(site4$newDate, site4$Chlorophyll, pch=19, ylab="Chlorophyll (µg/L)", xlab="Date", main="Site 4 Chlorophyll", ylim=c(0,65),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)

plot(site3$newDate, site3$Chlorophyll, pch=19, ylab="Chlorophyll (µg/L)", xlab="Date", main="Site 3 Chlorophyll", ylim=c(0,65),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)






#Secchi
windows()
par(mfrow=c(3,2))

plot(site6$newDate, site6$Secchi, pch=19, ylab="Secchi (ft)", xlab="Date", main="Site 6 Secchi", ylim=c(0,5),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)

plot(site1$newDate, site1$Secchi, pch=19, ylab="Secchi (ft)", xlab="Date", main="Site 1 Secchi", ylim=c(0,5),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)

plot(site5$newDate, site5$Secchi, pch=19, ylab="Secchi (ft)", xlab="Date", main="Site 5 Secchi", ylim=c(0,5),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)

plot(site2$newDate, site2$Secchi, pch=19, ylab="Secchi (ft)", xlab="Date", main="Site 2 Secchi", ylim=c(0,5),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)

plot(site4$newDate, site4$Secchi, pch=19, ylab="Secchi (ft)", xlab="Date", main="Site 4 Secchi", ylim=c(0,5),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)

plot(site3$newDate, site3$Secchi, pch=19, ylab="Secchi (ft)", xlab="Date", main="Site 3 Secchi", ylim=c(0,5),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)


#Color
windows()
par(mfrow=c(3,2))
plot(site6$newDate, site6$Color, pch=19, ylab="Color (Pt-Co Units)", xlab="Date", main="Site 6 Color", ylim=c(0,110),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)

plot(site1$newDate, site1$Color, pch=19, ylab="Color (Pt-Co Units)", xlab="Date", main="Site 1 Color", ylim=c(0,110),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)

plot(site5$newDate, site5$Color, pch=19, ylab="Color (Pt-Co Units)", xlab="Date", main="Site 5 Color", ylim=c(0,110),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)

plot(site2$newDate, site2$Color, pch=19, ylab="Color (Pt-Co Units)", xlab="Date", main="Site 2 Color", ylim=c(0,110),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)

plot(site4$newDate, site4$Color, pch=19, ylab="Color (Pt-Co Units)", xlab="Date", main="Site 4 Color", ylim=c(0,110),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)

plot(site3$newDate, site3$Color, pch=19, ylab="Color (Pt-Co Units)", xlab="Date", main="Site 3 Color", ylim=c(0,110),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)





#Specific Conductance
windows()

par(mfrow=c(3,2))
plot(site6$newDate, site6$allconduct, pch=19, ylab="Specific Conductance (mS/cm))", xlab="Date", main="Site 6 Specific Conductance", ylim=c(0,40),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)

plot(site1$newDate, site1$allconduct, pch=19, ylab="Specific Conductance (mS/cm))", xlab="Date", main="Site 1 Specific Conductance", ylim=c(0,40),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)


plot(site5$newDate, site5$allconduct, pch=19, ylab="Specific Conductance (mS/cm))", xlab="Date", main="Site 5 Specific Conductance", ylim=c(0,40),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)


plot(site2$newDate, site2$allconduct, pch=19, ylab="Specific Conductance (mS/cm))", xlab="Date", main="Site 2 Specific Conductance", ylim=c(0,40),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)


plot(site4$newDate, site4$allconduct, pch=19, ylab="Specific Conductance (mS/cm))", xlab="Date", main="Site 4 Specific Conductance", ylim=c(0,40),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)


plot(site3$newDate, site3$allconduct, pch=19, ylab="Specific Conductance (mS/cm))", xlab="Date", main="Site 6 Specific Conductance", ylim=c(0,40),format="%m-%y",las=2, cex=1.5)
par(new=TRUE)
plot(x=dis$newDate, y=dis$Values, ylab="", lwd= 2, xlab= "Date", main= "",ylim=c(0,35000), axes=FALSE,xlim = c(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-31")), type="l",col=rgb(0,0,1, 0.2))
polygon(xx,yy,col=rgb(0,0,1, 0.2), border=NA)
axis(4, at=pretty(c(0,35000)), col='black')
mtext("River Discharge cf/s", side = 4,cex=0.6)















                
                  
