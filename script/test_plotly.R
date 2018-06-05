library(cowplot)
library(devtools)
library(ggplot2)
library(ggpubr)
library(grid)
library(gridExtra)
library(lattice)
library(marelac)
library(scales)
library(zoo)
library(ggpubr)
library(plotly)



setwd("C:/Users/melimore86/Desktop")
labresults <- read.csv("cedarkeylabresults.csv", header= T)

#Creating new column names
colnames(labresults) <- c("County", "Name", "Date", "Month", "Day","Year","Station", "Phosphorus", "Nitrogen", "Chlorophyll", "Secchi", "Secchi 2", "Color", "SpecificConductancemicro", "SpecificConductancemilli")

labresults$newDate <- as.POSIXct(as.Date(labresults$Date, origin= "1899-12-30"))


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



subplot(
  plot_ly(site1, x = site1$newDate, y = site1$Phosphorus, name = "Site 1"),
  add_markers(name = "Site 1 Phosphorus", color="blue")
  )

windows()

subplot(
  plot_ly(site1, x = site1$newDate, y = site1$Phosphorus, name = "Site 1 Phosphorus"),
  plot_ly(site2, x = site2$newDate, y = site2$Phosphorus) %>% 
    add_markers(name = "Site 2 Phosphorus"),
  plot_ly(site3, x = site3$newDate, y = site3$Phosphorus) %>% 
    add_markers(name = "Site 3 Phosphorus"),
  plot_ly(site4, x = site4$newDate, y = site4$Phosphorus) %>% 
    add_markers(name = "Site 4 Phosphorus"),
  plot_ly(site5, x = site5$newDate, y = site5$Phosphorus) %>% 
    add_markers(name = "Site 5 Phosphorus"),
  plot_ly(site5, x = site6$newDate, y = site6$Phosphorus) %>% 
    add_markers(name = "Site 6 Phosphorus")
)


subplot(
  plot_ly(site1, x = site1$newDate, y = site1$Phosphorus, name = "Phosphorus  (µg/L)",color=I("black")),
  plot_ly(site1, x = site1$newDate, y = site1$Nitrogen) %>% 
    add_markers(name = "Nitrogen (µg/L)", color=I("red")),
  plot_ly(site1, x = site1$newDate, y = site1$Color) %>% 
    add_markers(name = "Color (Pt-Co Units)")
)

 labbing<- labresults %>% group_by(Station)

#A scatter trace is initialized with plot_ly or add_trace:
#   plot_ly(df, type="scatter"[, ...])
# add_trace(p, type="scatter"[, ...])
# A scatter trace accepts any of the keys listed belo


plot_ly(labbing, x = ~newDate, y = ~Phosphorus) %>% add_markers()


plot_ly(data = labresults, x = ~newDate, y = ~Phosphorus,mode="markers")

plot_ly(labresults, x = labresults$newDate, y =~Phosphorus) %>%
add_lines(color = ~Station, colors="black")

labresults %>%
  group_by(Station) %>%
  plot_ly(x =~labresults$newDate, y = ~labresults$Phosphorus,mode="markers") %>%
  add_markers(colors = ~labresults$Station)

plot_ly(labresults, x = ~labresults$newDate, y =~labresults$Phosphorus,group=labresults$Station, marker=list(color=~Station))
