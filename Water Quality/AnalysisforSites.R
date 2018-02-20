#Analyzing different variables based on sensor measurements.
#The most important measurements are temperature and conductivity, which will calculate
#salinity.
#I standarzied all of the time in a serial format, and reconverted them into a more reable format for plotting. 
#The files attached have the data with the serial time stamp, no other variables in the data files have been altered or changed.

rm(list=ls())

#Make sure that you read the errors for downloading, if you get an errors, carefully, many times you will need to download packages for ggplot2 that are not in base r

# Using developer ggplot2 by Hadley
#library(devtools)
#dev_mode(on=T)
#install_github("hadley/ggplot2")
#dev_mode(on=F)

#install.packages("colorspace")
#devtools::install_github("tidyverse/ggplot2",dep = TRUE)

#Packages needed for analysis
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

##### Site 1 Analysis with Diver sensor
setwd("C:/Users/melimore86/Desktop/Deployment/Lone Cabbage WQ Data/LC_WQ1/CSV")
LC_WQ1 <- read.csv("LC_WQ1_All_Days_R.csv", header= T)

#Naming columns, using the Diver sensors, collects date, pressure, temp, conductivity
colnames(LC_WQ1) <- c("DateTime_Serial", "Pressure", "Temperature", "Conductivity")
head(LC_WQ1)

#Changing the format of the dates to be able to plot against time
LC_WQ1$newDate <- as.POSIXct(as.Date(LC_WQ1$DateTime_Serial,origin= "1899-12-30"))

#Using `zoo` to add the date to the column
LC_WQ1_zoo <- zoo(LC_WQ1[, 2:5],order.by= LC_WQ1$newDate)

#Calculating salinity, using package `marelac` for this conversion 
standard=42.914
LC_WQ1$Salinity <- convert_RtoS(LC_WQ1$Conductivity/standard, 
                                t= LC_WQ1$Temperature, p= 0)

#Plotting for Site 1 Temp and Conductivity, using base R, this is for quick graphing of single variables 
plot(LC_WQ1_zoo$Temperature, ylab="Temperature(C)", lwd=2, xlab="Date", 
     ylim= c(15, 35), main= "Site 1 Diver Temperature")

plot(LC_WQ1_zoo$Conductivity, ylab="Conductivity", lwd=2, xlab="Date", 
     ylim= c(0, 40),main= "Site 1 Diver Conductivity")

plot(LC_WQ1$Salinity, ylab="Salinity (ppt)", lwd= 2, xlab= "Date", ylim= c(0, 40), main= "Site 1 Diver Salinity")

#####Site 2 Analysis with Star Oddi sensor
setwd("C:/Users/melimore86/Desktop/Deployment/Lone Cabbage WQ Data/LC_WQ2/CSV")
LC_WQ2 <- read.csv("LC_WQ2_All_Days_R.csv", header= T)

#Naming columns, using the Star Oddi software, records different measurements than the Star Oddi, date, temperature, salinity, conducitivity, sound/velocity
colnames(LC_WQ2) <- c("DateTime_Serial", "Temperature", "Salinity", "Conductivity", "Sound/Velocity")
head(LC_WQ2)

#Changing the format of the dates to plot
LC_WQ2$newDate <- as.POSIXct(as.Date(LC_WQ2$DateTime_Serial,origin= "1899-12-30"))

#Using `zoo` to add the date to the column
LC_WQ2_zoo <- zoo(LC_WQ2[, 2:4],order.by= LC_WQ2$newDate)
str(LC_WQ2_zoo)

#Calculating salinity, using package `marelac` for this conversion, even though the Diver sensor salinity, we want to standardize the salinity by calculating it with the temperature and conductivity
standard= 42.914
LC_WQ2$Salinity <- convert_RtoS(LC_WQ2$Conductivity/standard, 
                                t= LC_WQ2$Temperature, p=0)

#Plotting for site 2 Temperature and Conductivity
plot(LC_WQ2_zoo$Temperature, ylab="Temperature(C)", lwd= 2, xlab= "Date", 
     ylim= c(15, 35), main= "Site 2 Diver Temperature")

plot(LC_WQ2_zoo$Conductivity, ylab="Conductivity", lwd= 2, xlab= "Date", 
     ylim= c(0, 60), main= "Site 2 Diver Conductivity")

plot(LC_WQ2_zoo$Salinity, ylab="Salinity (ppt)", pch=20, xlab= "Date", 
     ylim= c(0, 40), main= "Site 2 Diver Salinity")


#### Site 3 Analysis with Diver sensor
setwd("C:/Users/melimore86/Desktop/Deployment/Lone Cabbage WQ Data/LC_WQ3/CSV")
LC_WQ3 <- read.csv("LC_WQ3_All_Days_R.csv", header= T)

#Naming columns using the Diver sensors, collects date, pressure, temp, conductivity
colnames(LC_WQ3) <- c("DateTime_Serial", "Pressure", "Temperature", "Conductivity")
head(LC_WQ3)

#Changing the format of the dates so I can plot
LC_WQ3$newDate <- as.POSIXct(as.Date(LC_WQ3$DateTime_Serial,origin= "1899-12-30"))

#Using `zoo` to add the date to the column
LC_WQ3_zoo <- zoo(LC_WQ3[, 2:4],order.by= LC_WQ3$newDate)

#Calculating salinity, using package `marelac` for this conversion
standard= 42.914
LC_WQ3$Salinity <- convert_RtoS(LC_WQ3$Conductivity/standard, 
                                t= LC_WQ3$Temperature, p= 0)

#Plotting for site 3 Temperature and Conductivtiy
plot(LC_WQ3_zoo$Temperature, ylab= "Temperature(C)", lwd= 2, xlab= "Date", 
     ylim= c(20, 35),main= "Site 3 Diver Temperature")

plot(LC_WQ3_zoo$Conductivity, ylab= "Conductivity", lwd= 2, xlab= "Date", 
     main= "Site 3 Diver Conductivity")

plot(LC_WQ3$Salinity, ylab="Salinity (ppt)", pch=20, xlab= "Date", 
ylim= c(0, 40), main= "Site 3 Diver Salinity")


#####Site 4 Analysis With Star Oddi Sensor
setwd("C:/Users/melimore86/Desktop/Deployment/Lone Cabbage WQ Data/LC_WQ4/CSV")
LC_WQ4 <- read.csv("LC_WQ4_All_Days_R.csv", header= T)

#Naming columns, using the Star Oddi software, records different measurements than the Star Oddi, date, temperature, salinity, conducitivity, sound/velocity
colnames(LC_WQ4) <- c("DateTime_Serial", "Temperature", "Salinity", "Conductivity", "Sound/Velocity")
head(LC_WQ4)


#Changing the format of the dates to plot
LC_WQ4$newDate <- as.POSIXct(as.Date(LC_WQ4$DateTime_Serial,origin= "1899-12-30"))

#Using `zoo` to add the date to the column
LC_WQ4_zoo <- zoo(LC_WQ4[, 2:4],order.by= LC_WQ4$newDate)

#Calculating salinity, using package `marelac` for this conversion, even though the Diver sensor salinity, we want to standardize the salinity by calculating it with the temperature and conductivity
standard= 42.914
LC_WQ4$Salinity <- convert_RtoS(LC_WQ4$Conductivity/standard, 
                                t= LC_WQ4$Temperature, p=0)

#Plotting for Site 4 Temperature and Conductivity
plot(LC_WQ4_zoo$Temperature, ylab="Temperature(C)", lwd= 2, xlab= "Date", 
     ylim= c(0, 35), main= "Site 4 Diver Temperature")

plot(LC_WQ4_zoo$Conductivity, ylab="Conductivity", lwd= 2, xlab= "Date", 
     ylim= c(0, 60), main= "Site 4 Diver Conductivity")

plot(LC_WQ4_zoo$Salinity, ylab="Salinity (ppt)", lwd= 2, xlab= "Date", 
     ylim= c(0, 40), main= "Site 4 Diver Salinity")


#####Site 5 Analysis with Star Oddi
setwd("C:/Users/melimore86/Desktop/Deployment/Lone Cabbage WQ Data/LC_WQ5/CSV")
LC_WQ5 <- read.csv("LC_WQ5_All_Days_R.csv", header= T)

#Naming columns, using the Star Oddi software, records different measurements than the Star Oddi, date, temperature, salinity, conducitivity, sound/velocity
colnames(LC_WQ5) <- c("DateTime_Serial", "Temperature", "Salinity", "Conductivity", "Sound/Velocity")
head(LC_WQ5)

#Changing the format of the dates to plot
LC_WQ5$newDate <- as.POSIXct(as.Date(LC_WQ5$DateTime_Serial,origin= "1899-12-30"))

#Using `zoo` to add the date to the column
LC_WQ5_zoo <- zoo(LC_WQ5[, 2:4],order.by= LC_WQ5$newDate)
str(LC_WQ5_zoo)

#Calculating salinity, using package `marelac` for this conversion, even though the Diver sensor salinity, we want to standardize the salinity by calculating it with the temperature and conductivity
standard= 42.914
LC_WQ5$Salinity <- convert_RtoS(LC_WQ5$Conductivity/standard, 
                                t= LC_WQ5$Temperature, p=0)

#Plotting for site 5 Temperature and Conductivity
plot(LC_WQ5_zoo$Temperature, ylab="Temperature(C)", lwd= 2, xlab= "Date", 
     ylim= c(15, 35), main= "Site 5 Diver Temperature")

plot(LC_WQ5_zoo$Conductivity, ylab="Conductivity", lwd= 2, xlab= "Date", 
     ylim= c(0, 60), main= "Site 5 Diver Conductivity")

plot(LC_WQ5_zoo$Salinity, ylab="Salinity (ppt)", lwd= 2, xlab= "Date", 
     ylim= c(0, 30), main= "Site 5 Diver Salinity")


#####Site 6 Analysis with Star Oddi
setwd("C:/Users/melimore86/Desktop/Deployment/Lone Cabbage WQ Data/LC_WQ6/CSV")
LC_WQ6 <- read.csv("LC_WQ6_All_Days_R.csv", header= T)

#Naming columns, using the Star Oddi software, records different measurements than the Star Oddi, date, temperature, salinity, conducitivity, sound/velocity
colnames(LC_WQ6) <- c("DateTime_Serial", "Temperature", "Salinity", "Conductivity", "Sound/Velocity")
head(LC_WQ6)

#Changing the format of the dates to plot
LC_WQ6$newDate <- as.POSIXct(as.Date(LC_WQ6$DateTime_Serial,origin= "1899-12-30"))

#Using `zoo` to add the date to the column
LC_WQ6_zoo <- zoo(LC_WQ6[, 2:4],order.by= LC_WQ6$newDate)
str(LC_WQ6_zoo)

#Calculating salinity, using package `marelac` for this conversion, even though the Diver sensor salinity, we want to standardize the salinity by calculating it with the temperature and conductivity
standard= 42.914
LC_WQ6$Salinity <- convert_RtoS(LC_WQ6$Conductivity/standard, 
                                t= LC_WQ6$Temperature, p=0)

#Plotting for Site 6 Temperature and Conductivity
plot(LC_WQ6_zoo$Temperature, ylab="Temperature(C)", lwd= 2, xlab= "Date", 
     ylim= c(15, 35), main= "Site 6 Diver Temperature")

plot(LC_WQ6_zoo$Conductivity, ylab="Conductivity", lwd= 2, xlab= "Date", 
     ylim= c(0, 50), main= "Site 6 Diver Conductivity")

plot(LC_WQ6_zoo$Salinity, ylab="Salinity (ppt)", lwd= 2, xlab= "Date", 
     ylim= c(0, 30), main= "Site 6 Diver Salinity")


#####Site 7 Analysis with Star Oddi
setwd("C:/Users/melimore86/Desktop/Deployment/Lone Cabbage WQ Data/LC_WQ7/CSV")
LC_WQ7 <- read.csv("LC_WQ7_All_Days_R.csv", header= T)

#Naming columns, using the Star Oddi software, records different measurements than the Star Oddi, date, temperature, salinity, conducitivity, sound/velocity
colnames(LC_WQ7) <- c("DateTime_Serial", "Temperature", "Salinity", "Conductivity", "Sound/Velocity")
head(LC_WQ7)

#Changing the format of the dates to plot
LC_WQ7$newDate <- as.POSIXct(as.Date(LC_WQ7$DateTime_Serial,origin= "1899-12-30"))

#Using `zoo` to add the date to the column
LC_WQ7_zoo <- zoo(LC_WQ7[, 2:4],order.by= LC_WQ7$newDate)
str(LC_WQ7_zoo)

#Calculating salinity, using package `marelac` for this conversion, even though the Diver sensor salinity, we want to standardize the salinity by calculating it with the temperature and conductivity
standard= 42.914
LC_WQ7$Salinity <- convert_RtoS(LC_WQ7$Conductivity/standard, 
                                t= LC_WQ7$Temperature, p=0)

#Plotting for Site 6 Temperature and Conductivity
plot(LC_WQ7_zoo$Temperature, ylab="Temperature(C)", lwd= 2, xlab= "Date", 
     ylim= c(15, 35), main= "Site 6 Diver Temperature")

plot(LC_WQ7_zoo$Conductivity, ylab="Conductivity", lwd= 2, xlab= "Date", 
     ylim= c(0, 50), main= "Site 6 Diver Conductivity")

plot(LC_WQ7_zoo$Salinity, ylab="Salinity (ppt)", lwd= 2, xlab= "Date", 
     ylim= c(0, 30), main= "Site 6 Diver Salinity")


#####Site 8 Analysis with Star Oddi
setwd("C:/Users/melimore86/Desktop/Deployment/Lone Cabbage WQ Data/LC_WQ8/CSV")
LC_WQ8 <- read.csv("LC_WQ8_All_Days_R.csv", header= T)

#Naming columns, using the Star Oddi software, records different measurements than the Star Oddi, date, temperature, salinity, conducitivity, sound/velocity
colnames(LC_WQ8) <- c("DateTime_Serial", "Temperature", "Salinity", "Conductivity", "Sound/Velocity")
head(LC_WQ8)

#Changing the format of the dates to plot
LC_WQ8$newDate <- as.POSIXct(as.Date(LC_WQ8$DateTime_Serial,origin= "1899-12-30"))

#Using `zoo` to add the date to the column
LC_WQ8_zoo <- zoo(LC_WQ8[, 2:4],order.by= LC_WQ8$newDate)
str(LC_WQ8_zoo)

#Calculating salinity, using package `marelac` for this conversion, even though the Diver sensor salinity, we want to standardize the salinity by calculating it with the temperature and conductivity
standard= 42.914
LC_WQ8$Salinity <- convert_RtoS(LC_WQ8$Conductivity/standard, 
                                t= LC_WQ8$Temperature, p=0)

#Plotting for Site 6 Temperature and Conductivity
plot(LC_WQ8_zoo$Temperature, ylab="Temperature(C)", lwd= 2, xlab= "Date", 
     ylim= c(15, 35), main= "Site 6 Diver Temperature")

plot(LC_WQ8_zoo$Conductivity, ylab="Conductivity", lwd= 2, xlab= "Date", 
     ylim= c(0, 50), main= "Site 6 Diver Conductivity")

plot(LC_WQ8_zoo$Salinity, ylab="Salinity (ppt)", lwd= 2, xlab= "Date", 
     ylim= c(0, 30), main= "Site 6 Diver Salinity")


#####Site 9 Analysis with Star Oddi
setwd("C:/Users/melimore86/Desktop/Deployment/Lone Cabbage WQ Data/LC_WQ9/CSV")
LC_WQ9 <- read.csv("LC_WQ9_All_Days_R.csv", header= T)

#Naming columns, using the Star Oddi software, records different measurements than the Star Oddi, date, temperature, salinity, conducitivity, sound/velocity
colnames(LC_WQ9) <- c("DateTime_Serial", "Temperature", "Salinity", "Conductivity", "Sound/Velocity")
head(LC_WQ9)

#Changing the format of the dates to plot
LC_WQ9$newDate <- as.POSIXct(as.Date(LC_WQ9$DateTime_Serial,origin= "1899-12-30"))

#Using `zoo` to add the date to the column
LC_WQ9_zoo <- zoo(LC_WQ9[, 2:4],order.by= LC_WQ9$newDate)
str(LC_WQ9_zoo)

#Calculating salinity, using package `marelac` for this conversion, even though the Diver sensor salinity, we want to standardize the salinity by calculating it with the temperature and conductivity
standard= 42.914
LC_WQ9$Salinity <- convert_RtoS(LC_WQ9$Conductivity/standard, 
                                t= LC_WQ9$Temperature, p=0)

#Plotting for Site 6 Temperature and Conductivity
plot(LC_WQ9_zoo$Temperature, ylab="Temperature(C)", lwd= 2, xlab= "Date", 
     ylim= c(15, 35), main= "Site 6 Diver Temperature")

plot(LC_WQ9_zoo$Conductivity, ylab="Conductivity", lwd= 2, xlab= "Date", 
     ylim= c(0, 50), main= "Site 6 Diver Conductivity")

plot(LC_WQ9_zoo$Salinity, ylab="Salinity (ppt)", lwd= 2, xlab= "Date", 
     ylim= c(0, 30), main= "Site 6 Diver Salinity")



#### River Discharge, this data was gathered from the River Discharge R file, and retrieved that way, the information is in the Rhistory for this analysis
head(dis)

#Naming columns, using the Diver sensors, collects date, pressure, temp, conductivity
colnames(dis) <- c("StaID", "Values", "Date", "QualCode", "Year", "Month")
head(dis)

#Changing the format of the dates to be able to plot against time
dis$newDate <- as.POSIXct(as.Date(dis$Date,origin= "1899-12-30"))

#Using `zoo` to add the date to the column
dis_zoo <- zoo(dis[, 2:5],order.by= dis$newDate)


#Since plotting the salinities is causing a problem in base R, using `ggplot` to make the plot, and then using `gridExtra` to arrange them together in the same window, adding addintion breaks with `scales`

Sal1 <- 
  ggplot(LC_WQ1, aes(y= Salinity, x= newDate)) +
  ggtitle("Site 1 Salinity") +
  labs(x= "Date", y= "Salinity(ppt)") +
  geom_path(data= LC_WQ1, color= "#E69F00") +
  scale_x_datetime(
    breaks = date_breaks("week") ,
    labels = date_format("%m/%d"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2017-08-10"),
      as.POSIXct("2018-01-30"))) +
  scale_y_continuous(limits=c(0,40)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)

Sal2 <-
  ggplot(LC_WQ2, aes(y= Salinity, x= newDate)) +
  ggtitle("Site 2 Salinity") +
  labs(x= "Date", y ="Salinity(ppt)") +
  geom_path(data= LC_WQ2, color= "#56B4E9") +
  scale_x_datetime(
    breaks = date_breaks("week") ,
    labels = date_format("%m/%d"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2017-08-10"),
      as.POSIXct("2018-01-30"))) +
  scale_y_continuous(limits=c(0,40)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)

Sal3 <-
  ggplot(LC_WQ3, aes(y= Salinity, x= newDate)) +
  ggtitle("Site 3 Salinity") +
  labs(x= "Date", y= "Salinity(ppt)") +
  geom_path(data= LC_WQ3, color= "#009E73")  +
  scale_x_datetime(
    breaks = date_breaks("week") ,
    labels = date_format("%m/%d"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2017-08-10"),
      as.POSIXct("2018-01-30"))) +
  scale_y_continuous(limits=c(0,40)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)

Sal4 <- 
  ggplot(LC_WQ4, aes(y= Salinity, x= newDate)) +
  ggtitle("Site 4 Salinity") +
  labs(x= "Date", y= "Salinity(ppt)") +
  geom_path(data= LC_WQ4, color= "#D55E00")  +
  scale_x_datetime(
    breaks = date_breaks("week") ,
    labels = date_format("%m/%d"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2017-08-10"),
      as.POSIXct("2018-01-30"))) +
  scale_y_continuous(limits=c(0,40)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)


Sal5 <- 
  ggplot(LC_WQ5, aes(y= Salinity, x= newDate)) +
  ggtitle("Site 5 Salinity") +
  labs(x= "Date", y= "Salinity(ppt)") +
  geom_path(data= LC_WQ5, color= "#999999")  +
  scale_x_datetime(
    breaks = date_breaks("week") ,
    labels = date_format("%m/%d"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2017-08-10"),
      as.POSIXct("2018-01-30"))) +
  scale_y_continuous(limits=c(0,40)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)
Sal6 <-
  ggplot(data= LC_WQ6, aes(x= newDate)) +
  ggtitle("Site 6 Salinity") +
  labs(x= "Date", y= "Salinity") +
  geom_path(data= LC_WQ6, aes(y= Salinity), color= "#000000")  +
  scale_x_datetime(
    breaks = date_breaks("week") ,
    labels = date_format("%m/%d"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2017-08-10"),
      as.POSIXct("2018-01-30"))) +
  scale_y_continuous(limits=c(0,40)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)



#Creating the 9 sites salinity/temp/river discharge

Var1 <-
  ggplot(data= LC_WQ1, aes(x= newDate)) +
  ggtitle("Site 1") +
  labs(x= "Date", y= "Temp(C) & Salinity (ppt)") +
  geom_ribbon(data= dis, aes(x= newDate, y=Values/500, ymin=0, ymax=Values/500, fill= "blue"), fill= "cornflowerblue", alpha=0.5) +
  geom_point(aes(y= Temperature, colour= "Temperature"), color= "red", size= .9) +
  geom_point(aes(y= Salinity, colour= "Salinity"), color= "#000000") +
  scale_y_continuous(sec.axis = sec_axis(~.*500, name = "River Discharge"), limits=c(0,40)) +
  scale_x_datetime(
    breaks = date_breaks("2 weeks") ,
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

Var2 <-
  ggplot(data= LC_WQ2, aes(x= newDate)) +
  ggtitle("Site 2") +
  labs(x= "Date", y= "Temp(C) & Salinity (ppt)") +
  geom_ribbon(data= dis, aes(x= newDate, y=Values/500, ymin=0, ymax=Values/500, fill= "blue"), fill= "cornflowerblue", alpha=0.5) +
  geom_point(aes(y= Temperature, colour= "Temperature"), color= "red", size= .9) +
  geom_point(aes(y= Salinity, colour= "Salinity"), color= "#000000") +
  scale_y_continuous(sec.axis = sec_axis(~.*500, name = "River Discharge"), limits=c(0,40)) +
  scale_x_datetime(
    breaks = date_breaks("2 weeks") ,
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

Var3 <-
  ggplot(data= LC_WQ3, aes(x= newDate)) +
  ggtitle("Site 3") +
  labs(x= "Date", y= "Temp(C) & Salinity (ppt)") +
  geom_ribbon(data= dis, aes(x= newDate, y=Values/500, ymin=0, ymax=Values/500, fill= "blue"), fill= "cornflowerblue", alpha=0.5) +
  geom_point(aes(y= Temperature, colour= "Temperature"), color= "red", size= .9) +
  geom_point(aes(y= Salinity, colour= "Salinity"), color= "#000000") +
  scale_y_continuous(sec.axis = sec_axis(~.*500, name = "River Discharge"), limits=c(0,40)) +
  scale_x_datetime(
    breaks = date_breaks("2 weeks") ,
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


Var4 <-
  ggplot(data= LC_WQ4, aes(x= newDate)) +
  ggtitle("Site 4") +
  labs(x= "Date", y= "Temp(C) & Salinity (ppt)") +
  geom_ribbon(data= dis, aes(x= newDate, y=Values/500, ymin=0, ymax=Values/500, fill= "blue"), fill= "cornflowerblue", alpha=0.5) +
  geom_point(aes(y= Temperature, colour= "Temperature"), color= "red", size= .9) +
  geom_point(aes(y= Salinity, colour= "Salinity"), color= "#000000") +
  scale_y_continuous(sec.axis = sec_axis(~.*500, name = "River Discharge"), limits=c(0,40)) +
  scale_x_datetime(
    breaks = date_breaks("2 weeks") ,
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

Var5 <-
  ggplot(data= LC_WQ5, aes(x= newDate)) +
  ggtitle("Site 5") +
  labs(x= "Date", y= "Temp(C) & Salinity (ppt)") +
  geom_ribbon(data= dis, aes(x= newDate, y=Values/500, ymin=0, ymax=Values/500, fill= "blue"), fill= "cornflowerblue", alpha=0.5) +
  geom_point(aes(y= Temperature, colour= "Temperature"), color= "red", size= .9) +
  geom_point(aes(y= Salinity, colour= "Salinity"), color= "#000000") +
  scale_y_continuous(sec.axis = sec_axis(~.*500, name = "River Discharge"), limits=c(0,40)) +
  scale_x_datetime(
    breaks = date_breaks("2 weeks") ,
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

Var6 <-
  ggplot(data= LC_WQ6, aes(x= newDate)) +
  ggtitle("Site 6") +
  labs(x= "Date", y= "Temp(C) & Salinity (ppt)") +
  geom_ribbon(data= dis, aes(x= newDate, y=Values/500, ymin=0, ymax=Values/500, fill= "blue"), fill= "cornflowerblue", alpha=0.5) +
  geom_point(aes(y= Temperature, colour= "Temperature"), color= "red", size= .9) +
  geom_point(aes(y= Salinity, colour= "Salinity"), color= "#000000") +
  scale_y_continuous(sec.axis = sec_axis(~.*500, name = "River Discharge"), limits=c(0,40)) +
  scale_x_datetime(
    breaks = date_breaks("2 weeks") ,
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

Var7 <-
  ggplot(data= LC_WQ7, aes(x= newDate)) +
  ggtitle("Site 7") +
  labs(x= "Date", y= "Temp(C) & Salinity (ppt)") +
  geom_ribbon(data= dis, aes(x= newDate, y=Values/500, ymin=0, ymax=Values/500, fill= "blue"), fill= "cornflowerblue", alpha=0.5) +
  geom_point(aes(y= Temperature, colour= "Temperature"), color= "red", size= .9) +
  geom_point(aes(y= Salinity, colour= "Salinity"), color= "#000000") +
  scale_y_continuous(sec.axis = sec_axis(~.*500, name = "River Discharge"), limits=c(0,40)) +
  scale_x_datetime(
    breaks = date_breaks("2 weeks") ,
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

Var8 <-
  ggplot(data= LC_WQ8, aes(x= newDate)) +
  ggtitle("Site 8") +
  labs(x= "Date", y= "Temp(C) & Salinity (ppt)") +
  geom_ribbon(data= dis, aes(x= newDate, y=Values/500, ymin=0, ymax=Values/500, fill= "blue"), fill= "cornflowerblue", alpha=0.5) +
  geom_point(aes(y= Temperature, colour= "Temperature"), color= "red", size= .9) +
  geom_point(aes(y= Salinity, colour= "Salinity"), color= "#000000") +
  scale_y_continuous(sec.axis = sec_axis(~.*500, name = "River Discharge"), limits=c(0,40)) +
  scale_x_datetime(
    breaks = date_breaks("2 weeks") ,
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


Var9 <-
  ggplot(data= LC_WQ9, aes(x= newDate)) +
  ggtitle("Site 9") +
  labs(x= "Date", y= "Temp(C) & Salinity (ppt)") +
  geom_ribbon(data= dis, aes(x= newDate, y=Values/500, ymin=0, ymax=Values/500, fill= "blue"), fill= "cornflowerblue", alpha=0.5) +
  geom_point(aes(y= Temperature, colour= "Temperature"), color= "red", size= .9) +
  geom_point(aes(y= Salinity, colour= "Salinity"), color= "#000000") +
  scale_y_continuous(sec.axis = sec_axis(~.*500, name = "River Discharge"), limits=c(0,40)) +
  scale_x_datetime(
    breaks = date_breaks("2 weeks") ,
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


#Plotting all variables, temp, sal, and RD

#Creating a legend to grob from at the end of ggdraw
legend <- 
  ggplot() +
  geom_point(data= LC_WQ1, aes(y= Salinity, x= newDate, fill= "Salinity   "), color= "#999999") +
  geom_point(data= LC_WQ1, aes(x= newDate, y= Temperature, fill= "Temperature   "), color= "#000000") +
  geom_point(data= dis, aes(x= newDate, y=Values/1000, fill= "River Discharge   "), color="cornflowerblue", size=2, show.legend = TRUE, pch=15, alpha=0.4) +
  guides(fill= guide_legend(show= TRUE, title="", override.aes= list(colour= c("cornflowerblue", "#000000", "red"), size=10))) +
  scale_y_continuous(sec.axis = sec_axis(~.*600, name = "River Discharge"), limits=c(0,40)) +
  theme(legend.position = "bottom", legend.text=element_text(size=30))

#creating a legend, to "grob" later in the ggdraw
legend_b <- get_legend(legend + theme(legend.position="bottom"))


varall<-
  ggdraw() +
  draw_plot(Var3, x=0.34, y=0, width=0.3, height=0.30 ) +
  draw_plot(Var2, x=0.34, y=0.30, width=0.3, height=0.30 ) +
  draw_plot(Var1, x=0.34, y=0.60, width=0.3, height=0.30 ) +
  draw_plot(Var4, x=0, y=0, width=0.3, height=0.30 ) +
  draw_plot(Var5, x=0, y=0.3, width=0.3, height=0.30 ) +
  draw_plot(Var6, x=0, y=0.6, width=0.3, height=0.30 ) +
  draw_plot(Var9, x=0.67, y=0, width=0.3, height=0.30 ) +
  draw_plot(Var8, x=0.67, y=0.3, width=0.3, height=0.30 ) +
  draw_plot(Var7, x=0.67, y=0.6, width=0.3, height=0.30 ) +
  draw_grob(legend_b, 0.9/2.5, 0.7, 2/7, 0.4, scale=0.5) 
setwd("C:/Users/melimore86/Desktop/Deployment/Lone Cabbage WQ Data/Comparison_graphs")
ggsave("varall.tiff", units="in", width=25, height=25, dpi=300, compression = 'lzw')
  
sites123<-
  ggdraw() +
  draw_plot(Var3, x=0.2, y=0, width=0.5, height=0.28 ) +
  draw_plot(Var2, x=0.2, y=0.30, width=0.5, height=0.28 ) +
  draw_plot(Var1, x=0.2, y=0.60, width=0.5, height=0.28 ) +
  draw_grob(legend_b, 0.9/3, 0.7, 2/7, 0.4, scale=0.4) 
setwd("C:/Users/melimore86/Desktop/Deployment/Lone Cabbage WQ Data/Comparison_graphs")
ggsave("sites123.tiff", units="in", width=25, height=25, dpi=300, compression = 'lzw')


sites456<-
  ggdraw() +
  draw_plot(Var4, x=0.2, y=0, width=0.5, height=0.28 ) +
  draw_plot(Var5, x=0.2, y=0.30, width=0.50, height=0.28 ) +
  draw_plot(Var6, x=0.2, y=0.60, width=0.50, height=0.28 ) +
  draw_grob(legend_b, 0.9/3, 0.7, 2/7, 0.4, scale=0.4) 
setwd("C:/Users/melimore86/Desktop/Deployment/Lone Cabbage WQ Data/Comparison_graphs")
ggsave("sites456.tiff", units="in", width=25, height=25, dpi=300, compression = 'lzw')


sites789<-
  ggdraw() +
  draw_plot(Var9, x=0.2, y=0, width=0.5, height=0.28 ) +
  draw_plot(Var8, x=0.2, y=0.30, width=0.5, height=0.28 ) +
  draw_plot(Var7, x=0.2, y=0.60, width=0.5, height=0.28 ) +
  draw_grob(legend_b, 0.9/3, 0.7, 2/7, 0.4, scale=0.4) 
setwd("C:/Users/melimore86/Desktop/Deployment/Lone Cabbage WQ Data/Comparison_graphs")
ggsave("sites789.tiff", units="in", width=25, height=25, dpi=300, compression = 'lzw')


#####Getting the map data for the ggplot, most likely will not use this map, but all of the sites lat/lon are in the df
library("rworldmap")
library("rworldxtra")
library("ggplot2")
library("ggmap")
require("ggthemes")

# creating a sample data.frame with your lat/lon points
lon <- c(-83.115630028769374,-83.095889976248145,-83.090120041742921,-83.092115018516779,-83.10149998404085,-83.118119034916162,-83.09822,-83.08027,-83.08271)
lat <- c(29.266499960795045,29.245640002191067,29.231049958616495,29.230171032249928,29.246092038229108,29.265770986676216,29.26773,29.25743,29.23215)
df <- as.data.frame(cbind(lon,lat))

# getting the map
mapwithpoints <- get_map(location = c(lon = mean(df$lon), lat = mean(df$lat)), zoom = 13,
                      maptype = "satellite", scale = 2)

# plotting the map with some points on it
satmap<-
  ggmap(mapwithpoints) +
  geom_point(data = df, aes(x = lon, y = lat, fill = "red"), size = 7.5, shape = 21) +
  labs(x= "Longitude", y= "Latitude") +
  guides(fill=FALSE, alpha=FALSE, size=FALSE) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        plot.title =element_text(size=30, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1))
setwd("C:/Users/melimore86/Desktop/Deployment/Lone Cabbage WQ Data/Comparison_graphs")
ggsave("satmap.tiff", units="in", width=25, height=25, dpi=300, compression = 'lzw')



#### Plotting an interactive map, using `leaflet`

require("leaflet")
require("htmlwidgets")
require("htmltools")

#Brings up the names of the providers for leaflet,the providers are the base maps or secondary layer maps that can be added to the `leaflet`
names(providers)

sites<-
  leaflet() %>%addProviderTiles("Esri.NatGeoWorldMap" )%>% setView(-83.08, 29.25, 13) %>%
  addMarkers(
    lng = -83.115630028769374, lat = 29.266499960795045,
    label = "Site 1",
    labelOptions = labelOptions(noHide = F)) %>%
  addMarkers(
    lng = -83.095889976248145, lat = 29.245640002191067,
    label = "Site 2",
    labelOptions = labelOptions(noHide = F)) %>%
  addMarkers(
    lng = -83.090120041742921, lat = 29.231049958616495,
    label = "Site 3",
    labelOptions = labelOptions(noHide = F)) %>%
  addMarkers(
    lng = -83.092115018516779, lat = 29.230171032249928,
    label = "Site 4",
    labelOptions = labelOptions(noHide = F)) %>%
  addMarkers(
    lng = -83.10149998404085, lat = 29.246092038229108,
    label = "Site 5",
    labelOptions = labelOptions(noHide = F)) %>%
  addMarkers(
    lng = -83.118119034916162, lat = 29.265770986676216,
    label = "Site 6",
    labelOptions = labelOptions(noHide = F))

#https://groups.google.com/forum/#!topic/shiny-discuss/2wGnM0FRS98
#http://leafletjs.com/plugins.html
#https://github.com/IHCantabria/Leaflet.CanvasLayer.Field


intermap<-leaflet() %>% addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>% addProviderTiles("CartoDB.Positron",group = "CartoDB Positron") %>% addProviderTiles("Esri.WorldImagery", group = "Esri WorldImagery") %>% addProviderTiles("OpenTopoMap", group = "OpenTopoMap") %>% 
 setView(-83.08, 29.25, 13) %>%
  addMarkers(
    lng = -83.115630028769374, lat = 29.266499960795045,
    label = "Site 1",
    labelOptions = labelOptions(noHide = F),
    popup= '<a href = "http://rpubs.com/melimore86/site1measurements"> Measurements for Site 1 </a>') %>%
  addMarkers(
    lng = -83.095889976248145, lat = 29.245640002191067,
    label = "Site 2",
    labelOptions = labelOptions(noHide = F),
    popup= '<a href = "http://rpubs.com/melimore86/site2measurements"> Measurements for Site 2 </a>') %>%
  addMarkers(
    lng = -83.090120041742921, lat = 29.231049958616495,
    label = "Site 3",
    labelOptions = labelOptions(noHide = F),
    popup= '<a href = "http://rpubs.com/melimore86/site3measurements"> Measurements for Site 3 </a>') %>%
  addMarkers(
    lng = -83.092115018516779, lat = 29.230171032249928,
    label = "Site 4",
    labelOptions = labelOptions(noHide = F),
    popup= '<a href = "http://rpubs.com/melimore86/site4measurements"> Measurements for Site 4 </a>') %>%
  addMarkers(
    lng = -83.10149998404085, lat = 29.246092038229108,
    label = "Site 5",
    labelOptions = labelOptions(noHide = F),
    popup= '<a href = "http://rpubs.com/melimore86/site5measurements"> Measurements for Site 5 </a>') %>%
  addMarkers(
    lng = -83.118119034916162, lat = 29.265770986676216,
    label = "Site 6",
    labelOptions = labelOptions(noHide = F),
    popup= '<a href = "http://rpubs.com/melimore86/site6measurements"> Measurements for Site 6 </a>')%>%
  addMarkers(
    lng = -83.09822, lat = 29.26773,
    label = "Site 7",
    labelOptions = labelOptions(noHide = F),
    popup= '<a href = "http://rpubs.com/melimore86/site7measurements"> Measurements for Site 7 </a>') %>%
  addMarkers(
    lng = -83.08027, lat = 29.25743,
    label = "Site 8",
    labelOptions = labelOptions(noHide = F),
    popup= '<a href = "http://rpubs.com/melimore86/site8measurements"> Measurements for Site 8 </a>') %>%
  addMarkers(
    lng = -83.08271, lat = 29.23215,
    label = "Site 9",
    labelOptions = labelOptions(noHide = F),
    popup= '<a href = "http://rpubs.com/melimore86/site9measurements"> Measurements for Site 9 </a>')%>%
  addProviderTiles("OpenWeatherMap.Clouds", group = "clouds2", options = tileOptions(opacity = 0.6)) %>%
  addTiles(urlTemplate = "http://{s}.tile.openweathermap.org/map/temp/{z}/{x}/{y}.png", attribution = "Weather data © OpenWeatherMap", group = "temp", options = tileOptions(opacity = 0.6)) %>% 
  addTiles(urlTemplate = "http://{s}.tile.openweathermap.org/map/precipitation/{z}/{x}/{y}.png", attribution = "Weather data © OpenWeatherMap", group = "precipitation", 
           options = tileOptions(opacity = 0.6)) %>% addTiles(urlTemplate = "http://{s}.tile.openweathermap.org/map/snow/{z}/{x}/{y}.png", attribution = "Weather data © OpenWeatherMap", group = "snow", options = tileOptions(opacity = 0.6)) %>% 
  addTiles(urlTemplate = "http://{s}.tile.openweathermap.org/map/wind/{z}/{x}/{y}.png", 
           attribution = "Weather data © OpenWeatherMap", group = "wind", 
           options = tileOptions(opacity = 0.6)) %>% # draw lines above
  addProviderTiles("Stamen.TonerLines", group = "Stamen Toner Lines") %>% 
  
  addLayersControl(baseGroups = c("openstreetmap", "OpenTopoMap", "CartoDB Positron", 
                                  "Esri WorldImagery", "none"), overlayGroups = c("clouds2", "temp","precipitation", "snow", "wind", "Stamen Toner Lines"), options = layersControlOptions(collapsed = TRUE)) %>% 
  hideGroup(c("temp", "precipitation", "snow", "wind"))

setwd("C:/Users/melimore86/Desktop/Deployment/Lone Cabbage WQ Data/Comparison_graphs")
saveWidget(intermap, file="intermap.html")



tester<-leaflet() %>% addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>% addProviderTiles("CartoDB.Positron",group = "CartoDB Positron") %>% addProviderTiles("Esri.WorldImagery", group = "Esri WorldImagery") %>% addProviderTiles("OpenWeatherMap.Temp", group=) %>% 
setView(10, 51, zoom = 3)%>% addLayersControl(baseGroups = c("openstreetmap", "OpenTopoMap", "CartoDB Positron", "Esri WorldImagery", "none"), overlayGroups = c("clouds2", "temp", 
                                                                              "precipitation", "snow", "wind", "Stamen Toner Lines"), options = layersControlOptions(collapsed = TRUE)) %>% 
  hideGroup(c("temp", "precipitation", "snow", "wind"))

saveWidget(tester, file="tester.html")


#4156404 is the weather station for Gainesville, FL 


test2<-leaflet() %>% addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>% addProviderTiles("CartoDB.Positron",group = "CartoDB Positron") %>% addProviderTiles("Esri.WorldImagery", group = "Esri WorldImagery") %>% addProviderTiles("OpenTopoMap", group = "OpenTopoMap") %>% 
  setView(-83.08, 29.25, 13) %>% addTiles(urlTemplate = "http://api.openweathermap.org/data/2.5/forecast?id=4156404&APPID=62428e6d96786b16c22f1589216a5159", 
                                          attribution = "Weather data © OpenWeatherMap", group = "temp", options = tileOptions(opacity = 0.6))%>% 
  
  addLayersControl(baseGroups = c("openstreetmap", "OpenTopoMap", "CartoDB Positron", 
                                  "Esri WorldImagery", "none"), overlayGroups = c("clouds2", "temp", 
                                                                                  "precipitation", "snow", "wind", "Stamen Toner Lines"), options = layersControlOptions(collapsed = TRUE)) %>% 
  hideGroup(c("temp", "precipitation", "snow", "wind"))


saveWidget(test2, file="test2.html")

#Needing to add wind data to the map
#ttps://github.com/crazycapivara/owmr


##### creating a sample data.frame with your lat/lon points
lon <- c(-83.115630028769374,-83.095889976248145,-83.090120041742921,-83.092115018516779,-83.10149998404085,-83.118119034916162,-83.09822,-83.08027,-83.08271)
lat <- c(29.266499960795045,29.245640002191067,29.231049958616495,29.230171032249928,29.246092038229108,29.265770986676216,29.26773,29.25743,29.23215)
df <- as.data.frame(cbind(lon,lat))


#####Getting the map data for the ggplot, a static satellite map of the sites, not interactivity
require("sf")
require("ggplot2")

worldmap <- getMap(resolution = "high")

map1<- sf::st_as_sf(worldmap )

##### Adding custom named cities for the map

##### Creating our map
oysterbeds2010<- readOGR(dsn= path.expand("C:/Users/melimore86/Desktop/Mapping"), layer="LC_2010")

mapplot<-
ggplot()+
  
  geom_sf(data=map1, fill="antiquewhite1") +
  
  coord_sf(xlim = c(-83.20, -83.0), ylim = c(29.20, 29.40), expand=FALSE ) +
  
 geom_point(data=df, aes(x = lon, y = lat, fill = "darkred"), size = 3.2, shape = 21, inherit.aes = TRUE) +
  
  guides(fill=FALSE, alpha=FALSE, size=FALSE) +
  
  xlab("Longitude")+ ylab("Latitude") +
  
  theme_classic() + 
  
  theme(legend.position = "none", panel.grid.major = element_line(colour = gray(.5), linetype = "dashed", size = 0.5),panel.background = element_rect(fill = "aliceblue"), panel.border=element_rect(fill=NA))

windows()
plot(mapplot)




windows()
ggplot()+
  #geom_sf(data=map1, fill="antiquewhite1") +
  #coord_sf(xlim = c(-83.20, -83.0), ylim = c(29.20, 29.40), expand=FALSE ) +
  geom_sf(data=oysterbeds2010v2)

oysterbeds2010v2<- sf::st_as_sf(oysterbeds2010)
plot(oysterbeds2010)
str(oysterbeds2010v2)
head(oysterbeds2010v2)
