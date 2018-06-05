library("RMySQL")
library("DBI")
library("GPArotation")
library("ggplot2")
library("tidyverse")
library("marelac")

#Guide: http://www.ahschulz.de/2013/07/23/installing-rmysql-under-windows/

# Download the MySQL connector https://dev.mysql.com/downloads/connector/odbc/


con <- dbConnect(MySQL(),
                 user="LCRoysterproject", 
                 password="HLLV6Pske0vTzhIZfSya",
                 dbname="LCRoysterproject", 
                 host="ict-prod-hosting05.mysql.osg.ufl.edu", 
                 port= 3359)

# Listing all of the columns in the database
dbListTables(con)

wq <- dbReadTable(conn = con, name = 'lcroyster_buoyobservation')
deployment<- dbReadTable(conn = con, name = 'lcroyster_sensordeploy')

wq$date <- as.POSIXct(as.Date(wq$observation_datetime,origin= "1899-12-30"))

standard=42.914
wq$sal <- convert_RtoS(wq$conductivity_mS_cm/standard, 
                                t= wq$temperature_c, p= 0)

class(wq$observation_datetime)

windows()
ggplot(data= wq, aes( x= date, y= salinity_psu)) + geom_point() +
  facet_wrap (~sensor_id, scales= "free_y")
  