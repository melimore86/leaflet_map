library("tidyverse")
library("lubridate")
library("waterData")
library("dplyr")
library("ggplot2")

source("script/dailyFunctions.R")

# Sensor water quality carpentry
wq <- read.csv("rmd/data/wq.csv", header= T) %>%
  filter(Site != 0)
wq$Date <- wq$Date %>% ymd_hms %>% round_date(unit="hour")
wq$Site <- factor(wq$Site, levels = c("6", "1", "7", "5", "2", "8","4", "3", "9"))

# Water discharge carpentry (dynamically updating local discharge file)
station = '02323500' 
stinfo  = siteInfo(station)
dis <- read_rds("data/dis.rds")
if (max(dis$dates) < (Sys.Date() - 5)) {
  sdate <- max(dis$dates) + 1
  newdis <- importDVs(staid = station, code = '00060', stat = '00003', 
                      sdate= as.character(sdate))
  dis <- bind_rows(dis, newdis) %>%
    distinct() # In case there's repeated rows due to code failure
  write_rds(dis, "data/dis.rds")
}

# Define start and end date of the plot
startDate <- ymd_hms("2017-08-01 00:00:00")
endDate <- ymd_hms("2018-07-30 23:00:00")

# Create sequence of dates (so that NA is recognized by geom_line) and build a table of
# salinity and temperature (remove other irrelevant ones)
d <- seq(startDate, endDate, by = "hour")
sal_temp <- expand(wq, Site, Date=d) %>%
  left_join(wq, by=c("Site" = "Site", "Date" = "Date")) %>%
  select(Site, Date, Salinity, Temperature)
sal_temp$d2 <- date(sal_temp$Date)

dsal <- seq(startDate, endDate, by = "hour")
sal_only <- expand(wq, Site, Date=dsal) %>%
  left_join(wq, by=c("Site" = "Site", "Date" = "Date")) %>%
  select(Site, Date, Salinity)
sal_only$dsal2 <- date(sal_only$Date)

dtemp <- seq(startDate, endDate, by = "hour")
temp_only <- expand(wq, Site, Date=dtemp) %>%
  left_join(wq, by=c("Site" = "Site", "Date" = "Date")) %>%
  select(Site, Date, Temperature)
temp_only$dtemp2 <- date(temp_only$Date)

# Calculate mean, min and max daily Sal and Temp
# Note: tidyr's gather is in use here, by converting the "wide" table to
# "long" one, we can use the aes(color=Measure) in ggplot
# Note2: I've created functions in dailyFunction.R to make calculating daily mean, min,
# max easy. The function allows certain proportions of NA here. In this example, 0.75
# of the daily numbers can be NA, and we'll still calculate the meant, min, max
sal_temp_summ <- sal_temp %>%
  gather(key = "Measure", value = "Value", Salinity, Temperature) %>%
  group_by(Site, d2, Measure) %>%
  summarise(meanVal = dailyMean(Value, 0.75), 
            minVal = dailyMin(Value, 0.75), 
            maxVal = dailyMax(Value, 0.75)) %>%
  ungroup()


sal_summ <- sal_only %>%
  filter(Site %in% c("6","5","4"))%>%
  gather(key = "Measure", value = "Value", Salinity) %>%
  group_by(Site, dsal2, Measure) %>%
  summarise(meanVal = dailyMean(Value, 0.75), 
            minVal = dailyMin(Value, 0.75), 
            maxVal = dailyMax(Value, 0.75)) %>%
  ungroup()


temp_summ <- temp_only %>%
  filter(Site %in% c("6","5","4"))%>%
  gather(key = "Measure", value = "Value", Temperature) %>%
  group_by(Site, dtemp2, Measure) %>%
  summarise(meanVal = dailyMean(Value, 0.75), 
            minVal = dailyMin(Value, 0.75), 
            maxVal = dailyMax(Value, 0.75)) %>%
  ungroup()

# Water discharge data manipulation, filter out the irrelevant dates
dis2 <- dis %>%
  mutate(Datetime = paste(dates, " 12:00:00") %>% ymd_hms()) %>%
  filter(Datetime >= startDate & Datetime <= endDate)

# First step to specify the ribbons and line and facet.
# Note here for the water discharge data I've added a -20 to the ymax and ymin, this pushes 
# the water discharge graph down to the region of -20 to 0, help to declutter
# Second step is to specify the scaling of x and y axes, by using the breaks argument
# we can control what to display on the y main and secondary axes, e.g. 0 to 40 for main,
# and 0 to 20000 for secondary
#tempplot<- 

  ggplot() +
  geom_line(data=temp_summ, aes(x=dtemp2, y=meanVal, color=Measure), size= 1.1) +
  geom_ribbon(data=temp_summ, aes(x=dtemp2, ymax=maxVal, ymin=minVal, fill=Measure), 
              alpha=0.4) +
    geom_ribbon(data=dis2, aes(x=dates, ymax=val/1000 - 20, ymin=-20, group= "River Discharge"), fill="#56B4E9",
                alpha=0.6) +
  xlab("Date") +
  scale_y_continuous(name = "Temperature (C)
                       ", 
                     limits=c(-20,40), 
                     breaks = seq(0, 40, 10),
                     sec.axis = sec_axis(~(.+20), 
                                         name = "River Discharge (1,000 cfs)
                                           ",
                                         breaks = seq(0, 20, 10))) +
  scale_x_date(date_breaks = "2 month", date_labels = "%Y/%m", expand = c(0, 0)) +
  
  scale_fill_manual(values = c("#D55E00")) +
  
  scale_color_manual(values = c("#D55E00")) +
  
  theme(legend.position=("top"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", size = 1, fill = NA, linetype="solid"),
        axis.text=element_text(size=10),
        axis.title=element_text(size=13,face="bold"),
        plot.title =element_text(size=13, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1, face= "bold"),
        axis.text.y = element_text(face= "bold"),
        legend.title=element_blank()) +
  facet_wrap(~ Site, ncol=1)




  ggplot() +
  geom_ribbon(data=dis2, aes(x=dates, ymax=val/1000 - 20, ymin=-20, group="Discharge"), fill="#56B4E9",
              alpha=0.4) +
  geom_line(data=sal_summ, aes(x=dsal2, y=meanVal, color=Measure), size= 1.1) +
  geom_ribbon(data=sal_summ, aes(x=dsal2, ymax=maxVal, ymin=minVal, fill=Measure), 
              alpha=0.6) +
  xlab("Date") +
  scale_y_continuous(name = "Salinity (ppt)", 
                     limits=c(-20,40), 
                     breaks = seq(0, 40, 10),
                     sec.axis = sec_axis(~(.+20), 
                                         name = "River Discharge (1,000 cfs)",
                                         breaks = seq(0, 20, 10))) +
  scale_x_date(date_breaks = "2 month", date_labels = "%Y/%m", expand = c(0, 0)) +
  
  scale_fill_manual(values = c("#0072B2")) +
  
  scale_color_manual(values = c("#0072B2")) +
  
  theme(legend.position=("top"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", size = 1, fill = NA, linetype="solid"),
        axis.text=element_text(size=10),
        axis.title=element_text(size=13,face="bold"),
        plot.title =element_text(size=13, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.title=element_blank()) +
  facet_wrap(~ Site, ncol=1)









