library(tidyverse)
library(dplyr)
library(stats)
library(ggplot2)
library(lubridate)
library(nationalparkcolors)
library(viridis)
library(cowplot)

##FOR GABY - MEDIAN DISCHARGE CODE 

data.spring.median <- data.spring %>% group_by(Bin, DOY) %>%
  mutate(Dis.Med = median(Discharge))

dis.doy.med <- ggplot(data.spring.median) +
  geom_line(aes(x = DOY, y = Dis.Med, color = as.factor(Bin))) +
  labs(x = "Day of Year", y = "Median Discharge") +
  scale_color_viridis_d() +
  theme(legend.position = "top")
print(dis.doy.med)


######################################

#Load the data
data <- read.csv("./DATA/PROCESSED/AlaskaTempPrecipDischarge.csv")

#Convert DATE column to class 'Date'
data$DATE <- ymd(data$DATE)

#Add date columns
data <- data %>% mutate(YEAR = year(DATE), MONTH = month(DATE), 
                        DAY = day(DATE), WEEK = week(DATE), 
                        DOY = yday(DATE))

data.spring <- data %>% filter(WEEK < 30)

data.year <- data.spring %>% filter(YEAR == 1960 | YEAR == 2000)

#Bin 1 Analysis
#Filter for only needed columns and bin
bin1 <- data.spring %>% select(DATE, YEAR, MONTH, DAY, WEEK, DOY, Bin, NAME, LATITUDE, LONGITUDE, Discharge) %>%
  filter(Bin == 1)

bin2 <- data.spring %>% select(DATE, YEAR, MONTH, DAY, WEEK, DOY, Bin, NAME, LATITUDE, LONGITUDE, Discharge) %>%
  filter(Bin == 2)

bin3 <- data.spring %>% select(DATE, YEAR, MONTH, DAY, WEEK, DOY, Bin, NAME, LATITUDE, LONGITUDE, Discharge) %>%
  filter(Bin == 3)

bin4 <- data.spring %>% select(DATE, YEAR, MONTH, DAY, WEEK, DOY, Bin, NAME, LATITUDE, LONGITUDE, Discharge) %>%
  filter(Bin == 4)

bin5 <- data.spring %>% select(DATE, YEAR, MONTH, DAY, WEEK, DOY, Bin, NAME, LATITUDE, LONGITUDE, Discharge) %>%
  filter(Bin == 5)

bin6 <- data.spring %>% select(DATE, YEAR, MONTH, DAY, WEEK, DOY, Bin, NAME, LATITUDE, LONGITUDE, Discharge) %>%
  filter(Bin == 6)

bin7 <- data.spring %>% select(DATE, YEAR, MONTH, DAY, WEEK, DOY, Bin, NAME, LATITUDE, LONGITUDE, Discharge) %>%
  filter(Bin == 7)

bin8 <- data.spring %>% select(DATE, YEAR, MONTH, DAY, WEEK, DOY, Bin, NAME, LATITUDE, LONGITUDE, Discharge) %>%
  filter(Bin == 8)

bin9 <- data.spring %>% select(DATE, YEAR, MONTH, DAY, WEEK, DOY, Bin, NAME, LATITUDE, LONGITUDE, Discharge) %>%
  filter(Bin == 9)

#Graph
#For all sites - DOY by Bin
dis.doy.bin <- ggplot(data.spring, aes(x = DOY, y = Discharge, color = as.factor(Bin))) +
  geom_point(alpha = 0.75) +
  labs(x = "Day of Year", y = "Discharge") +
  scale_color_brewer(palette = "Spectral", direction = -1) +
  theme(legend.position = "top")
print(dis.doy.bin)

dis.doy.bin <- ggplot(data.spring, aes(x = DOY, y = Discharge, color = as.factor(Bin))) +
  #geom_smooth() +
  geom_point(alpha = 0.75) +
  labs(x = "Day of Year", y = "Discharge") +
  scale_color_viridis_d(direction = 1) +
  theme(legend.position = "top")
print(dis.doy.bin)


#For all sites - DOY by Year
dis.doy.yr <- ggplot(data.spring, aes(x = DOY, y = Discharge, color = YEAR)) +
  geom_point(alpha = 0.75) +
  labs(x = "Day of Year", y = "Discharge") +
  scale_color_viridis_c(direction = 1) +
  theme(legend.position = "top")
print(dis.doy.yr)


#For all sites - DOY by Year - only 1920 and 200
dis.doy.yr <- ggplot(data.year, aes(x = DOY, y = Discharge, color = as.factor(YEAR))) +
  geom_point(alpha = 0.75) +
  labs(x = "Day of Year", y = "Discharge") +
  scale_color_manual(values = pal) +
  theme(legend.position = "top")
print(dis.doy.yr)

#For all sites - Week by Bin
dis.week.bin <- ggplot(data.spring, aes(x = WEEK, y = Discharge, color = as.factor(Bin))) +
  geom_point(alpha = 0.5) +
  labs(x = "Week of Year", y = "Discharge") +
  scale_color_brewer(palette="Spectral", direction = 1) +
  theme(legend.position = "top")
print(dis.week.bin)

#For all sites - Week by year - Spring season
dis.week.yr <- ggplot(data.spring, aes(x = WEEK, y = Discharge, color = YEAR)) +
  geom_point(alpha = 0.5) +
  labs(x = "Week of Year", y = "Discharge") +
  scale_color_viridis_c() +
  theme(legend.position = "top")
print(dis.week.yr)

#Bin 9
dis.week.9 <- ggplot(bin9, aes(x = WEEK, y = Discharge, color = YEAR)) +
  geom_point(alpha = 0.5) +
  labs(x = "Week of Year", y = "Discharge") +
  scale_color_viridis_c(direction = -1) +
  theme(legend.position = "top")
print(dis.week.9)

dis.doy.yr <- ggplot(data.spring) +
  geom_line(aes(x = DOY, y = Discharge, color = as.factor(Bin))) +
  labs(x = "Day of Year", y = "Discharge") +
  scale_color_viridis_d() +
  theme(legend.position = "top")
print(dis.doy.yr)

############

dis.doy.yr0 <- ggplot(data.spring) +
  geom_line(aes(x = DOY, y = Discharge, color = YEAR)) +
  labs(x = "Day of Year", y = "Discharge") +
  scale_color_viridis_c() +
  theme(legend.position = "top")
print(dis.doy.yr0)


data.spring.median <- data.spring %>% group_by(Bin, DOY) %>%
  mutate(Dis.Med = median(Discharge))
  
dis.doy.med <- ggplot(data.spring.median) +
  geom_line(aes(x = DOY, y = Dis.Med, color = as.factor(Bin))) +
  labs(x = "Day of Year", y = "Median Discharge") +
  scale_color_viridis_d() +
  theme(legend.position = "top")
print(dis.doy.med)

dis.doy.med0 <- ggplot(data.spring.median) +
  geom_line(aes(x = DOY, y = Dis.Med, color = as.factor(Bin))) +
  labs(x = "Day of Year", y = "Median Discharge") +
  scale_color_viridis_d() +
  theme(legend.position = "top") + 
  ylim(0, 1500)
print(dis.doy.med0)

############

dis.doy.yr1 <- ggplot(bin1) +
  geom_line(aes(x = DOY, y = Discharge, color = YEAR)) +
  labs(x = "Day of Year", y = "Discharge") +
  scale_color_viridis_c() +
  theme(legend.position = "top")
print(dis.doy.yr1)

dis.doy.yr2 <- ggplot(bin2) +
  geom_line(aes(x = DOY, y = Discharge, color = YEAR)) +
  labs(x = "Day of Year", y = "Discharge") +
  scale_color_viridis_c() +
  theme(legend.position = "top")
print(dis.doy.yr2)

dis.doy.yr3 <- ggplot(bin3) +
  geom_line(aes(x = DOY, y = Discharge, color = YEAR)) +
  labs(x = "Day of Year", y = "Discharge") +
  scale_color_viridis_c() +
  theme(legend.position = "top")
print(dis.doy.yr3)

dis.doy.yr4 <- ggplot(bin4) +
  geom_line(aes(x = DOY, y = Discharge, color = YEAR)) +
  labs(x = "Day of Year", y = "Discharge") +
  scale_color_viridis_c() +
  theme(legend.position = "top")
print(dis.doy.yr4)

dis.doy.yr5 <- ggplot(bin5) +
  geom_line(aes(x = DOY, y = Discharge, color = YEAR)) +
  labs(x = "Day of Year", y = "Discharge") +
  scale_color_viridis_c() +
  theme(legend.position = "top")
print(dis.doy.yr5)

#BIN 6 USE FOR TEST
dis.doy.yr6 <- ggplot(bin6) +
  geom_line(aes(x = DOY, y = Discharge, color = as.factor(YEAR)), 
            show.legend = FALSE) +
  labs(x = "Day of Year", y = "Discharge") +
  scale_color_viridis_d() +
  theme(legend.position = "top")
print(dis.doy.yr6)

#10 years interval
#Line with some error for each


#peak snowmelt versus first melt
#First ten-day period where we have double discharge than the first 50 
#30-day peak flow window


#Date of peak flow between 100-200

#First smooth the data
#Peak finder function  
#(pracma package findpeaks function)

#Year vs doy


dis.doy.yr7 <- ggplot(bin7) +
  geom_line(aes(x = DOY, y = Discharge, color = YEAR)) +
  labs(x = "Day of Year", y = "Discharge") +
  scale_color_viridis_c() +
  theme(legend.position = "top")
print(dis.doy.yr7)

dis.doy.yr8 <- ggplot(bin8) +
  geom_line(aes(x = DOY, y = Discharge, color = YEAR)) +
  labs(x = "Day of Year", y = "Discharge") +
  scale_color_viridis_c() +
  theme(legend.position = "top")
print(dis.doy.yr8)

dis.doy.yr9 <- ggplot(bin9) +
  geom_line(aes(x = DOY, y = Discharge, color = YEAR)) +
  labs(x = "Day of Year", y = "Discharge") +
  scale_color_viridis_c() +
  theme(legend.position = "top")
print(dis.doy.yr9)

dis.doy.yr1 <- ggplot(data.spring) +
  geom_line(aes(x = DOY, y = Discharge, color = as.factor(Bin))) +
  labs(x = "Day of Year", y = "Discharge") +
  scale_color_viridis_d() +
  theme(legend.position = "top")
print(dis.doy.yr1)