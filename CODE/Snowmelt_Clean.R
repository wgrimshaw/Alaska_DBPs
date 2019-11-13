library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(viridis)



#Load the data
AlaskaTempPrecipDischarge <- read.csv("./DATA/PROCESSED/AlaskaTempPrecipDischarge.csv")

#Convert DATE column to class 'Date'
AlaskaTempPrecipDischarge$DATE <- ymd(AlaskaTempPrecipDischarge$DATE)

#Add date columns
Snowmelt.Discharge <- AlaskaTempPrecipDischarge %>% mutate(YEAR = year(DATE), MONTH = month(DATE), 
                        DAY = day(DATE), WEEK = week(DATE), 
                        DOY = yday(DATE))

Snowmelt.Discharge.Spring <- Snowmelt.Discharge %>% filter(WEEK < 30)


#Make dataframes for bin analysis
#Filter for only needed columns and bin
Snowmelt.Discharge.Bin1 <- Snowmelt.Discharge.Spring %>% select(DATE, YEAR, MONTH, DAY, WEEK, DOY, Bin, NAME, LATITUDE, LONGITUDE, Discharge) %>%
  filter(Bin == 1)

Snowmelt.Discharge.Bin2 <- Snowmelt.Discharge.Spring %>% select(DATE, YEAR, MONTH, DAY, WEEK, DOY, Bin, NAME, LATITUDE, LONGITUDE, Discharge) %>%
  filter(Bin == 2)

Snowmelt.Discharge.Bin3 <- Snowmelt.Discharge.Spring %>% select(DATE, YEAR, MONTH, DAY, WEEK, DOY, Bin, NAME, LATITUDE, LONGITUDE, Discharge) %>%
  filter(Bin == 3)

Snowmelt.Discharge.Bin4 <- Snowmelt.Discharge.Spring %>% select(DATE, YEAR, MONTH, DAY, WEEK, DOY, Bin, NAME, LATITUDE, LONGITUDE, Discharge) %>%
  filter(Bin == 4)

Snowmelt.Discharge.Bin5 <- Snowmelt.Discharge.Spring %>% select(DATE, YEAR, MONTH, DAY, WEEK, DOY, Bin, NAME, LATITUDE, LONGITUDE, Discharge) %>%
  filter(Bin == 5)

Snowmelt.Discharge.Bin6 <- Snowmelt.Discharge.Spring %>% select(DATE, YEAR, MONTH, DAY, WEEK, DOY, Bin, NAME, LATITUDE, LONGITUDE, Discharge) %>%
  filter(Bin == 6)

Snowmelt.Discharge.Bin7 <- Snowmelt.Discharge.Spring %>% select(DATE, YEAR, MONTH, DAY, WEEK, DOY, Bin, NAME, LATITUDE, LONGITUDE, Discharge) %>%
  filter(Bin == 7)

Snowmelt.Discharge.Bin8 <- Snowmelt.Discharge.Spring %>% select(DATE, YEAR, MONTH, DAY, WEEK, DOY, Bin, NAME, LATITUDE, LONGITUDE, Discharge) %>%
  filter(Bin == 8)

Snowmelt.Discharge.Bin9 <- Snowmelt.Discharge.Spring %>% select(DATE, YEAR, MONTH, DAY, WEEK, DOY, Bin, NAME, LATITUDE, LONGITUDE, Discharge) %>%
  filter(Bin == 9)

#Plot mean discharge for each DOY through time by bin
Snowmelt.Discharge.Mean_TT <- Snowmelt.Discharge.Spring %>% group_by(as.factor(Bin), as.factor(DOY)) %>%
  mutate(Dis.Mean = mean(Discharge))

dis.doy.mean <- ggplot(Snowmelt.Discharge.Mean) +
  geom_line(aes(x = DOY, y = Dis.Mean, color = as.factor(Bin))) +
  labs(x = "Day of Year", y = "Mean Discharge", color = "Latitude Bin") +
  scale_color_viridis_d() +
  theme(legend.position = "top")
print(dis.doy.mean)


#Latitude Bin 6 
dis.doy.6 <- ggplot(Snowmelt.Discharge.Bin6) +
  geom_line(aes(x = DOY, y = Discharge, color = (YEAR)), 
            show.legend = FALSE) +
  labs(x = "Day of Year", y = "Discharge, cfs", color = "Year") +
  scale_color_viridis_c() +
  theme(legend.position = "top")
print(dis.doy.6)


#Latitude Bin 6 - discrete years - every 10
Snowmelt.Discharge.Bin6.10Year <- Snowmelt.Discharge.Bin6 %>% 
  filter(YEAR == 1969 | YEAR == 1979 | YEAR == 1989 | YEAR == 1999 | YEAR == 2009 | YEAR == 2019)


dis.doy.6.10 <- ggplot(Snowmelt.Discharge.Bin6.10Year) +
  geom_line(aes(x = DOY, y = Discharge, color = as.factor((YEAR)))) +
  labs(x = "Day of Year", y = "Discharge, cfs", color = "Year") +
  scale_color_viridis_d() +
  theme(legend.position = "top")
print(dis.doy.6.10)

##Year vs DOY
#dis.year.doy <- 
#  ggplot(Snowmelt.Discharge.Spring, aes(x = YEAR, y = MONTH, height = Discharge)) +
#  geom_density_ridges_gradient(stat = "identity", scale = 1) +
#  labs(x = "Year", y = "Day of Year", height = "Discharge") +
#  scale_color_viridis_c() +
#  theme(legend.position = "top")
#print(dis.year.doy)
#
