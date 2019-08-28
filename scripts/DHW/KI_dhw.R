
###################################################################################
################# CALCULATE DHW FOR ALL SITES AND REGIONS #########################
###################################################################################
# Clear R environment
rm(list=ls())
# Load necessary packages
library(zoo)
library(lubridate)

# Load hourly temperature dataset (2014-2016)
NOAA.temp<-read.csv(file="data/environmental/Temp data/annual.temps.csv")
df<-NOAA.temp[-1] #remove column

## Create startDate and endDate
# The first date that we want to interpolate from (started 2014-1-1 for ease later on)
startDate <- as.POSIXct("2014-01-01 00:00:00",format="%Y-%m-%d %H:%M:%S", tz="HST")

# The last date that we want to interpolate to (ended 2016-01-01 for ease later on)
endDate <- as.POSIXct("2016-01-01 00:00:00",format="%Y-%m-%d %H:%M:%S", tz="HST")

# Create a time vector (xi3) sampled to half week (for calculation of DHW)
xi3 <- (seq(from=startDate, to= endDate, by=302400))

# Subtract 1 day from xi3 to make half weekly calculations work
xi3 <- xi3[1:length(xi3)-1]

# Set the "Base" temperature (i.e. the mean monthly maximum, from NOAA) # 
# 27.7? (https://www.ndbc.noaa.gov/view_climplot.php?station=mokh1&meas=st) 2008-2012 maximum summer mean and Jokiel and Brown
base<-27.7

df$hour <- strftime(df$date.time, format="%H:00:00", breaks="hour")

hour.temp.means <- aggregate(Water.temp ~ hour+Date, df, mean)
hour.temp.means<- hour.temp.means %>%
  select(c(Date, hour, Water.temp))

head(hour.temp.means) # hourly means over time

## Now collapse to half-weekly temperature
# Create temperary tempmatrix
tempmatrix <- site3_1hr$temperature_1hr
# Truncate tempmatrix to allow for round half weekly calculations
tempmatrix <- tempmatrix[1:(floor(length(tempmatrix)/84)*84)]
# Reshape tempmatrix to be able to calculate half weekly temperature
dim(tempmatrix) = c(84,length(tempmatrix)/84)
# Calculate the mean of each half weekly temperature range
temperature_halfwk <- colMeans(tempmatrix,na.rm=TRUE)
# Bind together the time vector (xi3) with the half-weekly temperature
site3_halfwk<-cbind.data.frame(xi3,temperature_halfwk)
# Remove temperature_halfwk to avoid confusion later on
rm(temperature_halfwk)


###################################################################################
## Now calculate the hotspot for all sites
# Create a dataframe including time vector xi3
site3_hotspot <- data.frame(xi3)
# Create "hotspot" for ease of doing multiple sites
hotspot<-site3_hotspot$hotspot
# Assign hotspot value, this is (halfweekly temperature) - (base temperature)
hotspot<-site3_halfwk$temperature_halfwk-base
# Only keep hotspot temperature if the hotspot is >1 (for calculation of DHW)
hotspot<-ifelse(hotspot<1,hotspot==0,hotspot)
# Bind together the time vector and the hotspot vector
site3_hotspot <- cbind.data.frame(site3_hotspot,hotspot)
# Remove hotspot to avoid confusion later on
rm(hotspot)

###################################################################################
## Now calculate the Degree Heating Week (DHW) ##
# Set the window for the rolling sum (this is 24 half weeks = summing over 12 weeks)
k=24

# Create a dataframe including the time vector xi3
site3_DHW <- data.frame(xi3)
# Calculate the DHW using rollapply, summing across a 12 week window (k=24 half weeks). Multiply value by 0.5 to get DHW
site3_DHW$DHW<-(rollapply(site3_hotspot$hotspot, width=k, by=1, sum, fill=c(NA,NA,NA), align="right",partial=FALSE)*0.5)

