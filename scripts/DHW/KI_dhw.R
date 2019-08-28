###################################################################################
################# CALCULATE DHW FOR ALL SITES AND REGIONS #########################
###################################################################################
# Clear R environment
rm(list=ls())
# Load necessary packages
library(zoo)
# Set working directory
setwd("/Users/Dani/Documents/GitHub/ki-env/SST/")
# Load hourly temperature dataset
load(file="Data/merged/KI_SB_temp_1hr.RData")
## Create startDate and endDate
# The first date that we want to interpolate from (started 2011-1-1 for ease later on)
startDate <- as.POSIXct("2011/1/1 00:00:00",format="%Y/%m/%d %H:%M:%S",tz="Pacific/Kiritimati")
# The last date that we want to interpolate to (ended 2016-12-31 for ease later on)
endDate <- as.POSIXct("2016/12/31 23:59:00",format="%Y/%m/%d %H:%M:%S",tz="Pacific/Kiritimati")
# Create a site list for future reference
sitelist <- c("site3","site5","site8","site8.5","site9","site15","site19","site25","site27","site30","site32","site33","site34","site35","site40")
# Create a time vector (xi3) sampled to half week (for calculation of DHW)
xi3 <- (seq(from=startDate, to= endDate, by=302400))
# Subtract 1 day from xi3 to make half weekly calculations work
xi3 <- xi3[1:length(xi3)-1]
# Set the "Base" temperature (i.e. the mean monthly maximum, from NOAA)
base<-28.1366

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
# Do this for all sites

tempmatrix <- site5_1hr$temperature_1hr
tempmatrix <- tempmatrix[1:(floor(length(tempmatrix)/84)*84)]
dim(tempmatrix) = c(84,length(tempmatrix)/84)
temperature_halfwk <- colMeans(tempmatrix,na.rm=TRUE)
site5_halfwk<-cbind.data.frame(xi3,temperature_halfwk)
rm(temperature_halfwk)

tempmatrix <- site8_1hr$temperature_1hr
tempmatrix <- tempmatrix[1:(floor(length(tempmatrix)/84)*84)]
dim(tempmatrix) = c(84,length(tempmatrix)/84)
temperature_halfwk <- colMeans(tempmatrix,na.rm=TRUE)
site8_halfwk<-cbind.data.frame(xi3,temperature_halfwk)
rm(temperature_halfwk)

tempmatrix <- site9_1hr$temperature_1hr
tempmatrix <- tempmatrix[1:(floor(length(tempmatrix)/84)*84)]
dim(tempmatrix) = c(84,length(tempmatrix)/84)
temperature_halfwk <- colMeans(tempmatrix,na.rm=TRUE)
site9_halfwk<-cbind.data.frame(xi3,temperature_halfwk)
rm(temperature_halfwk)

tempmatrix <- site15_1hr$temperature_1hr
tempmatrix <- tempmatrix[1:(floor(length(tempmatrix)/84)*84)]
dim(tempmatrix) = c(84,length(tempmatrix)/84)
temperature_halfwk <- colMeans(tempmatrix,na.rm=TRUE)
site15_halfwk<-cbind.data.frame(xi3,temperature_halfwk)
rm(temperature_halfwk)

tempmatrix <- site19_1hr$temperature_1hr
tempmatrix <- tempmatrix[1:(floor(length(tempmatrix)/84)*84)]
dim(tempmatrix) = c(84,length(tempmatrix)/84)
temperature_halfwk <- colMeans(tempmatrix,na.rm=TRUE)
site19_halfwk<-cbind.data.frame(xi3,temperature_halfwk)
rm(temperature_halfwk)

tempmatrix <- site25_1hr$temperature_1hr
tempmatrix <- tempmatrix[1:(floor(length(tempmatrix)/84)*84)]
dim(tempmatrix) = c(84,length(tempmatrix)/84)
temperature_halfwk <- colMeans(tempmatrix,na.rm=TRUE)
site25_halfwk<-cbind.data.frame(xi3,temperature_halfwk)
rm(temperature_halfwk)

tempmatrix <- site27_1hr$temperature_1hr
tempmatrix <- tempmatrix[1:(floor(length(tempmatrix)/84)*84)]
dim(tempmatrix) = c(84,length(tempmatrix)/84)
temperature_halfwk <- colMeans(tempmatrix,na.rm=TRUE)
site27_halfwk<-cbind.data.frame(xi3,temperature_halfwk)
rm(temperature_halfwk)

tempmatrix <- site30_1hr$temperature_1hr
tempmatrix <- tempmatrix[1:(floor(length(tempmatrix)/84)*84)]
dim(tempmatrix) = c(84,length(tempmatrix)/84)
temperature_halfwk <- colMeans(tempmatrix,na.rm=TRUE)
site30_halfwk<-cbind.data.frame(xi3,temperature_halfwk)
rm(temperature_halfwk)

tempmatrix <- site32_1hr$temperature_1hr
tempmatrix <- tempmatrix[1:(floor(length(tempmatrix)/84)*84)]
dim(tempmatrix) = c(84,length(tempmatrix)/84)
temperature_halfwk <- colMeans(tempmatrix,na.rm=TRUE)
site32_halfwk<-cbind.data.frame(xi3,temperature_halfwk)
rm(temperature_halfwk)

tempmatrix <- site33_1hr$temperature_1hr
tempmatrix <- tempmatrix[1:(floor(length(tempmatrix)/84)*84)]
dim(tempmatrix) = c(84,length(tempmatrix)/84)
temperature_halfwk <- colMeans(tempmatrix,na.rm=TRUE)
site33_halfwk<-cbind.data.frame(xi3,temperature_halfwk)
rm(temperature_halfwk)

tempmatrix <- site34_1hr$temperature_1hr
tempmatrix <- tempmatrix[1:(floor(length(tempmatrix)/84)*84)]
dim(tempmatrix) = c(84,length(tempmatrix)/84)
temperature_halfwk <- colMeans(tempmatrix,na.rm=TRUE)
site34_halfwk<-cbind.data.frame(xi3,temperature_halfwk)
rm(temperature_halfwk)

tempmatrix <- site35_1hr$temperature_1hr
tempmatrix <- tempmatrix[1:(floor(length(tempmatrix)/84)*84)]
dim(tempmatrix) = c(84,length(tempmatrix)/84)
temperature_halfwk <- colMeans(tempmatrix,na.rm=TRUE)
site35_halfwk<-cbind.data.frame(xi3,temperature_halfwk)
rm(temperature_halfwk)

tempmatrix <- site40_1hr$temperature_1hr
tempmatrix <- tempmatrix[1:(floor(length(tempmatrix)/84)*84)]
dim(tempmatrix) = c(84,length(tempmatrix)/84)
temperature_halfwk <- colMeans(tempmatrix,na.rm=TRUE)
site40_halfwk<-cbind.data.frame(xi3,temperature_halfwk)
rm(temperature_halfwk)

# Now do for all regions
tempmatrix <- southlagoon_1hr$temperature_1hr
tempmatrix <- tempmatrix[1:(floor(length(tempmatrix)/84)*84)]
dim(tempmatrix) = c(84,length(tempmatrix)/84)
temperature_halfwk <- colMeans(tempmatrix,na.rm=TRUE)
southlagoon_halfwk<-cbind.data.frame(xi3,temperature_halfwk)
rm(temperature_halfwk)

tempmatrix <- northlagoon_1hr$temperature_1hr
tempmatrix <- tempmatrix[1:(floor(length(tempmatrix)/84)*84)]
dim(tempmatrix) = c(84,length(tempmatrix)/84)
temperature_halfwk <- colMeans(tempmatrix,na.rm=TRUE)
northlagoon_halfwk<-cbind.data.frame(xi3,temperature_halfwk)
rm(temperature_halfwk)

tempmatrix <- lagoonface_1hr$temperature_1hr
tempmatrix <- tempmatrix[1:(floor(length(tempmatrix)/84)*84)]
dim(tempmatrix) = c(84,length(tempmatrix)/84)
temperature_halfwk <- colMeans(tempmatrix,na.rm=TRUE)
lagoonface_halfwk<-cbind.data.frame(xi3,temperature_halfwk)
rm(temperature_halfwk)

tempmatrix <- northshore_1hr$temperature_1hr
tempmatrix <- tempmatrix[1:(floor(length(tempmatrix)/84)*84)]
dim(tempmatrix) = c(84,length(tempmatrix)/84)
temperature_halfwk <- colMeans(tempmatrix,na.rm=TRUE)
northshore_halfwk<-cbind.data.frame(xi3,temperature_halfwk)
rm(temperature_halfwk)

tempmatrix <- bayofwrecks_1hr$temperature_1hr
tempmatrix <- tempmatrix[1:(floor(length(tempmatrix)/84)*84)]
dim(tempmatrix) = c(84,length(tempmatrix)/84)
temperature_halfwk <- colMeans(tempmatrix,na.rm=TRUE)
bayofwrecks_halfwk<-cbind.data.frame(xi3,temperature_halfwk)
rm(temperature_halfwk)

tempmatrix <- vaskesbay_1hr$temperature_1hr
tempmatrix <- tempmatrix[1:(floor(length(tempmatrix)/84)*84)]
dim(tempmatrix) = c(84,length(tempmatrix)/84)
temperature_halfwk <- colMeans(tempmatrix,na.rm=TRUE)
vaskesbay_halfwk<-cbind.data.frame(xi3,temperature_halfwk)
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
# Do this for all sites

site5_hotspot <- data.frame(xi3)
hotspot<-site5_hotspot$hotspot
hotspot<-site5_halfwk$temperature_halfwk-base
hotspot<-ifelse(hotspot<1,hotspot==0,hotspot)
site5_hotspot <- cbind.data.frame(site5_hotspot,hotspot)
rm(hotspot)

site8_hotspot <- data.frame(xi3)
hotspot<-site8_hotspot$hotspot
hotspot<-site8_halfwk$temperature_halfwk-base
hotspot<-ifelse(hotspot<1,hotspot==0,hotspot)
site8_hotspot <- cbind.data.frame(site8_hotspot,hotspot)
rm(hotspot)

site8.5_hotspot <- data.frame(xi3)
hotspot<-site8.5_hotspot$hotspot
hotspot<-site8.5_halfwk$temperature_halfwk-base
hotspot<-ifelse(hotspot<1,hotspot==0,hotspot)
site8.5_hotspot <- cbind.data.frame(site8.5_hotspot,hotspot)
rm(hotspot)

site9_hotspot <- data.frame(xi3)
hotspot<-site9_hotspot$hotspot
hotspot<-site9_halfwk$temperature_halfwk-base
hotspot<-ifelse(hotspot<1,hotspot==0,hotspot)
site9_hotspot <- cbind.data.frame(site9_hotspot,hotspot)
rm(hotspot)

site15_hotspot <- data.frame(xi3)
hotspot<-site15_hotspot$hotspot
hotspot<-site15_halfwk$temperature_halfwk-base
hotspot<-ifelse(hotspot<1,hotspot==0,hotspot)
site15_hotspot <- cbind.data.frame(site15_hotspot,hotspot)
rm(hotspot)

site19_hotspot <- data.frame(xi3)
hotspot<-site19_hotspot$hotspot
hotspot<-site19_halfwk$temperature_halfwk-base
hotspot<-ifelse(hotspot<1,hotspot==0,hotspot)
site19_hotspot <- cbind.data.frame(site19_hotspot,hotspot)
rm(hotspot)

site25_hotspot <- data.frame(xi3)
hotspot<-site25_hotspot$hotspot
hotspot<-site25_halfwk$temperature_halfwk-base
hotspot<-ifelse(hotspot<1,hotspot==0,hotspot)
site25_hotspot <- cbind.data.frame(site25_hotspot,hotspot)
rm(hotspot)

site27_hotspot <- data.frame(xi3)
hotspot<-site27_hotspot$hotspot
hotspot<-site27_halfwk$temperature_halfwk-base
hotspot<-ifelse(hotspot<1,hotspot==0,hotspot)
site27_hotspot <- cbind.data.frame(site27_hotspot,hotspot)
rm(hotspot)

site30_hotspot <- data.frame(xi3)
hotspot<-site30_hotspot$hotspot
hotspot<-site30_halfwk$temperature_halfwk-base
hotspot<-ifelse(hotspot<1,hotspot==0,hotspot)
site30_hotspot <- cbind.data.frame(site30_hotspot,hotspot)
rm(hotspot)

site32_hotspot <- data.frame(xi3)
hotspot<-site32_hotspot$hotspot
hotspot<-site32_halfwk$temperature_halfwk-base
hotspot<-ifelse(hotspot<1,hotspot==0,hotspot)
site32_hotspot <- cbind.data.frame(site32_hotspot,hotspot)
rm(hotspot)

site33_hotspot <- data.frame(xi3)
hotspot<-site33_hotspot$hotspot
hotspot<-site33_halfwk$temperature_halfwk-base
hotspot<-ifelse(hotspot<1,hotspot==0,hotspot)
site33_hotspot <- cbind.data.frame(site33_hotspot,hotspot)
rm(hotspot)

site34_hotspot <- data.frame(xi3)
hotspot<-site34_hotspot$hotspot
hotspot<-site34_halfwk$temperature_halfwk-base
hotspot<-ifelse(hotspot<1,hotspot==0,hotspot)
site34_hotspot <- cbind.data.frame(site34_hotspot,hotspot)
rm(hotspot)

site35_hotspot <- data.frame(xi3)
hotspot<-site35_hotspot$hotspot
hotspot<-site35_halfwk$temperature_halfwk-base
hotspot<-ifelse(hotspot<1,hotspot==0,hotspot)
site35_hotspot <- cbind.data.frame(site35_hotspot,hotspot)
rm(hotspot)

site40_hotspot <- data.frame(xi3)
hotspot<-site40_hotspot$hotspot
hotspot<-site40_halfwk$temperature_halfwk-base
hotspot<-ifelse(hotspot<1,hotspot==0,hotspot)
site40_hotspot <- cbind.data.frame(site40_hotspot,hotspot)
rm(hotspot)

# Now do this for each region
southlagoon_hotspot <- data.frame(xi3)
hotspot<-southlagoon_hotspot$hotspot
hotspot<-southlagoon_halfwk$temperature_halfwk-base
hotspot<-ifelse(hotspot<1,hotspot==0,hotspot)
southlagoon_hotspot <- cbind.data.frame(southlagoon_hotspot,hotspot)
rm(hotspot)

northlagoon_hotspot <- data.frame(xi3)
hotspot<-northlagoon_hotspot$hotspot
hotspot<-northlagoon_halfwk$temperature_halfwk-base
hotspot<-ifelse(hotspot<1,hotspot==0,hotspot)
northlagoon_hotspot <- cbind.data.frame(northlagoon_hotspot,hotspot)
rm(hotspot)

lagoonface_hotspot <- data.frame(xi3)
hotspot<-lagoonface_hotspot$hotspot
hotspot<-lagoonface_halfwk$temperature_halfwk-base
hotspot<-ifelse(hotspot<1,hotspot==0,hotspot)
lagoonface_hotspot <- cbind.data.frame(lagoonface_hotspot,hotspot)
rm(hotspot)

northshore_hotspot <- data.frame(xi3)
hotspot<-northshore_hotspot$hotspot
hotspot<-northshore_halfwk$temperature_halfwk-base
hotspot<-ifelse(hotspot<1,hotspot==0,hotspot)
northshore_hotspot <- cbind.data.frame(northshore_hotspot,hotspot)
rm(hotspot)

bayofwrecks_hotspot <- data.frame(xi3)
hotspot<-bayofwrecks_hotspot$hotspot
hotspot<-bayofwrecks_halfwk$temperature_halfwk-base
hotspot<-ifelse(hotspot<1,hotspot==0,hotspot)
bayofwrecks_hotspot <- cbind.data.frame(bayofwrecks_hotspot,hotspot)
rm(hotspot)

vaskesbay_hotspot <- data.frame(xi3)
hotspot<-vaskesbay_hotspot$hotspot
hotspot<-vaskesbay_halfwk$temperature_halfwk-base
hotspot<-ifelse(hotspot<1,hotspot==0,hotspot)
vaskesbay_hotspot <- cbind.data.frame(vaskesbay_hotspot,hotspot)
rm(hotspot)

###################################################################################
## Now calculate the Degree Heating Week (DHW) ##
# Set the window for the rolling sum (this is 24 half weeks = summing over 12 weeks)
k=24

# Create a dataframe including the time vector xi3
site3_DHW <- data.frame(xi3)
# Calculate the DHW using rollapply, summing across a 12 week window (k=24 half weeks). Multiply value by 0.5 to get DHW
site3_DHW$DHW<-(rollapply(site3_hotspot$hotspot, width=k, by=1, sum, fill=c(NA,NA,NA), align="right",partial=FALSE)*0.5)
# Do this for all sites

site5_DHW <- data.frame(xi3)
site5_DHW$DHW<-(rollapply(site5_hotspot$hotspot, width=k, by=1, sum, fill=c(NA,NA,NA), align="right",partial=FALSE)*0.5)

site8_DHW <- data.frame(xi3)
site8_DHW$DHW<-(rollapply(site8_hotspot$hotspot, width=k, by=1, sum, fill=c(NA,NA,NA), align="right",partial=FALSE)*0.5)

site9_DHW <- data.frame(xi3)
site9_DHW$DHW<-(rollapply(site9_hotspot$hotspot, width=k, by=1, sum, fill=c(NA,NA,NA), align="right",partial=FALSE)*0.5)

site15_DHW <- data.frame(xi3)
site15_DHW$DHW<-(rollapply(site15_hotspot$hotspot, width=k, by=1, sum, fill=c(NA,NA,NA), align="right",partial=FALSE)*0.5)

site19_DHW <- data.frame(xi3)
site19_DHW$DHW<-(rollapply(site19_hotspot$hotspot, width=k, by=1, sum, fill=c(NA,NA,NA), align="right",partial=FALSE)*0.5)

site25_DHW <- data.frame(xi3)
site25_DHW$DHW<-(rollapply(site25_hotspot$hotspot, width=k, by=1, sum, fill=c(NA,NA,NA), align="right",partial=FALSE)*0.5)

site27_DHW <- data.frame(xi3)
site27_DHW$DHW<-(rollapply(site27_hotspot$hotspot, width=k, by=1, sum, fill=c(NA,NA,NA), align="right",partial=FALSE)*0.5)

site30_DHW <- data.frame(xi3)
site30_DHW$DHW<-(rollapply(site30_hotspot$hotspot, width=k, by=1, sum, fill=c(NA,NA,NA), align="right",partial=FALSE)*0.5)

site32_DHW <- data.frame(xi3)
site32_DHW$DHW<-(rollapply(site32_hotspot$hotspot, width=k, by=1, sum, fill=c(NA,NA,NA), align="right",partial=FALSE)*0.5)

site33_DHW <- data.frame(xi3)
site33_DHW$DHW<-(rollapply(site33_hotspot$hotspot, width=k, by=1, sum, fill=c(NA,NA,NA), align="right",partial=FALSE)*0.5)

site34_DHW <- data.frame(xi3)
site34_DHW$DHW<-(rollapply(site34_hotspot$hotspot, width=k, by=1, sum, fill=c(NA,NA,NA), align="right",partial=FALSE)*0.5)

site35_DHW <- data.frame(xi3)
site35_DHW$DHW<-(rollapply(site35_hotspot$hotspot, width=k, by=1, sum, fill=c(NA,NA,NA), align="right",partial=FALSE)*0.5)

site40_DHW <- data.frame(xi3)
site40_DHW$DHW<-(rollapply(site40_hotspot$hotspot, width=k, by=1, sum, fill=c(NA,NA,NA), align="right",partial=FALSE)*0.5)

# Do this for all regions
southlagoon_DHW <- data.frame(xi3)
southlagoon_DHW$DHW<-(rollapply(southlagoon_hotspot$hotspot, width=k, by=1, sum, fill=c(NA,NA,NA), align="right",partial=FALSE)*0.5)

northlagoon_DHW <- data.frame(xi3)
northlagoon_DHW$DHW<-(rollapply(northlagoon_hotspot$hotspot, width=k, by=1, sum, fill=c(NA,NA,NA), align="right",partial=FALSE)*0.5)

lagoonface_DHW <- data.frame(xi3)
lagoonface_DHW$DHW<-(rollapply(lagoonface_hotspot$hotspot, width=k, by=1, sum, fill=c(NA,NA,NA), align="right",partial=FALSE)*0.5)

northshore_DHW <- data.frame(xi3)
northshore_DHW$DHW<-(rollapply(northshore_hotspot$hotspot, width=k, by=1, sum, fill=c(NA,NA,NA), align="right",partial=FALSE)*0.5)

bayofwrecks_DHW <- data.frame(xi3)
bayofwrecks_DHW$DHW<-(rollapply(bayofwrecks_hotspot$hotspot, width=k, by=1, sum, fill=c(NA,NA,NA), align="right",partial=FALSE)*0.5)

vaskesbay_DHW <- data.frame(xi3)
vaskesbay_DHW$DHW<-(rollapply(vaskesbay_hotspot$hotspot, width=k, by=1, sum, fill=c(NA,NA,NA), align="right",partial=FALSE)*0.5)

# Save all DHW data to an .RData file
save(site3_DHW,site5_DHW,site8_DHW,site9_DHW,site15_DHW,site19_DHW,site25_DHW,site27_DHW,site30_DHW,site32_DHW,site33_DHW,site34_DHW,site35_DHW,site40_DHW,southlagoon_DHW,northlagoon_DHW,lagoonface_DHW,northshore_DHW,bayofwrecks_DHW,vaskesbay_DHW,file="Data/merged/KI_SB_temp_DHW.RData")
