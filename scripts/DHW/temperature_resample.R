#################################################################################
# Collating and resampling temperature data from Kiritimati Island Sea-Bird SBE56
#################################################################################

# Set your working directory
setwd("/Users/Dani/Documents/GitHub/ki-env/SST/")
setwd("/Users/KristinaTietjen/Documents/Git_Hub/ki-env/SST/")
# Clear your R environment
rm(list=ls())
# Load all necessary packages
library(scales)
library(stringr)
library(ggplot2)
library(plyr)
library(iemisc)
# Set ggplot theme
theme_set(theme_bw())

###################################################################################
## Load all .csv files from "cleaned_DC" (download date <= July 2015)
# List all of the .csv
temp = list.files(path="Data/SB-deployed/cleaned_DC/",pattern="*.csv")
# For loop that, for every csv file in "temp", creates a renamed data frame
for (i in 1:length(temp)) assign((gsub("56-","",gsub(".csv","",temp[i]))), read.csv(paste("Data/SB-deployed/cleaned_DC/",temp[i],sep="")))

SBE00637b_35 <- subset(SBE00637b_35,select=-c(Sample.Number))

# Create column "DateTime" that includes both "Date" and "Time" in POSIXct format
# FOR ALL TEMPERATURE RECORDS (this means that there are >1 record for many sites)
# Site 3
SBE00655_3$DateTime <- as.POSIXct(paste(SBE00655_3$Date,SBE00655_3$Time),format="%y/%m/%d %H:%M:%S",tz="Pacific/Kiritimati")
# Site 5
SBE04844_5$DateTime <- as.POSIXct(paste(SBE04844_5$Date,SBE04844_5$Time),format="%y/%m/%d %H:%M:%S",tz="Pacific/Kiritimati")
# Site 8.5
SBE0652_8.5$DateTime <- as.POSIXct(paste(SBE0652_8.5$Date,SBE0652_8.5$Time),format="%y/%m/%d %H:%M:%S",tz="Pacific/Kiritimati")
# Site 8
SBE04841_8$DateTime <- as.POSIXct(paste(SBE04841_8$Date,SBE04841_8$Time),format="%y/%m/%d %H:%M:%S",tz="Pacific/Kiritimati")
SBE04842_8$DateTime <- as.POSIXct(paste(SBE04842_8$Date,SBE04842_8$Time),format="%y/%m/%d %H:%M:%S",tz="Pacific/Kiritimati")
# Site 19
SBE00653_19$DateTime <- as.POSIXct(paste(SBE00653_19$Date,SBE00653_19$Time),format="%y/%m/%d %H:%M:%S",tz="Pacific/Kiritimati")
# Site 25
SBE00651_25$DateTime <- as.POSIXct(paste(SBE00651_25$Date,SBE00651_25$Time),format="%y/%m/%d %H:%M:%S",tz="Pacific/Kiritimati")
#Site 27
SBE00650_27$DateTime <- as.POSIXct(paste(SBE00650_27$Date,SBE00650_27$Time),format="%y/%m/%d %H:%M:%S",tz="Pacific/Kiritimati")
SBE00655_27$DateTime <- as.POSIXct(paste(SBE00655_27$Date,SBE00655_27$Time),format="%y/%m/%d %H:%M:%S",tz="Pacific/Kiritimati")
SBE04840_27$DateTime <- as.POSIXct(paste(SBE04840_27$Date,SBE04840_27$Time),format="%y/%m/%d %H:%M:%S",tz="Pacific/Kiritimati")
SBE00650b_27$DateTime <- as.POSIXct(paste(SBE00650b_27$Date,SBE00650b_27$Time),format="%y/%m/%d %H:%M:%S",tz="Pacific/Kiritimati")
# Site 30
SBE04839_30$DateTime <- as.POSIXct(paste(SBE04839_30$Date,SBE04839_30$Time),format="%y/%m/%d %H:%M:%S",tz="Pacific/Kiritimati")
# Site 32
SBE0652_32$DateTime <- as.POSIXct(paste(SBE0652_32$Date,SBE0652_32$Time),format="%y/%m/%d %H:%M:%S",tz="Pacific/Kiritimati")
SBE04844_32$DateTime <- as.POSIXct(paste(SBE04844_32$Date,SBE04844_32$Time),format="%y/%m/%d %H:%M:%S",tz="Pacific/Kiritimati")
# Site 34
SBE00650_34$DateTime <- as.POSIXct(paste(SBE00650_34$Date,SBE00650_34$Time),format="%y/%m/%d %H:%M:%S",tz="Pacific/Kiritimati")
SBE04840_34$DateTime <- as.POSIXct(paste(SBE04840_34$Date,SBE04840_34$Time),format="%y/%m/%d %H:%M:%S",tz="Pacific/Kiritimati")
# Site 35
SBE00637_35$DateTime <- as.POSIXct(paste(SBE00637_35$Date,SBE00637_35$Time),format="%y/%m/%d %H:%M:%S",tz="Pacific/Kiritimati")
SBE00637b_35$DateTime <- as.POSIXct(paste(SBE00637b_35$Date,SBE00637b_35$Time),format="%y/%m/%d %H:%M:%S",tz="Pacific/Kiritimati")
SBE04842_35$DateTime <- as.POSIXct(paste(SBE04842_35$Date,SBE04842_35$Time),format="%y/%m/%d %H:%M:%S",tz="Pacific/Kiritimati")
SBE04842_35$DateTime <- as.POSIXct(paste(SBE04842_35$Date,SBE04842_35$Time),format="%y/%m/%d %H:%M:%S",tz="Pacific/Kiritimati")
###################################################################################

###################################################################################
###### MARCH 2016 DOWNLOADS - Subset, only keeping new data ########
# Read in the 'unclean' csv files
# Site 5 - SBE00650 - deployed 12July2015 (?wasnt this switched by kim? changed to 2016-11-09 for now); retrieved 25 March 2016
SBE00650_Mar16_5 <- read.csv("Data/SB-deployed/unclean_JR/SBE05600650_2016-03-25_5.csv")
# Site 9 - SBE00655 - deployed 20July2015; retrieved 27 March 2016
SBE00655_Mar16_9 <- read.csv("Data/SB-deployed/unclean_JR/SBE05600655_2016-03-27_9.csv")
# Site 15 - SBE00637 - deployed 2May2015; retrieved 30 March 2016
SBE00637_Mar16_15 <- read.csv("Data/SB-deployed/unclean_JR/SBE05600637_2016-03-30_15.csv")
# Site 27 - SBE04843 - deployed 20July2015; retrieved 20 March 2016
SBE04843_Mar16_27 <- read.csv("Data/SB-deployed/unclean_JR/SBE05604843_2016-03-20_27.csv")
# Site 30 - SBE04839 - deployed 20July2015; retrieved 22 March 2016
SBE04839_Mar16_30 <- read.csv("Data/SB-deployed/unclean_JR/SBE05604839_2016-03-22_30.csv")
## Site 32 - SBE04842 - deployed 18July2015; retrieved 18 March 2016
SBE04842_Mar16_32 <- read.csv("Data/SB-deployed/unclean_JR/SBE05604842_2016-03-18_32.csv")
# Site 33 - SBE04841 - deployed 11May2015; retrieved 27 March 2016
SBE04841_Mar16_33 <- read.csv("Data/SB-deployed/unclean_JR/SBE05604841_2016-03-27_33.csv")
# Site 34 - SBE04844 - deployed?????; retrieved 26 March 2016
SBE04844_Mar16_34 <- read.csv("Data/SB-deployed/unclean_JR/SBE05604844_2016-03-26_34.csv")
# Site 35 - SBE04840 - deployed 19July2015; retrieved 19 March 2016
SBE04840_Mar16_35 <- read.csv("Data/SB-deployed/unclean_JR/SBE05604840_2016-03-19_35.csv")
# Site 8 isn't included here 

# Create column "DateTime" that includes both "Date" and "Time" in POSIXct format
SBE00650_Mar16_5$DateTime <- as.POSIXct(paste(SBE00650_Mar16_5$Date,SBE00650_Mar16_5$Time),format="%d/%m/%y %H:%M:%S",tz="Pacific/Kiritimati")
SBE00655_Mar16_9$DateTime <- as.POSIXct(paste(SBE00655_Mar16_9$Date,SBE00655_Mar16_9$Time),format="%d/%m/%y %H:%M:%S",tz="Pacific/Kiritimati")
SBE00637_Mar16_15$DateTime <- as.POSIXct(paste(SBE00637_Mar16_15$Date,SBE00637_Mar16_15$Time),format="%d/%m/%y %H:%M:%S",tz="Pacific/Kiritimati")
SBE04843_Mar16_27$DateTime <- as.POSIXct(paste(SBE04843_Mar16_27$Date,SBE04843_Mar16_27$Time),format="%d/%m/%y %H:%M:%S",tz="Pacific/Kiritimati")
SBE04839_Mar16_30$DateTime <- as.POSIXct(paste(SBE04839_Mar16_30$Date,SBE04839_Mar16_30$Time),format="%d/%m/%y %H:%M:%S",tz="Pacific/Kiritimati")
SBE04842_Mar16_32$DateTime <- as.POSIXct(paste(SBE04842_Mar16_32$Date,SBE04842_Mar16_32$Time),format="%d/%m/%y %H:%M:%S",tz="Pacific/Kiritimati")
SBE04841_Mar16_33$DateTime <- as.POSIXct(paste(SBE04841_Mar16_33$Date,SBE04841_Mar16_33$Time),format="%d/%m/%y %H:%M:%S",tz="Pacific/Kiritimati")
SBE04844_Mar16_34$DateTime <- as.POSIXct(paste(SBE04844_Mar16_34$Date,SBE04844_Mar16_34$Time),format="%d/%m/%y %H:%M:%S",tz="Pacific/Kiritimati")
SBE04840_Mar16_35$DateTime <- as.POSIXct(paste(SBE04840_Mar16_35$Date,SBE04840_Mar16_35$Time),format="%d/%m/%y %H:%M:%S",tz="Pacific/Kiritimati")

# Subset based on the deployment/retrieval dates at each site
SBE00650_Mar16_5 <- subset(SBE00650_Mar16_5, DateTime >=as.POSIXct('2015-11-09',tz="Pacific/Kiritimati",format="%Y-%m-%d") & DateTime <=as.POSIXct('2016-03-24',tz="Pacific/Kiritimati",format="%Y-%m-%d"))
SBE00655_Mar16_9 <- subset(SBE00655_Mar16_9, DateTime >=as.POSIXct('2015-07-21',tz="Pacific/Kiritimati",format="%Y-%m-%d") & DateTime <=as.POSIXct('2016-03-26',tz="Pacific/Kiritimati",format="%Y-%m-%d"))
SBE00637_Mar16_15 <- subset(SBE00637_Mar16_15, DateTime >=as.POSIXct('2015-05-03',tz="Pacific/Kiritimati",format="%Y-%m-%d") & DateTime <=as.POSIXct('2016-03-29',tz="Pacific/Kiritimati",format="%Y-%m-%d"))
SBE04843_Mar16_27 <- subset(SBE04843_Mar16_27, DateTime >=as.POSIXct('2015-07-21',tz="Pacific/Kiritimati",format="%Y-%m-%d") & DateTime <=as.POSIXct('2016-03-19',tz="Pacific/Kiritimati",format="%Y-%m-%d"))
SBE04839_Mar16_30 <- subset(SBE04839_Mar16_30, DateTime >=as.POSIXct('2015-07-21',tz="Pacific/Kiritimati",format="%Y-%m-%d") & DateTime <=as.POSIXct('2016-03-21',tz="Pacific/Kiritimati",format="%Y-%m-%d"))
SBE04842_Mar16_32 <- subset(SBE04842_Mar16_32, DateTime >=as.POSIXct('2015-07-19',tz="Pacific/Kiritimati",format="%Y-%m-%d") & DateTime <=as.POSIXct('2016-03-16',tz="Pacific/Kiritimati",format="%Y-%m-%d"))
SBE04841_Mar16_33 <- subset(SBE04841_Mar16_33, DateTime >=as.POSIXct('2015-05-12',tz="Pacific/Kiritimati",format="%Y-%m-%d") & DateTime <=as.POSIXct('2016-03-26',tz="Pacific/Kiritimati",format="%Y-%m-%d"))
SBE04844_Mar16_34 <- subset(SBE04844_Mar16_34, DateTime >=as.POSIXct('2015-07-19',tz="Pacific/Kiritimati",format="%Y-%m-%d") & DateTime <=as.POSIXct('2016-03-25',tz="Pacific/Kiritimati",format="%Y-%m-%d"))
SBE04840_Mar16_35 <- subset(SBE04840_Mar16_35, DateTime >=as.POSIXct('2015-07-20',tz="Pacific/Kiritimati",format="%Y-%m-%d") & DateTime <=as.POSIXct('2016-03-18',tz="Pacific/Kiritimati",format="%Y-%m-%d"))
###################################################################################

###################################################################################
###### NOVEMBER 2016 DOWNLOADS - Subset, only keeping new data ########
# Read in the 'unclean' csv files
# Site 5 - SBE00650 - deployed 26Mar16; retrieved 11Nov16
SBE00650_Nov16_5 <- read.csv("Data/SB-deployed/unclean_nov/SBE05600650_2016-11-10_5.csv")
# Site 5b - SBE04840 - deployed 11Nov16; retrieved 13Nov16
# Site 9 - SBE00655 - deployed 29Mar16; retrieved 19Nov16
SBE00655_Nov16_9 <- read.csv("Data/SB-deployed/unclean_nov/SBE05600655_2016-11-18_9.csv")
# Site 15 - SBE04841 - deployed 30Mar16  ; retrieved 15Nov16
SBE04841_Nov16_15 <- read.csv("Data/SB-deployed/unclean_nov/SBE05604841_2016-11-14_15.csv")
# Site 19 - SBE04845 - deployed 2May15; retrieved 15Nov16
SBE04845_Nov16_19 <- read.csv("Data/SB-deployed/unclean_nov/SBE05604845_2016-11-14_19.csv")
# Site 27 - SBE04843 - deployed 21March16; retrieved 12Nov16 
SBE04843_Nov16_27 <- read.csv("Data/SB-deployed/unclean_nov/SBE05604843_2016-11-12_27.csv")
# Site 30 - SBE04839 - deployed 24March16; retrieved 12Nov16
SBE04839_Nov16_30 <- read.csv("Data/SB-deployed/unclean_nov/SBE05604839_2016-11-12_30.csv")
# Site 32 - SBE04842 - deployed 18March16; retrieved 11Nov16
SBE04842_Nov16_32 <- read.csv("Data/SB-deployed/unclean_nov/SBE05604842_2016-11-10_32.csv")
# Site 34 - SBE04844 - deployed 27Mar16; retrieved 20Nov16
SBE04844_Nov16_34 <- read.csv("Data/SB-deployed/unclean_nov/SBE05604844_2016-11-19_34.csv")
# Site 35 - SBE04840 - deployed 20March16; retrieved 10Nov16
SBE04840_Nov16_35 <- read.csv("Data/SB-deployed/unclean_nov/SBE05604840_2016-11-10_35.csv")
# Site 40 - SBE00637 - deployed 4Apr16; retrieved 18Nov16
SBE00637_Nov16_40 <- read.csv("Data/SB-deployed/unclean_nov/SBE05600637_2016-11-17_40.csv")

# Create column "DateTime" that includes both "Date" and "Time" in POSIXct format
SBE00650_Nov16_5$DateTime <- as.POSIXct(paste(SBE00650_Nov16_5$Date,SBE00650_Nov16_5$Time),format="%m/%d/%y %H:%M:%S",tz="Pacific/Kiritimati")
SBE00655_Nov16_9$DateTime <- as.POSIXct(paste(SBE00655_Nov16_9$Date,SBE00655_Nov16_9$Time),format="%m/%d/%y %H:%M:%S",tz="Pacific/Kiritimati")
SBE04841_Nov16_15$DateTime <- as.POSIXct(paste(SBE04841_Nov16_15$Date,SBE04841_Nov16_15$Time),format="%m/%d/%y %H:%M:%S",tz="Pacific/Kiritimati")
SBE04845_Nov16_19$DateTime <- as.POSIXct(paste(SBE04845_Nov16_19$Date,SBE04845_Nov16_19$Time),format="%m/%d/%y %H:%M:%S",tz="Pacific/Kiritimati")
SBE04843_Nov16_27$DateTime <- as.POSIXct(paste(SBE04843_Nov16_27$Date,SBE04843_Nov16_27$Time),format="%m/%d/%y %H:%M:%S",tz="Pacific/Kiritimati")
SBE04839_Nov16_30$DateTime <- as.POSIXct(paste(SBE04839_Nov16_30$Date,SBE04839_Nov16_30$Time),format="%m/%d/%y %H:%M:%S",tz="Pacific/Kiritimati")
SBE04842_Nov16_32$DateTime <- as.POSIXct(paste(SBE04842_Nov16_32$Date,SBE04842_Nov16_32$Time),format="%m/%d/%y %H:%M:%S",tz="Pacific/Kiritimati")
SBE04844_Nov16_34$DateTime <- as.POSIXct(paste(SBE04844_Nov16_34$Date,SBE04844_Nov16_34$Time),format="%m/%d/%y %H:%M:%S",tz="Pacific/Kiritimati")
SBE04840_Nov16_35$DateTime <- as.POSIXct(paste(SBE04840_Nov16_35$Date,SBE04840_Nov16_35$Time),format="%m/%d/%y %H:%M:%S",tz="Pacific/Kiritimati")
SBE00637_Nov16_40$DateTime <- as.POSIXct(paste(SBE00637_Nov16_40$Date,SBE00637_Nov16_40$Time),format="%m/%d/%y %H:%M:%S",tz="Pacific/Kiritimati")

# Subset based on the deployment/retrieval dates at each site
SBE00650_Nov16_5 <- subset(SBE00650_Nov16_5, DateTime >=as.POSIXct('2016-03-27',tz="Pacific/Kiritimati",format="%Y-%m-%d") & DateTime <=as.POSIXct('2016-11-10',tz="Pacific/Kiritimati",format="%Y-%m-%d"))
SBE00655_Nov16_9 <- subset(SBE00655_Nov16_9, DateTime >=as.POSIXct('2016-03-30',tz="Pacific/Kiritimati",format="%Y-%m-%d") & DateTime <=as.POSIXct('2016-11-18',tz="Pacific/Kiritimati",format="%Y-%m-%d"))
SBE04841_Nov16_15 <- subset(SBE04841_Nov16_15, DateTime >=as.POSIXct('2016-03-31',tz="Pacific/Kiritimati",format="%Y-%m-%d") & DateTime <=as.POSIXct('2016-11-14',tz="Pacific/Kiritimati",format="%Y-%m-%d"))
SBE04845_Nov16_19 <- subset(SBE04845_Nov16_19, DateTime >=as.POSIXct('2015-05-03',tz="Pacific/Kiritimati",format="%Y-%m-%d") & DateTime <=as.POSIXct('2016-11-14',tz="Pacific/Kiritimati",format="%Y-%m-%d"))
SBE04843_Nov16_27 <- subset(SBE04843_Nov16_27, DateTime >=as.POSIXct('2016-03-22',tz="Pacific/Kiritimati",format="%Y-%m-%d") & DateTime <=as.POSIXct('2016-11-11',tz="Pacific/Kiritimati",format="%Y-%m-%d"))
SBE04839_Nov16_30 <- subset(SBE04839_Nov16_30, DateTime >=as.POSIXct('2016-03-23',tz="Pacific/Kiritimati",format="%Y-%m-%d") & DateTime <=as.POSIXct('2016-11-11',tz="Pacific/Kiritimati",format="%Y-%m-%d"))
SBE04842_Nov16_32 <- subset(SBE04842_Nov16_32, DateTime >=as.POSIXct('2016-03-19',tz="Pacific/Kiritimati",format="%Y-%m-%d") & DateTime <=as.POSIXct('2016-11-10',tz="Pacific/Kiritimati",format="%Y-%m-%d"))
SBE04844_Nov16_34 <- subset(SBE04844_Nov16_34, DateTime >=as.POSIXct('2016-03-28',tz="Pacific/Kiritimati",format="%Y-%m-%d") & DateTime <=as.POSIXct('2016-11-19',tz="Pacific/Kiritimati",format="%Y-%m-%d"))
SBE04840_Nov16_35 <- subset(SBE04840_Nov16_35, DateTime >=as.POSIXct('2016-03-21',tz="Pacific/Kiritimati",format="%Y-%m-%d") & DateTime <=as.POSIXct('2016-11-09',tz="Pacific/Kiritimati",format="%Y-%m-%d"))
SBE00637_Nov16_40 <- subset(SBE00637_Nov16_40, DateTime >=as.POSIXct('2016-04-05',tz="Pacific/Kiritimati",format="%Y-%m-%d") & DateTime <=as.POSIXct('2016-11-17',tz="Pacific/Kiritimati",format="%Y-%m-%d"))
###################################################################################

###################################################################################
############# Now Interpolate the Data to a Common Time Frame #####################
# The first date that we want to interpolate from (started 2011-1-1 for ease later on)
startDate <- as.POSIXct("2011/1/1 00:00:00",format="%Y/%m/%d %H:%M:%S",tz="Pacific/Kiritimati")
# The last date that we want to interpolate to (ended 2016-12-31 for ease later on)
endDate <- as.POSIXct("2016/12/31 23:59:00",format="%Y/%m/%d %H:%M:%S",tz="Pacific/Kiritimati")
# Create the POSIXct (time) object that we will be standardizing to - 1 minute interval
xi <- (seq(from=startDate, to= endDate, by=60)) # by=60 means interpolate by 1 minute intervals
xi2 <- (seq(from=startDate, to= endDate, by=3600)) # by=3600 means interpolate by 1 hour intervals
# Create a list regions for reference later
regions=c("southlagoon","northlagoon","lagoonface","northshore","bayofwrecks","vaskesbay")
# Create a list of sites for reference later
sitelist <- c("site3","site5","site8","site8.5","site9","site15","site19","site25","site27","site30","site32","site33","site34","site35","site40")


## Now do the actual interpolation
## Use DateTime to interpolate Temperature over xi. Don't remove NA values. The maximum gap (sequence of NAs) to interpolate over is 10800 seconds or 3 hours
# List all loggers for use in loop
logger <- grep("SBE",ls(),value=TRUE)
# For every logger, create and evaluate a string of form: 
# loggername_interp <- na.interp(loggername$DateTime, loggername$Temperature, xi, na.rm=FALSE, maxgap=10800)
for (i in 1:length(logger)) {
  evalstr<-paste((logger[i]),"_interp <- ","na.interp1(", logger[i], "$DateTime,",logger[i],"$Temperature, xi, na.rm=FALSE, maxgap=10800)",sep="")
  eval(parse(text=evalstr))
}

# Create dataframe for each site that includes all interpolated objects for that site
site3 <- ls(pattern="_3_interp")
site5 <- ls(pattern="_5_interp")
site8 <- ls(pattern="_8_interp")
site8.5 <- ls(pattern="_8.5_interp")
site9 <- ls(pattern="_9_interp")
site15 <- ls(pattern="_15_interp")
site19 <- ls(pattern="_19_interp")
site25 <- ls(pattern="_25_interp")
site27 <- ls(pattern="_27_interp")
site30 <- ls(pattern="_30_interp")
site32 <- ls(pattern="_32_interp")
site33 <- ls(pattern="_33_interp")
site34 <- ls(pattern="_34_interp")
site35 <- ls(pattern="_35_interp")
site40 <- ls(pattern="_40_interp")
# Create dataframe for each region that includes all interpolated objects for that site
southlagoon <- c(site8,site34,site35)
northlagoon <- c(site27,site30)
lagoonface <- c(site9,site32,site33,site40)
northshore <- c(site25,site3)
bayofwrecks <- c(site15,site19)
vaskesbay <- c(site5)
allsites <- c(site8,site34,site35,site27,site30,site9,site32,site33,site40,site25,site3,site15,site19,site5)

## Now, we merge all data from a single site into an object sampled at 1 minute intervals
# Create 'indivlogger' to inclue all loggers at site3
indivlogger <- paste(site3,collapse=",")
# Create a string (for later evaluation) that binds together all individual loggers
evalstr <- paste("temperature_1m <- cbind(",indivlogger,")")
# Evaluate that string
eval(parse(text=evalstr))
# Collapse all of the columns in temperature_1m into one column by using rowMeans
temperature_1m <- rowMeans(temperature_1m,na.rm=TRUE)
# Bind together the time vector (xi) and the temperature
site3_1m<-cbind.data.frame(xi,temperature_1m)
## Now calculate the mean for each hour
# Create a temporary vector 'tempmatrix'
tempmatrix <- temperature_1m
# Reshape tempmatrix to be able to use colMeans to collapse each hour
dim(tempmatrix) = c(60,length(temperature_1m)/60)
# Collapse by-minute data to hourly data, by taking the mean of all minutes in an hour
temperature_1hr <- colMeans(tempmatrix,na.rm=TRUE)
# Bind together the new time vector (xi2) and the hourly temperature
site3_1hr<-cbind.data.frame(xi2,temperature_1hr)
# Remove temporary variables to avoid confusion later on
rm(temperature_1m,temperature_1hr)

indivlogger <- paste(site5,collapse=",")
evalstr <- paste("temperature_1m <- cbind(",indivlogger,")")
eval(parse(text=evalstr))
temperature_1m <- rowMeans(temperature_1m,na.rm=TRUE)
site5_1m<-cbind.data.frame(xi,temperature_1m)
## Now calculate the mean for each hour
tempmatrix <- temperature_1m
dim(tempmatrix) = c(60,length(temperature_1m)/60)
temperature_1hr <- colMeans(tempmatrix,na.rm=TRUE)
site5_1hr<-cbind.data.frame(xi2,temperature_1hr)
rm(temperature_1m,temperature_1hr)

indivlogger <- paste(site8,collapse=",")
evalstr <- paste("temperature_1m <- cbind(",indivlogger,")")
eval(parse(text=evalstr))
temperature_1m <- rowMeans(temperature_1m,na.rm=TRUE)
site8_1m<-cbind.data.frame(xi,temperature_1m)
## Now calculate the mean for each hour
tempmatrix <- temperature_1m
dim(tempmatrix) = c(60,length(temperature_1m)/60)
temperature_1hr <- colMeans(tempmatrix,na.rm=TRUE)
site8_1hr<-cbind.data.frame(xi2,temperature_1hr)
rm(temperature_1m,temperature_1hr)

indivlogger <- paste(site8.5,collapse=",")
evalstr <- paste("temperature_1m <- cbind(",indivlogger,")")
eval(parse(text=evalstr))
temperature_1m <- rowMeans(temperature_1m,na.rm=TRUE)
site8.5_1m<-cbind.data.frame(xi,temperature_1m)
## Now calculate the mean for each hour
tempmatrix <- temperature_1m
dim(tempmatrix) = c(60,length(temperature_1m)/60)
temperature_1hr <- colMeans(tempmatrix,na.rm=TRUE)
site8.5_1hr<-cbind.data.frame(xi2,temperature_1hr)
rm(temperature_1m,temperature_1hr)

indivlogger <- paste(site9,collapse=",")
evalstr <- paste("temperature_1m <- cbind(",indivlogger,")")
eval(parse(text=evalstr))
temperature_1m <- rowMeans(temperature_1m,na.rm=TRUE)
site9_1m<-cbind.data.frame(xi,temperature_1m)
## Now calculate the mean for each hour
tempmatrix <- temperature_1m
dim(tempmatrix) = c(60,length(temperature_1m)/60)
temperature_1hr <- colMeans(tempmatrix,na.rm=TRUE)
site9_1hr<-cbind.data.frame(xi2,temperature_1hr)
rm(temperature_1m,temperature_1hr)

indivlogger <- paste(site15,collapse=",")
evalstr <- paste("temperature_1m <- cbind(",indivlogger,")")
eval(parse(text=evalstr))
temperature_1m <- rowMeans(temperature_1m,na.rm=TRUE)
site15_1m<-cbind.data.frame(xi,temperature_1m)
## Now calculate the mean for each hour
tempmatrix <- temperature_1m
dim(tempmatrix) = c(60,length(temperature_1m)/60)
temperature_1hr <- colMeans(tempmatrix,na.rm=TRUE)
site15_1hr<-cbind.data.frame(xi2,temperature_1hr)
rm(temperature_1m,temperature_1hr)

indivlogger <- paste(site19,collapse=",")
evalstr <- paste("temperature_1m <- cbind(",indivlogger,")")
eval(parse(text=evalstr))
temperature_1m <- rowMeans(temperature_1m,na.rm=TRUE)
site19_1m<-cbind.data.frame(xi,temperature_1m)
## Now calculate the mean for each hour
tempmatrix <- temperature_1m
dim(tempmatrix) = c(60,length(temperature_1m)/60)
temperature_1hr <- colMeans(tempmatrix,na.rm=TRUE)
site19_1hr<-cbind.data.frame(xi2,temperature_1hr)
rm(temperature_1m,temperature_1hr)

indivlogger <- paste(site25,collapse=",")
evalstr <- paste("temperature_1m <- cbind(",indivlogger,")")
eval(parse(text=evalstr))
temperature_1m <- rowMeans(temperature_1m,na.rm=TRUE)
site25_1m<-cbind.data.frame(xi,temperature_1m)
## Now calculate the mean for each hour
tempmatrix <- temperature_1m
dim(tempmatrix) = c(60,length(temperature_1m)/60)
temperature_1hr <- colMeans(tempmatrix,na.rm=TRUE)
site25_1hr<-cbind.data.frame(xi2,temperature_1hr)
rm(temperature_1m,temperature_1hr)

indivlogger <- paste(site27,collapse=",")
evalstr <- paste("temperature_1m <- cbind(",indivlogger,")")
eval(parse(text=evalstr))
temperature_1m <- rowMeans(temperature_1m,na.rm=TRUE)
site27_1m<-cbind.data.frame(xi,temperature_1m)
## Now calculate the mean for each hour
tempmatrix <- temperature_1m
dim(tempmatrix) = c(60,length(temperature_1m)/60)
temperature_1hr <- colMeans(tempmatrix,na.rm=TRUE)
site27_1hr<-cbind.data.frame(xi2,temperature_1hr)
rm(temperature_1m,temperature_1hr)

indivlogger <- paste(site30,collapse=",")
evalstr <- paste("temperature_1m <- cbind(",indivlogger,")")
eval(parse(text=evalstr))
temperature_1m <- rowMeans(temperature_1m,na.rm=TRUE)
site30_1m<-cbind.data.frame(xi,temperature_1m)
## Now calculate the mean for each hour
tempmatrix <- temperature_1m
dim(tempmatrix) = c(60,length(temperature_1m)/60)
temperature_1hr <- colMeans(tempmatrix,na.rm=TRUE)
site30_1hr<-cbind.data.frame(xi2,temperature_1hr)
rm(temperature_1m,temperature_1hr)

indivlogger <- paste(site32,collapse=",")
evalstr <- paste("temperature_1m <- cbind(",indivlogger,")")
eval(parse(text=evalstr))
temperature_1m <- rowMeans(temperature_1m,na.rm=TRUE)
site32_1m<-cbind.data.frame(xi,temperature_1m)
## Now calculate the mean for each hour
tempmatrix <- temperature_1m
dim(tempmatrix) = c(60,length(temperature_1m)/60)
temperature_1hr <- colMeans(tempmatrix,na.rm=TRUE)
site32_1hr<-cbind.data.frame(xi2,temperature_1hr)
rm(temperature_1m,temperature_1hr)

indivlogger <- paste(site33,collapse=",")
evalstr <- paste("temperature_1m <- cbind(",indivlogger,")")
eval(parse(text=evalstr))
temperature_1m <- rowMeans(temperature_1m,na.rm=TRUE)
site33_1m<-cbind.data.frame(xi,temperature_1m)
## Now calculate the mean for each hour
tempmatrix <- temperature_1m
dim(tempmatrix) = c(60,length(temperature_1m)/60)
temperature_1hr <- colMeans(tempmatrix,na.rm=TRUE)
site33_1hr<-cbind.data.frame(xi2,temperature_1hr)
rm(temperature_1m,temperature_1hr)

indivlogger <- paste(site34,collapse=",")
evalstr <- paste("temperature_1m <- cbind(",indivlogger,")")
eval(parse(text=evalstr))
temperature_1m <- rowMeans(temperature_1m,na.rm=TRUE)
site34_1m<-cbind.data.frame(xi,temperature_1m)
## Now calculate the mean for each hour
tempmatrix <- temperature_1m
dim(tempmatrix) = c(60,length(temperature_1m)/60)
temperature_1hr <- colMeans(tempmatrix,na.rm=TRUE)
site34_1hr<-cbind.data.frame(xi2,temperature_1hr)
rm(temperature_1m,temperature_1hr)

indivlogger <- paste(site35,collapse=",")
evalstr <- paste("temperature_1m <- cbind(",indivlogger,")")
eval(parse(text=evalstr))
temperature_1m <- rowMeans(temperature_1m,na.rm=TRUE)
site35_1m<-cbind.data.frame(xi,temperature_1m)
## Now calculate the mean for each hour
tempmatrix <- temperature_1m
dim(tempmatrix) = c(60,length(temperature_1m)/60)
temperature_1hr <- colMeans(tempmatrix,na.rm=TRUE)
site35_1hr<-cbind.data.frame(xi2,temperature_1hr)
rm(temperature_1m,temperature_1hr)
# save(site35_1hr,file="~/../Desktop/site35damnit.RData")

indivlogger <- paste(site40,collapse=",")
evalstr <- paste("temperature_1m <- cbind(",indivlogger,")")
eval(parse(text=evalstr))
temperature_1m <- rowMeans(temperature_1m,na.rm=TRUE)
site40_1m<-cbind.data.frame(xi,temperature_1m)
## Now calculate the mean for each hour
tempmatrix <- temperature_1m
dim(tempmatrix) = c(60,length(temperature_1m)/60)
temperature_1hr <- colMeans(tempmatrix,na.rm=TRUE)
site40_1hr<-cbind.data.frame(xi2,temperature_1hr)
rm(temperature_1m,temperature_1hr)

## Plot to visually check for outliers
# plot(site3_1hr,main="site3")
# plot(site5_1hr,main="site5")
# plot(site8_1hr,main="site8")
# plot(site9_1hr,main="site9")
# plot(site15_1hr,main="site15")
# plot(site19_1hr,main="site19")
# plot(site25_1hr,main="site25")
# plot(site27_1hr,main="site27")
# plot(site30_1hr,main="site30")
# plot(site32_1hr,main="site32")
# plot(site33_1hr,main="site33")
# plot(site34_1hr,main="site34")
# plot(site35_1hr,main="site35")
# plot(site40_1hr,main="site40")

# Now do the same thing using region temperature (group sites by region)
indivlogger <- paste(northlagoon,collapse=",")
evalstr <- paste("temperature_1m <- cbind(",indivlogger,")")
eval(parse(text=evalstr))
temperature_1m <- rowMeans(temperature_1m,na.rm=TRUE)
northlagoon_1m<-cbind.data.frame(xi,temperature_1m)
## Now calculate the mean for each hour
tempmatrix <- temperature_1m
dim(tempmatrix) = c(60,length(temperature_1m)/60)
temperature_1hr <- colMeans(tempmatrix,na.rm=TRUE)
northlagoon_1hr<-cbind.data.frame(xi2,temperature_1hr)
rm(temperature_1m,temperature_1hr)

indivlogger <- paste(southlagoon,collapse=",")
evalstr <- paste("temperature_1m <- cbind(",indivlogger,")")
eval(parse(text=evalstr))
temperature_1m <- rowMeans(temperature_1m,na.rm=TRUE)
southlagoon_1m<-cbind.data.frame(xi,temperature_1m)
## Now calculate the mean for each hour
tempmatrix <- temperature_1m
dim(tempmatrix) = c(60,length(temperature_1m)/60)
temperature_1hr <- colMeans(tempmatrix,na.rm=TRUE)
southlagoon_1hr<-cbind.data.frame(xi2,temperature_1hr)
rm(temperature_1m,temperature_1hr)

indivlogger <- paste(lagoonface,collapse=",")
evalstr <- paste("temperature_1m <- cbind(",indivlogger,")")
eval(parse(text=evalstr))
temperature_1m <- rowMeans(temperature_1m,na.rm=TRUE)
lagoonface_1m<-cbind.data.frame(xi,temperature_1m)
## Now calculate the mean for each hour
tempmatrix <- temperature_1m
dim(tempmatrix) = c(60,length(temperature_1m)/60)
temperature_1hr <- colMeans(tempmatrix,na.rm=TRUE)
lagoonface_1hr<-cbind.data.frame(xi2,temperature_1hr)
rm(temperature_1m,temperature_1hr)

indivlogger <- paste(northshore,collapse=",")
evalstr <- paste("temperature_1m <- cbind(",indivlogger,")")
eval(parse(text=evalstr))
temperature_1m <- rowMeans(temperature_1m,na.rm=TRUE)
northshore_1m<-cbind.data.frame(xi,temperature_1m)
## Now calculate the mean for each hour
tempmatrix <- temperature_1m
dim(tempmatrix) = c(60,length(temperature_1m)/60)
temperature_1hr <- colMeans(tempmatrix,na.rm=TRUE)
northshore_1hr<-cbind.data.frame(xi2,temperature_1hr)
rm(temperature_1m,temperature_1hr)

indivlogger <- paste(bayofwrecks,collapse=",")
evalstr <- paste("temperature_1m <- cbind(",indivlogger,")")
eval(parse(text=evalstr))
temperature_1m <- rowMeans(temperature_1m,na.rm=TRUE)
bayofwrecks_1m<-cbind.data.frame(xi,temperature_1m)
## Now calculate the mean for each hour
tempmatrix <- temperature_1m
dim(tempmatrix) = c(60,length(temperature_1m)/60)
temperature_1hr <- colMeans(tempmatrix,na.rm=TRUE)
bayofwrecks_1hr<-cbind.data.frame(xi2,temperature_1hr)
rm(temperature_1m,temperature_1hr)

indivlogger <- paste(vaskesbay,collapse=",")
evalstr <- paste("temperature_1m <- cbind(",indivlogger,")")
eval(parse(text=evalstr))
temperature_1m <- rowMeans(temperature_1m,na.rm=TRUE)
vaskesbay_1m<-cbind.data.frame(xi,temperature_1m)
## Now calculate the mean for each hour
tempmatrix <- temperature_1m
dim(tempmatrix) = c(60,length(temperature_1m)/60)
temperature_1hr <- colMeans(tempmatrix,na.rm=TRUE)
vaskesbay_1hr<-cbind.data.frame(xi2,temperature_1hr)
rm(temperature_1m,temperature_1hr)

indivlogger <- paste(allsites,collapse=",")
evalstr <- paste("temperature_1m <- cbind(",indivlogger,")")
eval(parse(text=evalstr))
temperature_1m <- rowMeans(temperature_1m,na.rm=TRUE)
KI_allsites_1m<-cbind.data.frame(xi,temperature_1m)
## Now calculate the mean for each hour
tempmatrix <- temperature_1m
dim(tempmatrix) = c(60,length(temperature_1m)/60)
temperature_1hr <- colMeans(tempmatrix,na.rm=TRUE)
KI_allsites_1hr<-cbind.data.frame(xi2,temperature_1hr)
rm(temperature_1m,temperature_1hr)


# # Plot to check
# plot(northlagoon_1hr,main="northlagoon")
# plot(southlagoon_1hr,main="southlagoon")
# plot(lagoonface_1hr,main="lagoonface")
# plot(northshore_1hr,main="northshore")
# plot(bayofwrecks_1hr,main="bayofwrecks")
# plot(vaskesbay_1hr,main="vaskesbay")

# Save all of the hourly temperature data
save(site3_1hr,site5_1hr,site8_1hr,site9_1hr,site15_1hr,site19_1hr,site25_1hr,site27_1hr,site30_1hr,site32_1hr,site33_1hr,site34_1hr,site35_1hr,site40_1hr,southlagoon_1hr,northlagoon_1hr,lagoonface_1hr,northshore_1hr,bayofwrecks_1hr,vaskesbay_1hr,KI_allsites_1hr,file="Data/merged/KI_SB_temp_1hr.RData")

# Save all of the by-minute temperature data
save(site3_1m,site5_1m,site8_1m,site9_1m,site15_1m,site19_1m,site25_1m,site27_1m,site30_1m,site32_1m,site33_1m,site34_1m,site35_1m,site40_1m,southlagoon_1m,northlagoon_1m,lagoonface_1m,northshore_1m,bayofwrecks_1m,vaskesbay_1m,KI_allsites_1m,file="Data/merged/KI_SB_temp_1m.RData",compress=TRUE)
