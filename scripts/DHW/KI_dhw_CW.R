

########## CALCULATE DHW FOR KBAY 

# Load hourly temperature dataset (2014-2016)
NOAA.temp<-read.csv(file="output/annual.temps.csv")
df<-NOAA.temp[-1] #remove column
df$date.time<-as.POSIXct(df$date.time) # correct formatting
df<-df[!(df$date.time>= "2016-03-01 00:00:00"),] # end date

# make a column for each date.hour and generate means
df$date.hour <- strftime(df$date.time, format="%Y-%m-%d %H:00:00", breaks="hour")
hour.temp.means <- aggregate(Water.temp ~ date.hour, df, mean)

# make date sequence for hourly data to identify data gaps in NOAA data
date.seq<-as.data.frame(
  seq.POSIXt(from = as.POSIXct('2014-01-01 00:00:00'), to = as.POSIXct('2016-03-01 00:00:00'), by = 'h'))
colnames(date.seq)<-"date.hour"

# merge sequence of dates generated above with actual data from NOAA
merged.hour.temp<-as.data.frame(join_all(list(date.seq, hour.temp.means), by = "date.hour", type='full'))
write.csv(merged.hour.temp, "data/environmental/Temp data/merged.hour.temp.csv")

# make DHW a matrix, then make sure even to round for DHW
date.DHW<-merged.hour.temp$date.hour
date.DHW<-date.DHW[1:(floor(length(date.DHW)/84)*84)]

# dates to export
half.wk.dates<- date.DHW[seq(1, length(date.DHW), 84)] 

# calculate temp mean at every 84 values = 3.5d mean temp
half.wks.temp<-merged.hour.temp$Water.temp
half.wks.temp<-half.wks.temp[1:(floor(length(half.wks.temp)/84)*84)]
dim(half.wks.temp) = c(84,length(half.wks.temp)/84) #dimensions match above?
half.wks.mean<-as.data.frame(.colMeans(merged.hour.temp$Water.temp, 84, 
                                       length(merged.hour.temp$Water.temp)/84, na.rm=TRUE))

# BIND TOGETHER
half.week.temps<-cbind(half.wk.dates,half.wks.mean)
colnames(half.week.temps)<-c("half.week.date.time", "Water.temp.half.week")


# Set the "Base" temperature (i.e. the mean monthly maximum, from NOAA) # 
# 27.7? (https://www.ndbc.noaa.gov/view_climplot.php?station=mokh1&meas=st) 2008-2012 maximum summer mean and Jokiel and Brown
# from NOAA Main Hawaiian Islands (https://coralreefwatch.noaa.gov/vs/data/hawaii.txt):
# Averaged Monthly Mean (Jan-Dec): 24.3713 24.0663 24.0859 24.1832 24.7437 25.4589 26.0265 26.5946 26.9824 26.8742 26.1951 25.0945
base<-27.7


###################################################################################
## Now calculate the hotspot for all sites
# Create a dataframe including time vector xi3
KBay.hotspot <- data.frame(half.wk.dates)

# Create "hotspot" for ease of doing multiple sites
hotspot<-KBay.hotspot$hotspot

# Assign hotspot value, this is (halfweekly temperature) - (base temperature)
hotspot<-half.week.temps$Water.temp.half.week-base

# Only keep hotspot temperature if the hotspot is >1 (for calculation of DHW)
hotspot<-ifelse(hotspot<1,hotspot==0,hotspot)

# Bind together the time vector and the hotspot vector
hotspot.20142016 <- cbind.data.frame(KBay.hotspot,hotspot)


###################################################################################
## Now calculate the Degree Heating Week (DHW) ##
# Set the window for the rolling sum (this is 24 half weeks = summing over 12 weeks)
k=24

# Create a dataframe including the time vector xi3
KBay.DHW <- data.frame(half.wk.dates)

# Calculate the DHW using rollapply, summing across a 12 week window (k=24 half weeks). Multiply value by 0.5 to get DHW
KBay.DHW$DHW<-(rollapply(hotspot.20142016$hotspot, width=k, by=1, sum, fill=c(NA,NA,NA), align="right",partial=FALSE)*0.5)

write.csv(KBay.DHW, "output/KBay.DHW.csv")
