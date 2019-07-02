# environmental data
# light and temperature reef 14 and lilipuna

##############################
#clear work space
rm(list=ls())
ls()

main<-setwd(getwd())
setwd("~/Desktop/Research and Teaching/UH MANOA/Research/Mydlarz_Gates project/Bleaching KBay project/")

setwd('R-stats') #folder name where data can be found

# Temperature and light data analysis ------
library(scales); library(zoo); library(lme4); library(lsmeans); library(car)
reefcols <- c("#8dd3c7", "#bebada", "#d9d9d9")

# Import temperature data

#2014 Oct - 2015 Jun
Lil.temp_log1 <- rbind(
        read.csv("Data_all/Temp data/Lilipuna/SN_10487932/lilipuna_oct-dec 2014.csv"),
        read.csv("Data_all/Temp data/Lilipuna/SN_10487932/lilipuna_dec-feb 2015.csv"))

Lil.temp_log2 <- rbind(
        read.csv("Data_all/Temp data/Lilipuna/SN_10487932/lilipuna_feb-apr 2015.csv"),
        read.csv("Data_all/Temp data/Lilipuna/SN_10487932/lilipuna_apr-jun 2015.csv"))

#2015 Sep - 2016 Jun
Lil.temp_log4 <- rbind(
        read.csv("Data_all/Temp data/Lilipuna/SN_10084646/lilipuna_sep-nov 2015.csv"),
        read.csv("Data_all/Temp data/Lilipuna/SN_10084646/lilipuna_nov-jan 2016.csv"),
        read.csv("Data_all/Temp data/Lilipuna/SN_10084646/lilipuna_jan-mar 2016.csv"))

Lil.temp_log5<- rbind(
        read.csv("Data_all/Temp data/Lilipuna/SN_10084646/lilipuna_apr-jun 2016.csv"))
                   

#2014 Oct - 2015 Jun
Rf14.temp_log1 <- rbind(
  read.csv("Data_all/Temp data/Reef 14/SN_10487931/reef14_oct-dec 2014.csv"),
  read.csv("Data_all/Temp data/Reef 14/SN_10487931/reef14_dec-feb 2015.csv"))

Rf14.temp_log2 <-rbind( 
  read.csv("Data_all/Temp data/Reef 14/SN_10487931/reef14_feb-apr 2015.csv"),
  read.csv("Data_all/Temp data/Reef 14/SN_10487931/reef14_apr-jun 2015.csv"))

Rf14.temp_log3 <- 
  read.csv("Data_all/Temp data/Reef 14/SN_9893760/reef14_jun-sep 2015.csv")

Rf14.temp_log4 <- rbind(
  read.csv("Data_all/Temp data/Reef 14/SN_9893760/reef14_sep-nov 2015.csv"),
  read.csv("Data_all/Temp data/Reef 14/SN_9893760/reef14_sep-nov 2015.csv"),
  read.csv("Data_all/Temp data/Reef 14/SN_9893760/reef14_nov-jan 2016.csv"),
  read.csv("Data_all/Temp data/Reef 14/SN_9893760/reef14_jan-mar 2016.csv"))
  
Rf14.temp_log5<- rbind(
  read.csv("Data_all/Temp data/Reef 14/SN_9893760/reef14_apr-jun 2016.csv"))
  

# set date to date class
Lil.temp_log1$Date <- as.Date(Lil.temp_log1$Date, format="%Y/%m/%e")
Lil.temp_log2$Date <- as.Date(Lil.temp_log2$Date, format="%Y/%m/%e")
Lil.temp_log4$Date <- as.Date(Lil.temp_log4$Date, format="%Y/%m/%e")
Lil.temp_log5$Date <- as.Date(Lil.temp_log5$Date, format="%Y/%m/%e")

Rf14.temp_log1$Date <- as.Date(Rf14.temp_log1$Date, format="%Y/%m/%e")
Rf14.temp_log2$Date <- as.Date(Rf14.temp_log2$Date, format="%Y/%m/%e")
Rf14.temp_log3$Date <- as.Date(Rf14.temp_log3$Date, format="%Y/%m/%e")
Rf14.temp_log4$Date <- as.Date(Rf14.temp_log4$Date, format="%Y/%m/%e")
Rf14.temp_log5$Date <- as.Date(Rf14.temp_log5$Date, format="%Y/%m/%e")

##########################

##Calibrate temperature

# Lil.temp_log1: SN_10487932   y = 1.0066x - 0.0085
# Lil.temp_log2: SN_10084646   y = 0.9978x + 0.0815
# Rf14.temp_log1: SN_10487931   y = 1.0047x + 0.0722
# Rf14.temp_log2: SN_9893760   y = 1.0044x - 0.0648

Lil.temp_log1$Temp_Calib<-(1.0066*(Lil.temp_log1$Temp)-0.0085)
Lil.temp_log2$Temp_Calib<-(1.0066*(Lil.temp_log2$Temp)-0.0085)
Lil.temp_log4$Temp_Calib<-(0.9978*(Lil.temp_log4$Temp)-0.0815)
Lil.temp_log5$Temp_Calib<-(0.9978*(Lil.temp_log5$Temp)-0.0815)

Rf14.temp_log1$Temp_Calib<-(1.0047*(Rf14.temp_log1$Temp)-0.0722)
Rf14.temp_log2$Temp_Calib<-(1.0047*(Rf14.temp_log2$Temp)-0.0722)
Rf14.temp_log3$Temp_Calib<-(1.0044*(Rf14.temp_log3$Temp)-0.0648)
Rf14.temp_log4$Temp_Calib<-(1.0044*(Rf14.temp_log4$Temp)-0.0648)
Rf14.temp_log5$Temp_Calib<-(1.0044*(Rf14.temp_log5$Temp)-0.0648)

#compile calibrated data from all time points into single files 

TIMESPAN<-rbind(Rf14.temp_log1, Rf14.temp_log2, Rf14.temp_log3, Rf14.temp_log4, Rf14.temp_log5) #all dates from period

Lil.Temp1 <-Lil.temp_log1 #  Lilipuna temp data start 
Lil.Temp2 <-Lil.temp_log2 # 2 week break in data, Feb until loggers lost
#Lil.Temp3 ==loggers lost
Lil.Temp4 <-Lil.temp_log4 # all data until March 2016 gap
Lil.Temp5 <-Lil.temp_log5 # 2016 March onward

Rf14.Temp1 <- Rf14.temp_log1 
Rf14.Temp2 <- Rf14.temp_log2 
Rf14.Temp3 <- Rf14.temp_log3 
Rf14.Temp4 <- Rf14.temp_log4                       
Rf14.Temp5 <- Rf14.temp_log5                       

#############################
## complete data for both sites across all periods of sampling (2014-2016)
Lil.Temp<-rbind(Lil.temp_log1,Lil.temp_log2, Lil.temp_log4, Lil.temp_log5)
write.csv(Lil.Temp, "output/Lilipuna temp all.csv")

Rf14.Temp<-rbind(Rf14.Temp1,Rf14.Temp2, Rf14.Temp3, Rf14.Temp4, Rf14.Temp5)
write.csv(Rf14.Temp, "output/Reef14 temp all.csv")

#############################

# Aggregate temperature data by daily mean, minimum, and maximum

TIMESPAN_split<-split(TIMESPAN, f=TIMESPAN$Date 
                     < as.Date("2014-10-10", format="%F"))

Lil.split1 <- split(Lil.Temp1, f=Lil.Temp1$Date 
                      < as.Date("2014-10-10", format="%F"))
Lil.split2 <- split(Lil.Temp2, f=Lil.Temp2$Date 
                    < as.Date("2014-10-10", format="%F"))
Lil.split4 <- split(Lil.Temp4, f=Lil.Temp4$Date 
                   < as.Date("2014-10-10", format="%F"))
Lil.split5 <-split(Lil.Temp5, f=Lil.Temp5$Date 
                     < as.Date("2014-10-10", format="%F"))

Rf14.split1 <- split(Rf14.Temp1, f=Rf14.Temp1$Date 
                       < as.Date("2014-10-10", format="%F"))
Rf14.split2 <- split(Rf14.Temp2, f=Rf14.Temp2$Date 
                     < as.Date("2014-10-10", format="%F"))
Rf14.split3 <- split(Rf14.Temp3, f=Rf14.Temp3$Date 
                    < as.Date("2014-10-10", format="%F"))
Rf14.split4 <- split(Rf14.Temp4, f=Rf14.Temp4$Date 
                     < as.Date("2014-10-10", format="%F"))
Rf14.split5 <- split(Rf14.Temp5, f=Rf14.Temp5$Date 
                     < as.Date("2014-10-10", format="%F"))

##########################
##########################
TIMESPAN_mean <- aggregate(data.frame(mean=TIMESPAN_split[[1]]$Temp_Calib), by=list(Date=TIMESPAN_split[[1]]$Date), FUN=mean)

# daily means
Lil_mean1 <- aggregate(data.frame(mean=Lil.split1[[1]]$Temp_Calib), by=list(Date=Lil.split1[[1]]$Date), FUN=mean)

Lil_mean2 <- aggregate(data.frame(mean=Lil.split2[[1]]$Temp_Calib), by=list(Date=Lil.split2[[1]]$Date), FUN=mean)

Lil_mean4 <- aggregate(data.frame(mean=Lil.split4[[1]]$Temp_Calib), by=list(Date=Lil.split4[[1]]$Date), FUN=mean)

Lil_mean5 <- aggregate(data.frame(mean=Lil.split5[[1]]$Temp_Calib), by=list(Date=Lil.split5[[1]]$Date), FUN=mean)

#######

Rf14_mean1 <- aggregate(data.frame(mean=Rf14.split1[[1]]$Temp_Calib), by=list(Date=Rf14.split1[[1]]$Date), FUN=mean)

Rf14_mean2 <- aggregate(data.frame(mean=Rf14.split2[[1]]$Temp_Calib), by=list(Date=Rf14.split2[[1]]$Date), FUN=mean)

Rf14_mean3 <- aggregate(data.frame(mean=Rf14.split3[[1]]$Temp_Calib), by=list(Date=Rf14.split3[[1]]$Date), FUN=mean)

Rf14_mean4 <- aggregate(data.frame(mean=Rf14.split4[[1]]$Temp_Calib), by=list(Date=Rf14.split4[[1]]$Date), FUN=mean)

Rf14_mean5 <- aggregate(data.frame(mean=Rf14.split5[[1]]$Temp_Calib), by=list(Date=Rf14.split5[[1]]$Date), FUN=mean)

# daily max temperatures
Lil_max1 <- aggregate(data.frame(max=Lil.split1[[1]]$Temp_Calib), by=list(Date=Lil.split1[[1]]$Date), FUN=max)

Lil_max2 <- aggregate(data.frame(max=Lil.split2[[1]]$Temp_Calib), by=list(Date=Lil.split2[[1]]$Date), FUN=max)

Lil_max4 <- aggregate(data.frame(max=Lil.split4[[1]]$Temp_Calib), by=list(Date=Lil.split4[[1]]$Date), FUN=max)

Lil_max5 <- aggregate(data.frame(max=Lil.split5[[1]]$Temp_Calib), by=list(Date=Lil.split5[[1]]$Date), FUN=max)

######
Rf14_max1 <- aggregate(data.frame(max=Rf14.split1[[1]]$Temp_Calib), by=list(Date=Rf14.split1[[1]]$Date), FUN=max)

Rf14_max2 <- aggregate(data.frame(max=Rf14.split2[[1]]$Temp_Calib), by=list(Date=Rf14.split2[[1]]$Date), FUN=max)

Rf14_max3 <- aggregate(data.frame(max=Rf14.split3[[1]]$Temp_Calib), by=list(Date=Rf14.split3[[1]]$Date), FUN=max)

Rf14_max4 <- aggregate(data.frame(max=Rf14.split4[[1]]$Temp_Calib), by=list(Date=Rf14.split4[[1]]$Date), FUN=max)

Rf14_max5 <- aggregate(data.frame(max=Rf14.split5[[1]]$Temp_Calib), by=list(Date=Rf14.split5[[1]]$Date), FUN=max)

# daily minimum temperature
Lil_min1 <- aggregate(data.frame(min=Lil.split1[[1]]$Temp_Calib), by=list(Date=Lil.split1[[1]]$Date), FUN=min)

Lil_min2 <- aggregate(data.frame(min=Lil.split2[[1]]$Temp_Calib), by=list(Date=Lil.split2[[1]]$Date), FUN=min)

Lil_min4 <- aggregate(data.frame(min=Lil.split4[[1]]$Temp_Calib), by=list(Date=Lil.split4[[1]]$Date), FUN=min)

Lil_min5 <- aggregate(data.frame(min=Lil.split5[[1]]$Temp_Calib), by=list(Date=Lil.split5[[1]]$Date), FUN=min)

######

Rf14_min1 <- aggregate(data.frame(min=Rf14.split1[[1]]$Temp_Calib), by=list(Date=Rf14.split1[[1]]$Date), FUN=min)

Rf14_min2 <- aggregate(data.frame(min=Rf14.split2[[1]]$Temp_Calib), by=list(Date=Rf14.split2[[1]]$Date), FUN=min)

Rf14_min3 <- aggregate(data.frame(min=Rf14.split3[[1]]$Temp_Calib), by=list(Date=Rf14.split3[[1]]$Date), FUN=min)

Rf14_min4 <- aggregate(data.frame(min=Rf14.split4[[1]]$Temp_Calib), by=list(Date=Rf14.split4[[1]]$Date), FUN=min)

Rf14_min5 <- aggregate(data.frame(min=Rf14.split5[[1]]$Temp_Calib), by=list(Date=Rf14.split5[[1]]$Date), FUN=min)


#####################
#####################
# calculate range for temperature from daily min and max

Lil_range1 <- data.frame(Lil_max1, Lil_min1$min); colnames(Lil_range1) <- c("Date","max", "min"); Lil_range1$range<-(Lil_range1$max-Lil_range1$min)

Lil_range2 <- data.frame(Lil_max2, Lil_min2$min); colnames(Lil_range2) <- c("Date","max", "min"); Lil_range2$range<-(Lil_range2$max-Lil_range2$min)

Lil_range4 <- data.frame(Lil_max4, Lil_min4$min); colnames(Lil_range4) <- c("Date","max", "min"); Lil_range4$range<-(Lil_range4$max-Lil_range4$min)

Lil_range5 <- data.frame(Lil_max5, Lil_min5$min); colnames(Lil_range5) <- c("Date","max", "min"); Lil_range5$range<-(Lil_range5$max-Lil_range5$min)

##########

Rf14_range1 <- data.frame(Rf14_max1, Rf14_min1$min); colnames(Rf14_range1) <- c("Date","max", "min"); Rf14_range1$range<-(Rf14_range1$max-Rf14_range1$min)

Rf14_range2 <- data.frame(Rf14_max2, Rf14_min2$min); colnames(Rf14_range2) <- c("Date","max", "min"); Rf14_range2$range<-(Rf14_range2$max-Rf14_range2$min)

Rf14_range3 <- data.frame(Rf14_max3, Rf14_min3$min); colnames(Rf14_range3) <- c("Date","max", "min"); Rf14_range3$range<-(Rf14_range3$max-Rf14_range3$min)

Rf14_range4 <- data.frame(Rf14_max4, Rf14_min4$min); colnames(Rf14_range4) <- c("Date","max", "min"); Rf14_range4$range<-(Rf14_range4$max-Rf14_range4$min)

Rf14_range5 <- data.frame(Rf14_max5, Rf14_min5$min); colnames(Rf14_range5) <- c("Date","max", "min"); Rf14_range5$range<-(Rf14_range5$max-Rf14_range5$min)


#####################
#####################


# Import light data

# Lilipuna
# 2014 Oct - 2016 Jun
Lil.PAR1<-rbind(
  read.csv("Data_all/Light data/Lilipuna/SN_2485/Lilipuna_2485_T1_oct-dec 2014.csv"),
  read.csv("Data_all/Light data/Lilipuna/SN_2485/Lilipuna_2485_T2_dec-feb 2015.csv"))

Lil.PAR2<-rbind(  
  read.csv("Data_all/Light data/Lilipuna/SN_2485/Lilipuna_2485_T3_feb-apr 2015.csv"),
  read.csv("Data_all/Light data/Lilipuna/SN_2485/Lilipuna_2485_T4_apr-jun 2015.csv"))

Lil.PAR4<-rbind(
  read.csv("Data_all/Light data/Lilipuna/SN_2489/Lilipuna_2489_T6_sep-nov 2015.csv"),
  read.csv("Data_all/Light data/Lilipuna/SN_2489/Lilipuna_2489_T7_nov-jan 2016.csv"),
  read.csv("Data_all/Light data/Lilipuna/SN_2489/Lilipuna_2489_T8_jan-mar 2016.csv"))

Lil.PAR5<-
  read.csv("Data_all/Light data/Lilipuna/SN_2489/Lilipuna_2489_T9_mar-jun 2016.csv")

  
# Reef 14
# 2014 Oct - 2016 Jun
Rf14.PAR1 <- rbind(
  read.csv("Data_all/Light data/Reef 14/SN_2488/Reef 14_2488_T1_oct-dec 2014.csv"),
  read.csv("Data_all/Light data/Reef 14/SN_2488/Reef 14_2488_T2_dec-feb 2015.csv"))

Rf14.PAR2 <- rbind(
  read.csv("Data_all/Light data/Reef 14/SN_2488/Reef 14_2488_T3_feb-apr 2015.csv"),
  read.csv("Data_all/Light data/Reef 14/SN_2488/Reef 14_2488_T4_apr-jun 2015.csv"))

Rf14.PAR3 <- 
  read.csv("Data_all/Light data/Reef 14/SN_2488/Reef 14_2488_T5_jun-sep 2015.csv")

Rf14.PAR4 <- rbind(
  read.csv("Data_all/Light data/Reef 14/SN_2488/Reef 14_2488_T6_sep-nov 2015.csv"),
  read.csv("Data_all/Light data/Reef 14/SN_2488/Reef 14_2488_T7_nov-jan 2016.csv"),
  read.csv("Data_all/Light data/Reef 14/SN_2488/Reef 14_2488_T8_jan-mar 2016.csv"))

Rf14.PAR5 <-
  read.csv("Data_all/Light data/Reef 14/SN_2488/Reef 14_2488_T9_apr-jun 2016.csv")


# Set date to date class
Lil.PAR1$Date <- as.Date(Lil.PAR1$Date, format="%e/%m/%Y")
Lil.PAR2$Date <- as.Date(Lil.PAR2$Date, format="%e/%m/%Y")
Lil.PAR4$Date <- as.Date(Lil.PAR4$Date, format="%e/%m/%Y")
Lil.PAR5$Date <- as.Date(Lil.PAR5$Date, format="%e/%m/%Y")

Rf14.PAR1$Date <- as.Date(Rf14.PAR1$Date, format="%e/%m/%Y")
Rf14.PAR2$Date <- as.Date(Rf14.PAR2$Date, format="%e/%m/%Y")
Rf14.PAR3$Date <- as.Date(Rf14.PAR3$Date, format="%e/%m/%Y")
Rf14.PAR4$Date <- as.Date(Rf14.PAR4$Date, format="%e/%m/%Y")
Rf14.PAR5$Date <- as.Date(Rf14.PAR5$Date, format="%e/%m/%Y")

##########################
# Recalibrate light data
# Lilipuna --- Logger SN: 2485 calibration: y = 23.674x - 48.339 <<< Jen calibration
# Lilipuna --- Logger SN: 2489 calibration: y = 89.459x
# Reef 14  --- Logger SN: 2488 calibration: y = 74.242x
##########################

#Reef 14 -- Odyssey logger 2488

Rf14.PAR1<-Rf14.PAR1[,-5]; head(Rf14.PAR1) #remove "calibrated column"
# integrate raw values over time internal (15min * 60 sec) and calibrate to LiCor
Rf14.PAR1$LiCor_Calibrated<-(Rf14.PAR1$Raw*74.242/(15*60))
  
  Rf14.PAR2<-Rf14.PAR2[,-5]; head(Rf14.PAR2)
  Rf14.PAR2$LiCor_Calibrated<-(Rf14.PAR2$Raw*74.242/(15*60))
  
    Rf14.PAR3<-Rf14.PAR3[,-5]; head(Rf14.PAR3)
    Rf14.PAR3$LiCor_Calibrated<-(Rf14.PAR3$Raw*74.242/(15*60))
    
      Rf14.PAR4<-Rf14.PAR4[,-5]; head(Rf14.PAR4)
      Rf14.PAR4$LiCor_Calibrated<-(Rf14.PAR4$Raw*74.242/(15*60))
      
         Rf14.PAR5<-Rf14.PAR5[,-5]; head(Rf14.PAR5)
         Rf14.PAR5$LiCor_Calibrated<-(Rf14.PAR5$Raw*74.242/(15*60))
         

# Lilipuna -- Odyssey logger 2485 .....calibration approximated, other values are nonsense
Lil.PAR1<-Lil.PAR1[,-5]; head(Lil.PAR1) 
Lil.PAR1$LiCor_Calibrated<-(Lil.PAR1$Raw*82.0/(15*60)); head(Lil.PAR1)
  Lil.PAR2<-Lil.PAR2[,-5]; head(Lil.PAR2)
  Lil.PAR2$LiCor_Calibrated<-(Lil.PAR2$Raw*82.0/(15*60)); head(Lil.PAR2)
  

# Lilipuna -- Odyssey logger 2489 calibration: 
Lil.PAR4<-Lil.PAR4[,-5]; head(Lil.PAR4)
Lil.PAR4$LiCor_Calibrated<-(Lil.PAR4$Raw*89.459/(15*60)); head(Lil.PAR4)
  Lil.PAR5<-Lil.PAR5[,-5]; head(Lil.PAR5)
  Lil.PAR5$LiCor_Calibrated<-(Lil.PAR5$Raw*89.459/(15*60)); head(Lil.PAR5)


# Combine all calibrated data for Lilipuna, Reef 14
Lil.PAR  <- rbind(Lil.PAR1, Lil.PAR2, Lil.PAR4, Lil.PAR5)
write.csv(Lil.PAR, "output/Lilipuna PAR all.csv")

Rf14.PAR <- rbind(Rf14.PAR1, Rf14.PAR2, Rf14.PAR3, Rf14.PAR4, Rf14.PAR5)
write.csv(Rf14.PAR, "output/Reef14 PAR all.csv")

#######################
#######################
# compiled data files

Lil.Temp  # Lilipuna temp data
Lil.PAR   # Lilipuna light data

Rf14.Temp # Reef 14 temp data
Rf14.PAR  # Reef 14 light data


#####
df<-Lil.PAR[c(2081:4361), ] # just values from November 2014 - January 2015 ( OA x light experiment)
df2 <- aggregate(data.frame(max.PAR=df$LiCor_Calibrated), by=list(Date=df$Date), FUN=max) # max PAR for this period, each day
min(df2$max.PAR) # what is minimum "peak irradiance" = 291
max(df2$max.PAR) # what is maximum "peak irradiance" = 1071
mean(df2$max.PAR) # what is average "peak irradiance" = 539
std.error(df2$max.PAR) # what is average "peak irradiance" = 539
######


########################

# Calculate daily light integrals
# Lilipuna
Lil.L1 <- aggregate(data.frame(mean=Lil.PAR1$LiCor_Calibrated), by=list(Date=Lil.PAR1$Date), FUN=mean)
Lil.L1$dli <- Lil.L1$mean * 0.0864 # Convert to mol.m2.d (daily light integral)

Lil.L2 <- aggregate(data.frame(mean=Lil.PAR2$LiCor_Calibrated), by=list(Date=Lil.PAR2$Date), FUN=mean); Lil.L2$dli <- Lil.L2$mean * 0.0864

Lil.L4 <- aggregate(data.frame(mean=Lil.PAR4$LiCor_Calibrated), by=list(Date=Lil.PAR4$Date), FUN=mean); Lil.L4$dli <- Lil.L4$mean * 0.0864

Lil.L5 <- aggregate(data.frame(mean=Lil.PAR5$LiCor_Calibrated), by=list(Date=Lil.PAR5$Date), FUN=mean); Lil.L5$dli <- Lil.L5$mean * 0.0864


#Reef 14
Rf14.L1 <- aggregate(data.frame(mean=Rf14.PAR1$LiCor_Calibrated), by=list(Date=Rf14.PAR1$Date), FUN=mean); Rf14.L1$dli <- Rf14.L1$mean * 0.0864

Rf14.L2 <- aggregate(data.frame(mean=Rf14.PAR2$LiCor_Calibrated), by=list(Date=Rf14.PAR2$Date), FUN=mean); Rf14.L2$dli <- Rf14.L2$mean * 0.0864

Rf14.L3 <- aggregate(data.frame(mean=Rf14.PAR3$LiCor_Calibrated), by=list(Date=Rf14.PAR3$Date), FUN=mean); Rf14.L3$dli <- Rf14.L3$mean * 0.0864

Rf14.L4 <- aggregate(data.frame(mean=Rf14.PAR4$LiCor_Calibrated), by=list(Date=Rf14.PAR4$Date), FUN=mean); Rf14.L4$dli <- Rf14.L4$mean * 0.0864

Rf14.L5 <- aggregate(data.frame(mean=Rf14.PAR5$LiCor_Calibrated), by=list(Date=Rf14.PAR5$Date), FUN=mean); Rf14.L5$dli <- Rf14.L5$mean * 0.0864


###########################
###########################

# Plot daily light integrals

par(mfrow=c(2,1), mar=c(2,3,1,1), mgp=c(2,0.5,0))
k=1; lwd=1 # k-day moving averages
plot(mean ~ Date, TIMESPAN_mean, type="n", ylab=(expression(paste("DLI mol", ~m^-2, ~d^-1, sep=""))), xaxt="n", xlab="", ylim=c(0,40))
mtext(expression(bold("B")), 2, adj=4.5, las=1, padj=-8)
axis.Date(1, at=seq(min(TIMESPAN_mean$Date), max(TIMESPAN_mean$Date), by="1 mon"), format="%b '%y")
legend("topleft", lty=1, col=c(reefcols[1:2], "darkgray"), legend=c("Reef 14","Lilipuna"), lwd=2, bty="n")
with(na.omit(data.frame(date=Lil.L1$Date, dli=rollmean(Lil.L1$dli, k, fill=NA))), lines(date, dli, col=reefcols[2], lwd=lwd))
with(na.omit(data.frame(date=Lil.L2$Date, dli=rollmean(Lil.L2$dli, k, fill=NA))), lines(date, dli, col=reefcols[2], lwd=lwd))
with(na.omit(data.frame(date=Lil.L4$Date, dli=rollmean(Lil.L4$dli, k, fill=NA))), lines(date, dli, col=reefcols[2], lwd=lwd))
with(na.omit(data.frame(date=Lil.L5$Date, dli=rollmean(Lil.L5$dli, k, fill=NA))), lines(date, dli, col=reefcols[2], lwd=lwd))
with(na.omit(data.frame(date=Rf14.L1$Date, dli=rollmean(Rf14.L1$dli, k, fill=NA))), lines(date, dli, col=reefcols[1], lwd=lwd))
with(na.omit(data.frame(date=Rf14.L2$Date, dli=rollmean(Rf14.L2$dli, k, fill=NA))), lines(date, dli, col=reefcols[1], lwd=lwd))
with(na.omit(data.frame(date=Rf14.L3$Date, dli=rollmean(Rf14.L3$dli, k, fill=NA))), lines(date, dli, col=reefcols[1], lwd=lwd))
with(na.omit(data.frame(date=Rf14.L4$Date, dli=rollmean(Rf14.L4$dli, k, fill=NA))), lines(date, dli, col=reefcols[1], lwd=lwd))
with(na.omit(data.frame(date=Rf14.L5$Date, dli=rollmean(Rf14.L5$dli, k, fill=NA))), lines(date, dli, col=reefcols[1], lwd=lwd))


#####################
#####################
# Plot temp. daily mean temperatures for each reef
#pdf(file="output/FigureS2.pdf",  width = 6.65354, height=5)


#######################
#######################
# Plot temp. daily mean temperatures for each reef
#######################
#######################

par(mfrow=c(2,1), mar=c(2,3,1,1), mgp=c(2,0.5,0))
k=1; lwd=1 # k-day moving averages
plot(mean ~ Date, TIMESPAN_mean, type="n", ylab="Temperature (°C)", ylim=c(21, 31), xaxt="n", xlab="")
mtext(expression(bold("A")), 2, adj=4.5, las=1, padj=-8)
axis.Date(1, at=seq(min(TIMESPAN_mean$Date), max(TIMESPAN_mean$Date), by="1 mon"), format="%b '%y")
legend("topright", lty=1, col=c(reefcols[1:2], "darkgray"), legend=c("Reef 14","Lilipuna"), lwd=2, bty="n")
with(na.omit(data.frame(date=Rf14_mean1$Date, mean=rollmean(Rf14_mean1$mean, k, fill=NA))), { 
  lines(date, mean, col=reefcols[1], lwd=1.5) 
})
with(na.omit(data.frame(date=Rf14_mean2$Date, mean=rollmean(Rf14_mean2$mean, k, fill=NA))), { 
  lines(date, mean, col=reefcols[1], lwd=1.5) 
})
with(na.omit(data.frame(date=Rf14_mean3$Date, mean=rollmean(Rf14_mean3$mean, k, fill=NA))), { 
  lines(date, mean, col=reefcols[1], lwd=1.5)
})
with(na.omit(data.frame(date=Rf14_mean4$Date, mean=rollmean(Rf14_mean4$mean, k, fill=NA))), { 
  lines(date, mean, col=reefcols[1], lwd=1.5)
})
with(na.omit(data.frame(date=Rf14_mean5$Date, mean=rollmean(Rf14_mean5$mean, k, fill=NA))), { 
  lines(date, mean, col=reefcols[1], lwd=1.5) 
})
with(na.omit(data.frame(date=Lil_mean1$Date, mean=rollmean(Lil_mean1$mean, k, fill=NA))), {
  lines(date, mean, col=reefcols[2], lwd=1.5)
})
with(na.omit(data.frame(date=Lil_mean2$Date, mean=rollmean(Lil_mean2$mean, k, fill=NA))), {
  lines(date, mean, col=reefcols[2], lwd=1.5)
})
with(na.omit(data.frame(date=Lil_mean4$Date, mean=rollmean(Lil_mean4$mean, k, fill=NA))), { 
  lines(date, mean, col=reefcols[2], lwd=1.5) 
})
with(na.omit(data.frame(date=Lil_mean5$Date, mean=rollmean(Lil_mean5$mean, k, fill=NA))), { 
  lines(date, mean, col=reefcols[2], lwd=1.5) 
})

#######################
#######################
# plot daily temp range for each site
#######################
#######################
k=1; lwd=1 # k-day moving averages
plot(mean ~ Date, TIMESPAN_mean, type="n", ylab="Temperature (°C)", ylim=c(0, 3), xaxt="n", xlab="")
mtext(expression(bold("A")), 2, adj=4.5, las=1, padj=-8)
axis.Date(1, at=seq(min(TIMESPAN_mean$Date), max(TIMESPAN_mean$Date), by="1 mon"), format="%b '%y")
legend("topright", lty=1, col=c(reefcols[1:2], "darkgray"), legend=c("Reef 14","Lilipuna"), lwd=2, bty="n")
with(na.omit(data.frame(date=Rf14_range1$Date, range=rollmean(Rf14_range1$range, k, fill=NA))), { 
  lines(date, range, col=reefcols[1], lwd=1.5) 
})
with(na.omit(data.frame(date=Rf14_range2$Date, range=rollmean(Rf14_range2$range, k, fill=NA))), { 
  lines(date, range, col=reefcols[1], lwd=1.5)
})
with(na.omit(data.frame(date=Rf14_range3$Date, range=rollmean(Rf14_range3$range, k, fill=NA))), { 
  lines(date, range, col=reefcols[1], lwd=1.5)
})
with(na.omit(data.frame(date=Rf14_range4$Date, range=rollmean(Rf14_range4$range, k, fill=NA))), { 
  lines(date, range, col=reefcols[1], lwd=1.5)
})
with(na.omit(data.frame(date=Rf14_range5$Date, range=rollmean(Rf14_range5$range, k, fill=NA))), { 
  lines(date, range, col=reefcols[1], lwd=1.5)
})
with(na.omit(data.frame(date=Lil_range1$Date, range=rollmean(Lil_range1$range, k, fill=NA))), {
  lines(date, range, col=reefcols[2], lwd=1.5)
})
with(na.omit(data.frame(date=Lil_range2$Date, range=rollmean(Lil_range2$range, k, fill=NA))), {
  lines(date, range, col=reefcols[2], lwd=1.5)
})
with(na.omit(data.frame(date=Lil_range4$Date, range=rollmean(Lil_range4$range, k, fill=NA))), { 
  lines(date, range, col=reefcols[2], lwd=1.5) 
})
with(na.omit(data.frame(date=Lil_range5$Date, range=rollmean(Lil_range5$range, k, fill=NA))), {
  lines(date, range, col=reefcols[2], lwd=1.5)
})

## are the ranges that different?
# combined dataframes
Rf.14.ranges<-rbind(Rf14_range1[,c(1,4)], Rf14_range2[,c(1,4)],Rf14_range3[,c(1,4)],Rf14_range4[,c(1,4)], Rf14_range5[,c(1,4)])

Lil.ranges<-rbind(Lil_range1[,c(1,4)], Lil_range2[,c(1,4)],Lil_range4[,c(1,4)], Lil_range5[,c(1,4)]); head(Lil.ranges)

# what is the mean range
mean(Lil.ranges$range) # 1.11 C
mean(Rf.14.ranges$range) # 0.92 C

#######################
#######################
# plot daily MINIMUM temp range for each site
#######################
#######################
k=1; lwd=1 # k-day moving averages
plot(mean ~ Date, TIMESPAN_mean, type="n", ylab="Temperature (°C)", ylim=c(21, 31), xaxt="n", xlab="")
mtext(expression(bold("A")), 2, adj=4.5, las=1, padj=-8)
axis.Date(1, at=seq(min(TIMESPAN_mean$Date), max(TIMESPAN_mean$Date), by="1 mon"), format="%b '%y")
legend("topright", lty=1, col=c(reefcols[1:2], "darkgray"), legend=c("Reef 14","Lilipuna"), lwd=2, bty="n")
with(na.omit(data.frame(date=Rf14_min1$Date, min=rollmean(Rf14_min1$min, k, fill=NA))), { 
  lines(date, min, col=reefcols[1], lwd=1.5) 
})
with(na.omit(data.frame(date=Rf14_min2$Date, min=rollmean(Rf14_min2$min, k, fill=NA))), { 
  lines(date, min, col=reefcols[1], lwd=1.5)
})
with(na.omit(data.frame(date=Rf14_min3$Date, min=rollmean(Rf14_min3$min, k, fill=NA))), { 
  lines(date, min, col=reefcols[1], lwd=1.5)
})
with(na.omit(data.frame(date=Rf14_min4$Date, min=rollmean(Rf14_min4$min, k, fill=NA))), { 
  lines(date, min, col=reefcols[1], lwd=1.5)
})
with(na.omit(data.frame(date=Rf14_min5$Date, min=rollmean(Rf14_min5$min, k, fill=NA))), { 
  lines(date, min, col=reefcols[1], lwd=1.5)
})
with(na.omit(data.frame(date=Lil_min1$Date, min=rollmean(Lil_min1$min, k, fill=NA))), {
  lines(date, min, col=reefcols[2], lwd=1.5)
})
with(na.omit(data.frame(date=Lil_min2$Date, min=rollmean(Lil_min2$min, k, fill=NA))), {
  lines(date, min, col=reefcols[2], lwd=1.5)
})
with(na.omit(data.frame(date=Lil_min4$Date, min=rollmean(Lil_min4$min, k, fill=NA))), { 
  lines(date, min, col=reefcols[2], lwd=1.5) 
})
with(na.omit(data.frame(date=Lil_min5$Date, min=rollmean(Lil_min5$min, k, fill=NA))), { 
  lines(date, min, col=reefcols[2], lwd=1.5) 
})


#######################
#######################
# plot daily MAXIMUM temp range for each site
#######################
#######################
k=1; lwd=1 # k-day moving averages
plot(mean ~ Date, TIMESPAN_mean, type="n", ylab="Temperature (°C)", ylim=c(21, 31), xaxt="n", xlab="")
mtext(expression(bold("A")), 2, adj=4.5, las=1, padj=-8)
axis.Date(1, at=seq(min(TIMESPAN_mean$Date), max(TIMESPAN_mean$Date), by="1 mon"), format="%b '%y")
legend("topright", lty=1, col=c(reefcols[1:2], "darkgray"), legend=c("Reef 14","Lilipuna"), lwd=2, bty="n")
with(na.omit(data.frame(date=Rf14_max1$Date, max=rollmean(Rf14_max1$max, k, fill=NA))), { 
  lines(date, max, col=reefcols[1], lwd=1.5) 
})
with(na.omit(data.frame(date=Rf14_max2$Date, max=rollmean(Rf14_max2$max, k, fill=NA))), { 
  lines(date, max, col=reefcols[1], lwd=1.5) 
})
with(na.omit(data.frame(date=Rf14_max3$Date, max=rollmean(Rf14_max3$max, k, fill=NA))), { 
  lines(date, max, col=reefcols[1], lwd=1.5)
})
with(na.omit(data.frame(date=Rf14_max4$Date, max=rollmean(Rf14_max4$max, k, fill=NA))), { 
  lines(date, max, col=reefcols[1], lwd=1.5)
})
with(na.omit(data.frame(date=Rf14_max5$Date, max=rollmean(Rf14_max5$max, k, fill=NA))), { 
  lines(date, max, col=reefcols[1], lwd=1.5) 
})
with(na.omit(data.frame(date=Lil_max1$Date, max=rollmean(Lil_max1$max, k, fill=NA))), {
  lines(date, max, col=reefcols[2], lwd=1.5)
})
with(na.omit(data.frame(date=Lil_max2$Date, max=rollmean(Lil_max2$max, k, fill=NA))), {
  lines(date, max, col=reefcols[2], lwd=1.5)
})
with(na.omit(data.frame(date=Lil_max4$Date, max=rollmean(Lil_max4$max, k, fill=NA))), { 
  lines(date, max, col=reefcols[2], lwd=1.5) 
})
with(na.omit(data.frame(date=Lil_max5$Date, max=rollmean(Lil_max5$max, k, fill=NA))), {
  lines(date, max, col=reefcols[2], lwd=1.5)
})


#############################################
#############################################
#############################################
#############################################

# testing for differences among reefs

## DAILY INTEGRATED LIGHT
# Light data dli
# Lil.PAR1-2,4-5  and Rf14.PAR1-5
Lil.PAR.dli.all<-rbind(Lil.L1, Lil.L2, Lil.L4, Lil.L5)
Rf14.PAR.dli.all<-rbind(Rf14.L1, Rf14.L2, Rf14.L3, Rf14.L4, Rf14.L5)

test.light<- rbind(
  data.frame(date=Lil.PAR.dli.all$Date, reef="Lilipuna", dli=Lil.PAR.dli.all$dli),
  data.frame(date=Rf14.PAR.dli.all$Date, reef="Reef 14", dli=Rf14.PAR.dli.all$dli))
lmod1<-lmer(dli~reef + (1|date), data=test.light)
Anova(lmod1, type=2)
lsmeans(lmod1, "reef", contr="pairwise")

# test for temperature
# compiled files = Lil.Temp and Rf14.Temp
# compiles means = Lilipuna = Lil_mean1, Lil_mean2, Lil_mean4, Lil_mean5
#                = Reef 14 = Rf14_mean1, Rf14_mean2, Rf14_mean3, Rf14_mean4, Rf14_mean5

# removing Reef 14_mean 3 and aligning mean4 gives a balanced dataframe with means for each day matched among sites

Rf14_mean4.amend<-Rf14_mean4[(Rf14_mean4$Date>="2015-09-28"),]

Lil.tempmean.all <-rbind(Lil_mean1, Lil_mean2, Lil_mean4, Lil_mean5)
Rf14.tempmean.all<-rbind(Rf14_mean1, Rf14_mean2, Rf14_mean4.amend, Rf14_mean5)


test.temp<- rbind(
  data.frame(date=Lil.tempmean.all$Date, reef="Lilipuna", temp=Lil.tempmean.all$mean),
  data.frame(date=Rf14.tempmean.all$Date, reef="Reef 14", temp=Rf14.tempmean.all$mean))

lmod2<-lmer(temp~reef + (1|date), data=test.temp)
Anova(lmod2, type=2)
lsmeans(lmod2, "reef", contr="pairwise")

head(test.temp)
aggregate(temp~reef, data=test.temp,mean)
min.temp<-aggregate(temp~reef, data=test.temp, min)
max.temp<-aggregate(temp~reef, data=test.temp, max)

temps<-cbind(Lil.tempmean.all, Rf14.tempmean.all[c(0,2)]); colnames(temps)<-c("Date", "Lil.temp", "Rf14.temp")
temps$temp.diff<-temps$Lil.temp-temps$Rf14.temp

plot(temps$temp.diff~temps$Date, pch=21, cex=1, xaxt="n", xlab="Date", ylab="Temperature (°C)", main="Differences in daily mean temperature at Lilipuna relative to Reef 14")
axis.Date(1, at=seq(min(temps$Date), max(temps$Date), by="1 mon"), format="%b '%y")
abline(0,0, lty=2, lwd=2, col="red")


## are the ranges that different?
# combined dataframes
Rf.14.ranges<-rbind(Rf14_range1[,c(1,4)], Rf14_range2[,c(1,4)],Rf14_range3[,c(1,4)],Rf14_range4[,c(1,4)], Rf14_range5[,c(1,4)])

Lil.ranges<-rbind(Lil_range1[,c(1,4)], Lil_range2[,c(1,4)],Lil_range4[,c(1,4)], Lil_range5[,c(1,4)]); head(Lil.ranges)


plot(Rf.14.ranges$range~Rf.14.ranges$Date, pch=21, cex=1, xaxt="n", xlab="Date", ylab="Temperature (°C)", main="Daily temperature variance at Lilipuna and Reef 14")
axis.Date(1, at=seq(min(temps$Date), max(temps$Date), by="1 mon"), format="%b '%y")
with(points(Lil.ranges$range~Lil.ranges$Date, pch=19, cex=0.5, xaxt="n", col="red"))
legend("topright", legend=c("Reef 14", "Lilipuna"), col=c("black", "red"), lwd=1,inset=c(-0.1, -0.1), pch=19, cex=1, bty="n", x.intersp=0.4, y.intersp=0.7)


