## qPCR script

#clear work space
rm(list=ls()) 
ls()

getwd() 

# --------------------------------------------------------------------------
# • Load data --------------------------------------------------------------
# --------------------------------------------------------------------------

# Use steponeR to import data and calculate proporation of C and D symbionts
install.packages("devtools")
library(devtools)
source_url("https://raw.githubusercontent.com/jrcunning/steponeR/master/steponeR.R")
Mcap.plates <- list.files(path="data/qPCR", pattern = "txt$", full.names = T); Mcap.plates
Mcap <- steponeR(files=Mcap.plates, delim="\t",
                 target.ratios=c("C.D"),
                 fluor.norm=list(C=2.26827, D=0),
                 copy.number=list(C=33, D=3),
                 ploidy=list(C=1, D=1), 
                 extract=list(C=0.813, D=0.813))

Mcap <- Mcap$result
head(Mcap)

## will split into 3 columns, name them, and add to dataframe
## dataframe needs packagin to new df to remove redundant or unnecessary columns
Mcap<-cbind(Mcap, colsplit(Mcap$Sample.Name, pattern= "-", c("Status", "Year", "Sample.ID")))
colnames(Mcap)

Mcap<-Mcap[c(10:12, 1:9)]; head(Mcap)

# remove TEC, BLANK, NaN, TE, NT, EMPTY, NEC3, NONE, BL, TE BLANK, + DNAB, NTC
Mcap <- Mcap[grep("TEC", Mcap$Sample.Name, fixed=T, invert = T), ]
Mcap <- Mcap[grep("BLANK", Mcap$Sample.Name, fixed=T, invert = T), ]
Mcap <- Mcap[grep("NaN", Mcap$Sample.Name, fixed=T, invert = T), ]
Mcap <- Mcap[grep("TE", Mcap$Sample.Name, fixed=T, invert = T), ]
Mcap <- Mcap[grep("NT", Mcap$Sample.Name, fixed=T, invert = T), ]
Mcap <- Mcap[grep("EMPTY", Mcap$Sample.Name, fixed=T, invert = T), ]
Mcap <- Mcap[grep("NEC3", Mcap$Sample.Name, fixed=T, invert = T), ]
Mcap <- Mcap[grep("NONE", Mcap$Sample.Name, fixed=T, invert = T), ]
Mcap <- Mcap[grep("BL", Mcap$Sample.Name, fixed=T, invert = T), ]
Mcap <- Mcap[grep("TE BLANK", Mcap$Sample.Name, fixed=T, invert = T), ]
Mcap <- Mcap[grep("+", Mcap$Sample.Name, fixed=T, invert = T), ]
Mcap <- Mcap[grep("DNAB", Mcap$Sample.Name, fixed=T, invert = T), ]
Mcap <- Mcap[grep("NTC", Mcap$Sample.Name, fixed=T, invert = T), ]
Mcap <- Mcap[grep("6.1", Mcap$Sample.ID, fixed=T, invert = T), ]
Mcap <- Mcap[grep("2.1", Mcap$Sample.Name, fixed=T, invert = T), ]
Mcap <- Mcap[grep("2.10", Mcap$Sample.Name, fixed=T, invert = T), ]
Mcap <- Mcap[grep("2.25", Mcap$Sample.Name, fixed=T, invert = T), ]
Mcap <- Mcap[grep("2.5", Mcap$Sample.Name, fixed=T, invert = T), ]
Mcap <- Mcap[grep("6.1", Mcap$Sample.Name, fixed=T, invert = T), ]
Mcap <- Mcap[grep("6.10", Mcap$Sample.Name, fixed=T, invert = T), ]
Mcap <- Mcap[grep("6.25", Mcap$Sample.Name, fixed=T, invert = T), ]
Mcap <- Mcap[grep("6.5", Mcap$Sample.Name, fixed=T, invert = T), ]
Mcap <- Mcap[grep("7.1", Mcap$Sample.Name, fixed=T, invert = T), ]
Mcap <- Mcap[grep("7.10", Mcap$Sample.Name, fixed=T, invert = T), ]
Mcap <- Mcap[grep("7.25", Mcap$Sample.Name, fixed=T, invert = T), ]
Mcap <- Mcap[grep("7.5", Mcap$Sample.Name, fixed=T, invert = T), ]
Mcap <- Mcap[grep("ddH2O", Mcap$Sample.Name, fixed=T, invert = T), ]

# to remove any early-amplification CT noise
Mcap$C.CT.mean[which(Mcap$C.CT.mean < 15)] <- 0
Mcap$D.CT.mean[which(Mcap$D.CT.mean < 15)] <- 0

# If C or D only detected in one technical replicate, set C:D ratio to...
Mcap$C.D[which(Mcap$D.reps==1)] <- 1 # where 1 = 100% C 
Mcap$C.D[which(Mcap$D.reps==0)] <- 1 # where 0 = 100% C 
Mcap$C.D[which(Mcap$C.reps==1)] <- 0 # where 0 = 100% D
Mcap$C.D[which(Mcap$C.reps==0)] <- 0 # where 0 = 100% D

#Remove failed samples, i.e., those where either C or D were NOT found in both reps
Mcap$fail <- ifelse(Mcap$C.reps < 2 & Mcap$D.reps < 2, TRUE, FALSE)
fails <- Mcap[Mcap$fail==TRUE, ]
Mcap <- Mcap[which(Mcap$fail==FALSE),]

# replace CT means with 'NA' as zero
Mcap$C.CT.mean[is.na(Mcap$C.CT.mean)] <-0
Mcap$D.CT.mean[is.na(Mcap$D.CT.mean)] <-0

Mcap$C.D[is.na(Mcap$C.D)] <- 1 # sets all infinity (= 100% C) to 1.0

#---------------------------------------
#---------------------------------------
# caluclate proportion C and proprtion D where C and D are both present
Mcap$propC<- Mcap$C.D / (Mcap$C.D + 1)
Mcap$propD<- 1 / (Mcap$C.D + 1)

# where C and D are not cooccuring...
# if C.D = 1 = 100% C, make 'PropC' = 1 and 'PropD' = 0
# if C.D = 0 = 100% D, make 'PropD' = 1 and 'PropC' = 0
Mcap$propC[which(Mcap$C.D==1)] <- 1
Mcap$propD[which(Mcap$propC==1)] <- 0

Mcap$propD[which(Mcap$C.D==0)] <- 1

#---------------------------------------
#---------------------------------------

# calculate FOUR COMMUNITY categories: C, C>D, D>C, D
Mcap$syms <- factor(ifelse(Mcap$propC > Mcap$propD, ifelse(Mcap$propD!= 0, "CD", "C"), ifelse(Mcap$propD > Mcap$propC, ifelse(Mcap$propC!=0, "DC", "D"), NA)), levels=c("C", "CD", "DC", "D"))

# Identify SINGLE dominant symbiont clade: C or D
Mcap$dom <- factor(substr(as.character(Mcap$syms), 1, 1))

# Set zeros to NA to facilitate log transformation
Mcap$propC[which(Mcap$propC==0)] <- NA
Mcap$propD[which(Mcap$propD==0)] <- NA

###########################
# Bleaching and Recovery time periods
###########################

# remove nonpertinent samples if nececcary 
#Mcap.field <- Mcap[grep("T3Hot", Mcap$Status, fixed=T, invert = T), ] # remove hot lab study samples
#Mcap.field <- Mcap.field[grep("T1Hot", Mcap$Status, fixed=T, invert = T), ] # remove hot lab study samples

Mcap.field<-Mcap
# remove NAs and high error samples
Mcap.field<-Mcap.field[!(Mcap.field$Sample.ID=="NA"),] # remove NA samples

### order data
Mcap.field<-Mcap.field[order(Mcap.field$Year, Mcap.field$Status, Mcap.field$Sample.ID),]

#write.csv(Mcap.field, "Mydlarz.labfield.qPCR.csv")

# separate dataframes for each event
Lab.2014<-Mcap.field[(Mcap.field$Year=="Feb2014"),]

Bleach.2014<-Mcap.field[(Mcap.field$Year=="2014" & Mcap.field$Status=="Bleaching"),]
Recov.2015<-Mcap.field[(Mcap.field$Year=="2015" & Mcap.field$Status=="Recovery"),]

Bleach.2015<-Mcap.field[(Mcap.field$Year=="2015" & Mcap.field$Status=="Bleaching"),]
Recov.2016<-Mcap.field[(Mcap.field$Year=="2016" & Mcap.field$Status=="Recovery"),]


######## look for duplicates in dataset by year and type of event (bleach/recover)

##########################
#### 2014 Lab duplicates
Lab.2014[duplicated(Lab.2014$Sample.ID), ]

# removes
Lab.2014<-Lab.2014[!(Lab.2014$Sample.ID=="R14_37" & Lab.2014$File.Name=="Wall_qPCR_plate11.txt"),] 
Lab.2014<-Lab.2014[!(Lab.2014$Sample.ID=="R14_43" & Lab.2014$File.Name=="Wall_qPCR_plate11.txt"),] 
Lab.2014<-Lab.2014[!(Lab.2014$Sample.ID=="Lil_46" & Lab.2014$File.Name=="Wall_qPCR_plate11.txt"),] 
Lab.2014<-Lab.2014[!(Lab.2014$Sample.ID=="Lil_47" & Lab.2014$File.Name=="Wall_qPCR_plate11.txt"),] 
Lab.2014<-Lab.2014[!(Lab.2014$Sample.ID=="Lil_32" & Lab.2014$File.Name=="Wall_qPCR_plate12.txt"),] 
Lab.2014<-Lab.2014[!(Lab.2014$Sample.ID=="Lil_5" & Lab.2014$File.Name=="Wall_qPCR_plate12.txt"),] 
Lab.2014<-Lab.2014[!(Lab.2014$Sample.ID=="R14_3" & Lab.2014$File.Name=="Wall_qPCR_plate11.txt"),] 

##########################
#### 2014 bleaching duplicates
Bleach.2014[duplicated(Bleach.2014$Sample.ID), ]

# removes
Bleach.2014<-Bleach.2014[!(Bleach.2014$Sample.ID=="16" & Bleach.2014$File.Name=="Wall_qPCR_plate12.txt"),]   
Bleach.2014<-Bleach.2014[!(Bleach.2014$Sample.ID=="30" & Bleach.2014$File.Name=="Wall_qPCR_plate12.txt"),]  
Bleach.2014<-Bleach.2014[!(Bleach.2014$Sample.ID=="6" & Bleach.2014$File.Name=="Wall_qPCR_plate1.txt"),]  
Bleach.2014<-Bleach.2014[!(Bleach.2014$Sample.ID=="64" & Bleach.2014$File.Name=="Wall_qPCR_plate12.txt"),] 
Bleach.2014<-Bleach.2014[!(Bleach.2014$Sample.ID=="80" & Bleach.2014$File.Name=="Wall_qPCR_plate2.txt"),] 
Bleach.2014<-Bleach.2014[!(Bleach.2014$Sample.ID=="81" & Bleach.2014$File.Name=="Wall_qPCR_plate2.txt"),] 
Bleach.2014<-Bleach.2014[!(Bleach.2014$Sample.ID=="85" & Bleach.2014$File.Name=="Wall_qPCR_plate2.txt"),] 
Bleach.2014<-Bleach.2014[!(Bleach.2014$Sample.ID=="9" & Bleach.2014$File.Name=="Wall_qPCR_plate1.txt"),] 


##########################
# 2014 recovery duplicates
Recov.2015[duplicated(Recov.2015$Sample.ID), ] 


# removes
Recov.2015<-Recov.2015[!(Recov.2015$Sample.ID=="15" & Recov.2015$File.Name=="Wall_qPCR_plate12.txt"),] 
Recov.2015<-Recov.2015[!(Recov.2015$Sample.ID=="22" & Recov.2015$File.Name=="Wall_qPCR_plate12.txt"),] 
Recov.2015<-Recov.2015[!(Recov.2015$Sample.ID=="24" & Recov.2015$File.Name=="Wall_qPCR_plate12.txt"),]
Recov.2015<-Recov.2015[!(Recov.2015$Sample.ID=="26" & Recov.2015$File.Name=="Wall_qPCR_plate12.txt"),]
Recov.2015<-Recov.2015[!(Recov.2015$Sample.ID=="32" & Recov.2015$File.Name=="Wall_qPCR_plate3.txt"),]
Recov.2015<-Recov.2015[!(Recov.2015$Sample.ID=="5" & Recov.2015$File.Name=="Wall_qPCR_plate3.txt"),]


##########################
# 2015 bleaching duplicates
Bleach.2015[duplicated(Bleach.2015$Sample.ID), ]


# removes
Bleach.2015<-Bleach.2015[!(Bleach.2015$Sample.ID=="10" & Bleach.2015$File.Name=="Wall_qPCR_plate5.txt"),]
Bleach.2015<-Bleach.2015[!(Bleach.2015$Sample.ID=="18" & Bleach.2015$File.Name=="Wall_qPCR_plate9.txt"),] 
Bleach.2015<-Bleach.2015[!(Bleach.2015$Sample.ID=="22" & Bleach.2015$File.Name=="Wall_qPCR_plate6.txt"),] 
Bleach.2015<-Bleach.2015[!(Bleach.2015$Sample.ID=="30" & Bleach.2015$File.Name=="Wall_qPCR_plate6.txt"),]
Bleach.2015<-Bleach.2015[!(Bleach.2015$Sample.ID=="36" & Bleach.2015$File.Name=="Wall_qPCR_plate9.txt"),]
Bleach.2015<-Bleach.2015[!(Bleach.2015$Sample.ID=="54" & Bleach.2015$File.Name=="Wall_qPCR_plate5.txt"),]
Bleach.2015<-Bleach.2015[!(Bleach.2015$Sample.ID=="58" & Bleach.2015$File.Name=="Wall_qPCR_plate5.txt"),]
Bleach.2015<-Bleach.2015[!(Bleach.2015$Sample.ID=="58" & Bleach.2015$File.Name=="Wall_qPCR_plate9.txt"),]
Bleach.2015<-Bleach.2015[!(Bleach.2015$Sample.ID=="60" & Bleach.2015$File.Name=="Wall_qPCR_plate5.txt"),]
Bleach.2015<-Bleach.2015[!(Bleach.2015$Sample.ID=="61" & Bleach.2015$File.Name=="Wall_qPCR_plate5.txt"),]
Bleach.2015<-Bleach.2015[!(Bleach.2015$Sample.ID=="62" & Bleach.2015$File.Name=="Wall_qPCR_plate5.txt"),]
Bleach.2015<-Bleach.2015[!(Bleach.2015$Sample.ID=="62" & Bleach.2015$File.Name=="Wall_qPCR_plate9.txt"),]

##########################
# 2016 recovery duplicates
Recov.2016[duplicated(Recov.2016$Sample.ID), ]


# removed
Recov.2016<-Recov.2016[!(Recov.2016$Sample.ID=="1" & Recov.2016$File.Name=="Wall_qPCR_plate7.txt"),]
Recov.2016<-Recov.2016[!(Recov.2016$Sample.ID=="2" & Recov.2016$File.Name=="Wall_qPCR_plate7.txt"),]
Recov.2016<-Recov.2016[!(Recov.2016$Sample.ID=="3" & Recov.2016$File.Name=="Wall_qPCR_plate7.txt"),]
Recov.2016<-Recov.2016[!(Recov.2016$Sample.ID=="30" & Recov.2016$File.Name=="Wall_qPCR_plate9.txt"),]
Recov.2016<-Recov.2016[!(Recov.2016$Sample.ID=="34" & Recov.2016$File.Name=="Wall_qPCR_plate9.txt"),]
Recov.2016<-Recov.2016[!(Recov.2016$Sample.ID=="38" & Recov.2016$File.Name=="Wall_qPCR_plate9.txt"),]
Recov.2016<-Recov.2016[!(Recov.2016$Sample.ID=="39" & Recov.2016$File.Name=="Wall_qPCR_plate9.txt"),]
Recov.2016<-Recov.2016[!(Recov.2016$Sample.ID=="45" & Recov.2016$File.Name=="Wall_qPCR_plate7.txt"),]
Recov.2016<-Recov.2016[!(Recov.2016$Sample.ID=="52" & Recov.2016$File.Name=="Wall_qPCR_plate9.txt"),]
Recov.2016<-Recov.2016[!(Recov.2016$Sample.ID=="54" & Recov.2016$File.Name=="Wall_qPCR_plate7.txt"),]
Recov.2016<-Recov.2016[!(Recov.2016$Sample.ID=="55" & Recov.2016$File.Name=="Wall_qPCR_plate7.txt"),]
Recov.2016<-Recov.2016[!(Recov.2016$Sample.ID=="58" & Recov.2016$File.Name=="Wall_qPCR_plate7.txt"),]
Recov.2016<-Recov.2016[!(Recov.2016$Sample.ID=="58" & Recov.2016$File.Name=="Wall_qPCR_plate8.txt"),]
Recov.2016<-Recov.2016[!(Recov.2016$Sample.ID=="66" & Recov.2016$File.Name=="Wall_qPCR_plate9.txt"),]
Recov.2016<-Recov.2016[!(Recov.2016$Sample.ID=="7" & Recov.2016$File.Name=="Wall_qPCR_plate8.txt"),]
Recov.2016<-Recov.2016[!(Recov.2016$Sample.ID=="71" & Recov.2016$File.Name=="Wall_qPCR_plate9.txt"),]
Recov.2016<-Recov.2016[!(Recov.2016$Sample.ID=="78" & Recov.2016$File.Name=="Wall_qPCR_plate9.txt"),]
Recov.2016<-Recov.2016[!(Recov.2016$Sample.ID=="78" & Recov.2016$File.Name=="Wall_qPCR_plate7.txt"),]
Recov.2016<-Recov.2016[!(Recov.2016$Sample.ID=="8" & Recov.2016$File.Name=="Wall_qPCR_plate7.txt"),]
Recov.2016<-Recov.2016[!(Recov.2016$Sample.ID=="8" & Recov.2016$File.Name=="Wall_qPCR_plate8.txt"),]
Recov.2016<-Recov.2016[!(Recov.2016$Sample.ID=="80" & Recov.2016$File.Name=="Wall_qPCR_plate8.txt"),]
Recov.2016<-Recov.2016[!(Recov.2016$Sample.ID=="80" & Recov.2016$File.Name=="Wall_qPCR_plate9.txt"),]

# all data present, QA/QCd and duplicates deleted


######
Bleach.2014$Sample.ID<-as.numeric(Bleach.2014$Sample.ID)
Recov.2015$Sample.ID<-as.numeric(Recov.2015$Sample.ID)
Bleach.2015$Sample.ID<-as.numeric(Bleach.2015$Sample.ID)
Recov.2016$Sample.ID<-as.numeric(Recov.2016$Sample.ID)

# add a site and event level to the lab dataframe
Lab.2014<-cbind(Lab.2014, colsplit(Lab.2014$Sample.ID, pattern= "_", c("Site", "Sample.ID"))) #split sample ID
Lab.2014$Site<-factor(ifelse(Lab.2014$Site == "R14", "Reef 14", "Lilipuna"))
Lab.2014$Event<-"lab2014"
Lab.2014<-Lab.2014[, c(1,2,19,4:18, 20)] # remove old sample ID and sort to match other dataframes

# add a site and event level to the ecological dataframe
Bleach.2014$Site<-factor(ifelse(Bleach.2014$Sample.ID <= 45, "Lilipuna", "Reef 14"))
Bleach.2014$Event<-factor("2014 Bleach")
  Bleach.2015$Site<-factor(ifelse(Bleach.2015$Sample.ID <= 40, "Lilipuna", "Reef 14"))
  Bleach.2015$Event<-factor("2015 Recover")
Recov.2015$Site<-factor(ifelse(Recov.2015$Sample.ID <= 40, "Lilipuna", "Reef 14"))
Recov.2015$Event<-factor("2015 Bleach")
  Recov.2016$Site<-factor(ifelse(Recov.2016$Sample.ID <= 40, "Lilipuna", "Reef 14"))
  Recov.2016$Event<-factor("2016 Recover")
Mydlarz.qPCR.QAQC<-rbind(Lab.2014, Bleach.2014, Recov.2015, Bleach.2015, Recov.2016)

Mydlarz.qPCR.QAQC<-Mydlarz.qPCR.QAQC[, c(1:2, 19, 18, 3:17)]

write.csv(Mydlarz.qPCR.QAQC, "output/Mydlarz.qPCR.QAQC.csv")



###############################
###############################
###############################
## making figures, tables, analyses

###############################
# make a table by dominant symb

df<-Mydlarz.qPCR.QAQC

str(df)
print(levels(df$Event))
df$Event<-factor(df$Event, levels(df$Event)[c(1, 3, 2, 4)]) # reorder levels to match chronology
df$Status<-as.factor(df$Status)
df$Year<-as.factor(df$Year)

symbcomp=table(df$syms, df$Event:df$Site) 
prop.table(symbcomp, margin = 2)

domsymb=table(df$dom, df$Event:df$Site)
prop.table(domsymb, margin = 2)

# to specify order in figure, can use... ...(prop.table(symbcomp[,c(1,3,2,4)], 3,4,1,2,5,6
barplot(prop.table(symbcomp, margin = 2), col = c("slategray4", "slategray2", "dodgerblue", "darkturquoise"), main= "2014 - 2015 qPCR", xlab = "Site and Status", cex.names=0.65, ylab = "Proportion of Symbiont Types")
legend("topright", cex=0.8, bty="n", legend=c("C","CD", "D", "DC"), fill=c("slategray4", "slategray2", "dodgerblue", "darkturquoise"), inset = c(-.25, 0), xpd = NA, x.intersp=0.1, y.intersp=0.7)

dev.copy(pdf, "figures/symb_4 levels.pdf", encod="MacRoman", height=6, width=14)
dev.off()


#####################
## figures for 2014-2015 dominant symbiont composition figure (2 categories)

# to sepcify order in figure, can use... ...(prop.table(symbcomp[,c(1,3,2,4)],
barplot(prop.table(domsymb, margin = 2), col = c("slategray4", "darkturquoise"), main= "2014 - 2015 qPCR", xlab = "Site and Status", cex.names=0.65, ylab = "Proportion of Symbiont Types")
legend("topright", legend=c("C", "D"), bty="n", fill=c("slategray4", "darkturquoise"), inset = c(-.23, 0), xpd = NA, x.intersp=0.1, y.intersp=0.7)

dev.copy(pdf, "figures/symb_2 levels.pdf", encod="MacRoman", height=6, width=14)
dev.off()




##############
##############
data<-read.csv("Mcap qPCR_2.csv",header=TRUE) #import your data file here
str(data)

df<-data[c(0,2:4,14:17)]
str(df)

#Chi Squared test for independence
symbcomp=table(df$sym, df$Status:df$Site)
domsymb=table(df$dom, df$Status:df$Site)

symbcomp
domsymb
chisq.test(symbcomp)
chisq.test(domsymb)

prop.table(symbcomp, margin = 2)
par(mar=c(3, 4, 2, 6))

# to sepcify order in figure, can use... ...(prop.table(symbcomp[,c(1,3,2,4)],
barplot(prop.table(symbcomp, margin = 2), col = c("slategray4", "slategray2", "dodgerblue", "darkturquoise"), main= "2015 - 2016 qPCR", xlab = "Site and Status", cex.names=0.65, ylab = "Proportion of Symbiont Types")
legend("topright", legend=c("C","CD", "D", "DC"), fill=c("slategray4", "slategray2", "dodgerblue", "darkturquoise"), inset = c(-.15, 0), xpd = NA)

##########
prop.table(domsymb, margin = 2)
par(mar=c(3, 4, 2, 6))

barplot(prop.table(domsymb, margin = 2), col = c("slategray4", "darkturquoise"), main= "2015 - 2016 qPCR", xlab = "Site and Status", cex.names=0.65, ylab = "Proportion of Symbiont Types")
legend("topright", legend=c("C", "D"), fill=c("slategray4", "darkturquoise"), inset = c(-.15, 0), xpd = NA)
 
