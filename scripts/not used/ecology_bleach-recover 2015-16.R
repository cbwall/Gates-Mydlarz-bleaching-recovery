## bleaching surveys 2015
#clear work space
rm(list=ls())
ls()

library(nlme)
library(lme4)
library(car)
library("gplots")
library("plotrix")
library("ggplot2")
library("grid")
library("gridExtra")
library("scales")
library('MASS')
library('cowplot')

#----------------------------
#Set Working Directory
main<-setwd(getwd())
setwd("~/Desktop/Research and Teaching/UH MANOA/Research/Mydlarz_Gates project/Bleaching KBay project/R-stats/")

setwd('Data') #folder name where data can be found
data<-read.csv("R-2015-2016 survey.csv") #import your data file here

head(data)
str(data)

##### make a dataframe for data means by transect (= unit of replication)

coral<-aggregate(coral.cover~Transect+Habitat+Site+Status, data=data, sum)
healthy<-aggregate(coral.pigmented~Transect+Habitat+Site+Status, data=data, sum)
bleached<-aggregate(coral.bleached~Transect+Habitat+Site+Status, data=data, sum)
white<-aggregate(coral.white~Transect+Habitat+Site+Status, data=data, sum)
MC<-aggregate(MC~Transect+Habitat+Site+Status, data=data, sum)
MC.nonbl<-aggregate(MC.pigmented~Transect+Habitat+Site+Status, data=data, sum)
MC.bleach<-aggregate(MC.bleached~Transect+Habitat+Site+Status, data=data, sum)
MC.white<-aggregate(MC.white~Transect+Habitat+Site+Status, data=data, sum)

df<-cbind(coral, healthy[c(5,0)], bleached[c(5,0)], white[c(5,0)], MC[c(5,0)], MC.nonbl[c(5,0)], MC.bleach[c(5,0)], MC.white[c(5,0)])
df
# use "df" to test models 

# df.m + dfSE to make figures
coral<-aggregate(coral.cover~Habitat+Site+Status, data=df, mean)
healthy<-aggregate(coral.pigmented~Habitat+Site+Status, data=df, mean)
bleached<-aggregate(coral.bleached~Habitat+Site+Status, data=df, mean)
white<-aggregate(coral.white~Habitat+Site+Status, data=df, mean)
MC<-aggregate(MC~Habitat+Site+Status, data=df, mean)
MC.nonbl<-aggregate(MC.pigmented~Habitat+Site+Status, data=df, mean)
MC.bleach<-aggregate(MC.bleached~Habitat+Site+Status, data=df, mean)
MC.white<-aggregate(MC.white~Habitat+Site+Status, data=df, mean)

df.m<-cbind(coral, healthy[c(4,0)], bleached[c(4,0)], white[c(4,0)], MC[c(4,0)], MC.nonbl[c(4,0)], MC.bleach[c(4,0)], MC.white[c(4,0)])
df.m

coralSD<-aggregate(coral.cover~+Habitat+Site+Status, data=df, sd)
healthySD<-aggregate(coral.pigmented~Habitat+Site+Status, data=df, sd)
bleachedSD<-aggregate(coral.bleached~Habitat+Site+Status, data=df, sd)
whiteSD<-aggregate(coral.white~Habitat+Site+Status, data=df, sd)
MCSD<-aggregate(MC~Habitat+Site+Status, data=df, sd)
MC.nonblSD<-aggregate(MC.pigmented~Habitat+Site+Status, data=df, sd)
MC.bleachSD<-aggregate(MC.bleached~Habitat+Site+Status, data=df, sd)
MC.whiteSD<-aggregate(MC.white~Habitat+Site+Status, data=df, sd)

dfSD<-cbind(coralSD, healthySD[c(4,0)], bleachedSD[c(4,0)], whiteSD[c(4,0)], MCSD[c(4,0)], MC.nonblSD[c(4,0)], MC.bleachSD[c(4,0)], MC.whiteSD[c(4,0)])
dfSD


df.fig<-cbind(df.m, dfSD[c(4:11,0)]); colnames(df.fig) <-c("Habitat", "Site", "Status", "coral.cover", "coral.pigmented", "coral.bleached", "coral.white", "MC", "MC.pigmented", "MC.bleached", "MC.white", "coral.SD", "coral.pigSD", "coral.blSD", "coral.whiteSD", "MCSD", "MC.pigSD", "MC.blSD", "MC.whiteSD"); df.fig


capture.output(df.fig, file="ecology summary.csv")

########################
## color palettes ######
########################

# color-blind palette for "all coral" bleaching
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
               "#D55E00","#CC79A7")

#stacked pallette
stack.palette<- c("#66FFFF", "#3399FF", "#CCFFFF") #bl pig white
MCstack.palette<-c("#FFFF33", "#FFCC66", "#FFFFCC") #bl pig white

# MC palette for "M. capitata" only bleaching
mcPalette<- c("#CCCCCC", "#FFCC00", "#99CCFF")

gray<-c("#FFFFFF", "#333333", "#CCCCCC")

              
# order the factor
df.fig$Habitat <- factor(df.fig$Habitat, c("Flat","Crest","Slope"))

# all coral cover
Fig1 <- ggplot(df.fig, aes(x=Site, y=coral.cover, fill=Habitat)) + 
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  geom_errorbar(aes(ymin=coral.cover-coral.SD, ymax= coral.cover+coral.SD), width=0.1,
  position=position_dodge(.9)) +
  xlab(expression(bold("Reef Site"))) +
  scale_fill_manual(values=cbPalette, breaks=c("Flat","Crest","Slope")) +
  ylab(expression(bold(paste("coral cover")))) +
  scale_y_continuous(limits=c(0,1),oob = rescale_none) +
  facet_grid(.~Status)
Fig1

# normal pigmented coral
Fig2 <- ggplot(df.fig, aes(x=Site, y=coral.pigmented, fill=Habitat)) + 
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  geom_errorbar(aes(ymin=coral.pigmented-coral.pigSD, ymax= coral.pigmented+coral.pigSD), width=0.1, position=position_dodge(.9)) +
  xlab(expression(bold("Reef Site"))) +
  scale_fill_manual(values=cbPalette) +
  ylab(expression(bold(paste("nonbleached corals")))) +
  scale_y_continuous(limits=c(0,1),oob = rescale_none)+
  facet_grid(.~Status)
Fig2

# bleached coral
Fig3 <- ggplot(df.fig, aes(x=Site, y=coral.bleached, fill=Habitat)) + 
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  geom_errorbar(aes(ymin=coral.bleached-coral.blSD, ymax= coral.bleached+coral.blSD), width=0.1, position=position_dodge(.9)) +
  xlab(expression(bold("Reef Site"))) +
  scale_fill_manual(values=cbPalette, breaks=c("Flat","Crest","Slope")) +
  ylab(expression(bold(paste("bleached corals")))) +
  scale_y_continuous(limits=c(0,1),oob = rescale_none)+
  facet_grid(.~Status)
Fig3

# bleached coral
Fig4 <- ggplot(df.fig, aes(x=Site, y=coral.white, fill=Habitat)) + 
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  geom_errorbar(aes(ymin=coral.white-coral.whiteSD, ymax= coral.white+coral.whiteSD), width=0.1, position=position_dodge(.9)) +
  xlab(expression(bold("Reef Site"))) +
  scale_fill_manual(values=cbPalette, breaks=c("Flat","Crest","Slope")) +
  ylab(expression(bold(paste("bleached white corals")))) +
  scale_y_continuous(limits=c(0,1),oob = rescale_none)+
  facet_grid(.~Status)
Fig4


##############
# MC corals
Fig5 <- ggplot(df.fig, aes(x=Site, y=MC, fill=Habitat)) + 
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  geom_errorbar(aes(ymin=MC-MCSD, ymax= MC+MCSD), width=0.1,position=position_dodge(.9)) +
  xlab(expression(bold("Reef Site"))) +
  scale_fill_manual(values=mcPalette, breaks=c("Flat","Crest","Slope")) +
  ylab(expression(bold(paste(~bolditalic("M. capitata")~cover, sep="")))) +
  scale_y_continuous(limits=c(0,1.05),oob = rescale_none)+
  facet_grid(.~Status)
Fig5

# normal pigmented coral
Fig6 <- ggplot(df.fig, aes(x=Site, y=MC.pigmented, fill=Habitat)) + 
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  geom_errorbar(aes(ymin=MC.pigmented-MC.pigSD, ymax= MC.pigmented+MC.pigSD), width=0.1, position=position_dodge(.9)) +
  xlab(expression(bold("Reef Site"))) +
  scale_fill_manual(values=mcPalette, breaks=c("Flat","Crest","Slope")) +
  ylab(expression(bold(paste(~bolditalic("M. capitata")~nonbleached, sep="")))) +
  scale_y_continuous(limits=c(0,1.06),oob = rescale_none)+
  facet_grid(.~Status)
Fig6

# bleached coral
Fig7 <- ggplot(df.fig, aes(x=Site, y=MC.bleached, fill=Habitat)) + 
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  geom_errorbar(aes(ymin=MC.bleached-MC.blSD, ymax= MC.bleached+MC.blSD), width=0.1, position=position_dodge(.9)) +
  xlab(expression(bold("Reef Site"))) +
  scale_fill_manual(values=mcPalette, breaks=c("Flat","Crest","Slope")) +
  ylab(expression(bold(paste(~bolditalic("M. capitata")~bleached, sep="")))) +
  scale_y_continuous(limits=c(0,1),oob = rescale_none)+
  facet_grid(.~Status)
Fig7

# bleached coral
Fig8 <- ggplot(df.fig, aes(x=Site, y=MC.white, fill=Habitat)) + 
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  geom_errorbar(aes(ymin=MC.white-MC.whiteSD, ymax= MC.white+MC.whiteSD), width=0.1, position=position_dodge(.9)) +
  scale_fill_manual(values=mcPalette, breaks=c("Flat","Crest","Slope")) +
  xlab(expression(bold("Reef Site"))) +
  ylab(expression(bold(paste(~bolditalic("M. capitata")~bleached~white, sep="")))) +
  scale_y_continuous(limits=c(0,1),oob = rescale_none) +
  facet_grid(.~Status)
Fig8

######
#grayscale figures
Fig1.BW<-Fig1+scale_fill_manual(values=gray, breaks=c("Flat","Crest","Slope")); Fig1.BW
Fig2.BW<-Fig2+scale_fill_manual(values=gray, breaks=c("Flat","Crest","Slope")); Fig2.BW
Fig3.BW<-Fig3+scale_fill_manual(values=gray, breaks=c("Flat","Crest","Slope")); Fig3.BW
Fig4.BW<-Fig4+scale_fill_manual(values=gray, breaks=c("Flat","Crest","Slope")); Fig4.BW
Fig5.BW<-Fig5+scale_fill_manual(values=gray, breaks=c("Flat","Crest","Slope")); Fig5.BW
Fig6.BW<-Fig6+scale_fill_manual(values=gray, breaks=c("Flat","Crest","Slope")); Fig6.BW
Fig7.BW<-Fig7+scale_fill_manual(values=gray, breaks=c("Flat","Crest","Slope")); Fig7.BW
Fig8.BW<-Fig8+scale_fill_manual(values=gray, breaks=c("Flat","Crest","Slope")); Fig8.BW

#######################
# stacked bar plots####
#######################

### data for stacked figure
stack<-read.csv("/Users/chriswall/Desktop/Research and Teaching/UH MANOA/Research/Mydlarz_Gates project/Bleaching KBay project/R-stats/Data/ecology summary.csv")
head(stack)

#### Fig4b # all coral bleached/recovery by Site:Habitat 2015-2016
Fig4.stack<-ggplot(stack,aes(x = Site:Habitat, y = bl.pig.prop, fill = Bl.pig)) + 
  geom_bar(position = "fill",stat = "identity", colour="black") + 
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values=stack.palette, breaks= c("White", "Bleached", "Pigmented"),  guide = guide_legend(title = "Pigmentation")) +
  scale_x_discrete(limits=c("Lilipuna:Flat","Lilipuna:Crest","Lilipuna:Slope", "Reef14:Flat", "Reef14:Crest", "Reef14:Slope")) +
  ggtitle("2015-2016 bleaching and recovery") +
  xlab(expression(bold("Reef Site and Habitat"))) +
  ylab(expression(bold(paste("% coral")))) +
  facet_grid(.~Status);  Fig4.stack
  
#### Fig4c # MC bleached/recovery by Site:Habitat 2015-2016
Fig4MC.stack<-ggplot(stack,aes(x = Site:Habitat, y = MCbl.pig.prop, fill = Bl.pig)) + 
    geom_bar(position = "fill",stat = "identity", colour="black") + 
    scale_y_continuous(labels = percent_format()) +
    scale_fill_manual(values=MCstack.palette, breaks= c("White", "Bleached", "Pigmented"),  guide = guide_legend(title = "Pigmentation")) +
    scale_x_discrete(limits=c("Lilipuna:Flat","Lilipuna:Crest","Lilipuna:Slope", "Reef14:Flat", "Reef14:Crest", "Reef14:Slope")) +
  xlab(expression(bold("Reef Site and Habitat"))) +
  ggtitle("2015-2016 bleaching and recovery") +
  ylab(expression(bold(paste(~bolditalic("% M. capitata")~nonbleached, sep="")))) +
    facet_grid(.~Status);  Fig4MC.stack

############
############
#export figures
library("cowplot")

#############
# color figures all corals
Fig_2015_2016_surveys<-plot_grid(Fig1, Fig2, Fig3, Fig4,
     labels=c('A', 'B', 'C', 'D'), label_size=15, hjust=-6, vjust= 2, ncol=2, nrow=2);
Fig_2015_2016_surveys

#############
#BW figures all corals
FigBW_2015_2016_surveys<-plot_grid(Fig1.BW, Fig2.BW, Fig3.BW, Fig4.BW,
    labels=c('A', 'B', 'C', 'D'), label_size=15, hjust=-6, vjust= 2, ncol=2, nrow=2);
FigBW_2015_2016_surveys

#############
# color figures M. capitata only
Fig_2015_2016_MC_surveys<-plot_grid(Fig5, Fig6, Fig7, Fig8,
    labels=c('A', 'B', 'C', 'D'), label_size=15, hjust=-6, vjust= 2, ncol=2, nrow=2);
Fig_2015_2016_MC_surveys

#############
# BW figures M. capitata only
FigBW_2015_2016_MC_surveys<-plot_grid(Fig5.BW, Fig6.BW, Fig7.BW, Fig8.BW,
    labels=c('A', 'B', 'C', 'D'), label_size=15, hjust=-6, vjust= 2, ncol=2, nrow=2);
FigBW_2015_2016_MC_surveys

#stacked
Figstack_2015_2016_surveys<-plot_grid(Fig4.stack, Fig4MC.stack,
                                 labels=c('A', 'B'), label_size=15, hjust=-6, vjust= 2, ncol=2, nrow=1);
Figstack_2015_2016_surveys

##################
###############
##########
#####
#
## TOTAL CORAL COMMUNITY BLEACHING ACROSS SITES AND HABITATS

################
# total coral cover
################

Habitat<-df$Habitat
Site<-df$Site

hist(df$coral.cover)

mod<-lm(coral.cover~Site*Habitat, data=df)

R <- resid(mod) #save glm residuals
op<-par(mfrow = c(2,2), mar=c(5,4,1,2))
plot(mod, add.smooth = FALSE, which=1)
QQ <- qqnorm(R, main = "") 
QQline <- qqline(R)
hist(R, xlab="Residuals", main ="")
plot(Site, R, xlab="Site", ylab="Residuals")
plot(Habitat, R, xlab="Habitat", ylab="Residuals")

shapiro <- shapiro.test(R) #runs a normality test on residuals
shapiro # null = normally distrubuted (P<0.05 = non-normal)

# Model
mod<-lm(coral.cover~Site*Habitat, data=df); summary(mod)
Anova(mod, type=3)


################
# pigmented coral
################
hist(df$coral.pigmented)

mod<-lm(coral.pigmented~Site*Habitat, data=df)

R <- resid(mod) #save glm residuals
op<-par(mfrow = c(2,2), mar=c(5,4,1,2))
plot(mod, add.smooth = FALSE, which=1)
QQ <- qqnorm(R, main = "") 
QQline <- qqline(R)
hist(R, xlab="Residuals", main ="")
plot(Site, R, xlab="Site", ylab="Residuals")
plot(Habitat, R, xlab="Habitat", ylab="Residuals")

shapiro <- shapiro.test(R) #runs a normality test on residuals
shapiro # null = normally distrubuted (P<0.05 = non-normal)

# Model
mod<-lm(coral.pigmented~Site*Habitat, data=df); summary(mod)
Anova(mod, type=3)

################
# bleached coral
################
hist(df$coral.bleached)

mod<-lm(coral.bleached~Site*Habitat, data=df)

R <- resid(mod) #save glm residuals
op<-par(mfrow = c(2,2), mar=c(5,4,1,2))
plot(mod, add.smooth = FALSE, which=1)
QQ <- qqnorm(R, main = "") 
QQline <- qqline(R)
hist(R, xlab="Residuals", main ="")
plot(Site, R, xlab="Site", ylab="Residuals")
plot(Habitat, R, xlab="Habitat", ylab="Residuals")

shapiro <- shapiro.test(R) #runs a normality test on residuals
shapiro # null = normally distrubuted (P<0.05 = non-normal)

# Models
mod<-lm(coral.bleached~Site*Habitat, data=df); summary(mod)
Anova(mod, type=3)

#######################
# completely white coral
#######################
hist(df$coral.white)

mod<-lm(coral.white~Site*Habitat, data=df)

R <- resid(mod) #save glm residuals
op<-par(mfrow = c(2,2), mar=c(5,4,1,2))
plot(mod, add.smooth = FALSE, which=1)
QQ <- qqnorm(R, main = "") 
QQline <- qqline(R)
hist(R, xlab="Residuals", main ="")
plot(Site, R, xlab="Site", ylab="Residuals")
plot(Habitat, R, xlab="Habitat", ylab="Residuals")

shapiro <- shapiro.test(R) #runs a normality test on residuals
shapiro # null = normally distrubuted (P<0.05 = non-normal)

# Models
mod<-lm(coral.white~Site*Habitat, data=df); summary(mod)
Anova(mod, type=3)

#
#####
##########
##############
####################
# MONTIPORA CAPITATA BLEACHING
####################
###############
##########
#####
#

################
# pigmented MC
################
hist(df$MC.pigmented)

mod<-lm(MC.pigmented~Site*Habitat, data=df)

R <- resid(mod) #save glm residuals
op<-par(mfrow = c(2,2), mar=c(5,4,1,2))
plot(mod, add.smooth = FALSE, which=1)
QQ <- qqnorm(R, main = "") 
QQline <- qqline(R)
hist(R, xlab="Residuals", main ="")
plot(Site, R, xlab="Site", ylab="Residuals")
plot(Habitat, R, xlab="Habitat", ylab="Residuals")


shapiro <- shapiro.test(R) #runs a normality test on residuals
shapiro # null = normally distrubuted (P<0.05 = non-normal)

# Model
mod<-lm(MC.pigmented~Site*Habitat, data=df); summary(mod)
Anova(mod, type=3)

################
# bleached MC
################
hist(df$MC.bleached)

mod<-lm(MC.bleached~Site*Habitat, data=df)

R <- resid(mod) #save glm residuals
op<-par(mfrow = c(2,2), mar=c(5,4,1,2))
plot(mod, add.smooth = FALSE, which=1)
QQ <- qqnorm(R, main = "") 
QQline <- qqline(R)
hist(R, xlab="Residuals", main ="")

shapiro <- shapiro.test(R) #runs a normality test on residuals
shapiro # null = normally distrubuted (P<0.05 = non-normal)

# Models
mod<-lm(MC.bleached~Site*Habitat, data=df); summary(mod)
Anova(mod, type=3)

#######################
# completely white MC
#######################
hist(df$MC.white)

mod<-lm(MC.white~Site*Habitat, data=df)

R <- resid(mod) #save glm residuals
op<-par(mfrow = c(2,2), mar=c(5,4,1,2))
plot(mod, add.smooth = FALSE, which=1)
QQ <- qqnorm(R, main = "") 
QQline <- qqline(R)
hist(R, xlab="Residuals", main ="")

shapiro <- shapiro.test(R) #runs a normality test on residuals
shapiro # null = normally distrubuted (P<0.05 = non-normal)

# Models
mod<-lm(MC.white~Site*Habitat, data=df); summary(mod)
Anova(mod, type=3)

