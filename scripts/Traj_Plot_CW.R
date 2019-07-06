install.packages("vegclust")
install.packages("RColorBrewer")
install.packages("smacof")
install.packages("MASS")
install.packages("vegan")
install.packages("ggplot2")

library(vegclust)
library(RColorBrewer)
library(smacof)
library(MASS)
library(vegan)
library(ggplot2)
library(dplyr)

#rm(list=ls())

#import normalized data
Data <- read.csv("output/df.normalized.csv", header=TRUE, sep=",")

# remove columns
Data <- Data[ , -which(names(Data) %in% c("X", "Species", "Status","coral.red.adj", "ID",
                                          "coral.green.adj", "coral.blue.adj", "Site_Status",
                                          "PC1", "propC", "propD", "syms"))]

# remove unwanted levels
Data<- Data[c(!Data$Period=="2014 Lab" & !Data$Period=="2014 Field.Feb"),]
Data$Period<-factor(Data$Period, levels= c("2014 Oct", "2015 Feb", "2015 Oct", "2016 Feb"))
Data$Site<-factor(Data$Site, levels= c("Lilipuna", "Reef 14"))

Data <-na.omit(Data)


# make an arbitrary number column that runs sequential by Status_Site
Data<-Data %>% 
  group_by(Period) %>% 
  dplyr::mutate(Arb.ID = row_number())

# make new factor of B (Bleaching) and R (Recovery) for each period 1 ('14-15) and 2 ('15-16)
Data<-Data %>%
  mutate(Period.short = if_else(Period=="2014 Oct", "B1", 
                                if_else(Period=="2015 Feb", "R1",
                                        if_else(Period=="2015 Oct", "B2", "R2"))))

# return to dataframe
Data<- as.data.frame(Data)

# matrix of responses to use in ordination
mat <- Data[,c(4:13)]

######### Should group (grp) be used as "site" and survey (per)? >> works with all test scripts

# groups combining Site and Symbiont #### this is for legend and shows order
grp <- paste0(Data$Site, Data$dom)
unique(grp)

# sampling units
sampu <- Data$Arb.ID

# groups as Period only
per <- as.numeric(Data$Period)
unique(per)

# scale and center data, and apply Euclidian distance matrix
mat <- scale(mat, center = TRUE, scale = TRUE)
dist <- dist(mat, method = "euclidean")

# figure of trajectories
######### Something wrong here--if you plot without colors you see MANY more lines. WHY??? #####
#pdf("output/trajectory_plotCW.pdf",width=6, height=6)
trajectoryPCoA(dist, sampu, per, traj.colors= c("black","gray", "red", "pink"), lwd = 1, survey.labels=TRUE)
      legend("topleft", col=c("black","gray", "red", "pink"), 
             legend=c("LilipunaD", "LilipunaC", "Reef 14D",  "Reef 14C"), bty="n", lty=1, lwd = 2)
#dev.off()

### trajectory lengths, angles, and directionality
# Lengths of trajectories
trajectoryLengths(dist, grp, per) 
# Total trajectory length runs from 213-250, Lilipuna C < D, Reef 14 C > D

# Angles and lengths accounted for in trajectories
trajectoryDirectionality(dist, grp, per) # all about equally straight

# trajectory Convergence
# trend analysis of sequences of distance between points
# Mann-Whitney trend test, tau statistic >0 diverging, <0 converging. 
# Assymmetric with "FALSE", sequence of distnaces between every point trajectory compared to others
trajectoryConvergence(dist, grp, per, symmetric = FALSE)

Ds<-segmentDistances(dist, grp, per)$Dseg; Ds
mMDS<-mds(Ds)

xret = mMDS$conf
par(mar=c(4,4,1,1))
plot(xret, xlab="axis 1", ylab = "axis 2", asp=1, pch=16,
     col=c(rep("black",3), rep("gray",3), rep("red",3), rep("pink")), 
     xlim=c(-1.5,1), ylim=c(-1,1.5))
#text(xret, labels=rep(paste0("s",1:3),3), pos=1)
legend("topleft", col=c("black","gray","red", "pink"), pch=16, bty="n", legend=c("LilipunaD", "LilipunaC", "Reef 14D",  "Reef 14C"))

trajectoryDistances(dist, grp, per, distance.type = "Hausdorff")
