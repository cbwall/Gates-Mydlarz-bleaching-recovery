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
Data<-Data %>% group_by(Period) %>% dplyr::mutate(Arb.ID = row_number())
Data<- as.data.frame(Data)

# matrix of responses
mat <- Data[,c(4:13)]

# groups combining Site and Symbiont #### this is for legend and shows order
grp <- paste0(Data$Site,Data$dom)
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
pdf("output/trajectory_plotCW.pdf",width=6, height=6)
trajectoryPCoA(dist, sampu, per, traj.colors= c("black","gray", "red", "pink"), lwd = 2)
      legend("topleft", col=c("black","gray", "red", "pink"), 
             legend=c("LilipunaD", "LilipunaC", "Reef 14D",  "Reef 14C"), bty="n", lty=1, lwd = 2)
dev.off()

# Lengths of trajectories
trajectoryLengths(dist, per, sampu)

