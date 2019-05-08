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

setwd("MyProjects/Gates-Mydlarz-bleaching-recovery/")

Data <- read.csv("../data/Test_HP.csv", header=TRUE, sep=",")
Data$Prot <- (Data$mg.prot..ml*Data$total.blastate.ml)/Data$surface.area.cm2
Data$Cells <- (Data$cells..ml*Data$total.blastate.ml)/Data$surface.area.cm2
Data$Chla <- (Data$ug.chla..ml*Data$total.blastate.ml)/Data$surface.area.cm2
Data$Chla.cell <- (Data$ug.chla..ml*Data$total.blastate.ml)/(Data$cells..ml*Data$total.blastate.ml)
Data$AFDW <- (Data$g.AFDW..ml*Data$total.blastate.ml)/Data$surface.area.cm2
Data <-na.omit(Data)
mat <- Data[,c(13:17,21,24:28)]

grp <- paste0(Data$Site,Data$dom)
unique(grp)
sampu <- Data$ID
per <- as.numeric(Data$Period)
unique(per)

mat <- scale(mat, center = TRUE, scale = TRUE)
dist <- dist(mat, method = "euclidean")

#pdf("../output/trajectory_plot.pdf",width=6, height=6)
trajectoryPCoA(dist, sampu, per, traj.colors = c("black","gray", "red", "pink"), lwd = 2)
legend("topleft", col=c("black","gray", "red", "pink"), 
       legend=c("LilipunaD", "LilipunaC", "Reef 14D",  "Reef 14C" ), bty="n", lty=1, lwd = 2)
#dev.off
