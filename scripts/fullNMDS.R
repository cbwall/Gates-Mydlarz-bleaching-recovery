# cleaned data for full dataframe NMDS (Site, Period, dom)

fullNMDS<-df.MV.noNA
fullNMDS$Period.Site.dom<-interaction(fullNMDS$Period, fullNMDS$Site, fullNMDS$dom)

fac.fullNMDS<-fullNMDS %>% 
  select(c(Period, Status, Site, dom, Period.Site.dom)) #  sans factors

var.fullNMDS<-fullNMDS[, 4:13] # just responses

##  Run MANOVA
all.Manova<-adonis2(var.fullNMDS~Site*Period*dom, data=fullNMDS,permutations=1000, 
                          method="bray", sqrt.dist = TRUE)

allNMDS<-metaMDS(var.fullNMDS, distane='bray', k=2, trymax=100) 
stressplot(allNMDS)

allNMDS.df<-data.frame(x=allNMDS$point[,1], y=allNMDS$point[,2], 
                            Period=as.factor(fac.fullNMDS[,1]),
                            Site=as.factor(fac.fullNMDS[,2]),
                            Status=as.factor(fac.fullNMDS[,3]),
                            dom=as.factor(fac.fullNMDS[,4]),
                            Period.Site.dom=as.factor(fac.fullNMDS[,5]))

colnames(allNMDS.df)[1:2]<-c("MDS1", "MDS2")

#centroid means
NMDS.mean=aggregate(allNMDS.df[,1:2],list(group=allNMDS.df$Period.Site.dom), mean)

############### All groups NMDS
fullNMDS$Period.Site.dom<-droplevels(fullNMDS$Period.Site.dom)
groups<-fullNMDS$Period.Site.dom
groups2<-c("C-Lilipuna", "C-Reef 14", "D-Lilipuna", "D-Reef 14")
colors=c(
  "firebrick1", #C Lilipuna
  "firebrick3", #C Reef 14
  "steelblue1", # D Lilipuna
  "steelblue3") # D Reef 14

### 

plot<-ordiplot(allNMDS, type="n", cex.main=1, display="sites", cex.lab=0.8, cex.axis=0.8, ylim=c(-0.3, 0.3))
axis(side = 1, labels = FALSE, tck = -0.05)
abline(h = 0, lty = "dotted")
abline(v = 0, lty = "dotted")
ordihull(plot, groups=fullNMDS$Period, draw="polygon", alpha=30, col="gray50", border=FALSE)

# points for start and end
with(plot, points(NMDS.mean[1,2], NMDS.mean[1,3], col=colors[1], pch=16, cex=0.7, lwd=1)) # Lil C
with(plot, points(NMDS.mean[4,2], NMDS.mean[4,3], col=colors[1], pch=17, cex=1, lwd=1)) # Lil C
with(plot, points(NMDS.mean[5,2], NMDS.mean[5,3], col=colors[2], pch=16, cex=0.7, lwd=1)) # R14 C
with(plot, points(NMDS.mean[8,2], NMDS.mean[8,3], col=colors[2], pch=17, cex=1, lwd=1)) # R14 C
with(plot, points(NMDS.mean[9,2], NMDS.mean[9,3], col=colors[3], pch=16, cex=0.7, lwd=1)) # Lil D
with(plot, points(NMDS.mean[12,2], NMDS.mean[12,3], col=colors[3], pch=17, cex=1, lwd=1)) # Lil D
with(plot, points(NMDS.mean[13,2], NMDS.mean[13,3], col=colors[4], pch=16, cex=0.7, lwd=1)) # R14 D
with(plot, points(NMDS.mean[16,2], NMDS.mean[16,3], col=colors[4], pch=17, cex=1, lwd=1)) # R14 D

# points for others (if want)
with(plot, points(NMDS.mean[2,2], NMDS.mean[2,3], col=colors[1], pch=16, cex=0.7, lwd=1)) # Lil C
with(plot, points(NMDS.mean[3,2], NMDS.mean[3,3], col=colors[1], pch=16, cex=0.7, lwd=1)) # Lil C
with(plot, points(NMDS.mean[6,2], NMDS.mean[6,3], col=colors[2], pch=16, cex=0.7, lwd=1)) # R14 C
with(plot, points(NMDS.mean[7,2], NMDS.mean[7,3], col=colors[2], pch=16, cex=0.7, lwd=1)) # R14 C
with(plot, points(NMDS.mean[10,2], NMDS.mean[10,3], col=colors[3], pch=16, cex=0.7, lwd=1)) # Lil D
with(plot, points(NMDS.mean[11,2], NMDS.mean[11,3], col=colors[3], pch=16, cex=0.7, lwd=1)) # Lil D
with(plot, points(NMDS.mean[14,2], NMDS.mean[14,3], col=colors[4], pch=16, cex=0.7, lwd=1)) # R14 D
with(plot, points(NMDS.mean[15,2], NMDS.mean[15,3], col=colors[4], pch=16, cex=0.7, lwd=1)) # R14 D

# lines for start and end
with(plot, lines(NMDS.mean[c(1:4),2], NMDS.mean[c(1:4),3], lwd=1.5, col =colors[1], lty=2))# Lil C
with(plot, lines(NMDS.mean[c(5:8),2], NMDS.mean[c(5:8),3], lwd=1.5, col =colors[2], lty=1))# R14 C
with(plot, lines(NMDS.mean[c(9:12),2], NMDS.mean[c(9:12),3], lwd=1.5, col =colors[3], lty=2))# Lil D
with(plot, lines(NMDS.mean[c(13:16),2], NMDS.mean[c(13:16),3], lwd=1.5, col =colors[4], lty=1))# R14 D

# add labels
text(-0.38, 0.28, "Bleaching \n 2015", cex=0.7, col="gray50")
text(-0.28, -0.2, "Bleaching \n 2014", cex=0.7, col="gray50")
text(0.27, -0.1, "Recovery \n 2014", cex=0.7, col="gray50")
text(0.07, 0.21, "Recovery \n 2015", cex=0.7, col="gray50")
legend("topright", legend=groups2, inset=c(0.045, -0.02), x.intersp=0.3, y.intersp=0.5, cex=0.7, pch=16, col=colors, lty=c(1,2,1,2), seg.len=1, pt.cex=1, bty="n")

dev.copy(pdf, "figures/1panel_allNMDS.pdf", height=5, width=5, useDingbats=FALSE)
dev.off() 


