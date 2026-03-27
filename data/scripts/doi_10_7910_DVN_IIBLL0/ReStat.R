##3 dimensional graph for 3rd generation men and women's earnings
#EARNINGS SONS
data <- read.csv("3gearn_1.csv")
datah<-subset(data, data$oil_f==2)
datah$rank_smoothed <- loess(rank20 ~ frank20 * gfrank20,
                             data=datah,
                             span=0.15)$fitted
library(lattice)
png("3dearnHigh.png")
wireframe(rank_smoothed ~ frank20 * gfrank20,
          data=datah,
          drape=TRUE,
          col.regions = colorRampPalette(c("blue","green", "yellow", "red"))(100),
          at=seq(from=7, to= 15, by=0.1),
          scales=list(arrows=FALSE),
          xlab=list("2nd generation rank",rot=30),
          ylab=list("1st generation rank",rot=-40),
          zlab=list("3rd generation rank",rot=90),
          zlim=c(7,15))
dev.off()

datal<-subset(data, data$oil_f==0)
datal$rank_smoothed <- loess(rank20 ~ frank20 * gfrank20,
                             data=datal,
                             span=0.15)$fitted
library(lattice)
png("3dearnLow.png")
wireframe(rank_smoothed ~ frank20 * gfrank20,
          data=datal,
          drape=TRUE,
          col.regions = colorRampPalette(c("blue","green", "yellow", "red"))(100),
          at=seq(from=7, to= 15, by=0.1),
          scales=list(arrows=FALSE),
          xlab=list("2nd generation rank",rot=30),
          ylab=list("1st generation rank",rot=-40),
          zlab=list("3rd generation rank",rot=90),
          zlim=c(7,15))
dev.off()

#EARNINGS DAUGHTERS
data <- read.csv("3gearn_2.csv")
datah<-subset(data, data$oil_f==2)
datah$rank_smoothed <- loess(rank20 ~ frank20 * gfrank20,
                             data=datah,
                             span=0.15)$fitted
library(lattice)
png("3dearnw_High.png")
wireframe(rank_smoothed ~ frank20 * gfrank20,
          data=datah,
          drape=TRUE,
          col.regions = colorRampPalette(c("blue","green", "yellow", "red"))(100),
          at=seq(from=7, to= 15, by=0.1),
          scales=list(arrows=FALSE),
          xlab=list("2nd generation rank",rot=30),
          ylab=list("1st generation rank",rot=-40),
          zlab=list("3rd generation rank",rot=90),
          zlim=c(7,15))
dev.off()

datal<-subset(data, data$oil_f==0)
datal$rank_smoothed <- loess(rank20 ~ frank20 * gfrank20,
                             data=datal,
                             span=0.15)$fitted
library(lattice)
png("3dearnw_Low.png")
wireframe(rank_smoothed ~ frank20 * gfrank20,
          data=datal,
          drape=TRUE,
          col.regions = colorRampPalette(c("blue","green", "yellow", "red"))(100),
          at=seq(from=7, to= 15, by=0.1),
          scales=list(arrows=FALSE),
          xlab=list("2nd generation rank",rot=30),
          ylab=list("1st generation rank",rot=-40),
          zlab=list("3rd generation rank",rot=90),
          zlim=c(7,15))
dev.off()
