#make Figure 1: extreme EU parties dotplot
#use eu_bam_extreme_partiesV12.dta
#R version 3.6.1

library(foreign)
library(lattice)
dat<-read.dta("eu_bam_extreme_partiesV12.dta")


dat$id2<-reorder(dat$party2, dat[,3])
pdf("extreme EU parties.pdf")
dotplot(dat$id2~dat[,3],cex=0.0, xlim=c(min(dat[,4])-1, max(dat[,5])+1),
        xlab=list(label = "Pro and Anti EU Parties", cex=1.1),
        main="Party Placements with 95% Credible Intervals",
        scales=list(cex=0.9, font=2),
        points = list(pch = c(16, 4), col = "black", cex = 1),
        panel = function(x,y,subscripts,lower, upper){
          panel.dotplot(x, y,col = "white", lty = 1)
          panel.segments(dat[,4], y, dat[,5], y, col="gray", lwd=2)
          panel.points(x,y, pch=16, col="black", cex=.7)
                  })

dev.off()
