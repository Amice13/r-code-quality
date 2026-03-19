require("RColorBrewer")
typenames = c("Macroeconomics", "Civil Rights", "Health", 
              "Agriculture", "Labor and Employment", "Education", "Environment", 
              "Energy", "Transportation", "Law and Crime", "Housing", 
              "Banking and Finance", "Defense", "Space, Science and Communications", 
              "International Affairs", "Government Operations", "Public Lands")

###################################################################################
## Graph for full ABP (Figure 1)
ABPfb = read.table("../data/fullbridgeIBP.csv",sep="\t",header=T)

colorscale = c("blue","red")
pchscale   = c(16, 17)
pdf(file="../results/Figure1.pdf", width=7, height=4)
par(mar=c(4,4,1,1)+0.1)
plot(ABPfb$Congress, ABPfb$ABP, type="n",cex=2,ylim=c(0,1), ylab="ABP", xlab="Congress", axes=F)
axis(1, at=ABPfb$Congress, las=2)
axis(2, las=2)
box()
breakpoints = (ABPfb$Congress[1] - 0.5) + cumsum(c(0,4,2,4,4,4))
greycolors = c(grey(0.80),grey(0.92),grey(0.80),grey(0.92),grey(0.80))
KKr = length(breakpoints)
for(kk in 1:KKr){
  xx = c(breakpoints[kk], breakpoints[kk+1], breakpoints[kk+1], breakpoints[kk])
  yy = c(0, 0, 1, 1)
  polygon(xx, yy, col=greycolors[kk], border=NA)
}
points(ABPfb$Congress, ABPfb$ABP, pch=pchscale[ABPfb$Control], col=colorscale[ABPfb$Control])
textpos = (ABPfb$Congress[1] - 0.5) + cumsum(c(2,3,3,4,4))
text(textpos, rep(0.9,length(textpos)), labels=c("Reagan","GWH Bush","Clinton","GW Bush","Obama"), font=2, cex=0.65)
legend(107,0.4,c("Democrat control","Republican control"),pch=pchscale,bty="n",cex=0.8)
dev.off()
###################################################################################


###################################################################################
## Graph for partial ABP (Figure 2)
ABPpartial = read.table("../data/partialbridgeABP.csv", header=T, sep=",")
colorscale = brewer.pal(5, "Set1")

## First 5
pdf(file="../results/Figure2a.pdf", width=7, height=4)
par(mar=c(4,4,1,1)+0.1)
plot(ABPpartial$Congress, ABPpartial$X1, ylim=c(0.55,0.95), type="n", xlab="Congress", ylab="Issue-Specific ABF", axes=F, las=1)
axis(1, at=ABPpartial$Congress, las=2)
axis(2, las=2)
box()
for(kk in 1:5){
  lines(ABPpartial$Congress, ABPpartial[,kk+1], lty=kk, col=colorscale[kk], lwd=2)
}
legend(107.5, 0.73, typenames[1:5], lty=1:kk, col=colorscale[1:kk], cex=0.8, bty="n", lwd=2)
dev.off()
## Next 4
pdf(file="../results/Figure2b.pdf", width=7, height=4)
par(mar=c(4,4,1,1)+0.1)
plot(ABPpartial$Congress, ABPpartial$X1, ylim=c(0.55,0.95), type="n", xlab="Congress", ylab="Issue-Specific ABF", axes=F, las=1)
axis(1, at=ABPpartial$Congress, las=2)
axis(2, las=2)
box()
for(kk in 1:4){
  lines(ABPpartial$Congress, ABPpartial[,kk+5], lty=kk, col=colorscale[kk], lwd=2)
}
legend(107.5, 0.73, typenames[5+1:4], lty=1:kk, col=colorscale[1:kk], cex=0.8, bty="n", lwd=2)
dev.off()
## Next 4
pdf(file="../results/Figure2c.pdf", width=7, height=4)
par(mar=c(4,4,1,1)+0.1)
plot(ABPpartial$Congress, ABPpartial$X1, ylim=c(0.55,0.95), type="n", xlab="Congress", ylab="Issue-Specific ABF", axes=F, las=1)
axis(1, at=ABPpartial$Congress, las=2)
axis(2, las=2)
box()
for(kk in 1:4){
  lines(ABPpartial$Congress, ABPpartial[,kk+9], lty=kk, col=colorscale[kk], lwd=2)
}
legend(107.5, 0.73, typenames[9+1:4], lty=1:kk, col=colorscale[1:kk], cex=0.8, bty="n", lwd=2)
dev.off()
## Next 4
pdf(file="../results/Figure2d.pdf", width=7, height=4)
par(mar=c(4,4,1,1)+0.1)
plot(ABPpartial$Congress, ABPpartial$X1, ylim=c(0.55,0.95), type="n", xlab="Congress", ylab="Issue-Specific ABF", axes=F, las=1)
axis(1, at=ABPpartial$Congress, las=2)
axis(2, las=2)
box()
for(kk in 1:4){
  lines(ABPpartial$Congress, ABPpartial[,kk+13], lty=kk, col=colorscale[kk], lwd=2)
}
legend(107.5, 0.73, typenames[13+1:4], lty=1:kk, col=colorscale[1:kk], cex=0.8, bty="n", lwd=2)
dev.off()
###################################################################################


###################################################################################
# Graph for full ABP vs percentage of variability explained by the first component of WNOMINATE (Figure 3)
ABPfb = read.table("../data/fullbridgeIBP.csv",sep="\t",header=T)
pchscale   <- c(19, 17)
pdf(file="../results/Figure3.pdf")
par(mar=c(4,4,1,1)+0.1)
plot(ABPfb$ABP,ABPfb$wnominate1eigenper, xlim=c(0.15,0.8), ylim=c(0.65,1), xlab="ABF", ylab="% variability in WNominates first eigenvalue", las=1, pch=pchscale[ABPfb$Control], cex=1.2, cex.axis=1.2, cex.lab=1.2)
text(ABPfb$ABP,ABPfb$wnominate1eigenper,ABPfb$Congress, pos=c(rep(4,13),3,1,2,1,3),cex=1.2)

breakpoints <- c(97,100,102,106,110,114)
colorscale <- brewer.pal(5, "Set1")

for(i in 1:5){
  xx = spline(ABPfb$Congress, ABPfb$ABP, n=500, method="fmm",
              xmin=breakpoints[i], xmax=breakpoints[i+1])
  yy = spline(ABPfb$Congress, ABPfb$wnominate1eigenper, n=500, method="fmm",
              xmin=breakpoints[i], xmax=breakpoints[i+1])
  lines(xx$y, yy$y, lwd=2, col=colorscale[i], lty=i)  #
}
text(0.23,0.73,"Reagan", col=colorscale[1], font=2, cex=1.2)
text(0.35,0.79,"GHW Bush", col=colorscale[2], font=2, cex=1.2)
text(0.3,0.90,"Clinton", col=colorscale[3], font=2, cex=1.2)
text(0.53,0.87,"GW Bush", col=colorscale[4], font=2, cex=1.2)
text(0.52,0.97,"Obama", col=colorscale[5], font=2, cex=1.2)

legend(0.55,0.8,c("Democrat control","Republican control"),pch=pchscale,bty="n",cex=1.5)
dev.off()
###################################################################################
