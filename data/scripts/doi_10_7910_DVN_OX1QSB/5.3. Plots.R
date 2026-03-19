################################################################

# Plots CSPE differences for combination forecasts -----

pdf(file="quarter1947.combinations.pdf")
par(mfrow=c(5,2),
    oma = c(1,2,1,1) + 0.1,
    mar = c(2,2,2,1) + 0.1)

plot(quarter1947combination$mc$cspe.diff,ylim=c(-0.07,0.12),main=expression(italic("Panel A: Mean")),xlab="",ylab="",cex.main=1)
nberShade(col="gray65")
abline(h=0,col="black")
lines(quarter1947combination$mc$cspe.diff,col="black",lwd=1)
lines(quarter1947combination$mc$cspe.diff.wls,col="gray50",lwd=1)
box("plot",col="black",lwd=2)

plot(quarter1947combination$md$cspe.diff,ylim=c(-0.07,0.12),main=expression(italic("Panel B: Median")),xlab="",ylab="",cex.main=1)
nberShade(col="gray65")
abline(h=0,col="black")
lines(quarter1947combination$md$cspe.diff,col="black",lwd=1)
lines(quarter1947combination$md$cspe.diff.wls,col="gray50",lwd=1)
box("plot",col="black",lwd=2)

plot(quarter1947combination$tc$cspe.diff,ylim=c(-0.07,0.12),main=expression(italic("Panel C: Trimmed mean")),xlab="",ylab="",cex.main=1)
nberShade(col="gray65")
abline(h=0,col="black")
lines(quarter1947combination$tc$cspe.diff,col="black",lwd=1)
lines(quarter1947combination$tc$cspe.diff.wls,col="gray50",lwd=1)
box("plot",col="black",lwd=2)

plot(quarter1947combination$imc$cspe.diff,ylim=c(-0.07,0.12),main=expression(italic("Panel D: IterMean")),xlab="",ylab="",cex.main=1)
nberShade(col="gray65")
abline(h=0,col="black")
lines(quarter1947combination$imc$cspe.diff,col="black",lwd=1)
lines(quarter1947combination$imc$cspe.diff.wls,col="gray50",lwd=1)
box("plot",col="black",lwd=2)

plot(quarter1947combination$imd$cspe.diff,ylim=c(-0.07,0.12),main=expression(italic("Panel E: IterMedian")),xlab="",ylab="",cex.main=1)
nberShade(col="gray65")
abline(h=0,col="black")
lines(quarter1947combination$imd$cspe.diff,col="black",lwd=1)
lines(quarter1947combination$imd$cspe.diff.wls,col="gray50",lwd=1)
box("plot",col="black",lwd=2)

plot(quarter1947combination$itc$cspe.diff,ylim=c(-0.07,0.12),main=expression(italic("Panel F: IterTrimmed")),xlab="",ylab="",cex.main=1)
nberShade(col="gray65")
abline(h=0,col="black")
lines(quarter1947combination$itc$cspe.diff,col="black",lwd=1)
lines(quarter1947combination$itc$cspe.diff.wls,col="gray50",lwd=1)
box("plot",col="black",lwd=2)

plot(quarter1947dmspe$theta1$cspe.diff,ylim=c(-0.07,0.12),main=expression(italic(paste("Panel G: DMSPE (", theta, " =1)"))),xlab="",ylab="",cex.main=1)
nberShade(col="gray65")
abline(h=0,col="black")
lines(quarter1947dmspe$theta1$cspe.diff,col="black",lwd=1)
lines(quarter1947dmspe$theta1$cspe.diff.wls,col="gray50",lwd=1)
box("plot",col="black",lwd=2)

plot(
  quarter1947dmspe$theta0.9$cspe.diff,
  ylim = c(-0.07, 0.12),
  main = expression(italic(paste("Panel H: DMSPE (", theta, " = 0.9)"))),
  xlab = "",
  ylab = "",
  cex.main = 1
)
nberShade(col = "gray65")
abline(h = 0, col = "black")
lines(quarter1947dmspe$theta0.9$cspe.diff,
      col = "black",
      lwd = 1)
lines(quarter1947dmspe$theta0.9$cspe.diff.wls,
      col = "gray50",
      lwd = 1)
box("plot", col = "black", lwd = 2)

plot(
  quarter1947dmspe$theta0.5$cspe.diff,
  ylim = c(-0.07, 0.12),
  main = expression(italic(paste("Panel H: DMSPE (", theta, " = 0.5)"))),
  xlab = "",
  ylab = "",
  cex.main = 1
)
nberShade(col = "gray65")
abline(h = 0, col = "black")
lines(quarter1947dmspe$theta0.5$cspe.diff,
      col = "black",
      lwd = 1)
lines(quarter1947dmspe$theta0.5$cspe.diff.wls,
      col = "gray50",
      lwd = 1)
box("plot", col = "black", lwd = 2)

plot(quarter1947cenet$cspe.diff,ylim=c(-0.07,0.12),main=expression(italic("Panel J: Comb-ENet")),xlab="",ylab="",cex.main=1)
nberShade(col="gray65")
abline(h=0,col="black")
lines(quarter1947cenet$cspe.diff,col="black",lwd=1)
lines(quarter1947cenet$cspe.diff.wls,col="gray50",lwd=1)
box("plot",col="black",lwd=2)

while (!is.null(dev.list()))  dev.off()

# Plots for other forecasts ---------------

pdf(file="quarter.1947.other.pdf")
par(mfrow=c(4,2),
    oma = c(1,2,1,1) + 0.1,
    mar = c(2,2,2,1) + 0.1)

plot(quarter1947kitchensink$cspe.diff,ylim=c(-0.4,0.12),main=expression(italic("Panel A: Kitchen sink",theta)),xlab="",ylab="",cex.main=1)
nberShade(col="gray65")
abline(h=0,col="black")
lines(quarter1947kitchensink$cspe.diff,col="black",lwd=1)
lines(quarter1947kitchensink$cspe.diff.wls,col="gray50",lwd=1)
box("plot",col="black",lwd=2)

plot(quarter1947Elasticnet$cspe.diff,ylim=c(-0.4,0.12),main=expression(italic("Panel B: ENet")),xlab="",ylab="",cex.main=1)
nberShade(col="gray65")
abline(h=0,col="black")
lines(quarter1947Elasticnet$cspe.diff,col="black",lwd=1)
lines(quarter1947Elasticnet$cspe.diff.wls,col="gray50",lwd=1)
box("plot",col="black",lwd=2)

plot(quarter1947LASSO$cspe.diff,ylim=c(-0.4,0.12),main=expression(italic("Panel C: LASSO")),xlab="",ylab="",cex.main=1)
nberShade(col="gray65")
abline(h=0,col="black")
lines(quarter1947LASSO$cspe.diff,col="black",lwd=1)
lines(quarter1947LASSO$cspe.diff.wls,col="gray50",lwd=1)
box("plot",col="black",lwd=2)

plot(quarter1947Ridge$cspe.diff,ylim=c(-0.4,0.12),main=expression(italic("Panel D: Ridge")),xlab="",ylab="",cex.main=1)
nberShade(col="gray65")
abline(h=0,col="black")
lines(quarter1947Ridge$cspe.diff,col="black",lwd=1)
lines(quarter1947Ridge$cspe.diff.wls,col="gray50",lwd=1)
box("plot",col="black",lwd=2)

plot(quarter1947pcr$firstcomp$cspe.diff,ylim=c(-0.07,0.12),main=expression(italic("Panel E: PrinComp(1)")),xlab="",ylab="",cex.main=1)
nberShade(col="gray65")
abline(h=0,col="black")
lines(quarter1947pcr$firstcomp$cspe.diff,col="black",lwd=1)
lines(quarter1947pcr$firstcomp$cspe.diff.wls,col="gray50",lwd=1)
box("plot",col="black",lwd=2)

plot(
  quarter1947pcr$rsquare$cspe.diff,
  ylim = c(-0.07, 0.12),
  main = expression(italic(paste(
    "Panel F: PrinComp(Opt))"))
), xlab = "", ylab = "", cex.main = 1)
nberShade(col = "gray65")
abline(h = 0, col = "black")
lines(quarter1947pcr$rsquare$cspe.diff,
      col = "black",
      lwd = 1)
lines(quarter1947pcr$rsquare$cspe.diff.wls,
      col = "gray50",
      lwd = 1)
box("plot", col = "black", lwd = 2)

plot(quarter1947threepass$cspe.diff,ylim=c(-0.07,0.12),main=expression(italic("Panel G: 3PassFilter")),xlab="",ylab="",cex.main=1)
nberShade(col="gray65")
abline(h=0,col="black")
lines(quarter1947threepass$cspe.diff,col="black",lwd=1)
lines(quarter1947threepass$cspe.diff.wls,col="gray50",lwd=1)
box("plot",col="black",lwd=2)

plot(quarter19473prfm$cspe.diff,ylim=c(-0.4,0.12),main=expression(italic("Panel H: 3PassFilterM")),xlab="",ylab="",cex.main=1)
nberShade(col="gray65")
abline(h=0,col="black")
lines(quarter19473prfm$cspe.diff,col="black",lwd=1)
lines(quarter19473prfm$cspe.diff.wls,col="gray50",lwd=1)
box("plot",col="black",lwd=2)

while (!is.null(dev.list()))  dev.off()