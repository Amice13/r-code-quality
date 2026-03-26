data <- read.csv("complete.data.iv.csv")
attach(data)
names(data)

library(car)

########################################################################

#Density Plots

########################################################################

#Density Plots of Log Mortality by Continent
png("MortbyCont.png")
plot(density(logem4[which(s.america==1)], na.rm=TRUE), lwd=2, xlim=c(1,9), main="", xlab="Log Mortality")
lines(density(logem4[which(africa==1)], na.rm=TRUE), lwd=2, lty=2, col="darkgreen")
lines(density(logem4[which(asia==1)], na.rm=TRUE), lwd=2, lty=3, col="blue")
lines(density(logem4[which(other==1)], na.rm=TRUE), lwd=2, lty=4, col="red")
legend(6, 2.2, c("S. America", "Africa", "Asia", "Other"), lwd=2, lty=c(1,2,3,4), col=c("black", "darkgreen", "blue", "red"))
rug(logem4[which(s.america==1)], col="black")
rug(logem4[which(africa==1)], col="darkgreen")
rug(logem4[which(asia==1)], col="blue")
rug(logem4[which(other==1)], col="red")
dev.off()

#Density Plots of Institutional Age by Continent
png("InstAgebyCont.png")
plot(density(inst.age.1817[which(s.america==1)], na.rm=TRUE), lwd=2, xlim=c(-1,500), ylim=c(0,.051), main="", xlab="Institutional Age")
lines(density(inst.age.1817[which(africa==1)], na.rm=TRUE), lwd=2, lty=2, col="darkgreen")
lines(density(inst.age.1817[which(asia==1 & inst.age.1817< 1000)], na.rm=TRUE), lwd=2, lty=3, col="blue")
lines(density(inst.age.1817[which(other==1)], na.rm=TRUE), lwd=2, lty=4, col="red")
legend(350, 0.05, c("S. America", "Africa", "Asia", "Other"), lwd=2, lty=c(1,2,3,4), col=c("black", "darkgreen", "blue", "red"))
rug(inst.age.1817[which(s.america==1)], col="black")
rug(inst.age.1817[which(africa==1)], col="darkgreen")
rug(inst.age.1817[which(asia==1 & inst.age.1817 < 1000)], col="blue")
rug(inst.age.1817[which(other==1)], col="red")
dev.off()


#Density plot of Log GDP 1995 by Continent
png("GDPbyCont.png")
plot(density(logpgp95[which(s.america==1)], na.rm=TRUE), lwd=2, xlim=c(5,12), ylim=c(0,1), main="", xlab="Log Per Capita GDP, 1995")
lines(density(logpgp95[which(africa==1)], na.rm=TRUE), lwd=2, lty=2, col="darkgreen")
lines(density(logpgp95[which(asia==1 & inst.age.1817< 1000)], na.rm=TRUE), lwd=2, lty=3, col="blue")
lines(density(logpgp95[which(other==1)], na.rm=TRUE), lwd=2, lty=4, col="red")
legend(5, 1, c("S. America", "Africa", "Asia", "Other"), lwd=2, lty=c(1,2,3,4), col=c("black", "darkgreen", "blue", "red"))
rug(logpgp95[which(s.america==1)], col="black")
rug(logpgp95[which(africa==1)], col="darkgreen")
rug(logpgp95[which(asia==1 & inst.age.1817 < 1000)], col="blue")
rug(logpgp95[which(other==1)], col="red")
dev.off()

#Density Plot of Constraint on the Executive 1900 by Continent
png("Cons00abyCont.png")
plot(density(cons00a[which(s.america==1)], na.rm=TRUE), lwd=2, ylim=c(0,.8), main="", xlab="Constraint on Exec. 1900")
lines(density(cons00a[which(africa==1)], na.rm=TRUE), lwd=2, lty=2, col="darkgreen")
lines(density(cons00a[which(asia==1 & inst.age.1817< 1000)], na.rm=TRUE), lwd=2, lty=3, col="blue")
lines(density(cons00a[which(other==1)], na.rm=TRUE), lwd=2, lty=4, col="red")
legend(6, .8, c("S. America", "Africa", "Asia", "Other"), lwd=2, lty=c(1,2,3,4), col=c("black", "darkgreen", "blue", "red"))
rug(cons00a[which(s.america==1)], col="black")
rug(cons00a[which(africa==1)], col="darkgreen")
rug(cons00a[which(asia==1 & inst.age.1817 < 1000)], col="blue")
rug(cons00a[which(other==1)], col="red")
dev.off()

#Density Plot of Constraint on the Executive 1900, Overall
png("Cons00aDens.png")
plot(density(cons00a, na.rm=TRUE), lwd=2, main="")
dev.off()


#Density Plot of Avg Prot Against Expr, Overall
png("AvrProtExp.png")
plot(density(avexpr[which(baseco==1)], na.rm=TRUE), lwd=2, main="")
dev.off()

