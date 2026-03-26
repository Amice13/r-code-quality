library("foreign")

# define function
se <- function(x) sd(x)/sqrt(length(x))

# load data
data <- read.dta("econ_militarydata_10-2-2015.dta")
#data <- read.dta("Make_or_Take_3Oct2017.dta")
## make this metric tons (1000kg) of coal equivalent per-person
#data$pec_pc <- data$pec/(data$tpop*1000)

## make this kilograms of coal equivalent per-person
#data$pec_pc1 <- data$pec/(data$tpop)

## make this kilograms of coal equivalent per-person
#data$pec_pc2 <- (data$pec*1000)/(data$tpop*1000)

test<- data
# set plot parameter values
par(mar=c(4,6,2,5))



MU <- aggregate(pec_pc ~ year, data=test, FUN=mean)
MU.se <- aggregate(pec_pc ~ year, data=test, FUN=se)
MU.99 <- aggregate(pec_pc ~ year, data=test, FUN=quantile, .99)
MU.95 <- aggregate(pec_pc ~ year, data=test, FUN=quantile, .95)
MU.75 <- aggregate(pec_pc ~ year, data=test, FUN=quantile, .75)
MU.50 <- aggregate(pec_pc ~ year, data=test, FUN=quantile, .50)
MU.25 <- aggregate(pec_pc ~ year, data=test, FUN=quantile, .25)
MU.05 <- aggregate(pec_pc ~ year, data=test, FUN=quantile, .05)

YEAR <- 1816:2007

plot(YEAR, log10(MU$pec_pc), ylim=c(-3.25,3.25), yaxt="n", xaxt="n", ylab="", xlab="", type="n", xlim=c(1816, 2020))

abline(h=0, col=grey(.75), lty=2)
abline(h=1, col=grey(.75), lty=2)
abline(h=-1, col=grey(.75), lty=2)
abline(h=2, col=grey(.75), lty=2)
abline(h=-2, col=grey(.75), lty=2)
abline(h=3, col=grey(.75), lty=2)
abline(h=-3, col=grey(.75), lty=2)

lines(YEAR, log10(MU.50$pec_pc), lwd=1.5)
#lines(YEAR, log10(MU.99$pec_pc), lwd=1.5)
lines(YEAR, log10(MU.95$pec_pc), lwd=1.5)
lines(YEAR, log10(MU.99$pec_pc), lwd=1.5)
#lines(YEAR, log10(MU.75$pec_pc), lwd=1.5)
lines(YEAR, log10(MU.50$pec_pc), lwd=1.5)
lines(YEAR[YEAR>=1900], log10(MU.25$pec_pc)[YEAR>=1900], lwd=1.5)



for(i in 1:length(YEAR)){
    
    lines(c(YEAR[i], YEAR[i]), c(log10(MU$pec_pc[i] - 1.0*MU.se$pec_pc[i]), log10(MU$pec_pc[i] + 1.0*MU.se$pec_pc[i])))
    
}


points(YEAR, log10(MU$pec_pc), pch=21, bg=grey(.9), col=grey(.1), cex=.5)

exp.y <- list(expression(10^-3), expression(10^-2), expression(10^-1), expression(10^0), expression(10^1), expression(10^2), expression(10^3))

for(i in 1:7){
	axis(side=2, at=c(i-4), labels=exp.y[[i]], font=1, cex.axis=1.35, padj=0, las=2)
	axis(side=4, at=c(i-4), labels=10^(i-4), font=1, cex.axis=1.35, padj=0, las=2)
}

#axis(1, at=c(1816), las=1)
axis(1, at=c(1825,1850,1875,1900,1925,1950,1975,2000), las=1)
axis(1, at=c(YEAR), labels=rep("", length(YEAR)), las=1)

mtext(side=2, "Metric tons of Coal or Equivalent (per capita)", line=4.0, font=2, cex=1.25)
mtext(side=1, "Year", line=2.5, font=2,  cex=1.25)
mtext(side=3, "Primary Energy Consumption Per Capita", font=2, cex=1.25, line=.5)

text(2018, .05+log10(MU.50$pec_pc)[length(YEAR)], labels="50%")
text(2018, .05+log10(MU.95$pec_pc)[length(YEAR)], labels="95%")
text(2018, .05+log10(MU$pec_pc)[length(YEAR)], labels="mean")
text(2018, .05+log10(MU.25$pec_pc)[length(YEAR)], labels="25%")



# -------------------------------------------------- #
plot(YEAR, MU$pec_pc, ylim=c(0,6.0), yaxt="n", xaxt="n", ylab="", xlab="", type="n", xlim=c(1816, 2020))

for(i in 1:length(YEAR)){
    
	lines(c(YEAR[i], YEAR[i]), c((MU$pec_pc[i] - 1.0*MU.se$pec_pc[i]),(MU$pec_pc[i] + 1.0*MU.se$pec_pc[i])))
    
}

points(YEAR, MU$pec_pc, pch=21, bg=grey(.9), col=grey(.1), cex=.5)

axis(1, at=c(1825,1850,1875,1900,1925,1950,1975,2000), las=1)
axis(1, at=c(YEAR), labels=rep("", length(YEAR)), las=1)
axis(2, at=c(0,1,2,3,4,5,6,7,8), las=2)
axis(4, at=c(0,1,2,3,4,5,6,7,8), las=2)

mtext(side=2, "Metric tons of Coal or Equivalent (per capita)", line=4.0, font=2, cex=1.25)
mtext(side=1, "Year", line=2.5, font=2,  cex=1.25)
mtext(side=3, "Primary Energy Consumption Per Capita", font=2, cex=1.25, line=.5)

text(2018, MU$pec_pc[length(YEAR)], labels="mean")




# -------------------------------------------------- #
MU <- aggregate(pec ~ year, data=test, FUN=mean)
MU.se <- aggregate(pec ~ year, data=test, FUN=se)
MU.99 <- aggregate(pec ~ year, data=test, FUN=quantile, .99)
MU.95 <- aggregate(pec ~ year, data=test, FUN=quantile, .95)
MU.75 <- aggregate(pec ~ year, data=test, FUN=quantile, .75)
MU.50 <- aggregate(pec ~ year, data=test, FUN=quantile, .50)
MU.25 <- aggregate(pec ~ year, data=test, FUN=quantile, .25)
MU.05 <- aggregate(pec ~ year, data=test, FUN=quantile, .05)


plot(YEAR, MU$pec, ylim=c(0,6.0), yaxt="n", xaxt="n", ylab="", xlab="", type="n", xlim=c(1816, 2020))

abline(h=0, col=grey(.75), lty=2)
abline(h=1, col=grey(.75), lty=2)
abline(h=2, col=grey(.75), lty=2)
abline(h=3, col=grey(.75), lty=2)
abline(h=4, col=grey(.75), lty=2)
abline(h=5, col=grey(.75), lty=2)
abline(h=6, col=grey(.75), lty=2)

lines(YEAR, log10(MU.50$pec), lwd=1.5)
#lines(YEAR, log10(MU.99$pec), lwd=1.5)
lines(YEAR, log10(MU.95$pec), lwd=1.5)
#lines(YEAR, log10(MU.75$pec), lwd=1.5)
lines(YEAR, log10(MU.50$pec), lwd=1.5)
lines(YEAR[YEAR>=1900], log10(MU.25$pec)[YEAR>=1900], lwd=1.5)



for(i in 1:length(YEAR)){
    
    lines(c(YEAR[i], YEAR[i]), c(log10(MU$pec[i] - 1.0*MU.se$pec[i]), log10(MU$pec[i] + 1.0*MU.se$pec[i])))
    
}


points(YEAR, log10(MU$pec), pch=21, bg=grey(.9), col=grey(.1), cex=.5)

exp.y <- list(expression(10^0), expression(10^1), expression(10^2), expression(10^3), expression(10^4), expression(10^5), expression(10^6))

for(i in 1:7){
	axis(side=2, at=c(i-1), labels=exp.y[[i]], font=1, cex.axis=1.35, padj=0, las=2)
	axis(side=4, at=c(i-1), labels=10^(i-1), font=1, cex.axis=1.35, padj=0, las=2)
}

#axis(1, at=c(1816), las=1)
axis(1, at=c(1825,1850,1875,1900,1925,1950,1975,2000), las=1)
axis(1, at=c(YEAR), labels=rep("", length(YEAR)), las=1)

mtext(side=2, "Metric tons of Coal or Equivalent", line=4.0, font=2, cex=1.25)
mtext(side=1, "Year", line=2.5, font=2,  cex=1.25)
mtext(side=3, "Primary Energy Consumption", font=2, cex=1.25, line=.5)

text(2018, .01+log10(MU.50$pec)[length(YEAR)], labels="50%")
text(2018, .01+log10(MU.95$pec)[length(YEAR)], labels="95%")
text(2018, .01+log10(MU$pec)[length(YEAR)], labels="mean")
text(2018, .01+log10(MU.25$pec)[length(YEAR)], labels="25%")



# -------------------------------------------------- #
plot(YEAR, MU$pec, ylim=c(0,200000), yaxt="n", xaxt="n", ylab="", xlab="", type="n", xlim=c(1816, 2020))

for(i in 1:length(YEAR)){
    
	lines(c(YEAR[i], YEAR[i]), c((MU$pec[i] - 1.0*MU.se$pec[i]),(MU$pec[i] + 1.0*MU.se$pec[i])))
    
}

points(YEAR, MU$pec, pch=21, bg=grey(.9), col=grey(.1), cex=.5)

axis(1, at=c(1825,1850,1875,1900,1925,1950,1975,2000), las=1)
axis(1, at=c(YEAR), labels=rep("", length(YEAR)), las=1)
axis(2, at=c(0,25000,50000,75000,100000,125000,150000,175000,200000), las=2)
axis(4, at=c(0,25000,50000,75000,100000,125000,150000,175000,200000), las=2)

mtext(side=2, "Metric tons of Coal or Equivalent", line=4.0, font=2, cex=1.25)
mtext(side=1, "Year", line=2.5, font=2,  cex=1.25)
mtext(side=3, "Primary Energy Consumption", font=2, cex=1.25, line=.5)

text(2018, MU$pec[length(YEAR)], labels="mean")


