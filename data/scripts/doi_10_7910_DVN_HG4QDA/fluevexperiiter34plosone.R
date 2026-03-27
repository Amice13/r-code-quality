
library(foreign)
library(extRemes)


mortality <-read.dta(file="mortalitydata.dta",convert.dates=TRUE, convert.factors=TRUE, missing.type=FALSE,
                 convert.underscore=FALSE,warn.missing.labels=TRUE)

attach(mortality)

summary(mortality)


# try some stuff with the power law
source("pareto.R")



# find cutoff with shalizi code - flu & pneumonia
pl_alt<- .pareto.fit.threshold(mortality$death_rate_combined,method="ml")
pl_alt
x<-mortality$death_rate_combined

set.seed(20160220)
pareto.tail.ks.test(x,2500)


nobs<-length(mortality$death_rate_combined)
nobs

ntail<-pl_alt$samples.over.threshold

adjst=ntail/nobs
xmin=pl_alt$xmin
alpha=pl_alt$exponent



#std error see eq (3.2) in ASN paper
se=(pl_alt$exponent-1)/sqrt(ntail)
se



# log-log plot death rates
x<-mortality$death_rate_combined
y <- (length(x) - rank(x, ties.method = "first"))/length(x)
plot(x, y, log = "xy", ylab = "Fraction with Death Rate(log scale)", xlab = "Death Rate (log scale)")

#------------------------------------------------------

# threshold excess range for rate
# first need to pick the threshold
threshrange.plot(mortality$death_rate_combined)
mrlplot(mortality$death_rate_combined,xlim=c(0,200))
#atdf(mortality$death_rate_combined,0.75)

#threshrange.plot(mortality$death_rate_combined,r=c(10,450))

# just for the flu

# threshold excess range for rate
# first need to pick the threshold
threshrange.plot(mortality$death_rate_flu)
mrlplot(mortality$death_rate_flu,xlim=c(0,50))


fitD <- fevd(death_rate_combined, mortality, threshold = 140, type = "GP",time.units="years")
summary(fitD)
distill(fitD)
plot(fitD)
ci(fitD,type="parameter")

# profile likelihood method
ci(fitD,type="parameter",which.par=2, method="proflik",xrange=c(0.0,1.6),verbose=TRUE)

return.level(fitD,return.period=c(2,5,10,15,20,25,30,50,100,200))
return.level(fitD,return.period=c(50,100,200,300,500,600))

pextRemes(fitD, q = c(122.7682, 232.87, 288.21, 365.45, 424.12), lower.tail = FALSE)


tes<-return.level(fitD,return.period=c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50),do.ci=TRUE)
tes
# plot of return period 
plot(tes[,2],type='o',ylab="Mortality Rate per 100,000",xlab="Return Period (years)" )
lines(tes[,1], lty="longdash")
lines(tes[,3], lty="longdash")

# return period in log scale on x axis
plot(tes[,2],log="x",type='o',ylab="Mortality Rate per 100,000",xlab="Return Period in Years (log scale)" )
lines(tes[,1], lty="longdash")
lines(tes[,3], lty="longdash")


# change the ylim include it down to 0
# see what referees say for that one
# return period in log scale on x axis
plot(tes[,2],log="x",type='o',ylab="Mortality Rate per 100,000",ylim=c(0,250),xlab="Return Period in Years (log scale)" )
lines(tes[,1], lty="longdash")
lines(tes[,3], lty="longdash")
 
                                    
# just fthe flu


fitD <- fevd(death_rate_flu, mortality, threshold = 20, type = "GP",time.units="years")
summary(fitD)
distill(fitD)
plot(fitD)
ci(fitD,type="parameter")
# profile likelihood method
ci(fitD,type="parameter",which.par=2, method="proflik",xrange=c(0,1.6),verbose=TRUE)

# profile likelihood method -return levels
ci(fitD,type="parameter",method="proflik",xrange=c(10,30),verbose=TRUE)


pextRemes(fitD, q = c(12.185, 73.968, 25, 30, 40, 50,100), lower.tail = FALSE)

return.level(fitD,return.period=c(2,5,10,15,20,25,30,50,100,200))
return.level(fitD,do.ci=TRUE)
tes<-return.level(fitD,return.period=c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50),do.ci=TRUE)
tes
# plot of return period 
plot(tes[,2],type='o',ylab="Mortality Rate per 100,000",xlab="Return Period (years)" )
lines(tes[,1], lty="longdash")
lines(tes[,3], lty="longdash")

# return period in log scale on x axis
plot(tes[,2],log="x",type='o',ylab="Mortality Rate per 100,000",xlab="Return Period in Years (log scale)" )
lines(tes[,1], lty="longdash")
lines(tes[,3], lty="longdash")


pextRemes(fitD, q = c(12.185, 73.968, 25, 30, 40, 50,100), lower.tail = FALSE)

