##################################################
# Begin replication code
##################################################

library(foreign)
library(locpol)
library(xtable)
library(sandwich)
library(lmtest)
library(apsrtable)
options(scipen=10)

rm(list=ls())
setwd("~/Dropbox/GSP/Current")

wb <- read.csv("wb-use.csv")
write.dta(wb, file="wb-use-stata.dta")
##################################################
# Functions to create lags, leads
##################################################

lagFun <- function(x,lags,csvar){
	cs.vec <- unique(csvar)
	for(i in cs.vec){
		tvar <- x[csvar==i]
		tvar.l <- length(tvar)
		tvar.r <- rep(NA,tvar.l)
		if(sum(csvar==i)>=lags){
			tvar.r[-(1:lags)] <- head(x[csvar==i], -lags)
		}
		if(i == cs.vec[1]){outvar <- tvar.r}
		if(i != cs.vec[1]){outvar <- c(outvar,tvar.r)}
	}
	return(outvar)
}


leadFun <- function(x,leads,csvar){	
	cs.vec <- unique(csvar)
	for(i in cs.vec){
		tvar <- x[csvar==i]
		tvar.l <- length(tvar)
		tvar.r <- rep(NA,tvar.l)
		if(sum(csvar==i)>=leads){
			tvar.r[-((tvar.l-leads+1):tvar.l)] <- tail(x[csvar==i], -leads)
		}
		if(i == cs.vec[1]){outvar <- tvar.r}
		if(i != cs.vec[1]){outvar <- c(outvar,tvar.r)}
	}
	return(outvar)
}

##################################################
# Cluster-robust standard error
##################################################

robust.se <- function(model, cluster){
	require(sandwich)
	require(lmtest)
	M <- length(unique(cluster))
	N <- length(cluster)
	K <- model$rank
	dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
	uj <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum))
	uj[is.na(uj)] <- 0
	rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
	rcse.se <- coeftest(model, rcse.cov)
	return(list(rcse.cov, rcse.se))
}


##################################################
# Forcing variable density test (McCrary 2008)
# for appendix
##################################################
# Set parameters:
histbin <- .015
xvar <- wb$IVibrd.sc

# Execute:
breakrange.sc <- range(xvar, na.rm=T)
breakrange <- c(histbin*floor((breakrange.sc/histbin)[1])-histbin/2,  histbin*ceiling((breakrange.sc/histbin)[2])+histbin/2)
histx <- hist(xvar, breaks=seq(from=breakrange[1], to=breakrange[2], by=histbin), plot=F)
pdf(file=paste("rd-out/densitytest.pdf", 
				sep=""),
		height=5, 
		width=8)
par(mfrow=c(1,1))
plot(histx$mids, histx$density, pch=19, cex=.25, xlim=breakrange, xlab="GNI/capita - IBRD Cutoff (log scale)", ylab="Density")
bw <- histbin*10

densdat <- data.frame(	    counts=histx$counts, 	
							mids=histx$mids, 
							density=histx$density, 
							triwt = apply(cbind(1-abs(histx$mids/bw),0),1,max),
							I.x = as.numeric(histx$mids>=0))
densdatl <- subset(densdat, abs(mids)<=abs(breakrange[1])&mids<=0.000000001)
densdatr <- subset(densdat, abs(mids)<=breakrange[2]&mids>=-.000000001)

lpl <- locpol(density~mids, data=densdatl, bw=bw, kernel=TrianK, deg=1)
lpr <- locpol(density~mids, data=densdatr, bw=bw, kernel=TrianK, deg=1)

points(lpl$lpFit$mids, lpl$lpFit$density, type="l", col="black")
points(lpr$lpFit$mids, lpr$lpFit$density, type="l", col="black")
abline(v=0, lty="solid")

# McCrary's density discontinuity test:
f.plu <- lpr$lpFit$density[1]
f.min <- lpl$lpFit$density[length(lpl$lpFit$density)]
theta.h <- log(f.plu) - log(f.min)
sigma.theta <- sqrt((1/(length(xvar)*bw))*(24/5)*((1/f.plu)+(1/f.min)))
dens.pval <- data.frame(pval = 2*(1-pnorm(abs(theta.h/sigma.theta))))
rownames(dens.pval) <- ""
dens.pval
text(2.5, .35, paste("McCrary test p =", round(dens.pval,2)), cex=1)
dev.off()


##################################################
# Effects on political liberalization RD estimation:
##################################################

# Main outcome variables
DV.main <- c("fh_score", "p_polity2", "fh_polity2", "uds_score")
DV.labs <- c("FH Score", "Polity Score", "Polity-FH Aggregate", "UDS Score")

# Lists to store results for main results and then different robustness checks
fit.list.b2.dv  <- 
	fit.list.dv  <- 
	fit.list.b5 <- 
	fit.list.b4 <- 
	fit.list.b3 <- 
	fit.list.b2 <- 
	fit.list.l  <- 
	fit.list.l.dv <- 
	fit.list.t <- 
	fit.list <- 
	fit.list.placebo.add <-
	fit.list.ee <-
	fit.list.usa <-
	fit.list.ceiling <-
	fit.list.linger <-
	list(NA)

for(i in 1:length(DV.main)){
DV.sc <- wb[,DV.main[i]]
dvUp <- DV.main[i]
IV <- wb$IVibrd.sc

# Standardize outcome scale relative to control group sd:
DV <- (DV.sc - mean(DV.sc[!is.na(IV)&IV<0], 
			na.rm=T))/sd(DV.sc[!is.na(IV)&IV<0], 
			na.rm=T)
DV.lab <- DV.labs[i]

sink(paste("rd-out/sd-", DV.main[i],".txt", sep=""))
sdUp <- sd(DV.sc[!is.na(IV)&IV<0], 
			na.rm=T)
print(sdUp)
sink()

lbw <- .1

##########################################
# Main results (analysis on changes)
##########################################

fit.data <- data.frame(	country = wb$country_name,
						year = wb$year,
						IV = IV,
						I.IV0 = as.numeric(IV>0),
						DV = DV,
						DV.l1 = DV-lagFun(DV, 2, wb$country_name),  #t-2 to t (placebo)
						DV.f2 = leadFun(DV, 2, wb$country_name)-DV, #t to t+2 (instantaneous)
						DV.f3 = leadFun(DV, 3, wb$country_name)-DV, #t to t+3 (1 year out)
						DV.f4 = leadFun(DV, 4, wb$country_name)-DV, #t to t+4 (2 years out)
						DV.f5 = leadFun(DV, 5, wb$country_name)-DV, #t to t+5 (3 years out)
						triwt = apply(cbind(1-abs(IV/lbw),0),1,max),
						grad = wb$grad 
						)

data.l1 <- subset(fit.data, triwt>0&!is.na(DV.l1)&!is.na(I.IV0)&!is.na(IV))
data.f2 <- subset(fit.data, triwt>0&!is.na(DV.f2)&!is.na(I.IV0)&!is.na(IV))
data.f3 <- subset(fit.data, triwt>0&!is.na(DV.f3)&!is.na(I.IV0)&!is.na(IV))
data.f4 <- subset(fit.data, triwt>0&!is.na(DV.f4)&!is.na(I.IV0)&!is.na(IV))
data.f5 <- subset(fit.data, triwt>0&!is.na(DV.f5)&!is.na(I.IV0)&!is.na(IV))

fit.l1 <- lm(DV.l1~I.IV0*IV, data=data.l1)
fit.f2 <- lm(DV.f2~I.IV0*IV, data=data.f2)
fit.f3 <- lm(DV.f3~I.IV0*IV, data=data.f3)
fit.f4 <- lm(DV.f4~I.IV0*IV, data=data.f4)
fit.f5 <- lm(DV.f5~I.IV0*IV, data=data.f5)

fit.l1$se <- robust.se(fit.l1,as.character(data.l1$country))[[2]][,2]
fit.f2$se <- robust.se(fit.f2,as.character(data.f2$country))[[2]][,2]
fit.f3$se <- robust.se(fit.f3,as.character(data.f3$country))[[2]][,2]
fit.f4$se <- robust.se(fit.f4,as.character(data.f4$country))[[2]][,2]
fit.f5$se <- robust.se(fit.f5,as.character(data.f5$country))[[2]][,2]

fit.list[[1+5*(i-1)]] <- fit.l1
fit.list[[2+5*(i-1)]] <- fit.f2
fit.list[[3+5*(i-1)]] <- fit.f3
fit.list[[4+5*(i-1)]] <- fit.f4
fit.list[[5+5*(i-1)]] <- fit.f5

# Regression discontinuity plot

ylimplot <- c(-.5,.5)
lrangelim <- .25

pdf(file=paste("rd-out/",DV.main[i], "-plot-main.pdf",sep=""), height=2, width=8)
par(mfrow=c(1,5))

# Placebo
lpl <- locpol(DV.l1 ~ IV, 
				data=subset(fit.data, IV<0&abs(IV)<lrangelim), 
				bw=lbw, 
				deg=1, 
				xeval=seq(from=-lrangelim, to =0, by=.005))
lpr <- locpol(DV.l1 ~ IV, 
				data=subset(fit.data, IV>0&abs(IV)<lrangelim), 
				bw=lbw, 
				deg=1, 
				xeval=seq(from=0, to =lrangelim, by=.005))

plot(fit.data$IV, fit.data$DV.l1, 
		xlim=c(-lrangelim, lrangelim), 
		type="p", 
		ylim=ylimplot, 
		xlab="Log GNI - IBRD Cut", 
		col="gray", 
		pch=19, 
		cex=.5, 
		ylab= bquote(Delta*.(DV.lab)),
		main="Placebo")
#abline(v=0)
points(lpl$lpFit$IV, lpl$lpFit$DV.l1, type="l")
points(lpr$lpFit$IV, lpr$lpFit$DV.l1, type="l")

# Instantaneous
lpl <- locpol(DV.f2 ~ IV, 
				data=subset(fit.data, IV<0&abs(IV)<lrangelim), 
				bw=lbw, 
				deg=1, 
				xeval=seq(from=-lrangelim, to =0, by=.005))
lpr <- locpol(DV.f2 ~ IV, 
				data=subset(fit.data, IV>0&abs(IV)<lrangelim),
				bw=lbw,
				deg=1,
				xeval=seq(from=0, to =lrangelim, by=.005))

plot(fit.data$IV, 
		fit.data$DV.f2, 
		xlim=c(-lrangelim, lrangelim), 
		type="p", 
		ylim=ylimplot,
		xlab="Log GNI - IBRD Cut",
		col="gray",
		pch=19,
		cex=.5, 
		ylab="",
		main="Instantaneous")
#abline(v=0)
points(lpl$lpFit$IV, lpl$lpFit$DV.f2, type="l")
points(lpr$lpFit$IV, lpr$lpFit$DV.f2, type="l")

# One year forward
lpl <- locpol(DV.f3 ~ IV, 
				data=subset(fit.data, IV<=0&abs(IV)<lrangelim), 
				bw=lbw,
				deg=1,
				xeval=seq(from=-lrangelim, to =0, by=.005))
lpr <- locpol(DV.f3 ~ IV, 
				data=subset(fit.data, IV>=0&abs(IV)<lrangelim),
				bw=lbw,
				deg=1,
				xeval=seq(from=0, to =lrangelim, by=.005))

plot(fit.data$IV, 
		fit.data$DV.f3,
		xlim=c(-lrangelim, lrangelim),
		type="p", 
		ylim=ylimplot,
		xlab="Log GNI - IBRD Cut",
		col="gray",
		pch=19,
		cex=.5, 
		ylab="",
		main="One year forward")
#abline(v=0)
points(lpl$lpFit$IV, lpl$lpFit$DV.f3, type="l")
points(lpr$lpFit$IV, lpr$lpFit$DV.f3, type="l")

# Two years forward
lpl <- locpol(DV.f4 ~ IV, 
				data=subset(fit.data, IV<=0&abs(IV)<lrangelim), 
				bw=lbw,
				deg=1,
				xeval=seq(from=-lrangelim, to =0, by=.005))
lpr <- locpol(DV.f4 ~ IV, 
				data=subset(fit.data, IV>=0&abs(IV)<lrangelim),
				bw=lbw,
				deg=1,
				xeval=seq(from=0, to =lrangelim, by=.005))

plot(fit.data$IV, 
		fit.data$DV.f4,
		xlim=c(-lrangelim, lrangelim),
		type="p", 
		ylim=ylimplot,
		xlab="Log GNI - IBRD Cut",
		col="gray",
		pch=19,
		cex=.5, 
		ylab="",
		main="Two years forward")
#abline(v=0)
points(lpl$lpFit$IV, lpl$lpFit$DV.f4, type="l")
points(lpr$lpFit$IV, lpr$lpFit$DV.f4, type="l")

# Three years forward
lpl <- locpol(DV.f5 ~ IV, 
				data=subset(fit.data, IV<=0&abs(IV)<lrangelim), 
				bw=lbw,
				deg=1,
				xeval=seq(from=-lrangelim, to =0, by=.005))
lpr <- locpol(DV.f5 ~ IV, 
				data=subset(fit.data, IV>=0&abs(IV)<lrangelim),
				bw=lbw,
				deg=1,
				xeval=seq(from=0, to =lrangelim, by=.005))

plot(fit.data$IV, 
		fit.data$DV.f5,
		xlim=c(-lrangelim, lrangelim),
		type="p", 
		ylim=ylimplot,
		xlab="Log GNI - IBRD Cut",
		col="gray",
		pch=19,
		cex=.5, 
		ylab="",
		main="Three years forward")
#abline(v=0)
points(lpl$lpFit$IV, lpl$lpFit$DV.f5, type="l")
points(lpr$lpFit$IV, lpr$lpFit$DV.f5, type="l")

dev.off()

# End of main results


##########################################
# Robustness 1: triangular kernel
##########################################

fit.l1.t <- lm(DV.l1~I.IV0*IV, data=data.l1, weights=data.l1$triwt)
fit.f2.t <- lm(DV.f2~I.IV0*IV, data=data.f2, weights=data.f2$triwt)
fit.f3.t <- lm(DV.f3~I.IV0*IV, data=data.f3, weights=data.f3$triwt)
fit.f4.t <- lm(DV.f4~I.IV0*IV, data=data.f4, weights=data.f4$triwt)
fit.f5.t <- lm(DV.f5~I.IV0*IV, data=data.f5, weights=data.f5$triwt)

fit.l1.t$se <- robust.se(fit.l1.t,as.character(data.l1$country))[[2]][,2]
fit.f2.t$se <- robust.se(fit.f2.t,as.character(data.f2$country))[[2]][,2]
fit.f3.t$se <- robust.se(fit.f3.t,as.character(data.f3$country))[[2]][,2]
fit.f4.t$se <- robust.se(fit.f4.t,as.character(data.f4$country))[[2]][,2]
fit.f5.t$se <- robust.se(fit.f5.t,as.character(data.f5$country))[[2]][,2]

fit.list.t[[1+5*(i-1)]] <- fit.l1.t
fit.list.t[[2+5*(i-1)]] <- fit.f2.t
fit.list.t[[3+5*(i-1)]] <- fit.f3.t
fit.list.t[[4+5*(i-1)]] <- fit.f4.t
fit.list.t[[5+5*(i-1)]] <- fit.f5.t

##########################################
# Robustness 2: different bandwidths
##########################################

lbw3 <- .075
lbw4 <- .125
lbw5 <- .15

fit.data$triwt3 <- apply(cbind(1-abs(IV/lbw3),0),1,max)
fit.data$triwt4 <- apply(cbind(1-abs(IV/lbw4),0),1,max)
fit.data$triwt5 <- apply(cbind(1-abs(IV/lbw5),0),1,max)

#BW3
data.l1.b3 <- subset(fit.data, triwt3>0&!is.na(DV.l1)&!is.na(I.IV0)&!is.na(IV))
data.f2.b3 <- subset(fit.data, triwt3>0&!is.na(DV.f2)&!is.na(I.IV0)&!is.na(IV))
data.f3.b3 <- subset(fit.data, triwt3>0&!is.na(DV.f3)&!is.na(I.IV0)&!is.na(IV))
data.f4.b3 <- subset(fit.data, triwt3>0&!is.na(DV.f4)&!is.na(I.IV0)&!is.na(IV))
data.f5.b3 <- subset(fit.data, triwt3>0&!is.na(DV.f5)&!is.na(I.IV0)&!is.na(IV))

fit.l1.b3 <- lm(DV.l1~I.IV0*IV, data=data.l1.b3)
fit.f2.b3 <- lm(DV.f2~I.IV0*IV, data=data.f2.b3)
fit.f3.b3 <- lm(DV.f3~I.IV0*IV, data=data.f3.b3)
fit.f4.b3 <- lm(DV.f4~I.IV0*IV, data=data.f4.b3)
fit.f5.b3 <- lm(DV.f5~I.IV0*IV, data=data.f5.b3)

fit.l1.b3$se <- robust.se(fit.l1.b3,as.character(data.l1.b3$country))[[2]][,2]
fit.f2.b3$se <- robust.se(fit.f2.b3,as.character(data.f2.b3$country))[[2]][,2]
fit.f3.b3$se <- robust.se(fit.f3.b3,as.character(data.f3.b3$country))[[2]][,2]
fit.f4.b3$se <- robust.se(fit.f4.b3,as.character(data.f4.b3$country))[[2]][,2]
fit.f5.b3$se <- robust.se(fit.f5.b3,as.character(data.f5.b3$country))[[2]][,2]

fit.list.b3[[1+5*(i-1)]] <- fit.l1.b3
fit.list.b3[[2+5*(i-1)]] <- fit.f2.b3
fit.list.b3[[3+5*(i-1)]] <- fit.f3.b3
fit.list.b3[[4+5*(i-1)]] <- fit.f4.b3
fit.list.b3[[5+5*(i-1)]] <- fit.f5.b3

#BW4
data.l1.b4 <- subset(fit.data, triwt4>0&!is.na(DV.l1)&!is.na(I.IV0)&!is.na(IV))
data.f2.b4 <- subset(fit.data, triwt4>0&!is.na(DV.f2)&!is.na(I.IV0)&!is.na(IV))
data.f3.b4 <- subset(fit.data, triwt4>0&!is.na(DV.f3)&!is.na(I.IV0)&!is.na(IV))
data.f4.b4 <- subset(fit.data, triwt4>0&!is.na(DV.f4)&!is.na(I.IV0)&!is.na(IV))
data.f5.b4 <- subset(fit.data, triwt4>0&!is.na(DV.f5)&!is.na(I.IV0)&!is.na(IV))

fit.l1.b4 <- lm(DV.l1~I.IV0*IV, data=data.l1.b4)
fit.f2.b4 <- lm(DV.f2~I.IV0*IV, data=data.f2.b4)
fit.f3.b4 <- lm(DV.f3~I.IV0*IV, data=data.f3.b4)
fit.f4.b4 <- lm(DV.f4~I.IV0*IV, data=data.f4.b4)
fit.f5.b4 <- lm(DV.f5~I.IV0*IV, data=data.f5.b4)

fit.l1.b4$se <- robust.se(fit.l1.b4,as.character(data.l1.b4$country))[[2]][,2]
fit.f2.b4$se <- robust.se(fit.f2.b4,as.character(data.f2.b4$country))[[2]][,2]
fit.f3.b4$se <- robust.se(fit.f3.b4,as.character(data.f3.b4$country))[[2]][,2]
fit.f4.b4$se <- robust.se(fit.f4.b4,as.character(data.f4.b4$country))[[2]][,2]
fit.f5.b4$se <- robust.se(fit.f5.b4,as.character(data.f5.b4$country))[[2]][,2]

fit.list.b4[[1+5*(i-1)]] <- fit.l1.b4
fit.list.b4[[2+5*(i-1)]] <- fit.f2.b4
fit.list.b4[[3+5*(i-1)]] <- fit.f3.b4
fit.list.b4[[4+5*(i-1)]] <- fit.f4.b4
fit.list.b4[[5+5*(i-1)]] <- fit.f5.b4

#BW5
data.l1.b5 <- subset(fit.data, triwt5>0&!is.na(DV.l1)&!is.na(I.IV0)&!is.na(IV))
data.f2.b5 <- subset(fit.data, triwt5>0&!is.na(DV.f2)&!is.na(I.IV0)&!is.na(IV))
data.f3.b5 <- subset(fit.data, triwt5>0&!is.na(DV.f3)&!is.na(I.IV0)&!is.na(IV))
data.f4.b5 <- subset(fit.data, triwt5>0&!is.na(DV.f4)&!is.na(I.IV0)&!is.na(IV))
data.f5.b5 <- subset(fit.data, triwt5>0&!is.na(DV.f5)&!is.na(I.IV0)&!is.na(IV))

fit.l1.b5 <- lm(DV.l1~I.IV0*IV, data=data.l1.b5)
fit.f2.b5 <- lm(DV.f2~I.IV0*IV, data=data.f2.b5)
fit.f3.b5 <- lm(DV.f3~I.IV0*IV, data=data.f3.b5)
fit.f4.b5 <- lm(DV.f4~I.IV0*IV, data=data.f4.b5)
fit.f5.b5 <- lm(DV.f5~I.IV0*IV, data=data.f5.b5)

fit.l1.b5$se <- robust.se(fit.l1.b5,as.character(data.l1.b5$country))[[2]][,2]
fit.f2.b5$se <- robust.se(fit.f2.b5,as.character(data.f2.b5$country))[[2]][,2]
fit.f3.b5$se <- robust.se(fit.f3.b5,as.character(data.f3.b5$country))[[2]][,2]
fit.f4.b5$se <- robust.se(fit.f4.b5,as.character(data.f4.b5$country))[[2]][,2]
fit.f5.b5$se <- robust.se(fit.f5.b5,as.character(data.f5.b5$country))[[2]][,2]

fit.list.b5[[1+5*(i-1)]] <- fit.l1.b5
fit.list.b5[[2+5*(i-1)]] <- fit.f2.b5
fit.list.b5[[3+5*(i-1)]] <- fit.f3.b5
fit.list.b5[[4+5*(i-1)]] <- fit.f4.b5
fit.list.b5[[5+5*(i-1)]] <- fit.f5.b5

##########################################
# Robustness 3: level outcomes, rather than changes
##########################################

fit.data.l <- data.frame(country = wb$country_name,
						year = wb$year,
						IV = IV,
						I.IV0 = as.numeric(IV>0),
						DV.l2 = lagFun(DV, 2, wb$country_name),
						DV = DV,				
						DV.f2 = leadFun(DV, 2, wb$country_name),
						DV.f3 = leadFun(DV, 3, wb$country_name),						
						DV.f4 = leadFun(DV, 4, wb$country_name),
						DV.f5 = leadFun(DV, 5, wb$country_name),
						triwt = apply(cbind(1-abs(IV/lbw),0),1,max) 
						)

data.l1.l <- subset(fit.data.l, triwt>0&!is.na(DV)&!is.na(I.IV0)&!is.na(IV))
data.f2.l <- subset(fit.data.l, triwt>0&!is.na(DV.f2)&!is.na(I.IV0)&!is.na(IV))
data.f3.l <- subset(fit.data.l, triwt>0&!is.na(DV.f3)&!is.na(I.IV0)&!is.na(IV))
data.f4.l <- subset(fit.data.l, triwt>0&!is.na(DV.f4)&!is.na(I.IV0)&!is.na(IV))
data.f5.l <- subset(fit.data.l, triwt>0&!is.na(DV.f5)&!is.na(I.IV0)&!is.na(IV))

fit.l1.l <- lm(DV~I.IV0*IV, data=data.l1.l)
fit.f2.l <- lm(DV.f2~I.IV0*IV, data=data.f2.l)
fit.f3.l <- lm(DV.f3~I.IV0*IV, data=data.f3.l)
fit.f4.l <- lm(DV.f4~I.IV0*IV, data=data.f4.l)
fit.f5.l <- lm(DV.f5~I.IV0*IV, data=data.f5.l)

fit.l1.l$se <- robust.se(fit.l1.l,as.character(data.l1.l$country))[[2]][,2]
fit.f2.l$se <- robust.se(fit.f2.l,as.character(data.f2.l$country))[[2]][,2]
fit.f3.l$se <- robust.se(fit.f3.l,as.character(data.f3.l$country))[[2]][,2]
fit.f4.l$se <- robust.se(fit.f4.l,as.character(data.f4.l$country))[[2]][,2]
fit.f5.l$se <- robust.se(fit.f5.l,as.character(data.f5.l$country))[[2]][,2]

fit.list.l[[1+5*(i-1)]] <- fit.l1.l
fit.list.l[[2+5*(i-1)]] <- fit.f2.l
fit.list.l[[3+5*(i-1)]] <- fit.f3.l
fit.list.l[[4+5*(i-1)]] <- fit.f4.l
fit.list.l[[5+5*(i-1)]] <- fit.f5.l

# Levels, controlling for lagged DV

data.f2.l.dv <- subset(fit.data.l, triwt>0&!is.na(DV)&!is.na(DV.f2)&!is.na(I.IV0)&!is.na(IV))
data.f3.l.dv <- subset(fit.data.l, triwt>0&!is.na(DV)&!is.na(DV.f3)&!is.na(I.IV0)&!is.na(IV))
data.f4.l.dv <- subset(fit.data.l, triwt>0&!is.na(DV)&!is.na(DV.f4)&!is.na(I.IV0)&!is.na(IV))
data.f5.l.dv <- subset(fit.data.l, triwt>0&!is.na(DV)&!is.na(DV.f5)&!is.na(I.IV0)&!is.na(IV))

fit.f2.l.dv <- lm(DV.f2~I.IV0*IV+DV, data=data.f2.l.dv)
fit.f3.l.dv <- lm(DV.f3~I.IV0*IV+DV, data=data.f3.l.dv)
fit.f4.l.dv <- lm(DV.f4~I.IV0*IV+DV, data=data.f4.l.dv)
fit.f5.l.dv <- lm(DV.f5~I.IV0*IV+DV, data=data.f5.l.dv)

fit.f2.l.dv$se <- robust.se(fit.f2.l.dv,as.character(data.f2.l.dv$country))[[2]][,2]
fit.f3.l.dv$se <- robust.se(fit.f3.l.dv,as.character(data.f3.l.dv$country))[[2]][,2]
fit.f4.l.dv$se <- robust.se(fit.f4.l.dv,as.character(data.f4.l.dv$country))[[2]][,2]
fit.f5.l.dv$se <- robust.se(fit.f5.l.dv,as.character(data.f5.l.dv$country))[[2]][,2]

fit.list.l.dv[[1+4*(i-1)]] <- fit.f2.l.dv
fit.list.l.dv[[2+4*(i-1)]] <- fit.f3.l.dv
fit.list.l.dv[[3+4*(i-1)]] <- fit.f4.l.dv
fit.list.l.dv[[4+4*(i-1)]] <- fit.f5.l.dv

# Reviewer request: add placebo regression
data.l2.l.dv <- subset(fit.data.l, triwt>0&!is.na(DV)&!is.na(DV.l2)&!is.na(I.IV0)&!is.na(IV))
fit.l2.l.dv <- lm(DV.l2~I.IV0*IV+DV, data=data.l2.l.dv)
fit.l2.l.dv$se <- robust.se(fit.l2.l.dv,as.character(data.l2.l.dv$country))[[2]][,2]

fit.list.placebo.add[[1+2*(i-1)]] <- fit.l2.l.dv


##########################################
# Robustness 4: changes, controlling for lagged DV
##########################################

data.f2.dv <- subset(fit.data, triwt>0&!is.na(DV)&!is.na(DV.f2)&!is.na(I.IV0)&!is.na(IV))
data.f3.dv <- subset(fit.data, triwt>0&!is.na(DV)&!is.na(DV.f3)&!is.na(I.IV0)&!is.na(IV))
data.f4.dv <- subset(fit.data, triwt>0&!is.na(DV)&!is.na(DV.f4)&!is.na(I.IV0)&!is.na(IV))
data.f5.dv <- subset(fit.data, triwt>0&!is.na(DV)&!is.na(DV.f5)&!is.na(I.IV0)&!is.na(IV))

fit.f2.dv <- lm(DV.f2~I.IV0*IV+DV, data=data.f2.dv)
fit.f3.dv <- lm(DV.f3~I.IV0*IV+DV, data=data.f3.dv)
fit.f4.dv <- lm(DV.f4~I.IV0*IV+DV, data=data.f4.dv)
fit.f5.dv <- lm(DV.f5~I.IV0*IV+DV, data=data.f5.dv)

# Adding squared control
fit.f2.dv2 <- lm(DV.f2~I.IV0*IV+DV+I(DV^2), data=data.f2.dv)
fit.f3.dv2 <- lm(DV.f3~I.IV0*IV+DV+I(DV^2), data=data.f3.dv)
fit.f4.dv2 <- lm(DV.f4~I.IV0*IV+DV+I(DV^2), data=data.f4.dv)
fit.f5.dv2 <- lm(DV.f5~I.IV0*IV+DV+I(DV^2), data=data.f5.dv)

fit.f2.dv$se <- robust.se(fit.f2.dv,as.character(data.f2.dv$country))[[2]][,2]
fit.f3.dv$se <- robust.se(fit.f3.dv,as.character(data.f3.dv$country))[[2]][,2]
fit.f4.dv$se <- robust.se(fit.f4.dv,as.character(data.f4.dv$country))[[2]][,2]
fit.f5.dv$se <- robust.se(fit.f5.dv,as.character(data.f5.dv$country))[[2]][,2]

fit.f2.dv2$se <- robust.se(fit.f2.dv2,as.character(data.f2.dv$country))[[2]][,2]
fit.f3.dv2$se <- robust.se(fit.f3.dv2,as.character(data.f3.dv$country))[[2]][,2]
fit.f4.dv2$se <- robust.se(fit.f4.dv2,as.character(data.f4.dv$country))[[2]][,2]
fit.f5.dv2$se <- robust.se(fit.f5.dv2,as.character(data.f5.dv$country))[[2]][,2]

fit.list.dv[[1+8*(i-1)]] <- fit.f2.dv
fit.list.dv[[2+8*(i-1)]] <- fit.f3.dv
fit.list.dv[[3+8*(i-1)]] <- fit.f4.dv
fit.list.dv[[4+8*(i-1)]] <- fit.f5.dv
fit.list.dv[[5+8*(i-1)]] <- fit.f2.dv2
fit.list.dv[[6+8*(i-1)]] <- fit.f3.dv2
fit.list.dv[[7+8*(i-1)]] <- fit.f4.dv2
fit.list.dv[[8+8*(i-1)]] <- fit.f5.dv2

# Reviewer request: add placebo regression
data.l1.dv <- subset(fit.data, triwt>0&!is.na(DV)&!is.na(DV.l1)&!is.na(I.IV0)&!is.na(IV))
fit.l1.l.dv <- lm(DV.l1~I.IV0*IV+DV+I(DV^2), data=data.l1.dv)
fit.l1.l.dv$se <- robust.se(fit.l1.l.dv,as.character(data.l1.dv$country))[[2]][,2]
fit.list.placebo.add[[2+2*(i-1)]] <- fit.l1.l.dv



##########################################
# EU Acceding Country Interaction
##########################################
# As per http://ec.europa.eu/economy_finance/international/enlargement/index_en.htm,
# (accessed 10/25/16) the following countries were either post 1987 accession, 
# applicant, or potential candidate countries:

ee.list <- c(	"Albania",
				"Bosnia and Herzegovina",
				"Bulgaria",
				"Czech Republic",
				"Croatia",
				"Estonia",
				"Cyprus",
				"Hungary",
				"Kosovo",
				"Latvia",
				"Lithuania",
				"Macedonia, FYR",
				"Malta",
				"Montenegro",
				"Poland",
				"Romania",
				"Serbia",
				"Slovak Republic",
				"Slovenia",
				"Turkey")

fit.data.ee <- data.frame(	country = wb$country_name,
						year = wb$year,
						IV = IV,
						I.IV0 = as.numeric(IV>0),
						DV = DV,
						DV.l1 = DV-lagFun(DV, 2, wb$country_name),  #t-2 to t (placebo)
						DV.f2 = leadFun(DV, 2, wb$country_name)-DV, #t to t+2 (instantaneous)
						DV.f3 = leadFun(DV, 3, wb$country_name)-DV, #t to t+3 (1 year out)
						DV.f4 = leadFun(DV, 4, wb$country_name)-DV, #t to t+4 (2 years out)
						DV.f5 = leadFun(DV, 5, wb$country_name)-DV, #t to t+5 (3 years out)
						triwt = apply(cbind(1-abs(IV/lbw),0),1,max),
						grad = wb$grad,
						I.EE = as.numeric(wb$country_name%in%ee.list) 
						)

data.l1.ee <- subset(fit.data.ee, triwt>0&!is.na(DV.l1)&!is.na(I.IV0)&!is.na(IV)&!is.na(I.EE))
data.f2.ee <- subset(fit.data.ee, triwt>0&!is.na(DV.f2)&!is.na(I.IV0)&!is.na(IV)&!is.na(I.EE))
data.f3.ee <- subset(fit.data.ee, triwt>0&!is.na(DV.f3)&!is.na(I.IV0)&!is.na(IV)&!is.na(I.EE))
data.f4.ee <- subset(fit.data.ee, triwt>0&!is.na(DV.f4)&!is.na(I.IV0)&!is.na(IV)&!is.na(I.EE))
data.f5.ee <- subset(fit.data.ee, triwt>0&!is.na(DV.f5)&!is.na(I.IV0)&!is.na(IV)&!is.na(I.EE))

fit.l1.ee <- lm(DV.l1~I.IV0 + I.IV0:I.EE + I.EE + IV+I.IV0:IV, data=data.l1.ee)
fit.f2.ee <- lm(DV.f2~I.IV0 + I.IV0:I.EE + I.EE + IV+I.IV0:IV, data=data.f2.ee)
fit.f3.ee <- lm(DV.f3~I.IV0 + I.IV0:I.EE + I.EE + IV+I.IV0:IV, data=data.f3.ee)
fit.f4.ee <- lm(DV.f4~I.IV0 + I.IV0:I.EE + I.EE + IV+I.IV0:IV, data=data.f4.ee)
fit.f5.ee <- lm(DV.f5~I.IV0 + I.IV0:I.EE + I.EE + IV+I.IV0:IV, data=data.f5.ee)

fit.l1.ee$se <- robust.se(fit.l1.ee,as.character(data.l1.ee$country))[[2]][,2]
fit.f2.ee$se <- robust.se(fit.f2.ee,as.character(data.f2.ee$country))[[2]][,2]
fit.f3.ee$se <- robust.se(fit.f3.ee,as.character(data.f3.ee$country))[[2]][,2]
fit.f4.ee$se <- robust.se(fit.f4.ee,as.character(data.f4.ee$country))[[2]][,2]
fit.f5.ee$se <- robust.se(fit.f5.ee,as.character(data.f5.ee$country))[[2]][,2]

fit.list.ee[[1+5*(i-1)]] <- fit.l1.ee
fit.list.ee[[2+5*(i-1)]] <- fit.f2.ee
fit.list.ee[[3+5*(i-1)]] <- fit.f3.ee
fit.list.ee[[4+5*(i-1)]] <- fit.f4.ee
fit.list.ee[[5+5*(i-1)]] <- fit.f5.ee

#checking for ceiling effect
data.0.ee <- subset(fit.data.ee, triwt>0&!is.na(DV)&!is.na(IV)&!is.na(I.EE)&IV>0)
fit.0.ee <- lm(DV~I.EE+IV, data=data.0.ee)
fit.0.ee$se <- robust.se(fit.0.ee,as.character(data.0.ee$country))[[2]][,2]
fit.list.ceiling[[1+2*(i-1)]] <- fit.0.ee


# Table to show countries included in estimate
tee.sc <- subset(data.f2.ee, I.IV0==1&I.EE==1)[,c("country","year")]
Category  <- c("Treated EU Candidate", rep("", (nrow(tee.sc)-1)))
tee <- cbind(Category, tee.sc)

tne.sc <- subset(data.f2.ee, I.IV0==1&I.EE==0)[,c("country","year")]
Category  <- c("Treated Non-candidate", rep("", (nrow(tne.sc)-1)))
tne <- cbind(Category, tne.sc)

cee.sc <- subset(data.f2.ee, I.IV0==0&I.EE==1)[,c("country","year")]
Category  <- c("Control EU Candidate", rep("", (nrow(cee.sc)-1)))
cee <- cbind(Category, cee.sc)

cne.sc <- subset(data.f2.ee, I.IV0==0&I.EE==0)[,c("country","year")]
Category  <- c("Control Non-candidate", rep("", (nrow(cne.sc)-1)))
cne <- cbind(Category, cne.sc)

eeTab <- rbind(tee, cee, tne, cne)
colnames(eeTab) <- c("Category","Country","Year")
rownames(eeTab) <- 1:nrow(eeTab)

sink(paste("rd-out/ee-cases-",dvUp,".tex", sep=""))
print.xtable(xtable(eeTab, caption=paste("EU Candidate Cases, 2 yr. (", DV.labs[i],")",sep="")),
				include.rownames=F,
				hline.after=c(0,
								nrow(tee),
								nrow(tee)+nrow(cee),
								nrow(tee)+nrow(cee)+nrow(tne),
								nrow(tee)+nrow(cee)+nrow(tne)+nrow(cne)))

sink()

##########################################
# US Allies Interaction
##########################################
# As per COW Alliance 4.0 data, the following countries were US allies
# in the period following 1987:

us.allies <- c(	"Antigua and Barbuda",
				"Argentina",
				"Australia",
				"Bahamas, The",
				"Barbados",
				"Belgium",
				"Belize",
				"Bolivia",
				"Brazil",
				"Canada",
				"Chile",
				"Colombia",
				"Costa Rica",
				"Czech Republic",
				"Denmark",
				"Dominica",
				"Dominican Republic",
				"Ecuador",
				"El Salvador",
				"France",
				"Germany",
				"Greece",
				"Grenada",
				"Guatemala",
				"Guyana",
				"Haiti",
				"Honduras",
				"Hungary",
				"Iceland",
				"Italy",
				"Jamaica",
				"Japan",
				"Liberia",
				"Luxembourg",
				"Mexico",
				"Netherlands",
				"Nicaragua",
				"Norway",
				"Pakistan",
				"Panama",
				"Paraguay",
				"Peru",
				"Philippines",
				"Poland",
				"Portugal",
				"Korea, Rep.",
				"Spain",
				"St. Kitts and Nevis",
				"St. Lucia",
				"St. Vincent and the Grenadines",
				"Suriname",
				"Trinidad and Tobago",
				"Turkey",
				"United Kingdom",
				"United States",
				"Uruguay",
				"Venezuela, RB")

fit.data.usa <- data.frame(	country = wb$country_name,
						year = wb$year,
						IV = IV,
						I.IV0 = as.numeric(IV>0),
						DV = DV,
						DV.l1 = DV-lagFun(DV, 2, wb$country_name),  #t-2 to t (placebo)
						DV.f2 = leadFun(DV, 2, wb$country_name)-DV, #t to t+2 (instantaneous)
						DV.f3 = leadFun(DV, 3, wb$country_name)-DV, #t to t+3 (1 year out)
						DV.f4 = leadFun(DV, 4, wb$country_name)-DV, #t to t+4 (2 years out)
						DV.f5 = leadFun(DV, 5, wb$country_name)-DV, #t to t+5 (3 years out)
						triwt = apply(cbind(1-abs(IV/lbw),0),1,max),
						grad = wb$grad,
						I.USA = as.numeric(wb$country_name%in% us.allies) 
						)

data.l1.usa <- subset(fit.data.usa, triwt>0&!is.na(DV.l1)&!is.na(I.IV0)&!is.na(IV)&!is.na(I.USA))
data.f2.usa <- subset(fit.data.usa, triwt>0&!is.na(DV.f2)&!is.na(I.IV0)&!is.na(IV)&!is.na(I.USA))
data.f3.usa <- subset(fit.data.usa, triwt>0&!is.na(DV.f3)&!is.na(I.IV0)&!is.na(IV)&!is.na(I.USA))
data.f4.usa <- subset(fit.data.usa, triwt>0&!is.na(DV.f4)&!is.na(I.IV0)&!is.na(IV)&!is.na(I.USA))
data.f5.usa <- subset(fit.data.usa, triwt>0&!is.na(DV.f5)&!is.na(I.IV0)&!is.na(IV)&!is.na(I.USA))

fit.l1.usa <- lm(DV.l1~I.IV0 + I.IV0:I.USA + I.USA + IV+I.IV0:IV, data=data.l1.usa)
fit.f2.usa <- lm(DV.f2~I.IV0 + I.IV0:I.USA + I.USA + IV+I.IV0:IV, data=data.f2.usa)
fit.f3.usa <- lm(DV.f3~I.IV0 + I.IV0:I.USA + I.USA + IV+I.IV0:IV, data=data.f3.usa)
fit.f4.usa <- lm(DV.f4~I.IV0 + I.IV0:I.USA + I.USA + IV+I.IV0:IV, data=data.f4.usa)
fit.f5.usa <- lm(DV.f5~I.IV0 + I.IV0:I.USA + I.USA + IV+I.IV0:IV, data=data.f5.usa)

fit.l1.usa$se <- robust.se(fit.l1.usa,as.character(data.l1.usa$country))[[2]][,2]
fit.f2.usa$se <- robust.se(fit.f2.usa,as.character(data.f2.usa$country))[[2]][,2]
fit.f3.usa$se <- robust.se(fit.f3.usa,as.character(data.f3.usa$country))[[2]][,2]
fit.f4.usa$se <- robust.se(fit.f4.usa,as.character(data.f4.usa$country))[[2]][,2]
fit.f5.usa$se <- robust.se(fit.f5.usa,as.character(data.f5.usa$country))[[2]][,2]

fit.list.usa[[1+5*(i-1)]] <- fit.l1.usa
fit.list.usa[[2+5*(i-1)]] <- fit.f2.usa
fit.list.usa[[3+5*(i-1)]] <- fit.f3.usa
fit.list.usa[[4+5*(i-1)]] <- fit.f4.usa
fit.list.usa[[5+5*(i-1)]] <- fit.f5.usa

#checking for ceiling effect
data.0.usa <- subset(fit.data.usa, triwt>0&!is.na(DV)&!is.na(IV)&!is.na(I.USA)&IV>0)
fit.0.usa <- lm(DV~I.USA+IV, data=data.0.usa)
fit.0.usa$se <- robust.se(fit.0.usa,as.character(data.0.usa$country))[[2]][,2]
fit.list.ceiling[[2+2*(i-1)]] <- fit.0.usa


# Table to show countries included in estimate
tua.sc <- subset(data.f2.usa, I.IV0==1&I.USA==1)[,c("country","year")]
Category  <- c("Treated US Allies", rep("", (nrow(tua.sc)-1)))
tua <- cbind(Category, tua.sc)

tna.sc <- subset(data.f2.usa, I.IV0==1&I.USA==0)[,c("country","year")]
Category  <- c("Treated Non-allies", rep("", (nrow(tna.sc)-1)))
tna <- cbind(Category, tna.sc)

cua.sc <- subset(data.f2.usa, I.IV0==0&I.USA==1)[,c("country","year")]
Category  <- c("Control US Allies", rep("", (nrow(cua.sc)-1)))
cua <- cbind(Category, cua.sc)

cna.sc <- subset(data.f2.usa, I.IV0==0&I.USA==0)[,c("country","year")]
Category  <- c("Control Non-allies", rep("", (nrow(cna.sc)-1)))
cna <- cbind(Category, cna.sc)

usaTab <- rbind(tua, cua, tna, cna)
colnames(usaTab) <- c("Category","Country","Year")
rownames(usaTab) <- 1:nrow(usaTab)

sink(paste("rd-out/usa-cases-",dvUp,".tex", sep=""))
print.xtable(xtable(usaTab, caption=paste("US Ally Cases, 2 yr. (", DV.labs[i],")",sep="")),
				include.rownames=F,
				hline.after=c(0,
								nrow(tua),
								nrow(tua)+nrow(cua),
								nrow(tua)+nrow(cua)+nrow(tna),
								nrow(tua)+nrow(cua)+nrow(tna)+nrow(cna)))
sink()

##########################################
# Addressing a reviewer comment: 
# Do countries just below threshold tend to remain in the data longer?
# Robustness to controlling for that?
##########################################

lingerShare <- tapply(data.l1$I.IV0, as.character(data.l1$country), mean, na.rm=T)

if(i == 1){
pdf(file="rd-out/lingerShare.pdf", width=6, height=4)
hist(lingerShare, freq=T, xlab="Share of years above threshold", main="", ylim=c(0,15))
dev.off()
}

wb$lingerShare <- 0
for(j in 1:length(lingerShare)){
	wb$lingerShare[wb$country_name==names(lingerShare)[j]] <- lingerShare[j]
}

fit.data.lin <- data.frame(	country = wb$country_name,
						year = wb$year,
						IV = IV,
						I.IV0 = as.numeric(IV>0),
						DV = DV,
						DV.l1 = DV-lagFun(DV, 2, wb$country_name),  #t-2 to t (placebo)
						DV.f2 = leadFun(DV, 2, wb$country_name)-DV, #t to t+2 (instantaneous)
						DV.f3 = leadFun(DV, 3, wb$country_name)-DV, #t to t+3 (1 year out)
						DV.f4 = leadFun(DV, 4, wb$country_name)-DV, #t to t+4 (2 years out)
						DV.f5 = leadFun(DV, 5, wb$country_name)-DV, #t to t+5 (3 years out)
						triwt = apply(cbind(1-abs(IV/lbw),0),1,max),
						grad = wb$grad,
						linger = wb$lingerShare 
						)

data.l1.lin <- subset(fit.data.lin, triwt>0&!is.na(DV.l1)&!is.na(I.IV0)&!is.na(IV))
data.f2.lin <- subset(fit.data.lin, triwt>0&!is.na(DV.f2)&!is.na(I.IV0)&!is.na(IV))
data.f3.lin <- subset(fit.data.lin, triwt>0&!is.na(DV.f3)&!is.na(I.IV0)&!is.na(IV))
data.f4.lin <- subset(fit.data.lin, triwt>0&!is.na(DV.f4)&!is.na(I.IV0)&!is.na(IV))
data.f5.lin <- subset(fit.data.lin, triwt>0&!is.na(DV.f5)&!is.na(I.IV0)&!is.na(IV))

fit.l1.lin <- lm(DV.l1~I.IV0*IV+linger, data=data.l1.lin)
fit.f2.lin <- lm(DV.f2~I.IV0*IV+linger, data=data.f2.lin)
fit.f3.lin <- lm(DV.f3~I.IV0*IV+linger, data=data.f3.lin)
fit.f4.lin <- lm(DV.f4~I.IV0*IV+linger, data=data.f4.lin)
fit.f5.lin <- lm(DV.f5~I.IV0*IV+linger, data=data.f5.lin)

fit.l1.lin$se <- robust.se(fit.l1.lin,as.character(data.l1.lin$country))[[2]][,2]
fit.f2.lin$se <- robust.se(fit.f2.lin,as.character(data.f2.lin$country))[[2]][,2]
fit.f3.lin$se <- robust.se(fit.f3.lin,as.character(data.f3.lin$country))[[2]][,2]
fit.f4.lin$se <- robust.se(fit.f4.lin,as.character(data.f4.lin$country))[[2]][,2]
fit.f5.lin$se <- robust.se(fit.f5.lin,as.character(data.f5.lin$country))[[2]][,2]

fit.list.linger[[1+5*(i-1)]] <- fit.l1.lin
fit.list.linger[[2+5*(i-1)]] <- fit.f2.lin
fit.list.linger[[3+5*(i-1)]] <- fit.f3.lin
fit.list.linger[[4+5*(i-1)]] <- fit.f4.lin
fit.list.linger[[5+5*(i-1)]] <- fit.f5.lin



##########################################
# Scaling estimates relative to change over time, in GNI/Cap 
##########################################

fit.data.scale <- data.frame(	country = wb$country_name,
						year = wb$year,
						GNIPC1K = wb$gnipc_hist/1000,
						GNIPC1K.f10 = leadFun(wb$gnipc_hist/1000, 10, wb$country_name)-wb$gnipc_hist/1000,
						IV = IV,
						DV = DV,
						DV.f05 = leadFun(DV, 5, wb$country_name)-DV,
						DV.f10 = leadFun(DV, 10, wb$country_name)-DV,
						DV.f15 = leadFun(DV, 15, wb$country_name)-DV,					
						DV.f20 = leadFun(DV, 20, wb$country_name)-DV,						
						triwt = apply(cbind(1-abs(IV/lbw),0),1,max)
						)

nearThresh.sc <-  tapply(fit.data.scale$IV, 
						fit.data.scale$country, 
						function(x) as.numeric(max(as.numeric(abs(x)<=.1), na.rm=T)==1))
threshCountries.sc <- row.names(nearThresh.sc)[nearThresh.sc==1]
fit.data.scale$bwcountry <- 0
fit.data.scale$bwcountry[fit.data.scale$country %in% threshCountries.sc] <- 1

sink(paste("rd-out/scale-mean-ch-", DV.main[i],".txt", sep=""))
print("5 yr")
print(mean(fit.data.scale$DV.f05, na.rm=T))
print("10 yr")
print(mean(fit.data.scale$DV.f10, na.rm=T))
print("15 yr")
print(mean(fit.data.scale$DV.f15, na.rm=T))
print("20 yr")
print(mean(fit.data.scale$DV.f20, na.rm=T))
print("5 yr-bw")
print(mean(fit.data.scale$DV.f05[fit.data.scale$bwcountry==1], na.rm=T))
print("10 yr-bw")
print(mean(fit.data.scale$DV.f10[fit.data.scale$bwcountry==1], na.rm=T))
print("15 yr-bw")
print(mean(fit.data.scale$DV.f15[fit.data.scale$bwcountry==1], na.rm=T))
print("20 yr-bw")
print(mean(fit.data.scale$DV.f20[fit.data.scale$bwcountry==1], na.rm=T))
sink()

data.scale <- subset(fit.data.scale, !is.na(DV.f10)&!is.na(GNIPC1K.f10))
data.scale2 <- subset(fit.data.scale, !is.na(DV)&!is.na(year))

fit.scale <- lm(DV.f10~GNIPC1K.f10, data = data.scale)
fit.scale2 <- lm(DV~year, data = data.scale2)

sink(paste("rd-out/scale-gnipc1k-", DV.main[i],".txt", sep=""))
print(coef(fit.scale))
sink()

sink(paste("rd-out/scale-year-", DV.main[i],".txt", sep=""))
print(coef(fit.scale2))
sink()

pdf(file=paste("rd-out/scale-plot-gnipc1k-", DV.main[i],".pdf", sep=""), height=6, width=6)
plot(data.scale$GNIPC1K.f10, data.scale$DV.f10, xlab="Change in GNIPC (1K)", ylab="Change in Outcome (SD)")
abline(fit.scale)
dev.off()

pdf(file=paste("rd-out/scale-plot-year-", DV.main[i],".pdf", sep=""), height=6, width=6)
plot(data.scale2$year, jitter(data.scale2$DV), xlab="Year", ylab="Outcome (SD units)")
abline(fit.scale2)
dev.off()

}


##########################################
##########################################
# End of regression estimation for main results
# and robustness checks.
##########################################
##########################################




##########################################
# Tables for main results on political liberalization
##########################################

sink("rd-out/reg-tab-main-results.tex")
print(apsrtable(	fit.list[[1]],
					fit.list[[2]],
					fit.list[[3]],
					fit.list[[4]],
					fit.list[[5]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Interaction term"),
			stars="default",
			caption=paste("Effects on political liberalization (Freedom House)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.10 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

print(apsrtable(	fit.list[[6]],
					fit.list[[7]],
					fit.list[[8]],
					fit.list[[9]],
					fit.list[[10]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Interaction term"),
			stars="default",
			caption=paste("Effects on political liberalization (Polity)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.10 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

print(apsrtable(	fit.list[[11]],
					fit.list[[12]],
					fit.list[[13]],
					fit.list[[14]],
					fit.list[[15]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Interaction term"),
			stars="default",
			caption=paste("Effects on political liberalization (Aggregate Polity and Freedom House)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.10 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

print(apsrtable(	fit.list[[16]],
					fit.list[[17]],
					fit.list[[18]],
					fit.list[[19]],
					fit.list[[20]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Interaction term"),
			stars="default",
			caption=paste("Effects on political liberalization (Unified Democracy Score)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.10 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

sink()

##########################################
# Graph showing robustness of main results
##########################################

coefPlot <- function(fit.up, xpos.up, ypos.up, pch.up){
	segments(xpos.up+coef(fit.up)["I.IV0"]+1.96*fit.up$se["I.IV0"],
	ypos.up,
	xpos.up+coef(fit.up)["I.IV0"]-1.96*fit.up$se["I.IV0"],
	ypos.up, col="gray")
	segments(xpos.up+coef(fit.up)["I.IV0"]+1.64*fit.up$se["I.IV0"],
	ypos.up,
	xpos.up+coef(fit.up)["I.IV0"]-1.64*fit.up$se["I.IV0"],
	ypos.up, col="gray", lwd=2)	
	points(xpos.up+coef(fit.up)["I.IV0"], ypos.up, pch=pch.up)
}

xinst <- 1.5 +.5
xf1 <- 3 +.5
xf2 <- 4.5+.5
xf3 <- 6 +.5

pdf(file="rd-out/robust-plot.pdf", width=12, height=5)

par(mar=c(3,1,3,0))
plot(c(0,xf3+1), 
		c(0,8.5), type="n", axes=F,
		xlab="",ylab="")
		
legend(xf1+((xf2-xf1)/2), 8.25, pch=c(19,17,15,18),
		legend=c("FH","Polity", "FH-Polity","UDS"), cex=.75,
		bty="n",horiz=F)
abline(v=xinst, lty="dashed")
abline(v=xf1, lty="dashed")
abline(v=xf2, lty="dashed")
abline(v=xf3, lty="dashed")

text(0,8, "Main specification", pos=4, cex=.75)
coefPlot(fit.list[[2]], xinst, 		8.3, 19)
coefPlot(fit.list[[3]], xf1, 		8.3, 19)
coefPlot(fit.list[[4]], xf2, 		8.3, 19)
coefPlot(fit.list[[5]], xf3, 		8.3, 19)

coefPlot(fit.list[[7]], xinst, 		8.2, 17)
coefPlot(fit.list[[8]], xf1, 		8.2, 17)
coefPlot(fit.list[[9]], xf2, 		8.2, 17)
coefPlot(fit.list[[10]], xf3, 		8.2, 17)

coefPlot(fit.list[[12]], xinst, 	8.1, 15)
coefPlot(fit.list[[13]], xf1, 		8.1, 15)
coefPlot(fit.list[[14]], xf2, 		8.1, 15)
coefPlot(fit.list[[15]], xf3, 		8.1, 15)

coefPlot(fit.list[[17]], xinst, 	8, 18)
coefPlot(fit.list[[18]], xf1, 		8, 18)
coefPlot(fit.list[[19]], xf2, 		8, 18)
coefPlot(fit.list[[20]], xf3, 		8, 18)


text(0,7, "Triangular kernel", pos=4, cex=.75)
coefPlot(fit.list.t[[2]], xinst, 		7.3, 19)
coefPlot(fit.list.t[[3]], xf1, 		7.3, 19)
coefPlot(fit.list.t[[4]], xf2, 		7.3, 19)
coefPlot(fit.list.t[[5]], xf3, 		7.3, 19)

coefPlot(fit.list.t[[7]], xinst, 		7.2, 17)
coefPlot(fit.list.t[[8]], xf1, 		7.2, 17)
coefPlot(fit.list.t[[9]], xf2, 		7.2, 17)
coefPlot(fit.list.t[[10]], xf3, 		7.2, 17)

coefPlot(fit.list.t[[12]], xinst, 	7.1, 15)
coefPlot(fit.list.t[[13]], xf1, 		7.1, 15)
coefPlot(fit.list.t[[14]], xf2, 		7.1, 15)
coefPlot(fit.list.t[[15]], xf3, 		7.1, 15)

coefPlot(fit.list.t[[17]], xinst, 	7, 18)
coefPlot(fit.list.t[[18]], xf1, 		7, 18)
coefPlot(fit.list.t[[19]], xf2, 		7, 18)
coefPlot(fit.list.t[[20]], xf3, 		7, 18)


text(0,6, "Bandwidth=.15", pos=4, cex=.75)
coefPlot(fit.list.b5[[2]], xinst, 		6.3, 19)
coefPlot(fit.list.b5[[3]], xf1, 		6.3, 19)
coefPlot(fit.list.b5[[4]], xf2, 		6.3, 19)
coefPlot(fit.list.b5[[5]], xf3, 		6.3, 19)

coefPlot(fit.list.b5[[7]], xinst, 		6.2, 17)
coefPlot(fit.list.b5[[8]], xf1, 		6.2, 17)
coefPlot(fit.list.b5[[9]], xf2, 		6.2, 17)
coefPlot(fit.list.b5[[10]], xf3, 		6.2, 17)

coefPlot(fit.list.b5[[12]], xinst, 	6.1, 15)
coefPlot(fit.list.b5[[13]], xf1, 		6.1, 15)
coefPlot(fit.list.b5[[14]], xf2, 		6.1, 15)
coefPlot(fit.list.b5[[15]], xf3, 		6.1, 15)

coefPlot(fit.list.b5[[17]], xinst, 	6, 18)
coefPlot(fit.list.b5[[18]], xf1, 		6, 18)
coefPlot(fit.list.b5[[19]], xf2, 		6, 18)
coefPlot(fit.list.b5[[20]], xf3, 		6, 18)


text(0,5, "Bandwidth=.125", pos=4, cex=.75)
coefPlot(fit.list.b4[[2]], xinst, 		5.3, 19)
coefPlot(fit.list.b4[[3]], xf1, 		5.3, 19)
coefPlot(fit.list.b4[[4]], xf2, 		5.3, 19)
coefPlot(fit.list.b4[[5]], xf3, 		5.3, 19)

coefPlot(fit.list.b4[[7]], xinst, 		5.2, 17)
coefPlot(fit.list.b4[[8]], xf1, 		5.2, 17)
coefPlot(fit.list.b4[[9]], xf2, 		5.2, 17)
coefPlot(fit.list.b4[[10]], xf3, 		5.2, 17)

coefPlot(fit.list.b4[[12]], xinst, 	5.1, 15)
coefPlot(fit.list.b4[[13]], xf1, 		5.1, 15)
coefPlot(fit.list.b4[[14]], xf2, 		5.1, 15)
coefPlot(fit.list.b4[[15]], xf3, 		5.1, 15)

coefPlot(fit.list.b4[[17]], xinst, 	5, 18)
coefPlot(fit.list.b4[[18]], xf1, 		5, 18)
coefPlot(fit.list.b4[[19]], xf2, 		5, 18)
coefPlot(fit.list.b4[[20]], xf3, 		5, 18)


text(0,4, "Bandwidth=.075", pos=4, cex=.75)
coefPlot(fit.list.b3[[2]], xinst, 		4.3, 19)
coefPlot(fit.list.b3[[3]], xf1, 		4.3, 19)
coefPlot(fit.list.b3[[4]], xf2, 		4.3, 19)
coefPlot(fit.list.b3[[5]], xf3, 		4.3, 19)

coefPlot(fit.list.b3[[7]], xinst, 		4.2, 17)
coefPlot(fit.list.b3[[8]], xf1, 		4.2, 17)
coefPlot(fit.list.b3[[9]], xf2, 		4.2, 17)
coefPlot(fit.list.b3[[10]], xf3, 		4.2, 17)

coefPlot(fit.list.b3[[12]], xinst, 	4.1, 15)
coefPlot(fit.list.b3[[13]], xf1, 		4.1, 15)
coefPlot(fit.list.b3[[14]], xf2, 		4.1, 15)
coefPlot(fit.list.b3[[15]], xf3, 		4.1, 15)

coefPlot(fit.list.b3[[17]], xinst, 	4, 18)
coefPlot(fit.list.b3[[18]], xf1, 		4, 18)
coefPlot(fit.list.b3[[19]], xf2, 		4, 18)
coefPlot(fit.list.b3[[20]], xf3, 		4, 18)


text(0,3, "Control for lagged Y", pos=4, cex=.75)
coefPlot(fit.list.dv[[1]], xinst, 	3.3, 19)
coefPlot(fit.list.dv[[2]], xf1, 		3.3, 19)
coefPlot(fit.list.dv[[3]], xf2, 		3.3, 19)
coefPlot(fit.list.dv[[4]], xf3, 		3.3, 19)

coefPlot(fit.list.dv[[9]], xinst, 	3.2, 17)
coefPlot(fit.list.dv[[10]], xf1, 		3.2, 17)
coefPlot(fit.list.dv[[11]], xf2, 		3.2, 17)
coefPlot(fit.list.dv[[12]], xf3, 	3.2, 17)

coefPlot(fit.list.dv[[17]], xinst, 	3.1, 15)
coefPlot(fit.list.dv[[18]], xf1, 		3.1, 15)
coefPlot(fit.list.dv[[19]], xf2, 		3.1, 15)
coefPlot(fit.list.dv[[20]], xf3, 	3.1, 15)

coefPlot(fit.list.dv[[25]], xinst, 	3, 18)
coefPlot(fit.list.dv[[26]], xf1, 		3, 18)
coefPlot(fit.list.dv[[27]], xf2, 		3, 18)
coefPlot(fit.list.dv[[28]], xf3, 	3, 18)


text(0,2, "Control for lagged Y & Ysq.", pos=4, cex=.75)
coefPlot(fit.list.dv[[5]], xinst, 	2.3, 19)
coefPlot(fit.list.dv[[6]], xf1, 		2.3, 19)
coefPlot(fit.list.dv[[7]], xf2, 		2.3, 19)
coefPlot(fit.list.dv[[8]], xf3, 		2.3, 19)

coefPlot(fit.list.dv[[13]], xinst, 	2.2, 17)
coefPlot(fit.list.dv[[14]], xf1, 		2.2, 17)
coefPlot(fit.list.dv[[15]], xf2, 		2.2, 17)
coefPlot(fit.list.dv[[16]], xf3, 	2.2, 17)

coefPlot(fit.list.dv[[21]], xinst, 	2.1, 15)
coefPlot(fit.list.dv[[22]], xf1, 		2.1, 15)
coefPlot(fit.list.dv[[23]], xf2, 		2.1, 15)
coefPlot(fit.list.dv[[24]], xf3, 	2.1, 15)

coefPlot(fit.list.dv[[29]], xinst, 	2, 18)
coefPlot(fit.list.dv[[30]], xf1, 		2, 18)
coefPlot(fit.list.dv[[31]], xf2, 		2, 18)
coefPlot(fit.list.dv[[32]], xf3, 	2, 18)



text(0,1, "Level outcomes", pos=4, cex=.75)
coefPlot(fit.list.l[[2]], xinst, 	1.3, 19)
coefPlot(fit.list.l[[3]], xf1, 		1.3, 19)
coefPlot(fit.list.l[[4]], xf2, 		1.3, 19)
coefPlot(fit.list.l[[5]], xf3, 		1.3, 19)

coefPlot(fit.list.l[[7]], xinst, 	1.2, 17)
coefPlot(fit.list.l[[8]], xf1, 		1.2, 17)
coefPlot(fit.list.l[[9]], xf2, 		1.2, 17)
coefPlot(fit.list.l[[10]], xf3, 	1.2, 17)

coefPlot(fit.list.l[[12]], xinst, 	1.1, 15)
coefPlot(fit.list.l[[13]], xf1, 		1.1, 15)
coefPlot(fit.list.l[[14]], xf2, 		1.1, 15)
coefPlot(fit.list.l[[15]], xf3, 	1.1, 15)

coefPlot(fit.list.l[[17]], xinst, 	1, 18)
coefPlot(fit.list.l[[18]], xf1, 		1, 18)
coefPlot(fit.list.l[[19]], xf2, 		1, 18)
coefPlot(fit.list.l[[20]], xf3, 	1, 18)



text(0,0, "Level outcomes, control for lagged Y", pos=4, cex=.75)
coefPlot(fit.list.l.dv[[1]], xinst, 	0.3, 19)
coefPlot(fit.list.l.dv[[2]], xf1, 		0.3, 19)
coefPlot(fit.list.l.dv[[3]], xf2, 		0.3, 19)
coefPlot(fit.list.l.dv[[4]], xf3, 		0.3, 19)

coefPlot(fit.list.l.dv[[5]], xinst, 	0.2, 17)
coefPlot(fit.list.l.dv[[6]], xf1, 		0.2, 17)
coefPlot(fit.list.l.dv[[7]], xf2, 		0.2, 17)
coefPlot(fit.list.l.dv[[8]], xf3, 	0.2, 17)

coefPlot(fit.list.l.dv[[9]], xinst, 	0.1, 15)
coefPlot(fit.list.l.dv[[10]], xf1, 		0.1, 15)
coefPlot(fit.list.l.dv[[11]], xf2, 		0.1, 15)
coefPlot(fit.list.l.dv[[12]], xf3, 	0.1, 15)

coefPlot(fit.list.l.dv[[13]], xinst, 	0, 18)
coefPlot(fit.list.l.dv[[14]], xf1, 		0, 18)
coefPlot(fit.list.l.dv[[15]], xf2, 		0, 18)
coefPlot(fit.list.l.dv[[16]], xf3, 	0, 18)





axis(1, c(xinst-.6,xinst-.4,xinst-.2, xinst, xinst+.2,xinst+.4,xinst+.6), 
			c("-.6","-.4","-.2","0",".2",".4",".6"), cex.axis=.75)
axis(1, c(xf1-.6,xf1-.4,xf1-.2, xf1, xf1+.2,xf1+.4,xf1+.6), 
			c("-.6","-.4","-.2","0",".2",".4",".6"), cex.axis=.75)
axis(1, c(xf2-.6,xf2-.4,xf2-.2, xf2, xf2+.2,xf2+.4,xf2+.6), 
			c("-.6","-.4","-.2","0",".2",".4",".6"), cex.axis=.75)			
axis(1, c(xf3-.6,xf3-.4,xf3-.2, xf3, xf3+.2,xf3+.4,xf3+.6), 
			c("-.6","-.4","-.2","0",".2",".4",".6"), cex.axis=.75)

axis(3, c(xinst, 
			xf1,
			xf2,
			xf3), 
			c("Instantaneous",
			"One year forward", 
			"Two years forward",
			"Three years forward"), 
			cex.axis=.75,  lwd=0)
			
dev.off()

##########################################
# Robustness checks tables for appendix
##########################################


sink("rd-out/reg-tab-tri.tex")
print(apsrtable(	fit.list.t[[1]],
					fit.list.t[[2]],
					fit.list.t[[3]],
					fit.list.t[[4]],
					fit.list.t[[5]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Interaction term"),
			stars="default",
			caption=paste("Effects on political liberalization (Freedom House, tri. kernel)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.10 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

print(apsrtable(	fit.list.t[[6]],
					fit.list.t[[7]],
					fit.list.t[[8]],
					fit.list.t[[9]],
					fit.list.t[[10]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Interaction term"),
			stars="default",
			caption=paste("Effects on political liberalization (Polity, tri. kernel)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.10 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

print(apsrtable(	fit.list.t[[11]],
					fit.list.t[[12]],
					fit.list.t[[13]],
					fit.list.t[[14]],
					fit.list.t[[15]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Interaction term"),
			stars="default",
			caption=paste("Effects on political liberalization (Aggregate Freedom House-Polity, tri. kernel)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.10 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

print(apsrtable(	fit.list.t[[16]],
					fit.list.t[[17]],
					fit.list.t[[18]],
					fit.list.t[[19]],
					fit.list.t[[20]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Interaction term"),
			stars="default",
			caption=paste("Effects on political liberalization (Unified Democracy Scores, tri. kernel)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.10 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

sink()



sink("rd-out/reg-tab-bw3.tex")
print(apsrtable(	fit.list.b3[[1]],
					fit.list.b3[[2]],
					fit.list.b3[[3]],
					fit.list.b3[[4]],
					fit.list.b3[[5]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Interaction term"),
			stars="default",
			caption=paste("Effects on political liberalization (Freedom House, alternative bandwidth)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.075 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

print(apsrtable(	fit.list.b3[[6]],
					fit.list.b3[[7]],
					fit.list.b3[[8]],
					fit.list.b3[[9]],
					fit.list.b3[[10]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Interaction term"),
			stars="default",
			caption=paste("Effects on political liberalization (Polity, alternative bandwidth)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.075 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

print(apsrtable(	fit.list.b3[[11]],
					fit.list.b3[[12]],
					fit.list.b3[[13]],
					fit.list.b3[[14]],
					fit.list.b3[[15]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Interaction term"),
			stars="default",
			caption=paste("Effects on political liberalization (Aggregate Freedom House and Polity, alternative bandwidth)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.075 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

print(apsrtable(	fit.list.b3[[16]],
					fit.list.b3[[17]],
					fit.list.b3[[18]],
					fit.list.b3[[19]],
					fit.list.b3[[20]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Interaction term"),
			stars="default",
			caption=paste("Effects on political liberalization (Unified Democracy Score, alternative bandwidth)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.075 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))




sink()





sink("rd-out/reg-tab-bw4.tex")
print(apsrtable(	fit.list.b4[[1]],
					fit.list.b4[[2]],
					fit.list.b4[[3]],
					fit.list.b4[[4]],
					fit.list.b4[[5]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Interaction term"),
			stars="default",
			caption=paste("Effects on political liberalization (Freedom House, alternative bandwidth)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.125 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

print(apsrtable(	fit.list.b4[[6]],
					fit.list.b4[[7]],
					fit.list.b4[[8]],
					fit.list.b4[[9]],
					fit.list.b4[[10]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Interaction term"),
			stars="default",
			caption=paste("Effects on political liberalization (Polity, alternative bandwidth)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.125 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

print(apsrtable(	fit.list.b4[[11]],
					fit.list.b4[[12]],
					fit.list.b4[[13]],
					fit.list.b4[[14]],
					fit.list.b4[[15]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Interaction term"),
			stars="default",
			caption=paste("Effects on political liberalization (Aggregate Freedom House and Polity, alternative bandwidth)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.125 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

print(apsrtable(	fit.list.b4[[16]],
					fit.list.b4[[17]],
					fit.list.b4[[18]],
					fit.list.b4[[19]],
					fit.list.b4[[20]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Interaction term"),
			stars="default",
			caption=paste("Effects on political liberalization (Unified Democracy Score, alternative bandwidth)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.125 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))
sink()

sink("rd-out/reg-tab-bw5.tex")
print(apsrtable(	fit.list.b5[[1]],
					fit.list.b5[[2]],
					fit.list.b5[[3]],
					fit.list.b5[[4]],
					fit.list.b5[[5]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Interaction term"),
			stars="default",
			caption=paste("Effects on political liberalization (Freedom House, alternative bandwidth)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.15 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

print(apsrtable(	fit.list.b5[[6]],
					fit.list.b5[[7]],
					fit.list.b5[[8]],
					fit.list.b5[[9]],
					fit.list.b5[[10]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Interaction term"),
			stars="default",
			caption=paste("Effects on political liberalization (Polity, alternative bandwidth)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.15 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

print(apsrtable(	fit.list.b5[[11]],
					fit.list.b5[[12]],
					fit.list.b5[[13]],
					fit.list.b5[[14]],
					fit.list.b5[[15]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Interaction term"),
			stars="default",
			caption=paste("Effects on political liberalization (Aggregate Freedom House and Polity, alternative bandwidth)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.15 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

print(apsrtable(	fit.list.b5[[16]],
					fit.list.b5[[17]],
					fit.list.b5[[18]],
					fit.list.b5[[19]],
					fit.list.b5[[20]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Interaction term"),
			stars="default",
			caption=paste("Effects on political liberalization (Unified Democracy Score, alternative bandwidth)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.15 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))


sink()

sink("rd-out/reg-tab-dv.tex")
print(apsrtable(	fit.list.placebo.add[[2]],
					fit.list.dv[[5]],
					fit.list.dv[[6]],
					fit.list.dv[[7]],
					fit.list.dv[[8]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Lagged Y",
							"Lagged Y sq.",
							"Interaction term"),
			stars="default",
			caption=paste("Effects on political liberalization (Freedom House, lagged DV)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.10 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

print(apsrtable(	fit.list.placebo.add[[4]],
					fit.list.dv[[13]],
					fit.list.dv[[14]],
					fit.list.dv[[15]],
					fit.list.dv[[16]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Lagged Y",
							"Lagged Y sq.",
							"Interaction term"),
			stars="default",
			caption=paste("Effects on political liberalization (Polity, lagged DV)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.10 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

print(apsrtable(	fit.list.placebo.add[[6]],
					fit.list.dv[[21]],
					fit.list.dv[[22]],
					fit.list.dv[[23]],
					fit.list.dv[[24]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Lagged Y",
							"Lagged Y sq.",
							"Interaction term"),
			stars="default",
			caption=paste("Effects on political liberalization (Aggregate Freedom House and Polity, lagged DV)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.10 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

print(apsrtable(	fit.list.placebo.add[[8]],
					fit.list.dv[[29]],
					fit.list.dv[[30]],
					fit.list.dv[[31]],
					fit.list.dv[[32]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Lagged Y",
							"Lagged Y sq.",
							"Interaction term"),
			stars="default",
			caption=paste("Effects on political liberalization (Unified Democracy Score, lagged DV)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.10 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

sink()

sink("rd-out/reg-tab-lev.tex")
print(apsrtable(	fit.list.l[[1]],
					fit.list.l[[2]],
					fit.list.l[[3]],
					fit.list.l[[4]],
					fit.list.l[[5]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Interaction term"),
			stars="default",
			caption=paste("Effects on political liberalization (Freedom House, level outcomes)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.10 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

print(apsrtable(	fit.list.l[[6]],
					fit.list.l[[7]],
					fit.list.l[[8]],
					fit.list.l[[9]],
					fit.list.l[[10]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Interaction term"),
			stars="default",
			caption=paste("Effects on political liberalization (Polity, level outcomes)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.10 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

print(apsrtable(	fit.list.l[[11]],
					fit.list.l[[12]],
					fit.list.l[[13]],
					fit.list.l[[14]],
					fit.list.l[[15]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Interaction term"),
			stars="default",
			caption=paste("Effects on political liberalization (Aggregate Freedom House and Polity, level outcomes)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.10 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

print(apsrtable(	fit.list.l[[16]],
					fit.list.l[[17]],
					fit.list.l[[18]],
					fit.list.l[[19]],
					fit.list.l[[20]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Interaction term"),
			stars="default",
			caption=paste("Effects on political liberalization (Unified Democracy Score, level outcomes)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.10 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

sink()


sink("rd-out/reg-tab-lev-dv.tex")
print(apsrtable(	fit.list.placebo.add[[1]],
					fit.list.l.dv[[1]],
					fit.list.l.dv[[2]],
					fit.list.l.dv[[3]],
					fit.list.l.dv[[4]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Lagged Y",
							"Interaction term"),
			stars="default",
			caption=paste("Effects on political liberalization (Freedom House, level outcomes and lagged DV)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.10 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

print(apsrtable(	fit.list.placebo.add[[3]],
					fit.list.l.dv[[5]],
					fit.list.l.dv[[6]],
					fit.list.l.dv[[7]],
					fit.list.l.dv[[8]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Lagged Y",
							"Interaction term"),
			stars="default",
			caption=paste("Effects on political liberalization (Polity, level outcomes and lagged DV)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.10 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

print(apsrtable(	fit.list.placebo.add[[5]],
					fit.list.l.dv[[9]],
					fit.list.l.dv[[10]],
					fit.list.l.dv[[11]],
					fit.list.l.dv[[12]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Lagged Y",
							"Interaction term"),
			stars="default",
			caption=paste("Effects on political liberalization (Aggregate Freedom House and Polity, level outcomes and lagged DV)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.10 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

print(apsrtable(	fit.list.placebo.add[[7]],
					fit.list.l.dv[[13]],
					fit.list.l.dv[[14]],
					fit.list.l.dv[[15]],
					fit.list.l.dv[[16]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Lagged Y",
							"Interaction term"),
			stars="default",
			caption=paste("Effects on political liberalization (Unified Democracy Score, level outcomes and lagged DV)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.10 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

sink()


##########################################
# EU Candidate interaction
##########################################

sink("rd-out/reg-tab-ee.tex")
print(apsrtable(	fit.list.ee[[1]],
					fit.list.ee[[2]],
					fit.list.ee[[3]],
					fit.list.ee[[4]],
					fit.list.ee[[5]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
 							"EU candidate",
							"Log GNI/cap. - c",
							"IBRD grad. elig. X EU candidate",
							"IBRD grad. elig. X (Log GNI/cap. - c)"),
			stars="default",
			caption=paste("Effects on political liberalization (Freedom House, EU candidate interaction)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.10 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

print(apsrtable(	fit.list.ee[[6]],
					fit.list.ee[[7]],
					fit.list.ee[[8]],
					fit.list.ee[[9]],
					fit.list.ee[[10]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
 							"EU candidate",
							"Log GNI/cap. - c",
							"IBRD grad. elig. X EU candidate",
							"IBRD grad. elig. X (Log GNI/cap. - c)"),
			stars="default",
			caption=paste("Effects on political liberalization (Polity, EU candidate interaction)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.10 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

print(apsrtable(	fit.list.ee[[11]],
					fit.list.ee[[12]],
					fit.list.ee[[13]],
					fit.list.ee[[14]],
					fit.list.ee[[15]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
 							"EU candidate",
							"Log GNI/cap. - c",
							"IBRD grad. elig. X EU candidate",
							"IBRD grad. elig. X (Log GNI/cap. - c)"),
			stars="default",
			caption=paste("Effects on political liberalization (Aggregate Freedom House and Polity, EU candidate interaction)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.10 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

print(apsrtable(	fit.list.ee[[16]],
					fit.list.ee[[17]],
					fit.list.ee[[18]],
					fit.list.ee[[19]],
					fit.list.ee[[20]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
 							"EU candidate",
							"Log GNI/cap. - c",
							"IBRD grad. elig. X EU candidate",
							"IBRD grad. elig. X (Log GNI/cap. - c)"),
			stars="default",
			caption=paste("Effects on political liberalization (Unified Democracy Score, EU candidate interaction)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.15 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))


sink()



##########################################
# US Allies interaction
##########################################

sink("rd-out/reg-tab-usa.tex")
print(apsrtable(	fit.list.usa[[1]],
					fit.list.usa[[2]],
					fit.list.usa[[3]],
					fit.list.usa[[4]],
					fit.list.usa[[5]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
 							"US Ally",
							"Log GNI/cap. - c",
							"IBRD grad. elig. X US Ally",
							"IBRD grad. elig. X (Log GNI/cap. - c)"),
			stars="default",
			caption=paste("Effects on political liberalization (Freedom House, US ally interaction)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.10 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

print(apsrtable(	fit.list.usa[[6]],
					fit.list.usa[[7]],
					fit.list.usa[[8]],
					fit.list.usa[[9]],
					fit.list.usa[[10]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
 							"US Ally",
							"Log GNI/cap. - c",
							"IBRD grad. elig. X US Ally",
							"IBRD grad. elig. X (Log GNI/cap. - c)"),
			stars="default",
			caption=paste("Effects on political liberalization (Polity, US ally interaction)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.10 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

print(apsrtable(	fit.list.usa[[11]],
					fit.list.usa[[12]],
					fit.list.usa[[13]],
					fit.list.usa[[14]],
					fit.list.usa[[15]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
 							"US Ally",
							"Log GNI/cap. - c",
							"IBRD grad. elig. X US Ally",
							"IBRD grad. elig. X (Log GNI/cap. - c)"),
			stars="default",
			caption=paste("Effects on political liberalization (Aggregate Freedom House and Polity, US ally interaction)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.10 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

print(apsrtable(	fit.list.usa[[16]],
					fit.list.usa[[17]],
					fit.list.usa[[18]],
					fit.list.usa[[19]],
					fit.list.usa[[20]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
 							"US Ally",
							"Log GNI/cap. - c",
							"IBRD grad. elig. X US Ally",
							"IBRD grad. elig. X (Log GNI/cap. - c)"),
			stars="default",
			caption=paste("Effects on political liberalization (Unified Democracy Score, US ally interaction)"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.15 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))


sink()



##########################################
# Ceiling Effects of EU Candidates and US Allies interactions
##########################################



sink("rd-out/reg-tab-ceiling.tex")
print(apsrtable(	fit.list.ceiling[[1]],
					fit.list.ceiling[[3]],
					fit.list.ceiling[[5]],
					fit.list.ceiling[[7]],
			digits=2,
			coef.names=c(	"(Constant)",
 							"EU candidate",
							"Log GNI/cap. - c"),
			stars="default",
			caption=paste("Checking ceiling effects for political liberalization (EU candidates vs non-candidates, control obs. only)"),
			model.names = c("FH","Polity","FH-Polity","UDS"),
			notes=list("Ordinary least squares estimates within 0.10 bandwidth to the left of cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

print(apsrtable(	fit.list.ceiling[[2]],
					fit.list.ceiling[[4]],
					fit.list.ceiling[[6]],
					fit.list.ceiling[[8]],
			digits=2,
			coef.names=c(	"(Constant)",
							"US ally",
 							"Log GNI/cap. - c"),
			stars="default",
			caption=paste("Checking ceiling effects for political liberalization (US allies vs non-allies, control obs. only)"),
			model.names = c("FH","Polity","FH-Polity","UDS"),
			notes=list("Ordinary least squares estimates within 0.10 bandwidth to the left of cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

sink()


##########################################
# Controlling for "lingering" effects
##########################################

sink("rd-out/reg-tab-linger.tex")
print(apsrtable(	fit.list.linger[[1]],
					fit.list.linger[[2]],
					fit.list.linger[[3]],
					fit.list.linger[[4]],
					fit.list.linger[[5]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Share yrs. above cut",
							"IBRD grad. elig. X (Log GNI/cap. - c)"),
			stars="default",
			caption=paste("Effects on political liberalization (Freedom House, controlling for ``lingering effects'')"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.10 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

print(apsrtable(	fit.list.linger[[6]],
					fit.list.linger[[7]],
					fit.list.linger[[8]],
					fit.list.linger[[9]],
					fit.list.linger[[10]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Share yrs. above cut",
							"IBRD grad. elig. X (Log GNI/cap. - c)"),
			stars="default",
			caption=paste("Effects on political liberalization (Polity, controlling for ``lingering effects'')"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.10 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

print(apsrtable(	fit.list.linger[[11]],
					fit.list.linger[[12]],
					fit.list.linger[[13]],
					fit.list.linger[[14]],
					fit.list.linger[[15]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Share yrs. above cut",
							"IBRD grad. elig. X (Log GNI/cap. - c)"),
			stars="default",
			caption=paste("Effects on political liberalization (Aggregate Freedom House and Polity, controlling for ``lingering effects'')"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.10 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))

print(apsrtable(	fit.list.linger[[16]],
					fit.list.linger[[17]],
					fit.list.linger[[18]],
					fit.list.linger[[19]],
					fit.list.linger[[20]],
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Share yrs. above cut",
							"IBRD grad. elig. X (Log GNI/cap. - c)"),
			stars="default",
			caption=paste("Effects on political liberalization (Unified Democracy Score, controlling for ``lingering effects'')"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.10 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						stars.note)))




sink()




##########################################
# Explaining the diminishing effect
##########################################

I.IV0.iv = as.numeric(IV>0)
data.iv0 <- data.frame(	country = wb$country_name,
						year = wb$year,
						IV = IV,
						I.IV0 = I.IV0.iv,
						I.IV0.l1 = lagFun(I.IV0.iv, 1, wb$country_name),
						I.IV0.f1 = leadFun(I.IV0.iv, 1, wb$country_name),
						I.IV0.f2 = leadFun(I.IV0.iv, 2, wb$country_name),
						I.IV0.f3 = leadFun(I.IV0.iv, 3, wb$country_name),
						I.IV0.f4 = leadFun(I.IV0.iv, 4, wb$country_name),
						triwt = apply(cbind(1-abs(IV/lbw),0),1,max) 
						)
data.iv0.c <- subset(data.iv0, triwt>0&!is.na(I.IV0)&!is.na(IV))

elig.tab <- cbind(	
		c(100*table(data.iv0.c$I.IV0.l1)/sum(!is.na(data.iv0.c$I.IV0.l1)), 
			sum(!is.na(data.iv0.c$I.IV0.l1))),
		c(100*table(data.iv0.c$I.IV0)/sum(!is.na(data.iv0.c$I.IV0)), 
			sum(!is.na(data.iv0.c$I.IV0))),
		c(100*table(data.iv0.c$I.IV0.f1)/sum(!is.na(data.iv0.c$I.IV0.f1)), 
			sum(!is.na(data.iv0.c$I.IV0.f1))),
		c(100*table(data.iv0.c$I.IV0.f2)/sum(!is.na(data.iv0.c$I.IV0.f2)), 
			sum(!is.na(data.iv0.c$I.IV0.f2))),
		c(100*table(data.iv0.c$I.IV0.f3)/sum(!is.na(data.iv0.c$I.IV0.f3)), 
			sum(!is.na(data.iv0.c$I.IV0.f3))),
		c(100*table(data.iv0.c$I.IV0.f4)/sum(!is.na(data.iv0.c$I.IV0.f4)), 
			sum(!is.na(data.iv0.c$I.IV0.f4))))

rownames(elig.tab) <- c("Grad. ineligible \\%","Grad. eligible \\%", "N$^a$")
colnames(elig.tab) <- c("-1 yr.","+0 yr.","+1 yr.", "+2 yrs.","+3 yrs.","+4 yrs.")

sink("rd-out/elig-tab.tex")
print(xtable(elig.tab,
		align="r|rrrrrr",
		digits=0,
		caption="Eligibility status relative to year of treatment-control comparison for countries in bandwidth"),
		hline.after=c(-1,0,2),
		sanitize.text.function = function(x){x},
		caption.placement="top")
sink()

##########################################
# Unpacking the political liberalization effects
##########################################

DV.unpack <- c(	"fh_cl",
				"fh_pr",
				"fh_press",
				"p_parcomp",
				"p_xropen",
				"p_xrcomp",
				"p_xconst",
				"p_parreg",
				"worker",
				"speech")
DV.unpack.lab <- c("FH Civ. Lib.","FH Pol. Rgts.", "FH Press", "Polity Compet.","Polity Open. Exec.", "Polity Compet. Exec.", "Polity Exec. Const.", "Polity Reg. Partic.", "CIRI Worker","CIRI Speech")


DV.vec <- c("DV.f2","DV.f3","DV.f4")
DV.vec.lab <- c("(Instant.)","(1 yr. fwd.)","(2 yr. fwd.)")


# matrix to store results
results.unpack <- matrix(NA, ncol=4, nrow=length(DV.unpack)*length(DV.vec))
colnames(results.unpack) <- c("Coef.","Stand. Err.","t", "N")
rownvec <- paste(DV.unpack.lab[1], DV.vec.lab, sep=" ")
for(j in 2:length(DV.unpack)){
	rownvec <- c(rownvec, paste(DV.unpack.lab[j], DV.vec.lab, sep=" "))
}
rownames(results.unpack) <- rownvec


for(i in 1:length(DV.unpack)){
	DV.sc <- wb[,DV.unpack[i]]
	IV <- wb$IVibrd.sc

	# Reverse code the ones for which higher means degradation:
	if(DV.unpack[i]%in%c(	"fh_cl",
							"fh_pr",
							"fh_press",
							"worker",
							"speech")){
	DV.sc <- max(DV.sc, na.rm=T)-DV.sc
	}
	# Standardize outcome scale:
	DV <- (DV.sc - mean(DV.sc[!is.na(IV)&IV<0], 
			na.rm=T))/sd(DV.sc[!is.na(IV)&IV<0], 
			na.rm=T)
	DV.lab <- DV.unpack.lab[i]

	lbw <- .1

	fit.data.unpack <- data.frame(	country = wb$country_name,
						year = wb$year,
						IV = IV,
						I.IV0 = as.numeric(IV>0),
						DV.f2 = leadFun(DV, 2, wb$country_name)-DV,
						DV.f3 = leadFun(DV, 3, wb$country_name)-DV,						
						DV.f4 = leadFun(DV, 4, wb$country_name)-DV,
						triwt = apply(cbind(1-abs(IV/lbw),0),1,max) 
						)

data.f2.unpack <- subset(fit.data.unpack, triwt>0&!is.na(DV.f2)&!is.na(I.IV0)&!is.na(IV))
data.f3.unpack <- subset(fit.data.unpack, triwt>0&!is.na(DV.f3)&!is.na(I.IV0)&!is.na(IV))
data.f4.unpack <- subset(fit.data.unpack, triwt>0&!is.na(DV.f4)&!is.na(I.IV0)&!is.na(IV))

fit.f2.unpack <- lm(DV.f2~I.IV0*IV, data=data.f2.unpack)
fit.f3.unpack <- lm(DV.f3~I.IV0*IV, data=data.f3.unpack)
fit.f4.unpack <- lm(DV.f4~I.IV0*IV, data=data.f4.unpack)

fit.f2.unpack$se <- robust.se(fit.f2.unpack,as.character(data.f2.unpack$country))[[2]][,2]
fit.f3.unpack$se <- robust.se(fit.f3.unpack,as.character(data.f3.unpack$country))[[2]][,2]
fit.f4.unpack$se <- robust.se(fit.f4.unpack,as.character(data.f4.unpack$country))[[2]][,2]


results.unpack[1+3*(i-1),] <- c(coef(fit.f2.unpack)["I.IV0"],
								fit.f2.unpack$se["I.IV0"],
								coef(fit.f2.unpack)["I.IV0"]/fit.f2.unpack$se["I.IV0"],
								nrow(data.f2.unpack))
results.unpack[2+3*(i-1),] <- c(coef(fit.f3.unpack)["I.IV0"],
								fit.f3.unpack$se["I.IV0"],
								coef(fit.f3.unpack)["I.IV0"]/fit.f3.unpack$se["I.IV0"],
								nrow(data.f3.unpack))
results.unpack[3+3*(i-1),] <- c(coef(fit.f4.unpack)["I.IV0"],
								fit.f4.unpack$se["I.IV0"],
								coef(fit.f4.unpack)["I.IV0"]/fit.f4.unpack$se["I.IV0"],
								nrow(data.f4.unpack))

}


pval.unpack <- 2*(1-pt(abs(results.unpack[,3]), (results.unpack[,4] - 4)))
stars.unpack <- ifelse(pval.unpack<.01, "^{**}", ifelse(pval.unpack<.05, "^*", ifelse(pval.unpack<.1,"^\\dagger", "")))



res.unpack.tab <- round(results.unpack[,-3], digits=2)

res.unpack.tab2 <- cbind(paste(res.unpack.tab[,1], stars.unpack, sep=""),
						paste("(",res.unpack.tab[,2],")", sep=""), 
						res.unpack.tab[,3])
colnames(res.unpack.tab2) <- c("Coef.","(S.E.)","N")
sink("rd-out/unpack.tex")
print(xtable(res.unpack.tab2),
		sanitize.text.function = function(x){x},
		hline.after=c(-1,0,3,6,9,12,15,18,21,24,27,30))
sink()

##########################################
# Countries included in the main results (for appendix)
##########################################

case.table <- cbind(	as.matrix(data.l1[order(data.l1$I.IV0),c("country","year","I.IV0")]),
						rbind(as.matrix(data.f2[order(data.f2$I.IV0),c("country","year","I.IV0")]),
						matrix(NA, nrow=nrow(data.l1)-nrow(data.f2), ncol=3)),
						rbind(as.matrix(data.f3[order(data.f3$I.IV0),c("country","year","I.IV0")]),
						matrix(NA, nrow=nrow(data.l1)-nrow(data.f3), ncol=3)),
						rbind(as.matrix(data.f4[order(data.f4$I.IV0),c("country","year","I.IV0")]),
						matrix(NA, nrow=nrow(data.l1)-nrow(data.f4), ncol=3)),
						rbind(as.matrix(data.f5[order(data.f5$I.IV0),c("country","year","I.IV0")]),
						matrix(NA, nrow=nrow(data.l1)-nrow(data.f5), ncol=3)))
rownames(case.table) <- 1:nrow(case.table)
case.table.pr <- rbind(rep(c("Country","Year","Grad. elig."), 5), case.table)
colnames(case.table.pr) <- c(	"Placebo",".",".",
							"Instantaneous",".",".",
							"One-yr. out",".",".",
							"Two-yrs. out",".",".",
							"Three yrs. out",".",".")

sink("rd-out/casetable.tex")
print(	xtable(case.table.pr,
			align="r|lll|lll|lll|lll|lll"))
sink()

##########################################
# Alternative explanations
##########################################


DV.alt 		<- c(	"iTM_TAX_MRCH_WM_AR_ZS",
					"iIC_TAX_TOTL_CP_ZS",       
					"iBX_KLT_DINV_WD_GD_ZS",
					"iCM_FIN_INTL_GD_ZS",
					"iGC_TAX_TOTL_CN")

DV.alt.labs <- c(	"Wtd. Total Tariff Rate$^a$",
					"Total Tax Rate on Profit$^a$",
					"FDI Pct. GDP$^a$",
					"Intl. Capital Pct. GDP$^a$",
					"Total Tax Revenue$^a$")

##########################################
# Variance treatment effects for tariff/tax rates
##########################################

DV.vec <- c("DV.f2","DV.f3","DV.f4")
DV.vec2v <- c("(Inst., var.)", "(1-yr fwd., var.)","(2-yr fwd., var.)")
DV.vec2 <- c("(Inst., mean)", "(1-yr fwd., mean)","(2-yr fwd., mean)")

results.mat <- matrix(NA, ncol=4, nrow=2*length(DV.vec))
rownames(results.mat) <- c(paste(DV.alt.labs[1], DV.vec2v, sep=" "),
							paste(DV.alt.labs[2], DV.vec2v, sep=" "))
colnames(results.mat) <- c("Coef.","(S.E.)","t", "N")

for(i in 1:2){	
	DV.sc <- wb[,DV.alt[i]]
	IV <- wb$IVibrd.sc

	# Standardize outcome scale:
	DV <- (DV.sc - mean(DV.sc[!is.na(IV)&IV<0], 
			na.rm=T))/sd(DV.sc[!is.na(IV)&IV<0], 
			na.rm=T)
	DV.lab <- DV.alt.labs[i]

	lbw <- .1

	fit.data.var <- data.frame(country = wb$country_name,
						year = wb$year,
						IV = IV,
						I.IV0 = as.numeric(IV>0),
						DV.f2 = leadFun(DV, 2, wb$country_name),
						DV.f3 = leadFun(DV, 3, wb$country_name),						
						DV.f4 = leadFun(DV, 4, wb$country_name),
						triwt = apply(cbind(1-abs(IV/lbw),0),1,max)
						) 
	for(j in 1:length(DV.vec)){
		yarg <- DV.vec[j]

		carg <- "country"
		xarg <- "IV"
		zarg <- "I.IV0"
		warg <- "triwt"

		data.up.sc <- fit.data.var[,c(carg, yarg, xarg, zarg, warg)]
		data.up.sc2 <- na.omit(data.up.sc)
		data.up <- subset(data.up.sc2, triwt>0)

		n <- nrow(data.up)

		clus <- data.up[,carg]
		y <- data.up[,yarg]
		x <- data.up[,xarg]
		z <- data.up[,zarg]
		w <- data.up[,warg]

		# Stacked regression:
		y2 <- y^2

		y.long <- c(y,y2)
		x.long <- c(x,x)
		z.long <- c(z,z)
		w.long <- c(w,w)
		clus.long <- c(clus,clus)

		I.sq <- c(rep(0,n), rep(1,n))

		fit.long <- lm(y.long~I.sq*z.long*x.long, weights=w.long)
		b <- coefficients(fit.long)

		sigma.out.full <- robust.se(fit.long, clus.long)[[1]]

		tau.hat <- b["z.long"]
		se.tau.hat <- sqrt(sigma.out.full[	match("z.long",rownames(sigma.out.full)),
								match("z.long",colnames(sigma.out.full))])

		theta.hat <- b["I.sq:z.long"] - 2*b["(Intercept)"]*b["z.long"] - b["z.long"]^2

		par.keep <- c("I.sq:z.long", "(Intercept)", "z.long")

		sigma.out <- sigma.out.full[match(par.keep, rownames(sigma.out.full)),
					match(par.keep, colnames(sigma.out.full))]
				
		Delta.f <- as.matrix(c(1, -2*b["z.long"], -2*(b["(Intercept)"]+b["z.long"])))

		V.theta.hat <- t(Delta.f)%*%sigma.out%*%Delta.f
		se.theta.hat <- sqrt(V.theta.hat)

		results.mat[j+length(DV.vec)*(i-1),] <- c(	theta.hat,
													se.theta.hat, 
													theta.hat/se.theta.hat,
													nrow(data.up))

	}
}


##########################################
# Mean treatment effects for other alternative outcomes
##########################################

results.mat2 <- matrix(NA, ncol=4, nrow=length(DV.alt)*length(DV.vec))

for(rowup in 1:length(DV.alt)){
	if(rowup==1){rownvec <- paste(DV.alt.labs[rowup], DV.vec2, sep=" ")}
	if(rowup>1)	{rownvec <- c(rownvec,paste(DV.alt.labs[rowup], DV.vec2, sep=" "))}
}

rownames(results.mat2) <- rownvec
colnames(results.mat2) <- c("Coef.","(S.E.)","t", "N")

for(i in 1:length(DV.alt)){
	DV.sc <- wb[,DV.alt[i]]
	IV <- wb$IVibrd.sc

	# Standardize outcome scale:
	DV <- (DV.sc - mean(DV.sc[!is.na(IV)&IV<0], 
			na.rm=T))/sd(DV.sc[!is.na(IV)&IV<0], 
			na.rm=T)
	DV.lab <- DV.alt.labs[i]

	lbw <- .1

	fit.data.alt <- data.frame(	country = wb$country_name,
						year = wb$year,
						IV = IV,
						I.IV0 = as.numeric(IV>0),
						DV.f2 = leadFun(DV, 2, wb$country_name)-DV,
						DV.f3 = leadFun(DV, 3, wb$country_name)-DV,						
						DV.f4 = leadFun(DV, 4, wb$country_name)-DV,
						triwt = apply(cbind(1-abs(IV/lbw),0),1,max) 
						)

data.f2.alt <- subset(fit.data.alt, triwt>0&!is.na(DV.f2)&!is.na(I.IV0)&!is.na(IV))
data.f3.alt <- subset(fit.data.alt, triwt>0&!is.na(DV.f3)&!is.na(I.IV0)&!is.na(IV))
data.f4.alt <- subset(fit.data.alt, triwt>0&!is.na(DV.f4)&!is.na(I.IV0)&!is.na(IV))

fit.f2.alt <- lm(DV.f2~I.IV0*IV, data=data.f2.alt)
fit.f3.alt <- lm(DV.f3~I.IV0*IV, data=data.f3.alt)
fit.f4.alt <- lm(DV.f4~I.IV0*IV, data=data.f4.alt)

fit.f2.alt$se <- robust.se(fit.f2.alt,as.character(data.f2.alt$country))[[2]][,2]
fit.f3.alt$se <- robust.se(fit.f3.alt,as.character(data.f3.alt$country))[[2]][,2]
fit.f4.alt$se <- robust.se(fit.f4.alt,as.character(data.f4.alt$country))[[2]][,2]


results.mat2[1+3*(i-1),] <- c(coef(fit.f2.alt)["I.IV0"],
								fit.f2.alt$se["I.IV0"],
								coef(fit.f2.alt)["I.IV0"]/fit.f2.alt$se["I.IV0"],
								nrow(data.f2.alt))
results.mat2[2+3*(i-1),] <- c(coef(fit.f3.alt)["I.IV0"],
								fit.f3.alt$se["I.IV0"],
								coef(fit.f3.alt)["I.IV0"]/fit.f3.alt$se["I.IV0"],
								nrow(data.f3.alt))
results.mat2[3+3*(i-1),] <- c(coef(fit.f4.alt)["I.IV0"],
								fit.f4.alt$se["I.IV0"],
								coef(fit.f4.alt)["I.IV0"]/fit.f4.alt$se["I.IV0"],
								nrow(data.f4.alt))

}

resmat <- round(rbind(results.mat[,-3],results.mat2[-(1:6),-3]),2)

tvec.alt <- c(results.mat[,3],results.mat2[-(1:6),3])
nvec.alt <- c(results.mat[,4],results.mat2[-(1:6),4])
pval.alt <- 2*(1-pt(abs(tvec.alt), (nvec.alt - 4)))
stars.alt <- ifelse(pval.alt<.01, "^{**}", ifelse(pval.alt<.05, "^*", ifelse(pval.alt<.1,"^\\dagger", "")))

panel.vec <- c(	"I",
				rep("",5),
				"II",
				rep("",5),
				"III",
				rep("",2))

resmat2 <- cbind(panel.vec, rownames(resmat), resmat)

colnames(resmat2)[1:2] <- c("Set", "Outcome")
resmat3 <- cbind(resmat2[,1:2],
			paste(resmat2[,3],stars.alt,sep=""), 
			paste("(",resmat2[,4],")",sep=""),
			resmat2[,5])
colnames(resmat3) <- colnames(resmat2)

resmat.x <- xtable(resmat3, digits=2, 
					align=c("l","l","l","r","r","r"),
					caption="Effects on alternative outcomes")

sink(file="rd-out/alttab.tex")
print(	resmat.x, 
		hline.after=c(-1,0,6,12,15), 
		include.rownames=F, digits=2, 
		caption.placement="top",
		table.placement="t",
		sanitize.text.function = function(x){x})
sink()

##########################################
# Missing data checks
##########################################

# Aggregate
DV.sc <- wb[,"fh_polity2"]
IV <- wb$IVibrd.sc

# Standardize outcome scale:
DV <- (DV.sc - mean(DV.sc[!is.na(IV)&IV<0], 
			na.rm=T))/sd(DV.sc[!is.na(IV)&IV<0], 
			na.rm=T)
DV.lab <- DV.labs[i]

lbw <- .1

fit.data.mi <- data.frame(	country = wb$country_name,
						year = wb$year,
						IV = IV,
						I.IV0 = as.numeric(IV>0),
						DV = DV,
						DV.l1 = as.numeric(!is.na(DV-lagFun(DV, 2, wb$country_name))),  #t-2 to t (placebo)
						DV.f2 = as.numeric(!is.na(leadFun(DV, 2, wb$country_name)-DV)), #t to t+2 (instantaneous)
						DV.f3 = as.numeric(!is.na(leadFun(DV, 3, wb$country_name)-DV)), #t to t+3 (1 year out)
						DV.f4 = as.numeric(!is.na(leadFun(DV, 4, wb$country_name)-DV)), #t to t+4 (2 years out)
						DV.f5 = as.numeric(!is.na(leadFun(DV, 5, wb$country_name)-DV)), #t to t+5 (3 years out)
						triwt = apply(cbind(1-abs(IV/lbw),0),1,max) 
						)
data.mi <- subset(fit.data.mi, triwt>0&!is.na(DV.l1)&!is.na(I.IV0)&!is.na(IV))

fit.l1.mi <- lm(DV.l1~I.IV0*IV, data=data.mi)
fit.f2.mi <- lm(DV.f2~I.IV0*IV, data=data.mi)
fit.f3.mi <- lm(DV.f3~I.IV0*IV, data=data.mi)
fit.f4.mi <- lm(DV.f4~I.IV0*IV, data=data.mi)
fit.f5.mi <- lm(DV.f5~I.IV0*IV, data=data.mi)

fit.l1.mi$se <- robust.se(fit.l1.mi,as.character(data.mi$country))[[2]][,2]
fit.f2.mi$se <- robust.se(fit.f2.mi,as.character(data.mi$country))[[2]][,2]
fit.f3.mi$se <- robust.se(fit.f3.mi,as.character(data.mi$country))[[2]][,2]
fit.f4.mi$se <- robust.se(fit.f4.mi,as.character(data.mi$country))[[2]][,2]
fit.f5.mi$se <- robust.se(fit.f5.mi,as.character(data.mi$country))[[2]][,2]

sink("rd-out/mi-analysis.tex")
print(apsrtable(	fit.l1.mi,
					fit.f2.mi,
					fit.f3.mi,
					fit.f4.mi,
					fit.f5.mi,
			digits=2,
			coef.names=c(	"(Constant)",
							"IBRD grad. elig.",
							"Log GNI/cap. - c",
							"Interaction term"),
			stars="default",
			caption=paste("Effects on missingness for main outcome regressions"),
			model.names = c("Placebo","Instant.","1 yr.","2 yr.","3 yr."),
			notes=list("Ordinary least squares estimates within 0.10 bandwidth around cut point.",
						"Standard errors account for clustering by country.",
						"Missingness patterns are identical for both the Aggregate measure and Polity.",
						stars.note)))
sink()

##########################################
# Income trajectories for countries that approach the threshold
##########################################

nearThresh <-  tapply(fit.data$IV, 
						fit.data$country, 
						function(x) as.numeric(max(as.numeric(abs(x)<=.1), na.rm=T)==1))
threshCountries <- row.names(nearThresh)[nearThresh==1]

pdf(file="rd-out/incometraj.pdf", width=13, height=10)
par(mfrow=c(6,5))
par(mar=c(2,2,4,1))
for(i in 1:length(threshCountries)){
	plotDataUp <- subset(fit.data, country==threshCountries[i])
	if(max(as.numeric(!is.na(plotDataUp$DV.f2)))>0){
	with(plotDataUp,
		plot(	year, IV, 
				xlim=c(1987, 2010),
				ylim=c(-1.75, 1.5),
				main=unique(country)[1],
				type="p",
				pch=21,
				bg=ifelse(I.IV0==1&!is.na(I.IV0)&grad==0&!is.na(grad), 
 							"gray",
 							ifelse(I.IV0==1&!is.na(I.IV0)&grad==1&!is.na(grad),
 							"black",
 							"white")),
				cex=1.5,
				xlab="",
				ylab=""))
	abline(h=0, col="gray")
	abline(h=c(-.1, .1), col="gray", lty="dashed")
	}
}
dev.off()

##########################################
# Outcome trajectories for countries that approach the threshold
##########################################

DV.sc <- wb[,"fh_polity2"]
#DV <- (DV.sc - mean(DV.sc[!is.na(IV)&IV<0], 
#			na.rm=T))/sd(DV.sc[!is.na(IV)&IV<0], 
#			na.rm=T)
DV <- wb[,"fh_polity2"]

fit.data.l <- data.frame(country = wb$country_name,
						year = wb$year,
						IV = IV,
						I.IV0 = as.numeric(IV>0),
						DV = DV,
						DV.l1 = DV,						
						DV.f2 = leadFun(DV, 2, wb$country_name),
						DV.f3 = leadFun(DV, 3, wb$country_name),						
						DV.f4 = leadFun(DV, 4, wb$country_name),
						DV.f5 = leadFun(DV, 5, wb$country_name),
						triwt = apply(cbind(1-abs(IV/lbw),0),1,max),grad=wb$grad 
						)
pdf(file="rd-out/libtraj.pdf", width=13, height=10)
par(mfrow=c(6,5))
par(mar=c(2,2,4,1))
for(i in 1:length(threshCountries)){
	plotDataUp.sc <- subset(fit.data.l, country== threshCountries[i])
	plotDataUp <- plotDataUp.sc[!is.na(plotDataUp.sc$IV),]
	if(max(as.numeric(!is.na(plotDataUp$DV.f2)))>0){
	with(plotDataUp,
		plot(	year, DV.f2, 
				xlim=c(1987, 2008),
				ylim=c(0,11),
				main=unique(country)[1],
				type="p",
				pch=21,
				bg=ifelse(I.IV0==1&!is.na(I.IV0)&grad==0&!is.na(grad), 
 							"gray",
 							ifelse(I.IV0==1&!is.na(I.IV0)&grad==1&!is.na(grad),
 							"black",
 							"white")),
				cex=1.5,
				xlab="",
				ylab=""))
		axis(2, c(2,6,10),c(2,6,10))
			}
}
dev.off()

# FE regression of what we see in the graphs
FE.data <- subset(fit.data.l, country %in% threshCountries)

FE.data.up0 <- na.omit(FE.data[,c("DV.f2","I.IV0","country")])
fit.FE0 <- lm(DV.f2~I.IV0+as.factor(country), data=FE.data.up0)
fit.FE0$se <- robust.se(fit.FE0, FE.data.up0$country)[[2]][,2]

FE.data.up1 <- na.omit(FE.data[,c("DV.f2","I.IV0","year","country")])
fit.FE1 <- lm(DV.f2~I.IV0+as.factor(year)+as.factor(country), data=FE.data.up1)
fit.FE1$se <- robust.se(fit.FE1, FE.data.up1$country)[[2]][,2]

FE.data.up2 <- na.omit(FE.data[,c("DV.f2","IV","I.IV0","year","country")])
fit.FE2 <- lm(DV.f2~ I.IV0+IV+as.factor(year)+as.factor(country), data= FE.data.up2)
fit.FE2$se <- robust.se(fit.FE2, FE.data.up2$country)[[2]][,2]

omitcoef.FE <- setdiff(unique(c(names(coef(fit.FE0)),	
					names(coef(fit.FE1)),
					names(coef(fit.FE2)))), 
					c("I.IV0","IV"))
sink("rd-out/lib-traj.tex")
print(apsrtable(	fit.FE0,
					fit.FE1,
					fit.FE2,
			digits=2,
			coef.names=c("IBRD grad. elig.","Log(GNI/cap.)"),
			stars="default",
			omitcoef=omitcoef.FE,
			caption=paste("Changes in Freedom House-Polity Aggregate Score after graduation eligibility"),
			model.names = c("Country F.E.","Year+Country F.E.","Year+Country F.E."),
			notes=list("Ordinary least squares estimates for countries and years in Figure XX.",
						"Standard errors account for clustering by country.",
						stars.note)))
sink()

# Grad

DV <- wb[,"grad"]
fit.data.l.grad <- data.frame(country = wb$country_name,
						year = wb$year,
						IV = IV,
						I.IV0 = as.numeric(IV>0),
						DV = DV,
						DV.f2 = leadFun(DV, 2, wb$country_name),
						DV.f3 = leadFun(DV, 3, wb$country_name),						
						DV.f4 = leadFun(DV, 4, wb$country_name),
						DV.f5 = leadFun(DV, 5, wb$country_name),
						DV.f6 = leadFun(DV, 6, wb$country_name),
						DV.f7 = leadFun(DV, 7, wb$country_name),												
						triwt = apply(cbind(1-abs(IV/lbw),0),1,max),grad=wb$grad 
						)


data.f3.l <- subset(fit.data.l.grad, triwt>0&!is.na(DV.f3)&!is.na(I.IV0)&!is.na(IV))
data.f4.l <- subset(fit.data.l.grad, triwt>0&!is.na(DV.f4)&!is.na(I.IV0)&!is.na(IV))
data.f5.l <- subset(fit.data.l.grad, triwt>0&!is.na(DV.f5)&!is.na(I.IV0)&!is.na(IV))
data.f6.l <- subset(fit.data.l.grad, triwt>0&!is.na(DV.f6)&!is.na(I.IV0)&!is.na(IV))
data.f7.l <- subset(fit.data.l.grad, triwt>0&!is.na(DV.f7)&!is.na(I.IV0)&!is.na(IV))

fit.f3.l <- lm(DV.f3~I.IV0*IV, data=data.f3.l)
fit.f4.l <- lm(DV.f4~I.IV0*IV, data=data.f4.l)
fit.f5.l <- lm(DV.f5~I.IV0*IV, data=data.f5.l)
fit.f6.l <- lm(DV.f6~I.IV0*IV, data=data.f6.l)
fit.f7.l <- lm(DV.f6~I.IV0*IV, data=data.f7.l)



fit.f3.l$se <- robust.se(fit.f3.l,as.character(data.f3.l$country))[[2]][,2]
fit.f4.l$se <- robust.se(fit.f4.l,as.character(data.f4.l$country))[[2]][,2]
fit.f5.l$se <- robust.se(fit.f5.l,as.character(data.f5.l$country))[[2]][,2]
fit.f6.l$se <- robust.se(fit.f6.l,as.character(data.f6.l$country))[[2]][,2]
fit.f7.l$se <- robust.se(fit.f7.l,as.character(data.f7.l$country))[[2]][,2]
omit.coef.arg <- setdiff(names(coef(fit.f7.l)), "I.IV0")
sink("rd-out/grad-effects.tex")
apsrtable(fit.f3.l,fit.f4.l,fit.f5.l,fit.f6.l,fit.f7.l,
			omitcoef = omit.coef.arg,
			coef.names="RD estimate:",
			model.names=c("+1 year","+2 years","+3 years.","+4 years","+5 years"),
			stars="default",
			notes=list("Least squares regression discontinuity estimates.",
						"0.10 bandwidth around cut point and rectangular kernel.",
						"Standard errors account for clustering by country.",
						stars.note))
sink()

