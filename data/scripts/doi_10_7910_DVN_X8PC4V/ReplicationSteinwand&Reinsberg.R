##Figure 1
rm(list=ls())
library(readstata13)
library(lubridate)
library(plyr)

#setwd("LOCATION OF FILES")

setwd("/Users/martin/Dropbox/Documents/ProjectMultilateralAid/Data/Rework2021/ReplicationWBER")
rawdat <- read.dta13("Figures1-2.dta")

#pick sectors
nosec <- c(10, 20, 30, 99, 60, 91, 93)
rawdat$purpose_code_c <- floor(rawdat$coalesced_purpose_code/1000)
rawdat <- rawdat[!rawdat$purpose_code_c%in%nosec,]
secs <- unique(rawdat$purpose_code_c)

#initiate some variables
yrs <- 1990:2013
ndon <- c()
npro <- c()
avpro <- c()
nrep <- c()
avprorep <- c()
nsec <- c() #number of sectors per year
avprosec <- c() #avg # of projects per sector, year
avcom <- c() #avg commitment per year

#count stuff
for(i in 1:length(yrs)){
    nsec <- c(nsec, length(table(rawdat[rawdat$year==yrs[i],]$purpose_code_c)))
    ndon <- c(ndon,length(table(rawdat[rawdat$year==yrs[i],]$donor)))
    npro <- c(npro,nrow(rawdat[rawdat$year==yrs[i],]))
    avpro <- c(avpro, npro[i]/ndon[i])
    nrep <- c(nrep, length(table(rawdat[rawdat$year==yrs[i],]$recipient)))
    avprosec <- c(avprosec, npro[i]/nsec[i]/nrep[i])
    avprorep <- c(avprorep, npro[i]/nrep[i])
    avcom <- c(avcom, mean(rawdat[rawdat$year==yrs[i],]$commitment_amount_usd_constant))
}
workdat <- data.frame(cbind(yrs, ndon, npro, avpro, avprorep, avprosec, avcom))

#Plot results 
par(mar=c(5,5,4,5)+.1)
plot(workdat$yrs, workdat$avcom/1000000, type="l", col="red",
        lwd=2.5, ylab="", xlab="",cex=1.5, axes=FALSE)
axis(2, col.ticks="red", cex.axis=1, at=seq(0, 30, 5))
mtext("Average Project Size, Mil. US Dollars", side=2, line=3, cex=1)
axis(1, cex.axis=1, at=c(seq(1990, 2005, 5), 2009, 2013))
mtext("Year", side=1, line=3, cex=1)
par(new=TRUE)
plot(workdat$yrs, workdat$avprosec, type="l", col="blue",
        xaxt="n",yaxt="n",xlab="",ylab="", lwd=2.5, axes=FALSE)
axis(4, col.ticks="blue",cex.axis=1, at=c(seq(0, 10, 2)))
mtext("Projects per Sector",side=4,line=3, cex=1)
legend(1994, 1300, c("Project Size", "Projects per Sector"), lty=c(1,1), 
        lwd=2, col=c("red", "blue"), cex=1)

#save plot
pdf(file="SectorTrend.pdf",onefile=FALSE, paper="letter", height=5.8, width=6)
par(mar=c(5,5,4,5)+.1)
plot(workdat$yrs, workdat$avcom/1000000, type="l", col="red",
        lwd=2.5, ylab="", xlab="",cex=1.5, axes=FALSE)
axis(2, col.ticks="red", cex.axis=1, at=seq(0, 30, 5))
mtext("Average Project Size, Mil. US Dollars", side=2, line=3, cex=1)
axis(1, cex.axis=1, at=c(seq(1990, 2005, 5), 2009, 2013))
mtext("Year", side=1, line=3, cex=1)
par(new=TRUE)
plot(workdat$yrs, workdat$avprosec, type="l", col="blue",
        xaxt="n",yaxt="n",xlab="",ylab="", lwd=2.5, axes=FALSE)
axis(4, col.ticks="blue",cex.axis=1, at=c(seq(0, 10, 2)))
mtext("Projects per Sector",side=4,line=3, cex=1)
legend(1993, 1300, c("Project Size", "Projects per Sector"), lty=c(1,1), 
        lwd=2, col=c("red", "blue"), cex=1)
dev.off()
#########################

##Figure 2
##Left panel
rm(list=ls())
library(readstata13)
library(lubridate)
library(plyr)

setwd("/Users/martin/Dropbox/Documents/ProjectMultilateralAid/Data/Rework2021/ReplicationWBER")
rawdat <- read.dta13("Figures1-2.dta")
#pick specific years
wkdat <- rawdat[rawdat$year>1997&rawdat$year<2005,]
wkdat2 <- rawdat[rawdat$year>2004,]

#pick sectors
nosec <- c(10, 20, 30, 99, 60, 91, 92, 93)
attach(wkdat)
secname <- as.matrix(c("Education", "Health", "Population Programs", "Water", "Government & Civil Society",
        "Social Infrastructure", "Transportation", "Communication", "Energy",
        "Banking", 
        "Business Services", "Agriculture, Forestry & Fishing", "Industry",
        "Trade Policy", "Environment", "Women", "Other", "General Budget Support",
        "Food Security", "Commodity Assistance", "Humanitarian Aid",
        "Emergency Response", "Reconstruction Relief", "Disaster Preparedness"))


#WB sector shares
wbdat <- aggregate(cbind(commitment_amount_usd_constant[d_ccode==1001]), 
    by=list(purpose_code_c[d_ccode==1001]), FUN=sum)
colnames(wbdat) <- c("purpose_code_c", "commitment_amount_usd_constant")
wbdat$sec_share_WB <- wbdat$commitment_amount_usd_constant/
    sum(wbdat$commitment_amount_usd_constant)


#Bilat sector shares
bilatdat <- aggregate(cbind(commitment_amount_usd_constant)[d_ccode<1000,], 
    by=list(purpose_code_c[d_ccode<1001]), FUN=sum)
colnames(bilatdat) <- c("purpose_code_c", "commitment_amount_usd_constant")
bilatdat$sec_share_bilat <- bilatdat$commitment_amount_usd_constant/
    sum(bilatdat$commitment_amount_usd_constant)

wbdat <- wbdat[!wbdat$purpose_code_c%in%nosec,] 
bilatdat <- bilatdat[!bilatdat$purpose_code_c%in%nosec,] 
rawdat <- rawdat[!rawdat$purpose_code_c%in%nosec,]
secs <- sort(unique(rawdat$purpose_code_c))
k <- length(secs)

#prep plot 
y.axis <- c(k:1)
#order large to small 
sec_ord_wb <- order(wbdat$sec_share_WB, decreasing = TRUE)

par(mar=c(5, 11, 2, .5), xpd=TRUE)
plot(c(0,.16),c(1,k), type="n", axes=F, main="1998-2004", ylab="", 
    xlab="Share of Aid to Sector")
points(wbdat$sec_share_WB[sec_ord_wb], y.axis, pch=19)
    #plot densities
points(bilatdat$sec_share_bilat[sec_ord_wb], y.axis, pch=5, col=)
legend(.09, 4, legend=c("World Bank","Bilateral Donors"), pch=c(19,5))
axis(2, at = y.axis, label = secname[sec_ord_wb], las = 1, tick = T, 
    cex.axis = .8)
axis(1)

#save plot
pdf(file="Fig_SecShares_WB_bilat_98-04.pdf", onefile=FALSE, paper="letter", 
    height=6, width=6.1)

par(mar=c(5, 11.3, 2, .5), xpd=TRUE)
plot(c(0,.16),c(1,k), type="n", axes=F, main="1998-2004", ylab="", 
    xlab="Share of Aid to Sector", cex.lab=.9)
points(wbdat$sec_share_WB[sec_ord_wb], y.axis, pch=19)
    #plot densities
points(bilatdat$sec_share_bilat[sec_ord_wb], y.axis, pch=5, col=)
legend(.09, 4, legend=c("World Bank","Bilateral Donors"), pch=c(19,5),
    cex=.9)
axis(2, at = y.axis, label = secname[sec_ord_wb], las = 1, tick = T, 
    cex.axis = .9)
axis(1)
dev.off()

##Right panel
detach(wkdat)
attach(wkdat2)

wbdat <- aggregate(cbind(commitment_amount_usd_constant[d_ccode==1001]), 
    by=list(purpose_code_c[d_ccode==1001]), FUN=sum)
colnames(wbdat) <- c("purpose_code_c", "commitment_amount_usd_constant")
wbdat$sec_share_WB <- wbdat$commitment_amount_usd_constant/
    sum(wbdat$commitment_amount_usd_constant)

#Bilat sector shares
bilatdat <- aggregate(cbind(commitment_amount_usd_constant)[d_ccode<1000,], 
    by=list(purpose_code_c[d_ccode<1001]), FUN=sum)
colnames(bilatdat) <- c("purpose_code_c", "commitment_amount_usd_constant")
bilatdat$sec_share_bilat <- bilatdat$commitment_amount_usd_constant/
    sum(bilatdat$commitment_amount_usd_constant)

wbdat <- wbdat[!wbdat$purpose_code_c%in%nosec,] 
bilatdat <- bilatdat[!bilatdat$purpose_code_c%in%nosec,] 
rawdat <- rawdat[!rawdat$purpose_code_c%in%nosec,]
secs <- sort(unique(rawdat$purpose_code_c))
k <- length(secs)

#prep plot 
y.axis <- c(k:1)
#order large to small 
sec_ord_wb <- order(wbdat$sec_share_WB, decreasing = TRUE)

#par(mfrow=c(1,2))
par(mar=c(5, 11, 2, .5), xpd=TRUE)
plot(c(0,.16),c(1,k), type="n", axes=F, main="2005-2013", ylab="", 
    xlab="Share of Aid to Sector")
points(wbdat$sec_share_WB[sec_ord_wb], y.axis, pch=19)
    #plot densities
points(bilatdat$sec_share_bilat[sec_ord_wb], y.axis, pch=5, col=)
legend(.09, 4, legend=c("World Bank","Bilateral Donors"), pch=c(19,5))
axis(2, at = y.axis, label = secname[sec_ord_wb], las = 1, tick = T, 
    cex.axis = .8)
axis(1)

pdf(file="Fig_SecShares_WB_bilat_05-13.pdf", onefile=FALSE, paper="letter", 
    height=6, width=6.1)

par(mar=c(5, 11.3, 2, .5), xpd=TRUE)
plot(c(0,.16),c(1,k), type="n", axes=F, main="2005-2013", ylab="", 
    xlab="Share of Aid to Sector", cex.lab=.9)
points(wbdat$sec_share_WB[sec_ord_wb], y.axis, pch=19)
    #plot densities
points(bilatdat$sec_share_bilat[sec_ord_wb], y.axis, pch=5, col=)
legend(.09, 4, legend=c("World Bank","Bilateral Donors"), pch=c(19,5),
    cex=.9)
axis(2, at = y.axis, label = secname[sec_ord_wb], las = 1, tick = T, 
    cex.axis = .9)
axis(1)
dev.off()
########################


##Figure 3, continued from ReplicationSteinwand&Reinsberg.do, RUN DO FILE FIRST! 
## Left panel:  dep var bilateral sector allcoations 
rm(list=ls())
library(readstata13)
library(MASS)
set.seed(110275)

#setwd("LOCATION OF FILES")
setwd("/Users/martin/Dropbox/Documents/ProjectMultilateralAid/Data/Rework2021/ReplicationWBER")
res <- read.dta13("ProbOutMA2_bilat_alt.dta")
rawdat <- read.dta13("Models_SecAloc_1-2.dta")

pars <- as.matrix(res[1,1:168])              #extract coefficient estimates
varcov <- res[1:168, 169:ncol(res)]#extract variance-covariance matrixs
k <- 24 #number of sectors
#list of sectors
seclist <- c(11, 12, 13, 14, 15, 16, 21, 22, 23,
        24, 25, 31, 32, 33, 41, 42, 43, 51, 52,
        53, 70, 72, 73, 74)
#profile of regressors
cprof <- c(mean(rawdat$ln_pop, na.rm=TRUE),
        mean(rawdat$ln_gdp, na.rm=TRUE),
        median(log(rawdat$committed_bilat_yr+1), na.rm=TRUE),
        median(log(rawdat$WB_rec_yr+1), na.rm=TRUE))

xprof <- rep(NA, 7*k)
for(i in 1:k){
    xprof[1+7*(i-1)] <- cprof[1]
    xprof[2+7*(i-1)] <- cprof[2]
    xprof[3+7*(i-1)] <- cprof[3]
    xprof[4+7*(i-1)] <- cprof[4]
    xprof[5+7*(i-1)] <- mean(eval(parse(text=paste("rawdat$sec_share_bilat_f_",seclist[i],sep=""))))
    xprof[6+7*(i-1)] <- mean(eval(parse(text=paste("rawdat$m3_sec_share_WB_f_",seclist[i],sep=""))))
    xprof[7+7*(i-1)] <- 1
}

#function for marginal effects, simulation of confidence bands
dirm <- function(pars, varcov, xprof, spos, ppos=6, r=1000){
    meff <- rep(NA, r)
    for (i in 1:r){
        rpars <- mvrnorm(1, pars, varcov)
        fxb <- rep(NA, k)
        for (j in 1:k){
            fxb[j] <- exp(xprof[(1:7)+(j-1)*7] %*% rpars[(1:7)+(j-1)*7])
        }
        fxb <- sum(fxb)^2

        pxb <- rep(NA, k-1)
        pxprof <- xprof[-((1:7)+(spos-1)*7)]
        ppars <- rpars[-((1:7)+(spos-1)*7)]
        for(j in 1:(k-1)){
            pxb[j] <- exp(pxprof[(1:7)+(j-1)*7] %*% ppars[(1:7)+(j-1)*7])
        }
        pxb <- sum(pxb) * rpars[ppos+(spos-1)*7] * exp(xprof[(1:7)+(spos-1)*7]%*%rpars[(1:7)+(spos-1)*7])
        meff[i] <- pxb/fxb
    }
    quantile(meff, probs = c(0.025,0.5,0.975))
}

meff <- matrix(NA, k, 3)

#call to margional effects function
for (i in 1:k){
    meff[i,] <- dirm(pars, varcov, xprof, i, 6, 5000)
    cat("sector ",i," finished\n")
}

#sector labels
secname <- as.matrix(c("Education", "Health", "Population Programs", "Water", "Government & Civil Society",
        "Social Infrastructure", "Transportation", "Communication", "Energy",
        "Banking", 
        "Business Services", "Agriculture, Forestry & Fishing", "Industry",
        "Trade Policy", "Environment", "Women", "Other", "General Budget Support",
        "Food Security", "Commodity Assistance", "Humanitarian Aid",
        "Emergency Response", "Reconstruction Relief", "Disaster Preparedness"))
y.axis <- c(k:1)

#order large to small effect
sec_ord_eff <- order(meff[,2], decreasing = TRUE)
meff_p <- meff*100
#plot marginal effects
par(mar=c(5, 11, 2, .5), xpd=TRUE)
plot(c(-5,10),c(1,k), type="n", axes=F, main="Effect WB on Bilateral", ylab="", 
    xlab="Percentage change")
points(meff_p[,2][sec_ord_eff], y.axis, pch=19)
segments(meff_p[,1][sec_ord_eff], y.axis,meff_p[,3][sec_ord_eff], y.axis)
axis(2, at = y.axis, label = secname[sec_ord_eff], las = 1, tick = T, 
    cex.axis = .8)
axis(1)
lines (c(0,0), c(0,k), lwd=1, lty=3)

#save plot
pdf(file="MeffWbilat_final.pdf",onefile=FALSE, paper="letter", 
    height=5.8, width=6)
par(mar=c(5, 11, 2, .5), xpd=TRUE)
plot(c(-5,10),c(1,k), type="n", axes=F, main= "WB on Bilateral", ylab="", 
    xlab="Percentage change")
points(meff_p[,2][sec_ord_eff], y.axis, pch=19)
segments(meff_p[,1][sec_ord_eff], y.axis,meff_p[,3][sec_ord_eff], y.axis)
axis(2, at = y.axis, label = secname[sec_ord_eff], las = 1, tick = T, 
    cex.axis = .8)
axis(1)
lines (c(0,0), c(0,k), lwd=1, lty=3)
dev.off()

## Right panel:  dep var WB sector allcoations 
set.seed(110275)
res <- read.dta13("ProbOutMA2_WB.dta")
rawdat <- read.dta13("Models_SecAloc_3-4.dta")

pars <- as.matrix(res[1,1:168])              #extract coefficient estimates
varcov <- res[1:168, 169:ncol(res)]#extract variance-covariance matrixs
k <- 24 #number of sectors
#list of sectors
seclist <- c(11, 12, 13, 14, 15, 16, 21, 22, 23,
        24, 25, 31, 32, 33, 41, 42, 43, 51, 52,
        53, 70, 72, 73, 74)

#profile of regressors
cprof <- c(mean(rawdat$ln_pop, na.rm=TRUE),
        mean(rawdat$ln_gdp, na.rm=TRUE),
        median(log(rawdat$committed_bilat_yr+1), na.rm=TRUE),
        median(log(rawdat$WB_rec_yr+1), na.rm=TRUE))

xprof <- rep(NA, 7*k)
for(i in 1:k){
    xprof[1+7*(i-1)] <- cprof[1]
    xprof[2+7*(i-1)] <- cprof[2]
    xprof[3+7*(i-1)] <- cprof[3]
    xprof[4+7*(i-1)] <- cprof[4]
    xprof[5+7*(i-1)] <- mean(eval(parse(text=paste("rawdat$sec_share_WB_f_",seclist[i],sep=""))))
    xprof[6+7*(i-1)] <- mean(eval(parse(text=paste("rawdat$m2_sec_share_bilat_f_",seclist[i],sep=""))))
    xprof[7+7*(i-1)] <- 1
}

#function for marginal effects, simulation of confidence bands
dirm <- function(pars, varcov, xprof, spos, ppos=6, r=1000){
    meff <- rep(NA, r)
    for (i in 1:r){
        rpars <- mvrnorm(1, pars, varcov)
        fxb <- rep(NA, k)
        for (j in 1:k){
            fxb[j] <- exp(xprof[(1:7)+(j-1)*7] %*% rpars[(1:7)+(j-1)*7])
        }
        fxb <- sum(fxb)^2

        pxb <- rep(NA, k-1)
        pxprof <- xprof[-((1:7)+(spos-1)*7)]
        ppars <- rpars[-((1:7)+(spos-1)*7)]
        for(j in 1:(k-1)){
            pxb[j] <- exp(pxprof[(1:7)+(j-1)*7] %*% ppars[(1:7)+(j-1)*7])
        }
        pxb <- sum(pxb) * rpars[ppos+(spos-1)*7] * exp(xprof[(1:7)+(spos-1)*7]%*%rpars[(1:7)+(spos-1)*7])
        meff[i] <- pxb/fxb
    }
    quantile(meff, probs = c(0.025,0.5,0.975))
}

meff <- matrix(NA, k, 3)

#call to marginal effects function
for (i in 1:k){
    meff[i,] <- dirm(pars, varcov, xprof, i, 6, 5000)
        cat("sector ",i," finished\r")
}

#label sectors
secname <- as.matrix(c("Education", "Health", "Population Programs", "Water", "Government & Civil Society",
        "Social Infrastructure", "Transportation", "Communication", "Energy",
        "Banking", 
        "Business Services", "Agriculture, Forestry & Fishing", "Industry",
        "Trade Policy", "Environment", "Women", "Other", "General Budget Support",
        "Food Security", "Commodity Assistance", "Humanitarian Aid",
        "Emergency Response", "Reconstruction Relief", "Disaster Preparedness"))
y.axis <- c(k:1)

#order large to small effect
sec_ord_eff <- order(meff[,2], decreasing = TRUE)
meff_p <- meff*100
#plot results
par(mar=c(5, 11, 2, .5), xpd=TRUE)
plot(c(-5,10),c(1,k), type="n", axes=F, main="Bilateral on WB", ylab="", 
    xlab="Percentage change")
points(meff_p[,2][sec_ord_eff], y.axis, pch=19)
segments(meff_p[,1][sec_ord_eff], y.axis,meff_p[,3][sec_ord_eff], y.axis)
axis(2, at = y.axis, label = secname[sec_ord_eff], las = 1, tick = T, 
    cex.axis = .8)
axis(1)
lines (c(0,0), c(0,k), lwd=1, lty=3)

#save plot
pdf(file="MeffWB_final.pdf",onefile=FALSE, paper="letter", 
    height=5.8, width=6)
par(mar=c(5, 11, 2, .5), xpd=TRUE)
plot(c(-5,10),c(1,k), type="n", axes=F, main="Bilateral on WB", ylab="", 
    xlab="Percentage change")
points(meff_p[,2][sec_ord_eff], y.axis, pch=19)
segments(meff_p[,1][sec_ord_eff], y.axis,meff_p[,3][sec_ord_eff], y.axis)
axis(2, at = y.axis, label = secname[sec_ord_eff], las = 1, tick = T, 
    cex.axis = .8)
axis(1)
lines (c(0,0), c(0,k), lwd=1, lty=3)
dev.off()
######################


##Figure 4 -- Needs runboot.do to execute 
rm(list=ls())

library(readstata13)
library(RStata)

#Set directory for sampling input and ouput
setwd("/Users/martin/Dropbox/Documents/ProjectMultilateralAid/Data/Rework2021/ReplicationWBER")
#read data from which to sample
rawdat <- read.dta13("base.dta")
k <- nrow(rawdat)

#set number of boostrap reps
r=5000
set.seed(110275)

#initiate time counter
time_all=0
#initiate output matrix
out_all <- matrix(NA, r, 168)

#function to call Stata -- stata.version & stata.path need setting to current machine!
call_stat <- function(zdat){
    stata("runboot.do", data.out=FALSE, stata.version=17,
         stata.path="/Applications/Stata/StataSE.app/Contents/MacOS/stata-se")
    samout <- read.dta13("sampout.dta")
    return(samout)
}

#actual bootstrap loop
for(i in 1:r){
    l_start <- proc.time()[3]
    #draw one sample
    idx <- sample(1:k, k, replace=T)
    sdat <- data.frame(rawdat[idx,])
    #strings to factors, fix problem with write.dta in stata routine
    for (colname in names(sdat)) {
        if (is.character(sdat[[colname]])) {
            sdat[[colname]] <- as.factor(sdat[[colname]])
        }
    }
    save.dta13(sdat, "sampdat.dta")
    #call Stata for estimation
    out_all[i,] <- as.vector(t(call_stat(sdat)))
    time_rep <- proc.time()[3] -l_start
    cat("repetition ", i," took ", time_rep," seconds\n")
    time_all=time_all+time_rep
    cat("remaining time:", time_all/i*(r-i) ,"seconds\n")
    cat("that's", trunc(time_all/i*(r-i)/3600), "h ", 
        trunc(time_all/i*(r-i)/60-trunc(time_all/i*(r-i)/3600)*3600/60),
        " m \n")
}

cat("The estimation took ", trunc(time_all/3600), "h ", 
        trunc(time_all/60-trunc(time_all/3600)*3600/60),
        " m \n")

#save bootstrap sample
save(out_all, file="raw_sim_5000.rda")

#obtain quantities of interest
est <- apply(out_all, 2, quantile, c(.025,.5,.975) )

#reattach labels
vars <- c("ln_pop", "ln_gdp", "l_committed_bilat_yr", "l_WB_rec_yr", "sec_share_bilat_11",  "E_WB_share_1", "Constant",  "ln_pop", "ln_gdp", "l_committed_bilat_yr", "l_WB_rec_yr", "sec_share_bilat_12",  "E_WB_share_2", "Constant",  "ln_pop", "ln_gdp", "l_committed_bilat_yr", "l_WB_rec_yr", "sec_share_bilat_13", "E_WB_share_3", "Constant", "ln_pop", "ln_gdp", "l_committed_bilat_yr", "l_WB_rec_yr", "sec_share_bilat_14", "E_WB_share_4", "Constant","ln_pop", "ln_gdp", "l_committed_bilat_yr", "l_WB_rec_yr", "sec_share_bilat_15", "E_WB_share_5", "Constant", "ln_pop", "ln_gdp", "l_committed_bilat_yr", "l_WB_rec_yr", "sec_share_bilat_16", "E_WB_share_6", "Constant", "ln_pop", "ln_gdp", "l_committed_bilat_yr", "l_WB_rec_yr", "sec_share_bilat_21", "E_WB_share_7", "Constant",  "ln_pop", "ln_gdp", "l_committed_bilat_yr", "l_WB_rec_yr", "sec_share_bilat_22", "E_WB_share_8", "Constant", "ln_pop", "ln_gdp", "l_committed_bilat_yr", "l_WB_rec_yr", "sec_share_bilat_23", "E_WB_share_9", "Constant", "ln_pop", "ln_gdp", "  l_committed_bilat_yr", "l_WB_rec_yr", "sec_share_bilat_24", "E_WB_share_10", "Constant", "ln_pop", "ln_gdp", "  l_committed_bilat_yr", "l_WB_rec_yr", "sec_share_bilat_25", "E_WB_share_11", "Constant", "ln_pop", "ln_gdp", "  l_committed_bilat_yr", "l_WB_rec_yr", "sec_share_bilat_31", "E_WB_share_12", "Constant","ln_pop", "ln_gdp", "  l_committed_bilat_yr", "l_WB_rec_yr", "sec_share_bilat_32", "E_WB_share_13", "Constant", "ln_pop", "ln_gdp", "  l_committed_bilat_yr", "l_WB_rec_yr", "sec_share_bilat_33", "E_WB_share_14", "Constant", "ln_pop",  "ln_gdp",  "l_committed_bilat_yr", "l_WB_rec_yr", "sec_share_bilat_41", "E_WB_share_15", "Constant",  "ln_pop", "ln_gdp", "  l_committed_bilat_yr", "l_WB_rec_yr", "sec_share_bilat_42", "E_WB_share_16", "Constant",  "ln_pop", "ln_gdp", "  l_committed_bilat_yr", "l_WB_rec_yr", "sec_share_bilat_43", "E_WB_share_17", "Constant", "ln_pop", "ln_gdp", "  l_committed_bilat_yr", "l_WB_rec_yr", "sec_share_bilat_51", "E_WB_share_18", "Constant","ln_pop", "ln_gdp", "  l_committed_bilat_yr", "l_WB_rec_yr", "sec_share_bilat_52", "E_WB_share_19", "Constant", "ln_pop", "ln_gdp", "  l_committed_bilat_yr", "l_WB_rec_yr", "sec_share_bilat_53", "E_WB_share_20", "Constant", "ln_pop", "ln_gdp", "  l_committed_bilat_yr", "l_WB_rec_yr", "sec_share_bilat_70", "E_WB_share_21", "Constant",  "ln_pop", "ln_gdp", "  l_committed_bilat_yr", "l_WB_rec_yr", "sec_share_bilat_72", "E_WB_share_22", "Constant", "ln_pop", "ln_gdp", "  l_committed_bilat_yr", "l_WB_rec_yr", "sec_share_bilat_73", "E_WB_share_23", "Constant",  "ln_pop", "ln_gdp", " l_committed_bilat_yr", "l_WB_rec_yr", "sec_share_bilat_74", "E_WB_share_24", "Constant")
colnames(est) <- vars

#plot results 
#same profile of regressors as in non-IV case:
sourcedat <- read.dta13("Models_SecAloc_1-2.dta")
k <- 24 #number of sectors
seclist <- c(11, 12, 13, 14, 15, 16, 21, 22, 23,
        24, 25, 31, 32, 33, 41, 42, 43, 51, 52,
        53, 70, 72, 73, 74)

cprof <- c(mean(sourcedat$ln_pop, na.rm=TRUE),
        mean(sourcedat$ln_gdp, na.rm=TRUE),
        median(log(sourcedat$committed_bilat_yr+1), na.rm=TRUE),
        median(log(sourcedat$WB_rec_yr+1), na.rm=TRUE))

xprof <- rep(NA, 7*k)
for(i in 1:k){
    xprof[1+7*(i-1)] <- cprof[1]
    xprof[2+7*(i-1)] <- cprof[2]
    xprof[3+7*(i-1)] <- cprof[3]
    xprof[4+7*(i-1)] <- cprof[4]
    xprof[5+7*(i-1)] <- mean(eval(parse(text=paste("sourcedat$sec_share_bilat_f_",seclist[i],sep=""))))
    xprof[6+7*(i-1)] <- mean(eval(parse(text=paste("sourcedat$m3_sec_share_WB_f_",seclist[i],sep=""))))
    xprof[7+7*(i-1)] <- 1
}

#function to obtain marginal effects
dirm <- function(xprof, spos, ppos=6){
    r <- nrow(out_all)
    meff_f <- rep(NA, r)
    for (i in 1:r){
        fxb <- rep(NA, k)
        for (j in 1:k){
            fxb[j] <- exp(xprof[(1:7)+(j-1)*7] %*% out_all[i,(1:7)+(j-1)*7])
        }
        fxb <- sum(fxb)^2

        pxb <- rep(NA, k-1)
        pxprof <- xprof[-((1:7)+(spos-1)*7)]
        ppars <- out_all[i,-((1:7)+(spos-1)*7)]
        for(j in 1:(k-1)){
            pxb[j] <- exp(pxprof[(1:7)+(j-1)*7] %*% ppars[(1:7)+(j-1)*7])
        }
        pxb <- sum(pxb) * out_all[i,ppos+(spos-1)*7] * exp(xprof[(1:7)+(spos-1)*7]%*%out_all[i,(1:7)+(spos-1)*7])
        meff_f[i] <- pxb/fxb
    }
    quantile(meff_f, probs = c(0.025,0.5,0.975), na.rm=TRUE)
}

meff <- matrix(NA, k, 3)

for (i in 1:k){
    meff[i,] <- dirm(xprof, i, 6)
    cat("sector ",i," finished\n")
}

#plot results
secname <- as.matrix(c("Education", "Health", "Population Programs", "Water", "Government & Civil Society",
        "Social Infrastructure", "Transportation", "Communication", "Energy",
        "Banking", 
        "Business Services", "Agriculture, Forestry & Fishing", "Industry",
        "Trade Policy", "Environment", "Women", "Other", "General Budget Support",
        "Food Security", "Commodity Assistance", "Humanitarian Aid",
        "Emergency Response", "Reconstruction Relief", "Disaster Preparedness"))
rownames(meff) <- secname
y.axis <- c(k:1)

#order large to small effect
sec_ord_eff <- order(meff[,2], decreasing = TRUE)
meff_p <- meff*100
#par(mfrow=c(1,2))
par(mar=c(5, 11, 2, .5), xpd=TRUE)
plot(c(-51,65),c(1,k), type="n", axes=F, main="Effect WB on Bilateral (instrumented)", ylab="", 
    xlab="Percentage change")
points(meff_p[,2][sec_ord_eff], y.axis, pch=19)
segments(meff_p[,1][sec_ord_eff], y.axis,meff_p[,3][sec_ord_eff], y.axis)
axis(2, at = y.axis, label = secname[sec_ord_eff], las = 1, tick = T, 
    cex.axis = .8)
axis(1)
lines (c(0,0), c(0,k), lwd=1, lty=3)

#save plot
pdf(file="MeffWbilat_bootstrap_5000.pdf",onefile=FALSE, paper="letter", 
    height=5.8, width=6)
par(mar=c(5, 11, 2, .5), xpd=TRUE)
plot(c(-51,65),c(1,k), type="n", axes=F, main= "WB on Bilateral (instrumented)", ylab="", 
    xlab="Percentage change")
points(meff_p[,2][sec_ord_eff], y.axis, pch=19)
segments(meff_p[,1][sec_ord_eff], y.axis,meff_p[,3][sec_ord_eff], y.axis)
axis(2, at = y.axis, label = secname[sec_ord_eff], las = 1, tick = T, 
    cex.axis = .8)
axis(1)
lines (c(0,0), c(0,k), lwd=1, lty=3)
dev.off()
#######################

## Fig 5 
rm(list=ls())
library(readstata13)
library(MASS)
library(ggplot2)

#setwd("LOCATION OF FILES")
setwd("/Users/martin/Dropbox/Documents/ProjectMultilateralAid/Data/Rework2021/ReplicationWBER/")
rawdat <- read.dta13("Models_ProjCount.dta")

noWBdense <- density(sqrt(rawdat[rawdat$noWB9==1,]$project_count_bilat_f))
WBdense <- density(sqrt(rawdat[rawdat$noWB==0,]$project_count_bilat_f))

plot(c(0,9), c(0, 1.5), type="n", xlab="Count of Bilateral Projects (Square Root Scale)",
    ylab="Density", axes=FALSE)
lines(noWBdense, lty=2, lwd=2)
lines(WBdense, lwd=2)
axis(1, at = c(0, 2, 4, 6, 8), label = c(0, 2, 4, 6, 8)^2, las = 1, tick = T)
axis(2)
legend(6, 1.5, c("No World Bank (9 yrs)", "Some World Bank"), lty=c(2,1), 
        lwd=1)

#save plot
pdf(file="BilatProjByWB.pdf",onefile=FALSE, paper="letter", height=5.5, width=6)
plot(c(0,9), c(0, 1.5), type="n", xlab="Count of Bilateral Projects (Square Root Scale)",
    ylab="Density", axes=FALSE)
lines(noWBdense, lty=2, lwd=1.5)
lines(WBdense, lwd=1.5)
axis(1, at = c(0, 2, 4, 6, 8), label = c(0, 2, 4, 6, 8)^2, las = 1, tick = T)
axis(2)
legend(4.4, 1.5, c("No World Bank (9 yrs)", "Some World Bank"), lty=c(2,1), 
        lwd=1.5)
dev.off()
########################



## Fig 6, continued from ReplicationSteinwand&Reinsberg.do, RUN DO FILE FIRST! 
rm(list=ls())
library(readstata13)
library(MASS)
set.seed(110275)


#setwd("LOCATION OF FILES")
setwd("/Users/martin/Dropbox/Documents/ProjectMultilateralAid/Data/Rework2021/ReplicationWBER/")
res <- read.dta13("ProjCtDDif_out.dta")
rawdat <- read.dta13("Models_ProjCount.dta")

#import estimates and var-cov matrix, & clean up dropped variables
pars <- as.matrix(res[1,1:56]) #extract coefficient estimates
pars <- pars[-which(pars==0)]
#pars <- pars[-length(pars)] #remove alpha estimate
#varcov <- res[1:235, 246:(ncol(res)-1)]#extract variance-covariance matrixs, leaving out alpha
varcov <- res[1:55, 57:(ncol(res))]#extract variance-covariance matrixs
varcov <- varcov[,-which(apply(varcov, 2, sum)==0)]
varcov <- as.matrix(varcov[-which(apply(varcov, 1, sum)==0),])

#representative profile of regressors
xprof <- matrix(0, length(pars)-1, 9)
    #note: each COLUMN is a different profile
xprof[1,1] <- 1
xprof[2,2] <- 1
xprof[3,3] <- 1
xprof[4,4] <- 1
xprof[8,5] <- 1
xprof[7,6] <- 1
xprof[6,7] <- 1
xprof[5,8] <- 1
xprof[9,] <- 1
xprof[9,9] <-0
xprof[10,] <- mean(rawdat[rawdat$ddifmod==1,]$committed_bilat_sec_yr)
xprof[11,] <- mean(rawdat[rawdat$ddifmod==1,]$committed_WB_sec_yr)
xprof[12,] <- mean(rawdat[rawdat$ddifmod==1,]$ln_pop)
xprof[13,] <- mean(rawdat[rawdat$ddifmod==1,]$gdppc)
xprof[length(pars)-1,]<-1


#sampling of expected means
r <- 5000 #number of samples
yhats <- matrix(NA, ncol(xprof)-1, r)
for (i in 1:r){
    rpars <- mvrnorm(1, pars, varcov)
 #   yhats[,i] <- t(xprof[,1:8]) %*% as.matrix(rpars[1:nrow(xprof)])
    yhats[,i] <- exp(t(xprof[,1:8]) %*% as.matrix(rpars[1:nrow(xprof)]))-rep(exp(xprof[,9] %*% as.matrix(rpars[1:nrow(xprof)])), 8)
}

ypred <- apply(yhats, 1, quantile,  probs = c(0.025,0.5,0.975))

#arrival rate at the representative profile
exp(xprof[,9]%*%pars[1:nrow(xprof)])

#plot results
effname <- c("3 years before", "2 years before", "1 year before", "Year of treatment", "1 year after", "2 years after", "3 years after", "4 years after")
y.axis <- c(length(effname):1)
par(mar=c(5, 7.5, 4, 2) + 0.1)
plot(c(0.05,.25),c(1,8), type="n", main="", ylab="", axes=F,  
    xlab="Expected additional projects\n Baseline: E(no. projects)=0.59")
points(ypred[2,1:8], y.axis, pch=19)
segments(ypred[1,1:8], y.axis,ypred[3,1:8], y.axis)
axis(1)
axis(2, at=y.axis, label=effname, las=1)

#save plot
pdf(file="DDifByYear.pdf",onefile=FALSE, paper="letter", height=5.8, width=6)
par(mar=c(5, 7.5, 4, 2) + 0.1)
plot(c(0.05,.25),c(1,8), type="n", main="", ylab="", axes=F,  
    xlab="Expected additional projects\n Baseline: E(no. projects)=0.59")
points(ypred[2,1:8], y.axis, pch=19)
segments(ypred[1,1:8], y.axis,ypred[3,1:8], y.axis)
axis(1)
axis(2, at=y.axis, label=effname, las=1)
dev.off()

