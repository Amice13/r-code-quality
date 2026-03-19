################################################################################
# HNA.R
#
# Analysis of D. DeSante's data from the Harvey Monroe Hall Research Natural 
# Area. 
#
# File created by J. Saracco 8 Sep 2020
# Last edited 25 Sep 2020
################################################################################

#load packages
library(plyr)
library(jagsUI)
library(plotrix)

# read climate data
clim <- read.csv("clim.csv")

# correlation between 95% snow-free date and spring temperature
cor.test(clim$Jdate, clim$Tave_sp.AprJun)

# Pearson's product-moment correlation
# 
# data:  clim$Jdate and clim$Tave_sp.AprJun
# t = -5.5975, df = 30, p-value = 4.315e-06
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.8512761 -0.4875620
# sample estimates:
#        cor 
# -0.7147427 

#-------------------------------------------------------------------------------
# Cimate time series figure (Fig 2)
#-------------------------------------------------------------------------------
tiff("figs/Fig2_clim_ts.tif", height=5, width=6.5, res=600, units="in")
par(mfrow = c(4,1),
    oma = c(5,4.5,1,0) + 0.1,
    mar = c(0,0,0,1))
plot(clim$Year, clim$Jdate, type = "l", xaxt="n", las=1, ylim = c(130, 240))
title(main = "95% snow-free date", font.main = 1, line = -1, adj=0.02)
plot(clim$Year, clim$Tave_sp.AprJun, type ="l", xaxt="n", las=1, ylim = c(0, 6))
title(main = expression(paste("Mean mean spring temperature (", degree, "C)", sep="")), 
      font.main = 1, line = -1, adj=0.02)
plot(clim$Year, clim$Tmin_sm.JunAug, type = "l", xaxt="n", las=1, ylim = c(2, 6.5))
title(main = expression(paste("Mean minimum summer temperature (", degree, "C)", sep="")), 
      font.main = 1, line = -1, adj=0.02)
plot(clim$Year, clim$PPT_sm.JunAug, type = "l", las=1, ylim = c(0, 135), yaxt="n")
axis(2, at = c(0, 25, 50, 75, 100, 125), las = 1)
title(main = "Summer precipitation (mm)", 
      font.main = 1, line = -1, adj=0.02)
mtext(side = 2, "Climate variable", line = 3, cex = 1.2, outer = T)

mtext(side = 1, "Year", line = 3, cex = 1.2, outer = T)
dev.off()

#-------------------------------------------------------------------------------
# Summer temperature trend model
#-------------------------------------------------------------------------------

sink("clim_reg.txt")
cat("
    
    model{
    
    b0 ~ dnorm(0, 0.001)
    b1 ~ dnorm(0, 0.001)
    
    tau <- 1/(sigma*sigma)
    sigma ~ dunif(0,1000)
    
    # likelihood
    for (i in 1:nyears){
    
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- b0 + b1*yr[i] 
    # Assess model fit using sums of squares type discrepancy
    
    residual[i] <- y[i] - mu[i] #residual for observed data
    predicted[i] <- mu[i] # predicted values
    sq[i] <- pow(residual[i], 2) # squared residuals for observed data
    
    # Generate replicate data and compute fit stats for them
    y.new[i] ~ dnorm(mu[i], tau)
    sq.new[i] <- pow(y.new[i] - predicted[i], 2)
    
    }
    
    fit <- sum(sq[])
    fit.new <- sum(sq.new[])
    test <- step(fit.new - fit)
    bpvalue <- mean(test)
    
    }
    ", fill=TRUE)
sink()

y <- clim$Tmin_sm.JunAug
yr <- clim$Year - 1994

jags.data <- list(y = y, nyears=nyears, yr=yr)  

# initial values
inits <- function(){
  list(b0=rnorm(1), b1=rnorm(1)) }
# 
parms <- c("b0", "b1", "bpvalue", "sigma")

sm_temp.out <- jags(jags.data, inits, parms, "clim_reg.txt", n.thin=nt, 
                    n.chains = nc, n.adapt=na, n.burnin = nb, n.iter = ni, 
                    parallel = T)

print(sm_temp.out)

# mean    sd   2.5%    50%  97.5% overlap0 f Rhat n.eff
# b0        4.352 0.123  4.109  4.352  4.595    FALSE 1    1 60000
# b1        0.066 0.013  0.040  0.066  0.092    FALSE 1    1 60000
# bpvalue   0.534 0.499  0.000  1.000  1.000     TRUE 1    1 19395
# sigma     0.688 0.094  0.535  0.678  0.903    FALSE 1    1 18601
# deviance 65.243 2.675 62.221 64.540 72.202    FALSE 1    1 24855

#-------------------------------------------------------------------------------
# Species richness and total number of territories models
#-------------------------------------------------------------------------------

# read in bird species list (used for taxonomic sorting - note this version is 
# not updated with 2019 AOS supplement, which has different ordering for sparrows)

specnum <- reas.csv("specnum.csv")

# read in bird data 
bm <- read.csv("bm.csv")

# Initially implemented poisson model, but underdispersed. So used normal.

sink("reg1_norm.txt")
cat("
    
    model{
    
    b0 ~ dnorm(0, 0.001)
    b1 ~ dnorm(0, 0.001)
    b2 ~ dnorm(0, 0.001)
    
    tau <- 1/(sigma*sigma)
    sigma ~ dunif(0,1000)
    
    # likelihood
    for (i in 1:nyears){
    
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- b0 + b1*sfd[i] + b2*sp.temp.r[i]  
    
    # Assess model fit using sums of squares type discrepancy
    residual[i] <- y[i] - mu[i] #residual for observed data
    predicted[i] <- mu[i] # predicted values
    sq[i] <- pow(residual[i], 2) # squared residuals for observed data
    
    # Generate replicate data and compute fit stats for them
    y.new[i] ~ dnorm(mu[i], tau)
    sq.new[i] <- pow(y.new[i] - predicted[i], 2)
    
    }
    
    fit <- sum(sq[])
    fit.new <- sum(sq.new[])
    test <- step(fit.new - fit)
    bpvalue <- mean(test)
    
    }
    ", fill=TRUE)
sink()

# data input for jags
y <- bm$sr
sp.temp.r <- as.numeric(lm(clim$Tave_sp.AprJun ~ clim$JDate)$residuals)
sp.temp.r <- (sp.temp.r - mean(sp.temp.r))/sd(sp.temp.r)
nyears <- 32
sfd <- (clim$Jdate - mean(clim$Jdate))/sd(clim$Jdate)
jags.data <- list(y = y, nyears=nyears, sp.temp.r = sp.temp.r, sfd = sfd)  

# initial values
inits <- function(){list(b0=runif(1, 0, 50), b1=runif(1, -50, 0), b2=rnorm(1))}

# parameters to monitor 
parms <- c("b0", "b1", "b2", "bpvalue", "sigma")

# McMC settings
ni <- 100000
nb <- 40000
na <- 80000
nt <- 4
nc <- 4

# run model
sr.norm.out <- jags(jags.data, inits, parms, "reg1_norm.txt", n.thin=nt, 
                    n.chains = nc, n.adapt=na, n.burnin = nb, n.iter = ni, 
                    parallel = T)

print(sr.norm.out)

# mean    sd    2.5%     50%   97.5% overlap0     f Rhat n.eff
# b0        18.088 0.488  17.122  18.090  19.056    FALSE 1.000    1 60000
# b1        -2.950 0.502  -3.940  -2.952  -1.964    FALSE 1.000    1 60000
# b2        -0.253 0.504  -1.246  -0.253   0.744     TRUE 0.698    1 60000
# bpvalue    0.536 0.499   0.000   1.000   1.000     TRUE 1.000    1 60000
# sigma      2.738 0.385   2.110   2.698   3.614    FALSE 1.000    1 60000
# deviance 153.646 3.160 149.763 152.919 161.655    FALSE 1.000    1 60000

y <- bm$tot
sp.temp.r <- as.numeric(lm(clim$Tave_sp.AprJun ~ clim$JDate)$residuals)
sp.temp.r <- (sp.temp.r - mean(sp.temp.r))/sd(sp.temp.r)
nyears <- 32
sfd <- (clim$Jdate - mean(clim$Jdate))/sd(clim$Jdate)
jags.data <- list(y = y, nyears=nyears, sp.temp.r = sp.temp.r, sfd = sfd)  

# initial values
inits <- function(){list(b0=runif(1, 100, 300), b1=runif(1, -50, 0), b2=rnorm(1))}

# parameters to monitor 
parms <- c("b0", "b1", "b2", "bpvalue", "sigma")

nterr_Tot.out <- jags(jags.data, inits, parms, "reg1_norm.txt", n.thin=nt, 
                      n.chains = nc, n.adapt=na, n.burnin = nb, n.iter = ni, 
                      parallel = T)

print(nterr_Tot.out)

# mean    sd    2.5%     50%   97.5% overlap0     f Rhat n.eff
# b0       178.184 3.984 169.970 178.298 185.709    FALSE 1.000    1 60000
# b1       -29.184 3.976 -36.982 -29.204 -21.268    FALSE 1.000    1 60000
# b2        -4.526 3.980 -12.420  -4.534   3.325     TRUE 0.876    1 60000
# bpvalue    0.536 0.499   0.000   1.000   1.000     TRUE 1.000    1 60000
# sigma     21.896 3.114  16.857  21.539  28.976    FALSE 1.000    1 41974
# deviance 286.682 3.554 282.304 285.866 295.814    FALSE 1.000    1 50354

#-------------------------------------------------------------------------------
# Fig. 3 - Species richness and number of territories as fn. of sfd
#-------------------------------------------------------------------------------

sfd.new <- seq(min(sfd), max(sfd), 0.02)
sr.sfd.pred <- matrix(NA, nrow=length(sr.norm.out$sims.list$b0), 
                      ncol=length(sfd.new))
nt.sfd.pred <- matrix(NA, nrow = length(nterr_Tot.out$sims.list$b0), 
                      ncol = length(sfd.new))

for (i in 1:dim(sr.sfd.pred)[1]){
  for (j in 1:dim(sr.sfd.pred)[2]){
    sr.sfd.pred[i,j] <- sr.norm.out$sims.list$b0[i] + sr.norm.out$sims.list$b1[i]*sfd.new[j]
    nt.sfd.pred[i,j] <- nterr_Tot.out$sims.list$b0[i] + nterr_Tot.out$sims.list$b1[i]*sfd.new[j]
  }
}

sr.sfd.mn <- colMeans(sr.sfd.pred)
sr.sfd.lcl <- apply(sr.sfd.pred, 2, quantile, probs = 0.025)
sr.sfd.ucl <- apply(sr.sfd.pred, 2, quantile, probs = 0.975)

nt.sfd.mn <- colMeans(nt.sfd.pred)
nt.sfd.lcl <- apply(nt.sfd.pred, 2, quantile, probs = 0.025)
nt.sfd.ucl <- apply(nt.sfd.pred, 2, quantile, probs = 0.975)

tiff("figs/Fig3_sr_nterr_sfd.tif", height=6.5, width=6.5, res=600, units="in")
par(mar=c(4,5,1,1), mfrow=c(2,1))
plot(sfd.new, rep(10, length(sfd.new)), type = "n", ylim = c(5, 35), las=1, xaxt="n", 
     xlab="95% snow-free date", 
     ylab="Breeding species richness", cex.lab=1, cex.axis=1)
xlabs <- seq(130, 215, 10)
axis(1, at = (xlabs-mean(clim$Jdate))/sd(clim$Jdate), labels=xlabs, cex.axis=1)
title(main = "A", line = -1.5, adj=0.05, cex.main = 1.5)

polygon(x=c(sfd.new,  rev(sfd.new)), 
        y=c(sr.sfd.lcl, 
            rev(sr.sfd.ucl)), col="gray90", border=NA)

lines(sfd.new, sr.sfd.mn, col = "black", lty=1, lwd=2)
points(sfd, bm$sr)

plot(sfd.new, rep(10, length(sfd.new)), type = "n", ylim = c(80, 280), las=1, xaxt="n", 
     xlab="95% snow-free date", 
     ylab="No. breeding territories", cex.lab=1, cex.axis=1)
xlabs <- seq(130, 215, 10)
axis(1, at = (xlabs-mean(clim$Jdate))/sd(clim$Jdate), labels=xlabs, cex.axis=1)
title(main = "B", line = -1.5, adj=0.05, cex.main = 1.5)

polygon(x=c(sfd.new,  rev(sfd.new)), 
        y=c(nt.sfd.lcl, 
            rev(nt.sfd.ucl)), col="gray90", border=NA)

lines(sfd.new, nt.sfd.mn, col = "black", lty=1, lwd=2)
points(sfd, bm$tot)

dev.off()

#-------------------------------------------------------------------------------
# Multispecies model of Lambda covariate relationships and trends
#-------------------------------------------------------------------------------

# Specify model in BUGS language

sink("Nterritories_lambda.txt")
cat("

         model {

         #-------------------------------------------------
         # 1. Define the priors for the parameters
         #-------------------------------------------------
         

         for (i in 1:nspec){
         
         mu.y[i,1] ~ dnorm(mu.y1, tau.y1)
         
         b0[i] ~ dnorm(mu.b0, tau.b0)
         b1[i] ~ dnorm(mu.b1, tau.b1)
         b2[i] ~ dnorm(mu.b2, tau.b2)
         
         for (t in 1:nyears){
         y[i,t] ~ dpois(mu.y[i,t])
         
         # goodness-of-fit stats
         y.sim[i,t] ~ dpois(mu.y[i,t])
         err[i,t] <- pow(y[i,t]-mu.y[i,t],2)/mu.y[i,t]
         ferr[i,t] <- pow(y.sim[i,t]-mu.y[i,t],2)/mu.y[i,t]
         }


         for(t in 2:(nyears)){
         mu.y[i,t] <- lambda[i,t-1]*mu.y[i,t-1]
         }
         
         for (t in 1:(nyears-1)){
         log(lambda[i,t]) <- b0[i] + b1[i]*sfd[t] + b2[i]*sp.temp.r[t] 
         + e[i,t]
         
         e[i,t] ~ dnorm(0, tau.e)
         }
         
         }
         
         gof <- sum(err[,])
         fgof <- sum(ferr[,])
         diffgof <- gof-fgof
         posdiff <- step(diffgof)
         
         tau.e <- pow(sigma.e, -2)
         sigma.e ~ dunif(0, 10)
         
         mu.y1 ~ dunif(0, 100)
         
         mu.b0 <- log(exp.mu.b0)
         exp.mu.b0 ~ dunif(0.01,5)
         mu.b1 ~ dnorm(0, 0.01)
         mu.b2 ~ dnorm(0, 0.01)
 
         tau.b0 <- pow(sigma.b0, -2)        
         tau.b1 <- pow(sigma.b1, -2)
         tau.b2 <- pow(sigma.b2, -2)
         tau.y1 <- pow(sigma.y1, -2)
         sigma.b0 ~ dunif(0,20)
         sigma.b1 ~ dunif(0, 20)
         sigma.b2 ~ dunif(0, 20)
         sigma.y1 ~ dunif(0, 20)
         
         }
         
         ",fill = TRUE)
sink()

# read in species x year territory matrix
ad.ct.df <- read.csv("adct.csv") 
ad.ct <- as.matrix(ad.ct.df[,2:ncol(ad.ct.df)])
row.names(ad.ct) <- ad.ct.df$SPEC
rm(ad.ct.df)
nyears <- 32
sfd <- (clim$Jdate[2:nyears] - mean(clim$Jdate[2:nyears]))/sd(clim$Jdate[2:nyears])
sp.temp.r <- as.numeric(lm(clim$Tave_sp.AprJun[2:nyears] ~ clim$Jdate[2:nyears])$residuals)
sp.temp.r <- (sp.temp.r - mean(sp.temp.r))/sd(sp.temp.r)
nspec <- nrow(ad.ct)

jags.data <- list(y = ad.ct, nspec=nspec, nyears=nyears, sp.temp.r = sp.temp.r, sfd = sfd)  

# initial values
inits <- function(){
  list(exp.mu.b0=runif(1, .8, 1.2), mu.b1=runif(1, 0, 1), sigma.y1 = sd(ad.ct[,1]), 
       sigma.b0=runif(1), mu.b2=runif(1, -.2, .2), sigma.b1=runif(1), sigma.b2=runif(1), 
       mu.y1 = mean(ad.ct[,1]), sigma.e = dunif(1, 0, 10))}

# parameters to monitor
parms <- c("mu.y1", "mu.y", "b0", "mu.b0", "b1", "mu.b1", "b2", "mu.b2", "sigma.y1","sigma.b0",
           "sigma.b1", "sigma.b2", "posdiff", "e")

# run model
terr.lam.out <- jags(jags.data, inits, parms, "Nterritories_lambda.txt", n.thin=nt, 
                     n.chains = nc, n.adapt=na, n.burnin = nb, n.iter = ni, 
                     parallel = T)

lam <- array(NA, dim = c(60000, nspec, (nyears-1)))

for (i in 1:60000){
  for (j in 1:nspec){
    for (k in 1:(nyears-1)){
      lam[i,j,k] <- exp(terr.lam.out$sims.list$b0[i,j] + terr.lam.out$sims.list$b1[i,j]*sfd[k] 
                        + terr.lam.out$sims.list$b2[i,j]*sp.temp.r[k] 
                        + terr.lam.out$sims.list$e[i,j,k])
    }
  }
}

yrmn.lambda <- apply(lam, c(2,3), mean)

gm_mean <- function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

gm_lam <- matrix(NA, nrow=dim(lam)[1], ncol = dim(lam)[2])
for (i in 1:nrow(gm_lam)){
  for (j in 1:nspec){
    gm_lam[i,j] <- gm_mean(lam[i,j,], na.rm=T)
  }
}

#-------------------------------------------------------------------------------
# Table 1 - trends
#-------------------------------------------------------------------------------
trend.mn <- colMeans(100*(gm_lam-1))
trend.lcl <- apply(100*(gm_lam-1), 2, quantile, probs=0.025)
trend.ucl <- apply(100*(gm_lam-1), 2, quantile, probs=0.975)
trends <- data.frame(SPEC=row.names(ad.ct), mn = trend.mn, lcl = trend.lcl, ucl = trend.ucl)
trends <- merge(specnum, trends)
trends <- trends[order(trends$SSN),]
write.csv(trends, "trends.csv", row.names=F)

#-------------------------------------------------------------------------------
# Fig. 4 - Lambda (multispecies) - covariate relationships
#-------------------------------------------------------------------------------

sfd.new <- seq(min(sfd), max(sfd), 0.02)
lam.sfd.pred <- matrix(NA, nrow=length(terr.lam.out$sims.list$mu.b0), 
                        ncol=length(sfd.new))

for (i in 1:dim(lam.sfd.pred)[1]){
  for (j in 1:dim(lam.sfd.pred)[2]){
    lam.sfd.pred[i,j] <- exp(terr.lam.out$sims.list$mu.b0[i] + terr.lam.out$sims.list$mu.b1[i]*sfd.new[j])
  }
}

lam.sfd.mn <- colMeans(lam.sfd.pred)
lam.sfd.lcl <- apply(lam.sfd.pred, 2, quantile, probs = 0.025)
lam.sfd.ucl <- apply(lam.sfd.pred, 2, quantile, probs = 0.975)

sp.temp.r.new <- seq(min(sp.temp.r), max(sp.temp.r), 0.02)
lam.temp.pred <- matrix(NA, nrow=length(terr.lam.out$sims.list$mu.b0), 
                        ncol=length(sp.temp.r.new))

for (i in 1:dim(lam.temp.pred)[1]){
  for (j in 1:dim(lam.temp.pred)[2]){
    lam.temp.pred[i,j] <- exp(terr.lam.out$sims.list$mu.b0[i] + terr.lam.out$sims.list$mu.b2[i]*sp.temp.r.new[j])
  }
}

lam.temp.mn <- colMeans(lam.temp.pred)
lam.temp.lcl <- apply(lam.temp.pred, 2, quantile, probs = 0.025)
lam.temp.ucl <- apply(lam.temp.pred, 2, quantile, probs = 0.975)

# species effects 
species.lam.ef <- data.frame(
  SPEC=row.names(ad.ct),
  b1.mn = apply(terr.lam.out$sims.list$b1, 2, mean),
  b1.lcl = apply(terr.lam.out$sims.list$b1, 2, quantile, probs=0.025),
  b1.ucl = apply(terr.lam.out$sims.list$b1, 2, quantile, probs=0.975),
  b2.mn = apply(terr.lam.out$sims.list$b2, 2, mean),
  b2.lcl = apply(terr.lam.out$sims.list$b2, 2, quantile, probs=0.025),
  b2.ucl = apply(terr.lam.out$sims.list$b2, 2, quantile, probs=0.975)
)

tiff("figs/Fig4_lambda_cov_relationships.tif", height=5, width=6.5, res=600, units="in")
par(mar=c(4,5,1,0), mfrow = c(2,2))
plot(sfd.new, rep(.5, length(sfd.new)), type = "n", ylim = c(0.5, 1.5), las=1, xaxt="n", 
     xlab="95% snow-free date", 
     ylab=expression(paste("Mean population change (", hat(lambda), ")")), cex.lab=1, cex.axis=1)
xlabs <- seq(130, 210, 10)
axis(1, at = (xlabs-mean(clim$Jdate[2:32]))/sd(clim$Jdate[2:32]), labels=xlabs, cex.axis=1)

polygon(x=c(sfd.new,  rev(sfd.new)), 
        y=c(lam.sfd.lcl, 
            rev(lam.sfd.ucl)), col="gray90", border=NA)

lines(sfd.new, lam.sfd.mn, col = "black", lty=1, lwd=2)
title(main = "A", line = -1.5, adj=0.05, cex.main = 1.5)
par(mar=c(4,3,1,3))
plotCI(x=species.lam.ef$b1.mn[order(species.lam.ef$b1.mn)], y=seq(1,nspec,1), 
       li=species.lam.ef$b1.lcl[order(species.lam.ef$b1.mn)],
       ui=species.lam.ef$b1.ucl[order(species.lam.ef$b1.mn)], err="x", 
       las=1, sfrac=0, yaxt="n", pch=NA, gap = 0.05, 
       xlab = expression(paste("95% snow-free-date coefficient ( ", hat(beta)[1], ")")),
       ylab = "")

text(x = species.lam.ef$b1.mn[order(species.lam.ef$b1.mn)],
     y=seq(1,nspec, 1), labels=species.lam.ef$SPEC[order(species.lam.ef$b1.mn)],
     cex=.6)
abline(v=0,lty=2)
title(main = "B", line = -1.5, adj=0.05, cex.main = 1.5)

par(mar=c(4,5,1,0))
plot(sp.temp.r.new, rep(.5, length(sp.temp.r.new)), type = "n", ylim = c(.5, 1.5), las=1, xaxt="n", 
     xlab="Spring temperature residuals", 
     ylab=expression(paste("Mean population change (", hat(lambda), ")")), cex.lab=1, cex.axis=1)
xlabs <- seq(-2, 1.5, 0.35)
axis(1, at = (xlabs-mean(sp.temp.r))/sd(sp.temp.r), labels=xlabs, cex.axis=1)

polygon(x=c(sp.temp.r.new,  rev(sp.temp.r.new)), 
        y=c(lam.temp.lcl, 
            rev(lam.temp.ucl)), col="gray90", border=NA)

lines(sp.temp.r.new, lam.temp.mn, col = "black", lty=1, lwd=2)
title(main = "C", line = -1.5, adj=0.05, cex.main = 1.5)

par(mar=c(4,3,1,3))
plotCI(x=species.lam.ef$b2.mn[order(species.lam.ef$b2.mn)], y=seq(1,nspec,1), 
       li=species.lam.ef$b2.lcl[order(species.lam.ef$b2.mn)],
       ui=species.lam.ef$b2.ucl[order(species.lam.ef$b2.mn)], err="x", 
       las=1, sfrac=0, yaxt="n", pch=NA, gap = 0.05, 
       xlab = expression(paste("Spring temp. residuals coefficient ( ", hat(beta)[2], ")")),
       ylab = "")
text(x = species.lam.ef$b2.mn[order(species.lam.ef$b2.mn)],
     y=seq(1,nspec, 1), labels=species.lam.ef$SPEC[order(species.lam.ef$b2.mn)],
     cex=.6)
abline(v=0,lty=2)
title(main = "D", line = -1.5, adj=0.05, cex.main = 1.5)

dev.off()

#-------------------------------------------------------------------------------
# Fledging date models
#-------------------------------------------------------------------------------

sink("fledg_pheno.txt")
cat("

    model {
         

    ## Likelihood
    for(i in 1:n){
      y[i] ~ dnorm(mu[i], tau)
      mu[i] <- b0[spec[i]] + b1[spec[i]]*snow[i] + b2[spec[i]]*sp.temp.r[i]
      
    residual[i] <- y[i] - mu[i] #residual for observed data
    predicted[i] <- mu[i] # predicted values
    sq[i] <- pow(residual[i], 2) # squared residuals for observed data
      
    # Generate replicate data and compute fit stats for them
    y.new[i] ~ dnorm(mu[i], tau)
    sq.new[i] <- pow(y.new[i] - predicted[i], 2)
    } 
    
    fit <- sum(sq[])
    fit.new <- sum(sq.new[])
    test <- step(fit.new - fit)
    bpvalue <- mean(test)
    
    ## Priors
    mu.b0 ~ dnorm(0, 0.001)
    mu.b1 ~ dnorm(0, 0.001)
    mu.b2 ~ dnorm(0, 0.001)
    
    tau.b0 <- pow(sigma.b0, -2)
    tau.b1 <- pow(sigma.b1, -2)
    tau.b2 <- pow(sigma.b2, -2)
    
    sigma.b0 ~ dunif(0, 1000)
    sigma.b1 ~ dunif(0, 1000)
    sigma.b2 ~ dunif(0, 1000)
    
    tau <- pow(sigma, -2)
    sigma ~ dunif(0, 1000)
  
    for (s in 1:nspec){
    b0[s] ~ dnorm(mu.b0, tau.b0)
    b1[s] ~ dnorm(mu.b1, tau.b1)
    b2[s] ~ dnorm(mu.b2, tau.b2)
    }
    
    }
         
         ",fill = TRUE)
sink()

fledgdat <- read.csv("fledgdat.csv")
fledgdat <- nest.dat[nest.dat$SPEC%in%names(a.ny[a.ny > 8]),]
fledgdat$year <- as.numeric(fledgdat$YEAR)
tempres <- as.numeric(lm(clim$Tave_sp.AprJun[1:16] ~ clim$Jdate[1:16])$residuals)
clim.red <- data.frame(year = seq(1, 16, 1), 
                       snow = (clim$Jdate[1:16] - mean(clim$Jdate[1:16]))/sd(clim$Jdate[1:16]),
                       sp.temp.r = (tempres - mean(tempres))/sd(tempres),
                       sm.tmin = (clim$Tmin_sm.JunAug[1:16] - mean(clim$Tmin_sm.JunAug[1:16]))/sd(clim$Tmin_sm.JunAug[1:16]),
                       sm.ppt = (clim$PPT_sm.JunAug[1:16] - mean(clim$PPT_sm.JunAug[1:16]))/sd(clim$PPT_sm.JunAug[1:16]))
fledgdat <- merge(fledgdat, clim.red)

snow <- fledgdat$snow
sp.temp.r <- fledgdat$sp.temp.r
n <- nrow(fledgdat)
y <- fledgdat$jdate
spec <- as.numeric(factor(fledgdat$SPEC))
year <- fledgdat$year
nspec <- max(spec)

jags.data <- list(n = n, y = y, spec = spec, year = year, snow = snow, 
                  sp.temp.r = sp.temp.r, nspec = nspec)

# initial values
inits <- function(){
  list(mu.b0=runif(1, 150, 250), mu.b1 = runif(1, -1, 1),  mu.b2=runif(1, -1, 1), 
       sigma.b0=runif(1), sigma.b1=runif(1), sigma.b2=runif(1))  }

parms <- c("b0", "mu.b0", "b1", "mu.b1", "b2", "mu.b2", "sigma.b0", "sigma.b1", 
           "sigma.b2", "sigma", "bpvalue")

# run model
pheno.out <- jags(jags.data, inits, parms, "fledg_pheno.txt", n.thin=nt, 
                  n.chains = nc, n.adapt=na, n.burnin = nb, n.iter = ni, 
                  parallel = T)

# some species ests for sfd
dufl.lo <- pheno.out$sims.list$b0[,7] + pheno.out$sims.list$b1[,7]*min(fledgdat$snow, na.rm = T)
dufl.hi <- pheno.out$sims.list$b0[,7] + pheno.out$sims.list$b1[,7]*max(fledgdat$snow, na.rm = T)
cafi.lo <- pheno.out$sims.list$b0[,4] + pheno.out$sims.list$b1[,4]*min(fledgdat$snow, na.rm = T)
cafi.hi <- pheno.out$sims.list$b0[,4] + pheno.out$sims.list$b1[,4]*max(fledgdat$snow, na.rm = T)
moch.lo <- pheno.out$sims.list$b0[,12] + pheno.out$sims.list$b1[,12]*min(fledgdat$snow, na.rm = T)
moch.hi <- pheno.out$sims.list$b0[,12] + pheno.out$sims.list$b1[,12]*max(fledgdat$snow, na.rm = T)
wtsp.lo <- pheno.out$sims.list$b0[,21] + pheno.out$sims.list$b1[,21]*min(fledgdat$snow, na.rm = T)
wtsp.hi <- pheno.out$sims.list$b0[,21] + pheno.out$sims.list$b1[,21]*max(fledgdat$snow, na.rm = T)

#-------------------------------------------------------------------------------
# Fig. 5 - Fledging phenology - covariatie relationships
#-------------------------------------------------------------------------------

spec.mn.pheno <- aggregate(jdate ~ SPEC + snow + sp.temp.r, fledgdat, mean, na.rm = T)
mn.pheno <- aggregate(jdate ~ snow + sp.temp.r, fledgdat, mean, na.rm = T)

snow.new <- seq(min(fledgdat$snow, na.rm = T), max(fledgdat$snow, na.rm = T), 0.02)
pheno.snow.pred <- matrix(NA, nrow=length(pheno.out$sims.list$mu.b0), 
                          ncol=length(snow.new))

for (i in 1:dim(pheno.snow.pred)[1]){
  for (j in 1:dim(pheno.snow.pred)[2]){
    pheno.snow.pred[i,j] <- pheno.out$sims.list$mu.b0[i] + 
      pheno.out$sims.list$mu.b1[i]*snow.new[j]
  }
}

pheno.snow.mn <- colMeans(pheno.snow.pred)
pheno.snow.lcl <- apply(pheno.snow.pred, 2, quantile, probs = 0.025)
pheno.snow.ucl <- apply(pheno.snow.pred, 2, quantile, probs = 0.975)

temp.new <- seq(min(fledgdat$sp.temp.r), max(fledgdat$sp.temp.r), 0.02)
pheno.temp.pred <- matrix(NA, nrow=length(pheno.out$sims.list$mu.b0), 
                          ncol=length(temp.new))

for (i in 1:dim(pheno.temp.pred)[1]){
  for (j in 1:dim(pheno.temp.pred)[2]){
    pheno.temp.pred[i,j] <- pheno.out$sims.list$mu.b0[i] + 
      pheno.out$sims.list$mu.b2[i]*temp.new[j]
  }
}

pheno.temp.mn <- colMeans(pheno.temp.pred)
pheno.temp.lcl <- apply(pheno.temp.pred, 2, quantile, probs = 0.025)
pheno.temp.ucl <- apply(pheno.temp.pred, 2, quantile, probs = 0.975)


# species effects 
species.pheno.ef <- data.frame(
  SPEC=levels(factor(fledgdat$SPEC)),
  b1.mn = apply(pheno.out$sims.list$b1, 2, mean),
  b1.lcl = apply(pheno.out$sims.list$b1, 2, quantile, probs=0.025),
  b1.ucl = apply(pheno.out$sims.list$b1, 2, quantile, probs=0.975),
  b2.mn = apply(pheno.out$sims.list$b2, 2, mean),
  b2.lcl = apply(pheno.out$sims.list$b2, 2, quantile, probs=0.025),
  b2.ucl = apply(pheno.out$sims.list$b2, 2, quantile, probs=0.975)
)

tiff("figs/Fig5_fenofig.tif", height=5, width=6.5, res=600, units="in")

par(mar=c(4,5,1,0), mfrow = c(2,2))

plot(snow.new, rep(10, length(snow.new)), type = "n", ylim = c(180, 220), las=1, 
     xaxt="n", xlab="95% snow-free date", ylab="Mean fledging date", cex.lab=1, 
     cex.axis=1)
xlabs <- seq(130, 215, 10)
axis(1, at = (xlabs-mean(clim$Jdate[1:16]))/sd(clim$Jdate[1:16]), 
     labels=xlabs, cex.axis=1)

polygon(x=c(snow.new,  rev(snow.new)), 
        y=c(pheno.snow.lcl, 
            rev(pheno.snow.ucl)), col="gray90", border=NA)

lines(snow.new, pheno.snow.mn, col = "black", lty=1, lwd=2)
points(mn.pheno$snow, mn.pheno$jdate)
title(main = "A", line = -1.5, adj=0.05, cex.main = 1.5)


par(mar=c(4,3,1,3))
plotCI(x=species.pheno.ef$b1.mn[order(species.pheno.ef$b1.mn)], y=seq(1,nspec,1), 
       li=species.pheno.ef$b1.lcl[order(species.pheno.ef$b1.mn)],
       ui=species.pheno.ef$b1.ucl[order(species.pheno.ef$b1.mn)], err="x", 
       las=1, sfrac=0, yaxt="n", pch=NA, gap = 0.05, 
       xlab = expression(paste("95% snow-free-date coefficient ( ", hat(beta)[1], ")")),
       ylab = "")

text(x = species.pheno.ef$b1.mn[order(species.pheno.ef$b1.mn)],
     y=seq(1,nspec, 1), labels=species.pheno.ef$SPEC[order(species.pheno.ef$b1.mn)],
     cex=.6)
abline(v=0,lty=2)
title(main = "B", line = -1.5, adj=0.05, cex.main = 1.5)

par(mar=c(4,5,1,0))
plot(temp.new, rep(10, length(temp.new)), type = "n", ylim = c(180, 220), las=1, xaxt="n", 
     xlab="Spring temperature residuals", 
     ylab="Mean fledging date", cex.lab=1, cex.axis=1)
xlabs <- seq(-2.5, 1.25, .25)
axis(1, at = (xlabs-mean(tempres))/sd(tempres), labels=xlabs, cex.axis=1)

polygon(x=c(temp.new,  rev(temp.new)), 
        y=c(pheno.temp.lcl, 
            rev(pheno.temp.ucl)), col="gray90", border=NA)

lines(temp.new, pheno.temp.mn, col = "black", lty=1, lwd=2)
points(mn.pheno$sp.temp.r, mn.pheno$jdate)
title(main = "C", line = -1.5, adj=0.05, cex.main = 1.5)


par(mar=c(4,3,1,3))
plotCI(x=species.pheno.ef$b2.mn[order(species.pheno.ef$b2.mn)], y=seq(1,nspec,1), 
       li=species.pheno.ef$b2.lcl[order(species.pheno.ef$b2.mn)],
       ui=species.pheno.ef$b2.ucl[order(species.pheno.ef$b2.mn)], err="x", 
       las=1, sfrac=0, yaxt="n", pch=NA, gap = 0.05, 
       xlab = expression(paste("Spring temp. residuals coefficient ( ", hat(beta)[2], ")")),
       ylab = "")
text(x = species.pheno.ef$b2.mn[order(species.pheno.ef$b2.mn)],
     y=seq(1,nspec, 1), labels=species.pheno.ef$SPEC[order(species.pheno.ef$b2.mn)],
     cex=.6)
abline(v=0,lty=2)
title(main = "D", line = -1.5, adj=0.05, cex.main = 1.5)



dev.off()

#-------------------------------------------------------------------------------
# Productivity models
#-------------------------------------------------------------------------------

# Specify model in BUGS language
sink("productivity_zin.txt")
cat("

         model {
         

    ## Likelihood
    for(i in 1:n){
      y[i] ~ dnorm(mu.hacked[i], tau.hacked)T(0,)
      mu.hacked[i] <- mu[i]*(succ[i]) + 1e-10*(1-succ[i])
      log(mu[i]) <- b0[spec[i]] + b1[spec[i]]*x1[i] + b2[spec[i]]*x2[i] 
      
      ## Zero-Inflation
      succ[i] ~ dbern(pi[i])
      # initial run suggested no covariate relationships with zero prob. 
      logit(pi[i]) <- a0[spec[i]] + a1[spec[i]]*x1[i] + a2[spec[i]]*x2[i]
      
    residual[i] <- y[i] - mu.hacked[i] #residual for observed data
    predicted[i] <- mu.hacked[i] # predicted values
    sq[i] <- pow(residual[i], 2) # squared residuals for observed data
    
    # Generate replicate data and compute fit stats for them
    y.new[i] ~ dnorm(mu.hacked[i], tau.hacked)
    sq.new[i] <- pow(y.new[i] - predicted[i], 2)
    
    # create new data sets under model
    z.new[i] ~ dbern(pi[i])
    z.pred[i] <- pi[i] # expected count
    
    #  discrepancy for actual data
    chi2.actual[i] <- pow(succ[i] - z.pred[i], 2)/(z.pred[i] + 0.0001)
    chi2.new[i] <- pow(z.new[i] - z.pred[i], 2)/(z.pred[i] + 0.0001)
    } 
    
    fity <- sum(sq[])
    fit.new <- sum(sq.new[])
    testy <- step(fit.new - fity)
    bpvaluey <- mean(testy)
    
    fitz <- sum(chi2.actual[])           # test statistic for data   
    fit.simz <- sum(chi2.sim[])   # test statistic for new predicted data   
    c.hat <- fitz/fit.simz # c-hat estimate (overdispersion)
    testz <- step(fit.simz - fitz)   # Test whether new data set more extreme
    bpvaluez <- mean(testz)
    
    ## Priors
    mu.b0 <- log(mu.eb0)
    mu.eb0 ~ dunif(0,6) # range of observed values
    mu.b1 ~ dnorm(0, 0.1)
    mu.b2 ~ dnorm(0, 0.1)
    
    mu.a0 ~ dnorm(0, 0.1)
    mu.a1 ~ dnorm(0, 0.1)
    mu.a2 ~ dnorm(0, 0.1)
    
    tau.hacked <- pow(sigma.hacked, -2)
    tau.b0 <- pow(sigma.b0, -2)
    tau.b1 <- pow(sigma.b1, -2)
    tau.b2 <- pow(sigma.b2, -2)
    
    tau.a0 <- pow(sigma.a0, -2)
    tau.a1 <- pow(sigma.a1, -2)
    tau.a2 <- pow(sigma.a2, -2) 
    
    sigma.hacked ~ dunif(0,10)
    sigma.b0 ~ dunif(0, 10)
    sigma.b1 ~ dunif(0, 10)
    sigma.b2 ~ dunif(0, 10)
    
    sigma.a0 ~ dunif(0, 10)
    sigma.a1 ~ dunif(0, 10)
    sigma.a2 ~ dunif(0, 10)
    
    for (s in 1:nspec){
    b0[s] ~ dnorm(mu.b0, tau.b0)
    b1[s] ~ dnorm(mu.b1, tau.b1)
    b2[s] ~ dnorm(mu.b2, tau.b2)
    
    
    a0[s] ~ dnorm(mu.a0, tau.a0)
    a1[s] ~ dnorm(mu.a1, tau.a1)
    a2[s] ~ dnorm(mu.a2, tau.a2)
    }
    
    }
         
         ",fill = TRUE)
sink()

#-------------------------------------------------------------------------------
# Productivity - spring covariate model
#-------------------------------------------------------------------------------

n <- nrow(fledgdat)
y <- fledgdat$NUMFLEDGE
spec <- as.numeric(factor(fledgdat$SPEC))
year <- fledgdat$year
nspec <- max(spec)

jags.data <- list(n = n, y = y, spec = spec, year = year, x1 = fledgdat$snow, 
                  x2 = fledgdat$sp.temp.r, nspec = nspec, succ=fledgdat$succ)


# initial values
inits <- function(){
  list(mu.eb0=runif(1, 2, 4), mu.b1 = runif(1, -1, 0),  mu.b2=runif(1, 0, 1), sigma.b1=runif(1),
       sigma.b2=runif(1), mu.a0=rnorm(1), mu.a1 = runif(1, -1, 0),  mu.a2=runif(1, 0, 1),
       sigma.a1=runif(1), sigma.a2=runif(1), sigma.hacked=runif(1))  }

parms <- c("b0", "mu.b0", "b1", "mu.b1", "b2", "mu.b2", "sigma.b0", "sigma.b1", "sigma.b2",
           "a0", "mu.a0", "a1", "mu.a1", "a2", "mu.a2", "sigma.a0", "sigma.a1", "sigma.a2",
           "sigma.hacked", "bpvaluey", "bpvaluez", "c.hat")


# run model
prod.out1 <- jags(jags.data, inits, parms, "productivity_zin.txt", n.thin=nt, 
                  n.chains = nc, n.adapt=na, n.burnin = nb, n.iter = ni, 
                  parallel = T)

spec.mn.psucc <- aggregate(success ~ SPEC + snow + sp.temp.r + sm.tmin + sm.ppt, fledgdat, sum)
x <- aggregate(success ~ SPEC + snow + sp.temp.r + sm.tmin + sm.ppt, fledgdat, length)
names(x)[6] <- "n"
spec.mn.psucc <- merge(spec.mn.psucc, x)
spec.mn.psucc$psucc <- spec.mn.psucc$success/spec.mn.psucc$n


mn.psucc <- aggregate(success ~ snow + sp.temp.r + sm.tmin + sm.ppt, fledgdat, sum)
x <- aggregate(success ~ snow + sp.temp.r + sm.tmin + sm.ppt, fledgdat, length)
names(x)[5] <- "n"
mn.psucc <- merge(mn.psucc, x)
mn.psucc$psucc <- mn.psucc$success/mn.psucc$n

#-------------------------------------------------------------------------------
# Fig. 6 - fledging success-spring covariate relationships
#-------------------------------------------------------------------------------

snow.new <- seq(min(fledgdat$snow), max(fledgdat$snow), 0.02)
psucc.snow.pred <- matrix(NA, nrow=length(prod.out1$sims.list$mu.a0), 
                          ncol=length(snow.new))

expit <- function(x){
  exp(x)/(exp(x)+1)
}

for (i in 1:dim(psucc.snow.pred)[1]){
  for (j in 1:dim(psucc.snow.pred)[2]){
    psucc.snow.pred[i,j] <- expit(prod.out1$sims.list$mu.a0[i] + prod.out1$sims.list$mu.a1[i]*snow.new[j])
  }
}

psucc.snow.mn <- colMeans(psucc.snow.pred)
psucc.snow.lcl <- apply(psucc.snow.pred, 2, quantile, probs = 0.025)
psucc.snow.ucl <- apply(psucc.snow.pred, 2, quantile, probs = 0.975)


temp.new <- seq(min(fledgdat$sp.temp.r), max(fledgdat$sp.temp.r), 0.02)
psucc.temp.pred <- matrix(NA, nrow=length(prod.out1$sims.list$mu.a0), 
                          ncol=length(temp.new))

for (i in 1:dim(psucc.temp.pred)[1]){
  for (j in 1:dim(psucc.temp.pred)[2]){
    psucc.temp.pred[i,j] <- expit(prod.out1$sims.list$mu.a0[i] + prod.out1$sims.list$mu.a2[i]*temp.new[j])
  }
}

psucc.temp.mn <- colMeans(psucc.temp.pred)
psucc.temp.lcl <- apply(psucc.temp.pred, 2, quantile, probs = 0.025)
psucc.temp.ucl <- apply(psucc.temp.pred, 2, quantile, probs = 0.975)

# species effects 
species.psucc.ef <- data.frame(
  SPEC=levels(factor(fledgdat$SPEC)),
  a1.mn = apply(prod.out1$sims.list$a1, 2, mean),
  a1.lcl = apply(prod.out1$sims.list$a1, 2, quantile, probs=0.025),
  a1.ucl = apply(prod.out1$sims.list$a1, 2, quantile, probs=0.975),
  a2.mn = apply(prod.out1$sims.list$a2, 2, mean),
  a2.lcl = apply(prod.out1$sims.list$a2, 2, quantile, probs=0.025),
  a2.ucl = apply(prod.out1$sims.list$a2, 2, quantile, probs=0.975)
)


tiff("figs/Fig6_spring_psucc.tif", height=5, width=6.5, res=600, units="in")
require(plotrix)
par(mar=c(4,5,1,0), mfrow = c(2,2))

plot(snow.new, rep(.5, length(snow.new)), type = "n", ylim = c(0.4, 1), las=1, xaxt="n", 
     xlab="95% snow-free date", 
     ylab="Fledging success probability", cex.lab=1, cex.axis=1)
xlabs <- seq(130, 215, 10)
axis(1, at = (xlabs-mean(clim$Jdate[1:16]))/sd(clim$Jdate[1:16]), labels=xlabs, cex.axis=1)

polygon(x=c(snow.new,  rev(snow.new)), 
        y=c(psucc.snow.lcl, 
            rev(psucc.snow.ucl)), col="gray90", border=NA)

lines(snow.new, psucc.snow.mn, col = "black", lty=1, lwd=2)
points(mn.psucc$snow, mn.psucc$psucc)
title(main = "A", line = -1.5, adj=0.05, cex.main = 1.5)

par(mar=c(4,3,1,3))
plotCI(x=species.psucc.ef$a1.mn[order(species.psucc.ef$a1.mn)], y=seq(1,nspec,1), 
       li=species.psucc.ef$a1.lcl[order(species.psucc.ef$a1.mn)],
       ui=species.psucc.ef$a1.ucl[order(species.psucc.ef$a1.mn)], err="x", 
       las=1, sfrac=0, yaxt="n", pch=NA, gap = 0.05, 
       xlab = expression(paste("95% snow-free-date coefficient ( ", hat(beta)[1], ")")),
       ylab = "")

text(x = species.psucc.ef$a1.mn[order(species.psucc.ef$a1.mn)],
     y=seq(1,nspec, 1), labels=species.psucc.ef$SPEC[order(species.psucc.ef$a1.mn)],
     cex=.6)
abline(v=0,lty=2)
title(main = "B", line = -1.5, adj=0.05, cex.main = 1.5)

par(mar=c(4,5,1,0))
plot(temp.new, rep(.5, length(temp.new)), type = "n", ylim = c(.4, 1), las=1, xaxt="n", 
     xlab=expression(paste("Spring temperature residuals (", degree, "C)", sep="")), 
     ylab="Fledging success probability", cex.lab=1, cex.axis=1)
xlabs <- seq(-2.5, 1.25, .25)
axis(1, at = (xlabs-mean(tempres))/sd(tempres), labels=xlabs, cex.axis=1)

polygon(x=c(temp.new,  rev(temp.new)), 
        y=c(psucc.temp.lcl, 
            rev(psucc.temp.ucl)), col="gray90", border=NA)

lines(temp.new, psucc.temp.mn, col = "black", lty=1, lwd=2)
points(mn.psucc$sp.temp.r, mn.psucc$psucc)
title(main = "C", line = -1.5, adj=0.05, cex.main = 1.5)

par(mar=c(4,3,1,3))
plotCI(x=species.psucc.ef$a2.mn[order(species.psucc.ef$a2.mn)], y=seq(1,nspec,1), 
       li=species.psucc.ef$a2.lcl[order(species.psucc.ef$a2.mn)],
       ui=species.psucc.ef$a2.ucl[order(species.psucc.ef$a2.mn)], err="x", 
       las=1, sfrac=0, yaxt="n", pch=NA, gap = 0.05, 
       xlab = expression(paste("Spring temp. residuals coefficient ( ", hat(beta)[2], ")")),
       ylab = "")
text(x = species.psucc.ef$a2.mn[order(species.psucc.ef$a2.mn)],
     y=seq(1,nspec, 1), labels=species.psucc.ef$SPEC[order(species.psucc.ef$a2.mn)],
     cex=.6)
abline(v=0,lty=2)
title(main = "D", line = -1.5, adj=0.05, cex.main = 1.5)

dev.off()

#-------------------------------------------------------------------------------
# Fig. 7 - number of fledglings - spring covariate relationships
#-------------------------------------------------------------------------------

nfledg.sum <- aggregate(NUMFLEDGE ~ snow + sp.temp.r + sm.tmin + sm.ppt, 
                        fledgdat[fledgdat$success==1,], mean)


snow.new <- seq(min(fledgdat$snow), max(fledgdat$snow), 0.02)
nfledg.snow.pred <- matrix(NA, nrow=length(prod.out1$sims.list$mu.b0), 
                           ncol=length(snow.new))

for (i in 1:dim(nfledg.snow.pred)[1]){
  for (j in 1:dim(nfledg.snow.pred)[2]){
    nfledg.snow.pred[i,j] <- exp(prod.out1$sims.list$mu.b0[i] + 
                                   prod.out1$sims.list$mu.b1[i]*snow.new[j])
  }
}

nfledg.snow.mn <- colMeans(nfledg.snow.pred)
nfledg.snow.lcl <- apply(nfledg.snow.pred, 2, quantile, probs = 0.025)
nfledg.snow.ucl <- apply(nfledg.snow.pred, 2, quantile, probs = 0.975)

temp.new <- seq(min(fledgdat$sp.temp.r), max(fledgdat$sp.temp.r), 0.02)
nfledg.temp.pred <- matrix(NA, nrow=length(prod.out1$sims.list$mu.b0), 
                           ncol=length(temp.new))

for (i in 1:dim(nfledg.temp.pred)[1]){
  for (j in 1:dim(nfledg.temp.pred)[2]){
    nfledg.temp.pred[i,j] <- exp(prod.out1$sims.list$mu.b0[i] + prod.out1$sims.list$mu.b2[i]*temp.new[j])
  }
}

nfledg.temp.mn <- colMeans(nfledg.temp.pred)
nfledg.temp.lcl <- apply(nfledg.temp.pred, 2, quantile, probs = 0.025)
nfledg.temp.ucl <- apply(nfledg.temp.pred, 2, quantile, probs = 0.975)

# species effects 
species.nfledg1.ef <- data.frame(
  SPEC=levels(factor(fledgdat$SPEC)),
  b1.mn = apply(prod.out1$sims.list$b1, 2, mean),
  b1.lcl = apply(prod.out1$sims.list$b1, 2, quantile, probs=0.025),
  b1.ucl = apply(prod.out1$sims.list$b1, 2, quantile, probs=0.975),
  b2.mn = apply(prod.out1$sims.list$b2, 2, mean),
  b2.lcl = apply(prod.out1$sims.list$b2, 2, quantile, probs=0.025),
  b2.ucl = apply(prod.out1$sims.list$b2, 2, quantile, probs=0.975)
)


tiff("figs/Fig7_spring_nfledg.tif", height=5, width=6.5, res=600, units="in")
require(plotrix)
par(mar=c(4,5,1,0), mfrow = c(2,2))

plot(snow.new, rep(.5, length(snow.new)), type = "n", ylim = c(1.5,3.5), las=1, xaxt="n", 
     xlab="95% snow-free date", 
     ylab="Number of fledglings", cex.lab=1, cex.axis=1)
xlabs <- seq(130, 215, 10)
axis(1, at = (xlabs-mean(clim$Jdate[1:16]))/sd(clim$Jdate[1:16]), labels=xlabs, cex.axis=1)

polygon(x=c(snow.new,  rev(snow.new)), 
        y=c(nfledg.snow.lcl, 
            rev(nfledg.snow.ucl)), col="gray90", border=NA)

lines(snow.new, nfledg.snow.mn, col = "black", lty=1, lwd=2)
points(nfledg.sum$snow, nfledg.sum$NUMFLEDGE)
title(main = "A", line = -1.5, adj=0.05, cex.main = 1.5)

par(mar=c(4,3,1,3))
plotCI(x=species.nfledg1.ef$b1.mn[order(species.nfledg1.ef$b1.mn)], y=seq(1,nspec,1), 
       li=species.nfledg1.ef$b1.lcl[order(species.nfledg1.ef$b1.mn)],
       ui=species.nfledg1.ef$b1.ucl[order(species.nfledg1.ef$b1.mn)], err="x", 
       las=1, sfrac=0, yaxt="n", pch=NA, gap = 0.05, 
       xlab = expression(paste("95% snow-free-date coefficient ( ", hat(beta)[1], ")")),
       ylab = "")

text(x = species.nfledg1.ef$b1.mn[order(species.nfledg1.ef$b1.mn)],
     y=seq(1,nspec, 1), labels=species.nfledg1.ef$SPEC[order(species.nfledg1.ef$b1.mn)],
     cex=.6)
abline(v=0,lty=2)
title(main = "B", line = -1.5, adj=0.05, cex.main = 1.5)

par(mar=c(4,5,1,0))
plot(temp.new, rep(.5, length(temp.new)), type = "n", ylim = c(1.5, 3.5), las=1, xaxt="n", 
     xlab="Spring temperature residuals", 
     ylab="Number of fledglings", cex.lab=1, cex.axis=1)
xlabs <- seq(-2.5, 1.25, .25)
axis(1, at = (xlabs-mean(tempres))/sd(tempres), labels=xlabs, cex.axis=1)

polygon(x=c(temp.new,  rev(temp.new)), 
        y=c(nfledg.temp.lcl, 
            rev(nfledg.temp.ucl)), col="gray90", border=NA)

lines(temp.new, nfledg.temp.mn, col = "black", lty=1, lwd=2)
points(nfledg.sum$sp.temp.r, nfledg.sum$NUMFLEDGE)
title(main = "C", line = -1.5, adj=0.05, cex.main = 1.5)

par(mar=c(4,3,1,3))
plotCI(x=species.nfledg1.ef$b2.mn[order(species.nfledg1.ef$b2.mn)], y=seq(1,nspec,1), 
       li=species.nfledg1.ef$b2.lcl[order(species.nfledg1.ef$b2.mn)],
       ui=species.nfledg1.ef$b2.ucl[order(species.nfledg1.ef$b2.mn)], err="x", 
       las=1, sfrac=0, yaxt="n", pch=NA, gap = 0.05, 
       xlab = expression(paste("Spring temp. residuals coefficient ( ", hat(beta)[2], ")")),
       ylab = "")
text(x = species.nfledg1.ef$b2.mn[order(species.nfledg1.ef$b2.mn)],
     y=seq(1,nspec, 1), labels=species.nfledg1.ef$SPEC[order(species.nfledg1.ef$b2.mn)],
     cex=.6)
abline(v=0,lty=2)
title(main = "D", line = -1.5, adj=0.05, cex.main = 1.5)

dev.off()

#-------------------------------------------------------------------------------
# Productivity - summer covariate model 
#-------------------------------------------------------------------------------

n <- nrow(fledgdat)
y <- fledgdat$NUMFLEDGE
fledgdat$year <- as.numeric(fledgdat$YEAR)
spec <- as.numeric(factor(fledgdat$SPEC))
year <- fledgdat$year
nspec <- max(spec)

jags.data <- list(n = n, y = y, spec = spec, year = year, x1 = fledgdat$sm.tmin, 
                  x2 = fledgdat$sm.ppt, nspec = nspec, succ=fledgdat$succ)


# initial values
inits <- function(){
  list(mu.eb0=runif(1, 2, 4), mu.b1 = runif(1, -1, 0),  mu.b2=runif(1, 0, 1), sigma.b1=runif(1),
       sigma.b2=runif(1), mu.a0=rnorm(1), mu.a1 = runif(1, -1, 0),  mu.a2=runif(1, 0, 1),
       sigma.a1=runif(1), sigma.a2=runif(1), sigma.hacked=runif(1))  }


parms <- c("b0", "mu.b0", "b1", "mu.b1", "b2", "mu.b2", "sigma.b0", "sigma.b1", "sigma.b2",
           "a0", "mu.a0", "a1", "mu.a1", "a2", "mu.a2", "sigma.a0", "sigma.a1", "sigma.a2",
           "sigma.hacked", "bpvaluey", "bpvaluez", "c.hat")


# run model
prod.out2 <- jags(jags.data, inits, parms, "productivity_zin.txt", n.thin=nt, 
                  n.chains = nc, n.adapt=na, n.burnin = nb, n.iter = ni, 
                  parallel = T)

sm.tmin.new <- seq(min(fledgdat$sm.tmin), max(fledgdat$sm.tmin), 0.02)
psucc.sm.tmin.pred <- matrix(NA, nrow=length(prod.out2$sims.list$mu.a0), 
                             ncol=length(sm.tmin.new))


for (i in 1:dim(psucc.sm.tmin.pred)[1]){
  for (j in 1:dim(psucc.sm.tmin.pred)[2]){
    psucc.sm.tmin.pred[i,j] <- expit(prod.out2$sims.list$mu.a0[i] + prod.out2$sims.list$mu.a1[i]*sm.tmin.new[j])
  }
}

psucc.sm.tmin.mn <- colMeans(psucc.sm.tmin.pred)
psucc.sm.tmin.lcl <- apply(psucc.sm.tmin.pred, 2, quantile, probs = 0.025)
psucc.sm.tmin.ucl <- apply(psucc.sm.tmin.pred, 2, quantile, probs = 0.975)

sm.ppt.new <- seq(min(fledgdat$sm.ppt), max(fledgdat$sm.ppt), 0.02)
psucc.sm.ppt.pred <- matrix(NA, nrow=length(prod.out2$sims.list$mu.a0), 
                            ncol=length(sm.ppt.new))

for (i in 1:dim(psucc.sm.ppt.pred)[1]){
  for (j in 1:dim(psucc.sm.ppt.pred)[2]){
    psucc.sm.ppt.pred[i,j] <- expit(prod.out2$sims.list$mu.a0[i] + prod.out2$sims.list$mu.a2[i]*sm.ppt.new[j])
  }
}

psucc.sm.ppt.mn <- colMeans(psucc.sm.ppt.pred)
psucc.sm.ppt.lcl <- apply(psucc.sm.ppt.pred, 2, quantile, probs = 0.025)
psucc.sm.ppt.ucl <- apply(psucc.sm.ppt.pred, 2, quantile, probs = 0.975)

# species effects 
species.psucc2.ef <- data.frame(
  SPEC=levels(factor(fledgdat$SPEC)),
  a1.mn = apply(prod.out2$sims.list$a1, 2, mean),
  a1.lcl = apply(prod.out2$sims.list$a1, 2, quantile, probs=0.025),
  a1.ucl = apply(prod.out2$sims.list$a1, 2, quantile, probs=0.975),
  a2.mn = apply(prod.out2$sims.list$a2, 2, mean),
  a2.lcl = apply(prod.out2$sims.list$a2, 2, quantile, probs=0.025),
  a2.ucl = apply(prod.out2$sims.list$a2, 2, quantile, probs=0.975)
)

#-------------------------------------------------------------------------------
# Fig. 8 - fledging success-summer covariate relationships
#-------------------------------------------------------------------------------

tiff("figs/Fig8_summer_psucc.tif", height=5, width=6.5, res=600, units="in")
par(mar=c(4,5,1,0), mfrow = c(2,2))

plot(sm.tmin.new, rep(.5, length(sm.tmin.new)), type = "n", ylim = c(0.4, 1), las=1, xaxt="n", 
     xlab=expression(paste("Mean summer min. temperature (", degree, "C)", sep="")), 
     ylab="Fledging success probability", cex.lab=1, cex.axis=1)
xlabs <- seq(3.25, 5, .25)
axis(1, at = (xlabs-mean(clim$Tmin_sm.JunAug[1:16]))/sd(clim$Tmin_sm.JunAug[1:16]), labels=xlabs, cex.axis=1)

polygon(x=c(sm.tmin.new,  rev(sm.tmin.new)), 
        y=c(psucc.sm.tmin.lcl, 
            rev(psucc.sm.tmin.ucl)), col="gray90", border=NA)

lines(sm.tmin.new, psucc.sm.tmin.mn, col = "black", lty=1, lwd=2)
points(mn.psucc$sm.tmin, mn.psucc$psucc)
title(main = "A", line = -1.5, adj=0.05, cex.main = 1.5)


par(mar=c(4,3,1,3))
plotCI(x=species.psucc2.ef$a1.mn[order(species.psucc2.ef$a1.mn)], y=seq(1,nspec,1), 
       li=species.psucc2.ef$a1.lcl[order(species.psucc2.ef$a1.mn)],
       ui=species.psucc2.ef$a1.ucl[order(species.psucc2.ef$a1.mn)], err="x", 
       las=1, sfrac=0, yaxt="n", pch=NA, gap = 0.05, 
       xlab = expression(paste("Summer temperature coefficient ( ", hat(beta)[1], ")")),
       ylab = "")

text(x = species.psucc2.ef$a1.mn[order(species.psucc2.ef$a1.mn)],
     y=seq(1,nspec, 1), labels=species.psucc2.ef$SPEC[order(species.psucc2.ef$a1.mn)],
     cex=.6)
abline(v=0,lty=2)
title(main = "B", line = -1.5, adj=0.05, cex.main = 1.5)

par(mar=c(4,5,1,0))
plot(sm.ppt.new, rep(.5, length(sm.ppt.new)), type = "n", ylim = c(.4, 1), las=1, xaxt="n", 
     xlab="Summer precipitation (mm)", 
     ylab="Fledging success probability", cex.lab=1, cex.axis=1)
xlabs <- seq(10, 110, 20)
axis(1, at = (xlabs-mean(clim$PPT_sm.JunAug[1:16]))/sd(clim$PPT_sm.JunAug[1:16]), labels=xlabs, cex.axis=1)

polygon(x=c(sm.ppt.new,  rev(sm.ppt.new)), 
        y=c(psucc.sm.ppt.lcl, 
            rev(psucc.sm.ppt.ucl)), col="gray90", border=NA)

lines(sm.ppt.new, psucc.sm.ppt.mn, col = "black", lty=1, lwd=2)
points(mn.psucc$sm.ppt, mn.psucc$psucc)
title(main = "C", line = -1.5, adj=0.05, cex.main = 1.5)


par(mar=c(4,3,1,3))
plotCI(x=species.psucc2.ef$a2.mn[order(species.psucc2.ef$a2.mn)], y=seq(1,nspec,1), 
       li=species.psucc2.ef$a2.lcl[order(species.psucc2.ef$a2.mn)],
       ui=species.psucc2.ef$a2.ucl[order(species.psucc2.ef$a2.mn)], err="x", 
       las=1, sfrac=0, yaxt="n", pch=NA, gap = 0.05, 
       xlab = expression(paste("Summer precipitation coefficient ( ", hat(beta)[2], ")")),
       ylab = "")
text(x = species.psucc2.ef$a2.mn[order(species.psucc2.ef$a2.mn)],
     y=seq(1,nspec, 1), labels=species.psucc2.ef$SPEC[order(species.psucc2.ef$a2.mn)],
     cex=.6)
abline(v=0,lty=2)
title(main = "D", line = -1.5, adj=0.05, cex.main = 1.5)



dev.off()

#-------------------------------------------------------------------------------
# Fig. 9 - number of fledglings-summer covariate relationships
#-------------------------------------------------------------------------------


nfledg.sm.tmin.pred <- matrix(NA, nrow=length(prod.out2$sims.list$mu.b0), 
                              ncol=length(sm.tmin.new))

for (i in 1:dim(nfledg.sm.tmin.pred)[1]){
  for (j in 1:dim(nfledg.sm.tmin.pred)[2]){
    nfledg.sm.tmin.pred[i,j] <- exp(prod.out2$sims.list$mu.b0[i] + prod.out2$sims.list$mu.b1[i]*sm.tmin.new[j])
  }
}

nfledg.sm.tmin.mn <- colMeans(nfledg.sm.tmin.pred)
nfledg.sm.tmin.lcl <- apply(nfledg.sm.tmin.pred, 2, quantile, probs = 0.025)
nfledg.sm.tmin.ucl <- apply(nfledg.sm.tmin.pred, 2, quantile, probs = 0.975)


sm.ppt.new <- seq(min(fledgdat$sm.ppt), max(fledgdat$sm.ppt), 0.02)
nfledg.sm.ppt.pred <- matrix(NA, nrow=length(prod.out2$sims.list$mu.b0), 
                             ncol=length(sm.ppt.new))

for (i in 1:dim(nfledg.sm.ppt.pred)[1]){
  for (j in 1:dim(nfledg.sm.ppt.pred)[2]){
    nfledg.sm.ppt.pred[i,j] <- exp(prod.out2$sims.list$mu.b0[i] + prod.out2$sims.list$mu.b2[i]*sm.ppt.new[j])
  }
}

nfledg.sm.ppt.mn <- colMeans(nfledg.sm.ppt.pred)
nfledg.sm.ppt.lcl <- apply(nfledg.sm.ppt.pred, 2, quantile, probs = 0.025)
nfledg.sm.ppt.ucl <- apply(nfledg.sm.ppt.pred, 2, quantile, probs = 0.975)



# species effects 
species.nfledg2.ef <- data.frame(
  SPEC=levels(factor(fledgdat$SPEC)),
  b1.mn = apply(prod.out2$sims.list$b1, 2, mean),
  b1.lcl = apply(prod.out2$sims.list$b1, 2, quantile, probs=0.025),
  b1.ucl = apply(prod.out2$sims.list$b1, 2, quantile, probs=0.975),
  b2.mn = apply(prod.out2$sims.list$b2, 2, mean),
  b2.lcl = apply(prod.out2$sims.list$b2, 2, quantile, probs=0.025),
  b2.ucl = apply(prod.out2$sims.list$b2, 2, quantile, probs=0.975)
)


tiff("figs/Fig9_summer_nfledg.tif", height=5, width=6.5, res=600, units="in")
par(mar=c(4,5,1,0), mfrow = c(2,2))

plot(sm.tmin.new, rep(.5, length(sm.tmin.new)), type = "n", ylim = c(1.5, 3.5), las=1, xaxt="n", 
     xlab=expression(paste("Mean summer min. temperature (", degree, "C)", sep="")),
     ylab="Number of fledglings", cex.lab=1, cex.axis=1)
xlabs <- seq(3.25, 5, .25)
axis(1, at = (xlabs-mean(clim$Tmin_sm.JunAug[1:16]))/sd(clim$Tmin_sm.JunAug[1:16]), labels=xlabs, cex.axis=1)

polygon(x=c(sm.tmin.new,  rev(sm.tmin.new)), 
        y=c(nfledg.sm.tmin.lcl, 
            rev(nfledg.sm.tmin.ucl)), col="gray90", border=NA)

lines(sm.tmin.new, nfledg.sm.tmin.mn, col = "black", lty=1, lwd=2)
points(nfledg.sum$sm.tmin, nfledg.sum$NUMFLEDGE)
title(main = "A", line = -1.5, adj=0.05, cex.main = 1.5)


par(mar=c(4,3,1,3))
plotCI(x=species.nfledg2.ef$b1.mn[order(species.nfledg2.ef$b1.mn)], y=seq(1,nspec,1), 
       li=species.nfledg2.ef$b1.lcl[order(species.nfledg2.ef$b1.mn)],
       ui=species.nfledg2.ef$b1.ucl[order(species.nfledg2.ef$b1.mn)], err="x", 
       las=1, sfrac=0, yaxt="n", pch=NA, gap = 0.05, 
       xlab = expression(paste("Summer temperature coefficient ( ", hat(beta)[1], ")")),
       ylab = "")

text(x = species.nfledg2.ef$b1.mn[order(species.nfledg2.ef$b1.mn)],
     y=seq(1,nspec, 1), labels=species.nfledg2.ef$SPEC[order(species.nfledg2.ef$b1.mn)],
     cex=.6)
abline(v=0,lty=2)
title(main = "B", line = -1.5, adj=0.05, cex.main = 1.5)

par(mar=c(4,5,1,0))
plot(sm.ppt.new, rep(.5, length(sm.ppt.new)), type = "n", ylim = c(1.5, 3.5), las=1, xaxt="n", 
     xlab="Summer precipitation (mm)", 
     ylab="Number of fledglings", cex.lab=1, cex.axis=1)
xlabs <- seq(10, 110, 20)
axis(1, at = (xlabs-mean(clim$PPT_sm.JunAug[1:16]))/sd(clim$PPT_sm.JunAug[1:16]), labels=xlabs, cex.axis=1)

polygon(x=c(sm.ppt.new,  rev(sm.ppt.new)), 
        y=c(nfledg.sm.ppt.lcl, 
            rev(nfledg.sm.ppt.ucl)), col="gray90", border=NA)

lines(sm.ppt.new, nfledg.sm.ppt.mn, col = "black", lty=1, lwd=2)
points(nfledg.sum$sm.ppt, nfledg.sum$NUMFLEDGE)
title(main = "C", line = -1.5, adj=0.05, cex.main = 1.5)
par(mar=c(4,3,1,3))
plotCI(x=species.nfledg2.ef$b2.mn[order(species.nfledg2.ef$b2.mn)], y=seq(1,nspec,1), 
       li=species.nfledg2.ef$b2.lcl[order(species.nfledg2.ef$b2.mn)],
       ui=species.nfledg2.ef$b2.ucl[order(species.nfledg2.ef$b2.mn)], err="x", 
       las=1, sfrac=0, yaxt="n", pch=NA, gap = 0.05, 
       xlab = expression(paste("Summer precipitation coefficient ( ", hat(beta)[2], ")")),
       ylab = "")
text(x = species.nfledg2.ef$b2.mn[order(species.nfledg2.ef$b2.mn)],
     y=seq(1,nspec, 1), labels=species.nfledg2.ef$SPEC[order(species.nfledg2.ef$b2.mn)],
     cex=.6)
abline(v=0,lty=2)
title(main = "D", line = -1.5, adj=0.05, cex.main = 1.5)
dev.off()

#-------------------------------------------------------------------------------
# supplementary CLNU analysis 
library(jagsUI)
#clark's nutcracker

y <- bm$CLNU[2:nrow(bm)]
x <- clim$Jdate[1:(nrow(clim)-1)]
x <- (x - mean(x))/sd(x)
nyears <- 31

sink("pois_reg.txt")
cat("

         model {

         b0 ~ dnorm(0, 0.01)
         b1 ~ dnorm(0, 0.01)
         
         for (t in 1:nyears){
         y[t] ~ dpois(lambda[t])
         
         log(lambda[t]) <- b0 + b1*x[t]  
         
         # goodness-of-fit stats
         y.sim[t] ~ dpois(lambda[t])
         err[t] <- pow(y[t]-lambda[t],2)/lambda[t]
         ferr[t] <- pow(y.sim[t]-lambda[t],2)/lambda[t]
         }
         
         gof <- sum(err[])
         fgof <- sum(ferr[])
         diffgof <- gof-fgof
         posdiff <- step(diffgof)
         
         }
         
         ",fill = TRUE)
sink()

# McMC settings
ni <- 100000
nb <- 40000
na <- 80000
nt <- 4
nc <- 4

jags.data <- list(y = y, nyears=nyears, x=x)  

inits <- function(){
  list(b0=rnorm(1), b1=rnorm(1)) }
# 
parms <- c("b0", "b1", "bpvalue", "sigma")

clnu_lag.out <- jags(jags.data, inits, parms, "pois_reg.txt", n.thin=nt, 
                     n.chains = nc, n.adapt=na, n.burnin = nb, n.iter = ni, 
                     parallel = T)

y <- bm$CLNU
x <- clim$Jdate
x <- (x - mean(x))/sd(x)
nyears <- 32

jags.data <- list(y = y, nyears=nyears, x=x)  

clnu.out <- jags(jags.data, inits, parms, "pois_reg.txt", n.thin=nt, 
                 n.chains = nc, n.adapt=na, n.burnin = nb, n.iter = ni, 
                 parallel = T)


snow.new <- seq(min(x), max(x), 0.02)
y.pred <- y.pred.lag <- matrix(NA, nrow=length(clnu.out$sims.list$b0), 
                               ncol=length(snow.new))


for (i in 1:dim(y.pred)[1]){
  for (j in 1:dim(y.pred)[2]){
    y.pred[i,j] <- exp(clnu.out$sims.list$b0[i] + clnu.out$sims.list$b1[i]*snow.new[j])
    y.pred.lag[i,j] <- exp(clnu_lag.out$sims.list$b0[i] + clnu_lag.out$sims.list$b1[i]*snow.new[j])
  }
}

clnu.snow.mn <- colMeans(y.pred)
clnu.snow.lcl <- apply(y.pred, 2, quantile, probs = 0.025)
clnu.snow.ucl <- apply(y.pred, 2, quantile, probs = 0.975)


clnu.snowlag.mn <- colMeans(y.pred.lag)
clnu.snowlag.lcl <- apply(y.pred.lag, 2, quantile, probs = 0.025)
clnu.snowlag.ucl <- apply(y.pred.lag, 2, quantile, probs = 0.975)


y1 <- bm$CLNU[2:nrow(bm)]
x1 <- clim$Jdate[1:(nrow(clim)-1)]
x1 <- (x1 - mean(x1))/sd(x1)

png("figs/clnu.png", height=2000, width=2000, res=300, units="px")
par(mfrow=c(2,1))
par(mar=c(4,5,1,1))
plot(snow.new, rep(10, length(snow.new)), type = "n", ylim = c(0, 10), las=1, xaxt="n", 
     xlab="95% snow-free date", 
     ylab="Number of territories", cex.lab=1, cex.axis=1)
xlabs <- seq(130, 215, 10)
axis(1, at = (xlabs-mean(clim$Jdate))/sd(clim$Jdate), labels=xlabs, cex.axis=1)

polygon(x=c(snow.new,  rev(snow.new)), 
        y=c(clnu.snow.lcl, 
            rev(clnu.snow.ucl)), col="gray90", border=NA)

lines(snow.new, clnu.snow.mn, col = "black", lty=1, lwd=2)
points(x, y)

title(main = "A", line = -1.5, adj=0.05, cex.main = 1.5)


plot(snow.new, rep(10, length(snow.new)), type = "n", ylim = c(0, 10), las=1, xaxt="n", 
     xlab="95% snow-free date", 
     ylab="Number of territories", cex.lab=1, cex.axis=1)
xlabs <- seq(130, 215, 10)
axis(1, at = (xlabs-mean(clim$Jdate))/sd(clim$Jdate), labels=xlabs, cex.axis=1)

polygon(x=c(snow.new,  rev(snow.new)), 
        y=c(clnu.snowlag.lcl, 
            rev(clnu.snowlag.ucl)), col="gray90", border=NA)

lines(snow.new, clnu.snowlag.mn, col = "black", lty=1, lwd=2)
points(x1, y1)

title(main = "B", line = -1.5, adj=0.05, cex.main = 1.5)
dev.off()





