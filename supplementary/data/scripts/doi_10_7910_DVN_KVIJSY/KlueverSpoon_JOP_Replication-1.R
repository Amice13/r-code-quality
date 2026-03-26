# ====================================
# Klüver/Spoon: Helping or Hurting?
# Journal of Politics
#  
# Replication Script
# ====================================
  

library(foreign)
library(nlme)
library(lattice)
library(Matrix)
library(lme4)
library(car)
library(arm)
library(memisc)
library(mice)
library(Zelig)#
library(Amelia)
library(plotrix)   
library(graphics)  
library(readstata13)
options(scipen=3)       
                        


# ========================================
# Figure 1 
# ========================================

rm(list=ls(all=TRUE))
setwd("C:/Users/klueverh/Dropbox/Dokumente/Papers/Party context paper/Paper #8/Replication 3rd Revision")

fixeff_raw <- read.csv("fixeff2.csv")
fixeff_untransposed <- fixeff_raw[1,]
fixeff2 <- t(fixeff_untransposed)
fixeff <- fixeff2[1:8]
varcov_raw <- read.csv("varcov2.csv")
(varcov <- varcov_raw[1:8,1:8])



# Step 1: get b and V
# ====================
(b <- as.vector(fixeff))		        
(V <- as.matrix(varcov))		      
(sqrt(diag(V)))                         



# Step 2: Set up MVN distribution
# ===============================
nsim <- 1000
mvn <- mvrnorm(nsim, mu=b, Sigma=V)         


# Step 3: Get coefficients
# ========================

dat <- read.dta13("m2.dta")
attach(dat)
str(dat)


# Predicted values
# ================


juniorpred <-  (
  + mvn[,1] * 0
  + mvn[,2] * 0
  + mvn[,3] * median(dat$niche_party)
  + mvn[,4] * mean(dat$seatshare)
  + mvn[,5] * mean(dat$policy_extreme_logit)
  + mvn[,6] * mean(dat$conflict_logit_new)
  + mvn[,7] * mean(dat$pervote)
  + mvn[,8] * 1
)



seniorpred <-  (
  + mvn[,1] * 1
  + mvn[,2] * 0
  + mvn[,3] * median(dat$niche_party)
  + mvn[,4] * mean(dat$seatshare)
  + mvn[,5] * mean(dat$policy_extreme_logit)
  + mvn[,6] * mean(dat$conflict_logit_new)
  + mvn[,7] * mean(dat$pervote)
  + mvn[,8] * 1
)


opppred <-  (
  + mvn[,1] * 0
  + mvn[,2] * 1
  + mvn[,3] * median(dat$niche_party)
  + mvn[,4] * mean(dat$seatshare)
  + mvn[,5] * mean(dat$policy_extreme_logit)
  + mvn[,6] * mean(dat$conflict_logit_new)
  + mvn[,7] * mean(dat$pervote)
  + mvn[,8] * 1
)


# Taking the means and quantiles of simulated values
# ==================================================

juniorpred_mean <- mean(juniorpred)
juniorpred_lb <- quantile(juniorpred, c(.025))
juniorpred_ub <- quantile(juniorpred, c(.975))

seniorpred_mean <- mean(seniorpred)
seniorpred_lb <- quantile(seniorpred, c(.025))
seniorpred_ub <- quantile(seniorpred, c(.975))

opppred_mean <- mean(opppred)
opppred_lb <- quantile(opppred, c(.025))
opppred_ub <- quantile(opppred, c(.975))


(means <- c(juniorpred_mean, seniorpred_mean, opppred_mean))
(lb <- c(juniorpred_lb, seniorpred_lb, opppred_lb))
(ub <- c(juniorpred_ub, seniorpred_ub, opppred_ub))
type <- c(1,2,3)



# Plotting point estimates with confidence intervals
win.metafile(file="C:/Users/klueverh/Dropbox/Dokumente/Papers/Party context paper/Paper #8/Replication 3rd Revision/figure1.emf", height=8, width=11)
par(mfrow=c(1,1),mar=c(5,5,1.5,3))
plotCI(type, means, ui=ub, li=lb, ylim = c(10,18), xlim = c(0.5,3.5), xlab = "", ylab = "Vote share at the next election (incl. 95% CI)",
       pch=19, xaxt="n", cex=1.5, cex.lab=1.5, cex.axis=1.5)
abline(h=100, col="black", lty=3)
axis(1, at=1:3, lab=c("Junior partner", "Senior partner", "Opposition"), cex.axis=1.2)
dev.off()




# =========================================
# Figure 3: Mispercetion graph 
# =========================================


rm(list=ls(all=TRUE))
setwd("C:/Users/klueverh/Dropbox/Dokumente/Papers/Party context paper/Paper #8/Replication 3rd Revision")


# Perception values
perception <- c(.2167206, .3580966, .6416512)
party <- c("CDU", "CSU", "FDP")

win.metafile(file="C:/Users/klueverh/Dropbox/Dokumente/Papers/Party context paper/Paper #8/Replication 3rd Revision/figure2.emf", height=8, width=11) 
barplot(perception, cex=1.5, cex.axis=2.0, cex.lab=2.0, ylim=c(0,1), xlab= "Party", ylab= "Misperception",
        names.arg=party)
dev.off()



# ======================================
# Figure A.2: Fully fulfilled pledges
# ======================================

rm(list=ls(all=TRUE))
setwd("C:/Users/klueverh/Dropbox/Dokumente/Papers/Party context paper/Paper #8/Replication 3rd Revision")

fixeff_raw <- read.csv("fixeff_pledges1a.csv")
fixeff_untransposed <- fixeff_raw[1,]
fixeff2 <- t(fixeff_untransposed)
fixeff <- fixeff2[1:4]
varcov_raw <- read.csv("varcov_pledges1a.csv")
(varcov <- varcov_raw[1:4,1:4])



# Step 1: get b and V
# ====================
(b <- as.vector(fixeff))		        
(V <- as.matrix(varcov))		      
(sqrt(diag(V)))                         



# Step 2: Set up MVN distribution
# ===============================
nsim <- 1000
mvn <- mvrnorm(nsim, mu=b, Sigma=V)         


# Step 3: Get coefficients
# ========================

dat <- read.dta13("pledges1a.dta")
attach(dat)
str(dat)


# Predicted values
# ================

juniorpred <-  (
  + mvn[,1] * 0
  + mvn[,2] * 0
  + mvn[,3] * 0
  + mvn[,4] * 1
)

singlepred <-  (
  + mvn[,1] * 1
  + mvn[,2] * 0
  + mvn[,3] * 0
  + mvn[,4] * 1
)

seniorpred <-  (
  + mvn[,1] * 0
  + mvn[,2] * 1
  + mvn[,3] * 0
  + mvn[,4] * 1
)


oppositionpred <-  (
  + mvn[,1] * 0
  + mvn[,2] * 0
  + mvn[,3] * 1
  + mvn[,4] * 1
)


# Taking the means and quantiles of simulated values
# ==================================================

juniorpred_mean <- mean(juniorpred)
juniorpred_lb <- quantile(juniorpred, c(.025))
juniorpred_ub <- quantile(juniorpred, c(.975))

seniorpred_mean <- mean(seniorpred)
seniorpred_lb <- quantile(seniorpred, c(.025))
seniorpred_ub <- quantile(seniorpred, c(.975))

singlepred_mean <- mean(singlepred)
singlepred_lb <- quantile(singlepred, c(.025))
singlepred_ub <- quantile(singlepred, c(.975))

oppositionpred_mean <- mean(oppositionpred)
oppositionpred_lb <- quantile(oppositionpred, c(.025))
oppositionpred_ub <- quantile(oppositionpred, c(.975))

(means <- c(juniorpred_mean, seniorpred_mean, singlepred_mean, oppositionpred_mean))
(lb <- c(juniorpred_lb, seniorpred_lb, singlepred_lb, oppositionpred_lb))
(ub <- c(juniorpred_ub, seniorpred_ub, singlepred_ub, oppositionpred_ub))
type <- c(1,2,3,4)



# Plotting point estimates with confidence intervals
win.metafile(file="C:/Users/klueverh/Dropbox/Dokumente/Papers/Party context paper/Paper #8/Replication 3rd Revision/figureA2.emf", height=8, width=11)
par(mfrow=c(1,1),mar=c(5,5,1.5,3)) 
plotCI(type, means, ui=ub, li=lb, ylim = c(0,1), xlim = c(0.5,4.5), xlab = "", ylab = "Relative share of fully fulfilled pledges (incl. 95% CI)",
       pch=19, xaxt="n", cex=1.5, cex.lab=1.5, cex.axis=1.5)
abline(h=100, col="black", lty=3)
axis(1, at=1:4, lab=c("Junior partner", "Senior partner", "Single party government", "Opposition"), cex.axis=1.2)
dev.off()





# ===============================
# Figure A.3 Unfulfilled pledges
# ===============================

rm(list=ls(all=TRUE))
setwd("C:/Users/klueverh/Dropbox/Dokumente/Papers/Party context paper/Paper #8/Replication 3rd Revision")

fixeff_raw <- read.csv("fixeff_pledges1c.csv")
fixeff_untransposed <- fixeff_raw[1,]
fixeff2 <- t(fixeff_untransposed)
fixeff <- fixeff2[1:4]
varcov_raw <- read.csv("varcov_pledges1c.csv")
(varcov <- varcov_raw[1:4,1:4])



# Step 1: get b and V
# ====================
(b <- as.vector(fixeff))		        
(V <- as.matrix(varcov))		      
(sqrt(diag(V)))                         



# Step 2: Set up MVN distribution
# ===============================
nsim <- 1000
mvn <- mvrnorm(nsim, mu=b, Sigma=V)         


# Step 3: Get coefficients
# ========================

dat <- read.dta13("pledges1c.dta")
attach(dat)
str(dat)


# Predicted values
# ================

juniorpred <-  (
  + mvn[,1] * 0
  + mvn[,2] * 0
  + mvn[,3] * 0
  + mvn[,4] * 1
)

singlepred <-  (
  + mvn[,1] * 1
  + mvn[,2] * 0
  + mvn[,3] * 0
  + mvn[,4] * 1
)

seniorpred <-  (
  + mvn[,1] * 0
  + mvn[,2] * 1
  + mvn[,3] * 0
  + mvn[,4] * 1
)


oppositionpred <-  (
  + mvn[,1] * 0
  + mvn[,2] * 0
  + mvn[,3] * 1
  + mvn[,4] * 1
)


# Taking the means and quantiles of simulated values
# ==================================================

juniorpred_mean <- mean(juniorpred)
juniorpred_lb <- quantile(juniorpred, c(.025))
juniorpred_ub <- quantile(juniorpred, c(.975))

seniorpred_mean <- mean(seniorpred)
seniorpred_lb <- quantile(seniorpred, c(.025))
seniorpred_ub <- quantile(seniorpred, c(.975))

singlepred_mean <- mean(singlepred)
singlepred_lb <- quantile(singlepred, c(.025))
singlepred_ub <- quantile(singlepred, c(.975))

oppositionpred_mean <- mean(oppositionpred)
oppositionpred_lb <- quantile(oppositionpred, c(.025))
oppositionpred_ub <- quantile(oppositionpred, c(.975))

(means <- c(juniorpred_mean, seniorpred_mean, singlepred_mean, oppositionpred_mean))
(lb <- c(juniorpred_lb, seniorpred_lb, singlepred_lb, oppositionpred_lb))
(ub <- c(juniorpred_ub, seniorpred_ub, singlepred_ub, oppositionpred_ub))
type <- c(1,2,3,4)



# Plotting point estimates with confidence intervals
win.metafile(file="C:/Users/klueverh/Dropbox/Dokumente/Papers/Party context paper/Paper #8/Replication 3rd Revision/figureA3.emf", height=8, width=11)
par(mfrow=c(1,1),mar=c(5,5,1.5,3))
plotCI(type, means, ui=ub, li=lb, ylim = c(0,1), xlim = c(0.5,4.5), xlab = "", ylab = "Relative share of unfulfilled pledges (incl. 95% CI)",
       pch=19, xaxt="n", cex=1.5, cex.lab=1.5, cex.axis=1.5)
abline(h=100, col="black", lty=3)
axis(1, at=1:4, lab=c("Junior partner", "Senior partner", "Single party government", "Opposition"), cex.axis=1.2)
dev.off()
