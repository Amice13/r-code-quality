library(foreign)
library(stargazer)
library(car)
library(lmtest)
library(sandwich)


sappCwd('/Users/jsievert/Dropbox/APSA 2014/JOP Submission/JOP Replication')

appC <- read.csv('jop_appendixC.csv')

#####################
##Results by Deacde##
#####################

appC$decade <- 0
appC$decade[appC$year >= 1842 & appC$year < 1852] <- 1
appC$decade[appC$year >= 1852 & appC$year < 1862] <- 2
appC$decade[appC$year >= 1862 & appC$year < 1872] <- 3
appC$decade[appC$year >= 1872 & appC$year < 1882] <- 4
appC$decade[appC$year >= 1882  ] <- 5

dqa <- array(NA, dim = c(5,4))
dpres <- array(NA, dim = c(5,4))
dvp <- array(NA, dim = c(5,4))

dqa.lo <- array(NA, dim = c(5,4))
dpres.lo <- array(NA, dim = c(5,4))
dvp.lo <- array(NA, dim = c(5,4))

dqa.hi <- array(NA, dim = c(5,4))
dpres.hi <- array(NA, dim = c(5,4))
dvp.hi <- array(NA, dim = c(5,4))

for(i in 1:5){
temp <- subset(appC, decade == i)
t1 <- subset(temp, inc_t != 0)
t2 <- subset(temp, inc_t == 0)
t3 <- subset(temp, pres_elec == 1)
t4 <- subset(temp, pres_elec == 0)

mod <- lm(dv ~ dvp +  demqa + dpres + south + as.factor(year), t1)

mod2 <- lm(dv ~ dvp + demqa + dpres + south + as.factor(year), t2)

mod3 <-  lm(dv ~ dvp + demqa + dpres + south + as.factor(year), t3)

mod4 <- lm(dv ~ dvp + demqa + dpres + south + as.factor(year), t4)

dqa[i,1] <- coef(mod)[3]
dqa[i,2] <- coef(mod2)[3]
dqa[i,3] <- coef(mod3)[3]
dqa[i,4] <- coef(mod4)[3]

dpres[i,1] <- coef(mod)[4]
dpres[i,2] <- coef(mod2)[4]
dpres[i,3] <- coef(mod3)[4]
dpres[i,4] <- coef(mod4)[4]

dvp[i,1] <- coef(mod)[2]
dvp[i,2] <- coef(mod2)[2]
dvp[i,3] <- coef(mod3)[2]
dvp[i,4] <- coef(mod4)[2]

dqa.lo[i,1] <- confint(mod, vcov = cluster.cov(mod, t1$cdID))[3,1]
dqa.lo[i,2] <- confint(mod2, vcov = cluster.cov(mod2, t2$cdID))[3,1]
dqa.lo[i,3] <- confint(mod3, vcov = cluster.cov(mod3, t3$cdID))[3,1]
dqa.lo[i,4] <- confint(mod4, vcov = cluster.cov(mod4, t4$cdID))[3,1]

dpres.lo[i,1] <- confint(mod, vcov = cluster.cov(mod, t1$cdID))[4,1]
dpres.lo[i,2] <- confint(mod2, vcov = cluster.cov(mod2, t2$cdID))[4,1]
dpres.lo[i,3] <- confint(mod3, vcov = cluster.cov(mod3, t3$cdID))[4,1]
dpres.lo[i,4] <- confint(mod4, vcov = cluster.cov(mod4, t4$cdID))[4,1]

dvp.lo[i,1] <- confint(mod, vcov = cluster.cov(mod, t1$cdID))[2,1]
dvp.lo[i,2] <- confint(mod2, vcov = cluster.cov(mod2, t2$cdID))[2,1]
dvp.lo[i,3] <- confint(mod3, vcov = cluster.cov(mod3, t3$cdID))[2,1]
dvp.lo[i,4] <- confint(mod4, vcov = cluster.cov(mod4, t4$cdID))[2,1]

dqa.hi[i,1] <- confint(mod, vcov = cluster.cov(mod, t1$cdID))[3,2]
dqa.hi[i,2] <- confint(mod2, vcov = cluster.cov(mod2, t2$cdID))[3,2]
dqa.hi[i,3] <- confint(mod3, vcov = cluster.cov(mod3, t3$cdID))[3,2]
dqa.hi[i,4] <- confint(mod4, vcov = cluster.cov(mod4, t4$cdID))[3,2]

dpres.hi[i,1] <- confint(mod, vcov = cluster.cov(mod, t1$cdID))[4,2]
dpres.hi[i,2] <- confint(mod2, vcov = cluster.cov(mod2, t2$cdID))[4,2]
dpres.hi[i,3] <- confint(mod3, vcov = cluster.cov(mod3, t3$cdID))[4,2]
dpres.hi[i,4] <- confint(mod4, vcov = cluster.cov(mod4, t4$cdID))[4,2]

dvp.hi[i,1] <- confint(mod, vcov = cluster.cov(mod, t1$cdID))[2,2]
dvp.hi[i,2] <- confint(mod2, vcov = cluster.cov(mod2, t2$cdID))[2,2]
dvp.hi[i,3] <- confint(mod3, vcov = cluster.cov(mod3, t3$cdID))[2,2]
dvp.hi[i,4] <- confint(mod4, vcov = cluster.cov(mod4, t4$cdID))[2,2]
}

############
#incumbents#
############

r <- 5:1
d <- c('1840s', '1850s', '1860s', '1870s', '1880s')

par(mfrow = c(1,3))

#dqa
plot(dqa[,1], r, xlim = c(-2, 6), yaxt='n', ylim = c(0.5, 5.5), pch = 16, xlab='', ylab='')
segments(x0 = dqa.lo[,1], y0= r, x1 = dqa.hi[,1], y1 = r)
abline(v = 0, lty = 2)
axis(side = 2, at = r, labels = d, las = 2)
mtext("Coefficient", side = 1, line = 2.75, cex = 0.8)
mtext("Dem. Quality. Adv", side = 3, line = 0.6, cex = 1.1, font = 2)

plot(dpres[,1], r, xlim = c(-0.2, 0.8), yaxt='n', ylim = c(0.5, 5.5), pch = 16, xlab='', ylab='')
segments(x0 = dpres.lo[,1], y0= r, x1 = dpres.hi[,1], y1 = r)
abline(v = 0, lty = 2)
axis(side = 2, at = r, labels = d, las = 2)
mtext("Coefficient", side = 1, line = 2.75, cex = 0.8)
mtext("Dem. Pres", side = 3, line = 0.6, cex = 1.1, font = 2)


plot(dvp[,1], r, xlim = c(-0.2, 0.8), yaxt='n', ylim = c(0.5, 5.5), pch = 16, xlab='', ylab='')
segments(x0 = dvp.lo[,1], y0= r, x1 = dvp.hi[,1], y1 = r)
abline(v = 0, lty = 2)
axis(side = 2, at = r, labels = d, las = 2)
mtext("Coefficient", side = 1, line = 2.75, cex = 0.8)
mtext("Dem. Vote Lagged", side = 3, line = 0.6, cex = 1.1, font = 2)


############
#open seat#
############

r <- 5:1
d <- c('1840s', '1850s', '1860s', '1870s', '1880s')

par(mfrow = c(1,3))

#dqa
plot(dqa[,2], r, xlim = c(-2, 6), yaxt='n', ylim = c(0.5, 5.5), pch = 16, xlab='', ylab='')
segments(x0 = dqa.lo[,2], y0= r, x1 = dqa.hi[,2], y1 = r)
abline(v = 0, lty = 2)
axis(side = 2, at = r, labels = d, las = 2)
mtext("Coefficient", side = 1, line = 2.75, cex = 0.8)
mtext("Dem. Quality. Adv", side = 3, line = 0.6, cex = 1.1, font = 2)

plot(dpres[,2], r, xlim = c(-0.2, 0.8), yaxt='n', ylim = c(0.5, 5.5), pch = 16, xlab='', ylab='')
segments(x0 = dpres.lo[,2], y0= r, x1 = dpres.hi[,2], y1 = r)
abline(v = 0, lty = 2)
axis(side = 2, at = r, labels = d, las = 2)
mtext("Coefficient", side = 1, line = 2.75, cex = 0.8)
mtext("Dem. Pres", side = 3, line = 0.6, cex = 1.1, font = 2)


plot(dvp[,2], r, xlim = c(-0.2, 0.8), yaxt='n', ylim = c(0.5, 5.5), pch = 16, xlab='', ylab='')
segments(x0 = dvp.lo[,2], y0= r, x1 = dvp.hi[,2], y1 = r)
abline(v = 0, lty = 2)
axis(side = 2, at = r, labels = d, las = 2)
mtext("Coefficient", side = 1, line = 2.75, cex = 0.8)
mtext("Dem. Vote Lagged", side = 3, line = 0.6, cex = 1.1, font = 2)


################
#pres. election#
################

r <- 5:1
d <- c('1840s', '1850s', '1860s', '1870s', '1880s')

par(mfrow = c(1,3))

#dqa
plot(dqa[,3], r, xlim = c(-2, 6), yaxt='n', ylim = c(0.5, 5.5), pch = 16, xlab='', ylab='')
segments(x0 = dqa.lo[,3], y0= r, x1 = dqa.hi[,3], y1 = r)
abline(v = 0, lty = 2)
axis(side = 2, at = r, labels = d, las = 2)
mtext("Coefficient", side = 1, line = 2.75, cex = 0.8)
mtext("Dem. Quality. Adv", side = 3, line = 0.6, cex = 1.1, font = 2)

plot(dpres[,3], r, xlim = c(-0.2, 1.0), yaxt='n', ylim = c(0.5, 5.5), pch = 16, xlab='', ylab='')
segments(x0 = dpres.lo[,3], y0= r, x1 = dpres.hi[,3], y1 = r)
abline(v = 0, lty = 2)
axis(side = 2, at = r, labels = d, las = 2)
mtext("Coefficient", side = 1, line = 2.75, cex = 0.8)
mtext("Dem. Pres", side = 3, line = 0.6, cex = 1.1, font = 2)


plot(dvp[,3], r, xlim = c(-0.2, 0.8), yaxt='n', ylim = c(0.5, 5.5), pch = 16, xlab='', ylab='')
segments(x0 = dvp.lo[,3], y0= r, x1 = dvp.hi[,3], y1 = r)
abline(v = 0, lty = 2)
axis(side = 2, at = r, labels = d, las = 2)
mtext("Coefficient", side = 1, line = 2.75, cex = 0.8)
mtext("Dem. Vote Lagged", side = 3, line = 0.6, cex = 1.1, font = 2)



##################
#midterm election#
#################

r <- 5:1
d <- c('1840s', '1850s', '1860s', '1870s', '1880s')

par(mfrow = c(1,3))

#dqa
plot(dqa[,4], r, xlim = c(-2, 6), yaxt='n', ylim = c(0.5, 5.5), pch = 16, xlab='', ylab='')
segments(x0 = dqa.lo[,4], y0= r, x1 = dqa.hi[,4], y1 = r)
abline(v = 0, lty = 2)
axis(side = 2, at = r, labels = d, las = 2)
mtext("Coefficient", side = 1, line = 2.75, cex = 0.8)
mtext("Dem. Quality. Adv", side = 3, line = 0.6, cex = 1.1, font = 2)

plot(dpres[,4], r, xlim = c(-0.2, 1.0), yaxt='n', ylim = c(0.5, 5.5), pch = 16, xlab='', ylab='')
segments(x0 = dpres.lo[,4], y0= r, x1 = dpres.hi[,4], y1 = r)
abline(v = 0, lty = 2)
axis(side = 2, at = r, labels = d, las = 2)
mtext("Coefficient", side = 1, line = 2.75, cex = 0.8)
mtext("Dem. Pres", side = 3, line = 0.6, cex = 1.1, font = 2)


plot(dvp[,4], r, xlim = c(-0.2, 0.8), yaxt='n', ylim = c(0.5, 5.5), pch = 16, xlab='', ylab='')
segments(x0 = dvp.lo[,4], y0= r, x1 = dvp.hi[,4], y1 = r)
abline(v = 0, lty = 2)
axis(side = 2, at = r, labels = d, las = 2)
mtext("Coefficient", side = 1, line = 2.75, cex = 0.8)
mtext("Dem. Vote Lagged", side = 3, line = 0.6, cex = 1.1, font = 2)
