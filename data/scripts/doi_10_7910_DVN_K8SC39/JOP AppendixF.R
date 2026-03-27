library(foreign)
library(stargazer)
library(car)
library(lme4)
library(effects)
library(multiwayvcov)
library(lmtest)
library(sandwich)
library(sampleSelection)
library(cem)


setwd('/Users/jsievert/Dropbox/APSA 2014/JOP Submission/JOP Replication')

options(scipen = 8)

appF <- read.csv('jop_appendixF.csv')

m2 <-subset(appF, pres_elec == 1)
m3 <- subset(appF, pres_elec == 0)

mat <- cem(treatment = "demqa", data = appF, drop = c("stcd", "dv", "year", "south") )
mat2 <- cem(treatment = "demqa", data = m2, drop = c("stcd", "dv", "year", "south") )
mat3 <- cem(treatment = "demqa", data = m3, drop = c("stcd", "dv", "year", "south") )

#SATT
est1a <- att(mat, dv ~ demqa, data = m)
est1b <- att(mat2, dv ~ demqa, data = m2)
est1c <- att(mat3, dv ~ demqa, data = m3)


est2a <- att(mat, dv ~ demqa + dpres + dvp + as.factor(year), data = m)
est2b <- att(mat2, dv ~ demqa + dpres + dvp + as.factor(year), data = m2)
est2c <- att(mat3, dv ~ demqa + dpres + dvp + as.factor(year), data = m3)

mod1 <- lm(dv ~ demqa + dvp + dpres + as.factor(year), m)
mod2 <- lm(dv ~ demqa + dvp + dpres + as.factor(year), m2)
mod3 <- lm(dv ~ demqa + dvp + dpres + as.factor(year), m3)

cem.c <- c(est2a$att.model[1,2], est2b$att.model[1,2], est2c$att.model[1,2])
cem.se <- c(est2a$att.model[2,2], est2b$att.model[2,2], est2c$att.model[2,2])
cem.lo <- cem.c - cem.se*qnorm(0.975)
cem.hi <- cem.c + cem.se*qnorm(0.975)

ci <- array(NA, dim = c(3,2))
coef <- c(coef(mod1)[2], coef(mod2)[2], coef(mod3)[2])
ci[1,] <- confint(mod1)[2,]
ci[2,] <- confint(mod2)[2,]
ci[3,] <- confint(mod3)[2,]

r <- seq(1, 3, 1)

plot(r-0.25, cem.c, xlim = c(0.5,3.5), ylim = c(0, 5), xaxt='n', yaxt='n', xlab='', ylab='', main = '', type = 'n')


segments(x0 = r + 0.15, x1 = r + 0.15, y0 = cem.lo, y1 = cem.hi)
segments(x0 = r - 0.15, x1 = r - 0.15, y0 = ci[,1], y1 = ci[,2], col = 'grey40')

points(r+0.15, cem.c, pch = 16)
points(r-0.15, coef, pch = 16, col = 'grey40')

abline(h = 0)
axis(side = 1, at = seq(1,3), labels = c("All", "Presidential", "Midterm"))
axis(side = 2, at = seq(0, 5, 0.5),  las = 2)
#axis(side = 2, at = seq(0,6,1), labels = seq(0, 6, 1), las = 2)
mtext("Estimate", side = 2, line = 2.75)

legend('topleft', bty = 'n', legend = c("Unmatched Estimates", "CEM Estimates"), pch = 16, col = c('grey40', 'black'), cex = 0.8)





