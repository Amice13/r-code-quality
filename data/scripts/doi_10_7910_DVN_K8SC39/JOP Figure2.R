
library(foreign)
library(car)
library(stargazer)
library(multiwayvcov)
library(lmtest)
library(sandwich)
library(effects)

setwd('/Users/jsievert/Dropbox/APSA 2014/JOP Submission/JOP Replication')


f2 <- read.csv('jop_fig2.csv')

#subset by election timing
nov <- subset(f2, nov == 1)
pre <- subset(f2, prenov == 1)
post <- subset(f2, postnov == 1)


m1 <- lm(dv ~ dvp*pres_elec + demqa*pres_elec + dpres*pres_elec + south, f2)
coeftest(m1, vcov = cluster.vcov(m1, f2$cdID))

#november elections
m2 <- lm(dv ~ dvp*pres_elec + demqa*pres_elec + dpres*pres_elec + south, nov)
coeftest(m2, vcov = cluster.vcov(m2, nov$cdID))


#prenov elections
m3 <- lm(dv ~ dvp*pres_elec + demqa*pres_elec + dpres*pres_elec + south, pre)
coeftest(m3, vcov = cluster.vcov(m3, pre$cdID))

#postnov elections
m4 <- lm(dv ~ dvp*pres_elec + demqa*pres_elec + dpres*pres_elec + south, post)
coeftest(m4, vcov = cluster.vcov(m4, post$cdID))

#######################
#MARGINAL EFFECTS: DQA#
#######################

o1 <- interplot(m = m1, var1 = "demqa", var2 = "pres_elec")$data
o2 <- interplot(m = m2, var1 = "demqa", var2 = "pres_elec")$data
o3 <- interplot(m = m3, var1 = "demqa", var2 = "pres_elec")$data
o4 <- interplot(m = m4, var1 = "demqa", var2 = "pres_elec")$data


#######################
#PLOT MARGINAL EFFECTS#
#######################

coefs <- matrix(c(o1[,2], o2[,2], o3[,2], o4[,2]), nrow = 2, ncol = 4)
lo <- matrix(c(o1[,4], o2[,4], o3[,4], o4[,4]), nrow = 2, ncol = 4)
hi <- matrix(c(o1[,3], o2[,3], o3[,3], o4[,3]), nrow = 2, ncol = 4)

r <- 1:4

plot(r-0.1, coefs[2,], ylim = c(0, 9), xlim = c(0.5, 4.5), xaxt='n', yaxt='n', xlab='', ylab='',
pch = 16, col = 'grey40')
segments(x0 = r-0.1 , x1 = r-0.1, y0 = lo[2,], y1 = hi[2,], col = 'grey40')
points(r+0.1, coefs[1,], pch = 16)
segments(x0 = r+0.1 , x1 = r+0.1, y0 = lo[1,], y1 = hi[1,], lty = 2)
abline(h=0)
name <- c("Full", "November", "Pre-November", "Post-November")
axis(side = 1, at = r, labels = name )
obs <- c("(n = 4409)", "(n = 2589)", "(n = 1197)", "(n = 623)")
axis(side = 1, at = r, labels = obs, line = 1, tick = FALSE, cex.axis = 0.8)
axis(side = 2, at = seq(0,8, 2), labels = seq(0,8, 2), las = 2)


legend('topleft', pch = c(16, 16), lty = c(1,2), col = c('grey40', 'black'), 
legend = c('Presidential', 'Midterm'), bty = 'n', cex = 0.8)
mtext("Estimate", side = 2, line = 2.75)


