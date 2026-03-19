library(foreign)
library(stargazer)
library(car)
library(sandwich)
library(effects)
library(lmtest)
library(interplot)

setwd('/Users/jsievert/Dropbox/APSA 2014/JOP Submission/JOP Replication')


appD <- read.csv('jop_appendixD.csv')



##############################
#effect in marginal districts#
##############################

t1 <- subset(appD, margin < 5)
t2 <- subset(appD, margin < 2.5)
t3 <- subset(appD, margin < 1)
t4 <- subset(appD, margin < 0.5)

m1 <- lm(dv ~ dvp + demqa + dpres + south + as.factor(year), t1)
o1 <- coeftest(m1, cluster.vcov(m1, t1$cdID))

m2 <- lm(dv ~ dvp + demqa + dpres + south + as.factor(year), t2)
o2 <- coeftest(m2, cluster.vcov(m2, t2$cdID))

m3 <- lm(dv ~ dvp + demqa + dpres + south + as.factor(year), t3)
o3 <- coeftest(m3, cluster.vcov(m3, t3$cdID))

m4 <- lm(dv ~ dvp + demqa + dpres + south + as.factor(year), t4)
o4 <- coeftest(m4, cluster.vcov(m4, t4$cdID))

coef <- c(o1[3,1], o2[3,1], o3[3,1], o4[3,1]) 
se <- c(o1[3,2], o2[3,2], o3[3,2], o4[3,2])
lo <- coef - se*qnorm(0.975)
hi <- coef + se*qnorm(0.975)

r <- 1:4

plot(r, coef, xlim = c(0.5, 4.5), ylim = c(0, 5), xaxt ='n', yaxt='n', xlab='', ylab='', main = '', type ='n')
points(r, coef, pch = 16)
segments(x0 = r, x1 = r, y0 = lo, y1 = hi, lty = 2)

obs <- c('(n = 2150)', '(n = 1251)', '(n = 517)', '(n = 276)')
axis(side = 1, at = 1:4, labels = c('> 5', '> 2.5', '> 1', '> 0.5') )
axis(side = 1, at = 1:4, labels = obs, line = 1, tick = FALSE, cex.axis = 0.8)
axis(side = 2, at = seq(0,5,1), labels = seq(0,5,1), las = 2)
mtext("Lagged Margin of Victory", side = 1, line = 3.5) 
mtext("Estimate", side = 2, line = 2.75)

#########################
#marginal: election type#
#########################

#all cases
t1 <- appD
t2 <- subset(appD, pres_elec == 1)
t3 <- subset(appD, pres_elec == 0)

mod1 <- lm(dv ~ dvp + demqa + dpres + south + as.factor(year), t1)
out1a <- coeftest(mod1, cluster.vcov(mod1, t1$cdID))

mod1b <- lm(dv ~ dvp + demqa + dpres + south + as.factor(year), t2)

out1b <- coeftest(mod1b, cluster.vcov(mod1b, t2$cdID))

mod1c <- lm(dv ~ dvp + demqa + dpres + south + as.factor(year), t3)
out1c <- coeftest(mod1c, cluster.vcov(mod1c, t3$cdID))

############
#incumbents#
###########
t1b <- subset(appD, inc_t != 0)
t2b <- subset(appD, inc_t != 0 & pres_elec == 1)
t3b <- subset(appD, inc_t != 0 & pres_elec == 0)


mod2 <- lm(dv ~ dvp + demqa + dpres + south + as.factor(year), t1b)
out2 <- coeftest(mod2, cluster.vcov(mod2, t1b$cdID))

mod3 <- lm(dv ~ dvp + demqa + dpres + south + as.factor(year), t2b)
out3 <- coeftest(mod3, cluster.vcov(mod3, t2b$cdID))

mod4 <- lm(dv ~ dvp + demqa + dpres + south + as.factor(year), t3b)
out4 <- coeftest(mod4, cluster.vcov(mod4, t3b$cdID))

###########
#open seat#
###########

t1c <- subset(appD, inc_t == 0)
t2c <- subset(appD, inc_t == 0 & pres_elec == 1)
t3c <- subset(appD, inc_t == 0 & pres_elec == 0)

mod5 <- lm(dv ~ dvp + demqa + dpres + south + as.factor(year), t1c)
out5 <- coeftest(mod5, cluster.vcov(mod5, t1c$cdID))

mod6 <- lm(dv ~ dvp + demqa + dpres + south + as.factor(year), t2c)
out6 <- coeftest(mod6, cluster.vcov(mod6, t2c$cdID))

mod7 <- lm(dv ~ dvp + demqa + dpres + south + as.factor(year), t3c)
out7 <- coeftest(mod7, cluster.vcov(mod7, t3c$cdID))


#create matrix w/ coefs and conf int
coefs <- cbind(c(out1a[3,1], out1b[3,1], out1c[3,1]), 
c(out2[3,1], out3[3,1], out4[3,1]),
c(out5[3,1], out6[3,1], out7[3,1])
)
 
se <- cbind(c(out1a[3,2], out1b[3,2], out1c[3,2]), 
c(out2[3,2], out3[3,2], out4[3,2]),
c(out5[3,2], out6[3,2], out7[3,2])
)


ci <- array(NA, dim = c(3, 6))
ci[,1] <- coefs[,1] - se[,1]*qnorm(0.975)
ci[,2] <- coefs[,1] + se[,1]*qnorm(0.975)
ci[,3] <- coefs[,2] - se[,2]*qnorm(0.975)
ci[,4] <- coefs[,2] + se[,2]*qnorm(0.975)
ci[,5] <- coefs[,3] - se[,3]*qnorm(0.975)
ci[,6] <- coefs[,3] + se[,3]*qnorm(0.975)


###################
#create figure d2#
##################

r <- 1:3

plot(r-0.25, coefs[1,], ylim = c(0, 5), xlim = c(0.5, 3.5), yaxt='n', xaxt='n', xlab='', ylab='', pch = 16)
points(r, coefs[2,], pch = 16, col = 'grey35')
points(r+0.25, coefs[3,], pch = 16, col = 'grey50')

#draw CIs
segments(x0 = r[1]-0.25, x1 = r[1]-0.25, y0 = ci[1,1], y1 = ci[1,2], lty = 2)
segments(x0 = r[2]-0.25, x1 = r[2]-0.25, y0 = ci[1,3], y1 = ci[1,4], lty = 2)
segments(x0 = r[3]-0.25, x1 = r[3]-0.25, y0 = ci[1,5], y1 = ci[1,6], lty = 2)

segments(x0 = r[1], x1 = r[1], y0 = ci[2,1], y1 = ci[2,2], lty = 2, col = 'grey35')
segments(x0 = r[2], x1 = r[2], y0 = ci[2,3], y1 = ci[2,4], lty = 2, col = 'grey35')
segments(x0 = r[3], x1 = r[3], y0 = ci[2,5], y1 = ci[2,6], lty = 2, col = 'grey35')


segments(x0 = r[1]+0.25, x1 = r[1]+0.25, y0 = ci[3,1], y1 = ci[3,2], lty = 2, col = 'grey50')
segments(x0 = r[2]+0.25, x1 = r[2]+0.25, y0 = ci[3,3], y1 = ci[3,4], lty = 2, col = 'grey50')
segments(x0 = r[3]+0.25, x1 = r[3]+0.25, y0 = ci[3,5], y1 = ci[3,6], lty = 2, col = 'grey50')

axis(side = 1, at = r, labels = c("All Cases", "Incumbents", "Open Seat") )
axis(side = 2 , at = 0:5, labels = 0:5, las = 2)
mtext("Estimate", side = 2, line = 2.75)
legend('bottomleft', legend = c("Pooled", "Presidential Election", "Midterm"), col = c('black', 'grey35', 'grey50'), pch = c(16,16,16), bty = 'n', cex = 0.85)


