rm(list = ls())

library("reshape")
library("ggplot2")
library("quantreg")


## Figure 26.1
## plot quantile
pdf("CDFquantile.pdf", height = 4, width = 6)
par(mar = c(2, 2, 2, 2))
yy = seq(-2, 5, 0.01)
y.cdf = pnorm(yy, 2, 1)
plot(y.cdf ~ yy, type = "l", bty = "n",
     ann = FALSE, 
     yaxt="n", xaxt = "n", yaxs="i")
axis(1, at = c(-3, 6))
axis(2, at = 0:1)
abline(h = 1, lty = 2)
mtext(side = 3, text = "distribution and quantile", 
      line = 0.5, font = 1)
abline(h = 0.4, lty = 3)
mtext(side = 2, text = expression(tau), 
      line = 0.5, at = 0.4)
mtext(side = 2, text = 0:1, 
      line = 0.5, at = 0:1)
q0.4 = qnorm(0.4, 2, 1)
segments(q0.4, 0, q0.4, 0.4, lty = 3)
mtext(side = 1, text = expression(F^-1~(tau)),
      line = 0.5)
dev.off()





## Figure 26.2
## check functions
pdf("checkfunctionplot.pdf", height = 3, width = 9)
uu = seq(-2, 2, 0.001)

par(mfrow = c(1, 3))
tau = 1/3
rho_tau = uu*(tau-(uu<0))
plot(rho_tau ~ uu, type = "l",
     xlab = "", ylab = "", main ="",
     yaxt="n", xaxt = "n", bty="n")  
title( expression(rho[tau](u)), cex.main = 1.5)     
text(2.09, 0, expression(u), cex = 1.5)
arrows(-2, 0, 2, 0, length = 0.1, angle = 15)
arrows(0, -1, 0, 1.38, length = 0.1, angle = 15)
mtext(expression(tau==1/3), 1, line = 2)

tau = 1/2
rho_tau = uu*(tau-(uu<0))
plot(rho_tau ~ uu, type = "l",
     xlab = "", ylab = "", main = "",
     yaxt="n", xaxt = "n", bty="n")  
title( expression(rho[tau](u)), cex.main = 1.5)     

text(2.09, 0, expression(u), cex= 1.5)
arrows(-2, 0, 2, 0, length = 0.1, angle = 15)
arrows(0, -1, 0, 1.04, length = 0.1, angle = 15)
mtext(expression(tau==1/2), 1, line = 2)


tau = 2/3
rho_tau = uu*(tau-(uu<0))
plot(rho_tau ~ uu, type = "l",
     xlab = "", ylab = "", main = "",
     yaxt="n", xaxt = "n", bty="n") 
title( expression(rho[tau](u)), cex.main = 1.5)           
text(2.09, 0, expression(u), cex= 1.5)
arrows(-2, 0, 2, 0, length = 0.1, angle = 15)
arrows(0, -1, 0, 1.38, length = 0.1, angle = 15)
mtext(expression(tau==2/3), 1, line = 2)
dev.off()




## Chapter 26.4.1: Sample quantiles
mc= 2000
n = 200
taus = (1:9)/10
## function for getting se
get.se = function(x){x$coef[1,2]}
## sample quantile
q.normal = replicate(mc,{
  y  = rnorm(n)
  qy = rq(y~1, tau = taus)
  se.iid = summary(qy, se = "iid")
  se.ker = summary(qy, se = "ker")
  se.boot= summary(qy, se = "boot")
  
  qy = qy$coef
  se.iid = sapply(se.iid, get.se)
  se.ker = sapply(se.ker, get.se)
  se.boot= sapply(se.boot, get.se)
  
  c(qy, se.iid, se.ker, se.boot)
})
## bias
dat.bias = data.frame(trueq = qnorm(taus),
                      estq  = apply(q.normal[1:9, ], 1, mean))
ggplot(dat.bias) + 
  geom_point(aes(x=trueq, y=estq)) + 
  geom_abline(aes(intercept=0, slope=1))

## se
normal.se = rbind(data.frame(tau = taus,
                          se  = apply(q.normal[10:18, ], 1, mean),
                          method = "iid",
                          dist = "Normal(0,1)"),
               data.frame(tau = taus,
                          se  = apply(q.normal[19:27, ], 1, mean),
                          method = "ker",
                          dist = "Normal(0,1)"),
               data.frame(tau = taus,
                          se  = apply(q.normal[28:36, ], 1, mean),
                          method = "boot",
                          dist = "Normal(0,1)"),
               data.frame(tau = taus,
                          se  = sqrt(taus*(1-taus)/n)/dnorm(qnorm(taus)),
                          method = "true",
                          dist = "Normal(0,1)"))
 
ggplot(normal.se) + 
  geom_point(aes(x=tau,y=se, shape=method), 
             alpha = 0.7) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) +  
  scale_x_continuous(breaks = taus) + 
  xlab("quantiles") + ylab("standard errors") +
  ggtitle("N(0,1) distribution")



q.exp = replicate(mc,{
  y  = rexp(n)
  qy = rq(y~1, tau = taus)
  se.iid = summary(qy, se = "iid")
  se.ker = summary(qy, se = "ker")
  se.boot= summary(qy, se = "boot")
  
  qy = qy$coef
  se.iid = sapply(se.iid, get.se)
  se.ker = sapply(se.ker, get.se)
  se.boot= sapply(se.boot, get.se)
  
  c(qy, se.iid, se.ker, se.boot)
})
## bias
dat.bias = data.frame(trueq = qexp(taus),
                      estq  = apply(q.exp[1:9, ], 1, mean))
ggplot(dat.bias) + 
  geom_point(aes(x=trueq, y=estq)) + 
  geom_abline(aes(intercept=0, slope=1))

## se
expo.se = rbind(data.frame(tau = taus,
                          se  = apply(q.exp[10:18, ], 1, mean),
                          method = "iid",
                          dist = "Exponential(1)"),
               data.frame(tau = taus,
                          se  = apply(q.exp[19:27, ], 1, mean),
                          method = "ker",
                          dist = "Exponential(1)"),
               data.frame(tau = taus,
                          se  = apply(q.exp[28:36, ], 1, mean),
                          method = "boot",
                          dist = "Exponential(1)"),
               data.frame(tau = taus,
                          se  = sqrt(taus*(1-taus)/n)/dexp(qexp(taus)),
                          method = "true",
                          dist = "Exponential(1)"))

ggplot(expo.se) + 
  geom_point(aes(x=tau,y=se, shape=method), 
             alpha = 0.7) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) +  
  scale_x_continuous(breaks = taus) + 
  xlab("quantiles") + ylab("standard errors") + 
  ggtitle("Exponential(1) distribution")

## combine two
dat.se = rbind(normal.se, expo.se)
ggplot(dat.se) + 
  geom_point(aes(x=tau,y=se, shape=method), 
             alpha = 0.7) + 
  geom_vline(aes(xintercept = tau),
             alpha = 0.3, linetype = "dashed") + 
  facet_grid(~dist) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) +  
  scale_x_continuous(breaks = taus) + 
  xlab("quantiles") + ylab("standard errors") 
ggsave("ese_quantiles.pdf", height = 4, width = 8.5)





## Chapter 26.4.2
## quantile regressions: LAD versus OLS
x = rnorm(n)
simu.normal = replicate(mc, {
  y = 1 + x + rnorm(n)
  c(lm(y~x)$coef[2], rq(y~x)$coef[2])
})
 
dat.normal = rbind(data.frame(error="Normal",
                              est  = "OLS",
                              beta = simu.normal[1,]),
                   data.frame(error="Normal",
                              est  = "LAD",
                              beta = simu.normal[2,]))
ggplot(dat.normal) + 
  geom_histogram(aes(beta, y=..density..)) + 
  facet_grid(~est)


simu.exp = replicate(mc, {
  y = 1 + x + rexp(n) 
  c(lm(y~x)$coef[2], rq(y~x)$coef[2])
})
 
dat.exp = rbind(data.frame(error="Exponential",
                              est  = "OLS",
                              beta = simu.exp[1,]),
                   data.frame(error="Exponential",
                              est  = "LAD",
                              beta = simu.exp[2,]))
ggplot(dat.exp) + 
  geom_histogram(aes(beta, y=..density..)) + 
  facet_grid(~est)


simu.laplace = replicate(mc, {
  y = 1 + x + rexp(n) - rexp(n)
  c(lm(y~x)$coef[2], rq(y~x)$coef[2])
})
 
dat.laplace = rbind(data.frame(error="Laplace",
                               est  = "OLS",
                               beta = simu.laplace[1,]),
                    data.frame(error="Laplace",
                               est  = "LAD",
                               beta = simu.laplace[2,]))
ggplot(dat.laplace) + 
  geom_histogram(aes(beta, y=..density..)) + 
  facet_grid(~est)
 
## E(y|x)=1+x
## F^-1(0.5|x)=1+log(2)x
x = abs(x)
simu.x = replicate(mc, {
  y = 1 + rexp(n)*x
  c(lm(y~x)$coef[2], rq(y~x)$coef[2])
})
 

dat.x = rbind(data.frame(error="Exponential X",
                           est  = "OLS",
                           beta = simu.x[1,]),
                data.frame(error="Exponential X",
                           est  = "LAD",
                           beta = simu.x[2,]))
ggplot(dat.x) + 
  geom_histogram(aes(beta, y=..density..)) + 
  facet_grid(~est)


dat = rbind(dat.normal, dat.laplace, dat.exp, dat.x)
dat.se = aggregate(beta~error*est, data=dat, FUN=sd)

ggplot(dat) + 
  geom_histogram(aes(beta, y=..density..),
                 alpha = 0.7, bins = 50) +
  geom_vline(aes(xintercept=1), 
             alpha = 0.3, linetype = "dashed") + 
  facet_grid(error~est) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.y=element_blank()) + 
  geom_text(data = dat.se, alpha = 0.8, 
            aes(x = 1.5, y = 5,
                label = paste0("se=", round(beta, 2)))) + 
  xlab(expression(hat(beta)))
ggsave("quantile_regression_simu.pdf", height = 7, width = 7)  



## Chapter 26.5.1
## Galton's data
library("HistData")
taus     = (1:9)/10
qr.galton = rq(childHeight ~ midparentHeight, 
               tau = taus,
               data = GaltonFamilies)
coef.galton = qr.galton$coef
dat.coef = data.frame(tau = factor(taus,
                                   ordered = TRUE,
                                   levels = rev(taus)),
                      a = coef.galton[1, ],
                      b = coef.galton[2, ])
ggplot(GaltonFamilies,
       aes(x=midparentHeight, y=jitter(childHeight))) +
  geom_point(size = 0.4, col = "grey",
             alpha = 0.5) +
  geom_abline(data = dat.coef,
              mapping = aes(intercept=a, slope=b,
                            linetype = tau),
              alpha = 0.7) +
  scale_linetype(expression(tau)) + 
  ylab("childHeight") +
  ggtitle("quantile regressions") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave("galton_qr.pdf", height = 5, width = 7)  



## Chapter 26.5.2
# This is the empirical example for the paper 
# "Quantile Regression under Misspecification" 
# by J. Angrist, V. Chernozhukov and I. Fernandez-Val
library(foreign)
census80 = read.dta("census80.dta")
census90 = read.dta("census90.dta")
census00 = read.dta("census00.dta")
f.reg = logwk ~ educ + exper + exper2 + black

## coefficient of educ
m.boot   = 500       
rq80     = rq(f.reg, data = census80, tau = taus)
rqlist80 = summary(rq80, se = "boot", 
                   bsmethod= "xy", mofn = m.boot)
rq90     = rq(f.reg, data = census90, tau = taus)
rqlist90 = summary(rq90, se = "boot", 
                   bsmethod= "xy", mofn = m.boot)
rq00     = rq(f.reg, data = census00, tau = taus)
rqlist00 = summary(rq00, se = "boot", 
                   bsmethod= "xy", mofn = m.boot)


## get point est and se
get.pe = function(x){x$coef[2,1]}
get.se = function(x){x$coef[2,2]}

rqeduc80 = sapply(rqlist80, get.pe)
seeduc80 = sapply(rqlist80, get.se)
rqeduc90 = sapply(rqlist90, get.pe)
seeduc90 = sapply(rqlist90, get.se)
rqeduc00 = sapply(rqlist00, get.pe)
seeduc00 = sapply(rqlist00, get.se)

dat.qr = rbind(data.frame(x=taus,
                          y=rqeduc80,
                          l.ci=rqeduc80 - 1.96*seeduc80,
                          u.ci=rqeduc80 + 1.96*seeduc80,
                          year="80"),
               data.frame(x=taus,
                          y=rqeduc90,
                          l.ci=rqeduc90 - 1.96*seeduc90,
                          u.ci=rqeduc90 + 1.96*seeduc90,
                          year="90"),
               data.frame(x=taus,
                          y=rqeduc00,
                          l.ci=rqeduc00 - 1.96*seeduc00,
                          u.ci=rqeduc00 + 1.96*seeduc00,
                          year="00"))
dat.qr$year = factor(dat.qr$year,
                     ordered = TRUE,
                     levels = c("00", "90", "80"))
ggplot(dat.qr, aes(x=x,y=y)) + 
  geom_smooth(aes(ymin = l.ci, ymax = u.ci, 
                  group = year, linetype = year),
              stat = "identity", col = "black", 
              size = 0.2, alpha = 0.3) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) + 
  xlab(expression(tau)) + 
  ylab("coefficient of educ") + 
  ggtitle("quantile regressions") 
ggsave("angrist_qr.pdf", height = 4, width = 7)



## weighted quantile regression
rq80     = rq(f.reg, data = census80, 
              tau = taus, weights = perwt)
rqlist80 = summary(rq80, se = "ker")
rq90     = rq(f.reg, data = census90, 
              tau = taus, weights = perwt)
rqlist90 = summary(rq90, se = "ker")
rq00     = rq(f.reg, data = census00, 
              tau = taus, weights = perwt)
rqlist00 = summary(rq00, se = "ker")


## get point est and se
get.pe = function(x){x$coef[2,1]}
get.se = function(x){x$coef[2,2]}

rqeduc80 = sapply(rqlist80, get.pe)
seeduc80 = sapply(rqlist80, get.se)
rqeduc90 = sapply(rqlist90, get.pe)
seeduc90 = sapply(rqlist90, get.se)
rqeduc00 = sapply(rqlist00, get.pe)
seeduc00 = sapply(rqlist00, get.se)

dat.qr = rbind(data.frame(x=taus,
                          y=rqeduc80,
                          l.ci=rqeduc80 - 1.96*seeduc80,
                          u.ci=rqeduc80 + 1.96*seeduc80,
                          year="80"),
               data.frame(x=taus,
                          y=rqeduc90,
                          l.ci=rqeduc90 - 1.96*seeduc90,
                          u.ci=rqeduc90 + 1.96*seeduc90,
                          year="90"),
               data.frame(x=taus,
                          y=rqeduc00,
                          l.ci=rqeduc00 - 1.96*seeduc00,
                          u.ci=rqeduc00 + 1.96*seeduc00,
                          year="00"))
dat.qr$year = factor(dat.qr$year,
                     ordered = TRUE,
                     levels = c("00", "90", "80"))
ggplot(dat.qr, aes(x=x,y=y)) + 
  geom_smooth(aes(ymin = l.ci, ymax = u.ci, 
                  group = year, linetype = year),
              stat = "identity", col = "black", 
              size = 0.2, alpha = 0.3) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) + 
  xlab(expression(tau)) + 
  ylab("coefficient of educ") + 
  ggtitle("quantile regressions") 
ggsave("angrist_weighted_qr.pdf", height = 4, width = 7)




## Chapter 26.6
## cluster-robust standard error in quantile regression 
star = read.csv("star.csv")
head(star)
star.rq  = rq(pscore ~ small + regaide + black + 
                girl + poor + tblack + texp + 
                tmasters + factor(fe),
              data = star)
res      = summary(star.rq, se = "boot")$coef[2:9, ]
res.clus = summary(star.rq, se = "boot", 
                   cluster = star$classid)$coef[2:9, ]
round(res, 3)
round(res.clus, 3)
