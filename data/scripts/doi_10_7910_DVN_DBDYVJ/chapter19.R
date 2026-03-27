rm(list = ls())


## Chapter 19.3.1
## FGLS Boston housing data
library(mlbench)
data(BostonHousing)
ols.fit = lm(medv ~ ., data = BostonHousing)
dat.res = BostonHousing
## log transformation of the squared residuals
dat.res$medv = log((ols.fit$residuals)^2)
t.res.ols = lm(medv ~ ., data = dat.res)
w.fgls = exp(- t.res.ols$fitted.values)
fgls.fit = lm(medv ~ ., weights = w.fgls, data = BostonHousing)
round(summary(ols.fit)$coef, 3)
round(summary(fgls.fit)$coef, 3)




## Chapter 19.3.2
## ecological regression 
library(ggplot2)
lavoteall = read.csv("lavoteall.csv")
scatter_plot = ggplot(lavoteall) + 
  geom_point(aes(x, t, cex = n),
             pch = 19, alpha = 0.2) +
  scale_size_continuous(range = c(0.1, 1)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ols.fit = lm(t ~ x, data = lavoteall)
wls.fit = lm(t ~ x, weights = n, data = lavoteall)
round(summary(ols.fit)$coef, 3)
round(summary(wls.fit)$coef, 3)


dat_line = rbind(data.frame(estimator = "WLS",
                            a = wls.fit$coef[1],
                            b = wls.fit$coef[2]),
                 data.frame(estimator = "OLS",
                            a = ols.fit$coef[1],
                            b = ols.fit$coef[2]))
scatter_plot + 
  geom_abline(data = dat_line,
              aes(intercept = a,
                  slope     = b,
                  linetype  = estimator))
ggsave("lavoteall_plot.pdf", width = 7, height = 4)


## Goodman regression
ols.fit = lm(t ~ 0 + x + I(1-x), data = lavoteall)
wls.fit = lm(t ~ 0 + x + I(1-x), weights = n, data = lavoteall)
round(summary(ols.fit)$coef, 3)
round(summary(wls.fit)$coef, 3)

 



## Chapter 19.4.1
## local polynomial regression 
library("KernSmooth")
n = 500
x = seq(0, 1, length.out = n)
fx= sin(8*x)
y = fx + rnorm(n, 0, 0.5)

pdf("localpolynomialplot.pdf", height = 4, width = 7)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 0.05))
plot(y ~ x, pch = 19, cex = 0.2, col = "grey", bty = "n",
     main = "local linear approximation", font.main = 1)
lines(fx ~ x, lwd = 2, col = "grey")

x0 = 0.4
y0 = sin(8*x0)
segments(x0, -3, x0, y0, lty = 2, col = "grey")
segments(-4, y0, x0, y0, lty = 2, col = "grey")

ylinear = sin(8*x0) + 8*cos(8*x0)*(x - x0)
lines(ylinear ~ x, lty = 2)
abline(v = x0 - 0.05, col = "grey")
abline(v = x0 + 0.05, col = "grey")


plot(y ~ x, pch = 19, cex = 0.2, col = "grey", bty = "n",
     main = "local linear fit", font.main = 1)
lines(fx ~ x, lwd = 2, col = "grey")
h = dpill(x, y)
locp.fit = locpoly(x, y, bandwidth = h)
lines(locp.fit, lty = 2)
dev.off()




## Chapter 19.4.2
## WLS with survey data
library(foreign)
census00 = read.dta("census00.dta")
ols.fit = lm(logwk ~ age + educ + exper + exper2 + black,
             data = census00)
wls.fit = lm(logwk ~ age + educ + exper + exper2 + black,
             weights = perwt, data = census00)
round(summary(ols.fit)$coef, 3)
round(summary(wls.fit)$coef, 3)



## homework problem 
fultongen = read.csv("fultongen.csv")
head(fultongen)

