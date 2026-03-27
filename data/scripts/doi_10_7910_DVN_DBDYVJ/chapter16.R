rm(list = ls())



## Box-Cox transformation
library(MASS)
library(mediation)
pdf("boxcox_jobs.pdf", height = 4, width = 8.5)
par(mfrow = c(1, 3))
jobslm = lm(job_seek ~ treat + econ_hard + depress1 + sex + age + occp + marital + 
              nonwhite + educ + income, data = jobs)
boxcox(jobslm, lambda = seq(1.5, 3, 0.1), plotit = TRUE)
jobslm2 = lm(I(job_seek^2) ~ treat + econ_hard + depress1 + sex + age + occp + marital + 
               nonwhite + educ + income, data = jobs)
hist(jobslm$residuals, xlab = "residual", ylab = "", 
     main = "job_seek", font.main = 1)
hist(jobslm2$residuals, , xlab = "residual", ylab = "", 
     main = "job_seek^2", font.main = 1)
dev.off()



penndata = read.table("pennbonus.txt")
pdf("boxcox_penn.pdf", height = 4, width = 8.5)
par(mfrow = c(1, 3))
pennlm = lm(duration ~ ., data = penndata)
boxcox(pennlm, lambda = seq(0.2, 0.4, 0.05), plotit = TRUE)

pennlm.3 = lm(I(duration^(0.3)) ~., data = penndata)

hist(pennlm$residuals, xlab = "residual", ylab = "", 
     main = "duration", font.main = 1)
hist(pennlm.3$residuals, xlab = "residual", ylab = "", 
     main = "duration^0.3", font.main = 1)
dev.off()





## polynomial regression
library(foreign)
census00 = read.dta("census00.dta")
head(census00)

census00ols1 = lm(logwk ~ educ + exper + black, 
                  data = census00)
census00ols2 = lm(logwk ~ educ + exper + I(exper^2) + black, 
                  data = census00)
round(summary(census00ols1)$coef, 4)
round(summary(census00ols2)$coef, 4)



## generalized additive model
library(mgcv)
n = 1000
dat = data.frame(x <- seq(0, 1, length.out = n),
                 true <- sin(x*10),
                 y <- true + rnorm(n))
np.fit = gam(y ~ s(x), data = dat)
pdf("npregbasis.pdf", height = 4, width = 6)
plot(y ~ x, data = dat, bty = "n",
     pch = 19, cex = 0.1, col = "grey")
lines(true ~ x, col = "grey") 
lines(np.fit$fitted.values ~ x, lty = 2)
legend("bottomright", c("true", "estimated"), 
       lty = 1:2, col = c("grey", "black"), 
       bty = "n")
dev.off()


census00gam = gam(logwk ~ s(educ) + s(exper) + black, 
                  data = census00) 
summary(census00gam)
pdf("gam_wage.pdf", height = 6, width = 8)
par(mfrow = c(1, 2))
plot(census00gam, bty = "n")
dev.off()




## plots for regression discontinuity and regression kink
library(ggplot2)
## regression discontinuity
n = 1000
x = runif(n, 0, 100)
x0 = 50
ylower  = x/100 + rnorm(n, 0, 0.2)
yhigher = x/50 + rnorm(n, 0, 0.2)
y = ifelse(x < x0, ylower, yhigher)

dat_disc = data.frame(factor = "regression discontinuity",
                      x = x,
                      y = y,
                      x0 = x0)
true_disc1 = data.frame(factor = "regression discontinuity",
                        x = seq(0, x0, 0.1),
                        y = seq(0, x0, 0.1)/100)
true_disc2 = data.frame(factor = "regression discontinuity",
                        x = seq(x0, 100, 0.1),
                        y = seq(x0, 100, 0.1)/50)



## regression kink
n = 1000
x = runif(n, 0, 100)
x0 = 50
ylower  = x/100 + rnorm(n, 0, 0.2)
yhigher = -0.5 + x/50 + rnorm(n, 0, 0.2)
y = ifelse(x < x0, ylower, yhigher)

dat_kink = data.frame(factor = "regression kink",
                      x = x,
                      y = y,
                      x0 = x0)
true_kink1 = data.frame(factor = "regression kink",
                        x = seq(0, x0, 0.1),
                        y = seq(0, x0, 0.1)/100)
true_kink2 = data.frame(factor = "regression kink",
                        x = seq(x0, 100, 0.1),
                        y = seq(x0, 100, 0.1)/50 - 0.5)

dat_ggplot = rbind(dat_disc, dat_kink)
true1_ggplot = rbind(true_disc1, true_kink1)
true2_ggplot = rbind(true_disc2, true_kink2)


ggplot(dat_ggplot) + 
  geom_point(aes(x = x, y = y), 
             pch = 19, cex = 0.3, col = "grey", alpha = 0.5) +
  geom_line(data = true1_ggplot,  aes(x= x, y = y)) +
  geom_line(data = true2_ggplot,  aes(x= x, y = y)) +
  facet_wrap(~ factor, scales = "free_y") +
  geom_vline(xintercept = x0, linetype = 2, alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
ggsave("regressiondiscontinuitykink_ggplot.pdf", 
       height = 4, width = 6)


