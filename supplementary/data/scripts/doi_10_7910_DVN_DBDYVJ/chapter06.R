rm(list = ls())
library(car)
library(ggplot2)



## Chapter 6.1.1
## covariates and beta
n     = 200
x     = runif(n, -2, 2)
beta  = 1
xbeta = x*beta 

 
## homoskedastic normal errors
Simu1  = replicate(5000,
                  {y = xbeta + rnorm(n)
                  ols.fit = lm(y ~ x)
                  c(summary(ols.fit)$coef[2, 1:2],
                    sqrt(hccm(ols.fit)[2, 2]))
                  })

## homoskedastic non-normal errors
Simu2  = replicate(5000,
                  {y = xbeta + rexp(n)
                  ols.fit = lm(y ~ x)
                  c(summary(ols.fit)$coef[2, 1:2],
                    sqrt(hccm(ols.fit)[2, 2]))
                  })
## heteroskedastic normal errors
Simu3  = replicate(5000,
                  {y = xbeta + rnorm(n, 0, abs(x))
                  ols.fit = lm(y ~ x)
                  c(summary(ols.fit)$coef[2, 1:2],
                    sqrt(hccm(ols.fit)[2, 2]))
                  })

## heteroskedastic non-normal errors
Simu4  = replicate(5000,
                  {y = xbeta + runif(n, -x^2, x^2)
                  ols.fit = lm(y ~ x)
                  c(summary(ols.fit)$coef[2, 1:2],
                    sqrt(hccm(ols.fit)[2, 2]))
                  })


point.est = rbind(data.frame(factor1 = "homoskedastic", 
                             factor2 = "Normal",
                             estimate = Simu1[1, ]),
                  data.frame(factor1 = "homoskedastic", 
                             factor2 = "non-Normal",
                             estimate = Simu2[1, ]),
                  data.frame(factor1 = "heteroskedastic", 
                             factor2 = "Normal",
                             estimate = Simu3[1, ]),
                  data.frame(factor1 = "heteroskedastic", 
                             factor2 = "non-Normal",
                             estimate = Simu4[1, ]))

Serror = rbind(data.frame(factor1 = "homoskedastic", 
                          factor2 = "Normal",
                          se0     = sd(Simu1[1, ]),
                          se1     = mean(Simu1[2, ]),
                          se2     = mean(Simu1[3, ])),
               data.frame(factor1 = "homoskedastic", 
                          factor2 = "non-Normal",
                          se0     = sd(Simu2[1, ]),
                          se1     = mean(Simu2[2, ]),
                          se2     = mean(Simu2[3, ])),
               data.frame(factor1 = "heteroskedastic", 
                          factor2 = "Normal",
                          se0     = sd(Simu3[1, ]),
                          se1     = mean(Simu3[2, ]),
                          se2     = mean(Simu3[3, ])),
               data.frame(factor1 = "heteroskedastic", 
                          factor2 = "non-Normal",
                          se0     = sd(Simu4[1, ]),
                          se1     = mean(Simu4[2, ]),
                          se2     = mean(Simu4[3, ])))

xx = seq(-3, 3, 0.001)
Dnormal = rbind(data.frame(factor1 = "homoskedastic", 
                           factor2 = "Normal",
                           xx = xx, 
                           yy = dnorm(xx, 1, sd(Simu1[1, ]))),
                data.frame(factor1 = "homoskedastic", 
                           factor2 = "non-Normal",
                           xx = xx,
                           yy = dnorm(xx, 1, sd(Simu2[1, ]))),
                data.frame(factor1 = "heteroskedastic", 
                           factor2 = "Normal",
                           xx = xx,
                           yy = dnorm(xx, 1, sd(Simu3[1, ]))),
                data.frame(factor1 = "heteroskedastic", 
                           factor2 = "non-Normal",
                           xx = xx,
                           yy = dnorm(xx, 1, sd(Simu4[1, ]))))


ggplot(point.est) + 
    geom_histogram(aes(x = estimate, y=..density..),
                   fill = "grey", alpha = 0.7) +
    geom_line(data = Dnormal, aes(x= xx, y = yy), alpha = 0.7) +
    xlim(0.6, 1.4) + 
    geom_vline(xintercept = 1, linetype = 2, alpha = 0.7) + 
    facet_grid(factor1 ~ factor2, scale  = "free_y") +
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    xlab(expression(hat(beta))) +
    geom_text(data = Serror, alpha = 0.8, 
              aes(x = 1.2, y = Inf, hjust = 0, vjust = 1.1,
                  label = paste0("se0=", round(se0, 3), "\n",
                                 "se1=", round(se1, 3), "\n",
                                 "se2=", round(se2, 3), "\n")))  

ggsave("asymptoticinference_ggplot.pdf", height = 6.5, width = 6.5)






## Chapter 6.5
library("Matching")
data(lalonde)
ols.fit = lm(re78 ~ ., data = lalonde)
ols.fit.hc0 = sqrt(diag(hccm(ols.fit, type = "hc0")))
ols.fit.hc1 = sqrt(diag(hccm(ols.fit, type = "hc1")))
ols.fit.hc2 = sqrt(diag(hccm(ols.fit, type = "hc2")))
ols.fit.hc3 = sqrt(diag(hccm(ols.fit, type = "hc3")))
ols.fit.hc4 = sqrt(diag(hccm(ols.fit, type = "hc4")))
ols.fit.coef =summary(ols.fit)$coef
tvalues = ols.fit.coef[,1]/
  cbind(ols.fit.coef[,2], ols.fit.hc0, ols.fit.hc1, 
        ols.fit.hc2, ols.fit.hc3, ols.fit.hc4)
colnames(tvalues) = c("ols", "hc0", "hc1", "hc2", "hc3", "hc4")
round(tvalues, 2)





library(foreign)
dat = read.dta("isq.dta")
dat = na.omit(dat[,c("multish", "lnpop", "lnpopsq", 
                     "lngdp", "lncolony", "lndist", 
                     "freedom", "militexp", "arms", 
                     "year83", "year86", "year89", "year92")])
ols.fit = lm(multish ~ lnpop + lnpopsq + lngdp +  lncolony 
             + lndist + freedom + militexp + arms 
             + year83 + year86 + year89 + year92, data=dat)
ols.fit.hc0 = sqrt(diag(hccm(ols.fit, type = "hc0")))
ols.fit.hc1 = sqrt(diag(hccm(ols.fit, type = "hc1")))
ols.fit.hc2 = sqrt(diag(hccm(ols.fit, type = "hc2")))
ols.fit.hc3 = sqrt(diag(hccm(ols.fit, type = "hc3")))
ols.fit.hc4 = sqrt(diag(hccm(ols.fit, type = "hc4")))
ols.fit.coef =summary(ols.fit)$coef
tvalues = ols.fit.coef[,1]/
  cbind(ols.fit.coef[,2], ols.fit.hc0, ols.fit.hc1, 
        ols.fit.hc2, ols.fit.hc3, ols.fit.hc4)
colnames(tvalues) = c("ols", "hc0", "hc1", "hc2", "hc3", "hc4")
round(tvalues, 2)



## log transformation of the outcome - See also Chapter 16
ols.fit = lm(log(multish + 1) ~ lnpop + lnpopsq + lngdp +  lncolony 
             + lndist + freedom + militexp + arms 
             + year83 + year86 + year89 + year92, data=dat)
ols.fit.hc0 = sqrt(diag(hccm(ols.fit, type = "hc0")))
ols.fit.hc1 = sqrt(diag(hccm(ols.fit, type = "hc1")))
ols.fit.hc2 = sqrt(diag(hccm(ols.fit, type = "hc2")))
ols.fit.hc3 = sqrt(diag(hccm(ols.fit, type = "hc3")))
ols.fit.hc4 = sqrt(diag(hccm(ols.fit, type = "hc4")))
ols.fit.coef =summary(ols.fit)$coef
tvalues = ols.fit.coef[,1]/
  cbind(ols.fit.coef[,2], ols.fit.hc0, ols.fit.hc1, 
        ols.fit.hc2, ols.fit.hc3, ols.fit.hc4)
colnames(tvalues) = c("ols", "hc0", "hc1", "hc2", "hc3", "hc4")
round(tvalues, 2)




library("mlbench")
data(BostonHousing)
ols.fit = lm(medv ~ ., data = BostonHousing)
summary(ols.fit)

ols.fit.hc0 = sqrt(diag(hccm(ols.fit, type = "hc0")))
ols.fit.hc1 = sqrt(diag(hccm(ols.fit, type = "hc1")))
ols.fit.hc2 = sqrt(diag(hccm(ols.fit, type = "hc2")))
ols.fit.hc3 = sqrt(diag(hccm(ols.fit, type = "hc3")))
ols.fit.hc4 = sqrt(diag(hccm(ols.fit, type = "hc4")))
ols.fit.coef =summary(ols.fit)$coef
tvalues = ols.fit.coef[,1]/
  cbind(ols.fit.coef[,2], ols.fit.hc0, ols.fit.hc1, 
        ols.fit.hc2, ols.fit.hc3, ols.fit.hc4)
colnames(tvalues) = c("ols", "hc0", "hc1", "hc2", "hc3", "hc4")
round(tvalues, 2)


## log transformation of the outcome - See also Chapter 16
ols.fit = lm(log(medv) ~ ., data = BostonHousing)
summary(ols.fit)

ols.fit.hc0 = sqrt(diag(hccm(ols.fit, type = "hc0")))
ols.fit.hc1 = sqrt(diag(hccm(ols.fit, type = "hc1")))
ols.fit.hc2 = sqrt(diag(hccm(ols.fit, type = "hc2")))
ols.fit.hc3 = sqrt(diag(hccm(ols.fit, type = "hc3")))
ols.fit.hc4 = sqrt(diag(hccm(ols.fit, type = "hc4")))
ols.fit.coef =summary(ols.fit)$coef
tvalues = ols.fit.coef[,1]/
  cbind(ols.fit.coef[,2], ols.fit.hc0, ols.fit.hc1, 
        ols.fit.hc2, ols.fit.hc3, ols.fit.hc4)
colnames(tvalues) = c("ols", "hc0", "hc1", "hc2", "hc3", "hc4")
round(tvalues, 2)



