rm(list = ls())


library("Matching")
data(lalonde)
ols.fit = lm(re78 ~ ., y = TRUE, data = lalonde)
ols.summary = summary(ols.fit)
r2 = ols.summary$r.squared
all.equal(r2, (cor(ols.fit$y, ols.fit$fitted.values))^2,
          check.names = FALSE)

fstat = ols.summary$fstatistic
all.equal(fstat[1], fstat[3]/fstat[2]*r2/(1-r2),
          check.names = FALSE)





library(foreign)
dat = read.dta("isq.dta")
dat = na.omit(dat[,c("multish", "lnpop", "lnpopsq", 
                     "lngdp", "lncolony", "lndist", 
                     "freedom", "militexp", "arms", 
                     "year83", "year86", "year89", "year92")])

ols.fit = lm(log(multish + 1) ~ lnpop + lnpopsq + lngdp +  lncolony 
             + lndist + freedom + militexp + arms 
             + year83 + year86 + year89 + year92, 
             y = TRUE, data=dat)
ols.summary = summary(ols.fit)
r2 = ols.summary$r.squared
all.equal(r2, (cor(ols.fit$y, ols.fit$fitted.values))^2,
          check.names = FALSE)

fstat = ols.summary$fstatistic
all.equal(fstat[1], fstat[3]/fstat[2]*r2/(1-r2),
          check.names = FALSE)

