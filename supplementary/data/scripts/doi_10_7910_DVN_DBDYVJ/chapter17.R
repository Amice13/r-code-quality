rm(list = ls())


## scale dependence and removable interaction
n  = 1000
x1 = rnorm(n)
x2 = rnorm(n)
y  = exp(x1 + x2 + rnorm(n))
ols.fit = lm(log(y) ~ x1*x2)
summary(ols.fit)
ols.fit = lm(y ~ x1*x2)
summary(ols.fit)


## main effect in the presence of interaction
## data from "https://stats.idre.ucla.edu/stat/data/hsbdemo.dta"
## socst = social studies score
hsbdemo = read.table("hsbdemo.txt")
ols.fit = lm(read ~ math + socst, data = hsbdemo)
summary(ols.fit)
ols.fit = lm(read ~ math*socst, data = hsbdemo)
summary(ols.fit)



## centering regressors
hsbdemo$math.c = hsbdemo$math - mean(hsbdemo$math)
hsbdemo$socst.c = hsbdemo$socst - mean(hsbdemo$socst)
ols.fit = lm(read ~ math.c*socst.c, data = hsbdemo)
summary(ols.fit)





