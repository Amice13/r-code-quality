# set working directory
# wd <- ""
setwd(wd)

# open the cross sectional database
library(haven)
d <- read_dta("Data/simple_cs.dta")

# estimate simple regressions

# make Table 3

library(dfadjust)

# salaried employees
a = lm(v2strenadm ~ protestant, data = d)
aSE = dfadjustSE(model = a)

#meritocratic recruitment
b = lm(v2stcritrecadm ~ protestant, data = d)
bSE = dfadjustSE(model = b)

#autonomy from ruler
c = lm(v3struinvadm ~ protestant, data = d)
cSE = dfadjustSE(model = c)

# impartial
e = lm(v2clrspct ~ protestant, data = d)
eSE = dfadjustSE(model = e)

seA = aSE$coefficients[,4]
seB = bSE$coefficients[,4]
seC = cSE$coefficients[,4]
seE = eSE$coefficients[,4]

library(stargazer)
stargazer(a,b,c,e,
          se = list(seA,seB,seC,seE))
