# set working directory
# wd <- ""
setwd(wd)

# open the cross sectional database
library(haven)
d <- read_dta("Data/venality_cs.dta")

# make Figure 4
library(dfadjust)

# estimate regression
f = lm(widespread ~ protestant + battles + urbanpop + absolutist, data = d)
fSE = dfadjustSE(model = f)

i = lm(widespread ~ battles + urbanpop + absolutist, data = d)
ii = lm(protestant ~ battles + urbanpop + absolutist, data = d)

partial_y = resid(i)
partial_x = resid(ii)

plot(y = partial_y,
     x = partial_x,
     xlab = "Protestant | Controls",
     ylab = "Widespread venality | Controls",
     col = "white")
abline(lm(partial_y ~ partial_x), col = "black", lwd = 2)
text(y = partial_y, x = partial_x, label = d$country_name, cex = 0.75)
text(y = 0.55, x = 0.57, "p-value (adjusted) = 0.085", col = "black", cex = 0.8)

