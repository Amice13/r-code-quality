# set working directory
# wd <- ""
setwd(wd)

# open the cross sectional database
library(haven)
d <- read_dta("Data/simple_cs.dta")


# make Figure 2
library(dfadjust)

a2 = lm(v2strenadm ~ protestant + urbanpop + battles + absolutist, data = d)
aSE2 = dfadjustSE(model = a2)

b2 = lm(v2stcritrecadm ~ protestant + urbanpop + battles + absolutist, data = d)
bSE2 = dfadjustSE(model = b2)

c2 = lm(v3struinvadm ~ protestant + urbanpop + battles + absolutist, data = d)
cSE2 = dfadjustSE(model = c2)

e2 = lm(v2clrspct ~ protestant + urbanpop + battles + absolutist, data = d)
eSE2 = dfadjustSE(model = e2)


# adjust p-values for multiple testing
p = c(0.008530154,0.03075428,0.21533464,0.8579725)
p.adjust(p, method="holm")

# partial regression plots
partial <- lm(protestant ~ urbanpop + battles + absolutist, data = d)
res <- resid(partial)

par(mfrow=c(2,2))
par(mar=c(5,4,1,4))
plot(y = d$v2strenadm,
     x = res,
     cex = 0.1,
     xlim = c(-1,1),
     ylim = c(-3,3),
     ylab = "Salaried",
     xlab = "Protestant | Controls")

abline(lm(d$v2strenadm ~ res), col = "black", lwd =2)
text(y = d$v2strenadm, x = res, label = d$country_name, cex = 0.75)
text(y = -2.9, x = 0.50, 
     "p-value (adjusted) = 0.008", 
     cex = 0.7,
     col = "black")

plot(y = d$v2stcritrecadm,
     x = res,
     cex = 0.1,
     xlim = c(-1,1),
     ylim = c(-3,3),
     ylab = "Meritocratic",
     xlab = "Protestant | Controls")
abline(lm(d$v2stcritrecadm ~ res), col = "black", lwd = 2)
text(y = d$v2stcritrecadm, x = res, label = d$country_name, cex = 0.75)
text(y = -2.9, x = 0.50, 
     "p-value (adjusted) = 0.030", 
     cex = 0.7,
     col = "black")

plot(y = d$v3struinvadm,
     x = res,
     cex = 0.1,
     xlim = c(-1,1),
     ylim = c(-3,3),
     ylab = "Autonomous",
     xlab = "Protestant | Controls")
abline(lm(d$v3struinvadm ~ res), col = "black", lwd = 2)
text(y = d$v3struinvadm, x = res, label = d$country_name, cex = 0.75)
text(y = -2.9, x = 0.50, 
     "p-value (adjusted) = 0.215", 
     cex = 0.7,
     col = "black")

plot(y = d$v2clrspct,
     x = res,
     cex = 0.1,
     xlim = c(-1,1),
     ylim = c(-3,3),
     ylab = "Impartial",
     xlab = "Protestant | Controls")
abline(lm(d$v2clrspct ~ res), col = "black", lwd = 2)
text(y = d$v2clrspct, x = res, label = d$country_name, cex = 0.75)
text(y = -2.9, x = 0.50, 
     "p-value (adjusted) = 0.857", 
     cex = 0.7,
     col = "black")

