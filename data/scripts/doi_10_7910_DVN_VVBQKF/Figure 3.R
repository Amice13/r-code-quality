# set working directory
# wd <- ""
setwd(wd)

# open the cross sectional database
library(haven)
d <- read_dta("Data/simple_cs.dta")


# make Figure 3
library(dfadjust)

# Leave one out regressions
# plot the coefficients
par(mfrow=c(2,2))

coefs_salaried = NA
coefs_meritocratic = NA
se_salaried = NA
se_meritocratic = NA

for(i in 1:13){
  
  LO = d$country_name[i]
  salariedLO = lm(v2strenadm ~ protestant, data = d[d$country_name!=LO,])
  meritocraticLO = lm(v2stcritrecadm ~ protestant, data = d[d$country_name!=LO,])
  
  salariedSE = dfadjustSE(model = salariedLO)
  meritocraticSE = dfadjustSE(model = meritocraticLO)
  
  se_salaried[i] <- salariedSE$coefficients[2,3]*(qt(0.95, df = salariedSE$coefficients[2,5])/qnorm(0.95))
  se_meritocratic[i] <- meritocraticSE$coefficients[2,3]*(qt(0.95, df = meritocraticSE$coefficients[2,5])/qnorm(0.95))
  
  coefs_salaried[i] <- salariedSE$coefficients[2,1]
  coefs_meritocratic[i] <- meritocraticSE$coefficients[2,1]
  
}

w_salaried = cbind.data.frame(d$country_name,
                              coefs_salaried,
                              se_salaried)

w_salaried = w_salaried[order(-w_salaried$coefs_salaried),]

par(mar=c(3,8,3,3))
plot(y = 1:13, x = w_salaried$coefs_salaried, 
     ylim=c(0,13),
     xlim = c(-2,5),
     pch=16,
     cex=1.5,
     main = "Salaried (no controls)", 
     ylab="", 
     axes = FALSE,
     xlab="Protestant premium")
box()


for(i in 1:13){
  lines(y=c(i,i), 
        x=c((w_salaried$coefs_salaried[i]-qnorm(0.95)*w_salaried$se_salaried[i]),(w_salaried$coefs_salaried[i]+qnorm(0.95)*w_salaried$se_salaried[i])), 
        lwd=2)
}

abline(v=0, col="black", lty=2)
abline(v=a$coefficients[2], col ="black", lty = 3)

axis(1)
axis(2, at=1:13,
     labels=w_salaried$`d$country_name`,
     las = 2,
     cex = 0.5)


w_meritocratic = cbind.data.frame(d$country_name,
                                  coefs_meritocratic,
                                  se_meritocratic)
w_meritocratic = subset(w_meritocratic, w_meritocratic$`d$country_name`!="Hungary")

w_meritocratic = w_meritocratic[order(-w_meritocratic$coefs_meritocratic),]


par(mar=c(3,8,3,3))
plot(y = 1:12, x = w_meritocratic$coefs_meritocratic, 
     ylim=c(0,12),
     xlim = c(-3,7),
     pch=16,
     cex=1.5,
     main = "Meritocratic (no controls)", 
     ylab="", 
     axes = FALSE,
     xlab="Protestant premium")
box()
# and plot lines for the confidence intervals
for(i in 1:12){
  lines(y=c(i,i), 
        x=c((w_meritocratic$coefs_meritocratic[i]-qnorm(0.95)*w_meritocratic$se_meritocratic[i]),(w_meritocratic$coefs_meritocratic[i]+qnorm(0.95)*w_meritocratic$se_meritocratic[i])), 
        lwd=2)
}
# plot a horizontal line representing a zero average treatment effect
abline(v=0, col="black", lty=2)
abline(v=b$coefficients[2], col ="black", lty = 3)
# draw the x and y axes
axis(1)
axis(2, at=1:12,
     labels=w_meritocratic$`d$country_name`,
     las = 2,
     cex = 0.5)


# full controls

coefs_salaried = NA
coefs_meritocratic = NA

se_salaried = NA
se_meritocratic = NA


for(i in 1:13){
  
  LO = d$country_name[i]
  salariedLO = lm(v2strenadm ~ protestant + battles + urbanpop + absolutist, data = d[d$country_name!=LO,])
  meritocraticLO = lm(v2stcritrecadm ~ protestant + battles + urbanpop + absolutist, data = d[d$country_name!=LO,])
  
  salariedSE = dfadjustSE(model = salariedLO)
  meritocraticSE = dfadjustSE(model = meritocraticLO)
  
  se_salaried[i] <- salariedSE$coefficients[2,3]*(qt(0.95, df = salariedSE$coefficients[2,5])/qnorm(0.95))
  se_meritocratic[i] <- meritocraticSE$coefficients[2,3]*(qt(0.95, df = meritocraticSE$coefficients[2,5])/qnorm(0.95))
  
  coefs_salaried[i] <- salariedSE$coefficients[2,1]
  coefs_meritocratic[i] <- meritocraticSE$coefficients[2,1]
  
}

w_salaried = cbind.data.frame(d$country_name,
                              coefs_salaried,
                              se_salaried)

w_salaried = w_salaried[order(-w_salaried$coefs_salaried),]

par(mar=c(3,8,3,3))
plot(y = 1:13, x = w_salaried$coefs_salaried, 
     ylim=c(0,13),
     xlim = c(-2,5),
     pch=16,
     cex=1.5,
     main = "Salaried (all controls)", 
     ylab="", 
     axes = FALSE,
     xlab="Protestant premium")
box()


for(i in 1:13){
  lines(y=c(i,i), 
        x=c((w_salaried$coefs_salaried[i]-qnorm(0.95)*w_salaried$se_salaried[i]),(w_salaried$coefs_salaried[i]+qnorm(0.95)*w_salaried$se_salaried[i])), 
        lwd=2)
}

abline(v=0, col="black", lty=2)
abline(v=a2$coefficients[2], col ="black", lty = 3)

axis(1)
axis(2, at=1:13,
     labels=w_salaried$`d$country_name`,
     las = 2,
     cex = 0.5)


w_meritocratic = cbind.data.frame(d$country_name,
                                  coefs_meritocratic,
                                  se_meritocratic)
w_meritocratic = subset(w_meritocratic, w_meritocratic$`d$country_name`!="Hungary")

w_meritocratic = w_meritocratic[order(-w_meritocratic$coefs_meritocratic),]


par(mar=c(3,8,3,3))
plot(y = 1:12, x = w_meritocratic$coefs_meritocratic, 
     ylim=c(0,12),
     xlim = c(-3,7),
     pch=16,
     cex=1.5,
     main = "Meritocratic (all controls)", 
     ylab="", 
     axes = FALSE,
     xlab="Protestant premium")
box()
# and plot lines for the confidence intervals
for(i in 1:12){
  lines(y=c(i,i), 
        x=c((w_meritocratic$coefs_meritocratic[i]-qnorm(0.95)*w_meritocratic$se_meritocratic[i]),(w_meritocratic$coefs_meritocratic[i]+qnorm(0.95)*w_meritocratic$se_meritocratic[i])), 
        lwd=2)
}
# plot a horizontal line representing a zero average treatment effect
abline(v=0, col="black", lty=2)
abline(v=b2$coefficients[2], col ="black", lty = 3)
# draw the x and y axes
axis(1)
axis(2, at=1:12,
     labels=w_meritocratic$`d$country_name`,
     las = 2,
     cex = 0.5)
