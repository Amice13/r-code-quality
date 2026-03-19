# set working directory
# wd <- ""
setwd(wd)

# load database
library(haven)
d <- read_dta("Data/oxford_graduates.dta")
d <- subset(d, d$year>=1500 & d$year<1715)

# estimate regression
library(lmtest)
library(sandwich)

lm1 <- lm(state ~ law + church, data = d)

coeftest(lm1, vcov = vcovHC(lm1, "HC1")) 
coefs <- lm1$coefficients[2:3]
CIs = confint(coeftest(lm1, vcov = vcovHC(lm1, "HC1")) , level = 0.95)
CIs = CIs[2:3,]

# produce coefficient plot
plot(1:2, coefs, 
     xlim=c(0.5,2.5),
     ylim=c(-0.002,0.015),
     pch=16,
     cex=1,
     main = "", 
     xlab="", 
     ylab="Marginal effect on state employment", 
     axes=FALSE)
box()
# and plot lines for the confidence intervals
for(i in 1:2){
  lines(c(i,i), CIs[i,], lwd=2)
}
# plot a horizontal line representing a zero average treatment effect
abline(h=0, col="black", lty=2)
# draw the x and y axes
axis(2)
axis(1, at=1:2, 
     labels=c("Law", "Church")) 
