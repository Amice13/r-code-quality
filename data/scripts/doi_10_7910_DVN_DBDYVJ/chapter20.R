rm(list = ls())


## Figure 20.1
z  = seq(-8, 8, 0.1)

pdf("binomiallinkfunctions.pdf", height = 5, width = 8.5)
par(mfrow = c(1, 2))
gz = pnorm(z)
plot(gz ~ z, type = "l", lty = 1, bty = "n", 
     xlab = expression(z),
     ylab = expression(g(z)),
     main = "distribution", font.main = 1)
gz = 1/(1 + exp(-z))
lines(gz ~ z, lty = 2)
gz = 1 - exp(-exp(z))
lines(gz ~ z, lty = 3)
gz = pcauchy(z)
lines(gz ~ z, lty = 4)
legend("topleft", 
       c("probit", "logit", "cloglog", "cauchit"), 
       lty = 1:4, bty = "n")

dgz = dnorm(z)
plot(dgz ~ z, type = "l", lty = 1, bty = "n", 
     xlab = expression(z),
     ylab = expression(dg(z)/dz),
     main = "density", font.main = 1)
gz  = 1/(1 + exp(-z))
dgz = gz*(1 - gz)
lines(dgz ~ z, lty = 2)
dgz = exp(z - exp(z))
lines(dgz ~ z, lty = 3)
dgz = dcauchy(z)
lines(dgz ~ z, lty = 4)
legend("topleft", 
       c("probit", "logit", "cloglog", "cauchit"), 
       lty = 1:4, bty = "n")
dev.off()



## Chapter 20.3.1
flu = read.table("fludata.txt", header = TRUE)
flu = within(flu, rm(receive))
assign.logit = glm(outcome ~ ., 
                   family  = binomial(link = logit), 
                   data    = flu)
summary(assign.logit)


## joint test
pchisq(assign.logit$null.deviance - assign.logit$deviance,
       df = assign.logit$df.null - assign.logit$df.residual,
       lower.tail = FALSE)



## Chapter 20.3.2
## prediction
emp.mean = apply(flu, 2, mean)
data.ave = rbind(emp.mean, emp.mean)
data.ave[1, 1] = 1
data.ave[2, 1] = 0
data.ave = data.frame(data.ave)
data.ave
predict(assign.logit, newdata = data.ave,
        type = "response", se.fit = TRUE)


## Chapter 20.4
## marginal effects
library("margins")
ape = margins(assign.logit)
summary(ape)



## Chapter 20.5
## misspecification of the link function
n  = 100
x  = rnorm(n, 0, 3)
prob = 1/(1 + exp(-1 + x))
y  = rbinom(n, 1, prob)
lpmfit    = lm(y ~ x)
probitfit = glm(y ~ x, family = binomial(link = "probit"))
logitfit  = glm(y ~ x, family = binomial(link = "logit"))
cloglogfit= glm(y ~ x, family = binomial(link = "cloglog"))
cauchitfit= glm(y ~ x, family = binomial(link = "cauchit"))

## coefficients
betacoef = c(lpmfit$coef[2], 
             probitfit$coef[2], 
             logitfit$coef[2], 
             cloglogfit$coef[2], 
             cauchitfit$coef[2]) 
names(betacoef) = c("lpm", "probit", "logit", "cloglog", "cauchit")
round(betacoef, 2)


## prediction
table(y, lpmfit$fitted.values>0.5)
table(y, probitfit$fitted.values>0.5)
table(y, logitfit$fitted.values>0.5)
table(y, cloglogfit$fitted.values>0.5)
table(y, cauchitfit$fitted.values>0.5)


## fitted probabilities
dat = rbind(data.frame(link  = "lpm",
                       pihat = lpmfit$fitted.values,
                       pi0   = prob),
            data.frame(link  = "probit",
                       pihat = probitfit$fitted.values,
                       pi0   = prob),
            data.frame(link  = "logit",
                       pihat = logitfit$fitted.values,
                       pi0   = prob),
            data.frame(link  = "cloglog",
                       pihat = cloglogfit$fitted.values,
                       pi0   = prob),
            data.frame(link  = "cauchit",
                       pihat = cauchitfit$fitted.values,
                       pi0   = prob))

library(ggplot2)
ggplot(dat) + 
  geom_point(aes(x = pi0, y = pihat), 
             col = "grey", cex = 0.5) + 
  geom_abline(aes(intercept = 0, slope = 1), alpha = 0.7) + 
  facet_grid(~link) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  xlab("true probability") + 
  ylab("fitted probability") + 
  ggtitle("the true link is logit")
ggsave("fittedprobabilities.pdf", height = 4, width = 8.5)




## compare different link functions
d.logit.probit = function(b){
  x = seq(-20, 20, 0.00001)
  max(abs(plogis(b*x) - pnorm(x)))
}
optimize(d.logit.probit, c(-10, 10))

d.logit.cauchit = function(b){
  x = seq(-20, 20, 0.00001)
  max(abs(plogis(b*x) - pcauchy(x)))
}
optimize(d.logit.cauchit, c(-10, 10))

f.cloglog = function(z){
  1 - exp(-exp(z))  
}
d.logit.cloglog = function(b){
  x = seq(-20, 20, 0.00001)
  max(abs(plogis(b*x) - f.cloglog(x)))
}
optimize(d.logit.cloglog, c(-10, 10))



## Chapter 20.6.2
## "pool" means pooled logistic regression
## a homework problem asks for stratified logistic regression
dat  = read.csv("samarani.csv")
pool.glm = glm(case_comb ~ ds1 + ds2 + ds3 + ds4_a + 
                 ds4_b + ds5 + ds1_3 + center,
               family = binomial(link = logit),
               data = dat)
summary(pool.glm)


