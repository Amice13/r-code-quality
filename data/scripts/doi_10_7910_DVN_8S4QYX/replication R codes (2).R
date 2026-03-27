##############################################
### "Public Preferences for Reallocating Aid in the Presence of Alternative Donors"
### Replication R codes for mediation analyses
##############################################
library(mediation)
library(stargazer)
library(car)
##############################################
LOWER <- function(vec){  
  lower <- quantile(vec, probs = seq(0, 1, 0.025))[[2]]
  return(lower)
} 
UPPER <- function(vec){
  upper <- quantile(vec, probs = seq(0, 1, 0.025))[[40]]
  return(upper)
} 
##############################################
data.jp <- read.csv("data.jp.csv")

data.us <- read.csv("data.us.csv")
##############################################
data.jp.africa <- data.jp[data.jp$africa == 1, ] ## from Africa to Southeast Asia
data.jp.asia <- data.jp[data.jp$asia == 1, ]
##############################################
data.us.af.la <- data.us[data.us$AF.LA == 1, ] ## from Africa to Latin America
data.us.af.se <- data.us[data.us$AF.SE == 1, ]
data.us.la.af <- data.us[data.us$LA.AF == 1, ]
data.us.la.se <- data.us[data.us$LA.SE == 1, ]
data.us.se.af <- data.us[data.us$SE.AF == 1, ]
data.us.se.la <- data.us[data.us$SE.LA == 1, ]
##############################################







##############################################
### Table A6: Effects of the Treatments on the Mediators in the Japan Survey
##############################################
MODEL <- list(NA, 4)
SE <- list(NA, 4)
##############################################
model.japan.med1 <- lm(MEDECON ~ friend + enemy
                        + imp.africa + imp.asia + male + age + factor(resid) + factor(income) + factor(educ)
                      , data = data.jp)

summary(model.japan.med1)

MODEL[[1]] <- model.japan.med1
SE[[1]] <- sqrt(diag(vcovHC(model.japan.med1)))  
##############################################
model.japan.med2 <- lm(MEDNS ~ friend + enemy
                        + imp.africa + imp.asia + male + age + factor(resid) + factor(income) + factor(educ)
                      , data = data.jp)

summary(model.japan.med2)

MODEL[[2]] <- model.japan.med2
SE[[2]] <- sqrt(diag(vcovHC(model.japan.med2)))  
##############################################
model.japan.med3 <- lm(MEDREP ~ friend + enemy
                        + imp.africa + imp.asia + male + age + factor(resid) + factor(income) + factor(educ)
                      , data = data.jp)

summary(model.japan.med3)

MODEL[[3]] <- model.japan.med3
SE[[3]] <- sqrt(diag(vcovHC(model.japan.med3)))  
##############################################
model.japan.med4 <- lm(MEDDEV ~ friend + enemy
                        + imp.africa + imp.asia + male + age + factor(resid) + factor(income) + factor(educ)
                      , data = data.jp)

summary(model.japan.med4)

MODEL[[4]] <- model.japan.med4
SE[[4]] <- sqrt(diag(vcovHC(model.japan.med4)))  
##############################################
stargazer(MODEL[[1]], MODEL[[2]], MODEL[[3]], MODEL[[4]], se = list(SE[[1]], SE[[2]], SE[[3]], SE[[4]]), star.char = c("*", "**", "***"), star.cutoffs = c(.05, .01, .001))
##############################################




##############################################
### Table A8: Effects of the Treatments on the Mediators in the US Survey
##############################################
MODEL <- list(NA, 4)
SE <- list(NA, 4)
##############################################
model <- lm(medecon ~ japan + uk + china
            + age + factor(gender) + factor(educ) + factor(division) + factor(profile_gross_household)
            + imp.af  + imp.la + imp.se, data = data.us)
summary(model)
MODEL[[1]] <- model
SE[[1]] <- sqrt(diag(vcovHC(model)))
model.us.med1 <- model


model <- lm(medns ~ japan + uk + china
            + age + factor(gender) + factor(educ) + factor(division) + factor(profile_gross_household)
            + imp.af  + imp.la + imp.se, data = data.us)
summary(model)
MODEL[[2]] <- model
SE[[2]] <- sqrt(diag(vcovHC(model)))
model.us.med2 <- model


model <- lm(medrep ~ japan + uk + china
            + age + factor(gender) + factor(educ) + factor(division) + factor(profile_gross_household)
            + imp.af  + imp.la + imp.se, data = data.us)
summary(model)
MODEL[[3]] <- model
SE[[3]] <- sqrt(diag(vcovHC(model)))
model.us.med3 <- model


model <- lm(meddev ~ japan + uk + china
            + age + factor(gender) + factor(educ) + factor(division) + factor(profile_gross_household)
            + imp.af  + imp.la + imp.se, data = data.us)
summary(model)
MODEL[[4]] <- model
SE[[4]] <- sqrt(diag(vcovHC(model)))
model.us.med4 <- model
##############################################
stargazer(MODEL[[1]], MODEL[[2]], MODEL[[3]], MODEL[[4]], se = list(SE[[1]], SE[[2]], SE[[3]], SE[[4]]), star.char = c("*", "**", "***"), star.cutoffs = c(.05, .01, .001))
##############################################





##############################################
### Table A7: Effects of the Treatments and Mediators on the Outcome Variable in the Japan Survey
### Table A9: Effects of the Treatments and Mediators on the Outcome Variable in the US Survey
##############################################
MODEL <- list(NA, 2)
SE <- list(NA, 2)
##############################################
model.japan.out <- lm(OUTCOME ~ friend + enemy + MEDECON + MEDNS + MEDREP + MEDDEV
                      + imp.africa + imp.asia + male + age + factor(resid) + factor(income) + factor(educ)
                     , data = data.jp)
summary(model.japan.out)
MODEL[[1]] <- model.japan.out
SE[[1]] <- sqrt(diag(vcovHC(model.japan.out)))
##############################################
stargazer(MODEL[[1]], se = list(SE[[1]]), star.char = c("*", "**", "***"), star.cutoffs = c(.05, .01, .001))
##############################################
model.us.out <- lm(outcome ~ japan + uk + china + medecon + medns + medrep + meddev
            + age + factor(gender) + factor(educ) + factor(division) + factor(profile_gross_household)
            + imp.af  + imp.la + imp.se, data = data.us)
summary(model.us.out)
MODEL[[2]] <- model.us.out
SE[[2]] <- sqrt(diag(vcovHC(model.us.out)))
##############################################
stargazer(MODEL[[2]], se = list(SE[[2]]), star.char = c("*", "**", "***"), star.cutoffs = c(.05, .01, .001))
##############################################





##############################################
### Mediation Analyses
##############################################
set.seed(1)

mediate.japan.us.econ <- mediate(model.m = model.japan.med1, model.y = model.japan.out, treat = "friend", mediator = "MEDECON", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.japan.us.econ)

mediate.japan.us.ns <- mediate(model.m = model.japan.med2, model.y = model.japan.out, treat = "friend", mediator = "MEDNS", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.japan.us.ns)

mediate.japan.us.rep <- mediate(model.m = model.japan.med3, model.y = model.japan.out, treat = "friend", mediator = "MEDREP", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.japan.us.rep)

mediate.japan.us.dev <- mediate(model.m = model.japan.med4, model.y = model.japan.out, treat = "friend", mediator = "MEDDEV", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.japan.us.dev)


mediate.japan.china.econ <- mediate(model.m = model.japan.med1, model.y = model.japan.out, treat = "enemy", mediator = "MEDECON", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.japan.china.econ)

mediate.japan.china.ns <- mediate(model.m = model.japan.med2, model.y = model.japan.out, treat = "enemy", mediator = "MEDNS", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.japan.china.ns)

mediate.japan.china.rep <- mediate(model.m = model.japan.med3, model.y = model.japan.out, treat = "enemy", mediator = "MEDREP", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.japan.china.rep)

mediate.japan.china.dev <- mediate(model.m = model.japan.med4, model.y = model.japan.out, treat = "enemy", mediator = "MEDDEV", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.japan.china.dev)
##############################################
mediate.us.japan.econ <- mediate(model.m = model.us.med1, model.y = model.us.out, treat = "japan", mediator = "medecon", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.us.japan.econ)

mediate.us.japan.ns <- mediate(model.m = model.us.med2, model.y = model.us.out, treat = "japan", mediator = "medns", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.us.japan.ns)

mediate.us.japan.rep <- mediate(model.m = model.us.med3, model.y = model.us.out, treat = "japan", mediator = "medrep", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.us.japan.rep)

mediate.us.japan.dev <- mediate(model.m = model.us.med4, model.y = model.us.out, treat = "japan", mediator = "meddev", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.us.japan.dev)


mediate.us.uk.econ <- mediate(model.m = model.us.med1, model.y = model.us.out, treat = "uk", mediator = "medecon", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.us.uk.econ)

mediate.us.uk.ns <- mediate(model.m = model.us.med2, model.y = model.us.out, treat = "uk", mediator = "medns", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.us.uk.ns)

mediate.us.uk.rep <- mediate(model.m = model.us.med3, model.y = model.us.out, treat = "uk", mediator = "medrep", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.us.uk.rep)

mediate.us.uk.dev <- mediate(model.m = model.us.med4, model.y = model.us.out, treat = "uk", mediator = "meddev", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.us.uk.dev)


mediate.us.china.econ <- mediate(model.m = model.us.med1, model.y = model.us.out, treat = "china", mediator = "medecon", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.us.china.econ)

mediate.us.china.ns <- mediate(model.m = model.us.med2, model.y = model.us.out, treat = "china", mediator = "medns", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.us.china.ns)

mediate.us.china.rep <- mediate(model.m = model.us.med3, model.y = model.us.out, treat = "china", mediator = "medrep", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.us.china.rep)

mediate.us.china.dev <- mediate(model.m = model.us.med4, model.y = model.us.out, treat = "china", mediator = "meddev", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.us.china.dev)
##############################################


##############################################
### Figure 5: Effects of aid substitution on support for aid allocation mediated through four mechanisms. 
##############################################
pdf("figure_5.pdf", width = 8, height = 4)

par(mfrow = c(1, 2))
##############################################
adj <- 0.1

ci.japan.us.econ <- mediate.japan.us.econ$d0.ci
ci.japan.us.ns <- mediate.japan.us.ns$d0.ci
ci.japan.us.rep <- mediate.japan.us.rep$d0.ci
ci.japan.us.dev <- mediate.japan.us.dev$d0.ci

ci.japan.china.econ <- mediate.japan.china.econ$d0.ci
ci.japan.china.ns <- mediate.japan.china.ns$d0.ci
ci.japan.china.rep <- mediate.japan.china.rep$d0.ci
ci.japan.china.dev <- mediate.japan.china.dev$d0.ci

ci.us.japan.econ <- mediate.us.japan.econ$d0.ci
ci.us.japan.ns <- mediate.us.japan.ns$d0.ci
ci.us.japan.rep <- mediate.us.japan.rep$d0.ci
ci.us.japan.dev <- mediate.us.japan.dev$d0.ci

ci.us.uk.econ <- mediate.us.uk.econ$d0.ci
ci.us.uk.ns <- mediate.us.uk.ns$d0.ci
ci.us.uk.rep <- mediate.us.uk.rep$d0.ci
ci.us.uk.dev <- mediate.us.uk.dev$d0.ci

ci.us.china.econ <- mediate.us.china.econ$d0.ci
ci.us.china.ns <- mediate.us.china.ns$d0.ci
ci.us.china.rep <- mediate.us.china.rep$d0.ci
ci.us.china.dev <- mediate.us.china.dev$d0.ci
##############################################
 
MAX <- 0.015
MIN <- -0.03

plot(0, 0, xlim = c(0.5, 4.5), ylim = c(MIN, MAX), col = 0, xlab = "Mechanism", ylab = "Mediation Effect", xaxt = "n", main = "Respondents in Japan\n(N=3000)")
 
text(x = 1:4 + 0.5, par("usr")[3] - 0.0025, labels = c("Economic\nInterest", "National\nSecurity", "International\nReputation", "Development\nProspects"), pos = 2, xpd = TRUE, cex = 0.75, srt = 45)
abline(h = 0, lty = 2)
 
points(y = c(mean(ci.japan.us.econ), mean(ci.japan.us.ns), mean(ci.japan.us.rep), mean(ci.japan.us.dev)), x = c(1, 2, 3, 4) - adj, pch = 1)
 
points(y = c(mean(ci.japan.china.econ), mean(ci.japan.china.ns), mean(ci.japan.china.rep), mean(ci.japan.china.dev)), x = c(1, 2, 3, 4) + adj, pch = 19)
 
lines(y = ci.japan.us.econ, x = c(1 - adj, 1 - adj))
lines(y = ci.japan.us.ns, x = c(2 - adj, 2 - adj))
lines(y = ci.japan.us.rep, x = c(3 - adj, 3 - adj))
lines(y = ci.japan.us.dev, x = c(4 - adj, 4 - adj))
 
lines(y = ci.japan.china.econ, x = c(1 + adj, 1 + adj))
lines(y = ci.japan.china.ns, x = c(2 + adj, 2 + adj))
lines(y = ci.japan.china.rep, x = c(3 + adj, 3 + adj))
lines(y = ci.japan.china.dev, x = c(4 + adj, 4 + adj))
 
legend(1.5, 0.013, legend = c("Substitution by US", "Substitution by China"), pch = c(1, 19), lty = 1, cex = 0.75, box.lty = 0)
 
##############################################

plot(0, 0, xlim = c(0.5, 4.5), ylim = c(MIN, MAX), col = 0, xlab = "Mechanism", ylab = "Mediation Effect", xaxt = "n", main = "Respondents in US\n(N=6095)")
 
text(x = 1:4 + 0.5, par("usr")[3] - 0.003, labels = c("Economic\nInterest", "National\nSecurity", "International\nReputation", "Development\nProspects"), pos = 2, xpd = TRUE, cex = 0.75, srt = 45)
abline(h = 0, lty = 2)
 
points(y = c(mean(ci.us.japan.econ), mean(ci.us.japan.ns), mean(ci.us.japan.rep), mean(ci.us.japan.dev)), x = c(1, 2, 3, 4) - adj, pch = 1)
 
points(y = c(mean(ci.us.uk.econ), mean(ci.us.uk.ns), mean(ci.us.uk.rep), mean(ci.us.uk.dev)), x = c(1, 2, 3, 4) + adj, pch = 2)

points(y = c(mean(ci.us.china.econ), mean(ci.us.china.ns), mean(ci.us.china.rep), mean(ci.us.china.dev)), x = c(1, 2, 3, 4) + 2 * adj, pch = 19)
 
lines(y = ci.us.japan.econ, x = c(1 - adj, 1 - adj))
lines(y = ci.us.japan.ns, x = c(2 - adj, 2 - adj))
lines(y = ci.us.japan.rep, x = c(3 - adj, 3 - adj))
lines(y = ci.us.japan.dev, x = c(4 - adj, 4 - adj))
 
lines(y = ci.us.uk.econ, x = c(1 + adj, 1 + adj))
lines(y = ci.us.uk.ns, x = c(2 + adj, 2 + adj))
lines(y = ci.us.uk.rep, x = c(3 + adj, 3 + adj))
lines(y = ci.us.uk.dev, x = c(4 + adj, 4 + adj))

lines(y = ci.us.china.econ, x = c(1 + 2* adj, 1 + 2 * adj))
lines(y = ci.us.china.ns, x = c(2 + 2 * adj, 2 + 2 * adj))
lines(y = ci.us.china.rep, x = c(3 + 2 * adj, 3 + 2 * adj))
lines(y = ci.us.china.dev, x = c(4 + 2 * adj, 4 + 2 * adj))
 
legend(1.5, -0.018, legend = c("Substitution by Japan", "Substitution by UK", "Substitution by China"), pch = c(1, 2, 19), lty = 1, cex = 0.75, box.lty = 0)
 

dev.off()
##############################################






##############################################
### Figure A7: Sensitivity analysis for average causal mediation effects in the Japan survey. 
##############################################
set.seed(1)

sens.japan.us.econ <- medsens(mediate.japan.us.econ, effect.type = "indirect")

sens.japan.us.ns <- medsens(mediate.japan.us.ns, effect.type = "indirect")

sens.japan.us.rep <- medsens(mediate.japan.us.rep, effect.type = "indirect")


sens.japan.china.econ <- medsens(mediate.japan.china.econ, effect.type = "indirect")

sens.japan.china.ns <- medsens(mediate.japan.china.ns, effect.type = "indirect")

sens.japan.china.rep <- medsens(mediate.japan.china.rep, effect.type = "indirect")
##############################################
summary(sens.japan.us.econ)
summary(sens.japan.us.ns)
summary(sens.japan.us.rep)

summary(sens.japan.china.econ)
summary(sens.japan.china.ns)
summary(sens.japan.china.rep)
##############################################
pdf("figure_A7.pdf", width = 9, height = 6)

par(mfrow=c(2, 3))


plot(sens.japan.us.econ, sens.par = "R2", r.type = "total", sign.prod = "positive", main = "Effect of substitution by US\nmediated through economic interest", cex.main = 1)
plot(sens.japan.us.ns, sens.par = "R2", r.type = "total", sign.prod = "positive", main = "Effect of substitution by US\nmediated through national security", cex.main = 1)
plot(sens.japan.us.rep, sens.par = "R2", r.type = "total", sign.prod = "positive", main = "Effect of substitution by US\nmediated through international reputation", cex.main = 1)

plot(sens.japan.china.econ, sens.par = "R2", r.type = "total", sign.prod = "positive", main = "Effect of substitution by China\nmediated through economic interest", cex.main = 1)
plot(sens.japan.china.ns, sens.par = "R2", r.type = "total", sign.prod = "positive", main = "Effect of substitution by China\nmediated through national security", cex.main = 1)
plot(sens.japan.china.rep, sens.par = "R2", r.type = "total", sign.prod = "positive", main = "Effect of substitution by China\nmediated through international reputation", cex.main = 1)

dev.off()
##############################################




##############################################
### Figure A6: Effects of aid substitution on support for aid allocation mediated through four mechanisms (binary outcome variables)
##############################################
MODEL <- list(NA, 2)
SE <- list(NA, 2)
##############################################
model.japan.out <- glm(outcome.bin ~ friend + enemy + MEDECON + MEDNS + MEDREP + MEDDEV
                      + imp.africa + imp.asia + male + age + factor(resid) + factor(income) + factor(educ)
                     , data = data.jp, family = binomial(link = "probit"))
summary(model.japan.out)
MODEL[[1]] <- model.japan.out
SE[[1]] <- sqrt(diag(vcovHC(model.japan.out)))
##############################################
stargazer(MODEL[[1]], se = list(SE[[1]]), star.char = c("*", "**", "***"), star.cutoffs = c(.05, .01, .001))
##############################################
model.us.out <- glm(outcome.bin ~ japan + uk + china + medecon + medns + medrep + meddev
            + age + factor(gender) + factor(educ) + factor(division) + factor(profile_gross_household)
            + imp.af  + imp.la + imp.se, data = data.us, family = binomial(link = "probit"))
summary(model.us.out)
MODEL[[2]] <- model.us.out
SE[[2]] <- sqrt(diag(vcovHC(model.us.out)))
##############################################
stargazer(MODEL[[2]], se = list(SE[[2]]), star.char = c("*", "**", "***"), star.cutoffs = c(.05, .01, .001))
##############################################
set.seed(1)

mediate.japan.us.econ <- mediate(model.m = model.japan.med1, model.y = model.japan.out, treat = "friend", mediator = "MEDECON", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.japan.us.econ)

mediate.japan.us.ns <- mediate(model.m = model.japan.med2, model.y = model.japan.out, treat = "friend", mediator = "MEDNS", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.japan.us.ns)

mediate.japan.us.rep <- mediate(model.m = model.japan.med3, model.y = model.japan.out, treat = "friend", mediator = "MEDREP", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.japan.us.rep)

mediate.japan.us.dev <- mediate(model.m = model.japan.med4, model.y = model.japan.out, treat = "friend", mediator = "MEDDEV", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.japan.us.dev)


mediate.japan.china.econ <- mediate(model.m = model.japan.med1, model.y = model.japan.out, treat = "enemy", mediator = "MEDECON", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.japan.china.econ)

mediate.japan.china.ns <- mediate(model.m = model.japan.med2, model.y = model.japan.out, treat = "enemy", mediator = "MEDNS", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.japan.china.ns)

mediate.japan.china.rep <- mediate(model.m = model.japan.med3, model.y = model.japan.out, treat = "enemy", mediator = "MEDREP", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.japan.china.rep)

mediate.japan.china.dev <- mediate(model.m = model.japan.med4, model.y = model.japan.out, treat = "enemy", mediator = "MEDDEV", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.japan.china.dev)
##############################################
mediate.us.japan.econ <- mediate(model.m = model.us.med1, model.y = model.us.out, treat = "japan", mediator = "medecon", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.us.japan.econ)

mediate.us.japan.ns <- mediate(model.m = model.us.med2, model.y = model.us.out, treat = "japan", mediator = "medns", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.us.japan.ns)

mediate.us.japan.rep <- mediate(model.m = model.us.med3, model.y = model.us.out, treat = "japan", mediator = "medrep", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.us.japan.rep)

mediate.us.japan.dev <- mediate(model.m = model.us.med4, model.y = model.us.out, treat = "japan", mediator = "meddev", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.us.japan.dev)


mediate.us.uk.econ <- mediate(model.m = model.us.med1, model.y = model.us.out, treat = "uk", mediator = "medecon", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.us.uk.econ)

mediate.us.uk.ns <- mediate(model.m = model.us.med2, model.y = model.us.out, treat = "uk", mediator = "medns", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.us.uk.ns)

mediate.us.uk.rep <- mediate(model.m = model.us.med3, model.y = model.us.out, treat = "uk", mediator = "medrep", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.us.uk.rep)

mediate.us.uk.dev <- mediate(model.m = model.us.med4, model.y = model.us.out, treat = "uk", mediator = "meddev", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.us.uk.dev)


mediate.us.china.econ <- mediate(model.m = model.us.med1, model.y = model.us.out, treat = "china", mediator = "medecon", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.us.china.econ)

mediate.us.china.ns <- mediate(model.m = model.us.med2, model.y = model.us.out, treat = "china", mediator = "medns", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.us.china.ns)

mediate.us.china.rep <- mediate(model.m = model.us.med3, model.y = model.us.out, treat = "china", mediator = "medrep", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.us.china.rep)

mediate.us.china.dev <- mediate(model.m = model.us.med4, model.y = model.us.out, treat = "china", mediator = "meddev", control.value = 0, treat.value = 1, robustSE = TRUE, sims = 5000)
summary(mediate.us.china.dev)
##############################################
pdf("figure_A6.pdf", width = 8, height = 4)

par(mfrow = c(1, 2))
##############################################
adj <- 0.1

ci.japan.us.econ <- mediate.japan.us.econ$d0.ci
ci.japan.us.ns <- mediate.japan.us.ns$d0.ci
ci.japan.us.rep <- mediate.japan.us.rep$d0.ci
ci.japan.us.dev <- mediate.japan.us.dev$d0.ci

ci.japan.china.econ <- mediate.japan.china.econ$d0.ci
ci.japan.china.ns <- mediate.japan.china.ns$d0.ci
ci.japan.china.rep <- mediate.japan.china.rep$d0.ci
ci.japan.china.dev <- mediate.japan.china.dev$d0.ci

ci.us.japan.econ <- mediate.us.japan.econ$d0.ci
ci.us.japan.ns <- mediate.us.japan.ns$d0.ci
ci.us.japan.rep <- mediate.us.japan.rep$d0.ci
ci.us.japan.dev <- mediate.us.japan.dev$d0.ci

ci.us.uk.econ <- mediate.us.uk.econ$d0.ci
ci.us.uk.ns <- mediate.us.uk.ns$d0.ci
ci.us.uk.rep <- mediate.us.uk.rep$d0.ci
ci.us.uk.dev <- mediate.us.uk.dev$d0.ci

ci.us.china.econ <- mediate.us.china.econ$d0.ci
ci.us.china.ns <- mediate.us.china.ns$d0.ci
ci.us.china.rep <- mediate.us.china.rep$d0.ci
ci.us.china.dev <- mediate.us.china.dev$d0.ci
##############################################
 
MAX <- 0.015
MIN <- -0.03

plot(0, 0, xlim = c(0.5, 4.5), ylim = c(MIN, MAX), col = 0, xlab = "Mechanism", ylab = "Mediation Effect", xaxt = "n", main = "Respondents in Japan\n(N=3000)")
 
text(x = 1:4 + 0.5, par("usr")[3] - 0.0025, labels = c("Economic\nInterest", "National\nSecurity", "International\nReputation", "Development\nProspects"), pos = 2, xpd = TRUE, cex = 0.75, srt = 45)
abline(h = 0, lty = 2)
 
points(y = c(mean(ci.japan.us.econ), mean(ci.japan.us.ns), mean(ci.japan.us.rep), mean(ci.japan.us.dev)), x = c(1, 2, 3, 4) - adj, pch = 1)
 
points(y = c(mean(ci.japan.china.econ), mean(ci.japan.china.ns), mean(ci.japan.china.rep), mean(ci.japan.china.dev)), x = c(1, 2, 3, 4) + adj, pch = 19)
 
lines(y = ci.japan.us.econ, x = c(1 - adj, 1 - adj))
lines(y = ci.japan.us.ns, x = c(2 - adj, 2 - adj))
lines(y = ci.japan.us.rep, x = c(3 - adj, 3 - adj))
lines(y = ci.japan.us.dev, x = c(4 - adj, 4 - adj))
 
lines(y = ci.japan.china.econ, x = c(1 + adj, 1 + adj))
lines(y = ci.japan.china.ns, x = c(2 + adj, 2 + adj))
lines(y = ci.japan.china.rep, x = c(3 + adj, 3 + adj))
lines(y = ci.japan.china.dev, x = c(4 + adj, 4 + adj))
 
legend(1.5, 0.013, legend = c("Substitution by US", "Substitution by China"), pch = c(1, 19), lty = 1, cex = 0.75, box.lty = 0)
 
##############################################

plot(0, 0, xlim = c(0.5, 4.5), ylim = c(MIN, MAX), col = 0, xlab = "Mechanism", ylab = "Mediation Effect", xaxt = "n", main = "Respondents in US\n(N=6095)")
 
text(x = 1:4 + 0.5, par("usr")[3] - 0.003, labels = c("Economic\nInterest", "National\nSecurity", "International\nReputation", "Development\nProspects"), pos = 2, xpd = TRUE, cex = 0.75, srt = 45)
abline(h = 0, lty = 2)
 
points(y = c(mean(ci.us.japan.econ), mean(ci.us.japan.ns), mean(ci.us.japan.rep), mean(ci.us.japan.dev)), x = c(1, 2, 3, 4) - adj, pch = 1)
 
points(y = c(mean(ci.us.uk.econ), mean(ci.us.uk.ns), mean(ci.us.uk.rep), mean(ci.us.uk.dev)), x = c(1, 2, 3, 4) + adj, pch = 2)

points(y = c(mean(ci.us.china.econ), mean(ci.us.china.ns), mean(ci.us.china.rep), mean(ci.us.china.dev)), x = c(1, 2, 3, 4) + 2 * adj, pch = 19)
 
lines(y = ci.us.japan.econ, x = c(1 - adj, 1 - adj))
lines(y = ci.us.japan.ns, x = c(2 - adj, 2 - adj))
lines(y = ci.us.japan.rep, x = c(3 - adj, 3 - adj))
lines(y = ci.us.japan.dev, x = c(4 - adj, 4 - adj))
 
lines(y = ci.us.uk.econ, x = c(1 + adj, 1 + adj))
lines(y = ci.us.uk.ns, x = c(2 + adj, 2 + adj))
lines(y = ci.us.uk.rep, x = c(3 + adj, 3 + adj))
lines(y = ci.us.uk.dev, x = c(4 + adj, 4 + adj))

lines(y = ci.us.china.econ, x = c(1 + 2* adj, 1 + 2 * adj))
lines(y = ci.us.china.ns, x = c(2 + 2 * adj, 2 + 2 * adj))
lines(y = ci.us.china.rep, x = c(3 + 2 * adj, 3 + 2 * adj))
lines(y = ci.us.china.dev, x = c(4 + 2 * adj, 4 + 2 * adj))
 
legend(1.5, -0.018, legend = c("Substitution by Japan", "Substitution by UK", "Substitution by China"), pch = c(1, 2, 19), lty = 1, cex = 0.75, box.lty = 0)
 

dev.off()
##############################################



