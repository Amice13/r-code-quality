##############################################
### "Public Preferences for Reallocating Aid in the Presence of Alternative Donors"
### Replication R codes for all analyses other than mediation 
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
### Table 1: Respondents Opinions on Foreign Aid
##############################################
x <- data.jp$increase
y <- data.us$increase
t.test(x)
t.test(y)
t.test(x, y)
sum(!is.na(x))
sum(!is.na(y))

x <- data.jp$effective
y <- data.us$effective
t.test(x)
t.test(y)
t.test(x, y)
sum(!is.na(x))
sum(!is.na(y))

x <- data.jp$influence
y <- data.us$influence
t.test(x)
t.test(y)
t.test(x, y)
sum(!is.na(x))
sum(!is.na(y))

x <- data.jp$security
y <- data.us$security
t.test(x)
t.test(y)
t.test(x, y)
sum(!is.na(x))
sum(!is.na(y))

x <- data.jp$moral
y <- data.us$moral
t.test(x)
t.test(y)
t.test(x, y)
sum(!is.na(x))
sum(!is.na(y))
##############################################


##############################################
### Figure A1: Distribution of Perceived Efficacy of Aid
##############################################
mean.jp <- mean(data.jp$effective, na.rm = T)
ci.jp <- t.test(data.jp$effective)[[4]]

mean.us <- mean(data.us$effective, na.rm = T)
ci.us <- t.test(data.us$effective)[[4]]
##############################################
pdf("figure_A1.pdf", width = 9, height = 3)

par(mfrow = c(1, 3))

barplot(prop.table(table(data.jp$effective)), ylab = "Proportion", xlab = "Very                                   Very\n    Ineffective                           Effective      ", main = "Japan\n(N=3000)", ylim = c(0, 0.25))

barplot(prop.table(table(data.us$effective)), ylab = "Proportion", xlab = "Very                                   Very\n    Ineffective                           Effective      ", main = "US\n(N=6095)", ylim = c(0, 0.25))



plot(0, 0, xlim = c(4.3, 5.2), ylim = c(0.5, 2.5), cex = 0, xlab = "Average Level of Perceived Efficacy of Aid\n(with 95% Confidence Interval)", ylab = "", yaxt = "n")

points(x = mean.jp, y = 2, pch = 20)
lines(x = ci.jp, y = rep(2, 2))
text(x = mean.jp, y = 2 + 0.2, labels = "Japan", cex = 1)

points(x = mean.us, y = 1, pch = 20)
lines(x = ci.us, y = rep(1, 2))
text(x = mean.us, y = 1 + 0.2, labels = "US", cex = 1)




dev.off()
##############################################




##############################################
### Figure 1: Distribution of the Outcome Variable in the Control Condition by Region for the Japan Survey
##############################################
data.af.con <- data.jp.africa[data.jp.africa$control == 1, ]

data.as.con <- data.jp.asia[data.jp.asia$control == 1, ]

mean.af <- mean(data.af.con$OUTCOME)
ci.af <- t.test(data.af.con$OUTCOME)[[4]]

mean.as <- mean(data.as.con$OUTCOME)
ci.as <- t.test(data.as.con$OUTCOME)[[4]]
##############################################
pdf("figure_1.pdf", width = 9, height = 3)

par(mfrow = c(1, 3))

barplot(prop.table(table(data.af.con$OUTCOME)), ylab = "Proportion", xlab = "Strongly                                      Strongly\n  Disagree                                      Agree     ", main = "Aid Reallocation from\nAfrica to Southeast Asia\n(N=494)")

barplot(prop.table(table(data.as.con$OUTCOME)), ylab = "Proportion", xlab = "Strongly                                      Strongly\n  Disagree                                      Agree     ", main = "Aid Reallocation from\nSoutheast Asia to Africa\n(N=500)")




plot(0, 0, xlim = c(2.35, 2.7), ylim = c(0.5, 2.5), cex = 0, xlab = "Average Level of Support for Aid Reallocation\n(with 95% Confidence Interval)", ylab = "", yaxt = "n")

points(x = mean.af, y = 2, pch = 20)
lines(x = ci.af, y = rep(2, 2))
text(x = mean.af, y = 2 + 0.2, labels = "Africa to\n Southeast Asia", cex = 1)

points(x = mean.as, y = 1, pch = 20)
lines(x = ci.as, y = rep(1, 2))
text(x = mean.as, y = 1 + 0.2, labels = "Southeast Asia to\n Africa", cex = 1)




dev.off()
##############################################





##############################################
### Figure 2: Distribution of the Outcome Variable in the Control Condition by Region for the U.S. Survey
##############################################
data.us.sub <- data.us.af.la
data.us.sub.con <- data.us.sub[data.us.sub$control == 1, ]
mean.af.la <- mean(data.us.sub.con$outcome)
ci.af.la <- t.test(data.us.sub.con$outcome)[[4]]

data.us.sub <- data.us.af.se
data.us.sub.con <- data.us.sub[data.us.sub$control == 1, ]
mean.af.se <- mean(data.us.sub.con$outcome)
ci.af.se <- t.test(data.us.sub.con$outcome)[[4]]

data.us.sub <- data.us.la.af
data.us.sub.con <- data.us.sub[data.us.sub$control == 1, ]
mean.la.af <- mean(data.us.sub.con$outcome)
ci.la.af <- t.test(data.us.sub.con$outcome)[[4]]

data.us.sub <- data.us.la.se
data.us.sub.con <- data.us.sub[data.us.sub$control == 1, ]
mean.la.se <- mean(data.us.sub.con$outcome)
ci.la.se <- t.test(data.us.sub.con$outcome)[[4]]

data.us.sub <- data.us.se.af
data.us.sub.con <- data.us.sub[data.us.sub$control == 1, ]
mean.se.af <- mean(data.us.sub.con$outcome)
ci.se.af <- t.test(data.us.sub.con$outcome)[[4]]

data.us.sub <- data.us.se.la
data.us.sub.con <- data.us.sub[data.us.sub$control == 1, ]
mean.se.la <- mean(data.us.sub.con$outcome)
ci.se.la <- t.test(data.us.sub.con$outcome)[[4]]

data.us.con <- data.us[data.us$control == 1, ]
mean.pool <- mean(data.us.con$outcome)
ci.pool <- t.test(data.us$outcome)[[4]]
##############################################
pdf("figure_2.pdf", width = 9, height = 9)

par(mfrow = c(3, 3))

data.us.sub <- data.us.af.la
data.us.sub.con <- data.us.sub[data.us.sub$control == 1, ]
barplot(prop.table(table(data.us.sub.con$outcome)), ylab = "Proportion", xlab = "Strongly                                     Strongly\n   Disagree                                     Agree      ", main = "Aid Reallocation from\nAfrica to Latin America\n(N=254)")

data.us.sub <- data.us.la.se
data.us.sub.con <- data.us.sub[data.us.sub$control == 1, ]
barplot(prop.table(table(data.us.sub.con$outcome)), ylab = "Proportion", xlab = "Strongly                                     Strongly\n   Disagree                                     Agree      ", main = "Aid Reallocation from\nLatin America to Southeast Asia\n(N=254)")

data.us.sub <- data.us.se.af
data.us.sub.con <- data.us.sub[data.us.sub$control == 1, ]
barplot(prop.table(table(data.us.sub.con$outcome)), ylab = "Proportion", xlab = "Strongly                                     Strongly\n   Disagree                                     Agree      ", main = "Aid Reallocation from\nSoutheast Asia to Africa\n(N=270)")

data.us.sub <- data.us.la.af
data.us.sub.con <- data.us.sub[data.us.sub$control == 1, ]
barplot(prop.table(table(data.us.sub.con$outcome)), ylab = "Proportion", xlab = "Strongly                                     Strongly\n   Disagree                                     Agree      ", main = "Aid Reallocation from\nLatin America to Africa\n(N=256)")


data.us.sub <- data.us.se.la
data.us.sub.con <- data.us.sub[data.us.sub$control == 1, ]
barplot(prop.table(table(data.us.sub.con$outcome)), ylab = "Proportion", xlab = "Strongly                                     Strongly\n   Disagree                                     Agree      ", main = "Aid Reallocation from\nSoutheast Asia to Latin America\n(N=239)")

data.us.sub <- data.us.af.se
data.us.sub.con <- data.us.sub[data.us.sub$control == 1, ]
barplot(prop.table(table(data.us.sub.con$outcome)), ylab = "Proportion", xlab = "Strongly                                     Strongly\n   Disagree                                     Agree      ", main = "Aid Reallocation from\nAfrica to Southeast Asia\n(N=250)")


plot(0, 0, xlab = "", ylab = "", axes = F, cex = 0)




plot(0, 0, xlim = c(2.1, 2.65), ylim = c(0.5, 6.5), cex = 0, xlab = "Average Level of Support for Aid Reallocation\n(with 95% Confidence Interval)", ylab = "", yaxt = "n")

points(x = mean.af.la, y = 6, pch = 20)
lines(x = ci.af.la, y = rep(6, 2))
text(x = mean.af.la, y = 6 + 0.4, labels = "Africa to\n Latin America", cex = 0.7)

points(x = mean.la.af, y = 5, pch = 20)
lines(x = ci.la.af, y = rep(5, 2))
text(x = mean.la.af, y = 5 + 0.4, labels = "Latin America to\n Africa", cex = 0.7)


points(x = mean.la.se, y = 4, pch = 20)
lines(x = ci.la.se, y = rep(4, 2))
text(x = mean.la.se, y = 4 + 0.4, labels = "Latin America to\n Southeast Asia", cex = 0.7)


points(x = mean.se.la, y = 3, pch = 20)
lines(x = ci.se.la, y = rep(3, 2))
text(x = mean.se.la, y = 3 + 0.4, labels = "Southeast Asia to\n Latin America", cex = 0.7)


points(x = mean.se.af, y = 2, pch = 20)
lines(x = ci.se.af, y = rep(2, 2))
text(x = mean.se.af, y = 2 + 0.4, labels = "Southeast Asia to\n Africa", cex = 0.7)


points(x = mean.af.se, y = 1, pch = 20)
lines(x = ci.af.se, y = rep(1, 2))
text(x = mean.af.se, y = 1 + 0.4, labels = "Africa to\n Southeast Asia", cex = 0.7)



dev.off()
##############################################



##############################################
### Figure A2.  Average Treatment Effects in Japan Survey for Two Treatments (without covariates)
### Table A1: Effects of the Treatments on the Outcome Variable in the Japan Survey (without covariates)
##############################################
MODEL <- list(NA, 3)
SE <- list(NA, 3)
##############################################
model <- lm(OUTCOME ~ friend + enemy, data = data.jp.africa)

summary(model)

MODEL[[1]] <- model
SE[[1]] <- sqrt(diag(vcovHC(model)))

coef.africa <- coef(model)
vcov.africa <- vcovHC(model)

linearHypothesis(model, "friend = enemy", vcov. = vcov.africa)
##############################################
model <- lm(OUTCOME ~ friend + enemy, data = data.jp.asia)

summary(model)

MODEL[[2]] <- model
SE[[2]] <- sqrt(diag(vcovHC(model)))

coef.asia <- coef(model)
vcov.asia <- vcovHC(model)

linearHypothesis(model, "friend = enemy", vcov. = vcov.asia)
##############################################
model <- lm(OUTCOME ~ friend + enemy, data = data.jp)

summary(model)

MODEL[[3]] <- model
SE[[3]] <- sqrt(diag(vcovHC(model)))

coef.pool <- coef(model)
vcov.pool <- vcovHC(model)

linearHypothesis(model, "friend = enemy", vcov. = vcov.pool)
##############################################
stargazer(MODEL[[1]], MODEL[[2]], MODEL[[3]], se = list(SE[[1]], SE[[2]], SE[[3]]), star.char = c("*", "**", "***"), star.cutoffs = c(.05, .01, .001))
##############################################
set.seed(1)

beta <- mvrnorm(1000, mu = coef.africa, Sigma = vcov.africa)
effect.friend.africa <- c(quantile(beta[, 2], prob = 0.025), mean(beta[, 2]), quantile(beta[, 2], prob = 0.975))
effect.enemy.africa <- c(quantile(beta[, 3], prob = 0.025), mean(beta[, 3]), quantile(beta[, 3], prob = 0.975))



beta <- mvrnorm(1000, mu = coef.asia, Sigma = vcov.asia)
effect.friend.asia <- c(quantile(beta[, 2], prob = 0.025), mean(beta[, 2]), quantile(beta[, 2], prob = 0.975))
effect.enemy.asia <- c(quantile(beta[, 3], prob = 0.025), mean(beta[, 3]), quantile(beta[, 3], prob = 0.975))


beta <- mvrnorm(1000, mu = coef.pool, Sigma = vcov.pool)
effect.friend.pool <- c(quantile(beta[, 2], prob = 0.025), mean(beta[, 2]), quantile(beta[, 2], prob = 0.975))
effect.enemy.pool <- c(quantile(beta[, 3], prob = 0.025), mean(beta[, 3]), quantile(beta[, 3], prob = 0.975))
##############################################
pdf("figure_A2.pdf", width = 9, height = 3)

par(mfrow = c(1, 3))

MAX <- max(effect.friend.africa, effect.friend.asia, effect.enemy.africa, effect.enemy.africa, effect.friend.pool, effect.friend.pool)
MIN <- min(effect.friend.africa, effect.friend.asia, effect.enemy.africa, effect.enemy.africa, effect.friend.pool, effect.friend.pool)

plot(y = c(1, 2), x = c(effect.friend.africa[2], effect.enemy.africa[2]), ylim = c(0.5, 2.5), xlim = c(MIN, -MIN), main = "Aid Reallocation from\nAfrica to Southeast Asia\n(N=1493)", xlab = "Average Treatment Effect", yaxt = "n", ylab = "", pch = 19)

lines(y = c(1, 1), x = c(effect.friend.africa[1], effect.friend.africa[3]))
lines(y = c(2, 2), x = c(effect.enemy.africa[1], effect.enemy.africa[3]))
abline(v = 0, lty = 2)

text(x = c(-MIN, -MIN) - 0.1, y = c(1, 2), labels = c("Substitution by US", "Substitution by China"), cex = 0.9)

plot(y = c(1, 2), x = c(effect.friend.asia[2], effect.enemy.asia[2]), ylim = c(0.5, 2.5), xlim = c(MIN, -MIN), main = "Aid Reallocation from\nSoutheast Asia to Africa\n(N=1507)", xlab = "Average Treatment Effect", yaxt = "n", ylab = "", pch = 19)

lines(y = c(1, 1), x = c(effect.friend.asia[1], effect.friend.asia[3]))
lines(y = c(2, 2), x = c(effect.enemy.asia[1], effect.enemy.asia[3]))
abline(v = 0, lty = 2)

text(x = c(-MIN, -MIN) - 0.1, y = c(1, 2), labels = c("Substitution by US", "Substitution by China"), cex = 0.9)

plot(y = c(1, 2), x = c(effect.friend.pool[2], effect.enemy.pool[2]), ylim = c(0.5, 2.5), xlim = c(MIN, -MIN), main = "Pooled\n(N=3000)", xlab = "Average Treatment Effect", yaxt = "n", ylab = "", pch = 19)

lines(y = c(1, 1), x = c(effect.friend.pool[1], effect.friend.pool[3]))
lines(y = c(2, 2), x = c(effect.enemy.pool[1], effect.enemy.pool[3]))
abline(v = 0, lty = 2)

text(x = c(-MIN, -MIN) - 0.1, y = c(1, 2), labels = c("Substitution by US", "Substitution by China"), cex = 0.9)

dev.off()
##############################################





##############################################
### Figure 3.  Average Treatment Effects in Japan Survey for Two Treatments
### Table A3: Effects of the Treatments on the Outcome Variable in the Japan Survey
##############################################
MODEL <- list(NA, 3)
SE <- list(NA, 3)
##############################################
model <- lm(OUTCOME ~ friend + enemy
            + imp.africa + imp.asia + male + age + factor(resid) + factor(income) + factor(educ), data = data.jp.africa)

summary(model)

MODEL[[1]] <- model
SE[[1]] <- sqrt(diag(vcovHC(model)))

coef.africa <- coef(model)
vcov.africa <- vcovHC(model)

linearHypothesis(model, "friend = enemy", vcov. = vcov.africa)
##############################################
model <- lm(OUTCOME ~ friend + enemy
            + imp.africa + imp.asia + male + age + factor(resid) + factor(income) + factor(educ), data = data.jp.asia)

summary(model)

MODEL[[2]] <- model
SE[[2]] <- sqrt(diag(vcovHC(model)))

coef.asia <- coef(model)
vcov.asia <- vcovHC(model)

linearHypothesis(model, "friend = enemy", vcov. = vcov.asia)
##############################################
model <- lm(OUTCOME ~ friend + enemy
            + imp.africa + imp.asia + male + age + factor(resid) + factor(income) + factor(educ), data = data.jp)

summary(model)

MODEL[[3]] <- model
SE[[3]] <- sqrt(diag(vcovHC(model)))

coef.pool <- coef(model)
vcov.pool <- vcovHC(model)

linearHypothesis(model, "friend = enemy", vcov. = vcov.pool)
##############################################
stargazer(MODEL[[1]], MODEL[[2]], MODEL[[3]], se = list(SE[[1]], SE[[2]], SE[[3]]), star.char = c("*", "**", "***"), star.cutoffs = c(.05, .01, .001))
##############################################
set.seed(1)

beta <- mvrnorm(1000, mu = coef.africa, Sigma = vcov.africa)
effect.friend.africa <- c(quantile(beta[, 2], prob = 0.025), mean(beta[, 2]), quantile(beta[, 2], prob = 0.975))
effect.enemy.africa <- c(quantile(beta[, 3], prob = 0.025), mean(beta[, 3]), quantile(beta[, 3], prob = 0.975))



beta <- mvrnorm(1000, mu = coef.asia, Sigma = vcov.asia)
effect.friend.asia <- c(quantile(beta[, 2], prob = 0.025), mean(beta[, 2]), quantile(beta[, 2], prob = 0.975))
effect.enemy.asia <- c(quantile(beta[, 3], prob = 0.025), mean(beta[, 3]), quantile(beta[, 3], prob = 0.975))


beta <- mvrnorm(1000, mu = coef.pool, Sigma = vcov.pool)
effect.friend.pool <- c(quantile(beta[, 2], prob = 0.025), mean(beta[, 2]), quantile(beta[, 2], prob = 0.975))
effect.enemy.pool <- c(quantile(beta[, 3], prob = 0.025), mean(beta[, 3]), quantile(beta[, 3], prob = 0.975))
##############################################
pdf("figure_3.pdf", width = 9, height = 3)

par(mfrow = c(1, 3))

MAX <- max(effect.friend.africa, effect.friend.asia, effect.enemy.africa, effect.enemy.africa, effect.friend.pool, effect.friend.pool)
MIN <- min(effect.friend.africa, effect.friend.asia, effect.enemy.africa, effect.enemy.africa, effect.friend.pool, effect.friend.pool)

plot(y = c(1, 2), x = c(effect.friend.africa[2], effect.enemy.africa[2]), ylim = c(0.5, 2.5), xlim = c(MIN, -MIN), main = "Aid Reallocation from\nAfrica to Southeast Asia\n(N=1493)", xlab = "Average Treatment Effect", yaxt = "n", ylab = "", pch = 19)

lines(y = c(1, 1), x = c(effect.friend.africa[1], effect.friend.africa[3]))
lines(y = c(2, 2), x = c(effect.enemy.africa[1], effect.enemy.africa[3]))
abline(v = 0, lty = 2)

text(x = c(-MIN, -MIN) - 0.08, y = c(1, 2), labels = c("Substitution by US", "Substitution by China"), cex = 0.9)

plot(y = c(1, 2), x = c(effect.friend.asia[2], effect.enemy.asia[2]), ylim = c(0.5, 2.5), xlim = c(MIN, -MIN), main = "Aid Reallocation from\nSoutheast Asia to Africa\n(N=1507)", xlab = "Average Treatment Effect", yaxt = "n", ylab = "", pch = 19)

lines(y = c(1, 1), x = c(effect.friend.asia[1], effect.friend.asia[3]))
lines(y = c(2, 2), x = c(effect.enemy.asia[1], effect.enemy.asia[3]))
abline(v = 0, lty = 2)

text(x = c(-MIN, -MIN) - 0.08, y = c(1, 2), labels = c("Substitution by US", "Substitution by China"), cex = 0.9)

plot(y = c(1, 2), x = c(effect.friend.pool[2], effect.enemy.pool[2]), ylim = c(0.5, 2.5), xlim = c(MIN, -MIN), main = "Pooled\n(N=3000)", xlab = "Average Treatment Effect", yaxt = "n", ylab = "", pch = 19)

lines(y = c(1, 1), x = c(effect.friend.pool[1], effect.friend.pool[3]))
lines(y = c(2, 2), x = c(effect.enemy.pool[1], effect.enemy.pool[3]))
abline(v = 0, lty = 2)

text(x = c(-MIN, -MIN) - 0.08, y = c(1, 2), labels = c("Substitution by US", "Substitution by China"), cex = 0.9)

dev.off()
##############################################





##############################################
### Figure A3.  Average Treatment Effects in U.S. Survey for Three Treatments (without covariates)
### Table A2: Effects of the Treatments on the Outcome Variable in the US Survey (without covariates)
##############################################
MODEL <- list(NA, 7)
SE <- list(NA, 7)
##############################################
model <- lm(outcome ~ japan + uk + china, data = data.us.af.la)
MODEL[[1]] <- model
SE[[1]] <- sqrt(diag(vcovHC(model)))
set.seed(1)
beta <- mvrnorm(1000, mu = coef(model), Sigma = vcovHC(model))
effect.japan.af.la <- c(quantile(beta[, 2], prob = 0.025), mean(beta[, 2]), quantile(beta[, 2], prob = 0.975))
effect.uk.af.la <- c(quantile(beta[, 3], prob = 0.025), mean(beta[, 3]), quantile(beta[, 3], prob = 0.975))
effect.china.af.la <- c(quantile(beta[, 4], prob = 0.025), mean(beta[, 4]), quantile(beta[, 4], prob = 0.975))



model <- lm(outcome ~ japan + uk + china, data = data.us.la.se)
MODEL[[2]] <- model
SE[[2]] <- sqrt(diag(vcovHC(model)))
set.seed(1)
beta <- mvrnorm(1000, mu = coef(model), Sigma = vcovHC(model))
effect.japan.la.se <- c(quantile(beta[, 2], prob = 0.025), mean(beta[, 2]), quantile(beta[, 2], prob = 0.975))
effect.uk.la.se <- c(quantile(beta[, 3], prob = 0.025), mean(beta[, 3]), quantile(beta[, 3], prob = 0.975))
effect.china.la.se <- c(quantile(beta[, 4], prob = 0.025), mean(beta[, 4]), quantile(beta[, 4], prob = 0.975))



model <- lm(outcome ~ japan + uk + china, data = data.us.se.af)
MODEL[[3]] <- model
SE[[3]] <- sqrt(diag(vcovHC(model)))
set.seed(1)
beta <- mvrnorm(1000, mu = coef(model), Sigma = vcovHC(model))
effect.japan.se.af <- c(quantile(beta[, 2], prob = 0.025), mean(beta[, 2]), quantile(beta[, 2], prob = 0.975))
effect.uk.se.af <- c(quantile(beta[, 3], prob = 0.025), mean(beta[, 3]), quantile(beta[, 3], prob = 0.975))
effect.china.se.af <- c(quantile(beta[, 4], prob = 0.025), mean(beta[, 4]), quantile(beta[, 4], prob = 0.975))




model <- lm(outcome ~ japan + uk + china, data = data.us.la.af)
MODEL[[4]] <- model
SE[[4]] <- sqrt(diag(vcovHC(model)))
set.seed(1)
beta <- mvrnorm(1000, mu = coef(model), Sigma = vcovHC(model))
effect.japan.la.af <- c(quantile(beta[, 2], prob = 0.025), mean(beta[, 2]), quantile(beta[, 2], prob = 0.975))
effect.uk.la.af <- c(quantile(beta[, 3], prob = 0.025), mean(beta[, 3]), quantile(beta[, 3], prob = 0.975))
effect.china.la.af <- c(quantile(beta[, 4], prob = 0.025), mean(beta[, 4]), quantile(beta[, 4], prob = 0.975))




model <- lm(outcome ~ japan + uk + china, data = data.us.se.la)
MODEL[[5]] <- model
SE[[5]] <- sqrt(diag(vcovHC(model)))
set.seed(1)
beta <- mvrnorm(1000, mu = coef(model), Sigma = vcovHC(model))
effect.japan.se.la <- c(quantile(beta[, 2], prob = 0.025), mean(beta[, 2]), quantile(beta[, 2], prob = 0.975))
effect.uk.se.la <- c(quantile(beta[, 3], prob = 0.025), mean(beta[, 3]), quantile(beta[, 3], prob = 0.975))
effect.china.se.la <- c(quantile(beta[, 4], prob = 0.025), mean(beta[, 4]), quantile(beta[, 4], prob = 0.975))


model <- lm(outcome ~ japan + uk + china, data = data.us.af.se)
MODEL[[6]] <- model
SE[[6]] <- sqrt(diag(vcovHC(model)))
set.seed(1)
beta <- mvrnorm(1000, mu = coef(model), Sigma = vcovHC(model))
effect.japan.af.se <- c(quantile(beta[, 2], prob = 0.025), mean(beta[, 2]), quantile(beta[, 2], prob = 0.975))
effect.uk.af.se <- c(quantile(beta[, 3], prob = 0.025), mean(beta[, 3]), quantile(beta[, 3], prob = 0.975))
effect.china.af.se <- c(quantile(beta[, 4], prob = 0.025), mean(beta[, 4]), quantile(beta[, 4], prob = 0.975))


model <- lm(outcome ~ japan + uk + china, data = data.us)
MODEL[[7]] <- model
SE[[7]] <- sqrt(diag(vcovHC(model)))
set.seed(1)
beta <- mvrnorm(1000, mu = coef(model), Sigma = vcovHC(model))
effect.japan.pool <- c(quantile(beta[, 2], prob = 0.025), mean(beta[, 2]), quantile(beta[, 2], prob = 0.975))
effect.uk.pool <- c(quantile(beta[, 3], prob = 0.025), mean(beta[, 3]), quantile(beta[, 3], prob = 0.975))
effect.china.pool <- c(quantile(beta[, 4], prob = 0.025), mean(beta[, 4]), quantile(beta[, 4], prob = 0.975))
##############################################
stargazer(MODEL[[1]], MODEL[[2]], MODEL[[3]], MODEL[[4]], se = list(SE[[1]], SE[[2]], SE[[3]], SE[[4]]), star.char = c("*", "**", "***"), star.cutoffs = c(.05, .01, .001), omit = c("age", "gender", "educ", "division", "profile_gross_household"))

stargazer(MODEL[[5]], MODEL[[6]], MODEL[[7]], se = list(SE[[5]], SE[[6]], SE[[7]]), star.char = c("*", "**", "***"), star.cutoffs = c(.05, .01, .001), omit = c("age", "gender", "educ", "division", "profile_gross_household"))
##############################################
pdf("figure_A3.pdf", width = 9, height = 9)

par(mfrow = c(3, 3))

XLAB <- list("Average Treatment Effect", cex = 1.2)
YLAB <- ""
cex.main <- 1.5
MIN <- -0.5
MAX <- 0.5
X.text <- 0.38

MAIN <- list("Aid Reallocation from\nAfrica to Latin America\n(N=1001)", cex = cex.main)
plot(0, 0, xlim = c(MIN, MAX), ylim = c(0.5, 3.5), xlab = XLAB, ylab = YLAB, main = MAIN, yaxt = "n")
lines(x = effect.japan.af.la[c(1, 3)], y = rep(3, 2))
points(x = effect.japan.af.la[2], pch = 20, y = 3)
lines(x = effect.uk.af.la[c(1, 3)], y = rep(2, 2))
points(x = effect.uk.af.la[2], pch = 20, y = 2)
lines(x = effect.china.af.la[c(1, 3)], y = rep(1, 2))
points(x = effect.china.af.la[2], pch = 20, y = 1)
text(x = X.text, y = 3, labels = "Substitution\nby Japan")
text(x = X.text, y = 2, labels = "Substitution\nby UK")
text(x = X.text, y = 1, labels = "Substitution\nby China")
abline(v = 0, lty = 2)



MAIN <- list("Aid Reallocation from\nLatin America to Southeast Asia\n(N=1007)", cex = cex.main)
plot(0, 0, xlim = c(MIN, MAX), ylim = c(0.5, 3.5), xlab = XLAB, ylab = YLAB, main = MAIN, yaxt = "n")
lines(x = effect.japan.la.se[c(1, 3)], y = rep(3, 2))
points(x = effect.japan.la.se[2], pch = 20, y = 3)
lines(x = effect.uk.la.se[c(1, 3)], y = rep(2, 2))
points(x = effect.uk.la.se[2], pch = 20, y = 2)
lines(x = effect.china.la.se[c(1, 3)], y = rep(1, 2))
points(x = effect.china.la.se[2], pch = 20, y = 1)
text(x = X.text, y = 3, labels = "Substitution\nby Japan")
text(x = X.text, y = 2, labels = "Substitution\nby UK")
text(x = X.text, y = 1, labels = "Substitution\nby China")
abline(v = 0, lty = 2)



MAIN <- list("Aid Reallocation from\nSoutheast Asia to Africa\n(N=1030)", cex = cex.main)
plot(0, 0, xlim = c(MIN, MAX), ylim = c(0.5, 3.5), xlab = XLAB, ylab = YLAB, main = MAIN, yaxt = "n")
lines(x = effect.japan.se.af[c(1, 3)], y = rep(3, 2))
points(x = effect.japan.se.af[2], pch = 20, y = 3)
lines(x = effect.uk.se.af[c(1, 3)], y = rep(2, 2))
points(x = effect.uk.se.af[2], pch = 20, y = 2)
lines(x = effect.china.se.af[c(1, 3)], y = rep(1, 2))
points(x = effect.china.se.af[2], pch = 20, y = 1)
text(x = X.text, y = 3, labels = "Substitution\nby Japan")
text(x = X.text, y = 2, labels = "Substitution\nby UK")
text(x = X.text, y = 1, labels = "Substitution\nby China")
abline(v = 0, lty = 2)



MAIN <- list("Aid Reallocation from\nLatin America to Africa\n(N=1028)", cex = cex.main)
plot(0, 0, xlim = c(MIN, MAX), ylim = c(0.5, 3.5), xlab = XLAB, ylab = YLAB, main = MAIN, yaxt = "n")
lines(x = effect.japan.la.af[c(1, 3)], y = rep(3, 2))
points(x = effect.japan.la.af[2], pch = 20, y = 3)
lines(x = effect.uk.la.af[c(1, 3)], y = rep(2, 2))
points(x = effect.uk.la.af[2], pch = 20, y = 2)
lines(x = effect.china.la.af[c(1, 3)], y = rep(1, 2))
points(x = effect.china.la.af[2], pch = 20, y = 1)
text(x = X.text, y = 3, labels = "Substitution\nby Japan")
text(x = X.text, y = 2, labels = "Substitution\nby UK")
text(x = X.text, y = 1, labels = "Substitution\nby China")
abline(v = 0, lty = 2)



MAIN <- list("Aid Reallocation from\nSoutheast Asia to Latin America\n(N=1000)", cex = cex.main)
plot(0, 0, xlim = c(MIN, MAX), ylim = c(0.5, 3.5), xlab = XLAB, ylab = YLAB, main = MAIN, yaxt = "n")
lines(x = effect.japan.se.la[c(1, 3)], y = rep(3, 2))
points(x = effect.japan.se.la[2], pch = 20, y = 3)
lines(x = effect.uk.se.la[c(1, 3)], y = rep(2, 2))
points(x = effect.uk.se.la[2], pch = 20, y = 2)
lines(x = effect.china.se.la[c(1, 3)], y = rep(1, 2))
points(x = effect.china.se.la[2], pch = 20, y = 1)
text(x = X.text, y = 3, labels = "Substitution\nby Japan")
text(x = X.text, y = 2, labels = "Substitution\nby UK")
text(x = X.text, y = 1, labels = "Substitution\nby China")
abline(v = 0, lty = 2)



MAIN <- list("Aid Reallocation from\nAfrica to Southeast Asia\n(N=1029)", cex = cex.main)
plot(0, 0, xlim = c(MIN, MAX), ylim = c(0.5, 3.5), xlab = XLAB, ylab = YLAB, main = MAIN, yaxt = "n")
lines(x = effect.japan.af.se[c(1, 3)], y = rep(3, 2))
points(x = effect.japan.af.se[2], pch = 20, y = 3)
lines(x = effect.uk.af.se[c(1, 3)], y = rep(2, 2))
points(x = effect.uk.af.se[2], pch = 20, y = 2)
lines(x = effect.china.af.se[c(1, 3)], y = rep(1, 2))
points(x = effect.china.af.se[2], pch = 20, y = 1)
text(x = X.text, y = 3, labels = "Substitution\nby Japan")
text(x = X.text, y = 2, labels = "Substitution\nby UK")
text(x = X.text, y = 1, labels = "Substitution\nby China")
abline(v = 0, lty = 2)



plot(0, 0, xlab = "", ylab = "", axes = F, cex = 0)


MAIN <- list("Pooled\n(N=6095)", cex = cex.main)
plot(0, 0, xlim = c(MIN, MAX), ylim = c(0.5, 3.5), xlab = XLAB, ylab = YLAB, main = MAIN, yaxt = "n")
lines(x = effect.japan.pool[c(1, 3)], y = rep(3, 2))
points(x = effect.japan.pool[2], pch = 20, y = 3)
lines(x = effect.uk.pool[c(1, 3)], y = rep(2, 2))
points(x = effect.uk.pool[2], pch = 20, y = 2)
lines(x = effect.china.pool[c(1, 3)], y = rep(1, 2))
points(x = effect.china.pool[2], pch = 20, y = 1)
text(x = X.text, y = 3, labels = "Substitution\nby Japan")
text(x = X.text, y = 2, labels = "Substitution\nby UK")
text(x = X.text, y = 1, labels = "Substitution\nby China")
abline(v = 0, lty = 2)


plot(0, 0, xlab = "", ylab = "", axes = F, cex = 0)

dev.off()
##############################################




##############################################
### Figure 4.  Average Treatment Effects in U.S. Survey for Three Treatments
### Table A4: Effects of the Treatments on the Outcome Variable in the US Survey (I)
### Table A5: Effects of the Treatments on the Outcome Variable in the US Survey (II)
##############################################
MODEL <- list(NA, 7)
SE <- list(NA, 7)
##############################################
model <- lm(outcome ~ japan + uk + china + age + factor(gender) + factor(educ) + factor(division) + factor(profile_gross_household) + imp.af  + imp.la, data = data.us.af.la)
MODEL[[1]] <- model
SE[[1]] <- sqrt(diag(vcovHC(model)))
set.seed(1)
beta <- mvrnorm(1000, mu = coef(model), Sigma = vcovHC(model))
effect.japan.af.la <- c(quantile(beta[, 2], prob = 0.025), mean(beta[, 2]), quantile(beta[, 2], prob = 0.975))
effect.uk.af.la <- c(quantile(beta[, 3], prob = 0.025), mean(beta[, 3]), quantile(beta[, 3], prob = 0.975))
effect.china.af.la <- c(quantile(beta[, 4], prob = 0.025), mean(beta[, 4]), quantile(beta[, 4], prob = 0.975))



model <- lm(outcome ~ japan + uk + china + age + factor(gender) + factor(educ) + factor(division) + factor(profile_gross_household) + imp.la + imp.se, data = data.us.la.se)
MODEL[[2]] <- model
SE[[2]] <- sqrt(diag(vcovHC(model)))
set.seed(1)
beta <- mvrnorm(1000, mu = coef(model), Sigma = vcovHC(model))
effect.japan.la.se <- c(quantile(beta[, 2], prob = 0.025), mean(beta[, 2]), quantile(beta[, 2], prob = 0.975))
effect.uk.la.se <- c(quantile(beta[, 3], prob = 0.025), mean(beta[, 3]), quantile(beta[, 3], prob = 0.975))
effect.china.la.se <- c(quantile(beta[, 4], prob = 0.025), mean(beta[, 4]), quantile(beta[, 4], prob = 0.975))



model <- lm(outcome ~ japan + uk + china + age + factor(gender) + factor(educ) + factor(division) + factor(profile_gross_household) + imp.se + imp.af, data = data.us.se.af)
MODEL[[3]] <- model
SE[[3]] <- sqrt(diag(vcovHC(model)))
set.seed(1)
beta <- mvrnorm(1000, mu = coef(model), Sigma = vcovHC(model))
effect.japan.se.af <- c(quantile(beta[, 2], prob = 0.025), mean(beta[, 2]), quantile(beta[, 2], prob = 0.975))
effect.uk.se.af <- c(quantile(beta[, 3], prob = 0.025), mean(beta[, 3]), quantile(beta[, 3], prob = 0.975))
effect.china.se.af <- c(quantile(beta[, 4], prob = 0.025), mean(beta[, 4]), quantile(beta[, 4], prob = 0.975))




model <- lm(outcome ~ japan + uk + china + age + factor(gender) + factor(educ) + factor(division) + factor(profile_gross_household) + imp.la + imp.af, data = data.us.la.af)
MODEL[[4]] <- model
SE[[4]] <- sqrt(diag(vcovHC(model)))
set.seed(1)
beta <- mvrnorm(1000, mu = coef(model), Sigma = vcovHC(model))
effect.japan.la.af <- c(quantile(beta[, 2], prob = 0.025), mean(beta[, 2]), quantile(beta[, 2], prob = 0.975))
effect.uk.la.af <- c(quantile(beta[, 3], prob = 0.025), mean(beta[, 3]), quantile(beta[, 3], prob = 0.975))
effect.china.la.af <- c(quantile(beta[, 4], prob = 0.025), mean(beta[, 4]), quantile(beta[, 4], prob = 0.975))




model <- lm(outcome ~ japan + uk + china + age + factor(gender) + factor(educ) + factor(division) + factor(profile_gross_household) + imp.se + imp.la, data = data.us.se.la)
MODEL[[5]] <- model
SE[[5]] <- sqrt(diag(vcovHC(model)))
set.seed(1)
beta <- mvrnorm(1000, mu = coef(model), Sigma = vcovHC(model))
effect.japan.se.la <- c(quantile(beta[, 2], prob = 0.025), mean(beta[, 2]), quantile(beta[, 2], prob = 0.975))
effect.uk.se.la <- c(quantile(beta[, 3], prob = 0.025), mean(beta[, 3]), quantile(beta[, 3], prob = 0.975))
effect.china.se.la <- c(quantile(beta[, 4], prob = 0.025), mean(beta[, 4]), quantile(beta[, 4], prob = 0.975))


model <- lm(outcome ~ japan + uk + china + age + factor(gender) + factor(educ) + factor(division) + factor(profile_gross_household) + imp.af + imp.se, data = data.us.af.se)
MODEL[[6]] <- model
SE[[6]] <- sqrt(diag(vcovHC(model)))
set.seed(1)
beta <- mvrnorm(1000, mu = coef(model), Sigma = vcovHC(model))
effect.japan.af.se <- c(quantile(beta[, 2], prob = 0.025), mean(beta[, 2]), quantile(beta[, 2], prob = 0.975))
effect.uk.af.se <- c(quantile(beta[, 3], prob = 0.025), mean(beta[, 3]), quantile(beta[, 3], prob = 0.975))
effect.china.af.se <- c(quantile(beta[, 4], prob = 0.025), mean(beta[, 4]), quantile(beta[, 4], prob = 0.975))


model <- lm(outcome ~ japan + uk + china + age + factor(gender) + factor(educ) + factor(division) + factor(profile_gross_household) + imp.af + imp.la + imp.se, data = data.us)
MODEL[[7]] <- model
SE[[7]] <- sqrt(diag(vcovHC(model)))
set.seed(1)
beta <- mvrnorm(1000, mu = coef(model), Sigma = vcovHC(model))
effect.japan.pool <- c(quantile(beta[, 2], prob = 0.025), mean(beta[, 2]), quantile(beta[, 2], prob = 0.975))
effect.uk.pool <- c(quantile(beta[, 3], prob = 0.025), mean(beta[, 3]), quantile(beta[, 3], prob = 0.975))
effect.china.pool <- c(quantile(beta[, 4], prob = 0.025), mean(beta[, 4]), quantile(beta[, 4], prob = 0.975))
##############################################
stargazer(MODEL[[1]], MODEL[[2]], MODEL[[3]], MODEL[[4]], se = list(SE[[1]], SE[[2]], SE[[3]], SE[[4]]), star.char = c("*", "**", "***"), star.cutoffs = c(.05, .01, .001), omit = c("age", "gender", "educ", "division", "profile_gross_household"))

stargazer(MODEL[[5]], MODEL[[6]], MODEL[[7]], se = list(SE[[5]], SE[[6]], SE[[7]]), star.char = c("*", "**", "***"), star.cutoffs = c(.05, .01, .001), omit = c("age", "gender", "educ", "division", "profile_gross_household"))
##############################################
pdf("figure_4.pdf", width = 9, height = 9)

par(mfrow = c(3, 3))

XLAB <- list("Average Treatment Effect", cex = 1.2)
YLAB <- ""
cex.main <- 1.5
MIN <- -0.5
MAX <- 0.5
X.text <- 0.38

MAIN <- list("Aid Reallocation from\nAfrica to Latin America\n(N=1001)", cex = cex.main)
plot(0, 0, xlim = c(MIN, MAX), ylim = c(0.5, 3.5), xlab = XLAB, ylab = YLAB, main = MAIN, yaxt = "n")
lines(x = effect.japan.af.la[c(1, 3)], y = rep(3, 2))
points(x = effect.japan.af.la[2], pch = 20, y = 3)
lines(x = effect.uk.af.la[c(1, 3)], y = rep(2, 2))
points(x = effect.uk.af.la[2], pch = 20, y = 2)
lines(x = effect.china.af.la[c(1, 3)], y = rep(1, 2))
points(x = effect.china.af.la[2], pch = 20, y = 1)
text(x = X.text, y = 3, labels = "Substitution\nby Japan")
text(x = X.text, y = 2, labels = "Substitution\nby UK")
text(x = X.text, y = 1, labels = "Substitution\nby China")
abline(v = 0, lty = 2)



MAIN <- list("Aid Reallocation from\nLatin America to Southeast Asia\n(N=1007)", cex = cex.main)
plot(0, 0, xlim = c(MIN, MAX), ylim = c(0.5, 3.5), xlab = XLAB, ylab = YLAB, main = MAIN, yaxt = "n")
lines(x = effect.japan.la.se[c(1, 3)], y = rep(3, 2))
points(x = effect.japan.la.se[2], pch = 20, y = 3)
lines(x = effect.uk.la.se[c(1, 3)], y = rep(2, 2))
points(x = effect.uk.la.se[2], pch = 20, y = 2)
lines(x = effect.china.la.se[c(1, 3)], y = rep(1, 2))
points(x = effect.china.la.se[2], pch = 20, y = 1)
text(x = X.text, y = 3, labels = "Substitution\nby Japan")
text(x = X.text, y = 2, labels = "Substitution\nby UK")
text(x = X.text, y = 1, labels = "Substitution\nby China")
abline(v = 0, lty = 2)



MAIN <- list("Aid Reallocation from\nSoutheast Asia to Africa\n(N=1030)", cex = cex.main)
plot(0, 0, xlim = c(MIN, MAX), ylim = c(0.5, 3.5), xlab = XLAB, ylab = YLAB, main = MAIN, yaxt = "n")
lines(x = effect.japan.se.af[c(1, 3)], y = rep(3, 2))
points(x = effect.japan.se.af[2], pch = 20, y = 3)
lines(x = effect.uk.se.af[c(1, 3)], y = rep(2, 2))
points(x = effect.uk.se.af[2], pch = 20, y = 2)
lines(x = effect.china.se.af[c(1, 3)], y = rep(1, 2))
points(x = effect.china.se.af[2], pch = 20, y = 1)
text(x = X.text, y = 3, labels = "Substitution\nby Japan")
text(x = X.text, y = 2, labels = "Substitution\nby UK")
text(x = X.text, y = 1, labels = "Substitution\nby China")
abline(v = 0, lty = 2)



MAIN <- list("Aid Reallocation from\nLatin America to Africa\n(N=1028)", cex = cex.main)
plot(0, 0, xlim = c(MIN, MAX), ylim = c(0.5, 3.5), xlab = XLAB, ylab = YLAB, main = MAIN, yaxt = "n")
lines(x = effect.japan.la.af[c(1, 3)], y = rep(3, 2))
points(x = effect.japan.la.af[2], pch = 20, y = 3)
lines(x = effect.uk.la.af[c(1, 3)], y = rep(2, 2))
points(x = effect.uk.la.af[2], pch = 20, y = 2)
lines(x = effect.china.la.af[c(1, 3)], y = rep(1, 2))
points(x = effect.china.la.af[2], pch = 20, y = 1)
text(x = X.text, y = 3, labels = "Substitution\nby Japan")
text(x = X.text, y = 2, labels = "Substitution\nby UK")
text(x = X.text, y = 1, labels = "Substitution\nby China")
abline(v = 0, lty = 2)



MAIN <- list("Aid Reallocation from\nSoutheast Asia to Latin America\n(N=1000)", cex = cex.main)
plot(0, 0, xlim = c(MIN, MAX), ylim = c(0.5, 3.5), xlab = XLAB, ylab = YLAB, main = MAIN, yaxt = "n")
lines(x = effect.japan.se.la[c(1, 3)], y = rep(3, 2))
points(x = effect.japan.se.la[2], pch = 20, y = 3)
lines(x = effect.uk.se.la[c(1, 3)], y = rep(2, 2))
points(x = effect.uk.se.la[2], pch = 20, y = 2)
lines(x = effect.china.se.la[c(1, 3)], y = rep(1, 2))
points(x = effect.china.se.la[2], pch = 20, y = 1)
text(x = X.text, y = 3, labels = "Substitution\nby Japan")
text(x = X.text, y = 2, labels = "Substitution\nby UK")
text(x = X.text, y = 1, labels = "Substitution\nby China")
abline(v = 0, lty = 2)



MAIN <- list("Aid Reallocation from\nAfrica to Southeast Asia\n(N=1029)", cex = cex.main)
plot(0, 0, xlim = c(MIN, MAX), ylim = c(0.5, 3.5), xlab = XLAB, ylab = YLAB, main = MAIN, yaxt = "n")
lines(x = effect.japan.af.se[c(1, 3)], y = rep(3, 2))
points(x = effect.japan.af.se[2], pch = 20, y = 3)
lines(x = effect.uk.af.se[c(1, 3)], y = rep(2, 2))
points(x = effect.uk.af.se[2], pch = 20, y = 2)
lines(x = effect.china.af.se[c(1, 3)], y = rep(1, 2))
points(x = effect.china.af.se[2], pch = 20, y = 1)
text(x = X.text, y = 3, labels = "Substitution\nby Japan")
text(x = X.text, y = 2, labels = "Substitution\nby UK")
text(x = X.text, y = 1, labels = "Substitution\nby China")
abline(v = 0, lty = 2)



plot(0, 0, xlab = "", ylab = "", axes = F, cex = 0)


MAIN <- list("Pooled\n(N=6095)", cex = cex.main)
plot(0, 0, xlim = c(MIN, MAX), ylim = c(0.5, 3.5), xlab = XLAB, ylab = YLAB, main = MAIN, yaxt = "n")
lines(x = effect.japan.pool[c(1, 3)], y = rep(3, 2))
points(x = effect.japan.pool[2], pch = 20, y = 3)
lines(x = effect.uk.pool[c(1, 3)], y = rep(2, 2))
points(x = effect.uk.pool[2], pch = 20, y = 2)
lines(x = effect.china.pool[c(1, 3)], y = rep(1, 2))
points(x = effect.china.pool[2], pch = 20, y = 1)
text(x = X.text, y = 3, labels = "Substitution\nby Japan")
text(x = X.text, y = 2, labels = "Substitution\nby UK")
text(x = X.text, y = 1, labels = "Substitution\nby China")
abline(v = 0, lty = 2)


plot(0, 0, xlab = "", ylab = "", axes = F, cex = 0)

dev.off()
##############################################




##############################################
### Figure A4.  Average Treatment Effects in Japan Survey for Two Treatments (binary outcome variable)
##############################################
MODEL <- list(NA, 3)
SE <- list(NA, 3)
##############################################
model <- glm(outcome.bin ~ friend + enemy
            + imp.africa + imp.asia + male + age + factor(resid) + factor(income) + factor(educ), data = data.jp.africa, family = binomial(link = "probit"))

summary(model)

MODEL[[1]] <- model
SE[[1]] <- sqrt(diag(vcovHC(model)))

coef.africa <- coef(model)
vcov.africa <- vcovHC(model)

linearHypothesis(model, "friend = enemy", vcov. = vcov.africa)
##############################################
model <- glm(outcome.bin ~ friend + enemy
            + imp.africa + imp.asia + male + age + factor(resid) + factor(income) + factor(educ), data = data.jp.asia, family = binomial(link = "probit"))

summary(model)

MODEL[[2]] <- model
SE[[2]] <- sqrt(diag(vcovHC(model)))

coef.asia <- coef(model)
vcov.asia <- vcovHC(model)

linearHypothesis(model, "friend = enemy", vcov. = vcov.asia)
##############################################
model <- glm(outcome.bin ~ friend + enemy
            + imp.africa + imp.asia + male + age + factor(resid) + factor(income) + factor(educ), data = data.jp, family = binomial(link = "probit"))

summary(model)

MODEL[[3]] <- model
SE[[3]] <- sqrt(diag(vcovHC(model)))

coef.pool <- coef(model)
vcov.pool <- vcovHC(model)

linearHypothesis(model, "friend = enemy", vcov. = vcov.pool)
##############################################
stargazer(MODEL[[1]], MODEL[[2]], MODEL[[3]], se = list(SE[[1]], SE[[2]], SE[[3]]), star.char = c("*", "**", "***"), star.cutoffs = c(.05, .01, .001))
##############################################
set.seed(1)

beta <- mvrnorm(1000, mu = coef.africa, Sigma = vcov.africa)

latent.africa.control <- beta[, 1] +
    beta[, 2] * 0 +
    beta[, 3] * 0 +
    beta[, 4] * mean(data.jp$imp.africa, na.rm = T) +
    beta[, 5] * mean(data.jp$imp.asia, na.rm = T) +
    beta[, 6] * 0  +
    beta[, 7] * mean(data.jp$age, na.rm = T) +
    beta[, 9] * 1 +
    beta[, 17] * 1 +
    beta[, 34] * 1 
prob.africa.control <- pnorm(latent.africa.control)

latent.africa.friend <- beta[, 1] +
    beta[, 2] * 1 +
    beta[, 3] * 0 +
    beta[, 4] * mean(data.jp$imp.africa, na.rm = T) +
    beta[, 5] * mean(data.jp$imp.asia, na.rm = T) +
    beta[, 6] * 0  +
    beta[, 7] * mean(data.jp$age, na.rm = T) +
    beta[, 9] * 1 +
    beta[, 17] * 1 +
    beta[, 34] * 1 
prob.africa.friend <- pnorm(latent.africa.friend)

latent.africa.enemy <- beta[, 1] +
    beta[, 2] * 0 +
    beta[, 3] * 1 +
    beta[, 4] * mean(data.jp$imp.africa, na.rm = T) +
    beta[, 5] * mean(data.jp$imp.asia, na.rm = T) +
    beta[, 6] * 0  +
    beta[, 7] * mean(data.jp$age, na.rm = T) +
    beta[, 9] * 1 +
    beta[, 17] * 1 +
    beta[, 34] * 1 
prob.africa.enemy <- pnorm(latent.africa.enemy)

x <- prob.africa.friend - prob.africa.control
effect.friend.africa <- c(quantile(x, prob = 0.025), mean(x), quantile(x, prob = 0.975))

x <- prob.africa.enemy - prob.africa.control
effect.enemy.africa <- c(quantile(x, prob = 0.025), mean(x), quantile(x, prob = 0.975))
##############################################
set.seed(1)

beta <- mvrnorm(1000, mu = coef.asia, Sigma = vcov.asia)

latent.asia.control <- beta[, 1] +
    beta[, 2] * 0 +
    beta[, 3] * 0 +
    beta[, 4] * mean(data.jp$imp.africa, na.rm = T) +
    beta[, 5] * mean(data.jp$imp.asia, na.rm = T) +
    beta[, 6] * 0  +
    beta[, 7] * mean(data.jp$age, na.rm = T) +
    beta[, 9] * 1 +
    beta[, 17] * 1 +
    beta[, 34] * 1 
prob.asia.control <- pnorm(latent.asia.control)

latent.asia.friend <- beta[, 1] +
    beta[, 2] * 1 +
    beta[, 3] * 0 +
    beta[, 4] * mean(data.jp$imp.africa, na.rm = T) +
    beta[, 5] * mean(data.jp$imp.asia, na.rm = T) +
    beta[, 6] * 0  +
    beta[, 7] * mean(data.jp$age, na.rm = T) +
    beta[, 9] * 1 +
    beta[, 17] * 1 +
    beta[, 34] * 1 
prob.asia.friend <- pnorm(latent.asia.friend)

latent.asia.enemy <- beta[, 1] +
    beta[, 2] * 0 +
    beta[, 3] * 1 +
    beta[, 4] * mean(data.jp$imp.africa, na.rm = T) +
    beta[, 5] * mean(data.jp$imp.asia, na.rm = T) +
    beta[, 6] * 0  +
    beta[, 7] * mean(data.jp$age, na.rm = T) +
    beta[, 9] * 1 +
    beta[, 17] * 1 +
    beta[, 34] * 1 
prob.asia.enemy <- pnorm(latent.asia.enemy)

x <- prob.asia.friend - prob.asia.control
effect.friend.asia <- c(quantile(x, prob = 0.025), mean(x), quantile(x, prob = 0.975))

x <- prob.asia.enemy - prob.asia.control
effect.enemy.asia <- c(quantile(x, prob = 0.025), mean(x), quantile(x, prob = 0.975))
##############################################
set.seed(1)

beta <- mvrnorm(1000, mu = coef.pool, Sigma = vcov.pool)

latent.pool.control <- beta[, 1] +
    beta[, 2] * 0 +
    beta[, 3] * 0 +
    beta[, 4] * mean(data.jp$imp.africa, na.rm = T) +
    beta[, 5] * mean(data.jp$imp.asia, na.rm = T) +
    beta[, 6] * 0  +
    beta[, 7] * mean(data.jp$age, na.rm = T) +
    beta[, 9] * 1 +
    beta[, 17] * 1 +
    beta[, 34] * 1 
prob.pool.control <- pnorm(latent.pool.control)

latent.pool.friend <- beta[, 1] +
    beta[, 2] * 1 +
    beta[, 3] * 0 +
    beta[, 4] * mean(data.jp$imp.africa, na.rm = T) +
    beta[, 5] * mean(data.jp$imp.asia, na.rm = T) +
    beta[, 6] * 0  +
    beta[, 7] * mean(data.jp$age, na.rm = T) +
    beta[, 9] * 1 +
    beta[, 17] * 1 +
    beta[, 34] * 1 
prob.pool.friend <- pnorm(latent.pool.friend)

latent.pool.enemy <- beta[, 1] +
    beta[, 2] * 0 +
    beta[, 3] * 1 +
    beta[, 4] * mean(data.jp$imp.africa, na.rm = T) +
    beta[, 5] * mean(data.jp$imp.asia, na.rm = T) +
    beta[, 6] * 0  +
    beta[, 7] * mean(data.jp$age, na.rm = T) +
    beta[, 9] * 1 +
    beta[, 17] * 1 +
    beta[, 34] * 1 
prob.pool.enemy <- pnorm(latent.pool.enemy)

x <- prob.pool.friend - prob.pool.control
effect.friend.pool <- c(quantile(x, prob = 0.025), mean(x), quantile(x, prob = 0.975))

x <- prob.pool.enemy - prob.pool.control
effect.enemy.pool <- c(quantile(x, prob = 0.025), mean(x), quantile(x, prob = 0.975))
##############################################
pdf("figure_A4.pdf", width = 9, height = 3)

par(mfrow = c(1, 3))

MAX <- max(effect.friend.africa, effect.friend.asia, effect.enemy.africa, effect.enemy.africa, effect.friend.pool, effect.friend.pool)
MIN <- min(effect.friend.africa, effect.friend.asia, effect.enemy.africa, effect.enemy.africa, effect.friend.pool, effect.friend.pool)

plot(y = c(1, 2), x = c(effect.friend.africa[2], effect.enemy.africa[2]), ylim = c(0.5, 2.5), xlim = c(MIN, -MIN), main = "Aid Reallocation from\nAfrica to Southeast Asia\n(N=1493)", xlab = "Average Treatment Effect", yaxt = "n", ylab = "", pch = 19)

lines(y = c(1, 1), x = c(effect.friend.africa[1], effect.friend.africa[3]))
lines(y = c(2, 2), x = c(effect.enemy.africa[1], effect.enemy.africa[3]))
abline(v = 0, lty = 2)

text(x = c(-MIN, -MIN) - 0.08, y = c(1, 2), labels = c("Substitution\nby US", "Substitution\nby China"), cex = 0.9)

plot(y = c(1, 2), x = c(effect.friend.asia[2], effect.enemy.asia[2]), ylim = c(0.5, 2.5), xlim = c(MIN, -MIN), main = "Aid Reallocation from\nSoutheast Asia to Africa\n(N=1507)", xlab = "Average Treatment Effect", yaxt = "n", ylab = "", pch = 19)

lines(y = c(1, 1), x = c(effect.friend.asia[1], effect.friend.asia[3]))
lines(y = c(2, 2), x = c(effect.enemy.asia[1], effect.enemy.asia[3]))
abline(v = 0, lty = 2)

text(x = c(-MIN, -MIN) - 0.08, y = c(1, 2), labels = c("Substitution\nby US", "Substitution\nby China"), cex = 0.9)

plot(y = c(1, 2), x = c(effect.friend.pool[2], effect.enemy.pool[2]), ylim = c(0.5, 2.5), xlim = c(MIN, -MIN), main = "Pooled\n(N=3000)", xlab = "Average Treatment Effect", yaxt = "n", ylab = "", pch = 19)

lines(y = c(1, 1), x = c(effect.friend.pool[1], effect.friend.pool[3]))
lines(y = c(2, 2), x = c(effect.enemy.pool[1], effect.enemy.pool[3]))
abline(v = 0, lty = 2)

text(x = c(-MIN, -MIN) - 0.08, y = c(1, 2), labels = c("Substitution\nby US", "Substitution\nby China"), cex = 0.9)

dev.off()
##############################################





##############################################
### Figure A5.  Average Treatment Effects in U.S. Survey for Two Treatments (binary outcome variable)
##############################################
MODEL <- list(NA, 7)
SE <- list(NA, 7)
##############################################
model <- glm(outcome.bin ~ japan + uk + china + age + factor(gender) + factor(educ) + factor(division) + factor(profile_gross_household) + imp.af  + imp.la, data = data.us.af.la, family = binomial(link = "probit"))
MODEL[[1]] <- model
SE[[1]] <- sqrt(diag(vcovHC(model)))
set.seed(1)
beta <- mvrnorm(1000, mu = coef(model), Sigma = vcovHC(model))

latent.control <- beta[, 1] +
    beta[, 2] * 0 +
    beta[, 3] * 0 +
    beta[, 4] * 0 +
    beta[, 5] * mean(data.us$age, na.rm = T) +
    beta[, 6] * 1 +
    beta[, 7] * 1 +
    beta[, 15] * 1 +
    beta[, 20] * 1 +
    beta[, 37] * mean(data.us$imp.af, na.rm = T) +
    beta[, 38] * mean(data.us$imp.la, na.rm = T) 
prob.control <- pnorm(latent.control)

latent.japan <- beta[, 1] +
    beta[, 2] * 1 +
    beta[, 3] * 0 +
    beta[, 4] * 0 +
    beta[, 5] * mean(data.us$age, na.rm = T) +
    beta[, 6] * 1 +
    beta[, 7] * 1 +
    beta[, 15] * 1 +
    beta[, 20] * 1 +
    beta[, 37] * mean(data.us$imp.af, na.rm = T) +
    beta[, 38] * mean(data.us$imp.la, na.rm = T) 
prob.japan <- pnorm(latent.japan)

latent.uk <- beta[, 1] +
    beta[, 2] * 0 +
    beta[, 3] * 1 +
    beta[, 4] * 0 +
    beta[, 5] * mean(data.us$age, na.rm = T) +
    beta[, 6] * 1 +
    beta[, 7] * 1 +
    beta[, 15] * 1 +
    beta[, 20] * 1 +
    beta[, 37] * mean(data.us$imp.af, na.rm = T) +
    beta[, 38] * mean(data.us$imp.la, na.rm = T) 
prob.uk <- pnorm(latent.uk)

latent.china <- beta[, 1] +
    beta[, 2] * 0 +
    beta[, 3] * 0 +
    beta[, 4] * 1 +
    beta[, 5] * mean(data.us$age, na.rm = T) +
    beta[, 6] * 1 +
    beta[, 7] * 1 +
    beta[, 15] * 1 +
    beta[, 20] * 1 +
    beta[, 37] * mean(data.us$imp.af, na.rm = T) +
    beta[, 38] * mean(data.us$imp.la, na.rm = T) 
prob.china <- pnorm(latent.china)

x <- prob.japan - prob.control
effect.japan.af.la <- c(quantile(x, prob = 0.025), mean(x), quantile(x, prob = 0.975))

x <- prob.uk - prob.control
effect.uk.af.la <- c(quantile(x, prob = 0.025), mean(x), quantile(x, prob = 0.975))

x <- prob.china - prob.control
effect.china.af.la <- c(quantile(x, prob = 0.025), mean(x), quantile(x, prob = 0.975))
##############################################
model <- glm(outcome.bin ~ japan + uk + china + age + factor(gender) + factor(educ) + factor(division) + factor(profile_gross_household) + imp.la + imp.se, data = data.us.la.se, family = binomial(link = "probit"))
MODEL[[2]] <- model
SE[[2]] <- sqrt(diag(vcovHC(model)))
set.seed(1)
beta <- mvrnorm(1000, mu = coef(model), Sigma = vcovHC(model))

latent.control <- beta[, 1] +
    beta[, 2] * 0 +
    beta[, 3] * 0 +
    beta[, 4] * 0 +
    beta[, 5] * mean(data.us$age, na.rm = T) +
    beta[, 6] * 1 +
    beta[, 7] * 1 +
    beta[, 15] * 1 +
    beta[, 20] * 1 +
    beta[, 37] * mean(data.us$imp.la, na.rm = T) +
    beta[, 38] * mean(data.us$imp.se, na.rm = T) 
prob.control <- pnorm(latent.control)

latent.japan <- beta[, 1] +
    beta[, 2] * 1 +
    beta[, 3] * 0 +
    beta[, 4] * 0 +
    beta[, 5] * mean(data.us$age, na.rm = T) +
    beta[, 6] * 1 +
    beta[, 7] * 1 +
    beta[, 15] * 1 +
    beta[, 20] * 1 +
    beta[, 37] * mean(data.us$imp.la, na.rm = T) +
    beta[, 38] * mean(data.us$imp.se, na.rm = T) 
prob.japan <- pnorm(latent.japan)

latent.uk <- beta[, 1] +
    beta[, 2] * 0 +
    beta[, 3] * 1 +
    beta[, 4] * 0 +
    beta[, 5] * mean(data.us$age, na.rm = T) +
    beta[, 6] * 1 +
    beta[, 7] * 1 +
    beta[, 15] * 1 +
    beta[, 20] * 1 +
    beta[, 37] * mean(data.us$imp.la, na.rm = T) +
    beta[, 38] * mean(data.us$imp.se, na.rm = T) 
prob.uk <- pnorm(latent.uk)

latent.china <- beta[, 1] +
    beta[, 2] * 0 +
    beta[, 3] * 0 +
    beta[, 4] * 1 +
    beta[, 5] * mean(data.us$age, na.rm = T) +
    beta[, 6] * 1 +
    beta[, 7] * 1 +
    beta[, 15] * 1 +
    beta[, 20] * 1 +
    beta[, 37] * mean(data.us$imp.la, na.rm = T) +
    beta[, 38] * mean(data.us$imp.se, na.rm = T) 
prob.china <- pnorm(latent.china)

x <- prob.japan - prob.control
effect.japan.la.se <- c(quantile(x, prob = 0.025), mean(x), quantile(x, prob = 0.975))

x <- prob.uk - prob.control
effect.uk.la.se <- c(quantile(x, prob = 0.025), mean(x), quantile(x, prob = 0.975))

x <- prob.china - prob.control
effect.china.la.se <- c(quantile(x, prob = 0.025), mean(x), quantile(x, prob = 0.975))
##############################################
model <- glm(outcome.bin ~ japan + uk + china + age + factor(gender) + factor(educ) + factor(division) + factor(profile_gross_household) + imp.se + imp.af, data = data.us.se.af, family = binomial(link = "probit"))
MODEL[[3]] <- model
SE[[3]] <- sqrt(diag(vcovHC(model)))
set.seed(1)
beta <- mvrnorm(1000, mu = coef(model), Sigma = vcovHC(model))

latent.control <- beta[, 1] +
    beta[, 2] * 0 +
    beta[, 3] * 0 +
    beta[, 4] * 0 +
    beta[, 5] * mean(data.us$age, na.rm = T) +
    beta[, 6] * 1 +
    beta[, 7] * 1 +
    beta[, 15] * 1 +
    beta[, 20] * 1 +
    beta[, 37] * mean(data.us$imp.se, na.rm = T) +
    beta[, 38] * mean(data.us$imp.af, na.rm = T) 
prob.control <- pnorm(latent.control)

latent.japan <- beta[, 1] +
    beta[, 2] * 1 +
    beta[, 3] * 0 +
    beta[, 4] * 0 +
    beta[, 5] * mean(data.us$age, na.rm = T) +
    beta[, 6] * 1 +
    beta[, 7] * 1 +
    beta[, 15] * 1 +
    beta[, 20] * 1 +
    beta[, 37] * mean(data.us$imp.se, na.rm = T) +
    beta[, 38] * mean(data.us$imp.af, na.rm = T) 
prob.japan <- pnorm(latent.japan)

latent.uk <- beta[, 1] +
    beta[, 2] * 0 +
    beta[, 3] * 1 +
    beta[, 4] * 0 +
    beta[, 5] * mean(data.us$age, na.rm = T) +
    beta[, 6] * 1 +
    beta[, 7] * 1 +
    beta[, 15] * 1 +
    beta[, 20] * 1 +
    beta[, 37] * mean(data.us$imp.se, na.rm = T) +
    beta[, 38] * mean(data.us$imp.af, na.rm = T) 
prob.uk <- pnorm(latent.uk)

latent.china <- beta[, 1] +
    beta[, 2] * 0 +
    beta[, 3] * 0 +
    beta[, 4] * 1 +
    beta[, 5] * mean(data.us$age, na.rm = T) +
    beta[, 6] * 1 +
    beta[, 7] * 1 +
    beta[, 15] * 1 +
    beta[, 20] * 1 +
    beta[, 37] * mean(data.us$imp.se, na.rm = T) +
    beta[, 38] * mean(data.us$imp.af, na.rm = T) 
prob.china <- pnorm(latent.china)

x <- prob.japan - prob.control
effect.japan.se.af <- c(quantile(x, prob = 0.025), mean(x), quantile(x, prob = 0.975))

x <- prob.uk - prob.control
effect.uk.se.af <- c(quantile(x, prob = 0.025), mean(x), quantile(x, prob = 0.975))

x <- prob.china - prob.control
effect.china.se.af <- c(quantile(x, prob = 0.025), mean(x), quantile(x, prob = 0.975))
##############################################
model <- glm(outcome.bin ~ japan + uk + china + age + factor(gender) + factor(educ) + factor(division) + factor(profile_gross_household) + imp.la + imp.af, data = data.us.la.af, family = binomial(link = "probit"))
MODEL[[4]] <- model
SE[[4]] <- sqrt(diag(vcovHC(model)))
set.seed(1)
beta <- mvrnorm(1000, mu = coef(model), Sigma = vcovHC(model))
latent.control <- beta[, 1] +
    beta[, 2] * 0 +
    beta[, 3] * 0 +
    beta[, 4] * 0 +
    beta[, 5] * mean(data.us$age, na.rm = T) +
    beta[, 6] * 1 +
    beta[, 7] * 1 +
    beta[, 15] * 1 +
    beta[, 20] * 1 +
    beta[, 37] * mean(data.us$imp.la, na.rm = T) +
    beta[, 38] * mean(data.us$imp.af, na.rm = T) 
prob.control <- pnorm(latent.control)

latent.japan <- beta[, 1] +
    beta[, 2] * 1 +
    beta[, 3] * 0 +
    beta[, 4] * 0 +
    beta[, 5] * mean(data.us$age, na.rm = T) +
    beta[, 6] * 1 +
    beta[, 7] * 1 +
    beta[, 15] * 1 +
    beta[, 20] * 1 +
    beta[, 37] * mean(data.us$imp.la, na.rm = T) +
    beta[, 38] * mean(data.us$imp.af, na.rm = T) 
prob.japan <- pnorm(latent.japan)

latent.uk <- beta[, 1] +
    beta[, 2] * 0 +
    beta[, 3] * 1 +
    beta[, 4] * 0 +
    beta[, 5] * mean(data.us$age, na.rm = T) +
    beta[, 6] * 1 +
    beta[, 7] * 1 +
    beta[, 15] * 1 +
    beta[, 20] * 1 +
    beta[, 37] * mean(data.us$imp.la, na.rm = T) +
    beta[, 38] * mean(data.us$imp.af, na.rm = T) 
prob.uk <- pnorm(latent.uk)

latent.china <- beta[, 1] +
    beta[, 2] * 0 +
    beta[, 3] * 0 +
    beta[, 4] * 1 +
    beta[, 5] * mean(data.us$age, na.rm = T) +
    beta[, 6] * 1 +
    beta[, 7] * 1 +
    beta[, 15] * 1 +
    beta[, 20] * 1 +
    beta[, 37] * mean(data.us$imp.la, na.rm = T) +
    beta[, 38] * mean(data.us$imp.af, na.rm = T) 
prob.china <- pnorm(latent.china)

x <- prob.japan - prob.control
effect.japan.la.af <- c(quantile(x, prob = 0.025), mean(x), quantile(x, prob = 0.975))

x <- prob.uk - prob.control
effect.uk.la.af <- c(quantile(x, prob = 0.025), mean(x), quantile(x, prob = 0.975))

x <- prob.china - prob.control
effect.china.la.af <- c(quantile(x, prob = 0.025), mean(x), quantile(x, prob = 0.975))
##############################################
model <- glm(outcome.bin ~ japan + uk + china + age + factor(gender) + factor(educ) + factor(division) + factor(profile_gross_household) + imp.se + imp.la, data = data.us.se.la, family = binomial(link = "probit"))
MODEL[[5]] <- model
SE[[5]] <- sqrt(diag(vcovHC(model)))
set.seed(1)
beta <- mvrnorm(1000, mu = coef(model), Sigma = vcovHC(model))
latent.control <- beta[, 1] +
    beta[, 2] * 0 +
    beta[, 3] * 0 +
    beta[, 4] * 0 +
    beta[, 5] * mean(data.us$age, na.rm = T) +
    beta[, 6] * 1 +
    beta[, 7] * 1 +
    beta[, 15] * 1 +
    beta[, 20] * 1 +
    beta[, 37] * mean(data.us$imp.se, na.rm = T) +
    beta[, 38] * mean(data.us$imp.la, na.rm = T) 
prob.control <- pnorm(latent.control)

latent.japan <- beta[, 1] +
    beta[, 2] * 1 +
    beta[, 3] * 0 +
    beta[, 4] * 0 +
    beta[, 5] * mean(data.us$age, na.rm = T) +
    beta[, 6] * 1 +
    beta[, 7] * 1 +
    beta[, 15] * 1 +
    beta[, 20] * 1 +
    beta[, 37] * mean(data.us$imp.se, na.rm = T) +
    beta[, 38] * mean(data.us$imp.la, na.rm = T) 
prob.japan <- pnorm(latent.japan)

latent.uk <- beta[, 1] +
    beta[, 2] * 0 +
    beta[, 3] * 1 +
    beta[, 4] * 0 +
    beta[, 5] * mean(data.us$age, na.rm = T) +
    beta[, 6] * 1 +
    beta[, 7] * 1 +
    beta[, 15] * 1 +
    beta[, 20] * 1 +
    beta[, 37] * mean(data.us$imp.se, na.rm = T) +
    beta[, 38] * mean(data.us$imp.la, na.rm = T) 
prob.uk <- pnorm(latent.uk)

latent.china <- beta[, 1] +
    beta[, 2] * 0 +
    beta[, 3] * 0 +
    beta[, 4] * 1 +
    beta[, 5] * mean(data.us$age, na.rm = T) +
    beta[, 6] * 1 +
    beta[, 7] * 1 +
    beta[, 15] * 1 +
    beta[, 20] * 1 +
    beta[, 37] * mean(data.us$imp.se, na.rm = T) +
    beta[, 38] * mean(data.us$imp.la, na.rm = T) 
prob.china <- pnorm(latent.china)

x <- prob.japan - prob.control
effect.japan.se.la <- c(quantile(x, prob = 0.025), mean(x), quantile(x, prob = 0.975))

x <- prob.uk - prob.control
effect.uk.se.la <- c(quantile(x, prob = 0.025), mean(x), quantile(x, prob = 0.975))

x <- prob.china - prob.control
effect.china.se.la <- c(quantile(x, prob = 0.025), mean(x), quantile(x, prob = 0.975))
##############################################
model <- glm(outcome.bin ~ japan + uk + china + age + factor(gender) + factor(educ) + factor(division) + factor(profile_gross_household) + imp.af + imp.se, data = data.us.af.se, family = binomial(link = "probit"))
MODEL[[6]] <- model
SE[[6]] <- sqrt(diag(vcovHC(model)))
set.seed(1)
beta <- mvrnorm(1000, mu = coef(model), Sigma = vcovHC(model))
latent.control <- beta[, 1] +
    beta[, 2] * 0 +
    beta[, 3] * 0 +
    beta[, 4] * 0 +
    beta[, 5] * mean(data.us$age, na.rm = T) +
    beta[, 6] * 1 +
    beta[, 7] * 1 +
    beta[, 15] * 1 +
    beta[, 20] * 1 +
    beta[, 37] * mean(data.us$imp.af, na.rm = T) +
    beta[, 38] * mean(data.us$imp.se, na.rm = T) 
prob.control <- pnorm(latent.control)

latent.japan <- beta[, 1] +
    beta[, 2] * 1 +
    beta[, 3] * 0 +
    beta[, 4] * 0 +
    beta[, 5] * mean(data.us$age, na.rm = T) +
    beta[, 6] * 1 +
    beta[, 7] * 1 +
    beta[, 15] * 1 +
    beta[, 20] * 1 +
    beta[, 37] * mean(data.us$imp.af, na.rm = T) +
    beta[, 38] * mean(data.us$imp.se, na.rm = T) 
prob.japan <- pnorm(latent.japan)

latent.uk <- beta[, 1] +
    beta[, 2] * 0 +
    beta[, 3] * 1 +
    beta[, 4] * 0 +
    beta[, 5] * mean(data.us$age, na.rm = T) +
    beta[, 6] * 1 +
    beta[, 7] * 1 +
    beta[, 15] * 1 +
    beta[, 20] * 1 +
    beta[, 37] * mean(data.us$imp.af, na.rm = T) +
    beta[, 38] * mean(data.us$imp.se, na.rm = T) 
prob.uk <- pnorm(latent.uk)

latent.china <- beta[, 1] +
    beta[, 2] * 0 +
    beta[, 3] * 0 +
    beta[, 4] * 1 +
    beta[, 5] * mean(data.us$age, na.rm = T) +
    beta[, 6] * 1 +
    beta[, 7] * 1 +
    beta[, 15] * 1 +
    beta[, 20] * 1 +
    beta[, 37] * mean(data.us$imp.af, na.rm = T) +
    beta[, 38] * mean(data.us$imp.se, na.rm = T) 
prob.china <- pnorm(latent.china)

x <- prob.japan - prob.control
effect.japan.af.se <- c(quantile(x, prob = 0.025), mean(x), quantile(x, prob = 0.975))

x <- prob.uk - prob.control
effect.uk.af.se <- c(quantile(x, prob = 0.025), mean(x), quantile(x, prob = 0.975))

x <- prob.china - prob.control
effect.china.af.se <- c(quantile(x, prob = 0.025), mean(x), quantile(x, prob = 0.975))
##############################################
model <- glm(outcome.bin ~ japan + uk + china + age + factor(gender) + factor(educ) + factor(division) + factor(profile_gross_household) + imp.af + imp.la + imp.se, data = data.us, family = binomial(link = "probit"))
MODEL[[7]] <- model
SE[[7]] <- sqrt(diag(vcovHC(model)))
set.seed(1)
beta <- mvrnorm(1000, mu = coef(model), Sigma = vcovHC(model))
latent.control <- beta[, 1] +
    beta[, 2] * 0 +
    beta[, 3] * 0 +
    beta[, 4] * 0 +
    beta[, 5] * mean(data.us$age, na.rm = T) +
    beta[, 6] * 1 +
    beta[, 7] * 1 +
    beta[, 15] * 1 +
    beta[, 20] * 1 +
    beta[, 37] * mean(data.us$imp.af, na.rm = T) +
    beta[, 38] * mean(data.us$imp.la, na.rm = T) +
    beta[, 39] * mean(data.us$imp.se, na.rm = T) 
prob.control <- pnorm(latent.control)

latent.japan <- beta[, 1] +
    beta[, 2] * 1 +
    beta[, 3] * 0 +
    beta[, 4] * 0 +
    beta[, 5] * mean(data.us$age, na.rm = T) +
    beta[, 6] * 1 +
    beta[, 7] * 1 +
    beta[, 15] * 1 +
    beta[, 20] * 1 +
    beta[, 37] * mean(data.us$imp.af, na.rm = T) +
    beta[, 38] * mean(data.us$imp.la, na.rm = T) +
    beta[, 39] * mean(data.us$imp.se, na.rm = T) 
prob.japan <- pnorm(latent.japan)

latent.uk <- beta[, 1] +
    beta[, 2] * 0 +
    beta[, 3] * 1 +
    beta[, 4] * 0 +
    beta[, 5] * mean(data.us$age, na.rm = T) +
    beta[, 6] * 1 +
    beta[, 7] * 1 +
    beta[, 15] * 1 +
    beta[, 20] * 1 +
    beta[, 37] * mean(data.us$imp.af, na.rm = T) +
    beta[, 38] * mean(data.us$imp.la, na.rm = T) +
    beta[, 39] * mean(data.us$imp.se, na.rm = T) 
prob.uk <- pnorm(latent.uk)

latent.china <- beta[, 1] +
    beta[, 2] * 0 +
    beta[, 3] * 0 +
    beta[, 4] * 1 +
    beta[, 5] * mean(data.us$age, na.rm = T) +
    beta[, 6] * 1 +
    beta[, 7] * 1 +
    beta[, 15] * 1 +
    beta[, 20] * 1 +
    beta[, 37] * mean(data.us$imp.af, na.rm = T) +
    beta[, 38] * mean(data.us$imp.la, na.rm = T) +
    beta[, 39] * mean(data.us$imp.se, na.rm = T) 
prob.china <- pnorm(latent.china)

x <- prob.japan - prob.control
effect.japan.pool <- c(quantile(x, prob = 0.025), mean(x), quantile(x, prob = 0.975))

x <- prob.uk - prob.control
effect.uk.pool <- c(quantile(x, prob = 0.025), mean(x), quantile(x, prob = 0.975))

x <- prob.china - prob.control
effect.china.pool <- c(quantile(x, prob = 0.025), mean(x), quantile(x, prob = 0.975))
##############################################
stargazer(MODEL[[1]], MODEL[[2]], MODEL[[3]], MODEL[[4]], se = list(SE[[1]], SE[[2]], SE[[3]], SE[[4]]), star.char = c("*", "**", "***"), star.cutoffs = c(.05, .01, .001), omit = c("age", "gender", "educ", "division", "profile_gross_household"))

stargazer(MODEL[[5]], MODEL[[6]], MODEL[[7]], se = list(SE[[5]], SE[[6]], SE[[7]]), star.char = c("*", "**", "***"), star.cutoffs = c(.05, .01, .001), omit = c("age", "gender", "educ", "division", "profile_gross_household"))
##############################################
pdf("figure_A5.pdf", width = 9, height = 9)

par(mfrow = c(3, 3))

XLAB <- list("Average Treatment Effect", cex = 1.2)
YLAB <- ""
cex.main <- 1.5
MIN <- -0.5
MAX <- 0.5
X.text <- 0.38

MAIN <- list("Aid Reallocation from\nAfrica to Latin America\n(N=1001)", cex = cex.main)
plot(0, 0, xlim = c(MIN, MAX), ylim = c(0.5, 3.5), xlab = XLAB, ylab = YLAB, main = MAIN, yaxt = "n")
lines(x = effect.japan.af.la[c(1, 3)], y = rep(3, 2))
points(x = effect.japan.af.la[2], pch = 20, y = 3)
lines(x = effect.uk.af.la[c(1, 3)], y = rep(2, 2))
points(x = effect.uk.af.la[2], pch = 20, y = 2)
lines(x = effect.china.af.la[c(1, 3)], y = rep(1, 2))
points(x = effect.china.af.la[2], pch = 20, y = 1)
text(x = X.text, y = 3, labels = "Substitution\nby Japan")
text(x = X.text, y = 2, labels = "Substitution\nby UK")
text(x = X.text, y = 1, labels = "Substitution\nby China")
abline(v = 0, lty = 2)



MAIN <- list("Aid Reallocation from\nLatin America to Southeast Asia\n(N=1007)", cex = cex.main)
plot(0, 0, xlim = c(MIN, MAX), ylim = c(0.5, 3.5), xlab = XLAB, ylab = YLAB, main = MAIN, yaxt = "n")
lines(x = effect.japan.la.se[c(1, 3)], y = rep(3, 2))
points(x = effect.japan.la.se[2], pch = 20, y = 3)
lines(x = effect.uk.la.se[c(1, 3)], y = rep(2, 2))
points(x = effect.uk.la.se[2], pch = 20, y = 2)
lines(x = effect.china.la.se[c(1, 3)], y = rep(1, 2))
points(x = effect.china.la.se[2], pch = 20, y = 1)
text(x = X.text, y = 3, labels = "Substitution\nby Japan")
text(x = X.text, y = 2, labels = "Substitution\nby UK")
text(x = X.text, y = 1, labels = "Substitution\nby China")
abline(v = 0, lty = 2)



MAIN <- list("Aid Reallocation from\nSoutheast Asia to Africa\n(N=1030)", cex = cex.main)
plot(0, 0, xlim = c(MIN, MAX), ylim = c(0.5, 3.5), xlab = XLAB, ylab = YLAB, main = MAIN, yaxt = "n")
lines(x = effect.japan.se.af[c(1, 3)], y = rep(3, 2))
points(x = effect.japan.se.af[2], pch = 20, y = 3)
lines(x = effect.uk.se.af[c(1, 3)], y = rep(2, 2))
points(x = effect.uk.se.af[2], pch = 20, y = 2)
lines(x = effect.china.se.af[c(1, 3)], y = rep(1, 2))
points(x = effect.china.se.af[2], pch = 20, y = 1)
text(x = X.text, y = 3, labels = "Substitution\nby Japan")
text(x = X.text, y = 2, labels = "Substitution\nby UK")
text(x = X.text, y = 1, labels = "Substitution\nby China")
abline(v = 0, lty = 2)



MAIN <- list("Aid Reallocation from\nLatin America to Africa\n(N=1028)", cex = cex.main)
plot(0, 0, xlim = c(MIN, MAX), ylim = c(0.5, 3.5), xlab = XLAB, ylab = YLAB, main = MAIN, yaxt = "n")
lines(x = effect.japan.la.af[c(1, 3)], y = rep(3, 2))
points(x = effect.japan.la.af[2], pch = 20, y = 3)
lines(x = effect.uk.la.af[c(1, 3)], y = rep(2, 2))
points(x = effect.uk.la.af[2], pch = 20, y = 2)
lines(x = effect.china.la.af[c(1, 3)], y = rep(1, 2))
points(x = effect.china.la.af[2], pch = 20, y = 1)
text(x = X.text, y = 3, labels = "Substitution\nby Japan")
text(x = X.text, y = 2, labels = "Substitution\nby UK")
text(x = X.text, y = 1, labels = "Substitution\nby China")
abline(v = 0, lty = 2)



MAIN <- list("Aid Reallocation from\nSoutheast Asia to Latin America\n(N=1000)", cex = cex.main)
plot(0, 0, xlim = c(MIN, MAX), ylim = c(0.5, 3.5), xlab = XLAB, ylab = YLAB, main = MAIN, yaxt = "n")
lines(x = effect.japan.se.la[c(1, 3)], y = rep(3, 2))
points(x = effect.japan.se.la[2], pch = 20, y = 3)
lines(x = effect.uk.se.la[c(1, 3)], y = rep(2, 2))
points(x = effect.uk.se.la[2], pch = 20, y = 2)
lines(x = effect.china.se.la[c(1, 3)], y = rep(1, 2))
points(x = effect.china.se.la[2], pch = 20, y = 1)
text(x = X.text, y = 3, labels = "Substitution\nby Japan")
text(x = X.text, y = 2, labels = "Substitution\nby UK")
text(x = X.text, y = 1, labels = "Substitution\nby China")
abline(v = 0, lty = 2)



MAIN <- list("Aid Reallocation from\nAfrica to Southeast Asia\n(N=1029)", cex = cex.main)
plot(0, 0, xlim = c(MIN, MAX), ylim = c(0.5, 3.5), xlab = XLAB, ylab = YLAB, main = MAIN, yaxt = "n")
lines(x = effect.japan.af.se[c(1, 3)], y = rep(3, 2))
points(x = effect.japan.af.se[2], pch = 20, y = 3)
lines(x = effect.uk.af.se[c(1, 3)], y = rep(2, 2))
points(x = effect.uk.af.se[2], pch = 20, y = 2)
lines(x = effect.china.af.se[c(1, 3)], y = rep(1, 2))
points(x = effect.china.af.se[2], pch = 20, y = 1)
text(x = X.text, y = 3, labels = "Substitution\nby Japan")
text(x = X.text, y = 2, labels = "Substitution\nby UK")
text(x = X.text, y = 1, labels = "Substitution\nby China")
abline(v = 0, lty = 2)



plot(0, 0, xlab = "", ylab = "", axes = F, cex = 0)


MAIN <- list("Pooled\n(N=6095)", cex = cex.main)
plot(0, 0, xlim = c(MIN, MAX), ylim = c(0.5, 3.5), xlab = XLAB, ylab = YLAB, main = MAIN, yaxt = "n")
lines(x = effect.japan.pool[c(1, 3)], y = rep(3, 2))
points(x = effect.japan.pool[2], pch = 20, y = 3)
lines(x = effect.uk.pool[c(1, 3)], y = rep(2, 2))
points(x = effect.uk.pool[2], pch = 20, y = 2)
lines(x = effect.china.pool[c(1, 3)], y = rep(1, 2))
points(x = effect.china.pool[2], pch = 20, y = 1)
text(x = X.text, y = 3, labels = "Substitution\nby Japan")
text(x = X.text, y = 2, labels = "Substitution\nby UK")
text(x = X.text, y = 1, labels = "Substitution\nby China")
abline(v = 0, lty = 2)


plot(0, 0, xlab = "", ylab = "", axes = F, cex = 0)

dev.off()
##############################################

