## Code, executed in R 3.5.1 for Windows (64-bit)
## Attach relevant packages: mgcv (version 1.8-24) and ggplot2 (version 3.0.0)
require("mgcv")
require("ggplot2")

## Input data
a <- read.csv("socialpatriotism.csv", stringsAsFactors = FALSE)

## Generate day-of-year counter for cyclical spline
a$day <- (a$birth - 11)%%365

## Reformat data for plots of term use against time
x <- aggregate(nytimes ~ birth, data = a, mean)
y <- aggregate(terror ~ birth, data = a, mean)


## Figure 1
qplot(data = x, xlab = "", ylab = "Anti-French sentiment salience", as.Date(birth, origin = "1960-01-01"), nytimes, geom = "smooth", span = 1/3, se = FALSE) + theme_minimal() + coord_cartesian(ylim = c(0, 17))


## Table 1
## Column 1 models (first name)
m <- gamm(data = a, french ~ s(day, bs = "cc", k = 5) + I(nytimes/100) + I(birth/365) + state, family = "binomial")
summary(m$gam)

## Column 2 models (middle name)
mmid <- gamm(data = a, midfrench ~ s(day, bs = "cc", k = 5) + I(nytimes/100) + I(birth/365) + state, family = "binomial")
summary(mmid$gam)


## Figure 2
## Generate values for x-axis (range of relevant dates)
nytimes <- min(a$nytimes, na.rm=TRUE):max(a$nytimes, na.rm=TRUE)

## Create prediction data with fixed control variables for each relevant date
sample.data <- data.frame(nytimes, day = rep(0, length(nytimes)), birth = rep(mean(a$birth, na.rm = TRUE), length(nytimes)), state = rep("A", length(nytimes)))

## Compute Table 1, Column 1 predictions (.est) with confidence-interval lower (.l95) and upper (.u95) bounds
preds <- predict(m$gam, newdata = sample.data, se.fit = TRUE)
preds.est <- exp(preds$fit)/(1 + exp(preds$fit))
preds.u95 <- exp(preds$fit + 1.96*preds$se.fit)/(1 + exp(preds$fit + 1.96*preds$se.fit))
preds.l95 <- exp(preds$fit - 1.96*preds$se.fit)/(1 + exp(preds$fit - 1.96*preds$se.fit))

## Compute Table 1, Column 2 predictions (.est) with confidence-interval lower (.l95) and upper (.u95) bounds
predsmid <- predict(mmid$gam, newdata = sample.data, se.fit = TRUE)
predsmid.est <- exp(predsmid$fit)/(1 + exp(predsmid$fit))
predsmid.u95 <- exp(predsmid$fit + 1.96*predsmid$se.fit)/(1 + exp(predsmid$fit + 1.96*predsmid$se.fit))
predsmid.l95 <- exp(predsmid$fit - 1.96*predsmid$se.fit)/(1 + exp(predsmid$fit - 1.96*predsmid$se.fit))

## Put calculated variables into data frame for graphing
b <- data.frame(x = rep(1:length(nytimes), 2), est = c(preds.est, predsmid.est), u95 = c(preds.u95, predsmid.u95), l95 = c(preds.l95, predsmid.l95), name = c(rep("First", length(nytimes)), rep("Middle", length(nytimes))))

## Graph data
qplot(data = b, x, ymin = l95, ymax = u95, geom = "ribbon", xlab = "Anti-French salience", ylab = "Predicted probability of French name", fill = name, alpha = I(0.2)) + geom_line(aes(x = b$x, y = b$est, linetype = b$name)) + facet_wrap(facets = ~ name, scales = "free") + scale_y_continuous(breaks = (1:26)/200) + theme_minimal() + theme(legend.position = "none")


## Table 2
## Generate multinomial categorical variable based on French quantity of first and middle names
a$multifrench <- a$french*(1-a$midfrench) + 2*(1-a$french)*a$midfrench + 3*(a$french)*(a$midfrench)

## Create indicator variables for Nevada (N) and Vermont (V) states
a$stateN <- a$state == "N"
a$stateV <- a$state == "V"

## Compute multinomial model
t2 <- gam(data = a, list(multifrench~s(day, bs = "cc", k = 5) + I(nytimes/100) + I(birth/365) + stateN + stateV, ~s(day, bs = "cc", k = 5) + I(nytimes/100) + I(birth/365) + stateN + stateV, ~s(day, bs = "cc", k = 5) + I(nytimes/100) + I(birth/365) + stateN + stateV), family = multinom(K = 3))
summary(t2)

## Generate predicted values from Table 2
predict(t2, data.frame(day = 0, nytimes = 10, birth = 0, stateN = FALSE, stateV = FALSE), type = "response")
predict(t2, data.frame(day = 0, nytimes = 29, birth = 0, stateN = FALSE, stateV = FALSE), type = "response")


## Figure 3
qplot(data = y, xlab = "", ylab = "Terrorism salience", as.Date(birth, origin = "1960-01-01"), terror, geom = "smooth", span = 1/3, se = FALSE) + theme_minimal()


## Table 3
## Column 1 model (first name, patriotic meaning)
n <- gamm(data = a, rally ~ s(day, bs = "cc", k = 5) + I(terror/100) + I(birth/365) + state, family = "binomial")
summary(n$gam)

## Column 2 model (middle name, patriotic meaning)
nmid <- gamm(data = a, midrally ~ s(day, bs = "cc", k = 5) + I(terror/100) + I(birth/365) + state, family = "binomial")
summary(nmid$gam)

## Column 3 model (first name, Southwest Asian origin)
nswas <- gamm(data = a, firstswas ~ s(day, bs = "cc", k = 5) + I(terror/100) + I(birth/365) + state, family = "binomial")
summary(nswas$gam)

## Column 4 model (middle name, Southwest Asian origin)
nswasmid <- gamm(data = a, midswas ~ s(day, bs = "cc", k = 5) + I(terror/100) + I(birth/365) + state, family = "binomial")
summary(nswasmid$gam)


## Figure 4
## Generate values for x-axis (range of relevant dates)
terror <- min(a$terror, na.rm = TRUE):max(a$terror, na.rm = TRUE)

## Create prediction data with fixed control variables for each relevant date
sample.terr <- data.frame(terror, day = rep(0, length(terror)), birth = rep(mean(a$birth, na.rm = TRUE), length(terror)), state = rep("A", length(terror)))

## Compute Table 3, Column 1 predictions (.est) with confidence-interval lower (.l95) and upper (.u95) bounds
predt <- predict(n$gam, newdata = sample.terr, se.fit = TRUE)
predt.est <- exp(predt$fit)/(1 + exp(predt$fit))
predt.u95 <- exp(predt$fit + 1.96*predt$se.fit)/(1 + exp(predt$fit + 1.96*predt$se.fit))
predt.l95 <- exp(predt$fit - 1.96*predt$se.fit)/(1 + exp(predt$fit - 1.96*predt$se.fit))

## Compute Table 3, Column 2 predictions (.est) with confidence-interval lower (.l95) and upper (.u95) bounds
predtmid <- predict(nmid$gam, newdata = sample.terr, se.fit = TRUE)
predtmid.est <- exp(predtmid$fit)/(1 + exp(predtmid$fit))
predtmid.u95 <- exp(predtmid$fit + 1.96*predtmid$se.fit)/(1 + exp(predtmid$fit + 1.96*predtmid$se.fit))
predtmid.l95 <- exp(predtmid$fit - 1.96*predtmid$se.fit)/(1 + exp(predtmid$fit - 1.96*predtmid$se.fit))

## Compute Table 3, Column 3 predictions (.est) with confidence-interval lower (.l95) and upper (.u95) bounds
predswas <- predict(nswas$gam, newdata = sample.terr, se.fit = TRUE)
predswas.est <- exp(predswas$fit)/(1 + exp(predswas$fit))
predswas.u95 <- exp(predswas$fit + 1.96*predswas$se.fit)/(1 + exp(predswas$fit + 1.96*predswas$se.fit))
predswas.l95 <- exp(predswas$fit - 1.96*predswas$se.fit)/(1 + exp(predswas$fit - 1.96*predswas$se.fit))

## Compute Table 3, Column 4 predictions (.est) with confidence-interval lower (.l95) and upper (.u95) bounds
predswasmid <- predict(nswasmid$gam, newdata = sample.terr, se.fit = TRUE)
predswasmid.est <- exp(predswasmid$fit)/(1 + exp(predswasmid$fit))
predswasmid.u95 <- exp(predswasmid$fit + 1.96*predswasmid$se.fit)/(1 + exp(predswasmid$fit + 1.96*predswasmid$se.fit))
predswasmid.l95 <- exp(predswasmid$fit - 1.96*predswasmid$se.fit)/(1 + exp(predswasmid$fit - 1.96*predswasmid$se.fit))

## Put calculated variables into data frame for graphing
bt <- data.frame(x = rep(1:length(terror), 4), est = c(predt.est, predtmid.est, predswas.est, predswasmid.est), u95 = c(predt.u95, predtmid.u95, predswas.u95, predswasmid.u95), l95 = c(predt.l95, predtmid.l95, predswas.l95, predswasmid.l95), name = rep(c(rep("First", length(terror)), rep("Middle", length(terror))), 2), yvar = c(rep(" Patriotic", length(terror)*2), rep("Islamic-Seeming", length(terror)*2)))

## Graph data
qplot(data = bt, x, ymin = l95, ymax = u95, geom = "ribbon", xlab = "Terrorism salience", ylab = "Predicted probability of name", fill = yvar, alpha = I(0.2)) + geom_line(aes(x = bt$x, y = bt$est, linetype = bt$name)) + facet_grid(facets = yvar ~ name, scales = "free_y") + scale_y_continuous() + theme_minimal() + theme(legend.position = "none")