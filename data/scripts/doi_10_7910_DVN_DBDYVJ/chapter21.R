rm(list = ls())


## Chapter 21.5
## logistic regressions using the Karolinska dataset
karolinska = read.table("karolinska.txt", header = TRUE)
karolinska = karolinska[, c("highdiag", "hightreat",
                            "age", "rural", 
                            "male", "survival")]

## logistic on the treatment
diagglm = glm(highdiag ~ age + rural + male, 
              data = karolinska, 
              family = binomial(link = "logit"))
summary(diagglm)

treatglm = glm(hightreat ~ age + rural + male, 
              data = karolinska, 
              family = binomial(link = "logit"))
summary(treatglm)

## outcome: longer than 1 year
karolinska$loneyear = (karolinska$survival != "1")
loneyearglm = glm(loneyear ~ highdiag + age + rural + male,
                 data = karolinska, 
                 family = binomial(link = "logit"))
summary(loneyearglm)


loneyearglm = glm(loneyear ~ hightreat + age + rural + male, 
                  data = karolinska, 
                  family = binomial(link = "logit"))
summary(loneyearglm)


## multinomial regression on outcome with 3 categories
library(nnet)
yearmultinom = multinom(survival ~ highdiag + age + rural + male,
                        data = karolinska)
summary(yearmultinom)
predict(yearmultinom, type = "probs")[1:5, ]

yearmultinom = multinom(survival ~ hightreat + age + rural + male,
                        data = karolinska)
summary(yearmultinom)
predict(yearmultinom, type = "probs")[1:5, ]



## proportional odds regression on outcome with orders
library(MASS)
yearpo = polr(factor(survival) ~ highdiag + age + rural + male, 
              Hess = TRUE, 
              data = karolinska)
summary(yearpo)
predict(yearpo, type = "probs")[1:5, ]

yearpo = polr(factor(survival) ~ hightreat + age + rural + male, 
              Hess = TRUE, 
              data = karolinska)
summary(yearpo)
predict(yearpo, type = "probs")[1:5, ]




## Chapter 21.6
## The following code is from the package "mlogit"
library("nnet")
library("mlogit")
data("Fishing")
head(Fishing)

Fish  = dfidx(Fishing, 
              varying = 2:9, 
              shape = "wide", 
              choice = "mode")
head(Fish)

## choice-specific only
summary(mlogit(mode ~ 0 + price + catch, data = Fish))

summary(mlogit(mode ~ price + catch, data = Fish))

## individual-specific only
summary(mlogit(mode ~ 0 | income, data = Fish))

## equivalently
summary(multinom(mode ~ income, data = Fishing))

## general model
summary(mlogit(mode ~ price + catch | income, data = Fish))





