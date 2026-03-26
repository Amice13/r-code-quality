#### use this generates table 1: probit regression coefficients.

rm(list=ls())
library(haven)
library(mfx)
library(dplyr)
library(stargazer)
library(arm)

setwd("/Users/Mandywu/Dropbox (MIT)/Replication Paper/May 2 Submission/data&code")

### model1: bayesglm coefficients
dat <- read_dta(file="dece.dta")
dat$govdummy1 <- as.factor(dat$govdummy)
dat$yeardummy <- as.factor(dat$year)
dat$ind2dummy <- as.factor(dat$ind2)

### calculate probit model coefficients
model1 <- bayesglm (formula = fdece ~lndis+lnasset+ROS+importance+Dfsoe+prov_gdpper+
                     prov_SOE+unemployment+govdummy1+yeardummy+ind2dummy, family=binomial(link="probit"),data=dat)

### model2: glm coefficients
model2 <- glm (formula = fdece ~lndis+lnasset+ROS+importance+Dfsoe+prov_gdpper+
                      prov_SOE+unemployment+govdummy1+yeardummy+ind2dummy, family=binomial(link="probit"),data=dat)


### model 3: glm coefficients using STATA obeservations
### dece_stata1.dta is produced in STATA, by generating a variable called "used" 
### used - suggesting whether this variable is used for glm regression
### the code to generate the variable in STATA is: gen byte used=e(sample)
stata <- read_dta(file="dece_stata1.dta")
stata <- stata[stata$used==1,]
stata <- stata[,1:52]

stata$govdummy1 <- as.factor(stata$govdummy)
stata$yeardummy <- as.factor(stata$year)
stata$ind2dummy <- as.factor(stata$ind2)

model3 <- glm (formula = fdece ~lndis+lnasset+ROS+importance+Dfsoe+prov_gdpper+
                prov_SOE+unemployment+govdummy1+yeardummy+ind2dummy, family=binomial(link="probit"),data=stata)

### the fist coefficient is constant.
print(model1$coefficients[2:9])
print(model2$coefficients[2:9])
print(model3$coefficients[2:9])


library(texreg)
### produce coefficient output
### the first column, model 2 is used to reserve a column for the STATA output. 
### we manually fixed the coefficients value for STATA output in paper.
texreg(list(model2,model1,model2,model3),omit.coef="(ind2dummy|govdummy|yeardummy|Iyear|Igovdum|Iind|(Intercept))",
       custom.coef.names = c("Distance_lag", "log(firm asset)_lag","ROS_lag", "Firm importance_lag",
                             "Fully state-owned_lag","GDP per capita_lag","State sector share_lag","unemployment rate_lag"),
       custom.model.names = c("(1)","(2)","(3)","(4)"),caption = "Regression Coefficents",caption.above = FALSE,
       label = "table:coefficients",stars = numeric(0),digits=4)


