# meta-analytic correlations

library(metafor)
library(grid)

# note that order of studies might be different than in paper

# correlations between measures of subjective SES ####

# correlation between the two different measures of subjective social class DESTINATIONS administered in three studies
ri <- c(.50,.44,.59)
ni <- c(502,1036,565)
dat <- escalc(measure="COR", ri=ri, ni=ni, vtype="LS")
res <- rma(yi, vi, weights=ni, data=dat, method="HS")
res

# correlation between the two different measures of subjective social class ORIGINS administered in three studies
ri <- c(.63,.73,.77)
ni <- c(503,1033,553)
dat <- escalc(measure="COR", ri=ri, ni=ni, vtype="LS")
res <- rma(yi, vi, weights=ni, data=dat, method="HS")
res

# correlations between SES measures and entitlement ####

# correlation between INCOME and entitlement
ri <- c(.13,.06,.11,.08)
ni <- c(500,996,1035,568)
dat <- escalc(measure="COR", ri=ri, ni=ni, vtype="LS")
res <- rma(yi, vi, weights=ni, data=dat, method="HS")
res

# correlation between PARENTAL INCOME and entitlement
ri <- c(.12,.06,.05,.04)
ni <- c(502,988,1006,568)
dat <- escalc(measure="COR", ri=ri, ni=ni, vtype="LS")
res <- rma(yi, vi, weights=ni, data=dat, method="HS")
res

# correlation between EDUCATION and entitlement
ri <- c(.01,.02,.01,.10)
ni <- c(503,997,1036,567)
dat <- escalc(measure="COR", ri=ri, ni=ni, vtype="LS")
res <- rma(yi, vi, weights=ni, data=dat, method="HS")
res

# correlation between PARENTAL EDUCATION and entitlement
ri <- c(.04,.05,.03,.04)
ni <- c(500,977,1004,559)
dat <- escalc(measure="COR", ri=ri, ni=ni, vtype="LS")
res <- rma(yi, vi, weights=ni, data=dat, method="HS")
res

# correlation between SUBJECTIVE SES DESTINATIONS and entitlement
ri <- c(.09,.04,.11,.16)
ni <- c(503,998,1036,568)
dat <- escalc(measure="COR", ri=ri, ni=ni, vtype="LS")
res <- rma(yi, vi, weights=ni, data=dat, method="HS")
res

# correlation between SUBJECTIVE SES ORIGINS and entitlement
ri <- c(.24,.04,.08,.03)
ni <- c(503,998,1036,568)
dat <- escalc(measure="COR", ri=ri, ni=ni, vtype="LS")
res <- rma(yi, vi, weights=ni, data=dat, method="HS")
res

# correlations between different indicators of SES ####

# correlation between current income and parental income
ri <- c(.12,.15,.27,.16)
ni <- c(499,986,1005,568)
dat <- escalc(measure="COR", ri=ri, ni=ni, vtype="LS")
res <- rma(yi, vi, weights=ni, data=dat, method="HS")
res

# correlation between education and parents education
ri <- c(.46,.31,.28,.18)
ni <- c(500,976,1004,558)
dat <- escalc(measure="COR", ri=ri, ni=ni, vtype="LS")
res <- rma(yi, vi, weights=ni, data=dat, method="HS")
res

# correlation between father education and mother education
ri <- c(.45,.62,.58,.57)
ni <- c(481,952,996,545)
dat <- escalc(measure="COR", ri=ri, ni=ni, vtype="LS")
res <- rma(yi, vi, weights=ni, data=dat, method="HS")
res

# correlation between subjective class destinations and subjective class origins
ri <- c(.33,.35,.36,.30)
ni <- c(503,998,1036,568)
dat <- escalc(measure="COR", ri=ri, ni=ni, vtype="LS")
res <- rma(yi, vi, weights=ni, data=dat, method="HS")
res

# correlation between subjective class destinations and current income
ri <- c(.41,.54,.37,.32)
ni <- c(500,569,1038,1000)
dat <- escalc(measure="COR", ri=ri, ni=ni, vtype="LS")
res <- rma(yi, vi, weights=ni, data=dat, method="HS")
res

# correlation between subjective class destinations and parental income
ri <- c(.19,.13,.27,.18)
ni <- c(501,569,1006,989)
dat <- escalc(measure="COR", ri=ri, ni=ni, vtype="LS")
res <- rma(yi, vi, weights=ni, data=dat, method="HS")
res

# correlation between subjective class destinations and education
ri <- c(.27,.29,.21,.28)
ni <- c(503,568,1039,1001)
dat <- escalc(measure="COR", ri=ri, ni=ni, vtype="LS")
res <- rma(yi, vi, weights=ni, data=dat, method="HS")
res

# correlation between subjective class destinations and parental education
ri <- c(.21,.11,.19,.14)
ni <- c(500,559,1004,978)
dat <- escalc(measure="COR", ri=ri, ni=ni, vtype="LS")
res <- rma(yi, vi, weights=ni, data=dat, method="HS")
res

# correlation between subjective class origins and current income
ri <- c(.21,.10,.13,.08)
ni <- c(500,569,1038,1000)
dat <- escalc(measure="COR", ri=ri, ni=ni, vtype="LS")
res <- rma(yi, vi, weights=ni, data=dat, method="HS")
res

# correlation between subjective class origins and parental income
ri <- c(.45,.50,.59,.52)
ni <- c(502,569,1006,989)
dat <- escalc(measure="COR", ri=ri, ni=ni, vtype="LS")
res <- rma(yi, vi, weights=ni, data=dat, method="HS")
res

# correlation between subjective class origins and education
ri <- c(.15,.07,.17,.20)
ni <- c(503,568,1039,1001)
dat <- escalc(measure="COR", ri=ri, ni=ni, vtype="LS")
res <- rma(yi, vi, weights=ni, data=dat, method="HS")
res

# correlation between subjective class origins and parental education
ri <- c(.35,.42,.42,.44)
ni <- c(501,559,1004,978)
dat <- escalc(measure="COR", ri=ri, ni=ni, vtype="LS")
res <- rma(yi, vi, weights=ni, data=dat, method="HS")
res

# correlation between current income and education
ri <- c(.35,.35,.26,.30)
ni <- c(500,568,1038,999)
dat <- escalc(measure="COR", ri=ri, ni=ni, vtype="LS")
res <- rma(yi, vi, weights=ni, data=dat, method="HS")
res

# correlation between current income and parents education
ri <- c(.20,.10,.19,.05)
ni <- c(497,559,1003,976)
dat <- escalc(measure="COR", ri=ri, ni=ni, vtype="LS")
res <- rma(yi, vi, weights=ni, data=dat, method="HS")
res

# correlation between parental income and education
ri <- c(.14,.10,.18,.18)
ni <- c(501,568,1006,988)
dat <- escalc(measure="COR", ri=ri, ni=ni, vtype="LS")
res <- rma(yi, vi, weights=ni, data=dat, method="HS")
res

# correlation between parental income and parents education
ri <- c(.32,.32,.44,.48)
ni <- c(499,559,1004,977)
dat <- escalc(measure="COR", ri=ri, ni=ni, vtype="LS")
res <- rma(yi, vi, weights=ni, data=dat, method="HS")
res