###
#
# Tyler Girard, "Bank Accounts for All: How Do State Policies Matter?" Journal of International Development
#
# This script will generate the results reported in Table 2 (Total Adult Population, Without Outliers)
#
# Due to commercial restrictions on the Banks and Wilson CNTS Data Archive, the replication data includes
# the 50 mutliply imputed datasets but excludes the CNTS variables used to perform the multiple imputation,
# as well as the variable "pct_literate" used in model 4
#
# Contact Info: tgirard2@uwo.ca
#
# March 27, 2020
#
###

#### Set Working Directory ####

#setwd("")

#### Load Packages ####

library(mice)
library(mitools)

#### Prepare Aux Function ####

# To log transform a variable that includes negative values
IHS <- function(x){(log(x + sqrt(x^2 + 1)))}

# To generate p-values from models estimated with multiply imputed data
pvals <- function(obj, two.sided=T){2^two.sided*pt(abs(obj$coefficients)/sqrt(diag(obj$variance)), obj$df, lower.tail=F)}

#### Load Data ####

load("Main2_TotalPop_PartialCountries_MI.RData")

#### Estimate Models ####

# m4 removed due to data restriction
m1 <- m2 <- m3 <- m5 <- m6 <- m7 <- m8 <- m9 <- list()
for(i in 1:length(dats)){
  m1[[i]] <- lm(acct ~ log(gdp) + poly(popden,2), data=dats[[i]])
  m2[[i]] <- lm(acct ~ log(gdp) + poly(popden,2) + log(newscore), data=dats[[i]])
  m3[[i]] <- lm(acct ~ log(gdp) + poly(popden,2) + log(newscore)
                + IHS(inflation) + employ, data=dats[[i]])
  #m4[[i]] <- lm(acct ~ log(gdp) + poly(popden,2) + log(newscore)
  #              + IHS(inflation) + employ + pct_literate, data=dats[[i]])
  m5[[i]] <- lm(acct ~ log(gdp) + poly(popden,2) + log(newscore)
                + IHS(inflation) + employ + pc1, data=dats[[i]])
  m6[[i]] <- lm(acct ~ log(gdp) + poly(popden,2) + log(newscore)
                + IHS(inflation) + employ + pc2, data=dats[[i]])
  m7[[i]] <- lm(acct ~ log(gdp) + poly(popden,2) + log(newscore)
                + IHS(inflation) + employ + ed1, data=dats[[i]])
  m8[[i]] <- lm(acct ~ log(gdp) + poly(popden,2) + log(newscore)
                + IHS(inflation) + employ + ed2, data=dats[[i]])
  m9[[i]] <- lm(acct ~ log(gdp) + poly(popden,2) + log(newscore)
                + IHS(inflation) + employ + ed3, data=dats[[i]])

}

#-------------------------------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------------------------------#

#### Extracting Results From Each Model ####


#### m1 <- lm(acct ~ log(gdp) + poly(popden,2)) ####

tm1 <- MIcombine(m1)

# results
res1 <- summary(tm1)
# two-sided p-values
p1 <- round(pvals(tm1), 5)
# m1 table
tab1 <- as.data.frame(cbind(res1$results, res1$se, p1))
names(tab1) <- c("b","se","pval")

#### m2 <- lm(acct ~ log(gdp) + poly(popden,2) + log(newscore)) ####

tm2 <- MIcombine(m2)

# results
res2 <- summary(tm2)
# two-sided p-values
p2 <- round(pvals(tm2), 5)
# m2 table
tab2 <- as.data.frame(cbind(res2$results, res2$se, p2))
names(tab2) <- c("b","se","pval")


#### m3 <- lm(acct ~ log(gdp) + poly(popden,2) + log(newscore) + IHS(inflation) + employ) ####

tm3 <- MIcombine(m3)

# results
res3 <- summary(tm3)
# two-sided p-values
p3 <- round(pvals(tm3), 5)
# m3 table
tab3 <- as.data.frame(cbind(res3$results, res3$se, p3))
names(tab3) <- c("b","se","pval")


#### m4 <- lm(acct ~ log(gdp) + poly(popden,2) + log(newscore) + IHS(inflation) + employ + pct_literate) ####

#tm4 <- MIcombine(m4)

# results
#res4 <- summary(tm4)
# two-sided p-values
#p4 <- round(pvals(tm4), 5)
# m4 table
#tab4 <- as.data.frame(cbind(res4$results, res4$se, p4))
#names(tab4) <- c("b","se","pval")


#### m5 <- lm(acct ~ log(gdp) + poly(popden,2) + log(newscore) + IHS(inflation) + employ + pc1) ####

tm5 <- MIcombine(m5)

# results
res5 <- summary(tm5)
# two-sided p-values
p5 <- round(pvals(tm5), 5)
# m5 table
tab5 <- as.data.frame(cbind(res5$results, res5$se, p5))
names(tab5) <- c("b","se","pval")

#### m6 <- lm(acct ~ log(gdp) + poly(popden,2) + log(newscore) + IHS(inflation) + employ + pc2) ####

tm6 <- MIcombine(m6)

# results
res6 <- summary(tm6)
# two-sided p-values
p6 <- round(pvals(tm6), 5)
# m6 table
tab6 <- as.data.frame(cbind(res6$results, res6$se, p6))
names(tab6) <- c("b","se","pval")

#### m7 <- lm(acct ~ log(gdp) + poly(popden,2) + log(newscore) + IHS(inflation) + employ + ed1) ####

tm7 <- MIcombine(m7)

# results
res7 <- summary(tm7)
# two-sided p-values
p7 <- round(pvals(tm7), 5)
# m7 table
tab7 <- as.data.frame(cbind(res7$results, res7$se, p7))
names(tab7) <- c("b","se","pval")

#### m8 <- lm(acct ~ log(gdp) + poly(popden,2) + log(newscore) + IHS(inflation) + employ + ed2) ####

tm8 <- MIcombine(m8)

# results
res8 <- summary(tm8)
# two-sided p-values
p8 <- round(pvals(tm8), 5)
# m8 table
tab8 <- as.data.frame(cbind(res8$results, res8$se, p8))
names(tab8) <- c("b","se","pval")

#### m9 <- lm(acct ~ log(gdp) + poly(popden,2) + log(newscore) + IHS(inflation) + employ + ed3) ####

tm9 <- MIcombine(m9)

# results
res9 <- summary(tm9)
# two-sided p-values
p9 <- round(pvals(tm9), 5)
# m9 table
tab9 <- as.data.frame(cbind(res9$results, res9$se, p9))
names(tab9) <- c("b","se","pval")

#-------------------------------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------------------------------#

#### Calculating R2 and Adjusted R2 ####

fit1 <- as.mira(m1)
pool.r.squared(fit1)
pool.r.squared(fit1, adjusted = TRUE)

fit2 <- as.mira(m2)
pool.r.squared(fit2)
pool.r.squared(fit2, adjusted = TRUE)

fit3 <- as.mira(m3)
pool.r.squared(fit3)
pool.r.squared(fit3, adjusted = TRUE)

#fit4 <- as.mira(m4)
#pool.r.squared(fit4)
#pool.r.squared(fit4, adjusted = TRUE)

fit5 <- as.mira(m5)
pool.r.squared(fit5)
pool.r.squared(fit5, adjusted = TRUE)

fit6 <- as.mira(m6)
pool.r.squared(fit6)
pool.r.squared(fit6, adjusted = TRUE)

fit7 <- as.mira(m7)
pool.r.squared(fit7)
pool.r.squared(fit7, adjusted = TRUE)

fit8 <- as.mira(m8)
pool.r.squared(fit8)
pool.r.squared(fit8, adjusted = TRUE)

fit9 <- as.mira(m9)
pool.r.squared(fit9)
pool.r.squared(fit9, adjusted = TRUE)


#-------------------------------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------------------------------#
#                                                                     END OF SCRIPT                                                                     #
#-------------------------------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------------------------------#
