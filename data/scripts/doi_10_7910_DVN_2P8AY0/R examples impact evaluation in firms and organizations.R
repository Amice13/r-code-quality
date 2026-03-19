########## R EXAMPLES ##############
# NOTE: The code was executed using R version 4.3.1

# 1
install.packages(c("causalweight","lmtest","sandwich")) # install packages
library(causalweight)                                # load causalweight package 
library(lmtest)                                      # load lmtest package
library(sandwich)                                    # load sandwich package
data(coffeeleaflet)                                  # load coffeeleaflet data
results=lm(awarewaste~treatment, data=coffeeleaflet) # linear regression
coeftest(results, vcov=vcovHC)                       # show impact

# 2
install.packages(c("datarium","np"))                 # install packages
library(datarium)                                    # load datarium package
library(np)                                          # load np package
data(marketing)                                      # load marketing data
results=npregbw(sales~newspaper, data=marketing)     # kernel regression
plot(results, plot.errors.method="asymptotic")       # plot average sales
plot(results, gradients=TRUE, plot.errors.method="asymptotic") # plot impact

# 3
library(causalweight)                                # load causalweight package 
library(lmtest)                                      # load lmtest package
library(sandwich)                                    # load sandwich package
data(coupon)                                         # load coupon data
results=lm(formula=dailyspending~.,data=coupon)      # run linear regression 
coeftest(results, vcov=vcovHC)                       # show impact

# 4
install.packages("Matching")                         # install package
library(Matching)                                    # load Matching package
y=coupon[,1]; d=coupon[,2]; x=as.matrix(coupon[,3:9])# define variables
results=Match(Y=y, Tr=d,X=x, estimand="ATE")         # pair matching
summary(results)                                     # show impact

# 5
results=treatweight(y=y, d=d, x=x)                   # run IPW 
results$effect; results$pval                         # show impact and p-value

# 6
install.packages(c("drgee"))                         # install packages
library(drgee)                                       # load drgee package
results=drgee(oformula=formula(y~x), eformula=formula(d~x), elink="logit") # DR
summary(results)                                     # show impact

# 7
library(causalweight)                                # load causalweight package 
data(coupon)                                         # load coupon data
y=coupon[,1]; d=coupon[,2]; x=as.matrix(coupon[,3:9])# define variables
results=treatDML(y=y, d=d, x=x)                      # run DML with lasso
results$effect; results$pval;                        # show impact and p-value

# 8
install.packages("grf")                              # install package
library(grf)                                         # load grf package 
results=causal_forest(X=x, Y=y, W=d)                 # run causal forest
hist(results$predictions)                            # distribution of CATEs

# 9
best_linear_projection(forest=results,A=x[,2])       # CATEs by past spending

# 10
heterogeneity=regression_forest(X=x, Y=results$predictions) #CATE predicted by x
variable_importance(heterogeneity)                   # predictive importance x

# 11
install.packages("policytree", "DiagrammeR")         # install packages
library(policytree)                                  # load policytree package 
library(DiagrammeR)                                  # load DiagrammeR package 
forest=multi_arm_causal_forest(X=x, Y=y, W=factor(d)) # treatment+outcome models
dr=double_robust_scores(forest)                      # DR functions
tree=policy_tree(X=x, Gamma=dr, depth=2)             # policies for 4 subgroups
plot(tree, leaf.labels=levels(factor(d)))            # tree with optimal policy

# 12
install.packages("AER", "LARF")                      # install packages
library(AER)                                         # load AER package 
library(LARF)                                        # load LARF package
data(c401k)                                          # load 401(k) pension data
results=ivreg(c401k[,2]~c401k[,3]|c401k[,4])         # run two stage regression
summary(results,vcov = vcovHC)                       # show impact

# 13
library(grf)                                         # load grf package
results=instrumental_forest(X=c401k[,5:11],Y=c401k[,2],W=c401k[,3],Z=c401k[,4])     
average_treatment_effect(results)                    # LATE and standard error

# 14
install.packages("rdrobust", "rddtools")             # install packages
library(rdrobust)                                    # load rdrobust library
library(rddtools)                                    # load rdrobust library
data(indh)                                           # load indh data 
results=rdrobust(y=indh[,1], x=indh[,2], c=30)       # run RDD (threshold: 30)
summary(results)                                     # show impact
rdplot(y=indh[,1], x=indh[,2], c=30)                 # plot outcome

# 15
install.packages("DRDID")                            # install package
library(DRDID)                                       # load wooldridge package
data(nsw_long)                                       # load nsw_long data
dat=na.omit(nsw_long)                                # drop missing values
drdid(yname="re",tname="year",idname="id",dname="treated",data=dat) #DiD

# 16
drdid(yname="re", tname="year", idname="id", dname="treated", data=dat,
    xformla=~educ+nodegree+age+married )             # DiD with x

# 17
install.packages("devtools")                         # install devtools package
library(devtools)                                    # load devtools package
install_github("synth-inference/synthdid")           # install synthdid package
library(synthdid)                                    # load synthdid package
data(california_prop99)                              # load smoking data
dat=panel.matrices(california_prop99)                # prepare data
results=synthdid_estimate(Y=dat$Y, N0=dat$N0, T0=dat$T0) # synthetic DiD
results                                              # show impact
sqrt(vcov(results, method='placebo'))                # show standard error 
plot(results)                                        # plot impact

