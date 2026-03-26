## authors:   Sebastian Sewerin, Lukas P. Fesenfeld, Blair Bateson, Tobias Schmidt,
## contact:   lukas.fesenfeld@ir.gess.ethz.ch
## file name: stickiness_paper_code
## Context:   Event History Analysis R Code for Paper "The Role of Policy Design in Explaining Policy Continuity and Supersession"
## Date:      2021-26-01

# Set Working Directory and Load Packages ---------------------------------------------------

setwd("//gess-fs.d.ethz.ch/home$/felukas/Documents/ETH/Stickiness Paper/Data Analysis/Final")
setwd("C:/Users/lf20q095/Documents/Documents/ETH/Stickiness Paper/Data Analysis/Final")

getwd()
#data2=read.csv("new_Data.csv", header=TRUE, sep=";")
#data2=read.csv("New_Dataset_Event_History_OM.csv", header=TRUE, sep=";")
data2=read.csv("Kopie von New_Dataset_Event_History_id_vars_FZ_v5.csv", header=TRUE, sep=";")

table(data2$id)

install.packages(c("survival", "survminer"))
install.packages("coxme")
install.packages("simPH")
install.packages("ggfortify")

library(simPH)
library("survival")
library("survminer")
library("tidyverse")
library("coxme")
library("ggfortify")

normFunc <- function(x){(x-mean(x, na.rm = T))/sd(x, na.rm = T)}


#Prepare Datasets---------
#Task: Select all variables used in this section + country variable and save new dataframe

#Create a new variable that indicates the time of survival of a policy until that policy was superseded or discontinued by subtracting the new end year variable from start year
data2$time = data2$End.Year.Num-data2$Start.Year.Num

#Create a tstart and tstop variable that indicates the beginning and end year of each policy per observed yearly period
data2$tstart_new =data2$Duration
data2$tstop_new =data2$Duration+1

#Relabel Variable Superseded.Numeric indicating that any type of event (supersession or discontinuation) took place in interveral

data2$Event.Numeric = data2$Superseded.Numeric

#Create new event variable indicating policy was superseded: Recode Policy.Superseded. to deal with missings, i.e. those that have no information and values on IPA/Tech Spe. for the superseeding policies are coded as 0
data2$supersession = ifelse(data2$Event.Numeric ==1 &is.na (data2$Diff.IPA)==FALSE &data2$End.Year.Num <=2014, 1,0)

data2$marker = ifelse(data2$Event.Numeric==1 & data2$Policy.Superseded.==1 & is.na (data2$Diff.IPA)==TRUE&data2$End.Year.Num <=2014, 1,0)

#Create new event variable indicating policy was superseded by a policy with a higher IPA score than the superseded policy
data2$supersession_IPA_high = ifelse(data2$supersession==1 & data2$Diff.IPA>0, 1,0)

#Create new event variable indicating policy was superseded by a policy with a lower IPA score than the superseded policy
data2$supersession_IPA_low = ifelse(data2$supersession==1 & data2$Diff.IPA<0, 1,0)

#Create new event variable indicating policy was superseded by a policy with a lower or equal IPA score than the superseded policy
data2$supersession_IPA_low_eq = ifelse(data2$supersession==1 & data2$Diff.IPA<=0, 1,0)

#Create new event variable indicating policy was discontinued but not superseded 
#for discontinuation please note: policies that were superseded but that have no information and values on IPA/Tech Spe. for the superseeding policies are coded as discontinued
data2$discontinuation = ifelse(data2$Event.Numeric ==1 &is.na (data2$Diff.IPA)==TRUE &data2$End.Year.Num <=2014, 1,0)
#for discontinuation2 please note: policies that were superseded but that have no information and values on IPA/Tech Spe. for the superseeding policies are not coded as discontinued
data2$discontinuation2 = ifelse(data2$Event.Numeric ==1 & data2$Policy.Superseded. !=1 &data2$End.Year.Num <=2014, 1,0)

#Create new variable indicating that a policy was superseded or discontinued within the period (1) or right-censonsered/continued (0) 
#Note: accounting for missings, i.e. those that have no information and values on IPA/Tech Spe. for the superseeding policies are coded as 1 (i.e. treated as if policy is changed)
data2$status = ifelse(data2$Event.Numeric ==1 & data2$End.Year.Num <=2014, 1,0)
#Note: accounting for missings, i.e. those that have no information and values on IPA/Tech Spe. for the superseeding policies are coded as 0 (i.e. treated as if policy not changed)
data2$status2 = ifelse(data2$supersession ==1 | data2$discontinuation2 ==1, 1,0)

#Recode Integration variable into binary variable indicating if a policy was part or not of a larger package
data2$Integration2 = ifelse(data2$Integration==1, 1, 0)


#Normalize Variables
data2$IPA_z <- normFunc(data2$IPA)
data2$Technology.Specificity_z <- normFunc(data2$Technology.Specificity)
data2$Integration2_z <- normFunc(data2$Integration2)
data2$Incentives_z <- normFunc(data2$Incentives)
data2$Public.Investment_z <- normFunc(data2$Public.Investment)
data2$Tradable_z <- normFunc(data2$Tradable)
data2$Voluntary_z <- normFunc(data2$Voluntary)
data2$Education_z <- normFunc(data2$Education)
data2$Financial_z <- normFunc(data2$Financial)
data2$R.D_z <- normFunc(data2$R.D)
data2$Regulatory_z <- normFunc(data2$Regulatory)
data2$Framework_z <- normFunc(data2$Framework)
data2$Oil_price_brent_z <- normFunc(data2$Oil_price_brent)
data2$GDP_capita_z <- normFunc(data2$GDP_capita)
data2$GDP_growth_z <- normFunc(data2$GDP_growth)
data2$Change.in.Govt_z <- normFunc(data2$Change.in.Govt)
data2$Veto.Player.Index_z <- normFunc(data2$Veto.Player.Index)
data2$Green.Party.Seat.Share_z <- normFunc(data2$Green.Party.Seat.Share)

#Subset data to exclude policies that experience event "supersession" in a time interval
data3 = subset(data2,supersession==0 )
data3a = subset (data2, supersession==0 & marker ==0)
#Subset data to exclude policies that experience event "discontinuation" in a time interval
data4 = subset(data2,discontinuation==0 )

#Subset data to exclude Spain; Spain excluded for theoretical and empirical reasons, namely that Spain was particularly strong affected by the economic recession as result of the financial crisis 2008 and this also influenced renewable energy policymaking

table(data2$Country, data2$marker)

data2_sm = subset (data2, Country !="Spain")
data3a_sm = subset (data3a, Country !="Spain")
data4_sm = subset (data4, Country !="Spain")

table(data3a_sm$Start.Year.New.Num)

#Fit Cox Proportional Hazard Model---------


#use stargazer for regression outputs

#H1a + H1b: For discontinued policies----------

attach (data3a_sm)

#Models including random effects (fraility) for country but not including time-interacted variables to correct for violation of proportional hazard assumption

cox.mod3a_control = coxph(Surv(tstart_new,tstop_new, discontinuation2)~IPA_z+Technology.Specificity_z+Integration2_z +Incentives_z+Public.Investment_z
                              +Tradable_z+Voluntary_z+Education_z+Financial_z+R.D_z+Regulatory_z+Framework_z +Oil_price_brent_z+GDP_capita_z+GDP_growth_z + Change.in.Govt_z +Veto.Player.Index_z+ Green.Party.Seat.Share_z+ frailty(Country))
summary(cox.mod3a_control)

#Models including fixed effects for country but not including time-interacted variables to correct for violation of proportional hazard assumption

cox.mod3a_control2  = coxph(Surv(tstart_new,tstop_new, discontinuation2)~IPA_z+Technology.Specificity_z+Integration2_z +Incentives_z+Public.Investment_z
                                +Tradable_z+Voluntary_z+Education_z+Financial_z+R.D_z+Regulatory_z+Framework_z +Oil_price_brent_z+GDP_capita_z+GDP_growth_z + Change.in.Govt_z +Veto.Player.Index_z+ Green.Party.Seat.Share_z+ Country)
summary(cox.mod3a_control2 )



#Main Models of Interest including fixed or random effects (fraility) for country and including time-interacted variables correct for violation of proportional hazard assumption

#using fixed effects for countries

#using fixed effects
cox.mod3a_control_fixed = coxph(Surv(tstart_new,tstop_new, discontinuation2)~IPA_z+Technology.Specificity_z+Integration2_z +Incentives_z+Public.Investment_z
                                +Tradable_z+Voluntary_z+Education_z+Financial_z+R.D_z+Regulatory_z+Framework_z +Oil_price_brent_z+GDP_capita_z+GDP_growth_z + Change.in.Govt_z +Veto.Player.Index_z+ Green.Party.Seat.Share_z+tt(Financial_z)+tt(Oil_price_brent_z)+tt(Veto.Player.Index_z)+ Country, tt=function(x, t, ...)x*log(t+20) )
summary(cox.mod3a_control_fixed)

#using frailty
cox.mod3a_control_full = coxph(Surv(tstart_new,tstop_new, discontinuation2)~IPA_z+Technology.Specificity_z+Integration2_z +Incentives_z+Public.Investment_z
                               +Tradable_z+Voluntary_z+Education_z+Financial_z+R.D_z+Regulatory_z+Framework_z +Oil_price_brent_z+GDP_capita_z+GDP_growth_z + Change.in.Govt_z +Veto.Player.Index_z+ Green.Party.Seat.Share_z+tt(Financial_z)+tt(Oil_price_brent_z)+tt(Veto.Player.Index_z)+ frailty(Country), tt=function(x, t, ...)x*log(t+20))
summary(cox.mod3a_control_full)

#using coxme random intercepts per country

cox.mod3a_control_full_rand = coxme(Surv(tstart_new,tstop_new,discontinuation2)~IPA_z+Technology.Specificity_z+Integration2_z +Incentives_z+Public.Investment_z
                                        +Tradable_z+Voluntary_z+Education_z+Financial_z+R.D_z+Regulatory_z+Framework_z +Oil_price_brent_z+GDP_capita_z+GDP_growth_z + Change.in.Govt_z +Veto.Player.Index_z+ Green.Party.Seat.Share_z+Financial_z:tstart_new+Oil_price_brent_z:tstart_new+Veto.Player.Index_z:tstart_new+ (1|Country))
summary(cox.mod3a_control_full_rand)

#H2a + H2b: For superseding policies with higher IPA----------

attach (data4_sm)

#Models including random effects (fraility) for country but not including time-interacted variables to correct for violation of proportional hazard assumption

cox.mod3_control_high = coxph(Surv(tstart_new,tstop_new, supersession_IPA_high)~IPA_z+Technology.Specificity_z+Integration2_z +Incentives_z+Public.Investment_z
                         +Tradable_z+Voluntary_z+Education_z+Financial_z+R.D_z+Regulatory_z+Framework_z +Oil_price_brent_z+GDP_capita_z+GDP_growth_z + Change.in.Govt_z +Veto.Player.Index_z+ Green.Party.Seat.Share_z+ frailty(Country))
summary(cox.mod3_control_high)

#Models including fixed effects for country but not including time-interacted variables to correct for violation of proportional hazard assumption

cox.mod3_control2_high  = coxph(Surv(tstart_new,tstop_new, supersession_IPA_high)~IPA_z+Technology.Specificity_z+Integration2_z +Incentives_z+Public.Investment_z
                         +Tradable_z+Voluntary_z+Education_z+Financial_z+R.D_z+Regulatory_z+Framework_z +Oil_price_brent_z+GDP_capita_z+GDP_growth_z + Change.in.Govt_z +Veto.Player.Index_z+ Green.Party.Seat.Share_z+ Country)
summary(cox.mod3_control2_high )



#Main Models of Interest including fixed or random effects (fraility) for country and including time-interacted variables correct for violation of proportional hazard assumption

#using fixed effects for countries

#using fixed effects
cox.mod3_control_fixed_high = coxph(Surv(tstart_new,tstop_new, supersession_IPA_high)~IPA_z+Technology.Specificity_z+Integration2_z +Incentives_z+Public.Investment_z
                                    +Tradable_z+Voluntary_z+Education_z+Financial_z+R.D_z+Regulatory_z+Framework_z +Oil_price_brent_z+GDP_capita_z+GDP_growth_z + Change.in.Govt_z +Veto.Player.Index_z+ Green.Party.Seat.Share_z+tt(Incentives_z)+tt(Financial_z)+tt(GDP_capita_z)+Country, tt=function(x, t, ...)x*log(t+20))
summary(cox.mod3_control_fixed_high)

#using frailty
cox.mod3_control_full_high = coxph(Surv(tstart_new,tstop_new, supersession_IPA_high)~IPA_z+Technology.Specificity_z+Integration2_z +Incentives_z+Public.Investment_z
                                   +Tradable_z+Voluntary_z+Education_z+Financial_z+R.D_z+Regulatory_z+Framework_z +Oil_price_brent_z+GDP_capita_z+GDP_growth_z + Change.in.Govt_z +Veto.Player.Index_z+ Green.Party.Seat.Share_z+tt(Incentives_z)+tt(Financial_z)+tt(GDP_capita_z)+frailty(Country), tt=function(x, t, ...)x*log(t+20))
summary(cox.mod3_control_full_high)

#using coxme random intercepts per country

cox.mod3_control_full_rand_high = coxme(Surv(tstart_new,tstop_new,supersession_IPA_high)~IPA_z+Technology.Specificity_z+Integration2_z +Incentives_z+Public.Investment_z
                                   +Tradable_z+Voluntary_z+Education_z+Financial_z+R.D_z+Regulatory_z+Framework_z +Oil_price_brent_z+GDP_capita_z+GDP_growth_z + Change.in.Govt_z +Veto.Player.Index_z+ Green.Party.Seat.Share_z+Financial_z:tstart_new+Incentives_z:tstart_new+GDP_capita_z:tstart_new+ (1|Country))
summary(cox.mod3_control_full_rand_high)


#H3a + H3b: For superseding policies with lower IPA----------

attach (data4_sm)

#Models including random effects (fraility) for country but not including time-interacted variables to correct for violation of proportional hazard assumption

cox.mod3_control_low = coxph(Surv(tstart_new,tstop_new, supersession_IPA_low)~IPA_z+Technology.Specificity_z+Integration2_z +Incentives_z+Public.Investment_z
                              +Tradable_z+Voluntary_z+Education_z+Financial_z+R.D_z+Regulatory_z+Framework_z +Oil_price_brent_z+GDP_capita_z+GDP_growth_z + Change.in.Govt_z +Veto.Player.Index_z+ Green.Party.Seat.Share_z+ frailty(Country))
summary(cox.mod3_control_low)

#Models including fixed effects for country but not including time-interacted variables to correct for violation of proportional hazard assumption

cox.mod3_control2_low  = coxph(Surv(tstart_new,tstop_new, supersession_IPA_low)~IPA_z+Technology.Specificity_z+Integration2_z +Incentives_z+Public.Investment_z
                                +Tradable_z+Voluntary_z+Education_z+Financial_z+R.D_z+Regulatory_z+Framework_z +Oil_price_brent_z+GDP_capita_z+GDP_growth_z + Change.in.Govt_z +Veto.Player.Index_z+ Green.Party.Seat.Share_z+ Country)
summary(cox.mod3_control2_low )


#Main Models of Interest including fixed or random effects (fraility) for country and including time-interacted variables correct for violation of proportional hazard assumption

#using fixed effects for countries

#using fixed effects
cox.mod3_control_fixed_low = coxph(Surv(tstart_new,tstop_new, supersession_IPA_low)~IPA_z+Technology.Specificity_z+Integration2_z +Incentives_z+Public.Investment_z
                                   +Tradable_z+Voluntary_z+Education_z+Financial_z+R.D_z+Regulatory_z+Framework_z +Oil_price_brent_z+GDP_capita_z+GDP_growth_z + Change.in.Govt_z +Veto.Player.Index_z+ Green.Party.Seat.Share_z+ tt(GDP_growth_z)+ Country, tt=function(x, t, ...)x*log(t+20))
summary(cox.mod3_control_fixed_low)

#using frailty
cox.mod3_control_full_low = coxph(Surv(tstart_new,tstop_new, supersession_IPA_low)~IPA_z+Technology.Specificity_z+Integration2_z +Incentives_z+Public.Investment_z
                                  +Tradable_z+Voluntary_z+Education_z+Financial_z+R.D_z+Regulatory_z+Framework_z +Oil_price_brent_z+GDP_capita_z+GDP_growth_z + Change.in.Govt_z +Veto.Player.Index_z+ Green.Party.Seat.Share_z+tt(GDP_growth_z)+ frailty(Country), tt=function(x, t, ...)x*log(t+20))
summary(cox.mod3_control_full_low)

#using coxme random intercepts per country

cox.mod3_control_full_rand_low = coxme(Surv(tstart_new,tstop_new,supersession_IPA_low)~IPA_z+Technology.Specificity_z++Integration2_z +Incentives_z+Public.Investment_z
                                        +Tradable_z+Voluntary_z+Education_z+Financial_z+R.D_z+Regulatory_z+Framework_z +Oil_price_brent_z+GDP_capita_z+GDP_growth_z + Change.in.Govt_z +Veto.Player.Index_z+ Green.Party.Seat.Share_z+ GDP_growth_z:tstart_new+ (1|Country))
summary(cox.mod3_control_full_rand_low)



#Test for Model Assumptions------

#1. Non-informative censoring: yes, cut-off value of 2015 is not related to the hazard rate of policies
#2. Survival times (t) are independent: yes, only potentially not if policies are superseded. 
#3. Hazards are proportional (Hazard ratio is constant over time): check with C-Log-Log Plot and Schoenfeld test see below; if not fulfilled we need to interact explanatory variables with time
#4. ln (Hazard) is a linear function of the X variables (only the numeric ones): check with residual plots
#5. Values of X do not change overtime: they do not in our case for most variables, but some are time-variant (GDP Growth, GDP Per Capita, Change in Govt., Veto Player Index, Green Party Share) 
#6. Baseline hazard is undefined 

#Checking for proportional hazard assumption (2)----
#test for prop hazard assumption using Schoenfeld test for PH; HO: Hazards are prop.; Harzard are not prop.


cox.zph(cox.mod3a_control)
plot (cox.zph(cox.mod3a_control)) 

cox.zph(cox.mod3_control_high)
plot (cox.zph(cox.mod3_control_high)) 

cox.zph(cox.mod3_control_low)
plot (cox.zph(cox.mod3_control_low)) 


#Checking of linearity assumption (3)----

plot(predict(cox.mod2_control), residuals(cox.mod2_control, type="martingale"), xlab ="fitted values", ylab ="Martingale residuals", main ="Residual Plot", las=1) + abline (h=0)

lines (smooth.spline(predict (cox.mod2_control), residuals(cox.mod2_control, type = "martingale")), col= "red")

plot(predict(cox.mod3a_control), residuals(cox.mod3a_control, type="martingale"), xlab ="fitted values", ylab ="Martingale residuals", main ="Residual Plot", las=1) + abline (h=0)

lines (smooth.spline(predict (cox.mod3a_control), residuals(cox.mod3a_control, type = "martingale")), col= "red")

plot(predict(cox.mod3_control_low), residuals(cox.mod3_control_low, type="martingale"), xlab ="fitted values", ylab ="Martingale residuals", main ="Residual Plot", las=1) + abline (h=0)

lines (smooth.spline(predict (cox.mod3_control_low), residuals(cox.mod3_control_low, type = "martingale")), col= "red")

plot(predict(cox.mod2), residuals(cox.mod2, type="martingale"), xlab ="fitted values", ylab ="Martingale residuals", main ="Residual Plot", las=1) + abline (h=0)

lines (smooth.spline(predict (cox.mod3_control_high), residuals(cox.mod3_control_high, type = "martingale")), col= "red")

plot(predict(cox.mod3_control_high), residuals(cox.mod3_control_high, type="martingale"), xlab ="fitted values", ylab ="Martingale residuals", main ="Residual Plot", las=1) + abline (h=0)

lines (smooth.spline(predict (cox.mod2_control), residuals(cox.mod2_control, type = "martingale")), col= "red")

plot(predict(cox.mod2_control_full), residuals(cox.mod2_control_full, type="martingale"), xlab ="fitted values", ylab ="Martingale residuals", main ="Residual Plot", las=1) + abline (h=0)

lines (smooth.spline(predict (cox.mod2_control_full), residuals(cox.mod2_control_full, type = "martingale")), col= "red")


