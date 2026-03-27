# This R script contains the code to replicate the aggregate-level analysis of the PRQ Publication: "The Role of State & National Institutional Evaluations in Fostering Collective Accountability Across the U.S. States"

# This R replication file reproduces: 

# Table 3: JARS OLS Models Predicting Party State Legislative Seat Change: Pooled Chambers

# Table 4: JARS OLS Models Predicting Party State Legislative Seat Change: Lower Chambers

# Table 5: JARS OLS Models Predicting Party State Legislative Seat Change: Upper Chambers

# Table 8: MRP OLS Models Predicting Party State Legislative Seat Change: Pooled Chambers

# Table 9: MRP OLS Models Predicting Party State Legislative Seat Change: Lower Chambers
 
# Table 10: MRP OLS Models Predicting Party State Legislative Seat Change: Upper Chambers

# Table 11: MRP OLS Models Predicting Party State Legislative Seat Change Over Time, 2006-2020

# Table 12: MRP OLS Models Predicting Party State Legislative Seat Change Over Time, 2006-2020

##### Load Relevant Packages #####

library(stargazer)
library(panelView)
library(descr)
library(fixest)
library(DataCombine)
library(modelsummary)
library(plyr)

##### 1) Load Aggregate Dataset #####

setwd("") # Set the working directory on local machine with the appropriate source data files

load("aggregate_data_state_legislatve.Rdata")

#### 2) JARS hypothesis testing ####

jars_legislative_election_years_merge <- subset(jars_legislative_election_years_merge,jars_legislative_election_years_merge$year >= 1958)

jars_legislative_election_years_merge$dem_president <- ifelse(jars_legislative_election_years_merge$year %in% c(1961:1968,1977:1980,1993:2000,2009:2016),1,0)

jars_legislative_election_years_merge$dem_governor <- ifelse(jars_legislative_election_years_merge$governor_party == 1,1,ifelse(jars_legislative_election_years_merge$governor_party == 0,0,NA))

jars_legislative_election_years_merge$dem_chamber <- ifelse(jars_legislative_election_years_merge$dem_chamber == "Democratic",1,ifelse(jars_legislative_election_years_merge$dem_chamber == "Republican",0,-1))

jars_legislative_election_years_merge$pres_mean_approval <- jars_legislative_election_years_merge$pres_mean_approval-50

jars_legislative_election_years_merge$gov_mean_approval <- jars_legislative_election_years_merge$gov_mean_approval-50

jars_legislative_election_years_merge$state_chamber <- paste(jars_legislative_election_years_merge$state,jars_legislative_election_years_merge$chamber)

jars_legislative_election_years_merge$net_chamber_d_prop <- jars_legislative_election_years_merge$net_chamber_d_prop * 100

x <- subset(jars_legislative_election_years_merge,select=c(year,state,upper_chamber,net_chamber_d_prop,gov_mean_approval,pres_mean_approval,complete_jars_data,gov_party_jars,pres_party_jars,state_chamber,dem_president,dem_governor))

# All Chambers

# Model 1
model1 <- feols(net_chamber_d_prop ~ i(dem_president,pres_mean_approval) | factor(state_chamber),data=x,cluster = c("state_chamber"))
#model1_effects$model <- "Baseline Presidential Approval Model"

# Model 2

model2 <- feols(net_chamber_d_prop ~ i(dem_governor,gov_mean_approval) | factor(state_chamber),data=x,cluster = c("state_chamber"))
#model2_effects$model <- "Baseline Gubernatorial Approval Model"

# Model 3

model3 <- feols(net_chamber_d_prop ~ i(dem_president,pres_mean_approval) + i(dem_governor,gov_mean_approval) | factor(state_chamber),data=x,cluster = c("state_chamber"))
#model3_effects$model <- "Executive Approval Model"

panels <- list('Presidential Approval Model'=model1,'Gubernatorial Approval Model'=model2,'Executive Approval Model'=model3)

modelsummary(panels,coef_rename=c("Presidential Approval x Republican President","Presidential Approval x Democratic President","Gubernatorial Approval x Republican Governor","Gubernatorial Approval x Democratic Governor"),gof_omit = 'DF|Deviance|RMSE|AIC|BIC',fmt = 3,estimate = "{estimate}{stars}",title="JARS OLS Models Predicting Democratic State Legislative Seat Change: Pooled Chambers",notes = c('Dependent Variable: Democratic Party Seat Turnover','Data: U.S. Officials Job Approval Ratings (JARs) Database'),output = "Table3_jars_models_pooled.tex")

#  Only Lower Chambers

# Model 1

x1 <- subset(x,x$upper_chamber == "Lower Chamber")

# Model 1
model1 <- feols(net_chamber_d_prop ~ i(dem_president,pres_mean_approval) | factor(state_chamber),data=x1,cluster = c("state_chamber"))
#model1_effects$model <- "Baseline Presidential Approval Model"

# Model 2

model2 <- feols(net_chamber_d_prop ~ i(dem_governor,gov_mean_approval) | factor(state_chamber),data=x1,cluster = c("state_chamber"))
#model2_effects$model <- "Baseline Gubernatorial Approval Model"

# Model 3

model3 <- feols(net_chamber_d_prop ~ i(dem_president,pres_mean_approval) + i(dem_governor,gov_mean_approval) | factor(state_chamber),data=x1,cluster = c("state_chamber"))
#model3_effects$model <- "Executive Approval Model"

panels <- list('Presidential Approval Model'=model1,'Gubernatorial Approval Model'=model2,'Executive Approval Model'=model3)

modelsummary(panels,coef_rename=c("Presidential Approval x Republican President","Presidential Approval x Democratic President","Gubernatorial Approval x Republican Governor","Gubernatorial Approval x Democratic Governor"),gof_omit = 'DF|Deviance|RMSE|AIC|BIC',fmt = 3,estimate = "{estimate}{stars}",title="JARS OLS Models Predicting Democratic State Legislative Seat Change: Lower Chambers",notes = c('Dependent Variable: Democratic Party Seat Turnover','Data: U.S. Officials Job Approval Ratings (JARs) Database'),output = "Table4_jars_models_lower.tex")
# Upper Chamber

# Model 1

x1 <- subset(x,x$upper_chamber == "Upper Chamber")

# Model 1
model1 <- feols(net_chamber_d_prop ~ i(dem_president,pres_mean_approval) | factor(state_chamber),data=x1,cluster = c("state_chamber"))
#model1_effects$model <- "Baseline Presidential Approval Model"

# Model 2

model2 <- feols(net_chamber_d_prop ~ i(dem_governor,gov_mean_approval) | factor(state_chamber),data=x1,cluster = c("state_chamber"))
#model2_effects$model <- "Baseline Gubernatorial Approval Model"

# Model 3

model3 <- feols(net_chamber_d_prop ~ i(dem_president,pres_mean_approval) + i(dem_governor,gov_mean_approval) | factor(state_chamber),data=x1,cluster = c("state_chamber"))
#model3_effects$model <- "Executive Approval Model"

panels <- list('Presidential Approval Model'=model1,'Gubernatorial Approval Model'=model2,'Executive Approval Model'=model3)

modelsummary(panels,coef_rename=c("Presidential Approval x Republican President","Presidential Approval x Democratic President","Gubernatorial Approval x Republican Governor","Gubernatorial Approval x Democratic Governor"),gof_omit = 'DF|Deviance|RMSE|AIC|BIC',fmt = 3,estimate = "{estimate}{stars}",title="JARS OLS Models Predicting Democratic State Legislative Seat Change: Upper Chambers",notes = c('Dependent Variable: Democratic Party Seat Turnover','Data: U.S. Officials Job Approval Ratings (JARs) Database'),output = "Table5_jars_models_upper.tex")

#### 3) MRP Estimates Hypothesis Testing ####

mrp_legislative_election_years_merge$dem_president <- ifelse(mrp_legislative_election_years_merge$year %in% c(2006:2008,2017:2020),0,1)

mrp_legislative_election_years_merge$dem_governor <- ifelse(mrp_legislative_election_years_merge$governor_party == 1,1,ifelse(mrp_legislative_election_years_merge$governor_party == 0, 0,NA))

table(mrp_legislative_election_years_merge$dem_president,mrp_legislative_election_years_merge$governor_party)

mrp_legislative_election_years_merge$dem_chamber[mrp_legislative_election_years_merge$dem_chamber == "Split"] <- NA

mrp_legislative_election_years_merge$dem_chamber <- factor(mrp_legislative_election_years_merge$dem_chamber,levels=c("Republican","Democratic"))

mrp_legislative_election_years_merge$dem_congress <- ifelse(mrp_legislative_election_years_merge$year %in% c(2007:2010),"Democratic",ifelse(mrp_legislative_election_years_merge$year %in% c(2006,2015:2018),"Republican",ifelse(mrp_legislative_election_years_merge$year %in% c(2011:2014,2019:2020),"Split",NA)))

mrp_legislative_election_years_merge$dem_congress <- factor(mrp_legislative_election_years_merge$dem_congress,levels=c("Republican","Split","Democratic"))

mrp_legislative_election_years_merge$net_chamber_d_prop <- mrp_legislative_election_years_merge$net_chamber_d_prop*100

mrp_legislative_election_years_merge$pres_mean_approval <- mrp_legislative_election_years_merge$pres_mean_approval-50

mrp_legislative_election_years_merge$gov_mean_approval <- mrp_legislative_election_years_merge$gov_mean_approval-50

mrp_legislative_election_years_merge$cong_mean_approval <- mrp_legislative_election_years_merge$cong_mean_approval-50

mrp_legislative_election_years_merge$state_leg_mean_approval <- mrp_legislative_election_years_merge$state_leg_mean_approval-50

mrp_legislative_election_years_merge$state_chamber <- paste(mrp_legislative_election_years_merge$state,mrp_legislative_election_years_merge$chamber)

x <- subset(state_leg_seats,select=c(year,state,dem_house,dem_senate))

x$dem_legislature <- ifelse(x$dem_house == "Democratic" & x$dem_senate == "Democratic","Democratic",ifelse(x$dem_house == "Republican" & x$dem_senate == "Republican","Republican","Split"))
x$dem_legislature <- factor(x$dem_legislature)

mrp_legislative_election_years_merge <- merge(mrp_legislative_election_years_merge,x,by=c("year","state"))

x <- subset(mrp_legislative_election_years_merge,select=c(year,state,state_chamber,upper_chamber,dem_president, dem_governor,dem_congress,dem_chamber,gov_mean_approval,pres_mean_approval,governor_party,state_leg_mean_approval,cong_mean_approval,net_chamber_d_prop,dem_legislature))

x <- x[order(x$state_chamber,x$year),]
x <- slide(x, Var = "net_chamber_d_prop", slideBy = -1,GroupVar=c("state_chamber"))
colnames(x)[16] <- "lag_net_chamber_d_prop"

x <- subset(x,x$year >= 2006)

# All Chambers

# Model 1
model1 <- feols(net_chamber_d_prop ~ i(dem_president,pres_mean_approval) + lag_net_chamber_d_prop + year | factor(state_chamber),data=x,cluster = c("state_chamber"))
#model1_effects$model <- "Baseline Presidential Approval Model"

# Model 2

model2 <- feols(net_chamber_d_prop ~ i(dem_governor,gov_mean_approval) + lag_net_chamber_d_prop + year | factor(state_chamber),data=x,cluster = c("state_chamber"))
#model2_effects$model <- "Baseline Gubernatorial Approval Model"

# Model 3

model3 <- feols(net_chamber_d_prop ~  i(dem_president,pres_mean_approval) + i(dem_governor,gov_mean_approval) + lag_net_chamber_d_prop + year | factor(state_chamber),data=x,cluster = c("state_chamber"))
#model3_effects$model <- "Executive Approval Model"

model4 <- feols(net_chamber_d_prop ~ i(dem_president,pres_mean_approval) + i(dem_governor,gov_mean_approval) + i(dem_congress,cong_mean_approval) + i(dem_legislature,state_leg_mean_approval) + lag_net_chamber_d_prop + year | factor(state_chamber),data=x,cluster = c("state_chamber"))
#model4_effects$model <- "Executive & Legislative Approval"

panels <- list('Presidential Approval Model'=model1,'Gubernatorial Approval Model'=model2,'Executive Approval Model'=model3,'Executive & Legislative Approval Model'=model4)

modelsummary(panels,coef_omit=c(3,4),coef_rename=c("Presidential Approval x Republican President","Presidential Approval x Democratic President","Governor Approval x Republican Governor","Governor Approval x Democratic Governor","Congressional Approval x Republican Congress","Congressional Approval x Split Congress","Congressional Approval x Democratic Congress","State Legislative Approval x Democratic Legislature","State Legislative Approval x Republican Legislature","State Legislative Approval x Split Legislature"),gof_omit = 'DF|Deviance|RMSE|AIC|BIC',fmt = 3,estimate = "{estimate}{stars}",title="MRP OLS Models Predicting Democratic State Legislative Seat Change: Pooled Chambers",notes = c('Dependent Variable: Democratic Party Seat Turnover','Data: MRP Estimates Derived from CES Survey Data.'),output = "Table8_mrp_models_pooled.tex")

#  Only Lower Chambers

# Model 1

x1 <- subset(x,x$upper_chamber == "Lower Chamber")

model1 <- feols(net_chamber_d_prop ~ i(dem_president,pres_mean_approval) + lag_net_chamber_d_prop + year | factor(state_chamber),data=x1,cluster = c("state_chamber"))
#model1_effects$model <- "Baseline Presidential Approval Model"

# Model 2

model2 <- feols(net_chamber_d_prop ~ i(dem_governor,gov_mean_approval) + lag_net_chamber_d_prop + year | factor(state_chamber),data=x1,cluster = c("state_chamber"))
#model2_effects$model <- "Baseline Gubernatorial Approval Model"

# Model 3

model3 <- feols(net_chamber_d_prop ~  i(dem_president,pres_mean_approval) + i(dem_governor,gov_mean_approval) + lag_net_chamber_d_prop + year | factor(state_chamber),data=x1,cluster = c("state_chamber"))
#model3_effects$model <- "Executive Approval Model"

model4 <- feols(net_chamber_d_prop ~ i(dem_president,pres_mean_approval) + i(dem_governor,gov_mean_approval) + i(dem_congress,cong_mean_approval) + i(dem_legislature,state_leg_mean_approval) + lag_net_chamber_d_prop + year | factor(state_chamber),data=x1,cluster = c("state_chamber"))
#model4_effects$model <- "Executive & Legislative Approval"

panels <- list('Presidential Approval Model'=model1,'Gubernatorial Approval Model'=model2,'Executive Approval Model'=model3,'Executive & Legislative Approval Model'=model4)

modelsummary(panels,coef_omit=c(3,4),coef_rename=c("Presidential Approval x Republican President","Presidential Approval x Democratic President","Governor Approval x Republican Governor","Governor Approval x Democratic Governor","Congressional Approval x Republican Congress","Congressional Approval x Split Congress","Congressional Approval x Democratic Congress","State Legislative Approval x Democratic Legislature","State Legislative Approval x Republican Legislature","State Legislative Approval x Split Legislature"),gof_omit = 'DF|Deviance|RMSE|AIC|BIC',fmt = 3,estimate = "{estimate}{stars}",title="MRP OLS Models Predicting Democratic State Legislative Seat Change: Lower Chambers",notes = c('Dependent Variable: Democratic Party Seat Turnover','Data: MRP Estimates Derived from CES Survey Data.'),output = "Table9_mrp_models_lower.tex")

# Upper Chamber

# Model 1

x1 <- subset(x,x$upper_chamber == "Upper Chamber")

model1 <- feols(net_chamber_d_prop ~ i(dem_president,pres_mean_approval) + lag_net_chamber_d_prop + year | factor(state_chamber),data=x1,cluster = c("state_chamber"))
#model1_effects$model <- "Baseline Presidential Approval Model"

# Model 2

model2 <- feols(net_chamber_d_prop ~ i(dem_governor,gov_mean_approval) + lag_net_chamber_d_prop + year | factor(state_chamber),data=x1,cluster = c("state_chamber"))
#model2_effects$model <- "Baseline Gubernatorial Approval Model"

# Model 3

model3 <- feols(net_chamber_d_prop ~  i(dem_president,pres_mean_approval) + i(dem_governor,gov_mean_approval) + lag_net_chamber_d_prop + year | factor(state_chamber),data=x1,cluster = c("state_chamber"))
#model3_effects$model <- "Executive Approval Model"

model4 <- feols(net_chamber_d_prop ~ i(dem_president,pres_mean_approval) + i(dem_governor,gov_mean_approval) + i(dem_congress,cong_mean_approval) + i(dem_legislature,state_leg_mean_approval) + lag_net_chamber_d_prop + year | factor(state_chamber),data=x1,cluster = c("state_chamber"))
#model4_effects$model <- "Executive & Legislative Approval"

panels <- list('Presidential Approval Model'=model1,'Gubernatorial Approval Model'=model2,'Executive Approval Model'=model3,'Executive & Legislative Approval Model'=model4)

modelsummary(panels,coef_omit=c(3,4),coef_rename=c("Presidential Approval x Republican President","Presidential Approval x Democratic President","Governor Approval x Republican Governor","Governor Approval x Democratic Governor","Congressional Approval x Republican Congress","Congressional Approval x Split Congress","Congressional Approval x Democratic Congress","State Legislative Approval x Democratic Legislature","State Legislative Approval x Republican Legislature","State Legislative Approval x Split Legislature"),gof_omit = 'DF|Deviance|RMSE|AIC|BIC',fmt = 3,estimate = "{estimate}{stars}",title="MRP OLS Models Predicting Democratic State Legislative Seat Change: Upper Chambers",notes = c('Dependent Variable: Democratic Party Seat Turnover','Data: MRP Estimates Derived from CES Survey Data.'),output = "Table10_mrp_models_upper.tex")

######### 4) Temporal Heterogenity Tables ######

# Temporal Heterogenity

x <- subset(mrp_legislative_election_years_merge,select=c(year,state,state_chamber,upper_chamber,dem_president, dem_governor,dem_congress,dem_chamber,gov_mean_approval,pres_mean_approval,governor_party,state_leg_mean_approval,cong_mean_approval,net_chamber_d_prop))

x <- x[order(x$state_chamber,x$year),]
x <- slide(x, Var = "net_chamber_d_prop", slideBy = -1,GroupVar=c("state_chamber"))
colnames(x)[15] <- "lag_net_chamber_d_prop"

x <- subset(x,x$year %in% seq(2006,2020,1))
x1 <- x

x1$president <- ifelse(x$year %in% seq(2006,2008,1),"Bush",ifelse(x$year %in% seq(2009,2012,1),"Obama",ifelse(x$year %in% seq(2013,2016,1),"Obama",ifelse(x$year %in% seq(2017,2020,1),"Trump",NA))))

x1$president <- factor(x1$president,levels=c("Bush","Obama","Trump"))

# Model 1

model1 <- feols(net_chamber_d_prop ~ i(president,pres_mean_approval) + lag_net_chamber_d_prop | factor(state_chamber),data=x1,cluster = c("state_chamber"))

# Model 2

x <- subset(x1,x1$upper_chamber == "Lower Chamber")

model2 <- feols(net_chamber_d_prop ~ i(president,pres_mean_approval) + lag_net_chamber_d_prop | factor(state_chamber),data=x,cluster = c("state_chamber"))

# Model 3

x <- subset(x1,x1$upper_chamber == "Upper Chamber")

model3 <- feols(net_chamber_d_prop ~ i(president,pres_mean_approval) + lag_net_chamber_d_prop | factor(state_chamber),data=x,cluster = c("state_chamber"))

panels <- list('All Chambers'=model1,'Lower Chambers'=model2,'Upper Chambers'=model3)

modelsummary(panels,coef_omit = c(4),coef_rename = c("Presidential Approval x Pres. Bush","Presidential Approval x Pres. Obama","Presidential Approval x Pres. Trump"),gof_omit = 'DF|Deviance|RMSE|AIC|BIC',fmt = 3,estimate = "{estimate}{stars}",title="MRP OLS Models Predicting Democratic State Legislative Seat Change Over Time, 2006-2020",notes = c('Dependent Variable: Democratic Party Seat Turnover','Data: MRP Estimates Derived from CES Survey Data.'),output = "Table11_temporal_mrp_models.tex")

# Option Two
# Model 1

model1 <- feols(net_chamber_d_prop ~ i(year,pres_mean_approval) + lag_net_chamber_d_prop | factor(state_chamber),data=subset(x1,x1$year %in% seq(2006,2020,2)),cluster = c("state_chamber"))

summary(model1, cluster = c("state_chamber"))
model1 <- summary(model1, cluster = c("state_chamber"))
model1_effects <- data.frame(model1$coeftable)
colnames(model1_effects) <- c("estimate","se","t_value","p_value")
model1_effects$effect <- rownames(model1_effects)
rownames(model1_effects) <- NULL
model1_effects$nobs <- nobs(model1)
model1_effects$model <- "All Chambers"

# Model 2

x <- subset(x1,x1$upper_chamber == "Lower Chamber")

model2 <- feols(net_chamber_d_prop ~ i(year,pres_mean_approval) + lag_net_chamber_d_prop | factor(state_chamber),data=subset(x,x$year %in% seq(2006,2020,2)),cluster = c("state_chamber"))

summary(model2, cluster = c("state_chamber"))
model2 <- summary(model2, cluster = c("state_chamber"))
model2_effects <- data.frame(model2$coeftable)
colnames(model2_effects) <- c("estimate","se","t_value","p_value")
model2_effects$effect <- rownames(model2_effects)
rownames(model2_effects) <- NULL
model2_effects$nobs <- nobs(model2)
model2_effects$model <- "Lower Chambers"


# Model 3

x <- subset(x1,x1$upper_chamber == "Upper Chamber")

model3 <- feols(net_chamber_d_prop ~ i(year,pres_mean_approval) + lag_net_chamber_d_prop | factor(state_chamber),data=subset(x,x$year %in% seq(2006,2020,2)),cluster = c("state_chamber"))

summary(model3, cluster = c("state_chamber"))
model3 <- summary(model3, cluster = c("state_chamber"))
model3_effects <- data.frame(model3$coeftable)
colnames(model3_effects) <- c("estimate","se","t_value","p_value")
model3_effects$effect <- rownames(model3_effects)
rownames(model3_effects) <- NULL
model3_effects$nobs <- nobs(model3)
model3_effects$model <- "Upper Chambers"

panels <- list('All Chambers'=model1,'Lower Chambers'=model2,'Upper Chambers'=model3)

modelsummary(panels,coef_omit = c(9),coef_rename = c("Pres. Approval x 2006","Pres. Approval x 2008","Pres. Approval x 2010","Pres. Approval x 2012","Pres. Approval x 2014","Pres. Approval x 2016","Pres. Approval x 2018","Pres. Approval x 2020"),gof_omit = 'DF|Deviance|RMSE|AIC|BIC',fmt = 3,estimate = "{estimate}{stars}",title="MRP OLS Models Predicting Democratic State Legislative Seat Change Over Time, 2006-2020",notes = c('Dependent Variable: Democratic Party Seat Turnover','Data: MRP Estimates Derived from CES Survey Data.'),output = "Table12_temporal_mrp_models_by_year.tex")

