# The Following R Code replicates the following items in the manuscript titled: "Nail in the Coffin or Lifeline? Evaluating the Electoral Impact of COVID-19 on President Trump in the 2020 Election"". The items are the following:

# 1) Full county-level model results presented articulated in the following tables of the appendix: Table 1A (regression results for presidential analysis using total cumulative COVID cases per 10,000 residents), Table 2A (regression results for presidential analysis using weekly trend in COVID cases per 10,000 residents), Table 3A (regression results for presidential analysis using total cumulative COVID deaths per 10,000 residents), and Table 4A (regression results for presidential analysis using weekly trend in COVID deaths per 10,000 residents) convey the full model results of the point estimates shown in Figure 3 of the manuscript.

########################## Prelude #####################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen = 3)
set.seed(1993)

######################## Upload Data ##################

#Set Working Directory
setwd("/PB Data Replication Files")

#Upload Data
load(file = "county_level_data.rda"); Pres_Master.1 <- Pres_Master.Pure

######################### Library #####################
library(rgdal)
library(spdep)
library(tmap) 
library(sf)
library(lme4) # mixed random effects models
library(lmtest) # coeftest function
library(lmerTest) # p-values for mixed random effects models
library(multiwayvcov) # clustered standard errors
library(spatialreg) # package for the spatial dependency regressions

library(stargazer)
library(MuMIn)
library(broom)
library(dplyr)

######################### Functions ###################
Model.DF <- function(Model, Robust.SE = NULL) {
  
  #Extract Coefficients
  Model.Output <- as.data.frame(coef(summary(Model)))
  Model.Output$Label <- rownames(Model.Output)
  rownames(Model.Output) <- NULL
  
  #Generate Confidence Intervals
  CI <- as.data.frame(confint(Model, variable.names(Model), level=0.95))
  CI$Label <- rownames(CI)
  rownames(CI) <- NULL
  
  #Merge Model and CIs together 
  Model.Output.Final <- merge(x = Model.Output, y = CI, by =c("Label"))
  
  #Name the columns numeric
  colnames(Model.Output.Final) <- c("Label", "Coeff", "SE", "t.value", "P.Value", "lower", "upper")
  
  Model.Output.Final$Sig.05 <- ifelse(Model.Output.Final$P.Value <= .05, 1,0)
  Model.Output.Final$Sig.10 <- ifelse(Model.Output.Final$P.Value <= .10, 1,0)
  
  #Adjusted R Squared
  Model.Output.Final$AdJ.R2 <- summary(Model)$adj.r.squared
  
  #Dependent Variable
  Model.Output.Final$DV <- all.vars(formula(Model))[1]
  
  #Check for NA's in Model
  for(n in names(coef(Model))){
    if(is.na(Model$coefficients[[n]]) == T){
      newRow <- data.frame(Label=n, 
                           Coeff = NA, 
                           SE = NA, 
                           t.value = NA,
                           P.Value = NA,
                           lower = NA,
                           upper = NA,
                           AdJ.R2 = NA, 
                           Sig.05 = NA,
                           Sig.10 = NA,
                           DV=all.vars(formula(Model))[1])
      
      Model.Output.Final <- rbind(Model.Output.Final, newRow)
      
    }
  }
  
  #Option for Robust Standard Errors
  if(is.null(Robust.SE) == F){
    x<- coeftest(Model, vcov = vcovHC(Model, type=Robust.SE))
    xr<- setNames(data.frame(x[1:dim(x)[1], 2]), c("Robust Standard Errors"))
    xr$Label<- rownames(xr); rownames(xr) <- NULL
    
    Model.Output.Final <- merge(Model.Output.Final, xr, by = "Label")
    
  }
  
  return(Model.Output.Final)
  
}


Alternative_ModelDF <- function(Model, Type){
  
  if(Type == "RE"){
    Results_Master.P2 <- data.frame(Coeff=fixef(Model), SE=sqrt(diag(vcov(Model, useScale = FALSE))), p.value =as.numeric(coef(summary(Model))[,5]), Model="Random Effects"); Results_Master.P2$Label <- rownames(Results_Master.P2)
    
    
  }
  
  if(Type == "Spatial Lag"){
    Results_Master.P2 <- data.frame(Coeff=Model$coefficients,SE=Model$rest.se, Model="Spatial Lag"); Results_Master.P2$Label <- rownames(Results_Master.P2)
    
    
  }
  
  
  if(Type == "Spatial Error"){
    Results_Master.P2 <- data.frame(Coeff=Model$coefficients, SE=Model$rest.se, Model="Spatial Error"); Results_Master.P2$Label <- rownames(Results_Master.P2)
    
    
  }
  
  rownames(Results_Master.P2) <- NULL
  
  #******************* Major Stats in P2 ************************
  
  #Calculate Confidence Intervals 
  Results_Master.P2$lower <- Results_Master.P2$Coeff - (qt(0.975,df=3000) * Results_Master.P2$SE)
  Results_Master.P2$upper <- Results_Master.P2$Coeff + (qt(0.975,df=3000) * Results_Master.P2$SE)
  
  #Significance 
  Results_Master.P2$Sig.05 <- ifelse(Results_Master.P2$lower <=0 & Results_Master.P2$upper >= 0, 0, 1)
  
  #******************* Merge Dataset ************************
  return(Results_Master.P2)
  
}


RMSE <- function(error) { sqrt(mean(error^2)) }

RMSE_DF <- function(M1, M2, M3, M4){
  RMSE_DF <-data.frame(Model = c("FE", "RE", "SE", "SL"),
                       RMSE = c(round(RMSE(M1$residuals), 3), round(RMSE(summary(M2)$residual), 3),
                                round(RMSE(M3$residuals), 3), round(RMSE(M4$residuals), 3)))
  
  return(RMSE_DF)
  
}


######################## Examine Data ####################
head(Pres_Master.1)
colnames(Pres_Master.1)

########################## Data Management #################

counties <- readOGR("tl_2017_us_county.shp")

counties@data$state_county_fips <- paste(counties@data$STATEFP,counties@data$COUNTYFP,sep="")

Pres_Master.1$total_covid_cases_10K <- Pres_Master.1$total_covid_cases_100K/10

Pres_Master.1$daily_change_cases_per10k_1week <- Pres_Master.1$mean_covid_cases_1w_100K/10

Pres_Master.1$total_covid_deaths_10K <- Pres_Master.1$total_covid_deaths_100K/10

Pres_Master.1$daily_change_deaths_per10k_1week <- Pres_Master.1$mean_covid_deaths_1w_100K/10

Pres_Master.1$votes_dem_2016[Pres_Master.1$state_county_fips %in% "29095"] <- 168972
Pres_Master.1$votes_gop_2016[Pres_Master.1$state_county_fips %in% "29095"] <- 116211

Pres_Master.1$per_gop_2016 <- (Pres_Master.1$votes_gop_2016/(Pres_Master.1$votes_gop_2016+Pres_Master.1$votes_dem_2016)) * 100
Pres_Master.1$per_dem_2016 <- (Pres_Master.1$votes_dem_2016/(Pres_Master.1$votes_gop_2016+Pres_Master.1$votes_dem_2016)) * 100

Pres_Master.1$per_gop <- (Pres_Master.1$votes_gop/(Pres_Master.1$votes_gop+Pres_Master.1$votes_dem)) * 100
Pres_Master.1$per_dem <- (Pres_Master.1$votes_dem/(Pres_Master.1$votes_gop+Pres_Master.1$votes_dem)) * 100

Pres_Master.1$per_dem_2016 <- (Pres_Master.1$per_dem_2016/(Pres_Master.1$per_dem_2016+Pres_Master.1$per_gop_2016))*100
Pres_Master.1$per_gop_2016 <- (Pres_Master.1$per_gop_2016/(Pres_Master.1$per_dem_2016+Pres_Master.1$per_gop_2016))*100

Pres_Master.1$gop_dem_margin_2020 <- Pres_Master.1$per_gop - Pres_Master.1$per_dem
Pres_Master.1$gop_dem_margin_2016 <- Pres_Master.1$per_gop_2016 - Pres_Master.1$per_dem_2016

Pres_Master.1$gop_vote_change_2020_lagged <- Pres_Master.1$gop_dem_margin_2020-Pres_Master.1$gop_dem_margin_2016 

gov <- read.csv("state_governors_2020.csv")
gov$dem_governor <- ifelse(gov$Governor.D %in% 1,"Democratic","Republican")

y <- subset(Pres_Master.1,select=c(state_county_fips,county_name,state.name,state_abb,gop_vote_change_2020_lagged,Unemployment_Rate,pop_sq_mi,prcntHS,prcntBA,prcntBlack,prcntMulti,prcntHisp,prcntForeignBorn,medianIncome,per_gop_2016,medianAge,prcntOld,total_covid_cases_10K,daily_change_cases_per10k_1week,total_covid_deaths_10K,daily_change_deaths_per10k_1week))
y <- na.omit(y)

colnames(gov)[1] <- "state_abb"
y <- merge(y,gov,by=c("state_abb"))

counties_merged <- sp::merge(counties,y,by=c("state_county_fips"))
counties_merged <- subset(counties_merged,!is.na(counties_merged@data$state.name))

set.ZeroPolicyOption(TRUE)
list.queen <- poly2nb(counties_merged, queen=T)
W <-  spdep::nb2listw(list.queen,glist=NULL, style="W", zero.policy=T)
plot(W,coordinates(counties_merged))

####################### Change in Vote Share Analysis: Cases #######################

#Note: Dependent Variable Created Using the Following Formula
# (Pres_Master_1$per_gop / (Pres_Master_1$per_gop + Pres_Master_1$per_dem)) - (Pres_Master_1$per_gop_2016 / (Pres_Master_1$per_gop_2016 + Pres_Master_1$per_dem_2016))

####################### Pres: Model 1-Total Covid Cases Per 100K ####################### 

pres_model1_cases_fe <- lm(gop_vote_change_2020_lagged ~ total_covid_cases_10K + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn+ medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor  + factor(state_abb), data = y); summary(pres_model1_cases_fe) 
pres_model1_cases_fe <- coeftest(pres_model1_cases_fe, vcov = cluster.vcov(pres_model1_cases_fe, cluster=y$state_abb)) # Model 1 Linear Model Estimator; OLS State Fixed-Effects with Clustered Standard Errors--Total Covid Cases Per 100K

pres_model1_cases_re <- lmer(gop_vote_change_2020_lagged ~ total_covid_cases_10K + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn + medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor  + (1 | state_abb), data = y); summary(pres_model1_cases_re) # Model 1 Linear Estimator; State-Level Random Effects Model Estimation--Total Covid Cases Per 100K

pres_model1_cases_error <- errorsarlm(gop_vote_change_2020_lagged ~ total_covid_cases_10K + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn+ medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor ,zero.policy =TRUE, data=counties_merged@data, W); print(summary(pres_model1_cases_error)) # Model 1 Linear Estimator; Spatial Simultaneous Autoregressive Error Model Estimation--Total Covid Cases Per 100K

pres_model1_cases_lag <- lagsarlm(gop_vote_change_2020_lagged ~ total_covid_cases_10K + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn+ medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor ,zero.policy =TRUE, data=counties_merged@data, W); print(summary(pres_model1_cases_lag)) # Model 1 Linear Estimator; Spatial Simultaneous Autoregressive Lag Model Estimation--Total Covid Cases Per 100K

#***************** Table **********************

#Key Variables
Key_Variables <-c("gop_vote_change_2020_lagged", "(Intercept)", "total_covid_deaths_10K", "total_covid_cases_10K", "daily_change_cases_per10k_1week", "daily_change_deaths_per10k_1week",
                  "Unemployment_Rate", "pop_sq_mi", "prcntHS", "prcntBA", "prcntBlack", "prcntMulti", "prcntHisp", "prcntForeignBorn", "medianIncome", "medianAge", "prcntOld", "per_gop_2016", "dem_governorRepublican")

#Initial Regression To Make Generate Table
Init_RE <- lm(gop_vote_change_2020_lagged ~ total_covid_cases_10K + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn+ medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor, data = y)

#Make DataFrames 
FE_DF <- subset(tidy(pres_model1_cases_fe), term %in% Key_Variables)
RE_DF <- subset(Alternative_ModelDF(pres_model1_cases_re, Type = "RE"), Label %in% Key_Variables)
SE_DF <- subset(tidy(pres_model1_cases_error), term %in% Key_Variables)
SL_DF <- subset(tidy(pres_model1_cases_lag), term %in% Key_Variables)

#Name Covariates
Covariate_Labels <- c("Cumulative COVID-19 Cases",
                      "Oct. Unemployment Rate", "Population Density", "Percent High School Degree", "Percent Bachelor's Degree","Percent Black","Percent Multiracial","Percent Hispanic","Percent Foreign-Born", "Median Income", "Median Age", "Percent 62 and Older", "2016 GOP Vote Share", "Governor's Party: Republican")

#R-Squared
pres_model1_cases_fe <- lm(gop_vote_change_2020_lagged ~ total_covid_cases_10K + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn+ medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor  + factor(state_abb), data = y)
R_FE <- round(summary(pres_model1_cases_fe)$adj.r.squared, 3)
MR2_RE <- round(r.squaredGLMM(pres_model1_cases_re)[1],3); CR2_RE <- round(r.squaredGLMM(pres_model1_cases_re)[2],3) 

#RMSE
rmsedf <-RMSE_DF(pres_model1_cases_fe, pres_model1_cases_re, pres_model1_cases_error, pres_model1_cases_lag)

#Sample Size
N_fe <- nrow(model.frame(pres_model1_cases_fe))
N_re <- nrow(model.frame(pres_model1_cases_re))

Pres_Table_1 <- stargazer(list(Init_RE, Init_RE, Init_RE, Init_RE), type = "text",
                          coef = list(FE_DF$estimate, RE_DF$Coeff, SE_DF$estimate, SL_DF$estimate),
                          se = list(FE_DF$std.error, RE_DF$SE, SE_DF$std.error, SL_DF$std.error),
                          covariate.labels = Covariate_Labels,
                          title    = "Table 1A: Regression Results for Presidential Analysis using Cumulative COVID-19 Cases",
                          column.labels   = c("State FE and State Clustered SE", "Random Effects", "Spatial Error", "Spatial Lag"),
                          column.separate = c(1, 1),
                          dep.var.caption = c("Dependent Variable:"),
                          dep.var.labels  = c("2020 Change in GOP Electoral Support"), #Write the DV Here
                          omit.stat = c("all"),
                          single.row = TRUE,
                          notes = c("'Conditional R-squared", 
                                    paste("'Random Effects, Marginal R-squared =", MR2_RE, sep = " ")),
                          add.lines = list(c("Observations", N_fe, N_re, nrow(counties_merged@data), nrow(counties_merged@data)),
                                           c("Adj. R Squared", R_FE, paste(CR2_RE, "'", sep = ""), "N/A", "N/A"),
                                           c("RMSE", subset(rmsedf,  Model =="FE")$RMSE,subset(rmsedf,  Model =="RE")$RMSE,
                                             subset(rmsedf,  Model =="SE")$RMSE,subset(rmsedf,  Model =="SL")$RMSE)))


Pres_Table_1 %>% paste(., collapse = "\n") %>% cat("\n")


####################### Pres: Model 2-Daily Change 7 Day Rolling Average Covid Cases Per 100K ####################### 
pres_model2_cases_fe <- lm(gop_vote_change_2020_lagged ~ daily_change_cases_per10k_1week+ Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn + medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor  + factor(state_abb), data = y); summary(pres_model2_cases_fe)
pres_model2_cases_fe <- coeftest(pres_model2_cases_fe, vcov = cluster.vcov(pres_model2_cases_fe, cluster=y$state_abb)) # Model 2 OLS State Fixed-Effects with Clustered Standard Errors--Daily Change 7 Day Rolling Average Covid Cases Per 100K

pres_model2_cases_re <- lmer(gop_vote_change_2020_lagged ~ daily_change_cases_per10k_1week + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn + medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor  + (1 | state_abb), data = y); summary(pres_model2_cases_re) # Model 2 Linear Estimator; State-Level Random Effects---Daily Change 7 Day Rolling Average Covid Cases Per 100K

pres_model2_cases_error <- errorsarlm(gop_vote_change_2020_lagged ~ daily_change_cases_per10k_1week + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn+ medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor ,zero.policy =TRUE, data=counties_merged@data, W); print(summary(pres_model2_cases_error)) # Model 1 Linear Estimator; Spatial Simultaneous Autoregressive Error Model Estimation--Daily Change 7 Day Rolling Average Covid Cases Per 100K

pres_model2_cases_lag <- lagsarlm(gop_vote_change_2020_lagged ~ daily_change_cases_per10k_1week + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn+ medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor ,zero.policy =TRUE, data=counties_merged@data, W); print(summary(pres_model2_cases_lag)) # Model 1 Linear Estimator; Spatial Simultaneous Autoregressive Lag Model Estimation--Daily Change 7 Day Rolling Average Covid Cases Per 100K


#***************** Table **********************

#Key Variables
Key_Variables <-c("gop_vote_change_2020_lagged", "(Intercept)", "total_covid_deaths_10K", "total_covid_cases_10K", "daily_change_cases_per10k_1week", "daily_change_deaths_per10k_1week",
                  "Unemployment_Rate", "pop_sq_mi", "prcntHS", "prcntBA", "prcntBlack", "prcntMulti", "prcntHisp", "prcntForeignBorn", "medianIncome", "medianAge", "prcntOld", "per_gop_2016", "dem_governorRepublican")

#Initial Regression To Make Generate Table
Init_RE <- lm(gop_vote_change_2020_lagged ~ daily_change_cases_per10k_1week + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn+ medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor, data = y)

#Make DataFrames 
FE_DF <- subset(tidy(pres_model2_cases_fe), term %in% Key_Variables)
RE_DF <- subset(Alternative_ModelDF(pres_model2_cases_re, Type = "RE"), Label %in% Key_Variables)
SE_DF <- subset(tidy(pres_model2_cases_error), term %in% Key_Variables)
SL_DF <- subset(tidy(pres_model2_cases_lag), term %in% Key_Variables)

#Name Covariates
Covariate_Labels <- c("Weekly Trend COVID-19 Cases",
                      "Oct. Unemployment Rate", "Population Density", "Percent High School Degree", "Percent Bachelor's Degree","Percent Black","Percent Multiracial","Percent Hispanic","Percent Foreign-Born", "Median Income", "Median Age", "Percent 62 and Older", "2016 GOP Vote Share", "Governor's Party: Republican")


#R-Squared
pres_model2_cases_fe <- lm(gop_vote_change_2020_lagged ~ daily_change_cases_per10k_1week+ Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn + medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor  + factor(state_abb), data = y); summary(pres_model2_cases_fe)
R_FE <- round(summary(pres_model2_cases_fe)$adj.r.squared, 3)
MR2_RE <- round(r.squaredGLMM(pres_model2_cases_re)[1],3); CR2_RE <- round(r.squaredGLMM(pres_model2_cases_re)[2],3) 

#RMSE
rmsedf <-RMSE_DF(pres_model2_cases_fe, pres_model2_cases_re, pres_model2_cases_error, pres_model2_cases_lag)

#Sample Size
N_fe <- nrow(model.frame(pres_model2_cases_fe))
N_re <- nrow(model.frame(pres_model2_cases_re))

Pres_Table_2 <- stargazer(list(Init_RE, Init_RE, Init_RE, Init_RE), type = "text",
                          coef = list(FE_DF$estimate, RE_DF$Coeff, SE_DF$estimate, SL_DF$estimate),
                          se = list(FE_DF$std.error, RE_DF$SE, SE_DF$std.error, SL_DF$std.error),
                          covariate.labels = Covariate_Labels,
                          title    = "Table 2A: Regression Results for Presidential Analysis using Weekly Trend COVID-19 Cases",
                          column.labels   = c("State FE and State Clustered SE", "Random Effects", "Spatial Error", "Spatial Lag"),
                          column.separate = c(1, 1),
                          dep.var.caption = c("Dependent Variable:"),
                          dep.var.labels  = c("2020 Change in GOP Electoral Support"), #Write the DV Here
                          omit.stat = c("all"),
                          single.row = TRUE,
                          notes = c("'Conditional R-squared", 
                                    paste("'Random Effects, Marginal R-squared =", MR2_RE, sep = " ")),
                          add.lines = list(c("Observations", N_fe, N_re, nrow(counties_merged@data), nrow(counties_merged@data)),
                                           c("Adj. R Squared", R_FE, paste(CR2_RE, "'", sep = ""), "N/A", "N/A"),
                                           c("RMSE", subset(rmsedf,  Model =="FE")$RMSE,subset(rmsedf,  Model =="RE")$RMSE,
                                             subset(rmsedf,  Model =="SE")$RMSE,subset(rmsedf,  Model =="SL")$RMSE)))

Pres_Table_2 %>% paste(., collapse = "\n") %>% cat("\n")

####################### Change in Vote Share Analysis: Deaths #######################
#######################  Pres: Model 1-Total Covid Deaths Per 100K ####################### 
pres_model1_deaths_fe <- lm(gop_vote_change_2020_lagged ~ total_covid_deaths_10K + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn+ medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor  + factor(state_abb), data = y); summary(pres_model1_deaths_fe) 
pres_model1_deaths_fe <- coeftest(pres_model1_deaths_fe, vcov = cluster.vcov(pres_model1_deaths_fe, cluster=y$state_abb)) # Model 1 Linear Estimator; OLS State Fixed-Effects with Clustered Standard Errors--Total Covid Deaths Per 100K

pres_model1_deaths_re <- lmer(gop_vote_change_2020_lagged ~ total_covid_deaths_10K + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn + medianIncome + medianAge + prcntOld + per_gop_2016 + + dem_governor  + (1 | state_abb), data = y); summary(pres_model1_deaths_re) # Model 1 Linear Estimator; State-Level Random Effects--Total Covid Deaths Per 100K

pres_model1_deaths_error <- errorsarlm(gop_vote_change_2020_lagged ~ total_covid_deaths_10K + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn+ medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor ,zero.policy =TRUE, data=counties_merged@data, W); print(summary(pres_model1_deaths_error)) # Model 1 Linear Estimator; Spatial Simultaneous Autoregressive Error Model Estimation--Total Covid Deaths Per 100K

pres_model1_deaths_lag <- lagsarlm(gop_vote_change_2020_lagged ~ total_covid_deaths_10K + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn+ medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor ,zero.policy =TRUE, data=counties_merged@data, W); print(summary(pres_model1_deaths_lag)) # Model 1 Linear Estimator; Spatial Simultaneous Autoregressive Lag Model Estimation--Total Covid Deaths Per 100K

#***************** Table **********************

#Key Variables
Key_Variables <-c("gop_vote_change_2020_lagged", "(Intercept)", "total_covid_deaths_10K", "total_covid_cases_10K", "daily_change_cases_per10k_1week", "daily_change_deaths_per10k_1week",
                  "Unemployment_Rate", "pop_sq_mi", "prcntHS", "prcntBA", "prcntBlack", "prcntMulti", "prcntHisp", "prcntForeignBorn", "medianIncome", "medianAge", "prcntOld", "per_gop_2016", "dem_governorRepublican")

#Initial Regression To Make Generate Table
Init_RE <- lm(gop_vote_change_2020_lagged ~ total_covid_deaths_10K + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn+ medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor, data = y)

#Make DataFrames 
FE_DF <- subset(tidy(pres_model1_deaths_fe), term %in% Key_Variables)
RE_DF <- subset(Alternative_ModelDF(pres_model1_deaths_re, Type = "RE"), Label %in% Key_Variables)
SE_DF <- subset(tidy(pres_model1_deaths_error), term %in% Key_Variables)
SL_DF <- subset(tidy(pres_model1_deaths_lag), term %in% Key_Variables)

#Name Covariates
Covariate_Labels <- c("Cumulative COVID-19 Deaths",
                      "Oct. Unemployment Rate", "Population Density", "Percent High School Degree", "Percent Bachelor's Degree","Percent Black","Percent Multiracial","Percent Hispanic","Percent Foreign-Born", "Median Income", "Median Age", "Percent 62 and Older", "2016 GOP Vote Share", "Governor's Party: Republican")

#R-Squared
pres_model1_deaths_fe <- lm(gop_vote_change_2020_lagged ~ total_covid_deaths_10K + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn+ medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor  + factor(state_abb), data = y); summary(pres_model1_deaths_fe) 
R_FE <- round(summary(pres_model1_deaths_fe)$adj.r.squared, 3)
MR2_RE <- round(r.squaredGLMM(pres_model1_deaths_re)[1],3); CR2_RE <- round(r.squaredGLMM(pres_model1_deaths_re)[2],3) 

#RMSE
rmsedf <-RMSE_DF(pres_model1_deaths_fe, pres_model1_deaths_re, pres_model1_deaths_error, pres_model1_deaths_lag)

#Sample Size
N_fe <- nrow(model.frame(pres_model1_deaths_fe))
N_re <- nrow(model.frame(pres_model1_deaths_re))


Pres_Table_3 <- stargazer(list(Init_RE, Init_RE, Init_RE, Init_RE), type = "text",
                          coef = list(FE_DF$estimate, RE_DF$Coeff, SE_DF$estimate, SL_DF$estimate),
                          se = list(FE_DF$std.error, RE_DF$SE, SE_DF$std.error, SL_DF$std.error),
                          covariate.labels = Covariate_Labels,
                          title    = "Table 3A: Regression Results for Presidential Analysis using Cumulative COVID-19 Deaths",
                          column.labels   = c("State FE and State Clustered SE", "Random Effects", "Spatial Error", "Spatial Lag"),
                          column.separate = c(1, 1),
                          dep.var.caption = c("Dependent Variable:"),
                          dep.var.labels  = c("2020 Change in GOP Electoral Support"), #Write the DV Here
                          omit.stat = c("all"),
                          single.row = TRUE,
                          notes = c("'Conditional R-squared", 
                                    paste("'Random Effects, Marginal R-squared =", MR2_RE, sep = " ")),
                          add.lines = list(c("Observations", N_fe, N_re, nrow(counties_merged@data), nrow(counties_merged@data)),
                                           c("Adj. R Squared", R_FE, paste(CR2_RE, "'", sep = ""), "N/A", "N/A"),
                                           c("RMSE", subset(rmsedf,  Model =="FE")$RMSE,subset(rmsedf,  Model =="RE")$RMSE,
                                             subset(rmsedf,  Model =="SE")$RMSE,subset(rmsedf,  Model =="SL")$RMSE)))

Pres_Table_3 %>% paste(., collapse = "\n") %>% cat("\n")

#######################  Pres: Model 2-Daily Change 7 Day Rolling Average Covid Deaths Per 100K ####################### 
pres_model2_deaths_fe <- lm(gop_vote_change_2020_lagged ~ daily_change_deaths_per10k_1week + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn + medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor  + factor(state_abb), data = y); summary(pres_model2_deaths_fe)
pres_model2_deaths_fe <- coeftest(pres_model2_deaths_fe, vcov = cluster.vcov(pres_model2_deaths_fe, cluster=y$state_abb)) # Model 2 Linear Estimator; OLS State Fixed-Effects with Clustered Standard Errors--Daily Change 7 Day Rolling Average Covid Deaths Per 100K

pres_model2_deaths_re <- lmer(gop_vote_change_2020_lagged ~ daily_change_deaths_per10k_1week + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn + medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor  + (1 | state_abb), data = y); summary(pres_model2_deaths_re) # Model 2 Linear Estimator; State-Level Random Effects---Daily Change 7 Day Rolling Average Covid Deaths Per 100K

pres_model2_deaths_error <- errorsarlm(gop_vote_change_2020_lagged ~ daily_change_deaths_per10k_1week + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn+ medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor ,zero.policy =TRUE, data=counties_merged@data, W); print(summary(pres_model2_deaths_error)) # Model 1 Linear Estimator; Spatial Simultaneous Autoregressive Error Model Estimation--Daily Change 7 Day Rolling Average Covid Deaths Per 100K

pres_model2_deaths_lag <- lagsarlm(gop_vote_change_2020_lagged ~ daily_change_deaths_per10k_1week + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn+ medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor ,zero.policy =TRUE, data=counties_merged@data, W); print(summary(pres_model2_deaths_lag)) # Model 1 Linear Estimator; Spatial Simultaneous Autoregressive Lag Model Estimation--Daily Change 7 Day Rolling Average Covid Deaths Per 100K

#***************** Table **********************

#Key Variables
Key_Variables <-c("gop_vote_change_2020_lagged", "(Intercept)", "total_covid_deaths_10K", "total_covid_cases_10K", "daily_change_cases_per10k_1week", "daily_change_deaths_per10k_1week",
                  "Unemployment_Rate", "pop_sq_mi", "prcntHS", "prcntBA", "prcntBlack", "prcntMulti", "prcntHisp", "prcntForeignBorn", "medianIncome", "medianAge", "prcntOld", "per_gop_2016", "dem_governorRepublican")

#Initial Regression To Make Generate Table
Init_RE <- lm(gop_vote_change_2020_lagged ~ daily_change_deaths_per10k_1week + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn+ medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor, data = y)

#Make DataFrames 
FE_DF <- subset(tidy(pres_model2_deaths_fe), term %in% Key_Variables)
RE_DF <- subset(Alternative_ModelDF(pres_model2_deaths_re, Type = "RE"), Label %in% Key_Variables)
SE_DF <- subset(tidy(pres_model2_deaths_error), term %in% Key_Variables)
SL_DF <- subset(tidy(pres_model2_deaths_lag), term %in% Key_Variables)

#Name Covariates
Covariate_Labels <- c("Weekly Trend COVID-19 Deaths",
                      "Oct. Unemployment Rate", "Population Density", "Percent High School Degree", "Percent Bachelor's Degree","Percent Black","Percent Multiracial","Percent Hispanic","Percent Foreign-Born", "Median Income", "Median Age", "Percent 62 and Older", "2016 GOP Vote Share", "Governor's Party: Republican")

#R-Squared
pres_model2_deaths_fe <- lm(gop_vote_change_2020_lagged ~ daily_change_deaths_per10k_1week + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn + medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor  + factor(state_abb), data = y); summary(pres_model2_deaths_fe)
R_FE <- round(summary(pres_model2_deaths_fe)$adj.r.squared, 3)
MR2_RE <- round(r.squaredGLMM(pres_model2_deaths_re)[1],3); CR2_RE <- round(r.squaredGLMM(pres_model2_deaths_re)[2],3) 

#RMSE
rmsedf <-RMSE_DF(pres_model2_deaths_fe, pres_model2_deaths_re, pres_model2_deaths_error, pres_model2_deaths_lag)

#Sample Size
N_fe <- nrow(model.frame(pres_model2_deaths_fe))
N_re <- nrow(model.frame(pres_model2_deaths_re))

Pres_Table_4 <- stargazer(list(Init_RE, Init_RE, Init_RE, Init_RE), type = "text",
                          coef = list(FE_DF$estimate, RE_DF$Coeff, SE_DF$estimate, SL_DF$estimate),
                          se = list(FE_DF$std.error, RE_DF$SE, SE_DF$std.error, SL_DF$std.error),
                          covariate.labels = Covariate_Labels,
                          title    = "Table 4A: Regression Results for Presidential Analysis using Weekly Trend COVID-19 Deaths",
                          column.labels   = c("State FE and State Clustered SE", "Random Effects", "Spatial Error", "Spatial Lag"),
                          column.separate = c(1, 1),
                          dep.var.caption = c("Dependent Variable:"),
                          dep.var.labels  = c("2020 Change in GOP Electoral Support"), #Write the DV Here
                          omit.stat = c("all"),
                          single.row = TRUE,
                          notes = c("'Conditional R-squared", 
                                    paste("'Random Effects, Marginal R-squared =", MR2_RE, sep = " ")),
                          add.lines = list(c("Observations", N_fe, N_re, nrow(counties_merged@data), nrow(counties_merged@data)),
                                           c("Adj. R Squared", R_FE, paste(CR2_RE, "'", sep = ""), "N/A", "N/A"),
                                           c("RMSE", subset(rmsedf,  Model =="FE")$RMSE,subset(rmsedf,  Model =="RE")$RMSE,
                                             subset(rmsedf,  Model =="SE")$RMSE,subset(rmsedf,  Model =="SL")$RMSE)))

Pres_Table_4 %>% paste(., collapse = "\n") %>% cat("\n")

###################### Final Tables ####################
cat("\f")
Pres_Table_1 %>% paste(., collapse = "\n") %>% cat("\n")
Pres_Table_2 %>% paste(., collapse = "\n") %>% cat("\n")
Pres_Table_3 %>% paste(., collapse = "\n") %>% cat("\n")
Pres_Table_4 %>% paste(., collapse = "\n") %>% cat("\n")


