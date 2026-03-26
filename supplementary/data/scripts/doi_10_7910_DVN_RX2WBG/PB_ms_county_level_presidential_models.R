# The Following R Code replicates the following items in the manuscript titled: "Nail in the Coffin or Lifeline? Evaluating the Electoral Impact of COVID-19 on President Trump in the 2020 Election"". The items are the following:

# 1) County-level presidential election models used in Figure 3: County COVID-19 Severity & Change in GOP Presidential Electoral Support


######################## Code Summary ##################
library(rgdal)
library(spdep)
library(tmap) 
library(sf)
library(lme4) # mixed random effects models
library(lmtest) # coeftest function
library(lmerTest) # p-values for mixed random effects models
library(multiwayvcov) # clustered standard errors
library(spatialreg) # package for the spatial dependency regressions

########################## Prelude #####################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen=999)
set.seed(1993)

######################## Upload Data ##################

#Set Local Working Directory
setwd("/PB Data Replication Files")

#Upload Data
load(file = "county_level_data.rda"); Pres_Master.1 <- Pres_Master.Pure

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

# Model 1-Total Covid Cases Per 100K

pres_model1_cases_fe <- lm(gop_vote_change_2020_lagged ~ total_covid_cases_10K + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn+ medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor  + factor(state_abb), data = y); summary(pres_model1_cases_fe) 
pres_model1_cases_fe <- coeftest(pres_model1_cases_fe, vcov = cluster.vcov(pres_model1_cases_fe, cluster=y$state_abb)) # Model 1 Linear Model Estimator; OLS State Fixed-Effects with Clustered Standard Errors--Total Covid Cases Per 100K

pres_model1_cases_re <- lmer(gop_vote_change_2020_lagged ~ total_covid_cases_10K + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn + medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor  + (1 | state_abb), data = y); summary(pres_model1_cases_re) # Model 1 Linear Estimator; State-Level Random Effects Model Estimation--Total Covid Cases Per 100K

pres_model1_cases_lag <- lagsarlm(gop_vote_change_2020_lagged ~ total_covid_cases_10K + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn+ medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor ,zero.policy =TRUE, data=counties_merged@data, W); print(summary(pres_model1_cases_lag)) # Model 1 Linear Estimator; Spatial Simultaneous Autoregressive Lag Model Estimation--Total Covid Cases Per 100K

pres_model1_cases_error <- errorsarlm(gop_vote_change_2020_lagged ~ total_covid_cases_10K + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn+ medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor ,zero.policy =TRUE, data=counties_merged@data, W); print(summary(pres_model1_cases_error)) # Model 1 Linear Estimator; Spatial Simultaneous Autoregressive Error Model Estimation--Total Covid Cases Per 100K

# Model 2-Daily Change 7 Day Rolling Average Covid Cases Per 100K

pres_model2_cases_fe <- lm(gop_vote_change_2020_lagged ~ daily_change_cases_per10k_1week+ Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn + medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor  + factor(state_abb), data = y); summary(pres_model2_cases_fe)
pres_model2_cases_fe <- coeftest(pres_model2_cases_fe, vcov = cluster.vcov(pres_model2_cases_fe, cluster=y$state_abb)) # Model 2 OLS State Fixed-Effects with Clustered Standard Errors--Daily Change 7 Day Rolling Average Covid Cases Per 100K

pres_model2_cases_re <- lmer(gop_vote_change_2020_lagged ~ daily_change_cases_per10k_1week + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn + medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor  + (1 | state_abb), data = y); summary(pres_model2_cases_re) # Model 2 Linear Estimator; State-Level Random Effects---Daily Change 7 Day Rolling Average Covid Cases Per 100K

pres_model2_cases_lag <- lagsarlm(gop_vote_change_2020_lagged ~ daily_change_cases_per10k_1week + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn+ medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor ,zero.policy =TRUE, data=counties_merged@data, W); print(summary(pres_model2_cases_lag)) # Model 1 Linear Estimator; Spatial Simultaneous Autoregressive Lag Model Estimation--Daily Change 7 Day Rolling Average Covid Cases Per 100K

pres_model2_cases_error <- errorsarlm(gop_vote_change_2020_lagged ~ daily_change_cases_per10k_1week + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn+ medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor ,zero.policy =TRUE, data=counties_merged@data, W); print(summary(pres_model2_cases_error)) # Model 1 Linear Estimator; Spatial Simultaneous Autoregressive Error Model Estimation--Daily Change 7 Day Rolling Average Covid Cases Per 100K

####################### Change in Vote Share Analysis: Deaths #######################

# Model 1-Total Covid Deaths Per 100K

pres_model1_deaths_fe <- lm(gop_vote_change_2020_lagged ~ total_covid_deaths_10K + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn+ medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor  + factor(state_abb), data = y); summary(pres_model1_deaths_fe) 
pres_model1_deaths_fe <- coeftest(pres_model1_deaths_fe, vcov = cluster.vcov(pres_model1_deaths_fe, cluster=y$state_abb)) # Model 1 Linear Estimator; OLS State Fixed-Effects with Clustered Standard Errors--Total Covid Deaths Per 100K

pres_model1_deaths_re <- lmer(gop_vote_change_2020_lagged ~ total_covid_deaths_10K + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn + medianIncome + medianAge + prcntOld + per_gop_2016 + + dem_governor  + (1 | state_abb), data = y); summary(pres_model1_deaths_re) # Model 1 Linear Estimator; State-Level Random Effects--Total Covid Deaths Per 100K

pres_model1_deaths_lag <- lagsarlm(gop_vote_change_2020_lagged ~ total_covid_deaths_10K + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn+ medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor ,zero.policy =TRUE, data=counties_merged@data, W); print(summary(pres_model1_deaths_lag)) # Model 1 Linear Estimator; Spatial Simultaneous Autoregressive Lag Model Estimation--Total Covid Deaths Per 100K

pres_model1_deaths_error <- errorsarlm(gop_vote_change_2020_lagged ~ total_covid_deaths_10K + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn+ medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor ,zero.policy =TRUE, data=counties_merged@data, W); print(summary(pres_model1_deaths_error)) # Model 1 Linear Estimator; Spatial Simultaneous Autoregressive Error Model Estimation--Total Covid Deaths Per 100K

# Model 2-Daily Change 7 Day Rolling Average Covid Deaths Per 100K

pres_model2_deaths_fe <- lm(gop_vote_change_2020_lagged ~ daily_change_deaths_per10k_1week + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn + medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor  + factor(state_abb), data = y); summary(pres_model2_deaths_fe)
pres_model2_deaths_fe <- coeftest(pres_model2_deaths_fe, vcov = cluster.vcov(pres_model2_deaths_fe, cluster=y$state_abb)) # Model 2 Linear Estimator; OLS State Fixed-Effects with Clustered Standard Errors--Daily Change 7 Day Rolling Average Covid Deaths Per 100K

pres_model2_deaths_re <- lmer(gop_vote_change_2020_lagged ~ daily_change_deaths_per10k_1week + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn + medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor  + (1 | state_abb), data = y); summary(pres_model2_deaths_re) # Model 2 Linear Estimator; State-Level Random Effects---Daily Change 7 Day Rolling Average Covid Deaths Per 100K

pres_model2_deaths_lag <- lagsarlm(gop_vote_change_2020_lagged ~ daily_change_deaths_per10k_1week + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn+ medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor ,zero.policy =TRUE, data=counties_merged@data, W); print(summary(pres_model2_deaths_lag)) # Model 1 Linear Estimator; Spatial Simultaneous Autoregressive Lag Model Estimation--Daily Change 7 Day Rolling Average Covid Deaths Per 100K

pres_model2_deaths_error <- errorsarlm(gop_vote_change_2020_lagged ~ daily_change_deaths_per10k_1week + Unemployment_Rate + pop_sq_mi  + prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn+ medianIncome + medianAge + prcntOld + per_gop_2016 + dem_governor ,zero.policy =TRUE, data=counties_merged@data, W); print(summary(pres_model2_deaths_error)) # Model 1 Linear Estimator; Spatial Simultaneous Autoregressive Error Model Estimation--Daily Change 7 Day Rolling Average Covid Deaths Per 100K

########################## Save Models #################

rm(counties,counties_merged,list.queen,Pres_Master.1,Pres_Master.Pure,W,y,gov)

save.image("presidential_model_results.rda")
