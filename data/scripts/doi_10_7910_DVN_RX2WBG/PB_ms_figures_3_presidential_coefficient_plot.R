# The Following R Code replicates the following items in the manuscript titled: "Nail in the Coffin or Lifeline? Evaluating the Electoral Impact of COVID-19 on President Trump in the 2020 Election"". The items are the following:

# 1) Pulls county-level presidential election models estimated in R code "PB_ms_county_level_presidential_models.R" and creates the coefficient plot in Figure 3: County COVID-19 Severity & Change in GOP Presidential Electoral Support

library(ggplot2)
library(lme4)

###### Presidential County-Level Models ####### 

# Load the results of the county-level model presidential election models
#Set Local Working Directory
setwd("/PB Data Replication Files")

load("presidential_model_results.rda")

# Model 1 Linear Model Estimator; OLS State Fixed-Effects with Clustered Standard Errors--Total Covid Deaths/Cases Per 100K

pres_model1_deaths_fe <- data.frame(coef=pres_model1_deaths_fe[,1],se=pres_model1_deaths_fe[,2],outcome="B) Cumulative COVID-19 Deaths",context="Presidency",model="Fixed-Effects",model2="Deaths")
pres_model1_cases_fe <- data.frame(coef=pres_model1_cases_fe[,1],se=pres_model1_cases_fe[,2],outcome="A) Cumulative COVID-19 Cases",context="Presidency",model="Fixed-Effects",model2="Cases")

# Model 1 Linear Estimator; State-Level Random Effects Model Estimation--Total Covid Deaths/Cases Per 100K

pres_model1_deaths_re <- data.frame(coef=fixef(pres_model1_deaths_re),se=sqrt(diag(vcov(pres_model1_deaths_re, useScale = FALSE))),outcome="B) Cumulative COVID-19 Deaths",context="Presidency",model="Random-Effects",model2="Deaths")
pres_model1_cases_re <- data.frame(coef=fixef(pres_model1_cases_re),se=sqrt(diag(vcov(pres_model1_cases_re, useScale = FALSE))),outcome="A) Cumulative COVID-19 Cases",context="Presidency",model="Random-Effects",model2="Cases")

# Model 1 Linear Estimator; Spatial Simultaneous Autoregressive Lag Model Estimation--Total Covid Deaths/Cases Per 100K

pres_model1_deaths_lag <- data.frame(coef=pres_model1_deaths_lag[["coefficients"]],se=pres_model1_deaths_lag[["rest.se"]],outcome="B) Cumulative COVID-19 Deaths",context="Presidency",model="Spatial Lag",model2="Deaths")
pres_model1_cases_lag <- data.frame(coef=pres_model1_cases_lag[["coefficients"]],se=pres_model1_cases_lag[["rest.se"]],outcome="A) Cumulative COVID-19 Cases",context="Presidency",model="Spatial Lag",model2="Cases")

# Model 1 Linear Estimator; Spatial Simultaneous Autoregressive Error Model Estimation--Total Covid Deaths/Cases Per 100K

pres_model1_deaths_error <- data.frame(coef=pres_model1_deaths_error[["coefficients"]],se=pres_model1_deaths_error[["rest.se"]],outcome="B) Cumulative COVID-19 Deaths",context="Presidency",model="Spatial Error",model2="Deaths")
pres_model1_cases_error <- data.frame(coef=pres_model1_cases_error[["coefficients"]],se=pres_model1_cases_error[["rest.se"]],outcome="A) Cumulative COVID-19 Cases",context="Presidency",model="Spatial Error",model2="Cases")

# Model 2 Linear Model Estimator; OLS State Fixed-Effects with Clustered Standard Errors--Daily Change 7 Day Rolling Average Covid Cases Per 100K

pres_model2_deaths_fe <- data.frame(coef=pres_model2_deaths_fe[,1],se=pres_model2_deaths_fe[,2],outcome="D) Weekly Trend COVID-19 Deaths",context="Presidency",model="Fixed-Effects",model2="Deaths")
pres_model2_cases_fe <- data.frame(coef=pres_model2_cases_fe[,1],se=pres_model2_cases_fe[,2],outcome="C) Weekly Trend COVID-19 Cases",context="Presidency",model="Fixed-Effects",model2="Cases")

# Model 2 Linear Estimator; State-Level Random Effects Model Estimation--Daily Change 7 Day Rolling Average Covid Cases Per 100K

pres_model2_deaths_re <- data.frame(coef=fixef(pres_model2_deaths_re),se=sqrt(diag(vcov(pres_model2_deaths_re, useScale = FALSE))),outcome="D) Weekly Trend COVID-19 Deaths",context="Presidency",model="Random-Effects",model2="Deaths")
pres_model2_cases_re <- data.frame(coef=fixef(pres_model2_cases_re),se=sqrt(diag(vcov(pres_model2_cases_re, useScale = FALSE))),outcome="C) Weekly Trend COVID-19 Cases",context="Presidency",model="Random-Effects",model2="Cases")

# Model 2 Linear Estimator; Spatial Simultaneous Autoregressive Lag Model Estimation--Daily Change 7 Day Rolling Average Covid Cases Per 100K

pres_model2_deaths_lag <- data.frame(coef=pres_model2_deaths_lag[["coefficients"]],se=pres_model2_deaths_lag[["rest.se"]],outcome="D) Weekly Trend COVID-19 Deaths",context="Presidency",model="Spatial Lag",model2="Deaths")
pres_model2_cases_lag <- data.frame(coef=pres_model2_cases_lag[["coefficients"]],se=pres_model2_cases_lag[["rest.se"]],outcome="C) Weekly Trend COVID-19 Cases",context="Presidency",model="Spatial Lag",model2="Cases")

# Model 2 Linear Estimator; Spatial Simultaneous Autoregressive Error Model Estimation--Daily Change 7 Day Rolling Average Covid Cases Per 100K

pres_model2_deaths_error <- data.frame(coef=pres_model2_deaths_error[["coefficients"]],se=pres_model2_deaths_error[["rest.se"]],outcome="D) Weekly Trend COVID-19 Deaths",context="Presidency",model="Spatial Error",model2="Deaths")
pres_model2_cases_error <- data.frame(coef=pres_model2_cases_error[["coefficients"]],se=pres_model2_cases_error[["rest.se"]],outcome="C) Weekly Trend COVID-19 Cases",context="Presidency",model="Spatial Error",model2="Cases")

pres_model1_deaths_fe$vars <- rownames(pres_model1_deaths_fe)
pres_model1_cases_fe$vars <- rownames(pres_model1_cases_fe)
pres_model1_deaths_re$vars <- rownames(pres_model1_deaths_re)
pres_model1_cases_re$vars <- rownames(pres_model1_cases_re)
pres_model1_deaths_lag$vars <- rownames(pres_model1_deaths_lag)
pres_model1_cases_lag$vars <- rownames(pres_model1_cases_lag)
pres_model1_deaths_error$vars <- rownames(pres_model1_deaths_error)
pres_model1_cases_error$vars <- rownames(pres_model1_cases_error)
pres_model2_deaths_fe$vars <- rownames(pres_model2_deaths_fe)
pres_model2_cases_fe$vars <- rownames(pres_model2_cases_fe)
pres_model2_deaths_re$vars <- rownames(pres_model2_deaths_re)
pres_model2_cases_re$vars <- rownames(pres_model2_cases_re)
pres_model2_deaths_lag$vars <- rownames(pres_model2_deaths_lag)
pres_model2_cases_lag$vars <- rownames(pres_model2_cases_lag)
pres_model2_deaths_error$vars <- rownames(pres_model2_deaths_error)
pres_model2_cases_error$vars <- rownames(pres_model2_cases_error)

pres <- rbind(pres_model1_deaths_fe,pres_model1_cases_fe,pres_model1_deaths_re,pres_model1_cases_re,pres_model1_deaths_lag,pres_model1_cases_lag,pres_model1_deaths_error,pres_model1_cases_error,pres_model2_deaths_fe,pres_model2_cases_fe,pres_model2_deaths_re,pres_model2_cases_re,pres_model2_deaths_lag,pres_model2_cases_lag,pres_model2_deaths_error,pres_model2_cases_error)
rownames(pres) <- NULL
rm(pres_model1_deaths_fe,pres_model1_cases_fe,pres_model1_deaths_re,pres_model1_cases_re,pres_model1_deaths_lag,pres_model1_cases_lag,pres_model1_deaths_error,pres_model1_cases_error,pres_model2_deaths_fe,pres_model2_cases_fe,pres_model2_deaths_re,pres_model2_cases_re,pres_model2_deaths_lag,pres_model2_cases_lag,pres_model2_deaths_error,pres_model2_cases_error)

###### Coefficient Plots ###### 

x <- pres
x$context <- factor(x$context,levels=c("U.S. House","U.S. Senate","Presidency"))
x <- subset(x,x$vars %in% c("daily_change_deaths_1week_10K","daily_change_deaths_per10k_1week","total_covid_deaths_10K","daily_change_cases_1week_10K","daily_change_cases_per10k_1week","total_covid_cases_10K","total_covid_cases_100K","total_covid_deaths_100K","daily_change_deaths_1week_100K","daily_change_cases_1week_100K"))

x$lower_90 <- x$coef - qt(0.95,df=3000) * x$se 
x$upper_90 <- x$coef + qt(0.95,df=3000) * x$se 
x$lower_95 <- x$coef - qt(0.975,df=3000) * x$se 
x$upper_95 <- x$coef + qt(0.975,df=3000) * x$se 

x$model <- factor(x$model,levels=c("Spatial Lag","Spatial Error","Random-Effects","Fixed-Effects"))
x$label <- ifelse(x$upper_95 > 0 & x$lower_95 > 0,as.character(round(x$coef,3)),ifelse(x$upper_95 < 0 & x$lower_95 < 0,as.character(round(x$coef,3)),NA))

  plot <- ggplot(x,aes(x=model,y=coef,factor=model,group=model,color=model,shape=model,fill=model,label=label)) + facet_wrap(~outcome,scales="free") + coord_flip() + geom_linerange(aes(x= model, ymin = lower_90, ymax = upper_90), position = position_dodge(width=0.75), lwd  = 1) + geom_pointrange(aes(x= model, ymin = lower_95, ymax = upper_95), lwd = 1/2, position = position_dodge(width=0.75),fill="white") + theme_bw() + scale_x_discrete("") + scale_y_continuous("Marginal Effect on 2020 Change in GOP Electoral Support (%)")  + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + theme(axis.text.x = element_text(hjust = 0.5),axis.text.y = element_text(hjust = 0.5)) + theme(legend.position = "none") + guides(color=guide_legend(title="Model Specification"),shape=guide_legend(title="Model Specification")) + scale_shape_manual("Model Specification",values=c(21,21,21,21))  + geom_label(vjust=-0.5,hjust=0.25,fill="white") + scale_color_manual("Model Specification",values=c("black","black","black","black"))
  ggsave("fig3_county_level_presidential_results.png",width = 8, height = 6, units = "in")
