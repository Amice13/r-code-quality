### Replication code
### Article: "How patronage delivers: Political appointments, bureaucratic accountability, and service delivery in Brazil"
### Author: Guillermo Toral (www.guillermotoral.com)
### Date: June 2022
### This file analyzes the politician survey data, generating plots and tables
### R version, platform, and package versions reported at the end of the file

# Prepare the environment -------------------------------------------------

### This section of the code prepares the environment 

# Clean the environment
rm(list = ls())

# Install required packages if not previously installed
package_list <- c("tidyverse", "here", "xtable", "texreg", "lmtest", "sandwich") 
packages_to_install <- package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(packages_to_install)>0){
  install.packages(packages_to_install)
}

# Load required packages
library(tidyverse)
library(here)
library(xtable)
library(texreg)
library(lmtest)
library(sandwich)

# Set Working Directory to wherever this file is located.
setwd(here())
# The directory where the code folder is located must also have a "plots" and a "tables" subdirectory

# Load analysis datasets ---------------------------------------------------

### This section loads the analysis dataset and prepares it for analysis with the "cjoint" package

d <- read_csv("../../datasets/analysis/survey_politicians/survey_politicians_analysis.csv")

dnr <- read_csv("../../datasets/analysis/survey_politicians/survey_politicians_nonresponse_analysis.csv")

# Generate Table 28 (correlates of response numbers by municipality)  --------------------------------------------

### This section generates Table 28 (in Appencix F.1), which reports the correlate of the number of responses per municipality

# Run regressions where the DV is the logged number of respondents, a dummy for no respondents, or the logged number of respondents excluding municipalities with no respondents
m1 <- lm(log(respondents+1) ~ log_population + deaths_perthousand + log_gdppercapita + mayor_reelected, data=dnr)
m2 <- lm(no_respondents ~ log_population + deaths_perthousand + log_gdppercapita + mayor_reelected, data=dnr)
m3 <- lm(log(respondents+1) ~ log_population + deaths_perthousand + log_gdppercapita + mayor_reelected, data=dnr, subset=dnr$respondents>0)

# Export regression results
texreg(list(m1,m2,m3),
       file="../../tables/pol_attrition.tex", table=T, float.pos="htp", scalebox=.75,
       digits=3,
       override.se = list(coeftest(m1,vcov=vcovHC(m1,type="HC1"))[,2], coeftest(m2,vcov=vcovHC(m2,type="HC1"))[,2], coeftest(m3,vcov=vcovHC(m3,type="HC1"))[,2]),
       override.pvalues = list(coeftest(m1,vcov=vcovHC(m1,type="HC1"))[,4], coeftest(m2,vcov=vcovHC(m2,type="HC1"))[,4], coeftest(m3,vcov=vcovHC(m3,type="HC1"))[,4]),
       booktabs = T, use.packages=F,
       custom.model.names = c("Respondents (log)", "No respondents \n (dummy)", "Respondents (log) \n w/o  zeroes"),
       custom.coef.map = list("log_population" = "Population (logged)", "log_gdppercapita" = "GDP per capita (logged)", "deaths_perthousand" = "Deaths per thousand", "mayor_reelected" = "Mayor was reelected in 2016","(Intercept)" = "Constant"),
       custom.gof.names = c("R-squared", "Observations"),
       include.adjrs = F, 
       stars = c(0.05, 0.01, 0.001),
       custom.note = paste("\\item %stars. HC1 standard errors in brackets."),
       caption="Correlates of the number of responses per municipality",
       caption.above = TRUE,
       label="tab:pol_attrition",
       single.row = TRUE, 
       custom.coef.names=c("Constant", NA))

# Generate Table 29 (politician survey descriptive statistics) ------------

### This section generates Table 29 (in Appendix F.2), which reports descriptive statistics for the survey of politicians, by position

# Vector with the order in which covariates will appear in the table
var <- c(7,6,8,9,5,4,3)

# Generate descriptive statistics for all politicians
### First create empty vectors to store means, and standard deviations
average <- c()
stddev <- c()
data <- d
### For each variable, collect the variable name, mean, and standard deviation
for(i in var){
  average <- c(average, mean(data[,i][[1]],na.rm=T))
  stddev <- c(stddev, sd(data[,i][[1]],na.rm=T))
}
### Bind the mean and the SD
all <- cbind(average, stddev)

# Generate descriptive statistics for mayors
average <- c()
stddev <- c()
data <- d[which(d$mayor==1),]
for(i in var){
  average <- c(average, mean(data[,i][[1]],na.rm=T))
  stddev <- c(stddev, sd(data[,i][[1]],na.rm=T))
}
mayors <- cbind(average, stddev)

# Generate descriptive statistics for mayors
average <- c()
stddev <- c()
data <- d[which(d$secretary==1),]
for(i in var){
  average <- c(average, mean(data[,i][[1]],na.rm=T))
  stddev <- c(stddev, sd(data[,i][[1]],na.rm=T))
}
secretaries <- cbind(average, stddev)

descriptives <- cbind(all, mayors, secretaries)
rownames(descriptives) <- c("Age",
                            "Female", 
                            "High school degree or less", 
                            "College degree or more",
                            "Party member", 
                            "Experience as bureaucrat (years)", 
                            "Experience as politician (years)")
colnames(descriptives) <- rep(c("Mean", "SD"),3)

print(file="../../tables/pol_descriptives.tex",
      only.contents=TRUE,
      type="latex",
      xtable(descriptives, 
             align=c("lcccccc"),
             digits=3),
      caption.placement="top", 
      comment=F,floating=F,booktabs=F)

# Extract statistics mentioned in the text ------------------------------------

### This section extracts statistics that are mentioned in the text of the article (in the section on the institutional context) and in Appendix F

# 76% of mayors rank mayors as the actor with most responsibility for improving public services
mean(d[which(d$mayor==1), "ranking_resp_improvement_mayor"][[1]]==1, na.rm=T)

# Secretaries of education, healthcare, and social assistance report on average about 12 meetings with street-level managers in their area in the previous 3 months, or about 1 per week
mean(d$meetings_managers_in_area, na.rm=T)

# There are 455 respondents
nrow(d)

# 50 of them are mayors, 405 of them are secretaries
table(d$mayor)
table(d$secretary)
