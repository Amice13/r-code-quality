### Replication code
### Article: "How patronage delivers: Political appointments, bureaucratic accountability, and service delivery in Brazil"
### Author: Guillermo Toral (www.guillermotoral.com)
### Date: June 2022
### This file analyzes the bureaucrat conjoint experiment data, generating plots and tables
### R version, platform, and package versions reported at the end of the file

# Prepare the environment -------------------------------------------------

### This section of the code prepares the environment 

# Clean the environment
rm(list = ls())

# Install required packages if not previously installed
package_list <- c("tidyverse", "here", "xtable", "cjoint") 
packages_to_install <- package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(packages_to_install)>0){
  install.packages(packages_to_install)
}

# Load required packages
library(tidyverse)
library(here)
library(xtable)
library(cjoint)

# Set Working Directory to wherever this file is located.
setwd(here())
# The directory where the code folder is located must also have a "plots" and a "tables" subdirectory

# Loads and prepares the conjoint analysis dataset ------------------------------------------------------------

### This section loads the analysis dataset and prepares it for analysis with the "cjoint" package

d <- read_csv("../../datasets/analysis/survey_bureaucrats/conjoint_bureaucrats_analysis.csv")

d$experience <- as.factor(d$experience)
d$education <- as.factor(d$education)
d$selection <- as.factor(d$selection)
d$political_connections <- as.factor(d$political_connections)
d$unit_performance <- as.factor(d$unit_performance)
d$relationship_to_professionals <- as.factor(d$relationship_to_professionals)

d$experience <- relevel(d$experience, "3 years")
d$education <- relevel(d$education, "Bachelors")
d$selection <- relevel(d$selection, "Civil service")
d$political_connections <- relevel(d$political_connections, "No connections")
d$unit_performance <- relevel(d$unit_performance, "Targets not met")
d$relationship_to_professionals <- relevel(d$relationship_to_professionals, "Bad")

### Remove profiles from tasks where no profile was chosen
### In some choice tasks respondents preferred to skip, or were (by lottery) exposed to a pair of identical profiles

d_reform <- subset(d, !is.na(d$chosen_reform))
d_performance <- subset(d, !is.na(d$chosen_performance))
d_implementation <- subset(d, !is.na(d$chosen_implementation))
d_communication <- subset(d, !is.na(d$chosen_communication))

# Generate Figure 7 (results from conjoint experiment with bureaucrats) -----------------------------------------

# Run regressions where the DV is the choice in each of the questions, covariates are profile attributes, and standard errors are clustered at the respondent level
m1 <- amce(chosen_reform ~ experience + education + selection + political_connections + unit_performance + relationship_to_professionals,
                  data=d_reform, respondent.id = "respondent_id", cluster=T)
m2 <- amce(chosen_performance ~ experience + education + selection + political_connections + unit_performance + relationship_to_professionals,
                       data=d_performance, respondent.id = "respondent_id", cluster=T)
m3 <- amce(chosen_implementation ~ experience + education + selection + political_connections + unit_performance + relationship_to_professionals,
                          data=d_implementation, respondent.id = "respondent_id", cluster=T)
m4 <- amce(chosen_communication ~ experience + education + selection + political_connections + unit_performance + relationship_to_professionals,
                         data=d_communication, respondent.id = "respondent_id", cluster=T)

# Define vector with attributes for plot
attributes <- rev(c("Political appointee \n (vs civil service)", "Elected \n (vs civil service)", "Has political \n connections (vs not)", "Holds Masters degree \n (vs Bachelors)", "Has 10 years of \n experience (vs 3 years)","Unit targets were \n met (vs not)","Has good relationships \n w/ professionals (vs bad)"))

# Create table with results for resources question
critical_value <- qt(.975, df=m1$samplesize_prof-8)
amce <- rev(c(m1$estimates$selection[1,2], m1$estimates$selection[1,1], m1$estimates$politicalconnections[1,1], m1$estimates$education[1,1],m1$estimates$experience[1,1], m1$estimates$unitperformance[1,1], m1$estimates$relationshiptoprofessionals[1,1]))
se <- rev(c(m1$estimates$selection[2,2], m1$estimates$selection[2,1],m1$estimates$politicalconnections[2,1], m1$estimates$education[2,1],m1$estimates$experience[2,1], m1$estimates$unitperformance[2,1], m1$estimates$relationshiptoprofessionals[2,1]))
results_resources <- as_tibble(cbind(attributes, amce, se))
results_resources <- results_resources %>%
  mutate(amce = as.numeric(amce), se = as.numeric(se),
         cilow = amce-critical_value*se, ciup=amce+critical_value*se)

# Create table with results for performance question
critical_value <- qt(.975, df=m2$samplesize_prof-8)
amce <- rev(c(m2$estimates$selection[1,2], m2$estimates$selection[1,1], m2$estimates$politicalconnections[1,1], m2$estimates$education[1,1], m2$estimates$experience[1,1], m2$estimates$unitperformance[1,1], m2$estimates$relationshiptoprofessionals[1,1]))
se <- rev(c(m2$estimates$selection[2,2], m2$estimates$selection[2,1],m2$estimates$politicalconnections[2,1], m2$estimates$education[2,1],m2$estimates$experience[2,1], m2$estimates$unitperformance[2,1], m2$estimates$relationshiptoprofessionals[2,1]))
results_results <- as_tibble(cbind(attributes, amce, se))
results_results <- results_results %>%
  mutate(amce = as.numeric(amce), se = as.numeric(se),
         cilow = amce-critical_value*se, ciup=amce+critical_value*se)

# Create table with results for implementation question
critical_value <- qt(.975, df=m3$samplesize_prof-8)
amce <- rev(c(m3$estimates$selection[1,2], m3$estimates$selection[1,1], m3$estimates$politicalconnections[1,1], m3$estimates$education[1,1], m3$estimates$experience[1,1], m3$estimates$unitperformance[1,1], m3$estimates$relationshiptoprofessionals[1,1]))
se <- rev(c(m3$estimates$selection[2,2], m3$estimates$selection[2,1],m3$estimates$politicalconnections[2,1], m3$estimates$education[2,1],m3$estimates$experience[2,1], m3$estimates$unitperformance[2,1], m3$estimates$relationshiptoprofessionals[2,1]))
results_implementation <- as_tibble(cbind(attributes, amce, se))
results_implementation <- results_implementation %>%
  mutate(amce = as.numeric(amce), se = as.numeric(se),
         cilow = amce-critical_value*se, ciup=amce+critical_value*se)

# Create table with results for communication question
critical_value <- qt(.975, df=m4$samplesize_prof-8)
amce <- rev(c(m4$estimates$selection[1,2], m4$estimates$selection[1,1], m4$estimates$politicalconnections[1,1], m4$estimates$education[1,1], m4$estimates$experience[1,1], m4$estimates$unitperformance[1,1], m4$estimates$relationshiptoprofessionals[1,1]))
se <- rev(c(m4$estimates$selection[2,2], m4$estimates$selection[2,1],m4$estimates$politicalconnections[2,1], m4$estimates$education[2,1],m4$estimates$experience[2,1], m4$estimates$unitperformance[2,1], m4$estimates$relationshiptoprofessionals[2,1]))
results_communication <- as_tibble(cbind(attributes, amce, se))
results_communication <- results_communication %>%
  mutate(amce = as.numeric(amce), se = as.numeric(se),
         cilow = amce-critical_value*se, ciup=amce+critical_value*se)

# Generate plot with AMCE estimates and their confidence intervals
pdf("../../plots/conjoint_bureaucrats.pdf", width=7.5, height=6.5)
par(mar=c(5, 11, 2, 2))
plot(NULL, ylim = c(0.5,7.5), xlim = c(-0.2, 0.25), yaxt = "n",
     xlab = "Average marginal component effect", ylab = "", cex.main = 1)
grid()
abline(v=0, col="gray", lty=1,lwd=1.5)
axis(2, at=c(1:7),labels=results_resources$attributes, las=2, cex=1.3)
segments(y0=c(1.15:7.15),x0 = results_communication$cilow, x1 = results_communication$ciup,
         col = "black", lwd = 2)
points(y = c(1.15:7.15), x = results_communication$amce, pch = 16, col="black", cex=1)
segments(y0=c(1.05:7.05),x0 = results_implementation$cilow, x1 = results_implementation$ciup,
         col = "blue", lwd = 2)
points(y = c(1.05:7.05), x = results_implementation$amce, pch = 17, col="blue", cex=1)
segments(y0=c(0.95:6.95),x0 = results_resources$cilow, x1 = results_resources$ciup,
         col = "red", lwd = 2)
points(y = c(0.95:6.95), x = results_resources$amce, pch = 15, col="red", cex=1)
segments(y0=c(0.85:6.85),x0 = results_results$cilow, x1 = results_results$ciup,
         col = "darkgreen", lwd = 2)
points(y = c(0.85:6.85), x = results_results$amce, pch = 18, col="darkgreen", cex=1)
legend("bottomleft", c("Communication", "Implementation", "Resource raising", "Performance"), lty=1, pch=c(16,17,15,18), col=c("black", "blue", "red","darkgreen"), bty="n")
dev.off()

# SUPLEMENTARY ANALYSES (reported in Online Appendix) ---------------------
# Generate Table 27 (results from conjoint experiment with bureaucrats) ---------------------------

# Define vector with row names for the rable
vars <- c("Appointment: Political (vs civil service)", "Appointment: Election (vs civil service)", "Political connections: Yes (vs no)","Education: Masters (vs Bachelors)", "Experience: 10 years (vs 3 years)", "Unit performance: Targets were met (vs not met)", "Relationship to professionals: Good (vs bad)",  "Number of respondents", "Number of valid profiles")

# Define vectors with regression coefficients, stars corresponding to p-values, and standard errors for each of the choice tasks
betas_commun <- c(paste0(paste0(round(m4$estimates$selection[1,2],3), ifelse(summary(m4)$amce[6,6]<0.001,"***", ifelse(summary(m4)$amce[6,6]<0.01,"**", ifelse(summary(m4)$amce[6,6]<0.05,"*",""))),sep=""),paste0(" (",round(m4$estimates$selection[2,2],3),")",sep=""),sep=""),
                  paste0(paste0(round(m4$estimates$selection[1,1],3), ifelse(summary(m4)$amce[5,6]<0.001,"***", ifelse(summary(m4)$amce[5,6]<0.01,"**", ifelse(summary(m4)$amce[5,6]<0.05,"*",""))),sep=""),paste0(" (",round(m4$estimates$selection[2,1],3),")",sep=""),sep=""),
                  paste0(paste0(round(m4$estimates$politicalconnections[1,1],3), ifelse(summary(m4)$amce[3,6]<0.001,"***", ifelse(summary(m4)$amce[3,6]<0.01,"**", ifelse(summary(m4)$amce[3,6]<0.05,"*",""))),sep=""),paste0(" (",round(m4$estimates$politicalconnections[2,1],3),")",sep=""),sep=""),
                  paste0(paste0(round(m4$estimates$education[1,1],3), ifelse(summary(m4)$amce[1,6]<0.001,"***", ifelse(summary(m4)$amce[1,6]<0.01,"**", ifelse(summary(m4)$amce[1,6]<0.05,"*",""))),sep=""),paste0(" (",round(m4$estimates$education[2,1],3),")",sep=""),sep=""),
                  paste0(paste0(round(m4$estimates$experience[1,1],3), ifelse(summary(m4)$amce[2,6]<0.001,"***", ifelse(summary(m4)$amce[2,6]<0.01,"**", ifelse(summary(m4)$amce[2,6]<0.05,"*",""))),sep=""),paste0(" (",round(m4$estimates$experience[2,1],3),")",sep=""),sep=""),
                  paste0(paste0(round(m4$estimates$unitperformance[1,1],3), ifelse(summary(m4)$amce[7,6]<0.001,"***", ifelse(summary(m4)$amce[7,6]<0.01,"**", ifelse(summary(m4)$amce[7,6]<0.05,"*",""))),sep=""),paste0(" (",round(m4$estimates$unitperformance[2,1],3),")",sep=""),sep=""),
                  paste0(paste0(round(m4$estimates$relationshiptoprofessionals[1,1],3), ifelse(summary(m4)$amce[4,6]<0.001,"***", ifelse(summary(m4)$amce[4,6]<0.01,"**", ifelse(summary(m4)$amce[4,6]<0.05,"*",""))),sep=""),paste0(" (",round(m4$estimates$relationshiptoprofessionals[2,1],3),")",sep=""),sep=""),
                  m4$numrespondents, m4$samplesize_prof)
betas_implem <- c(paste0(paste0(round(m3$estimates$selection[1,2],3), ifelse(summary(m3)$amce[6,6]<0.001,"***", ifelse(summary(m3)$amce[6,6]<0.01,"**", ifelse(summary(m3)$amce[6,6]<0.05,"*",""))),sep=""),paste0(" (",round(m3$estimates$selection[2,2],3),")",sep=""),sep=""),
                  paste0(paste0(round(m3$estimates$selection[1,1],3), ifelse(summary(m3)$amce[5,6]<0.001,"***", ifelse(summary(m3)$amce[5,6]<0.01,"**", ifelse(summary(m3)$amce[5,6]<0.05,"*",""))),sep=""),paste0(" (",round(m3$estimates$selection[2,1],3),")",sep=""),sep=""),
                  paste0(paste0(round(m3$estimates$politicalconnections[1,1],3), ifelse(summary(m3)$amce[3,6]<0.001,"***", ifelse(summary(m3)$amce[3,6]<0.01,"**", ifelse(summary(m3)$amce[3,6]<0.05,"*",""))),sep=""),paste0(" (",round(m3$estimates$politicalconnections[2,1],3),")",sep=""),sep=""),
                  paste0(paste0(round(m3$estimates$education[1,1],3), ifelse(summary(m3)$amce[1,6]<0.001,"***", ifelse(summary(m3)$amce[1,6]<0.01,"**", ifelse(summary(m3)$amce[1,6]<0.05,"*",""))),sep=""),paste0(" (",round(m3$estimates$education[2,1],3),")",sep=""),sep=""),
                  paste0(paste0(round(m3$estimates$experience[1,1],3), ifelse(summary(m3)$amce[2,6]<0.001,"***", ifelse(summary(m3)$amce[2,6]<0.01,"**", ifelse(summary(m3)$amce[2,6]<0.05,"*",""))),sep=""),paste0(" (",round(m3$estimates$experience[2,1],3),")",sep=""),sep=""),
                  paste0(paste0(round(m3$estimates$unitperformance[1,1],3), ifelse(summary(m3)$amce[7,6]<0.001,"***", ifelse(summary(m3)$amce[7,6]<0.01,"**", ifelse(summary(m3)$amce[7,6]<0.05,"*",""))),sep=""),paste0(" (",round(m3$estimates$unitperformance[2,1],3),")",sep=""),sep=""),
                  paste0(paste0(round(m3$estimates$relationshiptoprofessionals[1,1],3), ifelse(summary(m3)$amce[4,6]<0.001,"***", ifelse(summary(m3)$amce[4,6]<0.01,"**", ifelse(summary(m3)$amce[4,6]<0.05,"*",""))),sep=""),paste0(" (",round(m3$estimates$relationshiptoprofessionals[2,1],3),")",sep=""),sep=""),
                  m3$numrespondents, m3$samplesize_prof)
betas_results <- c(paste0(paste0(round(m2$estimates$selection[1,2],3), ifelse(summary(m2)$amce[6,6]<0.001,"***", ifelse(summary(m2)$amce[6,6]<0.01,"**", ifelse(summary(m2)$amce[6,6]<0.05,"*",""))),sep=""),paste0(" (",round(m2$estimates$selection[2,2],3),")",sep=""),sep=""),
                   paste0(paste0(round(m2$estimates$selection[1,1],3), ifelse(summary(m2)$amce[5,6]<0.001,"***", ifelse(summary(m2)$amce[5,6]<0.01,"**", ifelse(summary(m2)$amce[5,6]<0.05,"*",""))),sep=""),paste0(" (",round(m2$estimates$selection[2,1],3),")",sep=""),sep=""),
                   paste0(paste0(round(m2$estimates$politicalconnections[1,1],3), ifelse(summary(m2)$amce[3,6]<0.001,"***", ifelse(summary(m2)$amce[3,6]<0.01,"**", ifelse(summary(m2)$amce[3,6]<0.05,"*",""))),sep=""),paste0(" (",round(m2$estimates$politicalconnections[2,1],3),")",sep=""),sep=""),
                   paste0(paste0(round(m2$estimates$education[1,1],3), ifelse(summary(m2)$amce[1,6]<0.001,"***", ifelse(summary(m2)$amce[1,6]<0.01,"**", ifelse(summary(m2)$amce[1,6]<0.05,"*",""))),sep=""),paste0(" (",round(m2$estimates$education[2,1],3),")",sep=""),sep=""),
                   paste0(paste0(round(m2$estimates$experience[1,1],3), ifelse(summary(m2)$amce[2,6]<0.001,"***", ifelse(summary(m2)$amce[2,6]<0.01,"**", ifelse(summary(m2)$amce[2,6]<0.05,"*",""))),sep=""),paste0(" (",round(m2$estimates$experience[2,1],3),")",sep=""),sep=""),
                   paste0(paste0(round(m2$estimates$unitperformance[1,1],3), ifelse(summary(m2)$amce[7,6]<0.001,"***", ifelse(summary(m2)$amce[7,6]<0.01,"**", ifelse(summary(m2)$amce[7,6]<0.05,"*",""))),sep=""),paste0(" (",round(m2$estimates$unitperformance[2,1],3),")",sep=""),sep=""),
                   paste0(paste0(round(m2$estimates$relationshiptoprofessionals[1,1],3), ifelse(summary(m2)$amce[4,6]<0.001,"***", ifelse(summary(m2)$amce[4,6]<0.01,"**", ifelse(summary(m2)$amce[4,6]<0.05,"*",""))),sep=""),paste0(" (",round(m2$estimates$relationshiptoprofessionals[2,1],3),")",sep=""),sep=""),
                   m2$numrespondents, m2$samplesize_prof) 
betas_resour <- c(paste0(paste0(round(m1$estimates$selection[1,2],3), ifelse(summary(m1)$amce[6,6]<0.001,"***", ifelse(summary(m1)$amce[6,6]<0.01,"**", ifelse(summary(m1)$amce[6,6]<0.05,"*",""))),sep=""),paste0(" (",round(m1$estimates$selection[2,2],3),")",sep=""),sep=""),
                  paste0(paste0(round(m1$estimates$selection[1,1],3), ifelse(summary(m1)$amce[5,6]<0.001,"***", ifelse(summary(m1)$amce[5,6]<0.01,"**", ifelse(summary(m1)$amce[5,6]<0.05,"*",""))),sep=""),paste0(" (",round(m1$estimates$selection[2,1],3),")",sep=""),sep=""),
                  paste0(paste0(round(m1$estimates$politicalconnections[1,1],3), ifelse(summary(m1)$amce[3,6]<0.001,"***", ifelse(summary(m1)$amce[3,6]<0.01,"**", ifelse(summary(m1)$amce[3,6]<0.05,"*",""))),sep=""),paste0(" (",round(m1$estimates$politicalconnections[2,1],3),")",sep=""),sep=""),
                  paste0(paste0(round(m1$estimates$education[1,1],3), ifelse(summary(m1)$amce[1,6]<0.001,"***", ifelse(summary(m1)$amce[1,6]<0.01,"**", ifelse(summary(m1)$amce[1,6]<0.05,"*",""))),sep=""),paste0(" (",round(m1$estimates$education[2,1],3),")",sep=""),sep=""),
                  paste0(paste0(round(m1$estimates$experience[1,1],3), ifelse(summary(m1)$amce[2,6]<0.001,"***", ifelse(summary(m1)$amce[2,6]<0.01,"**", ifelse(summary(m1)$amce[2,6]<0.05,"*",""))),sep=""),paste0(" (",round(m1$estimates$experience[2,1],3),")",sep=""),sep=""),
                  paste0(paste0(round(m1$estimates$unitperformance[1,1],3), ifelse(summary(m1)$amce[7,6]<0.001,"***", ifelse(summary(m1)$amce[7,6]<0.01,"**", ifelse(summary(m1)$amce[7,6]<0.05,"*",""))),sep=""),paste0(" (",round(m1$estimates$unitperformance[2,1],3),")",sep=""),sep=""),
                  paste0(paste0(round(m1$estimates$relationshiptoprofessionals[1,1],3), ifelse(summary(m1)$amce[4,6]<0.001,"***", ifelse(summary(m1)$amce[4,6]<0.01,"**", ifelse(summary(m1)$amce[4,6]<0.05,"*",""))),sep=""),paste0(" (",round(m1$estimates$relationshiptoprofessionals[2,1],3),")",sep=""),sep=""),
                  m1$numrespondents, m1$samplesize_prof) 

# Bind all results together and name columns
results_bur <- as_tibble(cbind(vars,betas_commun, betas_implem, betas_resour, betas_results))
colnames(results_bur) <- c("", "Communication","Implementation","Resources","Performance")

# Export regression table
print(file="../../tables/conjoint_bureaucrats.tex",
      only.contents=TRUE,
      type="latex",
      xtable(results_bur, align=c("lccccc")),caption.placement="top", include.rownames=FALSE, comment=F,floating=F,booktabs=F)

# Generate Figure 21-left (conjoint results among unappointed bureaucrats) -----------------------------------------

# Load data, and exclude appointed ones
d <- read_csv("../../datasets/analysis/survey_bureaucrats/conjoint_bureaucrats_analysis.csv")
d <- d %>%
  dplyr::filter(appointed==0)

d$experience <- as.factor(d$experience)
d$education <- as.factor(d$education)
d$selection <- as.factor(d$selection)
d$political_connections <- as.factor(d$political_connections)
d$unit_performance <- as.factor(d$unit_performance)
d$relationship_to_professionals <- as.factor(d$relationship_to_professionals)

d$experience <- relevel(d$experience, "3 years")
d$education <- relevel(d$education, "Bachelors")
d$selection <- relevel(d$selection, "Civil service")
d$political_connections <- relevel(d$political_connections, "No connections")
d$unit_performance <- relevel(d$unit_performance, "Targets not met")
d$relationship_to_professionals <- relevel(d$relationship_to_professionals, "Bad")

### Remove profiles from tasks where no profile was chosen
### In some choice tasks respondents preferred to skip, or were (by lottery) exposed to a pair of identical profiles

d_reform <- subset(d, !is.na(d$chosen_reform))
d_performance <- subset(d, !is.na(d$chosen_performance))
d_implementation <- subset(d, !is.na(d$chosen_implementation))
d_communication <- subset(d, !is.na(d$chosen_communication))


# Run regressions where the DV is the choice in each of the questions, covariates are profile attributes, and standard errors are clustered at the respondent level
m1 <- amce(chosen_reform ~ experience + education + selection + political_connections + unit_performance + relationship_to_professionals,
           data=d_reform, respondent.id = "respondent_id", cluster=T)
m2 <- amce(chosen_performance ~ experience + education + selection + political_connections + unit_performance + relationship_to_professionals,
           data=d_performance, respondent.id = "respondent_id", cluster=T)
m3 <- amce(chosen_implementation ~ experience + education + selection + political_connections + unit_performance + relationship_to_professionals,
           data=d_implementation, respondent.id = "respondent_id", cluster=T)
m4 <- amce(chosen_communication ~ experience + education + selection + political_connections + unit_performance + relationship_to_professionals,
           data=d_communication, respondent.id = "respondent_id", cluster=T)

# Define vector with attributes for plot
attributes <- rev(c("Political appointee \n (vs civil service)", "Elected \n (vs civil service)", "Has political \n connections (vs not)", "Holds Masters degree \n (vs Bachelors)", "Has 10 years of \n experience (vs 3 years)","Unit targets were \n met (vs not)","Has good relationships \n w/ professionals (vs bad)"))

# Create table with results for resources question
critical_value <- qt(.975, df=m1$samplesize_prof-8)
amce <- rev(c(m1$estimates$selection[1,2], m1$estimates$selection[1,1], m1$estimates$politicalconnections[1,1], m1$estimates$education[1,1],m1$estimates$experience[1,1], m1$estimates$unitperformance[1,1], m1$estimates$relationshiptoprofessionals[1,1]))
se <- rev(c(m1$estimates$selection[2,2], m1$estimates$selection[2,1],m1$estimates$politicalconnections[2,1], m1$estimates$education[2,1],m1$estimates$experience[2,1], m1$estimates$unitperformance[2,1], m1$estimates$relationshiptoprofessionals[2,1]))
results_resources <- as_tibble(cbind(attributes, amce, se))
results_resources <- results_resources %>%
  mutate(amce = as.numeric(amce), se = as.numeric(se),
         cilow = amce-critical_value*se, ciup=amce+critical_value*se)

# Create table with results for performance question
critical_value <- qt(.975, df=m2$samplesize_prof-8)
amce <- rev(c(m2$estimates$selection[1,2], m2$estimates$selection[1,1], m2$estimates$politicalconnections[1,1], m2$estimates$education[1,1], m2$estimates$experience[1,1], m2$estimates$unitperformance[1,1], m2$estimates$relationshiptoprofessionals[1,1]))
se <- rev(c(m2$estimates$selection[2,2], m2$estimates$selection[2,1],m2$estimates$politicalconnections[2,1], m2$estimates$education[2,1],m2$estimates$experience[2,1], m2$estimates$unitperformance[2,1], m2$estimates$relationshiptoprofessionals[2,1]))
results_results <- as_tibble(cbind(attributes, amce, se))
results_results <- results_results %>%
  mutate(amce = as.numeric(amce), se = as.numeric(se),
         cilow = amce-critical_value*se, ciup=amce+critical_value*se)

# Create table with results for implementation question
critical_value <- qt(.975, df=m3$samplesize_prof-8)
amce <- rev(c(m3$estimates$selection[1,2], m3$estimates$selection[1,1], m3$estimates$politicalconnections[1,1], m3$estimates$education[1,1], m3$estimates$experience[1,1], m3$estimates$unitperformance[1,1], m3$estimates$relationshiptoprofessionals[1,1]))
se <- rev(c(m3$estimates$selection[2,2], m3$estimates$selection[2,1],m3$estimates$politicalconnections[2,1], m3$estimates$education[2,1],m3$estimates$experience[2,1], m3$estimates$unitperformance[2,1], m3$estimates$relationshiptoprofessionals[2,1]))
results_implementation <- as_tibble(cbind(attributes, amce, se))
results_implementation <- results_implementation %>%
  mutate(amce = as.numeric(amce), se = as.numeric(se),
         cilow = amce-critical_value*se, ciup=amce+critical_value*se)

# Create table with results for communication question
critical_value <- qt(.975, df=m4$samplesize_prof-8)
amce <- rev(c(m4$estimates$selection[1,2], m4$estimates$selection[1,1], m4$estimates$politicalconnections[1,1], m4$estimates$education[1,1], m4$estimates$experience[1,1], m4$estimates$unitperformance[1,1], m4$estimates$relationshiptoprofessionals[1,1]))
se <- rev(c(m4$estimates$selection[2,2], m4$estimates$selection[2,1],m4$estimates$politicalconnections[2,1], m4$estimates$education[2,1],m4$estimates$experience[2,1], m4$estimates$unitperformance[2,1], m4$estimates$relationshiptoprofessionals[2,1]))
results_communication <- as_tibble(cbind(attributes, amce, se))
results_communication <- results_communication %>%
  mutate(amce = as.numeric(amce), se = as.numeric(se),
         cilow = amce-critical_value*se, ciup=amce+critical_value*se)

# Generate plot with AMCE estimates and their confidence intervals
pdf("../../plots/conjoint_bureaucrats_unappointed.pdf", width=7.5, height=6.5)
par(mar=c(5, 11, 2, 2))
plot(NULL, ylim = c(0.5,7.5), xlim = c(-0.2, 0.25), yaxt = "n",
     xlab = "Average marginal component effect", ylab = "", cex.main = 1,
     main = "Only unappointed respondents")
grid()
abline(v=0, col="gray", lty=1,lwd=1.5)
axis(2, at=c(1:7),labels=results_resources$attributes, las=2, cex=1.3)
segments(y0=c(1.15:7.15),x0 = results_communication$cilow, x1 = results_communication$ciup,
         col = "black", lwd = 2)
points(y = c(1.15:7.15), x = results_communication$amce, pch = 16, col="black", cex=1)
segments(y0=c(1.05:7.05),x0 = results_implementation$cilow, x1 = results_implementation$ciup,
         col = "blue", lwd = 2)
points(y = c(1.05:7.05), x = results_implementation$amce, pch = 17, col="blue", cex=1)
segments(y0=c(0.95:6.95),x0 = results_resources$cilow, x1 = results_resources$ciup,
         col = "red", lwd = 2)
points(y = c(0.95:6.95), x = results_resources$amce, pch = 15, col="red", cex=1)
segments(y0=c(0.85:6.85),x0 = results_results$cilow, x1 = results_results$ciup,
         col = "darkgreen", lwd = 2)
points(y = c(0.85:6.85), x = results_results$amce, pch = 18, col="darkgreen", cex=1)
legend("bottomleft", c("Communication", "Implementation", "Resource raising", "Performance"), lty=1, pch=c(16,17,15,18), col=c("black", "blue", "red","darkgreen"), bty="n")
dev.off()

# Generate Figure 21-center (conjoint results among respondents who see politicians as programmatic) -----------------------------------------

# Load data, and exclude those who do not see politicians as programmatic
d <- read_csv("../../datasets/analysis/survey_bureaucrats/conjoint_bureaucrats_analysis.csv")
d <- d %>%
  dplyr::filter(sees_politicians_as_programmatic==1)

d$experience <- as.factor(d$experience)
d$education <- as.factor(d$education)
d$selection <- as.factor(d$selection)
d$political_connections <- as.factor(d$political_connections)
d$unit_performance <- as.factor(d$unit_performance)
d$relationship_to_professionals <- as.factor(d$relationship_to_professionals)

d$experience <- relevel(d$experience, "3 years")
d$education <- relevel(d$education, "Bachelors")
d$selection <- relevel(d$selection, "Civil service")
d$political_connections <- relevel(d$political_connections, "No connections")
d$unit_performance <- relevel(d$unit_performance, "Targets not met")
d$relationship_to_professionals <- relevel(d$relationship_to_professionals, "Bad")

### Remove profiles from tasks where no profile was chosen
### In some choice tasks respondents preferred to skip, or were (by lottery) exposed to a pair of identical profiles

d_reform <- subset(d, !is.na(d$chosen_reform))
d_performance <- subset(d, !is.na(d$chosen_performance))
d_implementation <- subset(d, !is.na(d$chosen_implementation))
d_communication <- subset(d, !is.na(d$chosen_communication))

# Run regressions where the DV is the choice in each of the questions, covariates are profile attributes, and standard errors are clustered at the respondent level
m1 <- amce(chosen_reform ~ experience + education + selection + political_connections + unit_performance + relationship_to_professionals,
           data=d_reform, respondent.id = "respondent_id", cluster=T)
m2 <- amce(chosen_performance ~ experience + education + selection + political_connections + unit_performance + relationship_to_professionals,
           data=d_performance, respondent.id = "respondent_id", cluster=T)
m3 <- amce(chosen_implementation ~ experience + education + selection + political_connections + unit_performance + relationship_to_professionals,
           data=d_implementation, respondent.id = "respondent_id", cluster=T)
m4 <- amce(chosen_communication ~ experience + education + selection + political_connections + unit_performance + relationship_to_professionals,
           data=d_communication, respondent.id = "respondent_id", cluster=T)

# Define vector with attributes for plot
attributes <- rev(c("Political appointee \n (vs civil service)", "Elected \n (vs civil service)", "Has political \n connections (vs not)", "Holds Masters degree \n (vs Bachelors)", "Has 10 years of \n experience (vs 3 years)","Unit targets were \n met (vs not)","Has good relationships \n w/ professionals (vs bad)"))

# Create table with results for resources question
critical_value <- qt(.975, df=m1$samplesize_prof-8)
amce <- rev(c(m1$estimates$selection[1,2], m1$estimates$selection[1,1], m1$estimates$politicalconnections[1,1], m1$estimates$education[1,1],m1$estimates$experience[1,1], m1$estimates$unitperformance[1,1], m1$estimates$relationshiptoprofessionals[1,1]))
se <- rev(c(m1$estimates$selection[2,2], m1$estimates$selection[2,1],m1$estimates$politicalconnections[2,1], m1$estimates$education[2,1],m1$estimates$experience[2,1], m1$estimates$unitperformance[2,1], m1$estimates$relationshiptoprofessionals[2,1]))
results_resources <- as_tibble(cbind(attributes, amce, se))
results_resources <- results_resources %>%
  mutate(amce = as.numeric(amce), se = as.numeric(se),
         cilow = amce-critical_value*se, ciup=amce+critical_value*se)

# Create table with results for performance question
critical_value <- qt(.975, df=m2$samplesize_prof-8)
amce <- rev(c(m2$estimates$selection[1,2], m2$estimates$selection[1,1], m2$estimates$politicalconnections[1,1], m2$estimates$education[1,1], m2$estimates$experience[1,1], m2$estimates$unitperformance[1,1], m2$estimates$relationshiptoprofessionals[1,1]))
se <- rev(c(m2$estimates$selection[2,2], m2$estimates$selection[2,1],m2$estimates$politicalconnections[2,1], m2$estimates$education[2,1],m2$estimates$experience[2,1], m2$estimates$unitperformance[2,1], m2$estimates$relationshiptoprofessionals[2,1]))
results_results <- as_tibble(cbind(attributes, amce, se))
results_results <- results_results %>%
  mutate(amce = as.numeric(amce), se = as.numeric(se),
         cilow = amce-critical_value*se, ciup=amce+critical_value*se)

# Create table with results for implementation question
critical_value <- qt(.975, df=m3$samplesize_prof-8)
amce <- rev(c(m3$estimates$selection[1,2], m3$estimates$selection[1,1], m3$estimates$politicalconnections[1,1], m3$estimates$education[1,1], m3$estimates$experience[1,1], m3$estimates$unitperformance[1,1], m3$estimates$relationshiptoprofessionals[1,1]))
se <- rev(c(m3$estimates$selection[2,2], m3$estimates$selection[2,1],m3$estimates$politicalconnections[2,1], m3$estimates$education[2,1],m3$estimates$experience[2,1], m3$estimates$unitperformance[2,1], m3$estimates$relationshiptoprofessionals[2,1]))
results_implementation <- as_tibble(cbind(attributes, amce, se))
results_implementation <- results_implementation %>%
  mutate(amce = as.numeric(amce), se = as.numeric(se),
         cilow = amce-critical_value*se, ciup=amce+critical_value*se)

# Create table with results for communication question
critical_value <- qt(.975, df=m4$samplesize_prof-8)
amce <- rev(c(m4$estimates$selection[1,2], m4$estimates$selection[1,1], m4$estimates$politicalconnections[1,1], m4$estimates$education[1,1], m4$estimates$experience[1,1], m4$estimates$unitperformance[1,1], m4$estimates$relationshiptoprofessionals[1,1]))
se <- rev(c(m4$estimates$selection[2,2], m4$estimates$selection[2,1],m4$estimates$politicalconnections[2,1], m4$estimates$education[2,1],m4$estimates$experience[2,1], m4$estimates$unitperformance[2,1], m4$estimates$relationshiptoprofessionals[2,1]))
results_communication <- as_tibble(cbind(attributes, amce, se))
results_communication <- results_communication %>%
  mutate(amce = as.numeric(amce), se = as.numeric(se),
         cilow = amce-critical_value*se, ciup=amce+critical_value*se)

# Generate plot with AMCE estimates and their confidence intervals
pdf("../../plots/conjoint_bureaucrats_believe_politicians_programmatic.pdf", width=6.5, height=7.65)
par(mar=c(5, 1, 2, 1))
plot(NULL, ylim = c(0.5,7.5), xlim = c(-0.2, 0.25), yaxt = "n",
     xlab = "Average marginal component effect", ylab = "", cex.main = 1,
     main = "Only respondents who see politicians \n as concerned with public service delivery")
grid()
abline(v=0, col="gray", lty=1,lwd=1.5)
axis(2, at=c(1:7),labels=results_resources$attributes, las=2, cex=1.3)
segments(y0=c(1.15:7.15),x0 = results_communication$cilow, x1 = results_communication$ciup,
         col = "black", lwd = 2)
points(y = c(1.15:7.15), x = results_communication$amce, pch = 16, col="black", cex=1)
segments(y0=c(1.05:7.05),x0 = results_implementation$cilow, x1 = results_implementation$ciup,
         col = "blue", lwd = 2)
points(y = c(1.05:7.05), x = results_implementation$amce, pch = 17, col="blue", cex=1)
segments(y0=c(0.95:6.95),x0 = results_resources$cilow, x1 = results_resources$ciup,
         col = "red", lwd = 2)
points(y = c(0.95:6.95), x = results_resources$amce, pch = 15, col="red", cex=1)
segments(y0=c(0.85:6.85),x0 = results_results$cilow, x1 = results_results$ciup,
         col = "darkgreen", lwd = 2)
points(y = c(0.85:6.85), x = results_results$amce, pch = 18, col="darkgreen", cex=1)
legend("bottomleft", c("Communication", "Implementation", "Resource raising", "Performance"), lty=1, pch=c(16,17,15,18), col=c("black", "blue", "red","darkgreen"), bty="n")
dev.off()

# Generate Figure 21-right (conjoint results among respondents who work in a municipality where all respondents are appointed) -----------------------------------------

# Load data, and exclude those who do not see politicians as programmatic
d <- read_csv("../../datasets/analysis/survey_bureaucrats/conjoint_bureaucrats_analysis.csv")
d <- d %>%
  dplyr::filter(municipality_all_appointed==1)

d$experience <- as.factor(d$experience)
d$education <- as.factor(d$education)
d$selection <- as.factor(d$selection)
d$political_connections <- as.factor(d$political_connections)
d$unit_performance <- as.factor(d$unit_performance)
d$relationship_to_professionals <- as.factor(d$relationship_to_professionals)

d$experience <- relevel(d$experience, "3 years")
d$education <- relevel(d$education, "Bachelors")
d$selection <- relevel(d$selection, "Civil service")
d$political_connections <- relevel(d$political_connections, "No connections")
d$unit_performance <- relevel(d$unit_performance, "Targets not met")
d$relationship_to_professionals <- relevel(d$relationship_to_professionals, "Bad")

### Remove profiles from tasks where no profile was chosen
### In some choice tasks respondents preferred to skip, or were (by lottery) exposed to a pair of identical profiles

d_reform <- subset(d, !is.na(d$chosen_reform))
d_performance <- subset(d, !is.na(d$chosen_performance))
d_implementation <- subset(d, !is.na(d$chosen_implementation))
d_communication <- subset(d, !is.na(d$chosen_communication))

# Run regressions where the DV is the choice in each of the questions, covariates are profile attributes, and standard errors are clustered at the respondent level
m1 <- amce(chosen_reform ~ experience + education + selection + political_connections + unit_performance + relationship_to_professionals,
           data=d_reform, respondent.id = "respondent_id", cluster=T)
m2 <- amce(chosen_performance ~ experience + education + selection + political_connections + unit_performance + relationship_to_professionals,
           data=d_performance, respondent.id = "respondent_id", cluster=T)
m3 <- amce(chosen_implementation ~ experience + education + selection + political_connections + unit_performance + relationship_to_professionals,
           data=d_implementation, respondent.id = "respondent_id", cluster=T)
m4 <- amce(chosen_communication ~ experience + education + selection + political_connections + unit_performance + relationship_to_professionals,
           data=d_communication, respondent.id = "respondent_id", cluster=T)

# Define vector with attributes for plot
attributes <- rev(c("Political appointee \n (vs civil service)", "Elected \n (vs civil service)", "Has political \n connections (vs not)", "Holds Masters degree \n (vs Bachelors)", "Has 10 years of \n experience (vs 3 years)","Unit targets were \n met (vs not)","Has good relationships \n w/ professionals (vs bad)"))

# Create table with results for resources question
critical_value <- qt(.975, df=m1$samplesize_prof-8)
amce <- rev(c(m1$estimates$selection[1,2], m1$estimates$selection[1,1], m1$estimates$politicalconnections[1,1], m1$estimates$education[1,1],m1$estimates$experience[1,1], m1$estimates$unitperformance[1,1], m1$estimates$relationshiptoprofessionals[1,1]))
se <- rev(c(m1$estimates$selection[2,2], m1$estimates$selection[2,1],m1$estimates$politicalconnections[2,1], m1$estimates$education[2,1],m1$estimates$experience[2,1], m1$estimates$unitperformance[2,1], m1$estimates$relationshiptoprofessionals[2,1]))
results_resources <- as_tibble(cbind(attributes, amce, se))
results_resources <- results_resources %>%
  mutate(amce = as.numeric(amce), se = as.numeric(se),
         cilow = amce-critical_value*se, ciup=amce+critical_value*se)

# Create table with results for performance question
critical_value <- qt(.975, df=m2$samplesize_prof-8)
amce <- rev(c(m2$estimates$selection[1,2], m2$estimates$selection[1,1], m2$estimates$politicalconnections[1,1], m2$estimates$education[1,1], m2$estimates$experience[1,1], m2$estimates$unitperformance[1,1], m2$estimates$relationshiptoprofessionals[1,1]))
se <- rev(c(m2$estimates$selection[2,2], m2$estimates$selection[2,1],m2$estimates$politicalconnections[2,1], m2$estimates$education[2,1],m2$estimates$experience[2,1], m2$estimates$unitperformance[2,1], m2$estimates$relationshiptoprofessionals[2,1]))
results_results <- as_tibble(cbind(attributes, amce, se))
results_results <- results_results %>%
  mutate(amce = as.numeric(amce), se = as.numeric(se),
         cilow = amce-critical_value*se, ciup=amce+critical_value*se)

# Create table with results for implementation question
critical_value <- qt(.975, df=m3$samplesize_prof-8)
amce <- rev(c(m3$estimates$selection[1,2], m3$estimates$selection[1,1], m3$estimates$politicalconnections[1,1], m3$estimates$education[1,1], m3$estimates$experience[1,1], m3$estimates$unitperformance[1,1], m3$estimates$relationshiptoprofessionals[1,1]))
se <- rev(c(m3$estimates$selection[2,2], m3$estimates$selection[2,1],m3$estimates$politicalconnections[2,1], m3$estimates$education[2,1],m3$estimates$experience[2,1], m3$estimates$unitperformance[2,1], m3$estimates$relationshiptoprofessionals[2,1]))
results_implementation <- as_tibble(cbind(attributes, amce, se))
results_implementation <- results_implementation %>%
  mutate(amce = as.numeric(amce), se = as.numeric(se),
         cilow = amce-critical_value*se, ciup=amce+critical_value*se)

# Create table with results for communication question
critical_value <- qt(.975, df=m4$samplesize_prof-8)
amce <- rev(c(m4$estimates$selection[1,2], m4$estimates$selection[1,1], m4$estimates$politicalconnections[1,1], m4$estimates$education[1,1], m4$estimates$experience[1,1], m4$estimates$unitperformance[1,1], m4$estimates$relationshiptoprofessionals[1,1]))
se <- rev(c(m4$estimates$selection[2,2], m4$estimates$selection[2,1],m4$estimates$politicalconnections[2,1], m4$estimates$education[2,1],m4$estimates$experience[2,1], m4$estimates$unitperformance[2,1], m4$estimates$relationshiptoprofessionals[2,1]))
results_communication <- as_tibble(cbind(attributes, amce, se))
results_communication <- results_communication %>%
  mutate(amce = as.numeric(amce), se = as.numeric(se),
         cilow = amce-critical_value*se, ciup=amce+critical_value*se)

# Generate plot with AMCE estimates and their confidence intervals
pdf("../../plots/conjoint_bureaucrats_muns_all_appointed.pdf", width=6.5, height=7.6)
par(mar=c(5, 1, 2, 1))
plot(NULL, ylim = c(0.5,7.5), xlim = c(-0.2, 0.25), yaxt = "n",
     xlab = "Average marginal component effect", ylab = "", cex.main = 1,
     main = "Only respondents in municipalities where \n all respondents are politically appointed")
grid()
abline(v=0, col="gray", lty=1,lwd=1.5)
axis(2, at=c(1:7),labels=results_resources$attributes, las=2, cex=1.3)
segments(y0=c(1.15:7.15),x0 = results_communication$cilow, x1 = results_communication$ciup,
         col = "black", lwd = 2)
points(y = c(1.15:7.15), x = results_communication$amce, pch = 16, col="black", cex=1)
segments(y0=c(1.05:7.05),x0 = results_implementation$cilow, x1 = results_implementation$ciup,
         col = "blue", lwd = 2)
points(y = c(1.05:7.05), x = results_implementation$amce, pch = 17, col="blue", cex=1)
segments(y0=c(0.95:6.95),x0 = results_resources$cilow, x1 = results_resources$ciup,
         col = "red", lwd = 2)
points(y = c(0.95:6.95), x = results_resources$amce, pch = 15, col="red", cex=1)
segments(y0=c(0.85:6.85),x0 = results_results$cilow, x1 = results_results$ciup,
         col = "darkgreen", lwd = 2)
points(y = c(0.85:6.85), x = results_results$amce, pch = 18, col="darkgreen", cex=1)
legend("bottomleft", c("Communication", "Implementation", "Resource raising", "Performance"), lty=1, pch=c(16,17,15,18), col=c("black", "blue", "red","darkgreen"), bty="n")
dev.off()

# NOTES -- R version, platform, and loaded packages -------------------------

### sessionInfo(package = NULL)

# R version 3.6.3 (2020-02-29)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS  10.16
# 
# Matrix products: default
# LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# attached base packages:
#   [1] grid      stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] xtable_1.8-4    cjoint_2.1.0    survey_4.0      survival_3.1-11 Matrix_1.2-18   lmtest_0.9-37  
# [7] zoo_1.8-7       sandwich_2.5-1  texreg_1.37.1   codebook_0.9.2  readxl_1.3.1    here_1.0.1     
# [13] forcats_0.5.0   stringr_1.4.0   dplyr_1.0.0     purrr_0.3.3     readr_1.4.0     tidyr_1.0.2    
# [19] tibble_3.0.0    ggplot2_3.3.5   tidyverse_1.3.0
# 
# loaded via a namespace (and not attached):
#   [1] nlme_3.1-145      fs_1.4.1          lubridate_1.7.4   webshot_0.5.2     httr_1.4.1        rprojroot_2.0.2  
# [7] repr_1.1.0        tools_3.6.3       backports_1.1.5   utf8_1.1.4        R6_2.4.1          DT_0.13          
# [13] DBI_1.1.0         colorspace_1.4-1  withr_2.1.2       tidyselect_1.1.0  compiler_3.6.3    cli_2.3.0        
# [19] rvest_0.3.5       xml2_1.3.0        desc_1.2.0        labeling_0.3      scales_1.1.0      digest_0.6.25    
# [25] rmarkdown_2.6     base64enc_0.1-3   pkgconfig_2.0.3   htmltools_0.4.0   labelled_2.5.0    fastmap_1.0.1    
# [31] dbplyr_1.4.2      highr_0.8         htmlwidgets_1.5.1 rlang_1.0.0       rstudioapi_0.11   shiny_1.4.0.2    
# [37] farver_2.0.3      generics_0.0.2    jsonlite_1.6.1    crosstalk_1.1.0.1 magrittr_1.5      Rcpp_1.0.7       
# [43] munsell_0.5.0     fansi_0.4.1       lifecycle_0.2.0   stringi_1.4.6     yaml_2.2.1        rmdpartials_0.5.8
# [49] plyr_1.8.6        parallel_3.6.3    listenv_0.8.0     promises_1.1.0    crayon_1.3.4      lattice_0.20-40  
# [55] haven_2.3.1       splines_3.6.3     hms_0.5.3         knitr_1.28        pillar_1.4.3      pkgload_1.0.2    
# [61] codetools_0.2-16  reprex_0.3.0      glue_1.3.2        evaluate_0.14     mitools_2.4       modelr_0.1.6     
# [67] vctrs_0.3.1       httpuv_1.5.2      testthat_2.3.2    cellranger_1.1.0  gtable_0.3.0      future_1.17.0    
# [73] assertthat_0.2.1  xfun_0.20         mime_0.9          skimr_2.1.3       broom_0.5.5       later_1.0.0      
# [79] rsconnect_0.8.16  globals_0.12.5    ellipsis_0.3.0   