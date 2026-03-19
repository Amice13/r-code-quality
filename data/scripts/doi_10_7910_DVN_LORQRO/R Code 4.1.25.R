########################################################################
## Inactivating Lipotoxic Ceramides to Treat Cardiomyopathy in LCFAOD ##
## Descriptive stats/Logistic Regression                              ##
## Marie K. Norris                                                    ##
## April 1st, 2025                                                  ##
########################################################################

########################################################################
## Code and Software Submission Checklist                             ##
## Software: RStudio 2024.12.0+467                                    ##
## Code has also been tested on:RStudio 2024.09.0+375                 ##
## Installation Guide: https://posit.co/downloads/.                   ##
      #See website for install time#                                  ##
## Instructions to run on data: see below                             ##
## Data expected output: see below                                    ##
## Data expected run time: immediate                                  ##          
########################################################################

library("knitr")
library("tidyverse") 
library("broom")
library("readxl")
library("ggplot2")
library("ggsignif")
library("epiDisplay")
library("epitools")
library("pROC")
library("pscl")
library("MASS")
library("ppcor")


#### Load Mayo Clinic Data#### 
library(readxl)
Mayo_Total_Data_for_Correlations_ <- read_excel("Mayo Total Data for Correlations  .xlsx")
View(Mayo_Total_Data_for_Correlations_)

## MAYO create column that indicates group (cases or controls)
Mayo_Total_Data_for_Correlations_ <- Mayo_Total_Data_for_Correlations_ %>%
  mutate(Group = case_when(
    ID %in% c("Control 1", "Control 2", "Control 3", "Control 4", "Control 5", "Control 6", 
              "Control 7", "Control 8", "Control 9", "Control 10", "Control 11", "Control 12", "Control 13", "Control 14", "Control 15",
              "Control 16", "Control 17", "Control 18", "Control 19", "Control 22", "Control 23", "Control 24", "Control 26", "Control 27", 
              "Control 29", "Control 30", "Control 31", "Control 34", "Control 35", "Control 36") ~ "control",
    ID %in% c("VLCAD1", "VLCAD2", "VLCAD3", "VLCAD4", "VLCAD5", "VLCAD6", 
              "VLCAD7", "VLCAD8", "VLCAD9", "VLCAD10", "VLCAD11", "VLCAD12", "VLCAD13", "VLCAD14", "VLCAD15", "VLCAD 17", "VLCAD 18") ~ "VLCAD",
    ID %in% c("TFP/LCHAD2", "TFP/LCHAD3", "TFP/LCHAD4", "TFP/LCHAD5", "TFP/LCHAD6", "TFP/LCHAD7", "TFP/LCHAD8", "TFP/LCHAD10") ~ "TFP/LCHAD",
    ID %in% c("CPT2-4", "CPT2-5", "CPT2-6", "CPT2-7") ~ "CPT-2",
    
    TRUE ~ NA_character_
  ))

glimpse(Mayo_Total_Data_for_Correlations_)

###########################################################################
### ODDS Ratios (OR) and Forest Plots#####
###########################################################################
##CERT1 OR and Forest Plot 
Mayo_Total_Data_for_Correlations_ <- Mayo_Total_Data_for_Correlations_ %>%
  mutate(CERT1_tertile = ntile(CERT1, 3))
Mayo_Total_Data_for_Correlations_$CERT1_tertile <- as.factor(Mayo_Total_Data_for_Correlations_$CERT1_tertile)

table(Mayo_Total_Data_for_Correlations_$CERT1_tertile)

log_model11 <- glm(`Numeric ID` ~ CERT1_tertile  , data = Mayo_Total_Data_for_Correlations_, family = binomial)
summary(log_model11)
summary_log_model11 <- summary(log_model11) 

# Extract coefficient estimates, standard errors, and p-values
estimates <- summary_log_model11$coefficients[, "Estimate"]
std_errors <- summary_log_model11$coefficients[, "Std. Error"]
p_values <- summary_log_model11$coefficients[, "Pr(>|z|)"]

# Calculate odds ratios and confidence intervals
odds_ratios <- exp(estimates)
lower_ci <- exp(estimates - 1.96 * std_errors)
upper_ci <- exp(estimates + 1.96 * std_errors)

# Combine everything into a data frame
results <- data.frame(Odds_Ratio = odds_ratios,
                      Lower_CI = lower_ci,
                      Upper_CI = upper_ci,
                      p_values = p_values)

# Print or use results as needed
print(results)

#Odds_Ratio  Lower_CI   Upper_CI    p_values
#(Intercept)     0.3333333 0.1211469  0.9171598 0.033382265
#CERT1_tertile2  2.7000000 0.6966108 10.4649540 0.150727568
#CERT1_tertile3 11.2500000 2.5178222 50.2666542 0.001529603

##################################################################
##CK OR and Forest Plot 
Mayo_Total_Data_for_Correlations_ <- Mayo_Total_Data_for_Correlations_ %>%
  mutate(CK_tertile = ntile(CK, 3))
Mayo_Total_Data_for_Correlations_$CK_tertile <- as.factor(Mayo_Total_Data_for_Correlations_$CK_tertile)

table(Mayo_Total_Data_for_Correlations_$CK_tertile)

log_model12 <- glm(`Numeric ID` ~ CK_tertile  , data = Mayo_Total_Data_for_Correlations_, family = binomial)
summary(log_model12)
summary_log_model12 <- summary(log_model12) 

# Extract coefficient estimates, standard errors, and p-values
estimates <- summary_log_model12$coefficients[, "Estimate"]
std_errors <- summary_log_model12$coefficients[, "Std. Error"]
p_values <- summary_log_model12$coefficients[, "Pr(>|z|)"]

# Calculate odds ratios and confidence intervals
odds_ratios <- exp(estimates)
lower_ci <- exp(estimates - 1.96 * std_errors)
upper_ci <- exp(estimates + 1.96 * std_errors)

# Combine everything into a data frame
results <- data.frame(Odds_Ratio = odds_ratios,
                      Lower_CI = lower_ci,
                      Upper_CI = upper_ci,
                      p_values = p_values)

# Print or use results as needed
print(results)

###Odds_Ratio   Lower_CI   Upper_CI     p_values
##(Intercept)   0.250000 0.08357861   0.747799 0.0131427069
#CK_tertile2   2.333333 0.55363223   9.834045 0.2483251669
#CK_tertile3  72.000000 7.27346473 712.727726 0.0002556898

##################################################################
##Nt-proBNP OR and Forest Plot 
# Make sure the column is numeric
Mayo_Total_Data_for_Correlations_$`NT-proBNP (pg/mL)` <- as.numeric(Mayo_Total_Data_for_Correlations_$`NT-proBNP (pg/mL)`)

# Create tertiles
Mayo_Total_Data_for_Correlations_ <- Mayo_Total_Data_for_Correlations_ %>%
  mutate(BNP_tertile = ntile(`NT-proBNP (pg/mL)`, 2))

# Convert tertiles to a factor
Mayo_Total_Data_for_Correlations_$BNP_tertile <- as.factor(Mayo_Total_Data_for_Correlations_$BNP_tertile)

# Check the distribution of the tertiles
table(Mayo_Total_Data_for_Correlations_$BNP_tertile)

# Fit the logistic regression model
log_model13 <- glm(`Numeric ID` ~ BNP_tertile, data = Mayo_Total_Data_for_Correlations_, family = binomial)
summary(log_model13)

# Extract the summary
summary_log_model13 <- summary(log_model13) 

# Extract coefficient estimates, standard errors, and p-values
estimates <- summary_log_model13$coefficients[, "Estimate"]
std_errors <- summary_log_model13$coefficients[, "Std. Error"]
p_values <- summary_log_model13$coefficients[, "Pr(>|z|)"]

# Calculate odds ratios and confidence intervals
odds_ratios <- exp(estimates)
lower_ci <- exp(estimates - 1.96 * std_errors)
upper_ci <- exp(estimates + 1.96 * std_errors)

# Combine everything into a data frame
results <- data.frame(Odds_Ratio = odds_ratios,
                      Lower_CI = lower_ci,
                      Upper_CI = upper_ci,
                      p_values = p_values)

# Print or use results as needed
print(results)

##Odds_Ratio  Lower_CI  Upper_CI   p_values
#(Intercept)   0.4444444 0.1932502  1.022151 0.05633453
#BNP_tertile2  3.3750000 1.0633821 10.711695 0.03899006

##################################################################
##C14:1 AC OR and Forest Plot
Mayo_Total_Data_for_Correlations_ <- Mayo_Total_Data_for_Correlations_ %>%
  mutate(C14_1_AC_tertile = ntile(`C14_ 1_AC`, 3))
Mayo_Total_Data_for_Correlations_$C14_1_AC_tertile <- as.factor(Mayo_Total_Data_for_Correlations_$C14_1_AC_tertile)

table(Mayo_Total_Data_for_Correlations_$C14_1_AC_tertile)

log_model14 <- glm(`Numeric ID` ~ C14_1_AC_tertile  , data = Mayo_Total_Data_for_Correlations_, family = binomial)
summary(log_model14)
summary_log_model14 <- summary(log_model4) 

# Extract coefficient estimates, standard errors, and p-values
estimates <- summary_log_model14$coefficients[, "Estimate"]
std_errors <- summary_log_model14$coefficients[, "Std. Error"]
p_values <- summary_log_model14$coefficients[, "Pr(>|z|)"]

# Calculate odds ratios and confidence intervals
odds_ratios <- exp(estimates)
lower_ci <- exp(estimates - 1.96 * std_errors)
upper_ci <- exp(estimates + 1.96 * std_errors)

# Combine everything into a data frame
results <- data.frame(Odds_Ratio = odds_ratios,
                      Lower_CI = lower_ci,
                      Upper_CI = upper_ci,
                      p_values = p_values)

# Print or use results as needed
print(results)

#Odds_Ratio  Lower_CI  Upper_CI   p_values
#(Intercept)        0.4285714 0.1646918  1.115256 0.08248523
#C14_1_AC_tertile2  1.3611111 0.3580033  5.174879 0.65093560
#C14_1_AC_tertile3 12.4444444 2.6136032 59.253141 0.00154206


##################################################################
##C18:1OH AC OR and Forest Plot
Mayo_Total_Data_for_Correlations_ <- Mayo_Total_Data_for_Correlations_ %>%
  mutate(C18_1OH_AC_tertile = ntile(`C18_ 1OH_AC`, 3))
Mayo_Total_Data_for_Correlations_$C18_1OH_AC_tertile <- as.factor(Mayo_Total_Data_for_Correlations_$C18_1OH_AC_tertile)

table(Mayo_Total_Data_for_Correlations_$C18_1OH_AC_tertile)

log_model15 <- glm(`Numeric ID` ~ C18_1OH_AC_tertile  , data = Mayo_Total_Data_for_Correlations_, family = binomial)
summary(log_model15)
summary_log_model15 <- summary(log_model15) 

# Extract coefficient estimates, standard errors, and p-values
estimates <- summary_log_model15$coefficients[, "Estimate"]
std_errors <- summary_log_model15$coefficients[, "Std. Error"]
p_values <- summary_log_model15$coefficients[, "Pr(>|z|)"]

# Calculate odds ratios and confidence intervals
odds_ratios <- exp(estimates)
lower_ci <- exp(estimates - 1.96 * std_errors)
upper_ci <- exp(estimates + 1.96 * std_errors)

# Combine everything into a data frame
results <- data.frame(Odds_Ratio = odds_ratios,
                      Lower_CI = lower_ci,
                      Upper_CI = upper_ci,
                      p_values = p_values)

# Print or use results as needed
print(results)

#Odds_Ratio  Lower_CI Upper_CI  p_values
#(Intercept)           0.800000 0.3157346 2.027019 0.6380493
#C18_1OH_AC_tertile2   0.625000 0.1618897 2.412909 0.4952687
#C18_1OH_AC_tertile3   1.964286 0.5207818 7.408896 0.3188808

##################################################################
##C18:1 AC OR and Forest Plot
Mayo_Total_Data_for_Correlations_ <- Mayo_Total_Data_for_Correlations_ %>%
  mutate(C18_1_AC_tertile = ntile(`C18_ 1_AC`, 3))  # Fixing the variable name

# Convert to factor
Mayo_Total_Data_for_Correlations_$C18_1_AC_tertile <- as.factor(Mayo_Total_Data_for_Correlations_$C18_1_AC_tertile)

# Display the distribution of tertiles
table(Mayo_Total_Data_for_Correlations_$C18_1_AC_tertile)

# Fit logistic regression model
log_model16 <- glm(`Numeric ID` ~ C18_1_AC_tertile, data = Mayo_Total_Data_for_Correlations_, family = binomial)
summary(log_model16)
summary_log_model16 <- summary(log_model16) ##0.00431

# Extract coefficient estimates, standard errors, and p-values
estimates <- summary_log_model16$coefficients[, "Estimate"]
std_errors <- summary_log_model16$coefficients[, "Std. Error"]
p_values <- summary_log_model16$coefficients[, "Pr(>|z|)"]

# Calculate odds ratios and confidence intervals
odds_ratios <- exp(estimates)
lower_ci <- exp(estimates - 1.96 * std_errors)
upper_ci <- exp(estimates + 1.96 * std_errors)

# Combine everything into a data frame
results <- data.frame(Odds_Ratio = odds_ratios,
                      Lower_CI = lower_ci,
                      Upper_CI = upper_ci,
                      p_values = p_values)

# Print or use results as needed
print(results)

#Odds_Ratio  Lower_CI  Upper_CI    p_values
#(Intercept)        0.3846154 0.1371156  1.078863 0.069408359
#C18_1_AC_tertile2  1.3000000 0.3133644  5.393083 0.717773623
#C18_1_AC_tertile3  9.1000000 1.9980123 41.446191 0.004306448

##################################################################
# DG OR and Forest Plot
Mayo_Total_Data_for_Correlations_ <- Mayo_Total_Data_for_Correlations_ %>%
  mutate(DG_total_tertile = ntile(`DG Total`, 3))  # Fixing the variable name

# Convert to factor
Mayo_Total_Data_for_Correlations_$DG_total_tertile <- as.factor(Mayo_Total_Data_for_Correlations_$DG_total_tertile)

# Display the distribution of tertiles
table(Mayo_Total_Data_for_Correlations_$DG_total_tertile)

# Fit logistic regression model
log_model17 <- glm(`Numeric ID` ~ DG_total_tertile, data = Mayo_Total_Data_for_Correlations_, family = binomial)
summary(log_model17)
summary_log_model17 <- summary(log_model17)

# Extract coefficient estimates, standard errors, and p-values
estimates <- summary_log_model17$coefficients[, "Estimate"]
std_errors <- summary_log_model17$coefficients[, "Std. Error"]
p_values <- summary_log_model17$coefficients[, "Pr(>|z|)"]

# Calculate odds ratios and confidence intervals
odds_ratios <- exp(estimates)
lower_ci <- exp(estimates - 1.96 * std_errors)
upper_ci <- exp(estimates + 1.96 * std_errors)

# Combine everything into a data frame
results <- data.frame(Odds_Ratio = odds_ratios,
                      Lower_CI = lower_ci,
                      Upper_CI = upper_ci,
                      p_values = p_values)

# Print or use results as needed
print(results)

#Odds_Ratio  Lower_CI Upper_CI  p_values
#(Intercept)        1.2500000 0.4933354 3.167217 0.6380493
#DG_total_tertile2  0.6400000 0.1718566 2.383383 0.5058679
#DG_total_tertile3  0.5090909 0.1349729 1.920190 0.3188808

##################################################################
#  TG OR and Forest Plot
Mayo_Total_Data_for_Correlations_ <- Mayo_Total_Data_for_Correlations_ %>%
  mutate(TG_total_tertile = ntile(`TG Total`, 3))  # Fixing the variable name

# Convert to factor
Mayo_Total_Data_for_Correlations_$TG_total_tertile <- as.factor(Mayo_Total_Data_for_Correlations_$TG_total_tertile)

# Display the distribution of tertiles
table(Mayo_Total_Data_for_Correlations_$TG_total_tertile)

# Fit logistic regression model
log_model18 <- glm(`Numeric ID` ~ TG_total_tertile, data = Mayo_Total_Data_for_Correlations_, family = binomial)
summary(log_model18)
summary_log_model18 <- summary(log_model18). 

# Extract coefficient estimates, standard errors, and p-values
estimates <- summary_log_model18$coefficients[, "Estimate"]
std_errors <- summary_log_model18$coefficients[, "Std. Error"]
p_values <- summary_log_model18$coefficients[, "Pr(>|z|)"]

# Calculate odds ratios and confidence intervals
odds_ratios <- exp(estimates)
lower_ci <- exp(estimates - 1.96 * std_errors)
upper_ci <- exp(estimates + 1.96 * std_errors)

# Combine everything into a data frame
results <- data.frame(Odds_Ratio = odds_ratios,
                      Lower_CI = lower_ci,
                      Upper_CI = upper_ci,
                      p_values = p_values)

# Print or use results as needed
print(results)

#Odds_Ratio  Lower_CI Upper_CI  p_values
#(Intercept)         0.800000 0.3157346 2.027019 0.6380493
#TG_total_tertile2   0.625000 0.1618897 2.412909 0.4952687
#TG_total_tertile3   1.964286 0.5207818 7.408896 0.3188808

##################################################################
# Fit logistic regression model PC
Mayo_Total_Data_for_Correlations_ <- Mayo_Total_Data_for_Correlations_ %>%
  mutate(PC_total_tertile = ntile(`PC Total`, 3))  # Fixing the variable name

# Convert to factor
Mayo_Total_Data_for_Correlations_$PC_total_tertile <- as.factor(Mayo_Total_Data_for_Correlations_$PC_total_tertile)

# Display the distribution of tertiles
table(Mayo_Total_Data_for_Correlations_$PC_total_tertile)

# Fit logistic regression model
log_model19 <- glm(`Numeric ID` ~ PC_total_tertile, data = Mayo_Total_Data_for_Correlations_, family = binomial)
summary(log_model19)
summary_log_model19 <- summary(log_model19) 

# Extract coefficient estimates, standard errors, and p-values
estimates <- summary_log_model19$coefficients[, "Estimate"]
std_errors <- summary_log_model19$coefficients[, "Std. Error"]
p_values <- summary_log_model19$coefficients[, "Pr(>|z|)"]

# Calculate odds ratios and confidence intervals
odds_ratios <- exp(estimates)
lower_ci <- exp(estimates - 1.96 * std_errors)
upper_ci <- exp(estimates + 1.96 * std_errors)

# Combine everything into a data frame
results <- data.frame(Odds_Ratio = odds_ratios,
                      Lower_CI = lower_ci,
                      Upper_CI = upper_ci,
                      p_values = p_values)

# Print or use results as needed
print(results)

# Odds_Ratio  Lower_CI Upper_CI  p_values
#(Intercept)              0.5 0.1876556 1.332228 0.1656570
#PC_total_tertile2        2.0 0.5201002 7.690826 0.3131294
#PC_total_tertile3        2.5 0.6475586 9.651635 0.1836849

