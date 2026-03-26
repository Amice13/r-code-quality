###############################################################################/
## REPLICATION MATERIAL: Behaviourally informed interventions can          ####/
## increase take-up of public employment services, but conversion remains  ####/
## challenging. Insights from an RCT in British Columbia, Canada           ####/
## By: Schimpf et al. 2025                                                 ####/
###############################################################################/

#############################################################################//
#### /// README ####
#############################################################################//

# This RScript contains all the relevant code to replicate the analysis in the 
# paper, including the appendix. For variable specific codes, see the appendix
# or pre-registration.
#
# PRIOR TO RUNNING THIS SCRIPT: Adjust the working directory in the section 
# "Working Directory." The directory should include the replication dataset and
# two subfolders, one titled 'tables' and one titled 'figures'.

#############################################################################//
#### /// Dataset Overview ####
#############################################################################//

# the dataset contains the following variables:

# ID = respondent ID (anonymous)
# condition = treatment condition
# enrollment_prereminder = WorkBC Enrollment before reminder emails (PRIMARY OUTCOME VARIABLE)
# enrollment= WorkBC Enrollment after 30 days 
# email_open_pre_adj = email opens 
# click_through_prer_adj = email click through
# click_through_cat_prer_adj = click through with explicit 'no' as separate category
# sf_submission_prer = short form submissions
# reminder_email_dummy = reminder email
# age=age
# gender_female=gender 
# education=eduction
# ei_application_lapsed=time since EI application
# ei_application_status = EI application status
# trf_match_prior=Prior TRF engagement 
# enrollment_pretrial=WorkBC pre-trial enrolment
# enrollment_10= WorkBC Enrollment after 10 days 
# enrollment_20= WorkBC Enrollment after 20 days 
# enrollment_40= WorkBC Enrollment after 40 days 
# enrollment_50= WorkBC Enrollment after 50 days 
# enrollment_60= WorkBC Enrollment after 60 days 
# enrollment_70= WorkBC Enrollment after 70 days 
# enrollment_80= WorkBC Enrollment after 80 days 
# enrollment_90= WorkBC Enrollment after 90 days

# for more details (e.g., on variable values), see SMII.7 in the Appendix

#############################################################################//
#### /// Load packages ####
#############################################################################//

if (!require("pacman")) install.packages("pacman")
pacman::p_load(ids, dplyr, foreign, randomizr, 
               stringr, tidyverse, rstatix,
               broom, estimatr, emmeans, grf,
               lubridate, gridExtra, ggdist,
               scales, ggpubr, policytree, caret,
               lemon, plotly, sjPlot, sjmisc, 
               sjlabelled, nnet, lmtest)

#############################################################################//
#### //// Working Directory ####
#############################################################################//

# set temporary working directory - general (folder must contain dataset)
temp_dir <- "[ADJUST]/"

# set temporary working directory - tables
temp_dir_tables <- "[ADJUST]/tables/"
# set temporary working directory - Figures
temp_dir_figures <- "[ADJUST]/figures/"

#############################################################################//
#### /// Read dataset in ####
#############################################################################//

obj_name <- load(paste(temp_dir, "bpp_replication_data0825.Rdata", sep=""))
df_final <- get(obj_name)
rm(list = obj_name)  

#############################################################################//
#### /// Descriptive Statistics ####
#############################################################################//

#### <<>> Summary Statistics (Table A3.1, Appendix) ####

#### summarize variables 
table_summary_stats <- df_final %>% 
  select(condition,
         # Enrollment variables
         enrollment, #WorkBC Enrollment after 30 days 
         enrollment_prereminder, #WorkBC Enrollment before reminder emails 
         #email opens - before reminder emails 
         email_open_pre_adj, 
         # click through - before reminder emails 
         click_through_prer_adj,
         # Short from submissions - before reminder emails 
         sf_submission_prer,
         # Reminder email
         reminder_email_dummy,
         # Additional Covariates
         age, gender_female, 
         education, ei_application_lapsed,
         ei_application_status,
         trf_match_prior,
         enrollment_pretrial) %>%
  get_summary_stats(.,
                    type = "common") 

#Save as table:
table_summary_stats <- as.data.frame(table_summary_stats)
write.csv(table_summary_stats, paste(temp_dir_tables, "table_summary_stats_final.csv", sep=""), row.names=FALSE)


#### <<>> Outcome variables by condition ####
#Note: Used for Figure 3 and Figure A2.1
table_outcome_by_condition <- df_final %>%
  group_by(condition) %>%
  summarize(n= n(),
            #webtracking
            email_open_avg = mean(email_open_pre_adj, na.rm=T),
            email_open_sd = sd(email_open_pre_adj, na.rm=T),
            email_open_se = email_open_sd/sqrt(n),
            click_through_avg = mean(click_through_prer_adj, na.rm=T),
            click_through_sd = sd(click_through_prer_adj, na.rm=T),
            click_through_se = click_through_sd/sqrt(n),
            shortform_submission_avg = mean(sf_submission_prer, na.rm=T),
            shortform_submission_sd = sd(sf_submission_prer, na.rm=T),
            shortform_submission_se = shortform_submission_sd/sqrt(n),
            #enrollment
            enrollment_avg = mean(enrollment_prereminder, na.rm=T),
            enrollment_sd = sd(enrollment_prereminder, na.rm=T),
            enrollment_se = enrollment_sd/sqrt(n),
            enrollment_avg_10 = mean(enrollment_10, na.rm=T),
            enrollment_sd_10 = sd(enrollment_10, na.rm=T),
            enrollment_se_10 = enrollment_sd_10/sqrt(n),
            enrollment_avg_20 = mean(enrollment_20, na.rm=T),
            enrollment_sd_20 = sd(enrollment_20, na.rm=T),
            enrollment_se_20 = enrollment_sd_20/sqrt(n),
            enrollment_avg_30 = mean(enrollment, na.rm=T),
            enrollment_sd_30 = sd(enrollment, na.rm=T),
            enrollment_se_30 = enrollment_sd_30/sqrt(n),   
            enrollment_avg_40 = mean(enrollment_40, na.rm=T),
            enrollment_sd_40 = sd(enrollment_40, na.rm=T),
            enrollment_se_40 = enrollment_sd_40/sqrt(n),
            enrollment_avg_50 = mean(enrollment_50, na.rm=T),
            enrollment_sd_50 = sd(enrollment_50, na.rm=T),
            enrollment_se_50 = enrollment_sd_50/sqrt(n),
            enrollment_avg_60 = mean(enrollment_60, na.rm=T),
            enrollment_sd_60 = sd(enrollment_60, na.rm=T),
            enrollment_se_60 = enrollment_sd_60/sqrt(n),
            enrollment_avg_70 = mean(enrollment_70, na.rm=T),
            enrollment_sd_70 = sd(enrollment_70, na.rm=T),
            enrollment_se_70 = enrollment_sd_70/sqrt(n),
            enrollment_avg_80 = mean(enrollment_80, na.rm=T),
            enrollment_sd_80 = sd(enrollment_80, na.rm=T),
            enrollment_se_80 = enrollment_sd_80/sqrt(n),
            enrollment_avg_90 = mean(enrollment_90, na.rm=T),
            enrollment_sd_90 = sd(enrollment_90, na.rm=T),
            enrollment_se_90 = enrollment_sd_90/sqrt(n))
table_outcome_by_condition <- as.data.frame(table_outcome_by_condition)

#############################################################################//
#### /// Estimate Regression Models (TABLE 1; APPENDIX MODELS) ####
#############################################################################//

#### <<<>>> M1 - adjust for pre-treatment enrollment + use pre reminder DV (not in PAP) ####

# This is the model in TABLE 1 and the basis for FIGURE 2. This is also
# M1 in Table A3.2 in Supplementary Material SMIII.2.

## uses the adjusted DV for analysis and adjusts for pre-treatment enrollment
reg_m1 <- df_final %>%
  ## Generate centered pre-treatment covariates
  mutate(education_m = education - mean(education),
         gender_female_m = gender_female - mean(gender_female),
         ei_application_lapsed_m = ei_application_lapsed - mean(ei_application_lapsed),
         ei_application_status_m = ei_application_status - mean(ei_application_status),
         trf_match_prior_m = trf_match_prior - mean(trf_match_prior),
         age_m = age - mean(age),
         enrollment_pretrial_m = enrollment_pretrial - mean(enrollment_pretrial)) %>%
  ## Lin Estimator
  lm_robust(enrollment_prereminder ~ condition + 
              condition*(education_m + 
                           gender_female_m +
                           age_m + 
                           ei_application_lapsed_m +
                           ei_application_status_m + 
                           trf_match_prior_m + 
                           enrollment_pretrial_m
              ),
            data=., se_type="HC1")

#### <<<>>> M2 -use pre reminder DV (not in PAP) - enrollment at 14 day mark ####

# This is M2 in Table A3.2 in Supplementary Material SMIII.2

## uses the adjusted DV for analysis
reg_m2 <- df_final %>%
  ## Generate centered pre-treatment covariates
  mutate(education_m = education - mean(education),
         gender_female_m = gender_female - mean(gender_female),
         ei_application_lapsed_m = ei_application_lapsed - mean(ei_application_lapsed),
         ei_application_status_m = ei_application_status - mean(ei_application_status),
         trf_match_prior_m = trf_match_prior - mean(trf_match_prior),
         age_m = age - mean(age)) %>%
  ## Lin Estimator
  lm_robust(enrollment_prereminder ~ condition + 
              condition*(education_m + 
                           gender_female_m +
                           age_m + 
                           ei_application_lapsed_m +
                           ei_application_status_m + 
                           trf_match_prior_m
              ),
            data=., se_type="HC1")

#### <<<>>> M4 - Pre-treatment covariated adjusted main model - enrollment at 30 day mark ####

# This is M4 in Table A3.2 in Supplementary Material SMIII.2; This was the main 
# model pre-registered in the PAP.
reg_m4 <- df_final %>%
  ## Generate centered pre-treatment covariates
  mutate(education_m = education - mean(education),
         gender_female_m = gender_female - mean(gender_female),
         ei_application_lapsed_m = ei_application_lapsed - mean(ei_application_lapsed),
         ei_application_status_m = ei_application_status - mean(ei_application_status),
         trf_match_prior_m = trf_match_prior - mean(trf_match_prior),
         age_m = age - mean(age)) %>%
  ## Lin Estimator
  lm_robust(enrollment ~ condition + 
              condition*(education_m + 
                           gender_female_m +
                           age_m + 
                           ei_application_lapsed_m +
                           ei_application_status_m + 
                           trf_match_prior_m
              ),
            data=., se_type="HC1")

#### <<<>>> M3 - Unadjusted model (not pre-registered with 14 day enrollment DV) ####

# This is M3 in Table A3.2 in Supplementary Material SMIII.2; 

reg_m3 <- df_final %>%
  lm_robust(enrollment_prereminder ~ condition,
            data=., se_type="HC1")

#### <<<>>> M5 - Unadjusted model (pre-registered with 30 day enrollment DV) ####

# This is M5 in Table A3.2 in Supplementary Material SMIII.2; This was the main 
# unadjusted model pre-registered in the PAP.

reg_m5 <- df_final %>%
  lm_robust(enrollment ~ condition,
            data=., se_type="HC1")


#### <<<>>> Regression table ####

# Note: we use tab_model to create the table. We adjust statistical significance
# tests manually based on the multiple comparison adjustment. 

# Order is: 
# 1. Adjusted regression with pre-reminder enrollment 
# 2. Adjusted regression with pre-reminder enrollment (no pre-trial enrolment)
# 3. Unadjusted regression with pre-reminder enrollment
# 4. Adjusted regression with end of trial enrollment
# 5. Unadjusted regression with end of trial enrollment

reg_table <- tab_model(reg_m1, reg_m2, reg_m3, 
                       reg_m4, reg_m5, 
                       show.se=TRUE,
                       show.ci = FALSE,
                       collapse.se = TRUE,
                       dv.labels = c("M1 Enrollment - 14 days", "M2 Enrollment - 14 days", 
                                     "M3 Enrollment - 14 days", "M4 Enrollment - 30 days", 
                                     "M5 Enrollment - 30 days"),
                       string.pred = "Coeffcient",
                       string.p = "P-Value",
                       digits = 5,
                       p.threshold = c(0.05),
                       p.style = "stars")
reg_table #content for regression table in paper appendix (TABLE A3.2)

#### <<<>>> Statistical Significance test using multiple comparison adjustment ####

# As per the PAP, we use Holm adjusted alphas. While the regression only includes
# four estimates, one comparing each of the treatments to the control group,
# we adjust for a total of nine comparison as we do in the marginal means below.
# The reason is that there are nine comparisons we make across the primary 
# hypothesis (H1) and secondary hypotheses (H2-H4):

### Adjustment for M1

## Capture pvalues (two-tailed tests, so signs are NOT important)
p.T1vsC_m1 <- summary(reg_m1)$coefficients[2,4] # Standard long 
p.T2vsC_m1 <- summary(reg_m1)$coefficients[3,4] # Standard short
p.T3vsC_m1 <- summary(reg_m1)$coefficients[4,4] # AC long
p.T4vsC_m1 <- summary(reg_m1)$coefficients[5,4] # AC short

## Rank p-values, make Holm adjustment, and store if p-values are stat sig

#STEP A: identify rank of p-values 
pvalues_m1 <- c(p.T1vsC_m1, p.T2vsC_m1, p.T3vsC_m1, p.T4vsC_m1)
sorted_pvalues_m1 <- sort(pvalues_m1)
#STEP B: enter into formula Target alpha / (n - rank + 1) within=9 here
alpha_test_T1vsC_m1 <- (0.05/(9-which(sorted_pvalues_m1 == p.T1vsC_m1)+1))
alpha_test_T2vsC_m1 <- (0.05/(9-which(sorted_pvalues_m1 == p.T2vsC_m1)+1))  
alpha_test_T3vsC_m1 <- (0.05/(9-which(sorted_pvalues_m1 == p.T3vsC_m1)+1))  
alpha_test_T4vsC_m1 <- (0.05/(9-which(sorted_pvalues_m1 == p.T4vsC_m1)+1))
#STEP C: compared p-values against adjusted alphas
p.T1vsC_m1 < alpha_test_T1vsC_m1
p.T2vsC_m1 < alpha_test_T2vsC_m1
p.T3vsC_m1 < alpha_test_T3vsC_m1
p.T4vsC_m1 < alpha_test_T4vsC_m1

### Adjustment for M2

## Capture pvalues (two-tailed tests, so signs are NOT important)
p.T1vsC_m2 <- summary(reg_m2)$coefficients[2,4] # Standard long 
p.T2vsC_m2 <- summary(reg_m2)$coefficients[3,4] # Standard short
p.T3vsC_m2 <- summary(reg_m2)$coefficients[4,4] # AC long
p.T4vsC_m2 <- summary(reg_m2)$coefficients[5,4] # AC short

## Rank p-values, make Holm adjustment, and store if p-values are stat sig

#STEP A: identify rank of p-values 
pvalues_m2 <- c(p.T1vsC_m2, p.T2vsC_m2, p.T3vsC_m2, p.T4vsC_m2)
sorted_pvalues_m2 <- sort(pvalues_m2)
#STEP B: enter into formula Target alpha / (n - rank + 1) within=9 here
alpha_test_T1vsC_m2 <- (0.05/(9-which(sorted_pvalues_m2 == p.T1vsC_m2)+1))
alpha_test_T2vsC_m2 <- (0.05/(9-which(sorted_pvalues_m2 == p.T2vsC_m2)+1))  
alpha_test_T3vsC_m2 <- (0.05/(9-which(sorted_pvalues_m2 == p.T3vsC_m2)+1))  
alpha_test_T4vsC_m2 <- (0.05/(9-which(sorted_pvalues_m2 == p.T4vsC_m2)+1))
#STEP C: compared p-values against adjusted alphas
p.T1vsC_m2 < alpha_test_T1vsC_m2
p.T2vsC_m2 < alpha_test_T2vsC_m2
p.T3vsC_m2 < alpha_test_T3vsC_m2
p.T4vsC_m2 < alpha_test_T4vsC_m2

### Adjustment for M3

## Capture pvalues (two-tailed tests, so signs are NOT important)
p.T1vsC_m3 <- summary(reg_m3)$coefficients[2,4] # Standard long 
p.T2vsC_m3 <- summary(reg_m3)$coefficients[3,4] # Standard short
p.T3vsC_m3 <- summary(reg_m3)$coefficients[4,4] # AC long
p.T4vsC_m3 <- summary(reg_m3)$coefficients[5,4] # AC short

## Rank p-values, make Holm adjustment, and store if p-values are stat sig

#STEP A: identify rank of p-values 
pvalues_m3 <- c(p.T1vsC_m3, p.T2vsC_m3, p.T3vsC_m3, p.T4vsC_m3)
sorted_pvalues_m3 <- sort(pvalues_m3)
#STEP B: enter into formula Target alpha / (n - rank + 1) within=9 here
alpha_test_T1vsC_m3 <- (0.05/(9-which(sorted_pvalues_m3 == p.T1vsC_m3)+1))
alpha_test_T2vsC_m3 <- (0.05/(9-which(sorted_pvalues_m3 == p.T2vsC_m3)+1))  
alpha_test_T3vsC_m3 <- (0.05/(9-which(sorted_pvalues_m3 == p.T3vsC_m3)+1))  
alpha_test_T4vsC_m3 <- (0.05/(9-which(sorted_pvalues_m3 == p.T4vsC_m3)+1))
#STEP C: compared p-values against adjusted alphas
p.T1vsC_m3 < alpha_test_T1vsC_m3
p.T2vsC_m3 < alpha_test_T2vsC_m3
p.T3vsC_m3 < alpha_test_T3vsC_m3
p.T4vsC_m3 < alpha_test_T4vsC_m3

### Adjustment for M4

## Capture pvalues (two-tailed tests, so signs are NOT important)
p.T1vsC_m4 <- summary(reg_m4)$coefficients[2,4] # Standard long 
p.T2vsC_m4 <- summary(reg_m4)$coefficients[3,4] # Standard short
p.T3vsC_m4 <- summary(reg_m4)$coefficients[4,4] # AC long
p.T4vsC_m4 <- summary(reg_m4)$coefficients[5,4] # AC short

## Rank p-values, make Holm adjustment, and store if p-values are stat sig

#STEP A: identify rank of p-values 
pvalues_m4 <- c(p.T1vsC_m4, p.T2vsC_m4, p.T3vsC_m4, p.T4vsC_m4)
sorted_pvalues_m4 <- sort(pvalues_m4)
#STEP B: enter into formula Target alpha / (n - rank + 1) within=9 here
alpha_test_T1vsC_m4 <- (0.05/(9-which(sorted_pvalues_m4 == p.T1vsC_m4)+1))
alpha_test_T2vsC_m4 <- (0.05/(9-which(sorted_pvalues_m4 == p.T2vsC_m4)+1))  
alpha_test_T3vsC_m4 <- (0.05/(9-which(sorted_pvalues_m4 == p.T3vsC_m4)+1))  
alpha_test_T4vsC_m4 <- (0.05/(9-which(sorted_pvalues_m4 == p.T4vsC_m4)+1))
#STEP C: compared p-values against adjusted alphas
p.T1vsC_m4 < alpha_test_T1vsC_m4
p.T2vsC_m4 < alpha_test_T2vsC_m4
p.T3vsC_m4 < alpha_test_T3vsC_m4
p.T4vsC_m4 < alpha_test_T4vsC_m4

### Adjustment for M5

## Capture pvalues (two-tailed tests, so signs are NOT important)
p.T1vsC_m5 <- summary(reg_m5)$coefficients[2,4] # Standard long 
p.T2vsC_m5 <- summary(reg_m5)$coefficients[3,4] # Standard short
p.T3vsC_m5 <- summary(reg_m5)$coefficients[4,4] # AC long
p.T4vsC_m5 <- summary(reg_m5)$coefficients[5,4] # AC short

## Rank p-values, make Holm adjustment, and store if p-values are stat sig

#STEP A: identify rank of p-values 
pvalues_m5 <- c(p.T1vsC_m5, p.T2vsC_m5, p.T3vsC_m5, p.T4vsC_m5)
sorted_pvalues_m5 <- sort(pvalues_m5)
#STEP B: enter into formula Target alpha / (n - rank + 1) within=9 here
alpha_test_T1vsC_m5 <- (0.05/(9-which(sorted_pvalues_m5 == p.T1vsC_m5)+1))
alpha_test_T2vsC_m5 <- (0.05/(9-which(sorted_pvalues_m5 == p.T2vsC_m5)+1))  
alpha_test_T3vsC_m5 <- (0.05/(9-which(sorted_pvalues_m5 == p.T3vsC_m5)+1))  
alpha_test_T4vsC_m5 <- (0.05/(9-which(sorted_pvalues_m5 == p.T4vsC_m5)+1))
#STEP C: compared p-values against adjusted alphas
p.T1vsC_m5 < alpha_test_T1vsC_m5
p.T2vsC_m5 < alpha_test_T2vsC_m5
p.T3vsC_m5 < alpha_test_T3vsC_m5
p.T4vsC_m5 < alpha_test_T4vsC_m5


#############################################################################//
#### /// FIGURE2 Estimate and plot Marginal Means ####
#############################################################################//

# Based on model M1 (DV=14 days; adjusted for pre-treatment enrollment)

## Get marginal means for condition

# look at the reference grid first:
ref_grid(reg_m1)
## Note: if we specify emmeans as follows: emmeans(reg_m1, ~ condition) then the
## package averages over the factor variables. However, it won't take the average
## in the data, which is ~zero because we mean centered the covariates. Instead, it will
## take the average between the two values in the reference grid. So for education_m
## it will calculate: (-0.51463+0.48537)/2. It will do so for any factor variable
## which inevitably leads to emmeans that are different from the estimates of the
## condition variables in the regression model. Hence, we set the values for the
## covariates here to the means of the data. The estimated marginal means for the
## condition variable are then the same as the estimates in the regression model
## and we proceed from there (note: this is not an issue for continuous variables).
## Background information: https://cran.r-project.org/web/packages/emmeans/vignettes/basics.html#:~:text=Estimated%20marginal%20means%20are%20defined,combinations%20%E2%80%93%20called%20the%20reference%20grid.
## (last accessed: June 9, 2024)

emm_m1 <- emmeans(reg_m1, ~ condition, at = list(
  #the mean is de-facto zero though for accuracy, we use the mean of the mean
  #centered covariates
  education_m=mean(df_final$education - mean(df_final$education)),
  gender_female_m=mean(df_final$gender_female - mean(df_final$gender_female)),
  age_m=mean(df_final$age - mean(df_final$age)),
  ei_application_lapsed_m=mean(df_final$ei_application_lapsed - mean(df_final$ei_application_lapsed)),
  ei_application_status_m=mean(df_final$ei_application_status - mean(df_final$ei_application_status)),
  trf_match_prior_m=mean(df_final$trf_match_prior - mean(df_final$trf_match_prior)),
  enrollment_pretrial_m=mean(df_final$enrollment_pretrial - mean(df_final$enrollment_pretrial))
)
)


## Set up contrasts
C = c(1,0,0,0,0) #Control
T1 = c(0,1,0,0,0) #T1 - standard-long
T2 = c(0,0,1,0,0) #T2 - standard-short
T3 = c(0,0,0,1,0) #T3 - AC-long
T4 = c(0,0,0,0,1) #T4 - AC-short
SC = (T1+T2)/2 # T Standard
AC = (T3+T4)/2 # T AC
LF = (T1+T3)/2 # T Long
SF = (T2+T4)/2 # T Short

## Estimate and save contrasts
contrasts_reg_m1 <- contrast(emm_m1, 
                             method = list(
                               #H1: any treatment will affect take-up relative to control
                               "Standard long - Control" = T1 - C,
                               "Standard short - Control" = T2 - C,
                               "AC long - Control" = T3 - C,
                               "AC short - Control" = T4 - C,
                               #H2: Enrollment will be highest in AC-Short Form condition
                               #"T4 - C" = T4 - C, does not need be included, comparison is part of H1
                               "AC short - Standard long" = T4 - T1,
                               "AC short - Standard short" = T4 - T2,
                               "AC short - AC long" = T4 - T3,
                               #H3: Higher enrollment in short form relative to long-form 
                               "Short form - Long form" = SF - LF,
                               #H4: Higher enrollment in active choice vs standard call to action
                               "AC - Standard" = AC - SC
                             ), 
                             #add Holm adjustment for multiple tests
                             adjust="Holm" )


#### <<>> Table A3.4: Save table ####
df_contrasts_reg_m1 <- as.data.frame(contrasts_reg_m1)
write.csv(df_contrasts_reg_m1, paste(temp_dir_tables, "reg_m1_emmeans.csv", sep=""), row.names=FALSE)

#### <<>> FIGURE 2: Plot results from primary regression model and estimated marginal means ####

# Plot estimated marginal means for treatment variable with 95% confidence intervals
plot_emm <- as.data.frame(emm_m1) %>% #select intercept + estimates for treatment
  ggplot(., aes(x=reorder(condition, emmean), y=emmean)) +
  geom_bar(stat="identity", fill="#56B4E9", alpha=0.5) +
  #the confidence intervals are 95% CIs. Not adjusted for N of tests.
  geom_errorbar( aes(x=condition, ymin=lower.CL, ymax=upper.CL), 
                 width=0.4, colour="#E69F00", alpha=0.9, size=1.3) +
  theme_minimal() + 
  scale_x_discrete(name="Condition",
                   labels=c("Control \n (N=1976)", "Active Choice x \n warm handoff \n  (N=1975)",
                            "Standard call x \n warm handoff \n  (N=1975)", "Active Choice x \n cool handoff \n  (N=1975)",
                            "Standard call x \n cool handoff \n  (N=1976)"))+
  ylab("Estimated take-up (marginal means)") +
  scale_y_continuous(labels = percent) +
  annotate(geom="text", x=1, y=0.00884+0.001, label="0.68%") + #control group
  annotate(geom="text", x=5, y=0.02321+0.001, label="1.82%*") + #Standard long group
  annotate(geom="text", x=3, y=0.01478+0.001, label="1.12%") + #Standard short group
  annotate(geom="text", x=4, y=0.02288+0.001, label="1.79%*") + #AC long group
  annotate(geom="text", x=2, y=0.01252+0.001, label="0.95%") #AC short group

# safe plot
tiff(paste(temp_dir_figures, "plot_emm.tiff", sep=""),
     units="in", width=7, height=4.5, res=900, compression = "lzw")
plot_emm
dev.off()


#### /// FIGURE 3 Take-up of WorkBC ####

#### <<>> data prep ####
df_funnel <- data.frame(
  Condition = rep(c("Standard call x \n cool handoff", 
                    "Standard call x \n warm handoff", 
                    "Active Choice x \n cool handoff", 
                    "Active Choice x \n warm handoff"), each=5),
  Stage = c("Participants", "Email Open", "Click-through", "Form submission", "Enrolment"),
  Count = c(
    # Standard long
    table_outcome_by_condition[2,2], #N of participants
    table_outcome_by_condition[2,3]*table_outcome_by_condition[2,2], #email open
    table_outcome_by_condition[2,6]*table_outcome_by_condition[2,2], #click through
    NA,
    table_outcome_by_condition[2,12]*table_outcome_by_condition[2,2], #enrollment
    # Standard short
    table_outcome_by_condition[3,2], #N of participants
    table_outcome_by_condition[3,3]*table_outcome_by_condition[3,2], #email open
    table_outcome_by_condition[3,6]*table_outcome_by_condition[3,2], #click through
    table_outcome_by_condition[3,9]*table_outcome_by_condition[3,2], #short form submission
    table_outcome_by_condition[3,12]*table_outcome_by_condition[3,2], #enrollment
    # AC long
    table_outcome_by_condition[4,2], #N of participants
    table_outcome_by_condition[4,3]*table_outcome_by_condition[4,2], #email open
    table_outcome_by_condition[4,6]*table_outcome_by_condition[4,2], #click through
    NA,
    table_outcome_by_condition[4,12]*table_outcome_by_condition[4,2], #enrollment
    #AC short
    table_outcome_by_condition[5,2], #N of participants
    table_outcome_by_condition[5,3]*table_outcome_by_condition[5,2], #email open
    table_outcome_by_condition[5,6]*table_outcome_by_condition[5,2], #click through
    table_outcome_by_condition[5,9]*table_outcome_by_condition[5,2], #short form submission
    table_outcome_by_condition[5,12]*table_outcome_by_condition[5,2] #enrollment
  ),
  Percentage = c(
    # Standard long
    1, #N of participants
    round(table_outcome_by_condition[2,3],3), #email open
    round(table_outcome_by_condition[2,6],3), #click through
    NA,
    round(table_outcome_by_condition[2,12],3), #enrollment
    # AC long
    1, #N of participants
    round(table_outcome_by_condition[3,3],3), #email open
    round(table_outcome_by_condition[3,6],3), #click through
    round(table_outcome_by_condition[3,9],3), #short form submission
    round(table_outcome_by_condition[3,12],3), #enrollment
    # Standard short
    1, #N of participants
    round(table_outcome_by_condition[4,3],3), #email open
    round(table_outcome_by_condition[4,6],3), #click through
    NA,
    round(table_outcome_by_condition[4,12],3), #enrollment
    #AC short
    1, #N of participants
    round(table_outcome_by_condition[5,3],3), #email open
    round(table_outcome_by_condition[5,6],3), #click through
    round(table_outcome_by_condition[5,9],3), #short form submission
    round(table_outcome_by_condition[5,12],3) #enrollment
  )
)

# Calculate CIs for each stage (excluding "Participants", where CI is not needed)
df_funnel$CI_lower <- NA
df_funnel$CI_upper <- NA

# Loop over each row and calculate CI for each funnel step
for (i in 1:nrow(df_funnel)) {
  # Skip "Participants" (stage 1) for CI calculation
  if (df_funnel$Stage[i] != "Participants" & !is.na(df_funnel$Count[i])) {
    n <- table_outcome_by_condition[i %/% 5 + 2, 2]  # Total participants in the condition
    p <- df_funnel$Percentage[i]  # Percentage as proportion
    
    # Calculate the confidence interval using prop.test
    ci <- prop.test(round(p * n), n, conf.level = 0.95)$conf.int
    
    df_funnel$CI_lower[i] <- ci[1]
    df_funnel$CI_upper[i] <- ci[2]
  }
}

# issue calcuating the last CIs so we add them "manually":
df_funnel[20,5] <- (prop.test(round(0.007 * 1975), n=1975, conf.level = 0.95)$conf.int)[1]
df_funnel[20,6] <- (prop.test(round(0.007 * 1975), n=1975, conf.level = 0.95)$conf.int)[2]

# Check CIs
head(df_funnel)

#### <<>> plot ####
plot_funnel_fig3 <- df_funnel %>%
  filter(!is.na(Percentage)) %>%
  ggplot(., aes(x = Percentage*100, y = reorder(Stage, Percentage))) +
  geom_bar(stat = "identity", fill = "#56B4E9", width=0.8, alpha=0.5) +
  geom_errorbar( aes(xmin = CI_lower*100, xmax = CI_upper*100, 
                     y = reorder(Stage, Percentage)), width=0.3, 
                 colour="#E69F00", alpha=0.9, size=1.3) +
  geom_text(aes(label = paste0("N = ", Count,  "\n", paste0(Percentage*100, "%"))), 
            hjust = -0.2, color = "black",
            size=3.5) +
  theme_minimal() +
  #xlim(0,130)+
  scale_x_continuous(limits=c(0,125),
                     breaks=c(0,25,50,75,100,125),
                     labels=c("0%", "25%", "50%", "75%", "100%", "")) +
  theme(legend.position = "none") +
  labs(x = "Percent of all participants at each stage", y = "WorkBC take-up stages") +
  #facet wrap and adjust order so that the two long conditions are in the top row,
  # here: https://forum.posit.co/t/re-ordering-facet-wrap/141999/2 (last accessed: August 31, 2024)
  facet_wrap(~factor(Condition, c("Standard call x \n cool handoff", 
                                  "Active Choice x \n cool handoff",
                                  "Standard call x \n warm handoff",
                                  "Active Choice x \n warm handoff")), 
             scales = "free_y") 

# safe plot
tiff(paste(temp_dir_figures, "plot_funnel_fig3_final.tiff", sep=""),
     units="in", width=10, height=5.5, res=900, compression = "lzw")
plot_funnel_fig3
dev.off()



#############################################################################//
#### /// Heterogeneity Analysis - Enrollment in Work BC                  #####
#############################################################################//

## Note: analysis based on adjusted regression model (DV=14 days; adjusted for
## pre-trial enrollment)

#### <<>> Set up multi arm causal forest model ####
X <- as.matrix(df_final %>%
                 #mean center relevant covariates
                 mutate(education_m = education - mean(education),
                        gender_female_m = gender_female - mean(gender_female),
                        ei_application_lapsed_m = ei_application_lapsed - mean(ei_application_lapsed),
                        ei_application_status_m = ei_application_status - mean(ei_application_status),
                        trf_match_prior_m = trf_match_prior - mean(trf_match_prior),
                        age_m = age - mean(age),
                        enrollment_pretrial_m= enrollment_pretrial - mean(enrollment_pretrial)) %>%
                 #set up matrix with covariates for ML
                 select(education_m, gender_female_m, ei_application_lapsed_m,
                        ei_application_status_m,trf_match_prior_m , age_m, enrollment_pretrial_m)
)

#X <- as.matrix(df_final[,c("age","gender_female",
#                           "education", "ei_application_lapsed",
#                           "ei_application_status", "trf_match_prior")])
Y <- df_final$enrollment_prereminder
W <- df_final$condition

# since this is an RCT, we can fix propensity scores of treatment 
# assignment:
N_mcforest <- 9877
W.hat <- matrix(1/5, N_mcforest, 5) # we can fix this at .2 because it was an RCT
colnames(W.hat) <- c("Control", 
                     "Standard long",
                     "Standard short",
                     "AC long",
                     "AC short") 

## Estimate a propensity score model first - check if there are any differences
## by pre-treatment covariate & check for overlap:
propensity.forest = probability_forest(X, W)
W.hat_pred = predict(propensity.forest)$predictions
hist(W.hat_pred, xlab = "propensity score")
## > values are not clustering at the extremes so model should be ok to run.
# link: https://github.com/grf-labs/grf/issues/1024
# note: Due to randomized assignment of participants into one of the five groups,
# there was no reason to expect any issues here. This was merely for completion
# and to detect any potential coding errors that may have led to issues of overlap.

#### <<>> Estimate the model ####

# main model as per PAP
mc.forest <- multi_arm_causal_forest(X, Y, W, 
                                     num.trees=20000, 
                                     min.node.size=50,
                                     seed=135689,
                                     W.hat = W.hat)
# Robustness check with packages default tuning parameters
mc.forest_rbc <- multi_arm_causal_forest(X, Y, W,
                                         seed=135639,
                                         W.hat = W.hat)
#### <<>> Average Treatment Effects ####
ate.hat <- average_treatment_effect(mc.forest) #main result
ate.hat.rbc <- average_treatment_effect(mc.forest_rbc) #robustness check


#### <<>> Save table (TABLE A3.6) ####

## Save table - M1 Final model
df_ate.hat <- as.data.frame(ate.hat)
write.csv(df_ate.hat, paste(temp_dir_tables, "df_ate.hat_08072024.csv", sep=""), row.names=FALSE)

## Save table - M2 RBC model 
df_ate.hat.rbc <- as.data.frame(ate.hat.rbc)
write.csv(df_ate.hat.rbc, paste(temp_dir_tables, "df_ate.hat.rbc_09042024.csv", sep=""), row.names=FALSE)


#### <<>> Variable Importance (TABLE A3.9) ####
df_var_imp <- cbind(c("age","gender_female",
                      "education", "ei_application_lapsed",
                      "ei_application_status", "trf_match_prior",
                      "enrollment_pretrial"), variable_importance(mc.forest))
## Save table
df_var_imp <- as.data.frame(df_var_imp)
write.csv(df_var_imp, paste(temp_dir_tables, "df_var_imp_08072024.csv", sep=""), row.names=FALSE)


#### <<>> How much heterogeneity is there in the treatment effects - RATE ####

#### <<<>>> Estimate Rank-Weighted Average Treatment Effect (RATE) ####

# generally requires separate training and test sample. Due to concerns about
# power, we use a heuristic based on out-of-bag estimation and one-sided tests.
# Documented here: https://grf-labs.github.io/grf/articles/rate_cv.html (last accessed
# July 12, 2024).

# predict Cate on OOB sample
cate.hat <-  predict(mc.forest, X)$predictions[, , 1]
# get doubly robust scores
DR.scores <- get_scores(mc.forest)[, , 1]
# form doubly robust RATE estimate on OOB (by treatment)
rateT1 <- rank_average_treatment_effect.fit(DR.scores[,1], cate.hat[,1], R=10000) # standard long
rateT2 <- rank_average_treatment_effect.fit(DR.scores[,2], cate.hat[,2], R=10000) # Standard short
rateT3 <- rank_average_treatment_effect.fit(DR.scores[,3], cate.hat[,3], R=10000) # AC long
rateT4 <- rank_average_treatment_effect.fit(DR.scores[,4], cate.hat[,4], R=10000) # AC short

# compute one-sided p-value 
p.val.onesided_T1 = round(pnorm(t.state.oob_T1 <- rateT1$estimate/rateT1$std.err,
                                lower.tail = FALSE) ,5)
p.val.onesided_T2 = round(pnorm(t.state.oob_T2 <- rateT1$estimate/rateT2$std.err,
                                lower.tail = FALSE) ,5)
p.val.onesided_T3 = round(pnorm(t.state.oob_T3 <- rateT1$estimate/rateT3$std.err,
                                lower.tail = FALSE) ,5)
p.val.onesided_T4 = round(pnorm(t.state.oob_T4 <- rateT1$estimate/rateT4$std.err,
                                lower.tail = FALSE) ,5)

# construct 95% confidence intervals. From GRF documentation:
# "# A significant result suggests that there are HTEs and that the CATE-based prioritization rule
# is effective at stratifying the sample."
# https://grf-labs.github.io/grf/reference/rank_average_treatment_effect.html (last accessed: July 10, 2024)
rateCIT1 <- round(rateT1$estimate + 1.96*c(-1, 1)*rateT1$std.err,3) # standard long
rateCIT2 <- round(rateT2$estimate + 1.96*c(-1, 1)*rateT2$std.err,3) # standard short
rateCIT3 <- round(rateT3$estimate + 1.96*c(-1, 1)*rateT3$std.err,3) # AC long
rateCIT4 <- round(rateT4$estimate + 1.96*c(-1, 1)*rateT4$std.err,3) # AC short

#### <<<>>> Save RATE results in table (Table A3.7) ####
table_RATE <- as.data.frame(rbind(cbind("Condition", "Estimate", "std. error", "95% CI"),
                                  cbind("Standard call x cool handoff", paste(round(rateT1[[1]],3), "*", sep=""), round(rateT1[[2]],3), paste(rateCIT1[1], ",", rateCIT1[2])),
                                  cbind("Standard call x warm handoff", paste(round(rateT2[[1]],3), "*", sep=""), round(rateT2[[2]],3), paste(rateCIT2[1], ",", rateCIT2[2])),
                                  cbind("Active Choice x cool handoff", paste(round(rateT3[[1]],3), "*", sep=""), round(rateT3[[2]],3), paste(rateCIT3[1], ",", rateCIT3[2])),
                                  cbind("Active Choice x warm handoff", paste(round(rateT4[[1]],3), "*", sep=""), round(rateT4[[2]],3), paste(rateCIT4[1], ",", rateCIT4[2]))
))
write.csv(table_RATE, paste(temp_dir_tables, "table_RATE_07122024.csv", sep=""), row.names=FALSE)


#### <<<>>> Plot RATE for appendix (FIGURE A3.2) ####
plot_rateT1 <- as.data.frame(rateT1$TOC) %>%
  ggplot(., aes(x=q, y=estimate, 
                ymin=estimate-1.96*std.err,
                ymax=estimate+1.96*std.err))+
  geom_line() + 
  geom_ribbon(alpha=0.5, fill="#E69F00") +
  theme_minimal() +
  scale_x_continuous(breaks=seq(0,1, 0.2)) +
  ylim(-0.005,0.1) +
  geom_hline(yintercept=0, linetype="dashed") +
  xlab("Treated fraction (q)")+
  ylab("") +
  ggtitle("A. Standard call x cool handoff vs \n Control")

plot_rateT2 <- as.data.frame(rateT2$TOC) %>%
  ggplot(., aes(x=q, y=estimate, 
                ymin=estimate-1.96*std.err,
                ymax=estimate+1.96*std.err))+
  geom_line() + 
  geom_ribbon(alpha=0.5, fill="#E69F00") +
  theme_minimal() +
  scale_x_continuous(breaks=seq(0,1, 0.2)) +
  ylim(-0.005,0.1) +
  geom_hline(yintercept=0, linetype="dashed") +
  xlab("Treated fraction (q)")+
  ylab("")+
  ggtitle("B. Standard call x warm handoff vs \n Control")

plot_rateT3 <- as.data.frame(rateT3$TOC) %>%
  ggplot(., aes(x=q, y=estimate, 
                ymin=estimate-1.96*std.err,
                ymax=estimate+1.96*std.err))+
  geom_line() + 
  geom_ribbon(alpha=0.5, fill="#E69F00") +
  theme_minimal() +
  scale_x_continuous(breaks=seq(0,1, 0.2)) + 
  ylim(-0.005,0.1) +
  geom_hline(yintercept=0, linetype="dashed") +
  xlab("Treated fraction (q)")+
  ylab("")+
  ggtitle("C. Active Choice x cool handoff vs \n Control")

plot_rateT4 <- as.data.frame(rateT4$TOC) %>%
  ggplot(., aes(x=q, y=estimate, 
                ymin=estimate-1.96*std.err,
                ymax=estimate+1.96*std.err))+
  geom_line() + 
  geom_ribbon(alpha=0.5, fill="#E69F00") +
  theme_minimal() +
  scale_x_continuous(breaks=seq(0,1, 0.2)) +
  ylim(-0.005,0.1) +
  geom_hline(yintercept=0, linetype="dashed") +
  xlab("Treated fraction (q) \n")+
  ylab("")+
  ggtitle("D. Active Choice x warm handoff vs \n Control")

# Safe plot
tiff(paste(temp_dir_figures, "RATE_plots.tiff", sep=""),
     units="in", width=8, height=9, res=900, compression = "lzw")
grid.arrange(plot_rateT1, plot_rateT2, plot_rateT3, plot_rateT4,
             ncol = 2,
             nrow = 2)
dev.off()

#### <<<>>> Plotting CATEs (FIGURE A3.1) ####
plot_cate1 <- as.data.frame(cate.hat) %>%
  ggplot(., aes(x=.[,1])) + 
  geom_histogram(fill="#56B4E9") +
  theme_minimal() +
  ylim(0,1000) +
  xlab("Standard call x cool handoff - Control") +
  ylab("Count") +
  # add line for ATE 
  geom_vline(xintercept = ate.hat[1,1], linetype="dotted", 
             color = "red", size=1)

plot_cate2 <- as.data.frame(cate.hat) %>%
  ggplot(., aes(x=.[,2])) + 
  geom_histogram(fill="#56B4E9") +
  theme_minimal() +
  ylim(0,1000) +
  xlab("Standard call x warm handoff - Control") +
  ylab("Count") +
  # add line for ATE 
  geom_vline(xintercept = ate.hat[2,1], linetype="dotted", 
             color = "red", size=1)

plot_cate3 <- as.data.frame(cate.hat) %>%
  ggplot(., aes(x=.[,3])) + 
  geom_histogram(fill="#56B4E9") +
  theme_minimal() +
  ylim(0,1000) +
  xlab("Active Choice x cool handoff - Control") +
  ylab("Count") +
  # add line for ATE 
  geom_vline(xintercept = ate.hat[3,1], linetype="dotted", 
             color = "red", size=1)

plot_cate4 <- as.data.frame(cate.hat) %>%
  ggplot(., aes(x=.[,4])) + 
  geom_histogram(fill="#56B4E9") +
  theme_minimal() +
  ylim(0,1000) +
  xlab("Active Choice x warm handoff - Control") +
  ylab("Count") +
  # add line for ATE 
  geom_vline(xintercept = ate.hat[4,1], linetype="dotted", 
             color = "red", size=1)

# Safe plot
tiff(paste(temp_dir_figures, "CATE_plots.tiff", sep=""),
     units="in", width=7, height=6, res=900, compression = "lzw")
grid.arrange(plot_cate1, plot_cate2, plot_cate3, plot_cate4,
             ncol = 2,
             nrow = 2)
dev.off()

#### <<>> Best linear projection (TABLE A3.8) ####

#BLP:
#"Procedurally, we do so by regressing doubly robust scores derived from the forest 
#against the Ai. Note the covariates Ai may consist of a subset of the Xi, or 
#they may be distinct. The case of the null model tau(Xi) ~ beta_0 is equivalent
#to fitting an average treatment effect via AIPW."
# Source: https://grf-labs.github.io/grf/reference/best_linear_projection.html

#> test is simple and works. so we can just do this for each treatment. Then
#> select the most important variables + what is relevant here.

# Standard long
best.linear.predictor_T1 <- lm(DR.scores[,1] ~ X[,1:7])
blp.summary_T1 <- lmtest::coeftest(best.linear.predictor_T1, vcov = sandwich::vcovCL, 
                                   type = "HC3")
# Standard short
best.linear.predictor_T2 <- lm(DR.scores[,2] ~ X[,1:7])
blp.summary_T2 <- lmtest::coeftest(best.linear.predictor_T2, vcov = sandwich::vcovCL, 
                                   type = "HC3")
# AC long
best.linear.predictor_T3 <- lm(DR.scores[,3] ~ X[,1:7])
blp.summary_T3 <- lmtest::coeftest(best.linear.predictor_T3, vcov = sandwich::vcovCL, 
                                   type = "HC3")

# AC short
best.linear.predictor_T4 <- lm(DR.scores[,4] ~ X[,1:7])
blp.summary_T4 <- lmtest::coeftest(best.linear.predictor_T4, vcov = sandwich::vcovCL, 
                                   type = "HC3")

# Table
table_blp <- as.data.frame(cbind(c("Intercept", "Education (=University/College)", 
                                   "Gender (=Female)", "Days since EI application",
                                   "EI applications status (=granted)", 
                                   "Prior TRF engagement (=yes)",
                                   "Age (in years)",
                                   "Enrolled pre-trial start (=yes)"),
                                 round(as.data.frame(blp.summary_T1[,])$Estimate, 4),
                                 round(as.data.frame(blp.summary_T1[,])$`Std. Error`, 4),
                                 round(as.data.frame(blp.summary_T2[,])$Estimate, 4),
                                 round(as.data.frame(blp.summary_T2[,])$`Std. Error`, 4),
                                 round(as.data.frame(blp.summary_T3[,])$Estimate, 4),
                                 round(as.data.frame(blp.summary_T3[,])$`Std. Error`, 4),
                                 round(as.data.frame(blp.summary_T4[,])$Estimate, 4),
                                 round(as.data.frame(blp.summary_T4[,])$`Std. Error`, 4)))
write.csv(table_blp, paste(temp_dir_tables, "table_BLP_08072024.csv", sep=""), row.names=FALSE)



#### <<>> ATEs for prior TRF engagement, education, gender, and age (TABLE A3.10) ####

## ATE by group: https://arxiv.org/pdf/1902.07409

### <<<>>> TRF Engagement

ate.trf = average_treatment_effect(mc.forest, subset = X[,5]>0) # ate for previous TRF
ate.trf_no = average_treatment_effect(mc.forest, subset = X[,5]<0 )  # ate for no previous TRF
# diff in ATE + 95%
ate.trf.diff <- cbind(round(ate.trf[1], 3), round(ate.trf_no[1], 3),
                      round(ate.trf[1] - ate.trf_no[1] , 3),
                      round(ate.trf[1] - ate.trf_no[1] , 3) + round(qnorm(0.975) * sqrt(ate.trf[2]^2 + ate.trf_no[2]^2), 3),
                      round(ate.trf[1] - ate.trf_no[1] , 3) - round(qnorm(0.975) * sqrt(ate.trf[2]^2 + ate.trf_no[2]^2), 3))
names(ate.trf.diff)[3] <- "Difference"
names(ate.trf.diff)[4] <- "95% low"
names(ate.trf.diff)[5] <- "95% high"

### <<<>>> Education

ate.edu = average_treatment_effect(mc.forest, subset = X[,1]>0) # ate for college/uni education
ate.edu_no = average_treatment_effect(mc.forest, subset = X[,1]<0 )  # ate for no college/uni education
# diff in ATE + 95%
ate.edu.diff <- cbind(round(ate.edu[1], 3), round(ate.edu_no[1], 3),
                      round( ate.edu[1] - ate.edu_no[1] , 3),
                      round( ate.edu[1] - ate.edu_no[1] , 3) + round(qnorm(0.975) * sqrt(ate.edu[2]^2 + ate.edu_no[2]^2), 3),
                      round( ate.edu[1] - ate.edu_no[1] , 3) - round(qnorm(0.975) * sqrt(ate.edu[2]^2 + ate.edu_no[2]^2), 3))
names(ate.edu.diff)[3] <- "Difference"
names(ate.edu.diff)[4] <- "95% low"
names(ate.edu.diff)[5] <- "95% high"

### <<<>>> Gender

ate.gen = average_treatment_effect(mc.forest, subset = X[,1]>0) # ate for female
ate.gen_no = average_treatment_effect(mc.forest, subset = X[,1]<0 )  # ate for not female
# diff in ATE + 95%
ate.gen.diff <- cbind(round(ate.gen[1], 3), round(ate.gen_no[1], 3),
                      round( ate.gen[1] - ate.gen_no[1] , 3),
                      round( ate.gen[1] - ate.gen_no[1] , 3) + round(qnorm(0.975) * sqrt(ate.gen[2]^2 + ate.gen_no[2]^2), 3),
                      round( ate.gen[1] - ate.gen_no[1] , 3) - round(qnorm(0.975) * sqrt(ate.gen[2]^2 + ate.gen_no[2]^2), 3))
names(ate.gen.diff)[3] <- "Difference"
names(ate.gen.diff)[4] <- "95% low"
names(ate.gen.diff)[5] <- "95% high"

### <<<>>> Age - Youth (30 or younger)

# -7.739698=is the mean centered equivalent to age 31 in our sample.
ate.youth = average_treatment_effect(mc.forest, subset = X[,6]< -7.739698) # ate youth (30 or under)
ate.youth_no = average_treatment_effect(mc.forest, subset = X[,6]> -8.73969828895414 )  # ate not youth (over 30)
# diff in ATE + 95%
ate.youth.diff <- cbind(round(ate.youth[1], 3), round(ate.youth_no[1], 3),
                        round( ate.youth[1] - ate.youth_no[1] , 3),
                        round( ate.youth[1] - ate.youth_no[1] , 3) + round(qnorm(0.975) * sqrt(ate.youth[2]^2 + ate.youth_no[2]^2), 3),
                        round( ate.youth[1] - ate.youth_no[1] , 3) - round(qnorm(0.975) * sqrt(ate.youth[2]^2 + ate.youth_no[2]^2), 3))
names(ate.youth.diff)[3] <- "Difference"
names(ate.youth.diff)[4] <- "95% low"
names(ate.youth.diff)[5] <- "95% high"

### <<<>>> Age - Youth (24 or younger)

# -7.739698=is the mean centered equivalent to age 31 in our sample.
ate.youth2 = average_treatment_effect(mc.forest, subset = X[,6]< -13.7396982889541 ) # ate youth (30 or under)
ate.youth_no2 = average_treatment_effect(mc.forest, subset = X[,6]> -14.7396982889541 )  # ate not youth (over 30)
# diff in ATE + 95%
ate.youth.diff2 <- cbind(round(ate.youth2[1], 3), round(ate.youth_no2[1], 3),
                         round(ate.youth2[1] - ate.youth_no2[1] , 3),
                         round( ate.youth2[1] - ate.youth_no2[1] , 3) + round(qnorm(0.975) * sqrt(ate.youth2[2]^2 + ate.youth_no2[2]^2), 3),
                         round( ate.youth2[1] - ate.youth_no2[1] , 3) - round(qnorm(0.975) * sqrt(ate.youth2[2]^2 + ate.youth_no2[2]^2), 3))
names(ate.youth.diff2)[3] <- "Difference"
names(ate.youth.diff2)[4] <- "95% low"
names(ate.youth.diff2)[5] <- "95% high"


## Table
df_ate_groups <- as.data.frame(cbind(rbind(ate.trf.diff,
                                           ate.edu.diff,
                                           ate.gen.diff,
                                           ate.youth.diff,
                                           ate.youth.diff2)),
                               rep(c("TRF", "Education",
                                     "Gender", "Youth U31",
                                     "Youth U35"), each=4))
write.csv(df_ate_groups, paste(temp_dir_tables, "table_ate_subgroups08132024.csv", sep=""), row.names=FALSE)


#### <<>> Policy Tree (FIGURE A3.3) ####

# the variable importance heuristic suggests that 

# Compute doubly robust reward estimates.
Gamma.matrix <- double_robust_scores(mc.forest)
head(Gamma.matrix)

set.seed(18506)
# train policy tree
opt.tree <- policy_tree(X, Gamma.matrix, depth = 2)
opt.tree
pt_plot <- plot(opt.tree, 
                leaf.labels = c("Control", "Standard long", "Standard short", 
                                "AC long", "AC short"))

# Variables are mean centred:
# age=-3.74 corresponds to age=35
# age=5.26 corresponds to age=44
# trf=-0.19 corresponds to No prior TRF matc

# Plot policy tree
tiff(paste(temp_dir_figures, "policy_tree_07152024.tiff", sep=""),
     units="in", width=8, height=9, res=900, compression = "lzw")
pt_plot
dev.off()

#### <<>> Why do TRF and age matter for standard vs AC intervention policy tree assignment (TABLE A3.11) ####

## Let's look at click through rates by age group and trf engagement, using the
## policy tree split results.
df_agetrf <- df_final %>%
  mutate(age_35 = case_when(
    age<36 ~ "under 36",
    age>35 & age<45 ~ "36-44",
    TRUE ~ "over 44"
  )) %>% 
  group_by(age_35, trf_match_prior) %>%
  summarize(avg=mean(click_through_prer_adj, na.rm=T))

# Paragraph on Table A3.10 mentions that click through rates are highest
# among those over 44. The following produces that result:
df_final %>%
  mutate(age_35 = case_when(
    age<36 ~ "under 36",
    age>35 & age<45 ~ "36-44",
    TRUE ~ "over 44"
  )) %>% 
  group_by(age_35) %>%
  summarize(avg=mean(click_through_prer_adj, na.rm=T))

# Table
write.csv(as.data.frame(df_agetrf), 
          paste(temp_dir_tables, "table_ctr_age_trf_final.csv", sep=""),
          row.names=FALSE)


#############################################################################//
#### /// Appendix - Other                                                 #####
#############################################################################//

#### <<>> FIGURE A2.1 - Enrolment Rates ####

#Plot to show cumulative enrollment rates over time

plot_enrollmenttime <- df_final %>%
  group_by(condition) %>%
  summarize(avg_0 = mean(enrollment_pretrial)*100,
            avg_14 = mean(enrollment_prereminder)*100,
            avg_30 = mean(enrollment)*100,
            avg_40 = mean(enrollment_40)*100,
            avg_50 = mean(enrollment_50)*100,
            avg_60 = mean(enrollment_60)*100,
            avg_70 = mean(enrollment_70)*100,
            avg_80 = mean(enrollment_80)*100,
            avg_90 = mean(enrollment_90)*100) %>%
  pivot_longer(cols = starts_with("avg"), 
               names_to = "variable", 
               values_to = "mean_value") %>%
  ggplot(aes(x = variable, y = mean_value, color = condition, group = condition)) +
  #annotate("rect", xmin=c(1,1), xmax=c(1,2), ymin=c(3.5,3.5) , ymax=c(4,4), alpha=0.05, color="blue", fill="blue")+
  geom_line() +
  theme_minimal() +
  labs(x = "Time since RCT start", y = "WorkBC take-up (%)", color = "Condition") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") + 
  scale_color_manual(labels=c("Control", "Standard call x \n cool handoff",
                              "Standard call x \n warm handoff",
                              "Active Choice x \n cool handoff",
                              "Active Choice x \n warm handoff"),
                     values=c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442")) +
  scale_x_discrete(breaks=c("avg_0","avg_14","avg_30","avg_40","avg_50","avg_60","avg_70","avg_80","avg_90"),
                   labels=c("Day 0", "Day 14", "Day 30", "Day 40", 
                            "Day 50", "Day 60", "Day 70", "Day 80", "Day 90")) +
  annotate(geom="text", x=2.1, y=3.75, label="Reminder \n Emails") +
  annotate(geom="text", x=3, y=3.75, label="Trial \n End") + 
  annotate("segment", x = 2.1, xend = 2.1, y = 3.5, yend = 2.2, colour = "red", size=1.5, alpha=0.6, arrow=arrow())+ 
  annotate("segment", x = 3, xend = 3, y = 3.5, yend = 2.5, colour = "red", size=1.5, alpha=0.6, arrow=arrow())

# safe plot
tiff(paste(temp_dir_figures, "plot_enrollmenttime.tiff", sep=""),
     units="in", width=7.5, height=5, res=900, compression = "lzw")
plot_enrollmenttime
dev.off() 

#### <<>> Randomization Check (TABLE A2.3 & A2.5) ####

### TABLE A2.3: summarize variables by condition (balance check, pre-treatment covariates)
table_balance_check <- df_final %>% 
  select(condition, 
         age, gender_female, 
         education, ei_application_lapsed,
         ei_application_status, 
         trf_match_prior,
         enrollment_pretrial
  ) %>%
  group_by(condition) %>%
  get_summary_stats(.,
                    type = "common") 

#Save as table:
table_balance_check <- as.data.frame(table_balance_check)
write.csv(table_balance_check, paste(temp_dir_tables, "table_balance_check_final.csv", sep=""), row.names=FALSE)

#### TABLE A2.4 Email opens by condition

table_balance_check2 <- df_final %>%
  select("condition", "email_open_pre_adj") %>%
  group_by(condition) %>%
  get_summary_stats(.,
                    type = "common") 
  
#Save as table:
table_balance_check2 <- as.data.frame(table_balance_check2)
write.csv(table_balance_check2, paste(temp_dir_tables, "table_balance_check2_final.csv", sep=""), row.names=FALSE)
  

#### TABLE A2.5 Multinomial model for randomization check 

# regress condition on pre-treatment covariates.

rc_mod <- df_final %>%
  #mean centre pre-treatment covariates
  mutate(education_m = education - mean(education),
         gender_female_m = gender_female - mean(gender_female),
         ei_application_lapsed_m = ei_application_lapsed - mean(ei_application_lapsed),
         ei_application_status_m = ei_application_status - mean(ei_application_status),
         trf_match_prior_m = trf_match_prior - mean(trf_match_prior),
         age_m = age - mean(age),
         enrollment_pretrial_m = enrollment_pretrial - mean(enrollment_pretrial)) %>%
  ## multinomial reg
  multinom(condition ~ education_m + 
             gender_female_m +
             age_m + 
             ei_application_lapsed_m +
             ei_application_status_m + 
             trf_match_prior_m + 
             enrollment_pretrial_m,
           data=.)

# table for appendix
rc_mod_table <- tab_model(rc_mod, 
                          show.se=TRUE,
                          show.ci = FALSE,
                          collapse.se = TRUE,
                          dv.labels = c("DV: RCT condition"),
                          string.pred = "Coeffcient",
                          string.p = "P-Value",
                          digits = 3,
                          p.threshold = c(0.05),
                          p.style = "stars")
rc_mod_table #content for =table in paper appendix (TABLE A2.5)

## Test for joint orthogonality (test for significance of all pre-treatment covariats together)

# Fit the null model
null_model <- multinom(condition ~ 1, data = df_final)

# Perform the likelihood ratio test
lr_test <- lrtest(rc_mod, null_model)
print(lr_test)

#### <<>> Table A3.3 - LATE ####

# Email opens are the measure for compliance with our treatment.

# LATE for Standard long vs Control
model_late1 <- df_final %>%
  filter(condition=="Control" | condition=="Standard long") %>%
  mutate(email_open_pre_adj=case_when(
    email_open_pre_adj==1 ~ 1,
    TRUE ~0
  )) %>%
  estimatr::iv_robust(enrollment_prereminder ~ condition | email_open_pre_adj, 
                      data = ., 
                      diagnostics = T, 
                      se_type="HC1") 

# LATE for Standard short vs Control
model_late2 <- df_final %>%
  filter(condition=="Control" | condition=="Standard short") %>%
  mutate(email_open_pre_adj=case_when(
    email_open_pre_adj==1 ~ 1,
    TRUE ~0
  )) %>%
  estimatr::iv_robust(enrollment_prereminder ~ condition | email_open_pre_adj, 
                      data = ., 
                      diagnostics = T, 
                      se_type="HC1") 

# LATE for AC long vs Control
model_late3 <- df_final %>%
  filter(condition=="Control" | condition=="AC long") %>%
  mutate(email_open_pre_adj=case_when(
    email_open_pre_adj==1 ~ 1,
    TRUE ~0
  )) %>%
  estimatr::iv_robust(enrollment_prereminder ~ condition | email_open_pre_adj, 
                      data = ., 
                      diagnostics = T, 
                      se_type="HC1") 


# LATE for AC short vs Control
model_late4 <- df_final %>%
  filter(condition=="Control" | condition=="AC short") %>%
  mutate(email_open_pre_adj=case_when(
    email_open_pre_adj==1 ~ 1,
    TRUE ~0
  )) %>%
  estimatr::iv_robust(enrollment_prereminder ~ condition | email_open_pre_adj, 
                      data = ., 
                      diagnostics = T, 
                      se_type="HC1") 

# table for appendix
late_table <- tab_model(model_late1, model_late2, model_late3, model_late4, 
                        show.se=TRUE,
                        show.ci = FALSE,
                        collapse.se = TRUE,
                        dv.labels = c("Standard call x cool handoff",
                                      "Standard call x warm handoff",
                                      "AC x cool handoff",
                                      "AC x warm handoff"),
                        string.pred = "Coeffcient",
                        string.p = "P-Value",
                        digits = 3,
                        p.threshold = c(0.05),
                        p.style = "stars")
late_table #content for =table in paper appendix.

#### <<>> Table A3.5 Web Tracking Analytics ####

# Note: The entries in the table come from the following:
table_outcome_by_condition

# t-test click throughs between Standard and Active Choice:
df_final %>%
  mutate(AC_Dummy = case_when(
    condition=="AC long" | condition=="AC short" ~ "AC",
    condition=="Standard long" | condition=="Standard short" ~ "Standard",
    TRUE ~ NA # Control group
  )) %>%
  t.test(click_through_prer_adj ~ AC_Dummy, data = ., var.equal = TRUE)

# t-test short form submissions
df_final %>%
  mutate(sf_dummy = case_when(
    condition=="AC short" ~ "AC",
    condition=="Standard short" ~ "Standard",
    TRUE ~ NA # Control group
  )) %>%
  t.test(sf_submission_prer ~ sf_dummy, data = ., var.equal = TRUE)

#### <<>> Conversion Rate Analysis (SMIII.4) ####

# To obtain the results reported in this section of the appendix, run:

df_final %>%
  filter(condition!="control" & click_through_prer_adj==1) %>%
  mutate(sf_Dummy = case_when(
    condition=="AC short" | condition=="Standard short" ~ "warm",
    condition=="AC long" | condition=="Standard long" ~ "cool",
    TRUE ~ NA # Control group
  )) %>%
  t.test(enrollment_prereminder ~ sf_Dummy, data = ., var.equal = TRUE)

#### <<>> Enrolment in WorkBC in Warm Handoff Conditions (SMIII.5) ####

# Do street-level bureaucrats use discretion?

#### Interest: short form submissions

#all results reported in-test in SMIII.5

#by education
df_final %>%
  filter(condition=="Standard short" & sf_submission_prer==1 |
           condition=="AC short" & sf_submission_prer==1) %>%
  group_by(education) %>%
  #t-test mentioned in-text
  t.test(enrollment_prereminder ~ education, data = ., var.equal = TRUE)

# add mean
df_final %>%
  filter(condition=="Standard short" & sf_submission_prer==1 |
           condition=="AC short" & sf_submission_prer==1) %>%
  group_by(education)%>%
  summarize(avg = mean(enrollment_prereminder) )  

# by gender
df_final %>%
  filter(condition=="Standard short" & sf_submission_prer==1 |
           condition=="AC short" & sf_submission_prer==1) %>%
  group_by(gender_female) %>%
  summarize(avg = mean(enrollment_prereminder))

# by age
df_final %>%
  mutate(age_group = case_when(
    age<31 ~ "Youth",
    TRUE ~ "Not youth"
  )) %>%
  filter(condition=="Standard short" & sf_submission_prer==1 |
           condition=="AC short" & sf_submission_prer==1) %>%
  group_by(age_group)  %>%
  summarize(avg = mean(enrollment_prereminder))

#### Interest: click throughs

# comparing enrollments across all conditions among those who clicked through

#by education
df_final %>%
  filter(click_through_prer_adj==1) %>%
  mutate(condition_dummy = case_when(
    condition=="Standard long" | condition=="AC long" ~ "long",
    TRUE ~ "short"
  )) %>%
  group_by(condition_dummy, education)%>%
  summarize(avg = mean(enrollment_prereminder)) 

# by gender
df_final %>%
  filter(click_through_prer_adj==1) %>%
  mutate(condition_dummy = case_when(
    condition=="Standard long" | condition=="AC long" ~ "long",
    TRUE ~ "short"
  )) %>%
  group_by(condition_dummy, gender_female) %>%
  summarize(avg = mean(enrollment_prereminder))

# by age
df_final %>%
  mutate(age_group = case_when(
    age<31 ~ "Youth",
    TRUE ~ "Not youth"
  ),
  condition_dummy = case_when(
    condition=="Standard long" | condition=="AC long" ~ "long",
    TRUE ~ "short"
  )) %>%
  filter(click_through_prer_adj==1) %>%
  group_by(condition_dummy, age_group)  %>%
  summarize(avg = mean(enrollment_prereminder))


#### <<>> Explicit preferences not to connect with WorkBC (SMIII.7) ####

# These analyses explore potential differences between participants in the 
# Active Choice conditions that decided to close the email without clicking on 
# a choice (no choice), the one's that clicked through ("Yes, I want to connect
# to WorkBC"), and the ones that clicked "No, I don't want to connect to WorkBC today"

# TABLE A3.12 Look at absolute N and relative percent of "No" in the AC conditions combined:
table_nochoice_rawnumbers <- df_final %>%
  filter(condition=="AC long" | condition=="AC short") %>%
  count(click_through_cat_prer_adj)%>%          # now required with changes to dplyr::count()
  mutate(prop = round(prop.table(n),4))



# TABLE A3.13 select participants in Active Choice conditions and analyze pre-treatment variable
#differences by "no choice," "yes", and "no"
table_nochoice_desc <- df_final %>%
  filter(condition=="AC long" | condition=="AC short") %>%
  group_by(click_through_cat_prer_adj) %>%
  summarize(avg_age = mean(age),
            sd_age = sd(age),
            avg_gender = mean(gender_female),
            sd_gender = sd(gender_female),
            avg_eilapse = mean(ei_application_lapsed),
            sd_eilapse = sd(ei_application_lapsed),
            avg_edu = mean(education),
            sd_edu = sd(education),
            avg_eistatus = mean(ei_application_status),
            sd_eistatus = sd(ei_application_status),
            avg_trf = mean(trf_match_prior),
            sd_trf = sd(trf_match_prior))

# TABLE A3.14 Multinomial Regression to explore demographic differences further.
ml_no_model <- df_final %>%
  filter(condition=="AC long" | condition=="AC short") %>%
  multinom(click_through_cat_prer_adj ~ education + age + gender_female +
             ei_application_lapsed + ei_application_status +
             trf_match_prior, data = .)

# table for appendix
mlno_mod_table <- tab_model(ml_no_model, 
                            show.se=TRUE,
                            show.ci = FALSE,
                            collapse.se = TRUE,
                            dv.labels = c("DV: AC Choice"),
                            string.pred = "Coeffcient",
                            string.p = "P-Value",
                            digits = 3,
                            p.threshold = c(0.05),
                            p.style = "stars")
mlno_mod_table #content for =table in paper appendix.

# Table: descriptives and MN regression:
table_nochoice_desc <- as.data.frame(cbind(table_nochoice_rawnumbers, table_nochoice_desc))
write.csv(table_nochoice_desc, paste(temp_dir_tables, "table_nochoice_desc3.csv", sep=""), row.names=FALSE)




#### <<>> Table A2.2 - MDES (NOTE: TIME INTENSIVE) ####

## Note: This code takes time and, because its simulation based, may yield
## results that differ from the results reported in Table A2.2

# N of sims
sims <- 100
# N of observations
N <- 9877
# N of effect size combinations
N_c <- 1000

# effect sizes
mdesT1 <- seq(from=0.002, to=0.03, by=0.002)
mdesT2 <- seq(from=0.002, to=0.03, by=0.002)
mdesT3 <- seq(from=0.002, to=0.03, by=0.002)
mdesT4 <- seq(from=0.002, to=0.03, by=0.002)

# Generate all combinations
all_combinations <- expand.grid(mdesT1 = mdesT1,
                                mdesT2 = mdesT2,
                                mdesT3 = mdesT3,
                                mdesT4 = mdesT4)

# Number of all possible combinations
total_combinations <- nrow(all_combinations)

# Randomly sample 1000 combinations
set.seed(123)  # Set seed for reproducibility
sampled_combinations <- all_combinations[sample(total_combinations, 1000, replace = FALSE), ]

#empty vectors for power
power.H1 <- rep(NA, N_c)
power.H2 <- rep(NA, N_c) 
power.H3 <- rep(NA, N_c) 
power.H4 <- rep(NA, N_c)

#empty vectors for randomly selected effect sizes
es.T1 <- rep(NA, N_c)
es.T2 <- rep(NA, N_c) 
es.T3 <- rep(NA, N_c) 
es.T4 <- rep(NA, N_c)

for (j in 1:N_c){ #set to run for 1,000 random combinations of possible effect sizes
  effect_size1 <- sampled_combinations[j,1]
  effect_size2 <- sampled_combinations[j,2]
  effect_size3 <- sampled_combinations[j,3]
  effect_size4 <- sampled_combinations[j,4]
  #vectors to store results from comparison of p-values vs Holm adjusted alphas
  power_res_H1 <- rep(NA, sims)
  power_res_H2 <- rep(NA, sims)
  power_res_H3 <- rep(NA, sims)
  power_res_H4 <- rep(NA, sims)
  # Function to simulate data and perform statistical test
  for (i in 1:sims){
    #Generate treatment effects
    Y0 <-  rbinom(n=N, size=1, prob=0.005) #assume 0.5% baseline enrollment in control
    tau_1 <- rbinom(n=N, size=1, prob=effect_size1)
    tau_2 <- rbinom(n=N, size=1, prob=effect_size2)
    tau_3 <- rbinom(n=N, size=1, prob=effect_size3)
    tau_4 <- rbinom(n=N, size=1, prob=effect_size4)
    
    # Outcomes for the four treatment arms
    Y1 <- Y0 + tau_1
    Y2 <- Y0 + tau_2
    Y3 <- Y0 + tau_3
    Y4 <- Y0 + tau_4
    
    #Random assignment of treatment
    Z.sim <- complete_ra(N=N, prob_each = c(.2, .2, .2, .2, .2))
    #Create additional variable that 
    
    #Generate data
    Y.sim <- Y0*(Z.sim=="T1") + Y1*(Z.sim=="T2") + Y2*(Z.sim=="T3") + Y3*(Z.sim=="T4") + Y4*(Z.sim=="T5")
    
    #Combine into dataframe
    frame.sim <- data.frame(Y.sim, Z.sim)
    
    # Perform analysis
    #regression model
    reg_m <- lm_robust(Y.sim ~ Z.sim, data = frame.sim, se_type="HC1")
    #marginal means
    emm_m <- emmeans(reg_m, ~ Z.sim)
    
    ## Set up contrasts
    C = c(1,0,0,0,0) #Control
    T1 = c(0,1,0,0,0) #T1 - standard-long
    T2 = c(0,0,1,0,0) #T2 - standard-short
    T3 = c(0,0,0,1,0) #T3 - AC-long
    T4 = c(0,0,0,0,1) #T4 - AC-short
    SC = (T1+T2)/2 # T Standard
    AC = (T3+T4)/2 # T AC
    LF = (T1+T3)/2 # T Long
    SF = (T2+T4)/2 # T Short
    
    ## Estimate contrasts
    contrasts_reg_m <- contrast(emm_m, 
                                method = list(
                                  #H1: any treatment will affect take-up relative to control
                                  "Standard long - Control" = T1 - C,
                                  "Standard short - Control" = T2 - C,
                                  "AC long - Control" = T3 - C,
                                  "AC short - Control" = T4 - C,
                                  #H2: Enrollment will be highest in AC-Short Form condition
                                  #"T4 - C" = T4 - C, does not need be included, comparison is part of H1
                                  "AC short - Standard long" = T4 - T1,
                                  "AC short - Standard short" = T4 - T2,
                                  "AC short - AC long" = T4 - T3,
                                  #H3: Higher enrollment in short form relative to long-form 
                                  "Short form - Long form" = SF - LF,
                                  #H4: Higher enrollment in active choice vs standard call to action
                                  "AC - Standard" = AC - SC
                                ), 
                                #add Holm adjustment for multiple tests
                                adjust="Holm" )
    
    ## Save tables
    df_contrasts_reg_m <- as.data.frame(contrasts_reg_m)
    
    # Significant results by hypothesis (1=sig; 0=not sig)
    #H1: at least on treatment > than control
    power_res_H1[i] <- ifelse(is.na(df_contrasts_reg_m$p.value[1]) |
                                is.na(df_contrasts_reg_m$p.value[2]) |
                                is.na(df_contrasts_reg_m$p.value[3]) |                                    
                                is.na(df_contrasts_reg_m$p.value[4]) 
                              ,0,
                              df_contrasts_reg_m$p.value[1]<0.05 |
                                df_contrasts_reg_m$p.value[2]<0.05 |
                                df_contrasts_reg_m$p.value[3]<0.05|
                                df_contrasts_reg_m$p.value[4]<0.05)
    #H2: AC short = largest positive effect
    power_res_H2[i] <- ifelse(is.na(df_contrasts_reg_m$p.value[4]) |
                                is.na(df_contrasts_reg_m$p.value[5]) |
                                is.na(df_contrasts_reg_m$p.value[6]) |                                    
                                is.na(df_contrasts_reg_m$p.value[7]) 
                              ,0,
                              df_contrasts_reg_m$p.value[4]<0.05 &
                                df_contrasts_reg_m$p.value[5]<0.05 &
                                df_contrasts_reg_m$p.value[6]<0.05 &
                                df_contrasts_reg_m$p.value[7]<0.05)
    #H3 short form > than long form
    power_res_H3[i] <- ifelse(is.na(df_contrasts_reg_m$p.value[8]), 
                              0, df_contrasts_reg_m$p.value[8]<0.05)
    #H4 AC > Standard
    power_res_H4[i] <- ifelse(is.na(df_contrasts_reg_m$p.value[9]), 
                              0, df_contrasts_reg_m$p.value[9]<0.05)
  }
  #power
  power.H1[j] <- mean(power_res_H1) 
  power.H2[j] <- mean(power_res_H2)
  power.H3[j] <- mean(power_res_H3)
  power.H4[j] <- mean(power_res_H4)
  #effect size combination
  es.T1[j] <- effect_size1
  es.T2[j] <- effect_size2
  es.T3[j] <- effect_size3
  es.T4[j] <- effect_size4
  print(j) 
  
}


df_sim <- as.data.frame(cbind(power.H1, power.H2, power.H3, power.H4,
                              es.T1, es.T2, es.T3, es.T4))  

df_sim %>%
  filter(power.H1>=0.8 & power.H2>=0.8 & power.H3>=0.8 & power.H4>=0.8 )
summarize(min_es1 = min(es.T1),
          min_es2 = min(es.T2),
          min_es3 = min(es.T3),
          min_es4 = min(es.T4))

########################### END OF R-SCRIPT ###############################