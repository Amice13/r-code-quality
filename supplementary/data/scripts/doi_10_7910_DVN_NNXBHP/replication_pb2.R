###############################################
# Replication Code for:                       #
# Oskooii, Lajevardi, Collingwood (PB)        #
# Opinion Shift and Stability                 #
# platform       x86_64-apple-darwin15.6.0    #
# arch           x86_64                       #
# os             darwin15.6.0                 #
# system         x86_64, darwin15.6.0         #
# version.string R version 3.5.1 (2018-07-02) #
# DATE: 5/6/2019                              #
###############################################

# Clear working environment #
rm(list =ls())

# Function to Check and Install CRAN Packages #
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Check to see if packages are installed, and then load them #
packages<-c("devtools","lubridate", "dplyr", "ggplot2", "car", "Zelig", "zeligverse",
            "pscl", "stargazer", "MASS", "xtable", "survey", "readxl",
            "MKmisc", "BiocManager", "grid", "stringr", "tidyverse", "data.table")

# Load Packages #
suppressWarnings(check.packages(packages))


#May need to install "limma" for latest version of R for Table B10
#BiocManager::install("limma")

##########################
# Installing from Github #
##########################

# If not installed, uncomment and run following line
#install_github("lorenc5/Rmturkcheck")
library(Rmturkcheck)

# If not installed, uncomment and run following line 
#install_github("PMassicotte/gtrendsR") 
library(gtrendsR)

###########################
# Load Necessary Datasets #
###########################

#Set your working directory first 

load ("replication_data.rdata")

# Examine Available Datasets #
objects()

############################################
#                 Table 1                  #
############################################

# Function tab_gen: returns 2 rows of proportions and counts
tab_gen <- function(x) {
  
  # x = vector (probably from dataset)
  props <- c ( round ( table(x) / sum(table(x)), 2), 1)
  counts <- c ( table(x), sum( table(x) ) )
  
  return ( rbind(props, counts) )
  
}

# Rbind wave variables together #
tab_gen_out <- rbind ( tab_gen(eo_add$eo_banimm),
                       tab_gen(eo_add$eo_banimm_w2),
                       tab_gen(w3$eo_banimm_w3) )

# Label Columns & Rows #
colnames(tab_gen_out) <- c("Str. Disagree", "Smw. Disagree", "Neither", "Smw. Agree", 
                           "Str. Agree", "Total")
rownames(tab_gen_out) <- c("Wave 1 prop.", "n1", "Wave 2 prop.", "n2", "Wave 3 prop.", "n3")
  
xtable(tab_gen_out, 
       caption = "Dependent Variable Distribution Across Waves. ``President Trump's executive
       order restricting immigration from Syria, Iran, Libya, Yemen, Somalia, and Chad – do
       you strongly agree (5), somewhat agree (4), neither agree nor disagree (3), somewhat
       disagree (2) or strongly disagree with this order (1)?''",
       lab = "dv_distribution")

##########################################################
#                        Table 2                         #
##########################################################

# Function: t_out -- extracts t-test values, etc. #

t_out <- function(test) {
  
  val1 <- test$estimate[1]
  val2 <- test$estimate[2] 
  stat <- test$statistic
  p.val <- test$p.value
  return(c ( val1, val2, stat, p.val) )
  
}

t1 <- t_out ( t.test(eo_add$eo_banimm, eo_add$eo_banimm_w2) )
t2 <- t_out ( t.test(eo_add$eo_banimm, w3$eo_banimm_w3) )
t3 <- t_out ( t.test(eo_add$eo_banimm_w2, w3$eo_banimm_w3) )

# Put together into Table #
ban_tests <- round ( rbind(t1, t2, t3), 3)
colnames(ban_tests) <- c("Mean x", "Mean y", "T Stat", "P Value")
rownames(ban_tests) <- c("Wave 1 v. Wave 2", "Wave 1 v. Wave 3", "Wave 2 v. Wave 3")

# Write to LaTeX #
xtable(ban_tests, caption="Difference of Means T-Tests of Ban Attitudes Between T1, T2, and T3.",
       label = "h1vh2", align = c("l", "r", "r", "r", "r"))

############################################################
#                         Table 3                          #
############################################################

#######
# GOP #
#######

gop <- na.omit(eo_add[, c("rep_p", "eo_banimm","eo_banimm_w2", "nw_hl","some_coll_less")])

# High Attention GOP Wave 1
gop1 <- with(gop[gop$rep_p==1 & gop$nw_hl==1,], sd(eo_banimm))

n1 <- table(is.na(gop[gop$rep_p==1 & gop$nw_hl==1,]$eo_banimm))

# High Attention GOP Wave 2
gop2 <- with(gop[gop$rep_p==1 & gop$nw_hl==1,], sd(eo_banimm_w2))

n2 <- table(is.na(gop[gop$rep_p==1 & gop$nw_hl==1,]$eo_banimm_w2))

# High Education GOP Wave 1
gop_b <- na.omit(eo_add[, c("rep_p", "eo_banimm","some_coll_less")]); dim(gop_b)

gop3 <- with(gop_b[gop_b$rep_p==1 & gop_b$some_coll_less==0,], sd(eo_banimm))

n3 <- table(is.na(gop_b[gop_b$rep_p==1 & gop_b$some_coll_less==0,]$eo_banimm))

# High Education GOP Wave 2 #
gop4 <- with(gop[gop$rep_p==1 & gop$some_coll_less==0,], sd(eo_banimm_w2))

n4 <- table(is.na(gop[gop$rep_p==1 & gop$some_coll_less==0,]$eo_banimm_w2))

# Combine into Data Frame #
gop_dat <- data.frame(attn1 = paste(round(gop1,2)," (",n1, ")", sep=""),
                      attn2 = paste(round(gop2,2)," (",n2, ")", sep=""), 
                      diff1 = (gop2-gop1), 
                      educ1 = paste(round(gop3,2)," (",n3, ")", sep=""), 
                      educ2 = paste(round(gop4,2)," (",n4, ")", sep=""),  
                      diff2 = (gop4-gop3))

# Label Columns #
colnames(gop_dat) <- c("Attention W1 SD", "Attention W2 SD","Att Diff", 
                       "College W1 SD", "College W2 SD", "College Diff")

############
# Democrat #
############

dem <- na.omit(eo_add[, c("dem_p", "eo_banimm","eo_banimm_w2","some_coll_less", "nw_hl")]); dim(dem)

# High Attention GOP Wave 1
dem1 <- with(dem[dem$dem_p==1 & dem$nw_hl==1,], sd(eo_banimm))

n1 <- table(is.na(dem[dem$dem_p==1 & dem$nw_hl==1,]$eo_banimm))

# High Attention GOP Wave 2
dem2 <- with(dem[dem$dem_p==1 & dem$nw_hl==1,], sd(eo_banimm_w2))

n2 <- table(is.na(dem[dem$dem_p==1 & dem$nw_hl==1,]$eo_banimm_w2))

# High Education Dem Wave 1
dem_b <- na.omit(eo_add[, c("dem_p", "eo_banimm","some_coll_less")]); dim(dem_b)
dem3 <- with(dem_b[dem_b$dem_p==1 & dem_b$some_coll_less==0,], sd(eo_banimm))

n3 <- table(is.na(dem_b[dem_b$dem_p==1 & dem_b$some_coll_less==0,]$eo_banimm))

# High Education dem Wave 2 #
dem4 <- with(dem[dem$dem_p==1 & dem$some_coll_less==0,], sd(eo_banimm_w2))

n4 <- table(is.na(dem[dem$dem_p==1 & dem$some_coll_less==0,]$eo_banimm_w2))

dem_dat <- data.frame(attn1 = paste(round(dem1,2)," (",n1, ")", sep=""),
                      attn2 = paste(round(dem2,2)," (",n2, ")", sep=""), 
                      diff1 = (dem2-dem1), 
                      educ1 = paste(round(dem3,2)," (",n3, ")", sep=""), 
                      educ2 = paste(round(dem4,2)," (",n4, ")", sep=""),  
                      diff2 = (dem4-dem3))

colnames(dem_dat) <- c("Attention W1 SD", "Attention W2 SD","Att Diff", 
                       "College W1 SD", "College W2 SD", "College Diff")

xtable(rbind(gop_dat, dem_dat), caption = "Muslim Ban Standard Deviations Across Waves for ``High Attention'' and
       College Educated Citizens. Note: Row 1 reports results for Republican identifiers; row
       2 reports results for Democratic identifiers. Cell sample size is reported in parentheses.",
       label="ban_sd_party_attention")

#############################################
#                 Table B1                  #
#############################################

# Wave 1 - 3 #

wh <- t_out ( t.test(eo_add$white, w3$white) )
fem <- t_out ( t.test(eo_add$female, w3$female) )
age <- t_out ( t.test(eo_add$age_r, w3$age_r) )
edu <- t_out ( t.test(eo_add$some_coll_less, w3$some_coll_less) )
inc <- t_out ( t.test(eo_add$income_less_60, w3$income_less_60) )
party <- t_out ( t.test(eo_add$rep_ind_dem, w3$rep_ind_dem) )
Trump_app <- t_out ( t.test(eo_add$trump_app_r, w3$trump_app_r) )
ai <- t_out ( t.test(eo_add$american_identity, w3$american_identity) )

# Table-Making #

out <- round( rbind(wh, fem, age, edu, inc, party, Trump_app, ai), 3)
colnames(out) <- c("Wave 1", "Wave 3", "T-Stat", "P-Value")
row.names(out) <- c("White", "Female", "Age", "Education", "Income","Party ID", "Trump Approval","American Identity")

# Write to LaTeX #

xtable(out, lab="wave1_3_balance", align = c("l", "r","r","r","r"),
       caption="Wave 1 to Wave 3 Balance Table. Demographic Difference of Mean Comparisons
                Across Respondents.")

#############################################
#                 Table B2                  #
#############################################

# Wave 2 - 3 #

wh <- t_out ( t.test(eo_add2$white, w3$white) )
fem <- t_out ( t.test(eo_add2$female, w3$female) )
age <- t_out ( t.test(eo_add2$age_r, w3$age_r) )
edu <- t_out ( t.test(eo_add2$some_coll_less, w3$some_coll_less) )
inc <- t_out ( t.test(eo_add2$income_less_60, w3$income_less_60) )
party <- t_out ( t.test(eo_add2$rep_ind_dem, w3$rep_ind_dem) )
Trump_app <- t_out ( t.test(eo_add2$trump_app_r, w3$trump_app_r) )
ai <- t_out ( t.test(eo_add2$american_identity, w3$american_identity) )
watch_dem <- t_out( t.test(eo_add2$watch_dem_dummy, w3$watch_dem_dummy))

# Table Making #
out <- round( rbind(wh, fem, age, edu, inc, party, Trump_app, ai, watch_dem), 3)
colnames(out) <- c("Wave 2", "Wave 3", "T-Stat", "P-Value")
row.names(out) <- c("White", "Female", "Age", "Education", "Income","Party ID", "Trump Approval","American Identity",
                    "Watched Demonstrations")
# Write to LaTeX #

xtable(out, lab="wave2_3_balance", align = c("l", "r","r","r","r"),
       caption="Wave 2 to Wave 3 Balance Table. Demographic Difference of Mean Comparisons
                Across Respondents.")

#############################################
#                 Table B3                  #
#############################################

# Wave 1 #
sum_w1 <- dplyr::select ( eo_add,
                          eo_banimm,
                          american_identity,
                          some_coll_less,
                          income_less_60,
                          dem_p, 
                          rep_p,
                          white,
                          female,
                          age_r,
                          trump_app_r,
                          muslim_scale)

# Execute Summary Function #
sum_w1 <- sum_func(sum_w1)

# Label Rows #
rownames(sum_w1) <- c("Muslim Ban", "American Identity", "Some College or Less", "Income less than $60",
                      "Democrat", "Republican", "White", "Female", "Age", "Trump Approval", "Muslim Scale")

# Title #
cap <- "Summary Statistics, Wave 1"

# Write to LaTeX #
xtable (sum_w1, caption=cap, align = c("l", rep("r", 5)), label = "sum_func_w1")

#############################################
#                 Table B4                  #
#############################################

#  Wave 2 #
sum_w2 <- dplyr::select ( eo_add2,
                          eo_banimm_w2,
                          american_identity,
                          some_coll_less,
                          income_less_60,
                          dem_p, 
                          rep_p,
                          white,
                          female,
                          age_r,
                          trump_app_r,
                          muslim_scale)

# Execute Summary Function #
sum_w2 <- sum_func(sum_w2)
rownames(sum_w2) <- c("Muslim Ban", "American Identity", "Some College or Less", "Income less than $60",
                      "Democrat", "Republican", "White", "Female", "Age", "Trump Approval", "Muslim Scale")

cap <- "Summary Statistics, Wave 2"
xtable (sum_w2, caption=cap, align = c("l", rep("r", 5)), label = "sum_func_w2")

#############################################
#                 Table B5                  #
#############################################

#  Wave 3 #
sum_w3 <- w3[, c("eo_banimm_w3", "american_identity","some_coll_less",
                 "income_less_60","dem_p", "rep_p","white","female",
                 "age_r","trump_app_r","muslim_scale")]

sum_w3 <- sum_func(sum_w3)

# Label Rows #
rownames(sum_w3) <- c("Muslim Ban", "American Identity", "Some College or Less", "Income less than $60",
                      "Democrat", "Republican", "White", "Female", "Age", "Trump Approval", "Muslim Scale")

# Title #
cap <- "Summary Statistics, Wave 3"

# Write out to LaTeX 
xtable (sum_w3, caption=cap, align = c("l", rep("r", 5)), label = "sum_func_w3")

#############################################
#                 Table B6                  #
#############################################

gen_survey_props <- function(vector) {
  
  if (!is.vector(vector)) stop("Input must be vector")
  
  x1 <- prop.table(table(vector))*100
  
  return (as.vector(x1))
  
}

weight_to <- function(vector,names) {
  
  if (!is.vector(vector)) stop("Input must be vector")
  
  names(vector) <- names
  
  return(vector)
  
}

# Subset to just voters
voters <- eo_add[eo_add$voted_2016 <4,]

# Age on age category
voters$age_cat <- car::recode(voters$age_r, "18:35=1; 35:50=2; 51:82=3; else=NA")

# Weight Proportions use Rweights3 and/or functions above#

weight_props_gender <- weight_to(c(48,52), c("Male","Female"))
weight_props_rep_ind_dem <- weight_to(c(32,33,35), c("Rep","Ind","Dem"))
weight_props_white <- weight_to(c(28,72), c("NonWhite", "White"))
weight_props_educ <- weight_to(c(26,74), c("College Plus", "Some College or Less"))
weight_props_age <- weight_to(c(31,23, 46), c("18-35", "36-50", "51+"))

# Existing Proportions #
female_current <- gen_survey_props(voters$female)
party_current <- gen_survey_props(voters$rep_ind_dem)
white_current <- gen_survey_props(voters$white)
educ_current <- gen_survey_props(voters$some_coll_less)
age_current <- gen_survey_props(voters$age_cat)

# Create Survey and CCES Vectors #
surv <- c(female_current, white_current, educ_current, age_current, party_current) # Turk data
cces <- c(weight_props_gender, weight_props_white, weight_props_educ, weight_props_age, weight_props_rep_ind_dem) #cces data

# Bring in CPS data #
# Current Population Survey, Annual Social and Economic Supplement, 2017																	
# Source: U.S. Census Bureau																	
# Demographic benchmarks #

CPS <- c(49, 51, 39, 61, 24.2, 75.8, 31.6, 24.6, 43.8, rep(NA, 3))

# Combine #
unweight <- data.frame(surv, cces, CPS, cces-surv, CPS - surv)

# Label Columns #
colnames(unweight) <- c("MTurk" ,"CCES '16","CPS '17", "MTurk-CCES", " MTURK-CPS")

# Write out to LaTeX #
xtable(unweight, caption="Comparison of Unweighted Mechanical Turk (MTurk), Cooperative Congressional
                        Elections Survey 2016 (CCES), and Current Population Survey 2017 (CPS)
                        Data", 
                        label="cces_mturk_compare", align= c("l", "r", "r", "r", "r", "r"))

################################################
#                   Table B7                   #
################################################

# Ban Model: Wave 1: 

summary(ban1 <- lm(eo_banimm ~ american_identity +
                     some_coll_less + income_less_60 +
                     dem_p + rep_p + white + female + age_r +
                     trump_app_r + muslim_scale, data = eo_add))

# Ban Model: Wave 2: 

summary(ban2 <- lm(eo_banimm_w2 ~ american_identity + 
                     some_coll_less + income_less_60 +
                     dem_p + rep_p + white + female + age_r +
                     trump_app_r + muslim_scale, data = eo_add))

# Ban Model: Wave 3: 

summary(ban3 <- lm(eo_banimm_w3 ~ american_identity + 
                     some_coll_less + income_less_60 + 
                     dem_p + rep_p + white + female + age_r + 
                     trump_app_r + muslim_scale, data = w3))

covs <- c("American Identity Wave 1", "Some College or Less", "Income less 60K",
          "Democrat", "Republican", "White", "Female", "Age", 
          "Trump Approval Wave 1", "Muslim Favorability Scale", "Constant")

cap <- "Predictors of Muslim Ban Attitudes (OLS)"
stargazer(ban1, ban2, ban3, covariate.labels=covs, label="ban_1_ban_2_ban_3", title=cap,
          star.cutoffs = c(0.05, 0.01, 0.001), 
          column.labels=c("Wave 1", "Wave 2", "Wave 3"), 
          dep.var.labels = c("Ban Attitude", "Ban Attitude", "Ban Attitude") )

################################################
#                   Table B8                   #
################################################

# Ban Model: Wave 1: 

summary(ban1 <- lm(eo_banimm ~ american_identity +
                     some_coll_less + income_less_60 +
                     dem_p + rep_p + white + female + age_r +
                     trump_app_r + muslim_scale, data = eo_add[eo_add$nw_hl==1,]))

# Ban Model: Wave 2: 

summary(ban2 <- lm(eo_banimm_w2 ~ american_identity + 
                     some_coll_less + income_less_60 +
                     dem_p + rep_p + white + female + age_r +
                     trump_app_r + muslim_scale, data = eo_add[eo_add$nw_hl==1,]))

# Ban Model: Wave 3: 

summary(ban3 <- lm(eo_banimm_w3 ~ american_identity + 
                     some_coll_less + income_less_60 + 
                     dem_p + rep_p + white + female + age_r + 
                     trump_app_r + muslim_scale, data = w3[w3$nw_hl==1,]))

# Ban Model: Wave 1 -- watch demonstrations: 

summary(ban4 <- lm(eo_banimm ~ american_identity +
                     some_coll_less + income_less_60 +
                     dem_p + rep_p + white + female + age_r +
                     trump_app_r + muslim_scale, data = eo_add[eo_add$watch_dem_dummy==1,]))

# Ban Model: Wave 2 -- watch demonstrations: 

summary(ban5 <- lm(eo_banimm_w2 ~ american_identity + 
                     some_coll_less + income_less_60 +
                     dem_p + rep_p + white + female + age_r +
                     trump_app_r + muslim_scale, data = eo_add[eo_add$watch_dem_dummy==1,]))

# Ban Model: Wave 3 -- watch demonstrations: #

summary(ban6 <- lm(eo_banimm_w3 ~ american_identity + 
                     some_coll_less + income_less_60 + 
                     dem_p + rep_p + white + female + age_r + 
                     trump_app_r + muslim_scale, data = w3[w3$watch_dem_dummy==1,]))

#    LaTeX Table Making     #

covs <- c("American Identity Wave 1", "Some College or Less", "Income less 60K",
          "Democrat", "Republican", "White", "Female", "Age", 
          "Trump Approval Wave 1", "Muslim Favorability Scale", "Constant")

cap <- "Predictors of Muslim Ban Attitudes (OLS) subset to those who read news or
watched local or national news (Models 1:3) and those who reported watching demonstrations
on TV or reading about them on the internet (Models 4:6)"
stargazer(ban1, ban2, ban3, ban4, ban5, ban6,
          covariate.labels=covs, label="ban_media", title=cap,
          star.cutoffs = c(0.05, 0.01, 0.001), 
          column.labels=c("News W1", "News W2", "News W3", "Watch Demon W1", 
                          "Watch Demon W2", "Watch Demon W3"), 
          dep.var.labels = rep("Ban", 6) )

################################################
#                   Table B9                   #
################################################

summary(ban1o <- polr(as.factor(eo_banimm) ~ american_identity +
                        some_coll_less + income_less_60 +
                        dem_p + rep_p + white + female + age_r + 
                        trump_app_r + muslim_scale, 
                      data = eo_add))
pR2(ban1o)[1]
AIC(ban1o)

summary(ban2o <- polr(as.factor(eo_banimm_w2) ~ american_identity + 
                        some_coll_less + income_less_60 +
                        dem_p + rep_p + white + female + age_r + 
                        trump_app_r + muslim_scale, data = eo_add))

pR2(ban2o)[1]
AIC(ban2o)

summary(ban3o <- polr(as.factor(eo_banimm_w3) ~ american_identity + 
                        some_coll_less + income_less_60 +
                        dem_p + rep_p + white + female + age_r + 
                        trump_app_r + muslim_scale, data = w3))

pR2(ban3o)[1]
AIC(ban3o)

covs <- c("American Identity Wave 1", "Some College or Less", "Income less 60K",
          "Democrat", "Republican", "White", "Female", "Age", 
          "Trump Approval Wave 1", "Muslim Favorability Scale", "Cut 1", "Cut 2", "Cut 3", "Cut 4")
cap <- "Predictors of Muslim Ban Attitudes (Ordered Logit)"

# Write to LaTeX #
stargazer(ban1o, ban2o, ban3o, covariate.labels=covs, label="ban_1_ban_2_ban_3_o", 
          title=cap, ord.intercepts = T, star.cutoffs = c(0.05, 0.01, 0.001), 
          column.labels=c("Wave 1", "Wave 2", "Wave 3"), 
          dep.var.labels = c("Ban Attitude", "Ban Attitude", "Ban Attitude") )

################################################
#                   Table B10                  #
################################################

# Wave 2 v. 3 #
imp_dat_w3 <- imp_dat_w3_a[, c("eo_banimm","eo_banimm_w2", "eo_banimm_w3", "american_identity", 
                               "some_coll_less", "income_less_60","dem_p", "rep_p", "white", 
                               "female", "age_r", "trump_app_r", "muslim_scale")]
# Imputation #
set.seed(19485)
imp_dat_w3a <- amelia(x = imp_dat_w3, noms = c("some_coll_less", "income_less_60", "dem_p", "rep_p", "white", "female","trump_app_r"),
                      ords = c("eo_banimm_w3","eo_banimm_w2", "eo_banimm"))

# Conduct T-Tests #
t1_1 <- t_out ( mi.t.test(imp_dat_w3a$imputations, x="eo_banimm", y = "eo_banimm_w2", paired=T) )
t2_1 <- t_out ( mi.t.test(imp_dat_w3a$imputations, x="eo_banimm", y = "eo_banimm_w3", paired=T) )
t3_1 <- t_out ( mi.t.test(imp_dat_w3a$imputations, x="eo_banimm_w2", y = "eo_banimm_w3", paired=T) )

# Wave 1 v. 3 #
imp_dat_w3 <- imp_dat_w3_b[, c("eo_banimm","eo_banimm_w2", "eo_banimm_w3", "american_identity", "some_coll_less", "income_less_60",
                             "dem_p", "rep_p", "white", "female", "age_r", "trump_app_r", "muslim_scale")]

# Imputation #
set.seed(2975689)
imp_dat_w3a <- amelia(x = imp_dat_w3, noms = c("some_coll_less", "income_less_60", "dem_p", "rep_p", "white", "female","trump_app_r"),
                      ords = c("eo_banimm_w3","eo_banimm_w2", "eo_banimm"))

# Conduct T-Tests #
t1_2 <- t_out ( mi.t.test(imp_dat_w3a$imputations, x="eo_banimm", y = "eo_banimm_w2", paired=T) )
t2_2 <- t_out ( mi.t.test(imp_dat_w3a$imputations, x="eo_banimm", y = "eo_banimm_w3", paired=T) )
t3_2 <- t_out ( mi.t.test(imp_dat_w3a$imputations, x="eo_banimm_w2", y = "eo_banimm_w3", paired=T) )

# Combine T-Tests #
t_comb <- rbind(t1_1, t2_1, t3_1, t1_2, t2_2, t3_2)

# Label Columns & Rows #
colnames(t_comb) <- c("Mean Diff", "SD of Diff", "T-Stat", "P-value")
row.names(t_comb) <- c ("Impute W1: Wave 1 v. Wave 2", "Impute W1: Wave 1 v. Wave 3", "Impute W1: Wave 2 v. Wave 3",
                        "Impute W2: Wave 1 v. Wave 2", "Impute W2: Wave 1 v. Wave 3", "Impute W2: Wave 2 v. Wave 3")

# Write out to LaTeX #
cap <- "Difference of Means T-Tests of Ban Attitudes between T1, T2, and T3. Based
off Imputations from Wave 1 and Wave 2."
xtable(t_comb, align = c("l", "r", "r", "r", "r"), caption=cap,
       label="tstat_impute")

################################################
#                   Table B11                  #
################################################

# Pipeline #
tp <- t_out (t.test(eo_add$eo_keystone, w3$eo_keystone_w3) )

# Ban Wall #
tw <- t_out (t.test(eo_add$eo_wall, w3$eo_wall_w3) )

# Put together into Table
placebo_tests <- round ( rbind(tp, tw), 3)
colnames(placebo_tests) <- c("Mean Wave 1", "Mean Wave 3", "T Stat", "P Value")
rownames(placebo_tests) <- c("Placebo: Keystone Pipeline", "Placebo: Border Wall"); placebo_tests

# Write to LaTeX #

xtable(placebo_tests, caption="Placebo Tests. Keystone Pipeline and 
                               Southern Border Wall Attitudes. Difference
                               of Means T-Tests (no statistically significant 
                               change from T1 to T3).",
       label = "placebo_t_test", align = c("l", "r", "r", "r", "r"))



################################################
#                   Table B12                  #
################################################


#################################
#    Weighting Surveys: CCES    #
#################################

# Wave 1 #
w1 <- dplyr::select(eo_add, eo_banimm, american_identity, some_coll_less, income_less_60,
                    dem_p, rep_p, white, female, age_r, trump_app_r, muslim_scale, 
                    age_cat, rep_ind_dem)
w1 <- na.omit(w1); dim(w1) # Omit missing observations in order to weight

# Set base weight to 1
w1$weight_cces <- 1 

# Create svydesign object
des_w1 <- svydesign(id = ~1, weights=~weight_cces, data = w1)

#####################
# CCES Demographics #
#####################

# Create Population Target Proportions #
pop.types <- data.frame(female = c(0, 1), Freq = c(nrow(w1)*.48, nrow(w1)*.52)) # Sex
pop.types_race <- data.frame(white = c(0, 1), Freq = c(nrow(w1)*.28, nrow(w1)*.72)   ) # Race
pop.types_college <- data.frame(some_coll_less = c(0, 1), Freq = c(nrow(w1)*.26, nrow(w1)*.74)) # College
pop.types_age <- data.frame(age_cat = c(1:3), Freq = unlist ( c( nrow(w1)*(demos[7:9, 3]/100)) ))  #Age
pop.types_pid <- data.frame(rep_ind_dem = c(1:3), Freq =  unlist ( c( nrow(w1)*(demos[10:12, 3]/100)) ))

###############################################
# Rake the Veights or they get the hose again #
###############################################

des_w1 <- rake(des_w1, list(~female, ~white, ~some_coll_less, ~age_cat, ~rep_ind_dem), 
               list(pop.types, pop.types_race, pop.types_college, pop.types_age, pop.types_pid) )

#################
# Wave 2: CCES  #
#################

w2 <- dplyr::select(eo_add, eo_banimm_w2, american_identity, some_coll_less, income_less_60,
                    dem_p, rep_p, white, female, age_r, trump_app_r, muslim_scale, 
                    age_cat, rep_ind_dem)
w2 <- na.omit(w2); dim(w2) # Omit missing observations #

# Set base weight to 1
w2$weight_cces <- 1 

# Create Survey Design Object #
des_w2 <- svydesign(id = ~1, weights=~weight_cces, data = w2)

#####################
# CCES Demographics #
#####################

pop.types <- data.frame(female = c(0, 1), Freq = c(nrow(w2)*.48, nrow(w2)*.52)) # Sex
pop.types_race <- data.frame(white = c(0, 1), Freq = c(nrow(w2)*.28, nrow(w2)*.72)   ) # Race
pop.types_college <- data.frame(some_coll_less = c(0, 1), Freq = c(nrow(w2)*.26, nrow(w2)*.74)) # College
pop.types_age <- data.frame(age_cat = c(1:3), Freq = unlist ( c( nrow(w2)*(demos[7:9, 3]/100)) ))  #Age
pop.types_pid <- data.frame(rep_ind_dem = c(1:3), Freq =  unlist ( c( nrow(w2)*(demos[10:12, 3]/100)) ))

####################
# Rake the Veights #
####################

des_w2 <- rake(des_w2, list(~female, ~white, ~some_coll_less, ~age_cat, ~rep_ind_dem), 
               list(pop.types, pop.types_race, pop.types_college, pop.types_age, pop.types_pid) )

#################
# Wave 3 Raking #
#################

w3$weight_cces <- 1 # Set base weight to 1
des_w3 <- svydesign(id = ~1, weights=~weight_cces, data = w3)

########################################
# CCES: Sex, Race, Education, Age, PID #
########################################

pop.types <- data.frame(female = c(0, 1), Freq = c(77.28,83.72)) # Sex
pop.types_race <- data.frame(white = c(0, 1), Freq = c(45.08,115.92)) # Race
pop.types_college <- data.frame(some_coll_less = c(0, 1), Freq = c(41.86, 119.14)) # College
pop.types_age <- data.frame(age_cat = 1:3, Freq =   nrow(w3) * ( as.vector(demos[7:9, 3])/100 )) #Age
colnames(pop.types_age)[2] <- "Freq"
pop.types_pid <- data.frame(rep_ind_dem = 1:3, Freq =   nrow(w3) * ( as.vector(demos[10:12, 3])/100 )) #REP-IND-DEM
colnames(pop.types_pid)[2] <- "Freq"

####################
# Rake the Weights #
####################

des_w3 <- rake(des_w3, list(~female, ~white, ~some_coll_less, ~age_cat, ~rep_ind_dem), 
               list(pop.types, pop.types_race, pop.types_college, pop.types_age, pop.types_pid) )

###########################
# Regressions with Weight #
###########################

# Ban Wave 1 #
summary ( ban1_cces <- svyglm(eo_banimm ~ american_identity +
                                some_coll_less + income_less_60 +
                                dem_p + rep_p + white + female + age_r + 
                                trump_app_r + muslim_scale,
                                data = w1, design=des_w1) )
# Ban Wave 2 #
summary(ban2_cces <- svyglm(eo_banimm_w2 ~ american_identity + 
                              some_coll_less + income_less_60 +
                              dem_p + rep_p + white + female + age_r + 
                              trump_app_r + muslim_scale, 
                              data = w2, design = des_w2))
# Ban Wave 3 #
summary(ban3_cces <- svyglm(eo_banimm_w3 ~ american_identity + 
                              some_coll_less + income_less_60 +
                              dem_p + rep_p + white + female + age_r + 
                              trump_app_r + muslim_scale, 
                              data = w3, design = des_w3))

######################################################
#    Weighting Surveys: Current Population Survey    #
######################################################

# Wave 1 #

# Create CPS Base Weight #
w1$weight_cps <- 1 

# Create Survey Sesign Object #
des_w1 <- svydesign(id = ~1, weights=~weight_cps, data = w1)

###########################################
# CPS Demographic Target Proportions 2017 #
# Sex, race, education, age               #
###########################################

pop.types <- data.frame(female = c(0, 1), Freq = c(nrow(w1)*.49, nrow(w1)*.51)) # Sex
pop.types_race <- data.frame(white = c(0, 1), Freq = c(nrow(w1)*.39, nrow(w1)*.61)   ) # Race
pop.types_college <- data.frame(some_coll_less = c(0, 1), Freq = c(nrow(w1)*.242, nrow(w1)*.758)) # College
pop.types_age <- data.frame(age_cat = c(1:3), Freq = unlist ( c( nrow(w1)*(demos[7:9, 4]/100)) ))  #Age

####################
# Rake the Weights #
####################

des_w1 <- rake(des_w1, list(~female, ~white, ~some_coll_less, ~age_cat), 
               list(pop.types, pop.types_race, pop.types_college, pop.types_age) )

#################
# Wave 2: CPS   #
#################

w2 <- dplyr::select(eo_add, eo_banimm_w2, american_identity, some_coll_less, income_less_60,
                    dem_p, rep_p, white, female, age_r, trump_app_r, muslim_scale, 
                    age_cat, rep_ind_dem)
w2 <- na.omit(w2); dim(w2)

w2$weight_cps <- 1 # Set base weight to 1
des_w2 <- svydesign(id = ~1, weights=~weight_cps, data = w2)

#####################
# CPS Demographics #
#####################
pop.types <- data.frame(female = c(0, 1), Freq = c(nrow(w2)*.49, nrow(w2)*.51)) # Sex
pop.types_race <- data.frame(white = c(0, 1), Freq = c(nrow(w2)*.39, nrow(w2)*.61)   ) # Race
pop.types_college <- data.frame(some_coll_less = c(0, 1), Freq = c(nrow(w2)*.242, nrow(w2)*.758)) # College
pop.types_age <- data.frame(age_cat = c(1:3), Freq = unlist ( c( nrow(w2)*(demos[7:9, 3]/100)) ))  #Age

####################
# Rake the Weights #
####################

des_w2 <- rake(des_w2, list(~female, ~white, ~some_coll_less, ~age_cat), 
               list(pop.types, pop.types_race, pop.types_college, pop.types_age) )

#################
# Wave 3 Raking #
#################

w3$weight_cps <- 1 # Set base weight to 1
des_w3 <- svydesign(id = ~1, weights=~weight_cps, data = w3)

# CPS #
pop.types <- data.frame(female = c(0, 1), Freq = c(nrow(w3)*.49, nrow(w3)*.51)) # Sex
pop.types_race <- data.frame(white = c(0, 1), Freq = c(nrow(w3)*.39, nrow(w3)*.61)   ) # Race
pop.types_college <- data.frame(some_coll_less = c(0, 1), Freq = c(nrow(w3)*.242, nrow(w3)*.758)) # College
pop.types_age <- data.frame(age_cat = c(1:3), Freq = unlist ( c( nrow(w3)*(demos[7:9, 3]/100)) ))  #Age


des_w3 <- rake(des_w3, list(~female, ~white, ~some_coll_less, ~age_cat), 
               list(pop.types, pop.types_race, pop.types_college, pop.types_age) )

###########################
# Regressions with Weight #
###########################

summary ( ban1_cps <- svyglm(eo_banimm ~ american_identity +
                               some_coll_less + income_less_60 +
                               dem_p + rep_p + white + female + age_r + 
                               trump_app_r + muslim_scale,
                             data = w1, design=des_w1) )

summary(ban2_cps <- svyglm(eo_banimm_w2 ~ american_identity + 
                             some_coll_less + income_less_60 +
                             dem_p + rep_p + white + female + age_r + 
                             trump_app_r + muslim_scale, 
                           data = w2, design = des_w2))

summary(ban3_cps <- svyglm(eo_banimm_w3 ~ american_identity + 
                             some_coll_less + income_less_60 +
                             dem_p + rep_p + white + female + age_r + 
                             trump_app_r + muslim_scale, 
                           data = w3, design = des_w3))

#############################
#    LaTeX Table Making     #
#############################

covs <- c("American Identity Wave 1", "Some College or Less", "Income less 60K",
          "Democrat", "Republican", "White", "Female", "Age", 
          "Trump Approval Wave 1", "Muslim Favorability Scale", "Constant")

cap <- "Predictors of Muslim Ban Attitudes (Generalized Linear Model Maximum
Likelihood Estimation), Weighted to Cooperative Congressional Election Studies 2016
(CCES) and Current Population Survey (2017) Demographics: Age, Sex, Race, Education,
and Party Identification."
stargazer(ban1_cces, ban2_cces, ban3_cces, ban1_cps, ban2_cps, ban3_cps,
          covariate.labels=covs, label="ban123_cces_cps", title=cap,
          star.cutoffs = c(0.05, 0.01, 0.001), 
          column.labels=c("CCES W1", "CCES W2", "CCES W3", "CPS W1", "CPS W2", "CPS W3"), 
          dep.var.labels = rep("Ban", 6) )

#########################################################################################################

#############################################
#                 FIGURE 1                  #
#############################################

# Date #
seg_date_count$date <- lubridate::ymd(seg_date_count$date)

# Initiate Plot #
plot(seg_date_count$date, seg_date_count$num_seg, pch=16,
     ylab="Number of Segments about Muslim Ban",
     xlab= "Date (2017)",
     main = "Muslim Ban CNN Segments Over Time",
     bty="n",
     ylim= c(0,100)
)

abline(v=as.Date("2017-01-27"), col="blue", lty=2, lwd=1)
text(as.Date("2017-01-10"), 100, "Ban 1.0", cex=.8)

abline(v=as.Date("2017-03-06"), col="blue", lty=2, lwd=1)
text(as.Date("2017-03-25"), 100, "Ban 2.0", cex=.8) # Ban 2.0

abline(v=as.Date("2017-06-26"), col="blue", lty=2, lwd=1)
text(as.Date("2017-07-26"), 100, "SCOTUS\nGrants Cert", cex=.8) # SCOTUS Cert

abline(v=as.Date("2017-09-24"), col="blue", lty=2, lwd=1)
text(as.Date("2017-10-24"), 100, "Ban 3.0", cex=.8) # Ban 3.0

#############################################
#                 FIGURE 2                  #
#############################################

# NOTE: This might time out at times #
lang_trend <- gtrends(keyword = c("Trump border wall", "Trump keystone pipeline", "Trump muslim ban", "Trump travel ban"),
                      time = "2017-01-01 2018-01-01",
                      geo = "US")  

plot(lang_trend)

#############################################
#                 FIGURE 3                  #
#############################################

# Date #
seg_date_count2$Date <- lubridate::ymd(seg_date_count2$Date)
  
# Initiate Plot #
plot(seg_date_count2$Date, seg_date_count2$Neutral, # Neutral #
     pch=16,
     ylab="Valence Statements about Muslim Ban",
     xlab= "Date (2017)",
     main = "Muslim Ban CNN Segments Over Time\nValence Statements",
     bty="n",
     ylim= c(0,50),
     type="l", lty=3, lwd=1.5, col="grey")

lines(seg_date_count2$Date, seg_date_count2[,4], lwd=2, lty=1, col="blue") # anti
lines(seg_date_count2$Date, seg_date_count2[,3], lwd=2, lty=2, col="brown") # pro

# Include legend #
legend("topright", 
       legend=c("Neutral/Info", "Anti-Ban", "Pro-Ban"),
       bty="n", 
       col=c("grey", "blue", "brown"),
       lty=c(3, 1, 2), lwd=c(1.5,2, 2))

#############################################
#                 FIGURE 4                  #
#############################################

##################################################
# func_sum -- takes splits list item from above  #
# sums 0, 1, and -1, NA gets 0                   #
##################################################

func_sum <- function(x) {
  
  sum0 <- sum(x$art_0, na.rm=T)
  
  if(is.na(sum0)) {
    sum0 <- 0
  }
  
  sum1 <- sum(x$art_1, na.rm=T)
  
  if(is.na(sum1)) {
    sum1 <- 0
  }
  
  sumNeg1 <- sum(x$art_neg1, na.rm=T)
  
  if(is.na(sumNeg1)) {
    sumNeg1 <- 0
  }
  
  n <- nrow(x)
  
  return(c(sum0, sum1, sumNeg1, n, sum0/n, sum1/n, sumNeg1/n))
  
}

#######################
# Split Data on Month #
#######################

np_s <- split(np, np$month) ; length(np_s)

###########################################
# Apply func_sum to list object           #
# returns a dataframe of counts per month #
###########################################

# Everything says week but it's month -- just initial weekly code that i switched to month
dist_filt <- plyr::ldply(np_s,  func_sum) #use ldply function from plyr package

# Label Columns #
colnames(dist_filt) <- c("month", "neutral", "anti_ban", "pro_ban", "n", "pneutral", "panti_ban", "ppro_ban")

#################
# Plot the Data #
#################

par(mar = c(5,5,2,4)) # adjust plotting margins 

plot(dist_filt$month, dist_filt$panti_ban, typ='l', bty='n', lwd=2, col="blue", # Anti-Ban Line
     ylim = c(0,1.3), yaxt='n',
     main = "Muslim Ban Newspaper Articles Over Time",
     xlab = "January                    Numeric Month                        December",
     ylab = "Proportion of Monthly Articles about Ban")
lines(dist_filt$month, dist_filt$pneutral, typ='l', col="gray", lwd=2, lty=3) # Neutral Line
lines(dist_filt$month, dist_filt$ppro_ban, typ='l', col="red", lwd=2, lty=2) # Pro Ban Line

axis(side=2, at = seq(0,1.2,.2), labels=c(seq(0,1,.2),"")) # Adjust left y axis

par(new = T) # open up right y-axis
plot(dist_filt$month, dist_filt$n, type='l', axes=F, xlab=NA, ylab=NA, lwd=2, lty=4)
axis(side = 4, at=c(0, 50, 100, 150, 200, 250, 300, 350), 
     labels = c(0, 50, 100, 150, 200, 250, 300, 350))
mtext(side = 4, line = 3, 'Number of Ban Articles Per Month', cex=.9)

legend("topright", lty=1:4, lwd=c(2,2,2,2), legend = c("Negative (anti-ban)", "Balanced/Informational", "Positive (pro-ban)", "# of Monthly Articles"),
       col=c("blue", "gray", "red", "black"), bty='n', cex=.75, title="EO Depiction")

#############################################
#                 FIGURE 5                  #
#############################################

# NOTE -- this put all figure into 2*2 throwing off the legend a bit. #
# For better visual produce one at a time #

# Unique source name (as appears in data)
sources <- c("NYT", "WAPO", "USA Today", "WSJ")

# Specific Labeling #
lab <- list( c(0,20,40,60,80, 100, 120, 140),
             c(0, 20, 40,60, 80),
             c(0, 5, 10,15, 20, 25, 30),
             c(0,20,40,60,80,100) )

# Initiate 2*2 plotting #
par(mfrow=c(2,2))

# Loop over each source #
for (i in 1: length ( unique(np$source))) {

  np_single <- np[np$source==sources[i],]

  np_s <- split(np_single, np_single$month) ; length(np_s)

  dist_filt <- plyr::ldply(np_s,  func_sum) #use ldply function from plyr package

  # Label Columns #
  colnames(dist_filt) <- c("month", "neutral", "anti_ban", "pro_ban", "n", "pneutral", "panti_ban", "ppro_ban")

  par(mar = c(5,5,2,4))
  plot(dist_filt$month, dist_filt$panti_ban, typ='l', bty='n', lwd=2, col="blue",
     ylim = c(0,1.3), yaxt='n',
     main = paste ("Muslim Ban ",sources[i], " Articles Over Time",sep=""),
     xlab = "January                    Numeric Month                        December",
     ylab = "Proportion of Monthly Articles about Ban")
  lines(dist_filt$month, dist_filt$pneutral, typ='l', col="gray", lwd=2, lty=3)
  lines(dist_filt$month, dist_filt$ppro_ban, typ='l', col="red", lwd=2, lty=2)

  axis(side=2, at = seq(0,1.2,.2), labels=c(seq(0,1,.2), ""))

  par(new = T)
  plot(dist_filt$month, dist_filt$n, type='l', axes=F, xlab=NA, ylab=NA, lwd=2, lty=4)
  axis(side = 4, at=lab[[i]], labels = lab[[i]])
  mtext(side = 4, line = 3, 'Number of Ban Articles Per Month', cex=.9)

  legend("topright", lty=1:4, lwd=c(2,2,2,2), legend = c("Negative (anti-ban)", "Balanced/Informational", "Positive (pro-ban)", "# of Monthly Articles"),
       col=c("blue", "gray", "red", "black"), bty='n', cex=.75, title="EO Depiction")

}

#############################################
#                 FIGURE 6                  #
#############################################

mv_extract_single <- function(s.out){
  
  myev <- s.out$get_qi(qi='ev')
  myev2 <- as.data.frame(matrix(unlist(myev), nrow =1000)) # Default is 1000 sims
  
  #This step is to create quantiles
  a<- apply(myev2, 2, quantile, probs = c(0.025,0.975, .05, .95, 0.25, 0.75)) 
  low <- a[1,]
  high <- a[2,]
  low_90 <- a[3,]
  high_90 <- a[4,]
  qt.1 <- a[5,]
  qt.3 <- a[6,]
  mean <- apply(myev2, 2, mean) 
  plotdata <- as.data.frame(cbind(low, high, mean, low_90, high_90, qt.1, qt.3))
  
  return(plotdata)

}

# Zelig Models #
summary(model_z <- zelig(eo_banimm ~ american_identity +
                           some_coll_less + income_less_60 +
                           dem_p + rep_p + white + female + age_r + 
                           trump_app_r + muslim_scale, 
                           data = eo_add, model="ls"))

x.low <- setx(model_z, american_identity = min(eo_add$american_identity, na.rm=T))
x.high <- setx(model_z, american_identity = max(eo_add$american_identity, na.rm=T))

# Run Simulation
s.out1 <- Zelig::sim(model_z, x = x.low)
s.out2 <- Zelig::sim(model_z, x = x.high)

w1pl <- mv_extract_single(s.out1)
w1pu <- mv_extract_single(s.out2)

##########
# Wave 2 #
##########

summary(model_z2 <- zelig(eo_banimm_w2 ~ american_identity + 
                            some_coll_less + income_less_60 +
                            dem_p + rep_p + white + female + age_r + 
                            trump_app_r + muslim_scale, data = eo_add, 
                            model="ls"))

x.low2 <- setx(model_z2, american_identity = min(eo_add$american_identity, na.rm=T))
x.high2 <- setx(model_z2, american_identity = max(eo_add$american_identity, na.rm=T))

# Run Simulation
s.out1 <- Zelig::sim(model_z2, x = x.low2)
s.out2 <- Zelig::sim(model_z2, x = x.high2)

w2pl <- mv_extract_single(s.out1)
w2pu <- mv_extract_single(s.out2)

##########
# Wave 3 #
##########

w3_r <- w3[, c("eo_banimm_w3","american_identity", "some_coll_less", 
               "income_less_60", "dem_p", "rep_p", "rep_ind_dem", "white", "female", 
               "age_r", "trump_app_r", "muslim_scale")]
w3_r <- na.omit(w3_r)

summary(model_z3 <- zelig(eo_banimm_w3 ~ american_identity + 
                            some_coll_less + income_less_60 +
                            dem_p + rep_p + white + female + age_r + 
                            trump_app_r + muslim_scale, data = w3_r, 
                            model="ls"))

x.low2 <- setx(model_z3, american_identity = min(w3_r$american_identity, na.rm=T))
x.high2 <- setx(model_z3, american_identity = max(w3_r$american_identity, na.rm=T))

# Run Simulation
s.out1 <- Zelig::sim(model_z3, x = x.low2)
s.out2 <- Zelig::sim(model_z3, x = x.high2)

w3pl <- mv_extract_single(s.out1)
w3pu <- mv_extract_single(s.out2)

# Initiate Plot #
dev.off() # from previous plot

plot(1:2, c( w1pl[3], w1pu[3]), 
     pch=5, xaxt="n",
     main="Attitudes on Muslim Ban",
     ylab = "E(Support Muslim Ban)",
     xlab = "Strength of American Identity\n(Measured Wave 1)",
     bty="n",
     ylim = c(2, 3.3))
points(1:2, c(w2pl[3], w2pu[3]), pch=6)
points(1:2, c(w3pl[3], w3pu[3]), pch=7)

lines(1:2, c( w1pl[3], w1pu[3]), lwd=2)
lines(1:2, c( w2pl[3], w2pu[3]), lwd=2, lty=2, col="brown")
lines(1:2, c(w3pl[3], w3pu[3]), lwd=2, lty=3, col="blue")

segments(1, w1pl$low_90, 1, w1pl$high_90, col="grey", lty=5)
segments(1, w2pl$low_90, 1, w2pl$high_90, col="grey", lty=5)
segments(1, w3pl$low_90, 1, w3pl$high_90, col="grey", lty=5)

segments(2, w1pu$low_90, 2, w1pu$high_90, col="grey", lty=5)
segments(2, w2pu$low_90, 2, w2pu$high_90, col="grey", lty=5)
segments(2, w3pu$low_90, 2, w3pu$high_90, col="grey", lty=5)

axis(1, at=c(1,1.5,2), labels=c("Low", "Mid", "High"))
text(1.9, 3.3, "Wave 1")
text(1.68, 2.77, "Wave 2")
text(1.45, 2.45, "Wave 3")

#############################################
#                 FIGURE 7                  #
#############################################

##################
# Party ID Plots #
##################

# Wave 1 #

# Take just American Identity Variable
iv_dat <- eo_add[, c("american_identity","rep_ind_dem")]
iv_dat <- na.omit(iv_dat) # Omit missing data 
iv_dat$dem_rep_ind <- car::recode(iv_dat$rep_ind_dem, "1=2; 2=3; 3=1")

facetedbars <- ggplot(data = iv_dat, aes(american_identity))  +
  ggtitle("Wave 1") +
  geom_histogram(aes(y =..density..*.5), binwidth = .5) +
  scale_y_continuous("Percent", labels = scales::percent) +
  scale_x_continuous("American Identity (low to high)", limits=c(2,22)) +
  scale_colour_identity() + 
  facet_wrap( ~ factor(dem_rep_ind, labels=c("Democrat", "Republican", "Independent")), 
              nrow=2)
p1 <- facetedbars + aes(fill = as.factor(dem_rep_ind)) + theme_minimal() +theme(legend.position = "none") 

# Wave 2 #

iv_dat <- eo_add2[, c("american_identity", "rep_ind_dem")]
iv_dat <- na.omit(iv_dat) # Omit missing data 
iv_dat$dem_rep_ind <- car::recode(iv_dat$rep_ind_dem, "1=2; 2=3; 3=1")

facetedbars <- ggplot(data = iv_dat, aes(american_identity))  +
  ggtitle("Wave 2") +
  geom_histogram(aes(y =..density..*.5), binwidth = .5) +
  scale_y_continuous("Percent", labels = scales::percent) +
  scale_x_continuous("American Identity (low to high)", limits=c(2,22)) +
  scale_colour_identity() + 
  facet_wrap( ~ factor(dem_rep_ind, labels=c("Democrat", "Republican", "Independent")), 
              nrow=2)
p2 <- facetedbars + aes(fill = as.factor(dem_rep_ind)) + theme_minimal() +theme(legend.position = "none") 

# Wave 3 #

iv_dat <- w3[, c("american_identity", "rep_ind_dem")]
iv_dat <- na.omit(iv_dat) # Omit missing data 
iv_dat$dem_rep_ind <- car::recode(iv_dat$rep_ind_dem, "1=2; 2=3; 3=1")

facetedbars <- ggplot(data = iv_dat, aes(american_identity))  +
  ggtitle("Wave 3") +
  geom_histogram(aes(y =..density..*.5), binwidth = .5) +
  scale_y_continuous("Percent", labels = scales::percent, limits=c(0,.30)) +
  scale_x_continuous("American Identity (low to high)", limits=c(2,22)) +
  scale_colour_identity() + 
  facet_wrap( ~ factor(dem_rep_ind, labels=c("Democrat", "Republican", "Independent")), 
              nrow=2)
p3 <- facetedbars + aes(fill = as.factor(dem_rep_ind)) + theme_minimal() +theme(legend.position = "none") 

suppressWarnings(multiplot(p1, p2, p3))

#############################################
#                 FIGURE 8                  #
#############################################

###################################################
# Party Identification Post-Estimation Simulation #
###################################################

# Wave 1 #
summary(model_z <- zelig(eo_banimm ~ american_identity +
                           some_coll_less + income_less_60 +
                           dem_p + rep_p + white + female + age_r + 
                           trump_app_r + muslim_scale, 
                         data = eo_add, model="ls"))

x.low <- setx(model_z, dem_p = 0, rep_p=1) # Rep 
x.high <- setx(model_z, dem_p=1, rep_p=0) # Dem

# Run Simulation
s.out1 <- Zelig::sim(model_z, x = x.low)
s.out2 <- Zelig::sim(model_z, x = x.high)

w1pl <- mv_extract_single(s.out1)
w1pu <- mv_extract_single(s.out2)

# Wave 2 #
summary(model_z2 <- zelig(eo_banimm_w2 ~ american_identity + 
                            some_coll_less + income_less_60 +
                            dem_p + rep_p + white + female + age_r + 
                            trump_app_r + muslim_scale, data = eo_add, 
                          model="ls"))

x.low2 <- setx(model_z2, dem_p = 0, rep_p=1) # Rep
x.high2 <- setx(model_z2, dem_p=1, rep_p=0) # Dem

# Run Simulation
s.out1 <- Zelig::sim(model_z2, x = x.low2)
s.out2 <- Zelig::sim(model_z2, x = x.high2)

w2pl <- mv_extract_single(s.out1)
w2pu <- mv_extract_single(s.out2)

# Subset for now
w3_r <- w3[, c("eo_banimm_w3","american_identity", "some_coll_less", 
               "income_less_60", "dem_p", "rep_p", "white", "female", 
               "age_r", "trump_app_r", "muslim_scale", "rep_ind_dem")] #"vote_trump", 
w3_r <- na.omit(w3_r); dim(w3_r)

summary(model_z3 <- zelig(eo_banimm_w3 ~ american_identity + 
                            some_coll_less + income_less_60 +
                            dem_p + rep_p + white + female + age_r + 
                            trump_app_r + muslim_scale, data = w3_r, 
                            model="ls"))

x.low2 <- setx(model_z3, dem_p = 0, rep_p=1) # Rep
x.high2 <- setx(model_z3, dem_p=1, rep_p=0) # Dem

# Run Simulation
s.out1 <- Zelig::sim(model_z3, x = x.low2)
s.out2 <- Zelig::sim(model_z3, x = x.high2)

w3pl <- mv_extract_single(s.out1)
w3pu <- mv_extract_single(s.out2)

plot(1:2, c( w1pl[3], w1pu[3]), 
     pch=5, xaxt="n",
     main="Attitudes on Muslim Ban\nEffect of Party Identification",
     ylab = "E(Support Muslim Ban)",
     xlab = "Party Identification\n(Measured Wave 1)",
     bty="n",
     ylim = c(2, 3.4))
points(1:2, c(w2pl[3], w2pu[3]), pch=6)
points(1:2, c(w3pl[3], w3pu[3]), pch=7)

lines(1:2, c( w1pl[3], w1pu[3]), lwd=2)
lines(1:2, c( w2pl[3], w2pu[3]), lwd=2, lty=2, col="brown")
lines(1:2, c(w3pl[3], w3pu[3]), lwd=2, lty=3, col="blue")

segments(1, w1pl$low_90, 1, w1pl$high_90, col="grey", lty=5)
segments(1, w2pl$low_90, 1, w2pl$high_90, col="grey", lty=5)
segments(1, w3pl$low_90, 1, w3pl$high_90, col="grey", lty=5)

segments(2, w1pu$low_90, 2, w1pu$high_90, col="grey", lty=5)
segments(2, w2pu$low_90, 2, w2pu$high_90, col="grey", lty=5)
segments(2, w3pu$low_90, 2, w3pu$high_90, col="grey", lty=5)

axis(1, at=c(1,2), labels=c("Republican",  "Democrat"))
text(1.9, 2.9, "Wave 1")
text(1.68, 2.7, "Wave 2")
text(1.5, 2.55, "Wave 3")

#############################################
#                 FIGURE 9                  #
#############################################

# Adjust some of the \x type non-alpha characters #
np$text <- iconv(np$text,"WINDOWS-1252","UTF-8") 

# Convert Text to Lower #
np$text <- tolower(np$text)

# Calculate article counts or the text patterns #
np$dem_count <- str_count(np$text,"democrat|democrats")
np$gop_count <- str_count(np$text,"republican|republicans")
np$trump_count <- str_count(np$text,"trump")
np$protests_count <- str_count(np$text,"protest|protesters|protests|airport|airports")
np$lind_count <- str_count(np$text,"graham|mccain")
np$pelosi_count <- str_count(np$text,"schumer|pelosi")
np$ai_count <- str_count(np$text,"american|unamerican|un-american|core values|religious freedom|religious test|violation|liberty|nation of immigrants")

np <- np[!is.na(np$text),] # drop missing

#################################
# Function to summarize "theme" #
#################################

party_mention <- function(x){
  
  dem <- sum(x$dem_count)
  rep <- sum(x$gop_count)
  tru <- sum(x$trump_count)
  prot <- sum(x$protests_count)
  lg <- sum(x$lind_count)
  pel <- sum(x$pelosi_count)
  ai <- sum(x$ai_count)
  
  return ( c(dem, rep, tru, prot, lg, pel, ai) )
}

# Subset data to Week for first 3 months #
npweek <- np[np$month < 4,]

# Split Data by Week #
npweek_s <- split(npweek, npweek$week) ; length(npweek_s)

###################################
# Create Weekly Data Distribution #
###################################

dist_filt <- plyr::ldply(npweek_s, party_mention) # party_mention function
colnames(dist_filt) <- c("week", "dem", "rep","trump", "protest", "gm", "ps", "ai")

# Initiate Plot #
plot(dist_filt$week, dist_filt$trump, typ='l', bty='n', lwd=2, col="black", lty=9,
     ylim = c(0,1800),
     main = "Themes in Newspaper Articles Over Time\n(First 3 Months of Year)",
     xlab = "Week 1                                 Numeric Week                                  Week 13",
     ylab = "Counts of themes appearing in Ban articles")
lines(dist_filt$week, dist_filt$dem, typ='l', col="blue", lwd=2, lty=2)
lines(dist_filt$week, dist_filt$rep, typ='l', col="red", lwd=2, lty=3) 
lines(dist_filt$week, dist_filt$protest, typ='l', col="green", lwd=2, lty=4)
lines(dist_filt$week, dist_filt$gm, typ='l', col="turquoise", lwd=2, lty=5)
lines(dist_filt$week, dist_filt$ps, typ='l', col="brown", lwd=2, lty=6)
lines(dist_filt$week, dist_filt$ai, typ='l', col="purple", lwd=2, lty=8) 
abline(v=4.5, col="grey", lwd=2, lty=1)
text(2.8, 1600, "EO Ban\nAnnounced")

legend("topright", 
       cex=.9,
       col = c("black","purple","green","blue", "red", "turquoise", "brown"), 
       lwd=rep(2,7), 
       lty=c(9,4,8,2,3,5,6), 
       bty='n', 
       title = "Theme",
       legend = c("Trump","American identity", "Protest/Airport","Democrat", 
                  "Republican", "Graham/McCain","Pelosi/Schumer"))

################################################################################################
#                                            END FILE                                          #
################################################################################################
