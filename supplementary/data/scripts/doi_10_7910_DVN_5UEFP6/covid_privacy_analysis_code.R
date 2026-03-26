#####################################################################################
######################### Replication code for ######################################
#### Americans' perceptions of privacy and surveillance in the COVID-19 Pandemic ####
########################### Zhang et al 2020 ########################################
#####################################################################################

# Set seed
set.seed(5820)

# Load packages
library(haven)
library(cjoint)
library(estimatr)
library(magrittr)
library(stringr)
library(reshape2)
library(ggplot2)
library(xtable)
library(stargazer)
library(dplyr)
library(multiwayvcov)
library(pastecs)
library(caret)
library(mice)
library(missForest)
library(labelled)
library(data.table)
library(purrr)
library(tidyr)
library(dummies)

# For survey weights
#devtools::install_github("aaronrudkin/autumn")
library(autumn)

# Functions

## to get number of clusters from a regression object
## enter cluster var as a vector (i.e. data$cluster)
cluster_n <- function(model, cluster_var){
  length(unique(cluster_var[as.numeric(row.names(model.matrix(model)))]))
}

## relabel variable
relabel_var <- function(old_var, old_labels, new_labels) {
  new_var <- rep(NA, length(old_var))
  if (is.factor(old_var)) {
    old_var <- as.character(old_var)
  }
  for (i in 1:length(old_labels)) {
    new_var[old_var == old_labels[i]] <- new_labels[i]
  }
  return(new_var)
}

## Trailing zeros rounding function
roundfunc <- function(x,
                      round_digits = 2,
                      lessthan = TRUE) {
  if (lessthan) {
    temp <- ifelse(x > 0 & round(x, round_digits) == 0,
                   paste0("<0.", paste(rep(0, (round_digits - 1)), collapse = ""), 1),
                   sprintf(paste0("%.", round_digits, "f"), round(x, round_digits)))
    temp <- ifelse(x < 0 & round(x, round_digits) == 0,
                   paste0(">-0.", paste(rep(0, (round_digits - 1)), collapse = ""), 1),
                   temp)
    temp[x == 0] <- 0
    return(temp)
  } else {
    return(sprintf(paste0("%.", round_digits, "f"), round(x, round_digits)))
  }
}

# Load data
load("survey_data_june_24_25_2020.RData")


##############################################################################################
######### DATA CLEANING ######################################################################
##############################################################################################

##### Survey weights ############

# Target proportions come from 2018 ACS 5-year public use microdata sample
# https://data.census.gov/mdat/?#/search?ds=ACSPUMS5Y2018

files <- list.files(path = ".", pattern = ".csv")
filenames <- gsub(".csv", "", files)
for(f in 1:length(files)){
        assign(paste("target", filenames[f], sep = "_"), read.csv(files[f]))
}

age_props <- target_age$Freq / sum(target_age$Freq)
names(age_props) <- target_age$Agegroup
education_props <- round(target_education$Freq / sum(target_education$Freq), digits = 3) # rounding so harvest() considers sum to 1
names(education_props) <- target_education$ppeducat4
gender_props <- target_gender$Freq / sum(target_gender$Freq)
names(gender_props) <- target_gender$ppgender
income_props <- target_income$Freq / sum(target_income$Freq)
names(income_props) <- target_income$ppincimp3
racehisp_props <- target_racehisp$Freq / sum(target_racehisp$Freq)
names(racehisp_props) <- target_racehisp$ppethm
regions_props <- target_regions$Freq / sum(target_regions$Freq)
names(regions_props) <- gsub(" Region", "", target_regions$census_region)

# Create target proportions
target.props.all <- list(
      rk_age = age_props,
      rk_education = education_props,
      rk_gender = gender_props,
      rk_income = income_props,
      rk_racehisp = racehisp_props,
      rk_region = regions_props)

# Create corresponding demo variables in the dataset

# using demographic variables from Lucid
# age
d$rk_age <- NA
d$rk_age[d$age >= 18 & d$age < 30] <- "18-29"
d$rk_age[d$age >= 30 & d$age < 45] <- "30-44"
d$rk_age[d$age >= 45 & d$age < 60] <- "45-59"
d$rk_age[d$age >= 60] <- "60+"

# level of education
d$rk_education <- NA
d$rk_education[d$education %in% c(6, 7, 8)] <- "Bachelor's degree or higher"
d$rk_education[d$education == 1] <- "Did not graduate high school"
d$rk_education[d$education %in% c(3:5)] <- "Some college, no degree" # including post-HS, some college, associates degree
d$rk_education[d$education == 2] <- "High school graduate, GED, or equivalent"

# gender
d$rk_gender <- NA
d$rk_gender[d$gender == 1] <- "Male"
d$rk_gender[d$gender == 2] <- "Female"

# income
d$rk_income <- NA
d$rk_income[d$hhi %in% c(19:24)] <- "$100,000+"
d$rk_income[d$hhi %in% c(9:18)] <- "$50,000-$99,999"
d$rk_income[d$hhi %in% c(1:8)] <- "Less than $50,000"

# race
d$rk_racehisp <- NA
# convert the Lucid data to numeric
d$ethnicity <- as.numeric(d$ethnicity)
d$hispanic <- as.numeric(d$hispanic)
# supplement the Lucid data with data collected in the survey
d$rk_racehisp[d$ethnicity == 1 & d$hispanic == 1] <- "White, Non-Hispanic"
d$rk_racehisp[d$race_1 == 1 & d$hispanic == 1] <- "White, Non-Hispanic"
d$rk_racehisp[d$ethnicity == 2 & d$hispanic == 1] <- "Black, Non-Hispanic"
d$rk_racehisp[d$race_2 == 1 & d$hispanic == 1] <- "Black, Non-Hispanic"
d$rk_racehisp[d$ethnicity %in% c(3:15) & d$hispanic == 1] <- "Other, Non-Hispanic"
d$rk_racehisp[(d$race_4 == 1 | d$race_5 == 1) & d$hispanic == 1] <- "Other, Non-Hispanic"
d$rk_racehisp[d$hispanic %in% c(2:14)] <- "Hispanic"
# use our self-reported race data to get the 2+ races
# sum up the race selected; exclude hispanic 
d$race_2plus <- rowSums(d[,c("race_1", "race_2", "race_4", "race_5")], na.rm = TRUE) > 1
d$rk_racehisp[d$race_2plus & d$hispanic == 1] <- "2+ Races, Non-Hispanic"

# region
d$rk_region <- NA
d$rk_region[d$region == 1] <- "Northeast"
d$rk_region[d$region == 2] <- "Midwest"
d$rk_region[d$region == 3] <- "South"
d$rk_region[d$region == 4] <- "West"

# Perform MICE to impute missing demographic variables

# Get the demographic variables
rk_d <- d[, c("rk_age", "rk_education", "rk_gender", "rk_income", "rk_racehisp", "rk_region")]
rk_d_raw <- rk_d
names(rk_d_raw) <- paste0(names(rk_d_raw), "_raw") # retain raw results for making the demographics table
# Create a data frame with indicators for missing data
rk_d_m <- as.data.frame(is.na(rk_d_raw))
names(rk_d_m) <- c("rk_age_m", "rk_education_m", "rk_gender_m", 
                   "rk_income_m", "rk_racehisp_m", "rk_region_m")
# Convert to factors
rk_d$rk_age <- factor(rk_d$rk_age)
rk_d$rk_education <- factor(rk_d$rk_education)
rk_d$rk_gender <- factor(rk_d$rk_gender)
rk_d$rk_income <- factor(rk_d$rk_income)
rk_d$rk_racehisp <- factor(rk_d$rk_racehisp)
rk_d$rk_region <- factor(rk_d$rk_region)
# MICE to impute the missing demographic variables
rk_d_imp_mice <- mice(rk_d)
mice_rk_d <- mice::complete(rk_d_imp_mice)

# Create a dataset for the MICE imputation
d[, c("rk_age", "rk_education", "rk_gender", "rk_income", "rk_racehisp", "rk_region")] <-
  mice_rk_d
# Add back the raw results for making the demographics table
d <- cbind(d, rk_d_raw, rk_d_m)
# Remove the datasets that have been merged in already
rm("rk_d_raw")
rm("rk_d_m")

# Weight - weights appear in "weights" column
d <- harvest(d, target.props.all)
# Trim the weights to between the 5 and 95th percentile
w_trimmed <- d$weights
w_trimmed[d$weights < quantile(d$weights, probs = 0.05)] <- quantile(d$weights, probs = 0.05)
w_trimmed[d$weights > quantile(d$weights, probs = 0.95)] <- quantile(d$weights, probs = 0.95)
d$weights <- w_trimmed

# Construct individual-level covariates

lucid_parties <- c("Strong Democrat",
                   "Not very strong Democrat",
                   "Independent Democrat",
                   "Independent - neither",
                   "Independent Republican",
                   "Other - leaning Democrat",
                   "Other - neither",
                   "Other - leaning Republican",
                   "Not very strong Republican",
                   "Strong Republican")
d$pid_l_all <- relabel_var(d$political_party, c(1:10), lucid_parties)
# divide Lucid pid into three classifications
d$pid_l_short <- NA
d$pid_l_short[d$political_party %in% c(1:3, 6)] <- "Democrat"
d$pid_l_short[d$political_party %in% c(5, 8:10)] <- "Republican"
d$pid_l_short[d$political_party %in% c(4, 7)] <- "Independent/other"

# race
d$race_white <- ifelse(d$race_1 == 1, 1, 0)
table(d$race_white, useNA = "always")
d$race_black <- ifelse(d$race_2 == 1, 1, 0)
d$race_latino <- ifelse(d$race_3 == 1, 1, 0)
d$race_native <- ifelse(d$race_4 == 1, 1, 0)
d$race_asian <- ifelse(d$race_5 == 1, 1, 0)
d$race_other <- ifelse(d$race_6 == 1, 1, 0)
# number of races
d$num_races <- apply(d[, paste("race_", c(1:6), sep = "")], 1, function(x){x[x < 0] <- NA; sum(x)})
d$race <- NA
d$race[d$num_races > 1] <- "multiracial"
d$race[d$race_white == 1 & d$num_races == 1] <- "white"
d$race[d$race_black == 1 & d$num_races == 1] <- "black"
d$race[d$race_latino == 1 & d$num_races == 1] <- "latino"
d$race[d$race_native == 1 & d$num_races == 1] <- "native_american"
d$race[d$race_asian == 1 & d$num_races == 1] <- "asian"
d$race[d$race_other == 1 & d$num_races == 1] <- "other"

# shorter version of the race variable
d$race_short <- d$race
d$race_short[d$race %in% c("asian", "multiracial", "native_american")] <- "other"
# table(d$race_short, useNA = "always")
# add in data from Lucid to fill out the missings
d$race_short[is.na(d$race_short) & d$rk_racehisp == "Hispanic"] <- "latino"
d$race_short[is.na(d$race_short) & d$rk_racehisp == "Black, Non-Hispanic"] <- "black"
d$race_short[is.na(d$race_short) & d$rk_racehisp == "White, Non-Hispanic"] <- "white"
d$race_short[is.na(d$race_short) & d$rk_racehisp == "2+ Races, Non-Hispanic"] <- "other"
d$race_short[is.na(d$race_short) & d$rk_racehisp == "Other, Non-Hispanic"] <- "other"

# trust in insitutions
# clean up the variables
inst <- c("Journalists", "Public health authorities", "Elected officials", "Tech companies",
  "Law enforcement", "Medical scientists", "Pharmaceutical companies")
var_inst <- paste("trust_inst_", 15:21)

trust_inst_clean <- function(inst_num) {
  x <- d[,paste0("trust_inst_", inst_num)]
  shown <- grepl(pattern = inst_num, x = d$DO_Q_trust_inst) & !is.na(d$trust_inst_t_1)
  xn <- x
  xn <- relabel_var(old_var = xn, old_labels = c(NA, -99, -88, 1, 6, 7, 5), new_labels = c(NA, -99, -88, 3, 2, 1, 0))
  xn[shown & (xn %in% c(-99, -88) | is.na(xn))] <- mean(xn[shown & !xn %in% c(-99, -88) & !is.na(xn)])
  xn[!shown] <- NA
  return(xn)  
}

# Get the clean variables
d$c_trust_inst_1 <- trust_inst_clean(15)
d$c_trust_inst_2 <- trust_inst_clean(16)
d$c_trust_inst_3 <- trust_inst_clean(17)
d$c_trust_inst_4 <- trust_inst_clean(18)
d$c_trust_inst_5 <- trust_inst_clean(19)
d$c_trust_inst_6 <- trust_inst_clean(20)
d$c_trust_inst_7 <- trust_inst_clean(21)

# Clean the trust in advice variable
trust_advice <- c("Trump", "CDC", "Governor of respondent's state")
# Trump
d$c_trust_advice_people_1 <- d$trust_advice_people_1
d$c_trust_advice_people_1[d$c_trust_advice_people_1 %in% c(-99,-88) | is.na(d$c_trust_advice_people_1)] <-
  mean(d$c_trust_advice_people_1[!d$c_trust_advice_people_1 %in% c(-99,-88) & !is.na(d$c_trust_advice_people_1)])
# CDC
d$c_trust_advice_people_2 <- d$trust_advice_people_6 
d$c_trust_advice_people_2[d$c_trust_advice_people_2 %in% c(-99,-88) | is.na(d$c_trust_advice_people_2)] <-
  mean(d$c_trust_advice_people_2[!d$c_trust_advice_people_2 %in% c(-99,-88) & !is.na(d$c_trust_advice_people_2)])
# Governor
d$c_trust_advice_people_3 <- d$trust_advice_people_9
d$c_trust_advice_people_3[d$c_trust_advice_people_3 %in% c(-99,-88) | is.na(d$c_trust_advice_people_3)] <-
  mean(d$c_trust_advice_people_3[!d$c_trust_advice_people_3 %in% c(-99,-88) & !is.na(d$c_trust_advice_people_3)])

# health
d$num_conditions <- apply(d[, paste("health_conditions", c(1:5), sep = "_")], 1, function(x){x[x < 0] <- NA; sum(x)})
d$health_condition <- ifelse(d$num_conditions > 0, 1, 0)
# mean impute
d$health_condition_c_i <- d$health_condition
d$health_condition_c_i[is.na(d$health_condition)] <- mean(d$health_condition, na.rm = TRUE)
# indicator for the misisngs
d$health_condition_c_m <- is.na(d$health_condition)
d$num_conditions_nona <- apply(d[, paste("health_conditions", c(1:5), sep = "_")], 1, function(x){x[x < 0] <- NA; sum(x, na.rm = T)})
d$health_condition_nona <- ifelse(d$num_conditions_nona > 0, 1, 0)

# COVID impact index variables
# know someone who has tested positive
d$know_test_pos_c <- NA
d$know_test_pos_c[d$know_test_pos == 1] <- 1
d$know_test_pos_c[d$know_test_pos == 0] <- 0

# know someone who has died
d$know_die_c <- NA
d$know_die_c[d$know_die == 1] <- 1
d$know_die_c[d$know_die == 0] <- 0

# lost job 
d$affectjob_c <- NA
d$affectjob_c[d$affectjob == 1] <- 1
d$affectjob_c[d$affectjob == 0] <- 0

# likely to lose job
d$affectjob_likely_c <- NA
d$affectjob_likely_c[d$affectjob == 3] <- 1
d$affectjob_likely_c[d$affectjob == 2] <- 0.5
d$affectjob_likely_c[d$affectjob %in% c(0, 1)] <- 0

# financial situation getting worse
d$financial_change_worse <- NA
d$financial_change_worse[d$financial_change == -1] <- 1
d$financial_change_worse[d$financial_change %in% c(0, 1)] <- 0

# Mean impute for the index
# also make indicator variables for missingness
# health
d$know_test_pos_c_i <- d$know_test_pos_c
d$know_test_pos_c_i[is.na(d$know_test_pos_c)] <- mean(d$know_test_pos_c, na.rm = TRUE)
d$know_test_pos_c_m <- is.na(d$know_test_pos_c)
#
d$know_die_c_i <- d$know_die_c
d$know_die_c_i[is.na(d$know_die_c)] <- mean(d$know_die_c, na.rm = TRUE)
d$know_die_c_m <- is.na(d$know_die_c)
# economic
d$affectjob_c_i <- d$affectjob_c
d$affectjob_c_i[is.na(d$affectjob_c)] <- mean(d$affectjob_c, na.rm = TRUE)
d$affectjob_c_m <- is.na(d$affectjob_c)
# 
d$affectjob_likely_c_i <- d$affectjob_likely_c
d$affectjob_likely_c_i[is.na(d$affectjob_likely_c)] <- mean(d$affectjob_likely_c, na.rm = TRUE)
d$affectjob_likely_c_m <- is.na(d$affectjob_likely_c)
#
d$financial_change_worse_i <- d$financial_change_worse
d$financial_change_worse_i[is.na(d$financial_change_worse)] <-
  mean(d$financial_change_worse, na.rm = TRUE)
d$financial_change_worse_m <- is.na(d$financial_change_worse)

# Construct COVID impact indices

# Health impact index
d$covid_impact_index_health <- d$know_test_pos_c_i + 
  2*d$know_die_c_i
# indicator variable for where we are missing data
d$covid_impact_index_health_m <- 
  (is.na(d$know_test_pos_c) + is.na(d$know_die_c)) > 0

# Economic impact index
d$covid_impact_index_economic <- d$affectjob_c_i*2 + 
  d$affectjob_likely_c_i + 
  d$financial_change_worse_i
# indicator variable for where we are missing data
d$covid_impact_index_economic_m <-
  (is.na(d$affectjob_c) + is.na(d$affectjob_likely_c) + is.na(d$financial_change_worse)) > 0

# States and copartisan governors
states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA",
            "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA",
            "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY",
            "NC", "ND", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
            "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "Outside US", NA)
state_nums <- c(1:53, -99)
r_govs <- c("AL", "AK", "AZ", "AR", "FL", "GA", "ID", "IN", "IA", "MD", "MA", "MS", "MO", "NE", "NH", "ND", "OH", "OK", "SC", "SD", "TN", "TX", "UT", "VT", "WV", "WY", "PR")
d_govs <- c("CA", "CO", "CT", "DE", "HI", "IL", "KS", "KY", "LA", "ME", "MI", "MN", "MT", "NV", "NJ", "NM", "NY", "NC", "OR", "PA", "RI", "VA", "WA", "WI", "DC") # counting DC mayor
d$state_name <- factor(d$state, levels = state_nums, labels = states)

# fill in missing states using Lucid zip code data
data("zipcode") # load from zipcode package
table(d$zip %in% zipcode$zip) # check all Lucid zipcodes are in the zipcode list
d$zip[!d$zip %in% zipcode$zip] # one is missing - nonsensical answer
lucid_states <- sapply(d$zip, function(x){zipcode$state[zipcode$zip == x]})
lucid_states[!d$zip %in% zipcode$zip] <- NA # replace nonsensical zipcode with NA
d$state_name_i <- d$state_name
d$state_name_i[is.na(d$state_name) | d$state == -99] <- lucid_states[is.na(d$state_name) | d$state == -99]
table(d$state_name_i, useNA = "always") # one NA and one outside the US
# outside US respondent has a CA zipcode
d$state_name_i[which(d$state == 53)] <- lucid_states[which(d$state == 53)]

# Governor copartisan
d$copartisan <- NA
d$copartisan[d$pid_l_short == "Democrat" & d$state_name_i %in% d_govs] <- 1
d$copartisan[d$pid_l_short == "Democrat" & d$state_name_i %in% r_govs] <- 0
d$copartisan[d$pid_l_short == "Republican" & d$state_name_i %in% d_govs] <- 0
d$copartisan[d$pid_l_short == "Republican" & d$state_name_i %in% r_govs] <- 1
table(d$copartisan, useNA = "always") # 471 NAs
# Create partisan variable
d$partisan <- d$pid_l_short %in% c("Democrat", "Republican")
table(d$copartisan[d$partisan], useNA = "always") # 0 missing among Ds & Rs

# Experimental variables
# Federal vs state
d$federal <- ifelse(d$government_level == "the federal government", 1, 0)
# Priming treatment
d$experience_prime <- d$experience_prime == "1"

##############################################################################################
######### ANALYSIS ###########################################################################
##############################################################################################

##### SI Fig 2. Distribution of Survey Weights

pdf("./survey2_survey_weights.pdf")
hist(d$weights,
     main = "Distribution of Survey Weights",
     xlab = "Weights",
     col = "gray")
dev.off()

##### SI Table 1. Sample Demographics 

ddem <- dummy.data.frame(d[, c("rk_age_raw", "rk_gender_raw", 
                               "rk_income_raw", "rk_education_raw", "rk_racehisp_raw",
                               "rk_region_raw")])  
ddem_tab <- data.frame(t(stat.desc(ddem))[, c("mean", "sum")])
varnames <- c("18-29",
              "30-44",
              "45-59",
              "60+",
              "Female",
              "Male",
              "$100K+",
              "$50K-90K",
              "<$50K",
              "Income NA",
              "BA or higher",
              "Did not graduate HS",
              "HS graduate",
              "Some college",
              "Education NA",
              "2+ Races, Non-Hispanic",
              "Black, Non-Hispanic",
              "Hispanic",
              "Other, Non-Hispanic",
              "White, Non-Hispanic",
              "Race NA",
              "Midwest",
              "Northeast",
              "South",
              "West")
colnames(ddem_tab) <- c("Proportion", "Count")
rownames(ddem_tab) <- NULL
ddem_tab$Variable <- c(rep("Age", 4), rep("Gender", 2), rep("Income", 4), 
                       rep("Education", 5), rep("Race", 6), rep("Region", 4))
ddem_tab$Value <- varnames
ddem_tab <- data.frame(ddem_tab[, c("Variable", "Value", "Proportion", "Count")])
ddem_tab_x <- xtable(ddem_tab, digits = c(0, 0, 0, 3, 0), 
                     label = "tab:sampledemo",
                     caption = "Sample Demographics",
                     )
print(ddem_tab_x, include.rownames = FALSE,
      type = "latex",
      caption.placement = "top",
      file  = "survey2_sample_demographics.tex"
      )

####################################################################
##### Attitudes toward surveillance policies #######################
####################################################################

policy_outcomes <- c("Temperature checks",
                     "Check credit/debit card transactions",
                     "Electronic device monitoring",
                     "CCTV cameras and drones",
                     "Smartphone contact tracing apps",
                     "Thermal cameras",
                     "Centralized quarantine",
                     "Immunity pass for public transit and travel")

##### SI Fig 14: Distribution of responses to surveillance policy questions

# Plot all the numeric variables as histograms
# Make the labels
policy_support_label <- c(
  'policy_support_1' = policy_outcomes[1],
  'policy_support_2' = policy_outcomes[2],
  'policy_support_3' = policy_outcomes[3],
  'policy_support_4' = policy_outcomes[4],
  'policy_support_5' = policy_outcomes[5],
  'policy_support_6' = policy_outcomes[6],
  'policy_support_7' = policy_outcomes[7],
  'policy_support_8' = policy_outcomes[8],
  "trad_ct_support" = "Traditional contact tracing"
)

# Generate the "long" dataset
long_policy_support <- d[,c("trad_ct_support", paste0("policy_support_", 1:8))] %>%
  keep(is.numeric) %>% 
  gather()

# Make the histogram
ggplot(long_policy_support, aes(value)) +
  facet_wrap(~ key, scales = "free", labeller = as_labeller(policy_support_label), 
             ncol = 2) + xlab("Values") + ylab("Count") +
  geom_histogram() + theme_bw()
ggsave("survey2_histograms_policy_support.pdf", dpi = 300,
       height = 10, width = 7)


# Create long version of the policy data for regression analysis

# Get the percent missing for each outcome variable
missing_getperc_policy <- function(num, shown_var) {
  x <- d[,names(policy_support_label)[num]]
  shown_var <- shown_var
  missing_dk <- mean(x[shown_var] == -99 | x[shown_var] == -88 | is.na(x[shown_var]))
  output <- data.frame(Outcome = policy_support_label[num], 
                       Percent_missing = missing_dk, N = length(x[shown_var]))
  row.names(output) <- NULL
  return(output)
}

policy_outcome_missing <-
  rbind(missing_getperc_policy(num = 1, shown_var = !is.na(d$policy_description_1)),
missing_getperc_policy(num = 2, shown_var = !is.na(d$policy_description_2)),
missing_getperc_policy(num = 3, shown_var = !is.na(d$policy_description_3)),
missing_getperc_policy(num = 4, shown_var = !is.na(d$policy_description_4)),
missing_getperc_policy(num = 5, shown_var = !is.na(d$policy_description_5)),
missing_getperc_policy(num = 6, shown_var = !is.na(d$policy_description_6)),
missing_getperc_policy(num = 7, shown_var = !is.na(d$policy_description_7)),
missing_getperc_policy(num = 8, shown_var = !is.na(d$policy_description_8)),
missing_getperc_policy(num = 9, shown_var = !is.na(d$trad_ct_desc)))

get_policy_rows <- function(num){
  outcome_var <- paste("policy_support", num, sep = "_")
  shown_var <- paste("policy_description", num, sep = "_")
  x <- d[, outcome_var]
  shown <- !is.na(d[, shown_var])
  x <- x[shown]
  ids <- d$rid[shown]
  x_missing <- x %in% c(-99, -77, -88) | is.na(x)
  x[x %in% c(-99, -77, -88) | is.na(x)] <- mean(x[!(x %in% c(-99, -77, -88) | is.na(x))])
  return(data.frame(rid = ids, policy = policy_outcomes[num], support = x, 
                    support_m = x_missing))
}
# the long version
all_policy_support <- sapply(c(1:8), get_policy_rows, simplify = F)
all_policy_support <- do.call("rbind", all_policy_support)

# incorporate traditional contact tracting
# change the outcome name
outcome_var <- "trad_ct_support"
shown_var <- "trad_ct_desc"
x <- d[, outcome_var]
shown <- !is.na(d[, shown_var])
x <- x[shown]
ids <- d$rid[shown]
x_missing <- x %in% c(-99, -77, -88) | is.na(x)
x[x %in% c(-99, -77, -88) | is.na(x)] <- mean(x[!(x %in% c(-99, -77, -88) | is.na(x))])
trad_ct_support <- data.frame(rid = ids, 
                              policy = rep("Traditional contact tracing", length(x)), 
                              support = x, support_m = x_missing)

all_policy_support <- rbind(all_policy_support, trad_ct_support)
rm("trad_ct_support") # remove traditional contact tracing

# re-order demographic variables for plots
d$race_short <- factor(d$race_short,
                                        levels = c("white", "black", "latino", "other"),
                                        labels = c("White", "Black", "Latino", "Other"))
d$rk_gender <- factor(d$rk_gender,
                                       levels = c("Male", "Female"))
d$rk_education <- factor(d$rk_education,
                                          levels = c("Did not graduate high school",
                                                     "High school graduate, GED, or equivalent",
                                                     "Some college, no degree",
                                                     "Bachelor's degree or higher"))
d$pid_l_short <- factor(d$pid_l_short,
                                         levels = c("Independent/other",
                                                    "Democrat",
                                                    "Republican"))
d$rk_income <- factor(d$rk_income,
                                       levels = c("Less than $50,000",
                                                  "$50,000-$99,999",
                                                  "$100,000+"))
# merge in individual-level covariates
merge_vars <- c("rid", "federal", "experience_prime",
                "pid_l_short", "copartisan", "partisan",
                "state_name", "state_name_i",
                "weights",
                "health_condition", "health_condition_c_i", "health_condition_c_m", 
                "know_test_pos_c", "know_test_pos_c_i", "know_test_pos_c_m",  
                "covid_impact_index_health", "covid_impact_index_health_m",
                "covid_impact_index_economic", "covid_impact_index_economic_m",
                "race_short", 
                "rk_age", "rk_gender",
                "rk_education", "rk_income", "rk_region"
               )
merge_data <- d[, merge_vars]
all_policy_support <- merge(all_policy_support, merge_data, by = "rid")

##### SI Table 4. Level of Government and Support for Surveillance

# Effects of state/federal manipulation on surveillance support

# fully saturated
mod.policy.statefed.lin <- lm_lin(support ~ federal, covariates = ~ policy, 
                                  clusters = rid, data = all_policy_support)
summary(mod.policy.statefed.lin)

# bivariate
mod.policy.statefed <- lm(support ~ federal, data = all_policy_support)
summary(mod.policy.statefed)
mod.policy.statefed.c <- coeftest(mod.policy.statefed, cluster.vcov(mod.policy.statefed, cluster = all_policy_support$rid))
mod.policy.statefed.c

# interacted with party [subset to republicans and democrats only]
mod.policy.statefed.party <- lm(support ~ federal*pid_l_short, 
                                data = all_policy_support[all_policy_support$partisan,])
mod.policy.statefed.party.c <- coeftest(mod.policy.statefed.party, 
                                        cluster.vcov(mod.policy.statefed.party, 
                                                     cluster = all_policy_support$rid[all_policy_support$partisan]))
mod.policy.statefed.party.c

# interacted with copartisanship [subset to republicans and democrats only]
mod.policy.cop <- lm(support ~ federal*copartisan, 
                     data = all_policy_support[all_policy_support$partisan, ]) 
mod.policy.cop.c <- coeftest(mod.policy.cop, cluster.vcov(mod.policy.cop, 
                    cluster = all_policy_support$rid[all_policy_support$partisan]))
mod.policy.cop.c 

# triple interaction between party, fed, and copartisan
mod.policy.statefed.cop <- lm(support ~ federal*pid_l_short*copartisan, 
                              data = all_policy_support[all_policy_support$partisan, ])
mod.policy.statefed.cop.c <- coeftest(mod.policy.statefed.cop, 
                                      cluster.vcov(mod.policy.statefed.cop, cluster = all_policy_support$rid[all_policy_support$partisan]))
mod.policy.statefed.cop.c

# Get number of clusters
cluster_ns <- formatC(c(cluster_n(mod.policy.statefed, all_policy_support$rid), 
                     rep(cluster_n(mod.policy.statefed.party, 
                                   all_policy_support$rid[all_policy_support$partisan]), 3)), big.mark = ",")
ses <- list(mod.policy.statefed.c[, 2],
            mod.policy.statefed.party.c[, 2],
            mod.policy.cop.c[, 2],
            mod.policy.statefed.cop.c[, 2])
star <- stargazer(
          mod.policy.statefed,
          mod.policy.statefed.party,
          mod.policy.cop,
          mod.policy.statefed.cop,
          se = ses,
          covariate.labels = c("Federal", "Republican", "Federal x Republican", "Copartisan",
                               "Federal x Copartisan", "Republican x Copartisan", "Federal x Republican x Copartisan", "Constant"),
          #omit = "scale",
          omit.stat = c("ser", "f"),
          add.lines = list(c("Subset", "All", rep("D \\& R", 3)),
            c("Respondents", cluster_ns)),
          type = "latex",
          title = "Level of Government and Support for Surveillance",
          font.size = "footnotesize",
          dep.var.labels = "Policy Support",
          notes.append = FALSE,
          notes.label = "CRSE at respondent level",
          star.cutoffs = c(0.05, 0.01, 0.001),
          label = "tab:statefedparty",          
          out = "survey2_statefed_party.tex")

##### SI Fig 3. Demographic predictors of support for surveillance policies

mod.policy.demo <- lm(support ~ rk_gender + race_short + pid_l_short + rk_age + rk_income + rk_education + rk_region, data = all_policy_support[!all_policy_support$support_m, ], weights = weights)
mod.policy.demo.c <- coeftest(mod.policy.demo, cluster.vcov(mod.policy.demo, all_policy_support$rid[!all_policy_support$support_m]))

mod.policy.demo_r <- lm_robust(support ~ rk_gender + race_short + pid_l_short + rk_age + rk_income + 
                           rk_education + rk_region, data = all_policy_support[!all_policy_support$support_m,], 
                          weights = weights, clusters = all_policy_support$rid[!all_policy_support$support_m])

# plot of demographic model
mod.policy.demo.results <- data.frame(predictor = names(mod.policy.demo$coefficients[-1]),
                                       coef = mod.policy.demo_r$coefficients[-1],
                                       se = mod.policy.demo_r$std.error[-1])
cluster_n(mod.policy.demo, all_policy_support$rid[!all_policy_support$support_m])

predictor_names <- c("Female", "Black", "Latinx","Other race",
                     "Democrat", "Republican",
                     "30-44","45-59","60+","$50k-99k", ">99$k",
                     "HS graduate","Some college",
                     "Bachelors or higher","Northeast","South","West")
mod.policy.demo.results$predictor_name <- predictor_names
level_order <- predictor_names

ggplot(mod.policy.demo.results, aes(x = factor(predictor_name, level = rev(level_order)),
                                    y = coef, 
                            ymin = coef + qnorm(0.025)*se,
                            ymax = coef + qnorm(0.975)*se)) +
  geom_hline(yintercept = 0, alpha = 0.75, color = "red", linetype = 2) +
  ylab("Coefficient") + xlab("Demographic variables") +
  geom_pointrange() + #scale_x_discrete(name = "Outcome", 
                       #                labels = function(x) str_wrap(x, width = 25)) +
  coord_flip() + theme_bw()
ggsave("survey2_demographic_predictors_surveillance.pdf",
       dpi = 300, width = 7.5, height = 6)


# Compare Democrats versus Republicans 
car::linearHypothesis(model = mod.policy.demo_r,
                      c("pid_l_shortDemocrat = pid_l_shortRepublican"))

##### SI Table 3. COVID-19 experience and support for surveillance

demo_vars <- c("rk_gender", "race_short", "pid_l_short", "rk_age", "rk_income", "rk_education", "rk_region")
demographics <- paste0(demo_vars, collapse = " + ")

mod.policy.health <- lm(formula(paste("support ~ health_condition_c_i + 
                                      scale(health_condition_c_m, TRUE, FALSE)", 
                                      demographics, sep = "+")), 
                        data = all_policy_support[!all_policy_support$support_m,], 
                        weights = weights)
mod.policy.health.c <- coeftest(mod.policy.health, cluster.vcov(mod.policy.health,
                                                                all_policy_support$rid[!all_policy_support$support_m]))
mod.policy.health.c

mod.policy.knowpos <- lm(formula(paste("support ~ know_test_pos_c_i +
                                       scale(know_test_pos_c_m, TRUE, FALSE)", 
                                       demographics, sep = "+")), 
                         data = all_policy_support[!all_policy_support$support_m,], weights = weights)
mod.policy.knowpos.c <- coeftest(mod.policy.knowpos, cluster.vcov(mod.policy.knowpos, all_policy_support$rid[!all_policy_support$support_m]))
mod.policy.knowpos.c

mod.policy.covidimpact <- lm(formula(paste("support ~ covid_impact_index_health +
                                           scale(covid_impact_index_health_m, TRUE, FALSE)", 
                                           demographics, sep = "+")), 
                             data = all_policy_support[!all_policy_support$support_m,], 
                             weights = weights)
mod.policy.covidimpact.c <- coeftest(mod.policy.covidimpact, 
                                     cluster.vcov(mod.policy.covidimpact, all_policy_support$rid[!all_policy_support$support_m]))
mod.policy.covidimpact.c

mod.policy.covidimpact.econ <- lm(formula(paste("support ~ covid_impact_index_economic +
                                                scale(covid_impact_index_economic_m, TRUE, FALSE)", 
                                                demographics, sep = "+")), 
                                  data = all_policy_support[!all_policy_support$support_m,], weights = weights)
mod.policy.covidimpact.econ.c <- coeftest(mod.policy.covidimpact.econ, cluster.vcov(mod.policy.covidimpact.econ, 
                                                                                    all_policy_support$rid[!all_policy_support$support_m]))
mod.policy.covidimpact.econ.c

cluster_ns <- sapply(list(mod.policy.health,
            mod.policy.knowpos,
            mod.policy.covidimpact,
            mod.policy.covidimpact.econ),
       function(x) cluster_n(x, all_policy_support$rid[!all_policy_support$support_m]))

ses <- list(mod.policy.health.c[, 2],
            mod.policy.knowpos.c[, 2],
            mod.policy.covidimpact.c[, 2],
            mod.policy.covidimpact.econ.c[, 2])
star <- stargazer(mod.policy.health,
          mod.policy.knowpos,
          mod.policy.covidimpact,
          mod.policy.covidimpact.econ,
          omit = c("rk", "race", "pid", "Constant", "scale"),
          omit.stat = c("ser", "f"),
          se = ses,
          dep.var.caption = "Policy Support",
          dep.var.labels.include = F,
          add.lines = list(c("Demographic Controls", rep('Y', 4)),
                           c("Respondents", formatC(cluster_ns, big.mark = ","))),
          covariate.labels = c("Comorbidity", "Know COVID Positive", "Health Impact Index", "Economic Impact Index"),
          title = "COVID-19 Experience and Support for Surveillance",
          label = "tab:covidpredictors",
          type = "latex",
          notes.label = "CRSE at respondent level",
          notes.append = F,
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "survey2_covid_experience_surveillance.tex"
        )

##### SI Fig 16. Trust in institutions and support for surveillance policies

# create a wide data frame
all_policy_support_w <- spread(all_policy_support, key = policy, value = support)
# merge in the trust questions
trust_d <- c("rid", paste0("c_trust_inst_", 1:7), paste0("c_trust_advice_people_", 1:3))
all_policy_support_w <- merge(x = all_policy_support_w, y = d[,trust_d], all.x = TRUE, by = "rid")

corr_long <- expand.grid(policy = unique(all_policy_support$policy), trust_var = trust_d[-1])
corr_long$policy <- as.character(corr_long$policy)
corr_long$trust_var <- as.character(corr_long$trust_var)

# Function to get the Pearson correlation
corr_matrix <- function(row_num) {
  x <- all_policy_support_w[,corr_long$policy[row_num]]
  y <- all_policy_support_w[,corr_long$trust_var[row_num]]
  pairwise_complete_n <- sum(!is.na(x) & !is.na(y))
  return(data.frame(pairwise_complete_n, pearson_cor = cor(x = x, 
      y = y, 
      use = "pairwise.complete.obs")))
}
# Combine with the dataset
corr_long <- cbind(corr_long, do.call(rbind, lapply(1:nrow(corr_long), corr_matrix)))
# Label the trust_var
trust_label_d <- data.frame(trust_var = trust_d[-1], trust_var_label = c(inst, 
                                                                         paste0(trust_advice, "*")))
# Merge in the labels
corr_long <- merge(x = corr_long, y = trust_label_d, all.x = TRUE, by = "trust_var")

# Policy outcome names
policy_outcomes <- c("Temperature checks",
                     "Check credit/debit card transactions",
                     "Electronic device monitoring",
                     "CCTV cameras and drones",
                     "Smartphone contact tracing apps",
                     "Thermal cameras",
                     "Centralized quarantine",
                     "Immunity pass for public transit and travel")
policy_outcomes_all <- c("Traditional contact tracing", policy_outcomes)

# Clean up the factors
corr_long$trust_var_label <- factor(corr_long$trust_var_label, levels = trust_label_d$trust_var_label)
corr_long$policy <- factor(corr_long$policy, levels = rev(policy_outcomes_all))

# Make the graph
ggplot(corr_long, aes(x = trust_var_label, y = policy, fill = pearson_cor))+
  geom_bin2d(color = "white") + xlab("Bob") + 
  scale_y_discrete(name = "Policy",
                   labels = function(x) str_wrap(x, width = 15)) +
  scale_x_discrete(name = "Institution or actor",
                   labels = function(x) str_wrap(x, width = 15)) +
  geom_text(aes(x = trust_var_label, y = policy, 
                 label = roundfunc(pearson_cor, 2)), color = "black", size = 3) + 
  scale_fill_gradient2(name = "Pearson correlation", 
                       midpoint = 0,
                       low = "#1b7837", mid = "#f7f7f7", high = "#762a83") + 
  # scale_fill_gradient(name = "Percent who considers the issue very important",
  #                     low = "white", high = "#00003f", labels = scales::percent) + 
  theme_bw() + theme(legend.position = "bottom",
                     axis.text.x = element_text(angle = 270, hjust = 0)) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5,
                                barwidth = 15))
ggsave("survey2_trust_correlation_graph.pdf",
       dpi = 300, width = 7.5, height = 8)

##### SI Table 11. Trust in institutions and support for surveillance policies

d$c_inst_trust_mean <- rowMeans(d[,paste0("c_trust_inst_", 1:7)], na.rm = TRUE)
all_policy_support <- merge(x = all_policy_support, y = d[,c("rid", "c_inst_trust_mean",
                                                             paste0("c_trust_advice_people_", 1:3))],
                            all.x = TRUE, by = "rid")
trust_md_1_f <- "support ~ policy + c_inst_trust_mean + c_trust_advice_people_1 + c_trust_advice_people_2 + c_trust_advice_people_3"
trust_md_2_f <- paste0(trust_md_1_f, " + ", paste(demographics, collapse = "+"))
trust_md_1 <- lm_robust(formula = as.formula(trust_md_1_f),
          data = all_policy_support[!all_policy_support$support_m,], clusters = rid)
trust_md_2 <- lm_robust(formula = as.formula(trust_md_2_f),
                        data = all_policy_support[!all_policy_support$support_m,], clusters = rid)

# Make the custom coefficient names
trust_coef <- rep(NA, length(trust_md_2$coefficients))
trust_coef[c(1, 10:13)] <- c("(Intercept)", "Mean trust in institutions",
                             "Trust in advice related to COVID-19 from Trump",
                             "Trust in advice related to COVID-19 from the CDC",
                             "Trust in advice related to COVID-19 from the respondent's state governor")
names(trust_coef) <- names(trust_md_2$coefficients)
trust_coef <- trust_coef[!is.na(trust_coef)]

# Output the model to LaTeX
modelsummary::modelsummary(
  models = list(trust_md_1, trust_md_2),
  coef_map = trust_coef,
  output = "latex",
  title = "Association between trust in institutions and support for surveillance policies",
  stars = c(
    "*" = 0.05,
    "**" = 0.01,
    "***" = 0.001
  )
)

##### Support for individual surveillance policies

##### Fig 3. Mean levels of support for government adoption of surveillance policies

# Get the outcome variable
numerical_summary <- function(outcome_var, outcome_name, weights = "weights",
                              shown_var, subset, subset_name, dataset) {
        x <- dataset[, outcome_var]
        shown <- !is.na(dataset[, shown_var])
        shown <- shown & subset
        x <- x[shown]
        if (!is.null(weights)) {
          weights <- dataset[shown, "weights"] 
        } else {
          weights <- rep(1, sum(shown))
        }
        # Mean impute the missing data
        x_missing <- x %in% c(-99, -77, -88) | is.na(x)
        x[x %in% c(-99, -77, -88) | is.na(x)] <- mean(x[!(x %in% c(-99, -77, -88) | is.na(x))])
        x_bin <- ifelse(x >= 60, 1, 0) # support or strongly support
        # Get the missing percent
        percent_missing <- sum(x_missing)/length(x)
        md <- if (percent_missing > 0.1) {
                # If more than 10 percent is missing, then we condition on normalized dummy variable for missingness
                se_md <- lm(x ~ scale(x_missing), weights = weights)
                se_md_bin <- lm(x_bin ~ scale(x_missing), weights = weights) # binary version
                c(coeftest(se_md, vcov = vcovHC(se_md, type = "HC2"))[1, 1:2],
                  coeftest(se_md_bin, vcov = vcovHC(se_md_bin, type = "HC2"))[1, 1:2])
        } else {
                se_md <- lm(x ~ 1, weights = weights)
                se_md_bin <- lm(x_bin ~ 1, weights = weights) # binary version
                c(coeftest(se_md, vcov = vcovHC(se_md, type = "HC2"))[1:2],
                coeftest(se_md_bin, vcov = vcovHC(se_md_bin, type = "HC2"))[1:2])
        }
        return(data.frame(outcome_name, mean = md[1], se = md[2], mean_bin = md[3]*100, se_bin = md[4]*100,
                          n = length(x), subset = subset_name, percent_missing = percent_missing))  
        
}

# Policy support function
policy_support_analysis <- function(num, subset = rep(TRUE, nrow(d)),
                                    subset_name = "All respondents") {
        numerical_summary(outcome_var = paste0("policy_support_", num), 
                          shown_var = paste0("policy_description_", num), 
                          outcome_name = policy_outcomes[num], 
                          subset = subset,
                          subset_name = subset_name, 
                          dataset = d)        
}

# Function to create different subsets 
policy_support_subset <- function(subset = rep(TRUE, nrow(d)),
                                   subset_name = "All respondents") {
        # Get the traditional contact tracing results
        trad_ct <- numerical_summary(outcome_var = "trad_ct_support", shown_var = "trad_ct_desc", 
                                     outcome_name = "Traditional contact tracing", dataset = d, 
                                     subset = subset,
                                     subset_name = subset_name)
        other_policies <- do.call(rbind, lapply(1:length(policy_outcomes), 
                                                policy_support_analysis,
                                                subset = subset,
                                                subset_name = subset_name))
        return(rbind(trad_ct, other_policies))
}


# All respondents
all_policies <- policy_support_subset()
all_policies$labels <- paste(str_wrap(all_policies$outcome_name, width = 30), "\n(N = ", all_policies$n, ")", sep = "")
all_policies$labels <- factor(all_policies$labels, 
                                    levels = all_policies$labels[order(all_policies$mean)])
ggplot(all_policies, aes(x = labels, y = mean, ymin = mean + qnorm(0.025)*se,
                         ymax = mean + qnorm(0.975)*se)) +
        geom_pointrange() + coord_flip() + 
  ggtitle("Support for public health surveillance measures") +
        ylab("Mean level of support\n(0 = strongly oppose; 100 = strongly support)") +
        scale_x_discrete(name = "Policy") +
        theme_bw()
ggsave("survey2_policy_support_all_respondents.pdf",
              dpi = 300, width = 7.5, height = 6)
ggsave("survey2_policy_support_all_respondents.png",
              dpi = 300, width = 8, height = 4.5)


##### SI Table 2. Support for public health surveillance measures

all_policies_tab <- xtable(all_policies[, -c(4, 5, 7)], label = "tab:policysupportall")
print(all_policies_tab,
      type = "latex",
      file = "survey2_policy_support_all_respondents.tex")

all_policies_output <- all_policies[,c("outcome_name", "mean", "se", "mean_bin", "n", "percent_missing")]

knitr::kable(all_policies_output, format = "latex", digits = 2,
             caption = "Support for public health surveillance measures: summary of responses",
             col.names = c("Outcome", "Mean level of support", "SE", "Percent support/strongly support",
                           "N", "Proportion missing"), booktabs = TRUE)


##### SI Fig 4. Mean levels of support for surveillance policies by level of government

statefed_policies <- rbind(policy_support_subset(subset = d$federal == 1,
                                                 subset_name = "Federal"),
                           policy_support_subset(subset = d$federal == 0,
                                                 subset_name = "State"))
# Create labels
statefed_policies <- statefed_policies %>% 
  group_by(outcome_name) %>% 
  mutate(outcome_n = sum(n)) %>% # get total n per policy 
  ungroup() %>% # create label
  mutate(label = paste(str_wrap(outcome_name, width = 30), "\n(N = ", outcome_n, ")", sep = ""))
# Create group-level labels
group_ns <- statefed_policies %>% group_by(subset) %>% summarize(group_n = sum(n)) %>%
  mutate(group_label = subset)
# Reorder
statefed_policies$label <- factor(statefed_policies$label, 
                                  levels = statefed_policies$label[order(all_policies$mean)])
# Plot
ggplot(statefed_policies, aes(x = label, y = mean, ymin = mean + qnorm(0.025)*se,
                              ymax = mean + qnorm(0.975)*se, color = subset, shape = subset)) +
  geom_pointrange(position = position_dodge(width = 1)) + coord_flip() + 
  ylab("Mean level of support\n(0 = strongly oppose; 100 = strongly support)") +
  theme_bw() +
  geom_vline(xintercept=seq(0.5, 20, 1), colour="black", alpha = 0.2) + 
  scale_color_manual(values = c("darkorange", "purple"), 
                     name = "Level of government",
                     labels = group_ns$group_label) +
  scale_shape_manual(values = c(3, 4),
                     name = "Level of government",
                     labels = group_ns$group_label) + theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_blank()) +
  scale_x_discrete(name = "Policy")
ggsave("policy_support_statefed.pdf",
       dpi = 300, width = 7.5, height = 7.5)

##### SI Fig 5. Mean levels of support for surveillance policies by party identification

pid_policies_l <- rbind(policy_support_subset(subset = d$pid_l_short == "Republican" & !is.na(d$pid_l_short),
                                            subset_name = "Republican"),
                      policy_support_subset(subset = d$pid_l_short == "Democrat" & !is.na(d$pid_l_short),
                                            subset_name = "Democrat"),
                      policy_support_subset(subset = d$pid_l_short == "Independent/other" & !is.na(d$pid_l_short),
                                            subset_name = "Independent/other"))
# Create labels
pid_policies_l <- pid_policies_l %>% 
  group_by(outcome_name) %>% 
  mutate(outcome_n = sum(n)) %>% # get total n per policy 
  ungroup() %>% # create label
  mutate(label = paste(str_wrap(outcome_name, width = 30), "\n(N = ", outcome_n, ")", sep = ""))
# Create group-level labels
group_ns <- pid_policies_l %>% group_by(subset) %>% summarize(group_n = sum(n)) %>%
  mutate(group_label = subset)
# Reorder
pid_policies_l$label <- factor(pid_policies_l$label, 
                                  levels = pid_policies_l$label[order(all_policies$mean)])
pid_policies_l$subset <- factor(pid_policies_l$subset, levels = 
         c("Republican", "Democrat", "Independent/other"))
ggplot(pid_policies_l, aes(x = label, y = mean, ymin = mean + qnorm(0.025)*se,
                         ymax = mean + qnorm(0.975)*se, color = subset, shape = subset)) +
        geom_pointrange(position = position_dodge(width = 1)) + coord_flip() + 
        ylab("Mean level of support\n(0 = strongly oppose; 100 = strongly support)") +
  theme_bw() +
        geom_vline(xintercept=seq(0.5, 20, 1), colour="black", alpha = 0.2) + 
        scale_color_manual(values = c("red", "blue", "darkgreen"), 
                           name = "Party identification") +
        scale_shape_discrete(name = "Party identification") + theme_bw() +
        theme(legend.position = "bottom",
              panel.grid.major.y = element_blank()) +
          scale_x_discrete(name = "Policy")
ggsave("survey2_policy_support_pid_lucid.pdf",
      dpi = 300, width = 7.5, height = 7.5)
ggsave("survey2_policy_support_pid_lucid.png",
       dpi = 300, width = 8, height = 4.5)

##### SI Fig 6. Mean levels of support for surveillance policies by race

race_policies <- rbind(policy_support_subset(subset = d$race_short == "White", subset_name = "White"),
                       policy_support_subset(subset = d$race_short == "Black", subset_name = "Black"),
                       policy_support_subset(subset = d$race_short == "Latino", subset_name = "Latinx"),
                       policy_support_subset(subset = d$race_short == "Other", subset_name = "Other"))
race_policies$outcome_name <- factor(race_policies$outcome_name, 
                                     levels = all_policies$outcome_name[order(all_policies$mean)])
race_policies$subset <- factor(race_policies$subset, 
                               levels = c("White", "Black", "Latinx", "Other"))
ggplot(race_policies, aes(x = outcome_name, y = mean, ymin = mean + qnorm(0.025)*se,
                          ymax = mean + qnorm(0.975)*se, color = subset, shape = subset)) +
  geom_pointrange(position = position_dodge(width = 1)) + coord_flip() + 
  ylab("Mean level of support\n(0 = strongly oppose; 100 = strongly support)") +
  theme_bw() +
  geom_vline(xintercept=seq(0.5, 20, 1), colour="black", alpha = 0.2) + 
  scale_color_manual(values = c("indianred", "slateblue", "dodgerblue", "olivedrab"), 
                     name = "Race") +
  scale_shape_discrete(name = "Race") + theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_blank()) +
  scale_x_discrete(name = "Policy", 
                   labels = function(x) str_wrap(x, width = 25))
ggsave("survey2_policy_support_race.pdf",
       dpi = 300, width = 7.5, height = 10)

##### SI Fig 7. Distribution of support for traditional CT and smartphone apps by party identification

## Bottom panel: Response to smartphone contact tracing apps (by party ID)

categorical_summary <- function(outcome_var, outcome_name, s_weights = "weights",
                                shown_var, subset, subset_name, dataset = d,
                                x_values_labels) {
  x <- dataset[subset, outcome_var]
  shown <- dataset[subset, shown_var]
  s_weights <- dataset[subset, s_weights]
  x <- x[!is.na(shown)]
  s_weights <- s_weights[!is.na(shown)]
  x_missing <- x == -99 | is.na(x)
  # Get the missing percent
  percent_missing <- sum(x_missing)/length(x)
  # Convert all the missings to -99
  x[is.na(x)] <- -99
  # # recode to yes/no/don't know
  # x_categorical <- factor(x, levels = c(-88, 0, 1), labels = c("Don't know", "No", "Yes"))
  # Make the frequency table
  # unique values in x in descending order
  x_values <- unique(x)[order(unique(x), decreasing = TRUE)]
  # Here is some basic code to take into account the survey weights
  sum_func <- function(outcome_var, value, survey_weights) {
    se_md <- lm(outcome_var == value ~ 1, 
                weights = survey_weights)
    out <- coeftest(se_md, vcov = vcovHC(se_md, type="HC2"))
    return(data.frame(num = value, 
                      freq = sum(outcome_var == value),
                      prop = as.numeric(out[1]), 
                      se = as.numeric(out[2])))
  }
  perc_res <- do.call(rbind, lapply(x_values, sum_func, outcome_var = x, survey_weights = s_weights))
  res <- data.frame(subset_name,
                    outcome_name,
                    values = factor(x_values_labels, levels = rev(x_values_labels)),
                    perc_res, N = length(x))
  return(res)
}

# Break down the policy support into 5 bins
all_policy_support$support_cat <- NA
all_policy_support$support_cat[all_policy_support$support < 20] <- 1
all_policy_support$support_cat[all_policy_support$support >= 20 &
                                 all_policy_support$support < 40] <- 2
all_policy_support$support_cat[all_policy_support$support >= 40 &
                                 all_policy_support$support < 60] <- 3
all_policy_support$support_cat[all_policy_support$support >= 60 &
                                 all_policy_support$support < 80] <- 4
all_policy_support$support_cat[all_policy_support$support >= 80] <- 5
# variable for shown smartphone contact tracing app
all_policy_support$shown_smartphone_app <- all_policy_support$policy == "Smartphone contact tracing apps"
all_policy_support$shown_trad_contact_tracing <- all_policy_support$policy == "Traditional contact tracing"

all_support_app <- categorical_summary(outcome_var = "support_cat", s_weights = "weights",
                                       outcome_name = "Response to smartphone contact tracing apps",
                                       shown_var = "shown_smartphone_app",
                                       subset = all_policy_support$policy == "Smartphone contact tracing apps",
                                       dataset = all_policy_support,
                                       subset_name = "All Respondents",
                                       x_values_labels = rev(c("Strongly oppose", "Somewhat oppose", "Neutral",
                                                               "Somewhat support", "Strongly support")))

pid_support_func <- function(pid, outcome_name, shown_var, policy) {
  categorical_summary(outcome_var = "support_cat", s_weights = "weights", 
                      outcome_name = outcome_name, 
                      shown_var = shown_var, 
                      subset = all_policy_support$pid_l_short == pid &
                        all_policy_support$policy == policy, 
                      dataset = all_policy_support,
                      subset_name = pid,
                      x_values_labels = rev(c("Strongly oppose", "Somewhat oppose", "Neutral",
                                              "Somewhat support", "Strongly support")))  
}

support_app_c_pid <- rbind(all_support_app,
                           do.call(rbind, lapply(c("Democrat", "Republican",
                                                   "Independent/other"), 
                                                 pid_support_func,
                                                 outcome_name = "Response to smartphone contact tracing apps",
                                                 shown_var = "shown_smartphone_app",
                                                 policy = "Smartphone contact tracing apps")))

# clean up the results
support_app_c_pid$values <- factor(support_app_c_pid$values,
                                   rev(c("Strongly oppose", "Somewhat oppose", "Neutral",
                                         "Somewhat support", "Strongly support")))
support_app_c_pid$subset_name_N <- paste0(support_app_c_pid$subset_name, "\n(N = ", support_app_c_pid$N, ")")
support_app_c_pid$subset_name_N <- factor(support_app_c_pid$subset_name_N,
                                          rev(unique(support_app_c_pid$subset_name_N)))
# make the graph
ggplot(support_app_c_pid, aes(fill = values, y= prop*100, x = subset_name_N, label = round(prop*100))) + 
  ggtitle("Response to smartphone contact tracing apps (by party identification)") +
  geom_bar(position="stack", stat="identity", alpha = 0.7) + 
  geom_text(position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = rev(c('#7b3294','#c2a5cf','#f7f7f7','#a6dba0','#008837')),
                    name = "Response",
                    guide = guide_legend(reverse = TRUE)) +
  ylab("% respondents") + scale_x_discrete("Respondent type",
                                           labels = function(x) str_wrap(x, width = 25)) +
  coord_flip() + theme_bw() + theme(legend.position = "bottom")
ggsave("survey2_response_contact_tracing_app_pid.png", dpi = 300,
       height = 5, width = 10)
ggsave("survey2_response_contact_tracing_app_pid.pdf", dpi = 300,
       height = 5, width = 10)


## Top panel: Response to traditional contact tracing (by party ID)

all_support_trad <- categorical_summary(outcome_var = "support_cat", s_weights = "weights", 
                                        outcome_name = "Response to traditional contact tracing", 
                                        shown_var = "shown_trad_contact_tracing", 
                                        subset = all_policy_support$policy == "Traditional contact tracing", 
                                        dataset = all_policy_support,
                                        subset_name = "All Respondents",
                                        x_values_labels = rev(c("Strongly oppose", "Somewhat oppose", "Neutral",
                                                                "Somewhat support", "Strongly support")))
support_trad_c_pid <- rbind(all_support_trad,
                            do.call(rbind, lapply(c("Democrat", "Republican",
                                                    "Independent/other"), 
                                                  pid_support_func,
                                                  outcome_name = "Response to traditional contact tracing",
                                                  shown_var = "shown_trad_contact_tracing",
                                                  policy = "Traditional contact tracing")))
# clean up the results
support_trad_c_pid$values <- factor(support_trad_c_pid$values,
                                    rev(c("Strongly oppose", "Somewhat oppose", "Neutral",
                                          "Somewhat support", "Strongly support")))
support_trad_c_pid$subset_name_N <- paste0(support_trad_c_pid$subset_name, "\n(N = ", support_trad_c_pid$N, ")")
support_trad_c_pid$subset_name_N <- factor(support_trad_c_pid$subset_name_N,
                                           rev(unique(support_trad_c_pid$subset_name_N)))
# make the graph
ggplot(support_trad_c_pid, aes(fill = values, y= prop*100, x = subset_name_N, label = round(prop*100))) + 
  ggtitle("Response to traditional contact tracing (by party identification)") +
  geom_bar(position="stack", stat="identity", alpha = 0.7) + 
  geom_text(position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = rev(c('#7b3294','#c2a5cf','#f7f7f7','#a6dba0','#008837')),
                    name = "Response",
                    guide = guide_legend(reverse = TRUE)) +
  ylab("% respondents") + scale_x_discrete("Respondent type",
                                           labels = function(x) str_wrap(x, width = 25)) +
  coord_flip() + theme_bw() + theme(legend.position = "bottom")
ggsave("survey2_response_contact_trad_pid.png", dpi = 300,
       height = 5, width = 10)
ggsave("survey2_response_contact_trad_pid.pdf", dpi = 300,
       height = 5, width = 10)


##### SI Fig 17. Respondents' predicted outcomes of the surveillance policies

policy_matrix_names <- c("Help limit COVID-19 spread",
                         "Improve the economy",
                         "Make it safer to return to work",
                         "Make it safer to return to school",
                         "Make it safer to visit friends and family",
                         "Violate privacy",
                         "Violate civil liberties",
                         "Threaten democracy",
                         "Make tech companies too powerful")


# App outcomes matrix function
# Traditional contact tracing needs to be analyzed seperately
matrix_trad <- function(matrix_num, subset = rep(TRUE, nrow(d)), 
                        subset_name = "All Respondents") {
  categorical_summary(outcome_var = paste0("trad_ct_effects_", matrix_num), 
                      shown_var = "trad_ct_desc",
                      outcome_name = policy_matrix_names[matrix_num], 
                      subset = subset,
                      subset_name = subset_name,
                      x_values_labels = c("Yes", "No", "Don't know", "Missing"),
                      dataset = d) 
}
trad_matrix_res <- data.frame(policy_name = "Traditional contact tracing",
                              do.call(rbind, lapply(1:length(policy_matrix_names), matrix_trad)))

# Function to do the rest of the policies
policy_outcomes_matrix_summary <- function(policy_num, matrix_num, subset = rep(TRUE, nrow(d)), 
                                           subset_name = "All Respondents") {
  categorical_summary(outcome_var = paste("policy_effects", matrix_num, policy_num, sep = "_"), 
                      shown_var = paste0("policy_description_", policy_num),
                      outcome_name = policy_matrix_names[matrix_num], 
                      subset = subset,
                      subset_name = subset_name,
                      x_values_labels = c("Yes", "No", "Don't know", "Missing"),
                      dataset = d)        
}

policy_outcomes_matrix_results <- function(policy_num){
  res <- do.call(rbind, lapply(1:length(policy_matrix_names), policy_outcomes_matrix_summary, policy_num = policy_num))
  policy_name <- policy_outcomes[policy_num]
    return(data.frame(policy_name = policy_name, res))
}

# Create one plot that summarizes the results
policy_matrix_all <- do.call(rbind, lapply(c(1:length(policy_outcomes)), policy_outcomes_matrix_results))
policy_matrix_all <- rbind(trad_matrix_res, policy_matrix_all)
# Arrange order of the policies
policy_matrix_all$policy_name <- factor(policy_matrix_all$policy_name,
                                        unique(policy_matrix_all$policy_name))

ggplot(policy_matrix_all[policy_matrix_all$values == "Yes",], 
       aes(x = policy_name, y =  outcome_name, fill = prop))+
  geom_bin2d(color = "white") + 
  scale_y_discrete(name = "Policy outcome",
                   labels = function(x) str_wrap(x, width = 15)) +
  scale_x_discrete(name = "Policy",
                   labels = function(x) str_wrap(x, width = 15)) +
  geom_text(aes(x = policy_name, y = outcome_name, 
                label = round(prop*100)), color = "black", size = 3) + 
  scale_fill_gradient(name = "Percentage who responded 'yes'",
                      low = "white", high = "cornflowerblue", labels = scales::percent) + 
  theme_bw() + theme(legend.position = "bottom",
                     axis.text.x = element_text(angle = 270, hjust = 0)) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5,
                                barwidth = 15))
ggsave("policy_matrix_policy_heatmap.pdf",
       dpi = 300, width = 7.5, height = 8)


###############################################
##### Conjoint analysis #######################
###############################################

# Create short variable names for conjoint attributes

## App name
d$app_name[d$app_name == "a contact tracing"] <- "Contact tracing"
d$app_name[d$app_name == "an exposure notification"] <- "Exposure notification"
d$app_name <- factor(d$app_name)

## App developer
d$app_developer[d$app_developer == "A group of researchers at leading universities is"] <- "University researchers"
d$app_developer[d$app_developer == "Apple and Google are"] <- "Apple and Google"
d$app_developer[d$app_developer == "The Centers for Disease Control and Prevention (CDC) is"] <- "CDC"
d$app_developer[d$app_developer == "Your state government is"] <- "State government"
d$app_developer <- factor(d$app_developer)

## Tech used
d$tech_used[d$tech_used == "Bluetooth data that does not track users’ location"] <- "Bluetooth (no location tracking)"
d$tech_used[d$tech_used == "GPS data that track users’ location"] <- "GPS (location tracking)"
d$tech_used[d$tech_used == ""] <- "Bluetooth (no location tracking) with explainer"
d$tech_used <- factor(d$tech_used, levels = c("GPS (location tracking)",
                                              "Bluetooth (no location tracking)",
                                              "Bluetooth (no location tracking) with explainer"))

## Percent needed
d$percent_needed[d$percent_needed == "60"] <- "At least 60%"
d$percent_needed[d$percent_needed == "80"] <- "At least 80%"
d$percent_needed <- factor(d$percent_needed)

## Data storage
d$data_storage[grep("only when necessary", d$data_storage)] <- "Decentralized"
d$data_storage[grep("central server controlled", d$data_storage)] <- "Centralized"
d$data_storage <- factor(d$data_storage)

## Expire app
d$expire_app[grep("vaccine", d$expire_app)] <- "After vaccine found"
d$expire_app[grep("declares", d$expire_app)] <- "After CDC declares pandemic over"
d$expire_app[d$expire_app == ""] <- "No information"
d$expire_app <- factor(d$expire_app, levels = c("No information",
                                                "After vaccine found",
                                                "After CDC declares pandemic over"))

# Variable for whether the respondents saw the conjoint analysis
d$seen_ca <- !is.na(d$ca_graphic.0) | !is.na(d$ca_n_graphic)
# Variable for whether people answered the download/report question (based on having a smartphone)
d$cellphone_tf <- NA
d$cellphone_tf[d$cellphone == 1] <- TRUE
d$cellphone_tf[d$cellphone == 0] <- FALSE
d$self_app_shown <- d$cellphone_tf & !is.na(d$cellphone_tf)
d$self_app_shown[d$cellphone_type == 5] <- FALSE # have a cellphone but not a smartphone

# Check how many people saw the conjoint analysis and also own smartphones
table(d$seen_ca & d$self_app_shown, useNA = "always")


##### SI Fig 15. Distribution of responses to conjoint analysis questions

# Make the labels
app_outcome_labels <- c(
  'download_likely' = "Likelihood of downloading and using app",
  'report_likely' = "Likelihood of reporting positive test result",
  'guess_perc' = "Respondents' estimates of percentage\nin their town/city who will use app",
  'confident_data' = "Confidence that personal data will be protected",
  'auto_install_app' = "Apple and Google should\nautomatically install app",
  'gov_use_app' = "Statement: Government should\nrequire people to use app",
  'empl_use_app' = "Statement: Employers should\nrequire employees to use app",
  'rel_use_app' = "Statement: Places of worship\nshould require worshippers to use app",
  "obligation_report" = "Statement: Users should\nhave a choice to report positive result"
)

# Generate the "long" dataset
long_app_outcomes <- d[,names(app_outcome_labels)] %>%
  keep(is.numeric) %>% 
  gather()
long_app_outcomes <- long_app_outcomes[long_app_outcomes$value != -99 &
                                         long_app_outcomes$value != -88 &
                                         !is.na(long_app_outcomes$value),]
long_app_outcomes$key <- factor(long_app_outcomes$key,
                                levels = names(app_outcome_labels))

# Make the histogram
ggplot(long_app_outcomes, aes(value)) +
  facet_wrap(~ key, scales = "free", labeller = as_labeller(app_outcome_labels), 
             ncol = 2) + xlab("Values") + ylab("Count") +
  geom_histogram() + theme_bw()
ggsave("survey2_histograms_ca_app_outcomes.pdf", dpi = 300,
       height = 10, width = 7)

# Get the percent missing for each outcome variable
app_outcome_labels <- c(
  'download_likely' = "Likelihood of downloading and using app",
  'report_likely' = "Likelihood of reporting positive test result",
  'guess_perc' = "Respondents' estimates of percentage in their town/city who will use app",
  'confident_data' = "Confidence that personal data will be protected",
  'auto_install_app' = "Apple and Google should automatically install app",
  'gov_use_app' = "Statement: Government should require people to use app",
  'empl_use_app' = "Statement: Employers should require employees to use app",
  'rel_use_app' = "Statement: Places of worship should require worshippers to use app",
  "obligation_report" = "Statement: Users should have a choice to report positive result"
)

missing_getperc_apps <- function(num, shown_var = d$seen_ca) {
  x <- d[,names(app_outcome_labels)[num]]
  shown_var <- shown_var
  missing_dk <- mean(x[shown_var] == -99 | x[shown_var] == -88 | is.na(x[shown_var]))
  output <- data.frame(Outcome = app_outcome_labels[num], 
                       Percent_missing = missing_dk, N = length(x[shown_var]))
  row.names(output) <- NULL
  return(output)
}

app_outcome_missing <- 
  rbind(missing_getperc_apps(1, d$seen_ca & d$self_app_shown),
        missing_getperc_apps(2, d$seen_ca & d$self_app_shown),
        do.call(rbind, lapply(3:length(app_outcome_labels), missing_getperc_apps)))


# Clean up conjoint outcome variables

# Likelihood of downloading the app
# missing data indicator
d$download_likely_m <- NA
d$download_likely_m[(is.na(d$download_likely) | d$download_likely == -99) & 
                        d$seen_ca & d$self_app_shown] <- TRUE
d$download_likely_m[d$download_likely != -99 & !is.na(d$download_likely)] <- FALSE
# Mean impute
d$download_likely[(is.na(d$download_likely) | d$download_likely == -99) & 
                            d$seen_ca & d$self_app_shown] <-
        mean(d$download_likely[d$download_likely != -99 & !is.na(d$download_likely)])
mean(d$download_likely, na.rm = T)

# Likelihood of downloading: binary version
d$download_likely_bin <- ifelse(d$download_likely >= 60, 1, 0)*100
mean(d$download_likely_bin, na.rm = T)

# Reporting to the app
# missing data indicator
d$report_likely_m <- NA
d$report_likely_m[d$report_likely[d$report_likely != -99 & 
                                        !is.na(d$report_likely)]] <- TRUE
d$report_likely_m[d$report_likely != -99 & !is.na(d$report_likely)] <- FALSE
# mean impute
d$report_likely[(is.na(d$report_likely) | d$report_likely == -99) & 
                          d$seen_ca & d$self_app_shown] <-
        mean(d$report_likely[d$report_likely != -99 & !is.na(d$report_likely)])
# Reporting to the app (binary version)
d$report_likely_bin <- ifelse(d$report_likely >= 60, 1, 0)*100
mean(d$report_likely_bin, na.rm = T)

# Guess percentage app use
d$guess_perc[(is.na(d$guess_perc) | 
                       d$guess_perc == -99) & d$seen_ca] <-
        mean(d$guess_perc[d$guess_perc != -99 & !is.na(d$guess_perc)])

# Confidence that data will be protected
# missing data indicator
d$confident_data_m <- NA
d$confident_data_m[(is.na(d$confident_data) | d$confident_data == -99) & d$seen_ca] <- TRUE
d$confident_data_m[d$confident_data != -99 & !is.na(d$confident_data)] <- FALSE
# mean impute
d$confident_data[(is.na(d$confident_data) | d$confident_data == -99) & d$seen_ca] <-
        mean(d$confident_data[d$confident_data != -99 & !is.na(d$confident_data)])

# Agreement block - creating continuous version for conjoint analysis (keeping originals without mean imputation for plots)
# Government should require people to use app
d$gov_use_app_imp <- d$gov_use_app
# missing data indicator
d$gov_use_app_imp_m <- NA
d$gov_use_app_imp_m[(is.na(d$gov_use_app) | d$gov_use_app == -99) & d$seen_ca] <- TRUE
d$gov_use_app_imp_m[d$gov_use_app != -99 & !is.na(d$gov_use_app)] <- FALSE
# mean impute
d$gov_use_app_imp[(is.na(d$gov_use_app) | d$gov_use_app == -99) & d$seen_ca] <-
        mean(d$gov_use_app[d$gov_use_app != -99 & !is.na(d$gov_use_app)])

# Apple and Google should automatically install the app
d$auto_install_app_imp <- d$auto_install_app
# missing data indicator
d$auto_install_app_imp_m <- NA
d$auto_install_app_imp_m[(is.na(d$auto_install_app) | d$auto_install_app == -99) & 
                           d$seen_ca] <- TRUE
d$auto_install_app_imp_m[d$auto_install_app != -99 & 
                                              !is.na(d$auto_install_app)] <- FALSE
# mean impute
d$auto_install_app_imp[(is.na(d$auto_install_app) | d$auto_install_app == -99) & 
                            d$seen_ca] <-
        mean(d$auto_install_app[d$auto_install_app != -99 & !is.na(d$auto_install_app)])

# Employers should require employees to use the app
d$empl_use_app_imp <- d$empl_use_app
# missing data indicator
d$empl_use_app_imp_m <- NA
d$empl_use_app_imp_m[(is.na(d$empl_use_app) | d$empl_use_app == -99) & d$seen_ca] <- TRUE
d$empl_use_app_imp_m[d$empl_use_app != -99 & !is.na(d$empl_use_app)] <- FALSE
table(d$empl_use_app_imp_m, useNA = "always")
# mean impute
d$empl_use_app_imp[(is.na(d$empl_use_app) | d$empl_use_app == -99) & d$seen_ca] <-
        mean(d$empl_use_app[d$empl_use_app != -99 & !is.na(d$empl_use_app)])

# Places of religious worship should require those who want to worship there to use the app
d$rel_use_app_imp <- d$rel_use_app
# missing data indicator
d$rel_use_app_imp_m <- NA
d$rel_use_app_imp_m[(is.na(d$rel_use_app) | d$rel_use_app == -99) & d$seen_ca] <- TRUE
d$rel_use_app_imp_m[d$rel_use_app != -99 & !is.na(d$rel_use_app)] <- FALSE
# mean impute
d$rel_use_app_imp[(is.na(d$rel_use_app) | d$rel_use_app == -99) & d$seen_ca] <-
        mean(d$rel_use_app[d$rel_use_app != -99 & !is.na(d$rel_use_app)])


# People should have a choice in sharing if they test positive
d$obligation_report_imp <- d$obligation_report
# missing data indicator
d$obligation_report_imp_m <- NA
d$obligation_report_imp_m[(is.na(d$obligation_report) | d$obligation_report == -99) & d$seen_ca] <- TRUE
d$obligation_report_imp_m[d$obligation_report != -99 & !is.na(d$obligation_report)] <- FALSE
# mean impute
d$obligation_report_imp[(is.na(d$obligation_report) | d$obligation_report == -99) & d$seen_ca] <-
        mean(d$obligation_report[d$obligation_report != -99 & !is.na(d$obligation_report)])

# manipulation check
# location tracking
d$recall_check_1_c <- NA
d$recall_check_1_c[d$recall_check_1 == 1 & d$tech_used == "GPS (location tracking)"] <- 1
d$recall_check_1_c[d$recall_check_1 == 1 & d$tech_used != "GPS (location tracking)"] <- 0
d$recall_check_1_c[d$recall_check_1 == 0 & d$tech_used == "GPS (location tracking)"] <- 0
d$recall_check_1_c[d$recall_check_1 == 0 & d$tech_used != "GPS (location tracking)"] <- 1
# identify name
d$recall_check_2_c <- NA
d$recall_check_2_c[d$recall_check_2 == 1] <- 0
d$recall_check_2_c[d$recall_check_2 == 0] <- 1
# data storage
d$recall_check_3_c <- NA
d$recall_check_3_c[d$recall_check_3 == 1 & d$tech_used == "Centralized"] <- 1
d$recall_check_3_c[d$recall_check_3 == 1 & d$tech_used != "Centralized"] <- 0
d$recall_check_3_c[d$recall_check_3 == 0 & d$tech_used == "Centralized"] <- 0
d$recall_check_3_c[d$recall_check_3 == 0 & d$tech_used != "Centralized"] <- 1
# who is building the app
d$recall_check_4_c <- NA
d$recall_check_4_c[d$recall_check_4 == 1 & d$app_developer == "Apple and Google"] <- 1
d$recall_check_4_c[d$recall_check_4 == 1 & d$app_developer != "Apple and Google"] <- 0
d$recall_check_4_c[d$recall_check_4 == 0 & d$app_developer == "Apple and Google"] <- 0
d$recall_check_4_c[d$recall_check_4 == 0 & d$app_developer != "Apple and Google"] <- 1
# percentage needed
d$recall_check_5_c <- NA
d$recall_check_5_c[d$recall_check_5 == 1 & d$percent_needed == "At least 60%"] <- 1
d$recall_check_5_c[d$recall_check_5 == 1 & d$percent_needed != "At least 60%"] <- 0
d$recall_check_5_c[d$recall_check_5 == 0 & d$percent_needed == "At least 60%"] <- 0
d$recall_check_5_c[d$recall_check_5 == 0 & d$percent_needed != "At least 60%"] <- 1

##### SI Table 14. Proportion of correct responses to statements in the manipulation check

d$mc_sum <- d$recall_check_1_c + d$recall_check_2_c + 
  d$recall_check_3_c + d$recall_check_4_c +
  d$recall_check_5_c

mc_labels <- c("The app will track your location data.",
               "The app will send you the names of infected people you have been in close contact with.",
               "All user data will be stored on a central server.",
               "Apple and Google are building this app.",
               "Public health experts say that at least 60% of smartphone users needs to use this app for it to be effective at limiting the spread of COVID-19.")

mc_table_func <- function(num) {
  data.frame(Statement = mc_labels[num], 
             Proportion = roundfunc(mean(d[,paste0("recall_check_", num, "_c")], 
                                           na.rm = TRUE), 2))
}
knitr::kable(do.call(rbind, lapply(1:5, mc_table_func)), format = "latex", 
             col.names = c("Statement", "Proportion answered correctly"), 
             caption = "Proportion of correct responses to each statement in the conjoint analysis manipulation check")

# raw frequency
table(d$mc_sum[d$seen_ca], useNA = "always")
# percentage
table(d$mc_sum[d$seen_ca], useNA = "always")/sum(d$seen_ca)
# Mean impute the miissing
d$mc_sum_c <- d$mc_sum
d$mc_sum_c[is.na(d$mc_sum) & d$seen_ca] <- mean(d$mc_sum_c[!is.na(d$mc_sum) & d$seen_ca])
# Mean number of correct responses
mean(d$mc_sum_c[d$seen_ca])
# Create a variable that's the numbers of questions wrong
d$mc_sum_w <- 5 - d$mc_sum_c
# Identifying name
mean(d$recall_check_2_c[d$seen_ca], na.rm = TRUE)

##### SI Table 12. Predicting number of wrong answers to conjoint analysis manipulation check

# Get the mean policy support 
mean_policy_support <- all_policy_support %>% group_by(rid) %>% dplyr::summarise(
  mean_policy_support = mean(support)
)
d <- merge(x = d, y = mean_policy_support, all.x = T, by = "rid")
# Run the regression
wrong_formula <- paste0("mc_sum_w ~ mean_policy_support + ", paste(demographics, collapse = " + "))
wrong_md <- lm_robust(formula = as.formula(wrong_formula), data = d[d$seen_ca,])
wrong_md_coef <- c("(Intercept)", "Mean support for public health surevillance policies", predictor_names)
names(wrong_md_coef) <- names(wrong_md$coefficients)
modelsummary::modelsummary(wrong_md, output = "latex",
                           coef_map = wrong_md_coef,
                           title = "Predicting the number of wrong manipulation check answers using support for surevillance policy and demographic variables",
                           stars = c(
                             "*" = 0.05,
                             "**" = 0.01,
                             "***" = 0.001
                           ))

##### SI Table 13. Association between outcomes in conjoint analysis experiments and wrong answers in manipulation check

# Likelihood of downloading
wrong_formula_download <- paste0("download_likely ~ mc_sum_w + ", paste(demographics, collapse = " + "))
wrong_download_1 <- lm_robust(formula = download_likely ~ mc_sum_w, data = d[d$self_app_shown,])
wrong_download_2 <- lm_robust(formula = as.formula(wrong_formula_download), data = d[d$self_app_shown,])
# Likelihood of reporting
wrong_formula_report <- paste0("report_likely ~ mc_sum_w + ", paste(demographics, collapse = " + "))
wrong_report_1 <- lm_robust(formula = report_likely ~ mc_sum_w, data = d[d$self_app_shown,])
wrong_report_2 <- lm_robust(formula = as.formula(wrong_formula_report), data = d[d$self_app_shown,])
# Government should get everyone to download it
wrong_formula_gov_use_app <- paste0("gov_use_app_imp ~ mc_sum_w + ", paste(demographics, collapse = " + "))
wrong_gov_use_app_1 <- lm_robust(formula = gov_use_app_imp ~ mc_sum_w, data = d[d$seen_ca,])
wrong_gov_use_app_2 <- lm_robust(formula = as.formula(wrong_formula_gov_use_app), data = d[d$seen_ca,])

# Output to LaTeX
coef_outcome_wrong <- c("(Intercept)", "Number of wrong answers in the manipulation check")
names(coef_outcome_wrong) <- names(wrong_report_1$coefficients)
modelsummary::modelsummary(list(wrong_download_1, wrong_download_2), output = "latex",
                           coef_map = coef_outcome_wrong,
                           stars = c(
                             "*" = 0.05,
                             "**" = 0.01,
                             "***" = 0.001
                           ))
modelsummary::modelsummary(list(wrong_report_1, wrong_report_2), output = "latex",
                           coef_map = coef_outcome_wrong,
                           stars = c(
                             "*" = 0.05,
                             "**" = 0.01,
                             "***" = 0.001
                           ))
modelsummary::modelsummary(list(wrong_gov_use_app_1, wrong_gov_use_app_2), output = "latex",
                           coef_map = coef_outcome_wrong,
                           stars = c(
                             "*" = 0.05,
                             "**" = 0.01,
                             "***" = 0.001
                           ))

# Function to get out the AMCE results and plot 

amce_analysis <- function(outcome_var, graph_title, shown, units) {
        attribute_names <- c("App developer", "App name",
                             "Data storage", "Expiration",
                             "Percentage of smartphone users needed",
                             "Technology used")
        regression_formula <- as.formula(paste0(outcome_var, " ~ app_name + app_developer + tech_used + 
                         percent_needed + data_storage + expire_app"))
        app_amce <- amce(regression_formula,
                         data = d[shown,], 
                         cluster = FALSE, design = "uniform", 
                         baselines = list(appdeveloper = "Apple and Google",
                                          appname = "Contact tracing",
                                          datastorage = "Centralized",
                                          expireapp = "No information",
                                          percentneeded = "At least 60%",
                                          techused = "GPS (location tracking)"))
        # Make the plot
        plot(app_amce,
                     main = graph_title,
                     attribute.names = attribute_names, 
                     xlab = paste0("Average marginal component effect ", units))
        ggsave(paste0("survey2_ca_graph_", outcome_var, ".pdf"),
               dpi = 300, width = 10, heigh = 8)
        ggsave(paste0("survey2_ca_graph_", outcome_var, ".png"),
               dpi = 300, width = 10, heigh = 5)
       return(summary(app_amce))
}

##### Fig 4. Average marginal component effects of contact tracing app attributes on
##### reported likelihood of downloading the app

ca_download_likely <- 
        amce_analysis(outcome_var = "download_likely", shown = d$seen_ca & d$self_app_shown, 
              graph_title = "Likelihood of downloading and using app",
              units = "(in percentage points)")

##### SI Fig 8. Average marginal component effects of contact tracing app attributes on
##### probability likely to download the app

ca_download_likely_bin <- 
        amce_analysis(outcome_var = "download_likely_bin", shown = d$seen_ca & d$self_app_shown, 
                      graph_title = "Likely or very likely to download app",
                      units = "(%)")

##### SI Fig 10. Average marginal component effects of contact tracing app attributes on
##### reported likelihood of reporting a positive COVID test to the app

ca_report_likely_1 <- 
        amce_analysis(outcome_var = "report_likely", 
                      shown = d$seen_ca & d$self_app_shown, 
                      graph_title = "Likelihood of reporting positive test result", 
                      units = "(in percentage points)")

##### SI Fig 11. Average marginal component effects of contact tracing app attributes on
##### confidence that personal data will be protected

ca_confident_data <- 
  amce_analysis(outcome_var = "confident_data", 
                shown = d$seen_ca,
                graph_title = "Confidence that personal data will be protected", 
                units = "(4-point scale; 0 = not confident at all; 4 = very confident)")

##### SI Fig 12. Average marginal component effects of contact tracing app attributes on
##### percentage of people in respondents' town or city they believe would be likely to download the app

ca_guess_perc <- 
        amce_analysis(outcome_var = "guess_perc", 
                      shown = d$seen_ca,
                      graph_title = "Respondents' estimates of percentage in their town/city who will use app", 
                      units = "(in percentage points)")

##### SI Fig 13. Average marginal component effects of contact tracing app attributes on
##### number of correct answers to the manipulation check

ca_mc_sum_c <- 
  amce_analysis(outcome_var = "mc_sum_c", shown = d$seen_ca, 
                graph_title = "Manipulation check",
                units = "(number of correct answers out of 5)")

# Marginal means

library(cregg)
attribute_order <- c("data_storage",
                     "tech_used",
                     "app_name",
                     "app_developer",
                     "percent_needed",
                     "expire_app")
attribute_label <- list(data_storage = "Data storage", 
                        tech_used = "Technology used", 
                        app_name = "App name",
                        app_developer = "App developer", 
                        percent_needed = "Percentage of smartphone users needed", 
                        expire_app = "Expiration")

# Label the features
var_label(d$data_storage) <- "Data storage"
var_label(d$tech_used) <- "Technology used"
var_label(d$app_name) <- "App name"
var_label(d$app_developer) <- "App developer"
var_label(d$percent_needed) <- "Percentage of smartphone users needed"
var_label(d$expire_app) <- "Expiration"

##### SI Fig 9. Marginal mean probability a respondent is likely to download the app for each
##### conjoint attribute value

regression_formula_bin <- formula("download_likely_bin ~ app_name + app_developer + tech_used +  percent_needed + data_storage + expire_app")
mm_res_bin <- cregg::mm(data = d[d$seen_ca & d$self_app_shown, ], regression_formula_bin, 
               feature_order = attribute_order, feature_labels = attribute_label)
plot(mm_res_bin, 
     xlab = "Marginal mean (% that would download and use app)") +
  ggtitle("Likelihood of downloading and using the app based on features of the app")
ggsave("survey2_ca_graph_mm_download_likely_bin.pdf",
       dpi = 300, width = 10, heigh = 8)
ggsave("survey2_ca_graph_mm_download_likely_bin.png",
       dpi = 300, width = 10, heigh = 6)


##### SI Table 5. Marginal means from the conjoint analysis: likelihood to download and use app

# marginal means for continuous download outcome
regression_formula_cont <- as.formula(paste0("download_likely", " ~ app_name + app_developer + tech_used + 
                         percent_needed + data_storage + expire_app"))
mm_res_cont <- cregg::mm(data = d[d$seen_ca & d$self_app_shown,], regression_formula_cont, 
                         feature_order = attribute_order, feature_labels = attribute_label)
nrow(d[d$seen_ca & d$self_app_shown,])
# generate table
mm_tab <- data.frame(mm_res_cont[,c("feature", "level", "estimate", "std.error")], 
           mm_res_bin[, c("estimate")])
knitr::kable(mm_tab, format = "latex", digits = 2, booktab = TRUE,
             caption = "Marginal means from the conjoint analysis: likelihood to download and use app", 
             col.names = c("Feature type", "Feature", "Marginal means (likelihood)", "SE", "Marginal means (percent with >60% likelihood)"))

##### SI Table 6. Marginal means for agreement that government should require use of app

# Function to generate marginal means table
mm_tab_fun <- function(var, caption){
  # marginal means
  regression_formula <- as.formula(paste0(var, " ~ app_name + app_developer + tech_used + 
                         percent_needed + data_storage + expire_app"))
  mm_res <- cregg::mm(data = d[d$seen_ca,], regression_formula, 
                      feature_order = attribute_order, feature_labels = attribute_label)
  # overall means and SE
  myvar <- d[, var]
  mod_overall <- lm(myvar[d$seen_ca] ~ 1)
  # generate table
  mm_tab <- data.frame(feature = c("Overall", as.character(mm_res$feature)),
                       level = c("--", as.character(mm_res$level)),
                       estimate = c(summary(mod_overall)$coefficients[1], mm_res$estimate),
                       std.error = c(summary(mod_overall)$coefficients[2], mm_res$std.error))
  
  knitr::kable(mm_tab, format = "latex", digits = 2, booktab = TRUE,
               caption = caption, 
               col.names = c("Feature type", "Feature", "Prop. agree", "SE"))
  
}

# Create binary version of agreement variable
d$gov_use_app_bin <- ifelse(d$gov_use_app_imp > 0, 1, 0)
# Create table
mm_tab_fun("gov_use_app_bin", caption = "Conjoint analysis results: percentage agreement with statement ‘The government should require
everyone who has a smartphone to use this app.’ Respondents who answered “strongly agree” or “somewhat
agree” are counted as those who agree.")

##### SI Table 7. Marginal means for agreement that employers should require use of app

d$empl_use_app_bin <- ifelse(d$empl_use_app_imp > 0, 1, 0)
mm_tab_fun("empl_use_app_bin", "Conjoint analysis results: percentage agreement with statement ‘Employers should require
their employees with smartphones to use this app.’ Respondents who answered “strongly agree” or “somewhat
agree” are counted as those who agree.")


##### SI Table 8. Marginal means for agreement that Apple and Google should automatically install the app

d$auto_install_app_bin <- ifelse(d$auto_install_app_imp > 0, 1, 0)
mm_tab_fun("auto_install_app_bin", "Conjoint analysis results: percentage agreement with statement ‘Apple and Google should
automatically install this app on users’ iPhones or Android phones as part of a software update.’ Respondents
who answered “strongly agree” or “somewhat agree” are counted as those who agree.")

##### SI Table 9. Marginal means for agreement that places of worship should require use of app

d$rel_use_app_bin <- ifelse(d$rel_use_app_imp > 0, 1, 0)
mm_tab_fun("rel_use_app_bin", "Conjoint analysis results: percentage agreement with statement ‘Places of religious worship,
like churches, synagogues, and mosques, should require everyone who has a smartphone to use this app if
they want to worship there.’ Respondents who answered “strongly agree” or “somewhat agree” are counted as
those who agree.")

##### SI Table 10. Marginal means for app users should have a choice to report positive test

d$obligation_report_bin <- ifelse(d$obligation_report_imp > 0, 1, 0)
mm_tab_fun("obligation_report_bin", "Conjoint analysis results: percentage agreement with statement ‘App users should have a
choice in sharing their test outcomes with the app if they tested positive for COVID-19.’ Respondents who
answered “strongly agree” or “somewhat agree” are counted as those who agree.")

##### Fig 5. Estimated effects of COVID-19 experience block priming on support for smartphone apps

experience_prime_analysis <- function(outcome_var, outcome_name, shown = d$seen_ca) {
        lm_output <- lm_lin(as.formula(paste0("scale(", outcome_var, ") ~ experience_prime")), 
                            covariates = ~ app_name + app_developer + tech_used + 
                                    percent_needed + data_storage + expire_app,
                            data = d[shown,])
        output_estimate <- data.frame(outcome_name = outcome_name,
                   estimate = lm_output$coefficients[2],
                   se = lm_output$std.error[2],
                   p = lm_output$p.value[2],
                   n = lm_output$nobs)
        row.names(output_estimate) <- NULL
        return(output_estimate)
}

# Estimated effect of priming on responses
priming_results <- rbind(
        experience_prime_analysis("download_likely", "Likelihood of downloading and using app", 
                                  shown = d$seen_ca & d$self_app_shown),
        experience_prime_analysis("report_likely", "Likelihood of reporting positive test result", 
                                  shown = d$seen_ca & d$self_app_shown),
           experience_prime_analysis("gov_use_app", 
                                  "Statement: Government should require people to use app")
)

# Clean up the factors for data viz

priming_results$outcome_name <- factor(priming_results$outcome_name, 
                                       levels = rev(priming_results$outcome_name))
priming_results$labels <- paste(priming_results$outcome_name, "\n(N = ", priming_results$n, ")", sep = "")
priming_results$labels <- factor(priming_results$labels,
                                 levels = priming_results$labels[order(priming_results$outcome_name)])

# Make the ggplot
ggplot(priming_results, aes(x = labels, y = estimate, 
                            ymin = estimate + qnorm(0.025)*se,
                            ymax = estimate + qnorm(0.975)*se)) +
        geom_hline(yintercept = 0, alpha = 0.75, color = "red", linetype = 2) +
        ylab("Standardized effect size; controlling for app features") +
        geom_pointrange() + scale_x_discrete(name = "Outcome",  
                                             labels = priming_results$labels[order(priming_results$outcome_name)]) +
        coord_flip() + theme_bw()
ggsave("survey2_covid_experience_priming.pdf",
       dpi = 300, width = 7.5, height = 3)
ggsave("survey2_covid_experience_priming.pdf",
       dpi = 300, width = 7.5, height = 3)


