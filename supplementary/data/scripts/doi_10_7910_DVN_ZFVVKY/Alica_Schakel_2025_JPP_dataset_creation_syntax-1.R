
### This is the replication material for the following article:
# Alica, Berkay and Schakel, Arjan H. (2025) 'Does multilevel government
# increase legitimacy? Citizens' preferences for subnational authority and
# acceptance of governmental decisions,' Journal of Public Policy, forthcoming. ###

# The article relies on Norwegian Citizen Panel Round 19 which can be downloaded
# here: https://doi.org/10.18712/NSD-NSD2940-V6.
# This replication R-script is to produce a replication dataset that can be used
# to reproduce the figures and tables presented in the paper and in the Appendix.


setwd("/Users/uibmacbookpro/Desktop/WtA") # Set up your own working directory.
dir() # See your working directory.

# 1. Download all necessary packages for producing the dataset and conducting analyses. ####

my_packages <- c("foreign", "car", "MASS", "stargazer", "ggplot2", "ggeffects",
                 "glm.predict", "haven", "dplyr", "viridis", "brant", "fixest",
                 "DescTools", "devtools", "coreSim", "separationplot",
                 "reshape2", "modmarg", "ggthemes", "gridExtra", "scales",
                 "repmis", "FactoMineR", "corrplot", "psych", "rms", "ordinal",
                 "effects", "erer", "arm", "lme4", "sjPlot", "jtools", "expss",
                 "Hmisc", "gmodels", "pollster", "installr", 'tidyr',
                 'marginaleffects', 'ggpubr', 'patchwork', 'modelsummary')

my_installed_packages <- my_packages %in% rownames(installed.packages())
if (any(my_installed_packages == FALSE)) {
  install.packages(my_packages[!my_installed_packages])
}

invisible(lapply(my_packages, library, character.only = TRUE))


# Import the dataset and save it as NCP19.
NCP19 <- read_sav("Norwegian Citizen Panel - round 19 - v-102-O.sav")

my_vars_19 <- c("responseid", "r19rekruttert", "r19interview_start",
                "r19interview_end",
                "r19tdrpreferanser_1", "r19tdrpreferanser_2",
                "r19tdrpreferanser_3", "r19tdrpreferanser_4",
                "r19tdrpreferanser_5", "r19tdrpreferanser_6",
                "r19tdrgodta_a2_ran", "r19tdrgodta_b2_ran",
                "r19tdrgodta_c2_ran", "r19tdrgodta_a1", "r19tdrgodta_a2",
                "r19tdrgodta_b1", "r19tdrgodta_b2", "r19tdrgodta_c1",
                "r19tdrgodta_c2")

NCP19 <- NCP19[my_vars_19]


NCP19[NCP19 >= 97] <- NA

MyData <- NCP19[complete.cases(NCP19[ , c("r19tdrpreferanser_1", "r19tdrpreferanser_2",
                                      "r19tdrpreferanser_3", "r19tdrpreferanser_4",
                                      "r19tdrpreferanser_5", "r19tdrpreferanser_6",
                                      "r19tdrgodta_a2_ran", "r19tdrgodta_b2_ran",
                                      "r19tdrgodta_c2_ran", "r19tdrgodta_a1", "r19tdrgodta_a2",
                                      "r19tdrgodta_b1", "r19tdrgodta_b2", "r19tdrgodta_c1"
)]), ]


# 2. Recode variables ####

## 2.1. Recode dependent variables ####

# Q A1: Municipality closes kindergarten. To what extent decision acceptable?

MyData$a1_answer <- NA
MyData$a1_answer[MyData$r19tdrgodta_a1 == 5] <- 1 # "Not at all" coded as 1
MyData$a1_answer[MyData$r19tdrgodta_a1 == 4] <- 2 # "Ta a small extent" as 2
MyData$a1_answer[MyData$r19tdrgodta_a1 == 3] <- 3 # "To some extent" coded as 3
MyData$a1_answer[MyData$r19tdrgodta_a1 == 2] <- 4 # "To a large extent" as 4
MyData$a1_answer[MyData$r19tdrgodta_a1 == 1] <- 5 # To a very large extent" as 5


# Q. A2: Mun closes kind. After learning the support of other tiers, how acc.?

MyData$a2_answer <- NA
MyData$a2_answer[MyData$r19tdrgodta_a2 == 5] <- 1 # "Not at all" coded as 1
MyData$a2_answer[MyData$r19tdrgodta_a2 == 4] <- 2 # "Ta a small extent" as 2
MyData$a2_answer[MyData$r19tdrgodta_a2 == 3] <- 3 # "To some extent" coded as 3
MyData$a2_answer[MyData$r19tdrgodta_a2 == 2] <- 4 # "To a large extent" as 4
MyData$a2_answer[MyData$r19tdrgodta_a2 == 1] <- 5 # To a very large extent" as 5

# Q B1: County closes videregående. To what extent is this acceptable?

MyData$b1_answer <- NA
MyData$b1_answer[MyData$r19tdrgodta_b1 == 5] <- 1 # "Not at all" coded as 1
MyData$b1_answer[MyData$r19tdrgodta_b1 == 4] <- 2 # "Ta a small extent" as 2
MyData$b1_answer[MyData$r19tdrgodta_b1 == 3] <- 3 # "To some extent" coded as 3
MyData$b1_answer[MyData$r19tdrgodta_b1 == 2] <- 4 # "To a large extent" as 4
MyData$b1_answer[MyData$r19tdrgodta_b1 == 1] <- 5 # To a very large extent" as 5

# Q B2: County closes videregående. To what extent is this acceptable, if supp.?

MyData$b2_answer <- NA
MyData$b2_answer[MyData$r19tdrgodta_b2 == 5] <- 1 # "Not at all" coded as 1
MyData$b2_answer[MyData$r19tdrgodta_b2 == 4] <- 2 # "Ta a small extent" as 2
MyData$b2_answer[MyData$r19tdrgodta_b2 == 3] <- 3 # "To some extent" coded as 3
MyData$b2_answer[MyData$r19tdrgodta_b2 == 2] <- 4 # "To a large extent" as 4
MyData$b2_answer[MyData$r19tdrgodta_b2 == 1] <- 5 # To a very large extent" as 5

# Q C1: Nat. closes høyskole To what extent is this acceptable?

MyData$c1_answer <- NA
MyData$c1_answer[MyData$r19tdrgodta_c1 == 5] <- 1 # "Not at all" coded as 1
MyData$c1_answer[MyData$r19tdrgodta_c1 == 4] <- 2 # "Ta a small extent" as 2
MyData$c1_answer[MyData$r19tdrgodta_c1 == 3] <- 3 # "To some extent" coded as 3
MyData$c1_answer[MyData$r19tdrgodta_c1 == 2] <- 4 # "To a large extent" as 4
MyData$c1_answer[MyData$r19tdrgodta_c1 == 1] <- 5 # To a very large extent" as 5

# Q C2: Nat. closes høyskole To what extent is this acceptable?

MyData$c2_answer <- NA
MyData$c2_answer[MyData$r19tdrgodta_c2 == 5] <- 1 # "Not at all" coded as 1
MyData$c2_answer[MyData$r19tdrgodta_c2 == 4] <- 2 # "Ta a small extent" as 2
MyData$c2_answer[MyData$r19tdrgodta_c2 == 3] <- 3 # "To some extent" coded as 3
MyData$c2_answer[MyData$r19tdrgodta_c2 == 2] <- 4 # "To a large extent" as 4
MyData$c2_answer[MyData$r19tdrgodta_c2 == 1] <- 5 # To a very large extent" as 5

# Create a "Change" in Willingness Variable for DV #

MyData$change_A <- MyData$a2_answer - MyData$a1_answer
MyData$change_B <- MyData$b2_answer - MyData$b1_answer
MyData$change_C <- MyData$c2_answer - MyData$c1_answer

# Create a Three-level and Dummy Change Var #

MyData$chng_A_ord <- NA
MyData$chng_A_ord[MyData$change_A < 0] <- -1
MyData$chng_A_ord[MyData$change_A == 0] <- 0
MyData$chng_A_ord[MyData$change_A > 0] <- 1

MyData$chng_A_dum <- NA
MyData$chng_A_dum[MyData$chng_A_ord <= 0] <- 0
MyData$chng_A_dum[MyData$chng_A_ord == 1] <- 1

MyData$chng_B_ord <- NA
MyData$chng_B_ord[MyData$change_B < 0] <- -1
MyData$chng_B_ord[MyData$change_B == 0] <- 0
MyData$chng_B_ord[MyData$change_B > 0] <- 1

MyData$chng_B_dum <- NA
MyData$chng_B_dum[MyData$chng_B_ord <= 0] <- 0
MyData$chng_B_dum[MyData$chng_B_ord == 1] <- 1

MyData$chng_C_ord <- NA
MyData$chng_C_ord[MyData$change_C < 0] <- -1
MyData$chng_C_ord[MyData$change_C == 0] <- 0
MyData$chng_C_ord[MyData$change_C > 0] <- 1

MyData$chng_C_dum <- NA
MyData$chng_C_dum[MyData$chng_C_ord <= 0] <- 0
MyData$chng_C_dum[MyData$chng_C_ord == 1] <- 1

## 2.2. Recode independent variables ####

# Create a character vector for the tier that supports the decision:

MyData$trtmnt_A <- NA

MyData$trtmnt_A[MyData$r19tdrgodta_a2_ran == 1] <- "C"

MyData$trtmnt_A[MyData$r19tdrgodta_a2_ran == 2] <- "N"

MyData$trtmnt_A[MyData$r19tdrgodta_a2_ran == 3] <- "C+N"



MyData$trtmnt_B <- NA

MyData$trtmnt_B[MyData$r19tdrgodta_b2_ran == 1] <- "M"

MyData$trtmnt_B[MyData$r19tdrgodta_b2_ran == 2] <- "N"

MyData$trtmnt_B[MyData$r19tdrgodta_b2_ran == 3] <- "M+N"



MyData$trtmnt_C <- NA

MyData$trtmnt_C[MyData$r19tdrgodta_c2_ran == 1] <- "M"

MyData$trtmnt_C[MyData$r19tdrgodta_c2_ran == 2] <- "C"

MyData$trtmnt_C[MyData$r19tdrgodta_c2_ran == 3] <- "M+C"



# Self-rule (SF) & Shared rule variables (SH) #
### Preferences for subnational authority are measured by six survey items;
# three survey items tap self-rule and three survey items to tap shared rule.
# Preferences ###for subnational authority is measured by all six survey items##

####The respondent was asked Norway has a system of governance comprising of
# three levels – the national, the regional (counties) and the municipal (municipality)
# level of governance. Please state if you think each of these is a desirable
# feature, or an undesirable feature of having different levels of government. # 

##Self-rule item 1: Having power divided up between different levels of government##

MyData$sf1 <- NA
MyData$sf1[MyData$r19tdrpreferanser_1 == 1] <- 4 # Very desirable
MyData$sf1[MyData$r19tdrpreferanser_1 == 2] <- 3 # Somewhat desirable
MyData$sf1[MyData$r19tdrpreferanser_1 == 3] <- 2 # Somewhat undesirable
MyData$sf1[MyData$r19tdrpreferanser_1 == 4] <- 1 # Very undesirable


##Self-rule item 2: Allowing different laws in response to varying needs and conditions in different parts of Norway##

MyData$sf2 <- NA 
MyData$sf2[MyData$r19tdrpreferanser_2 == 1] <- 4 # Very desirable
MyData$sf2[MyData$r19tdrpreferanser_2 == 2] <- 3 # Somewhat desirable
MyData$sf2[MyData$r19tdrpreferanser_2 == 3] <- 2 # Somewhat undesirable
MyData$sf2[MyData$r19tdrpreferanser_2 == 4] <- 1 # Very undesirable


##Self-rule item 3: Different levels of government having power to hold each other to account for problems##

MyData$sf3 <- NA
MyData$sf3[MyData$r19tdrpreferanser_3 == 1] <- 4 # Very desirable
MyData$sf3[MyData$r19tdrpreferanser_3 == 2] <- 3 # Somewhat desirable
MyData$sf3[MyData$r19tdrpreferanser_3 == 3] <- 2 # Somewhat undesirable
MyData$sf3[MyData$r19tdrpreferanser_3 == 4] <- 1 # Very undesirable


##Shared rule item 1: Allowing the governments of different parts of Norway to get involved in decision-making on national issues##

MyData$sh1 <- NA
MyData$sh1[MyData$r19tdrpreferanser_4 == 1] <- 4 # Very desirable
MyData$sh1[MyData$r19tdrpreferanser_4 == 2] <- 3 # Somewhat desirable
MyData$sh1[MyData$r19tdrpreferanser_4 == 3] <- 2 # Somewhat undesirable
MyData$sh1[MyData$r19tdrpreferanser_4 == 4] <- 1 # Very undesirable


##Shared rule item 2: Different governments arguing over the best way to solve a particular problem##

MyData$sh2 <- NA
MyData$sh2[MyData$r19tdrpreferanser_5 == 1] <- 4 # Very desirable
MyData$sh2[MyData$r19tdrpreferanser_5 == 2] <- 3 # Somewhat desirable
MyData$sh2[MyData$r19tdrpreferanser_5 == 3] <- 2 # Somewhat undesirable
MyData$sh2[MyData$r19tdrpreferanser_5 == 4] <- 1 # Very undesirable


##Shared rule item 3: Different levels of government being forced to respect each other’s roles and responsibilities when dealing with a problem ##

MyData$sh3 <- NA
MyData$sh3[MyData$r19tdrpreferanser_6 == 1] <- 4 # Very desirable
MyData$sh3[MyData$r19tdrpreferanser_6 == 2] <- 3 # Somewhat desirable
MyData$sh3[MyData$r19tdrpreferanser_6 == 3] <- 2 # Somewhat undesirable
MyData$sh3[MyData$r19tdrpreferanser_6 == 4] <- 1 # Very undesirable


#### Create interval variables for self-rule and shared rule 
MyData$sf_sum <- NA
MyData$sf_sum <- MyData$sf1 + MyData$sf2 + MyData$sf3
MyData$sh_sum <- MyData$sh1 + MyData$sh2 + MyData$sh3

MyData$sf_intvl <- scales::rescale(MyData$sf_sum)
MyData$sh_intvl <- scales::rescale(MyData$sh_sum)

MyData$sf_dum <- NA
MyData$sf_dum[MyData$sf_intvl < 0.6 ] <- 0
MyData$sf_dum[MyData$sf_intvl > 0.6 ] <- 1

MyData$sh_dum <- NA
MyData$sh_dum[MyData$sh_intvl < 0.7 ] <- 0
MyData$sh_dum[MyData$sh_intvl > 0.7 ] <- 1

MyData$psa <- NA # Create the preference for subnational authority variable.
MyData$psa <- (MyData$sf_intvl + MyData$sh_intvl) / 2

# Replace ALL 97 and 98 Values with NAs

# Identify numeric columns
numeric_cols <- sapply(MyData, is.numeric)

# Replace values only in numeric columns
MyData[numeric_cols][MyData[numeric_cols] >= 97] <- NA

my_df <- MyData[complete.cases(MyData[ , c("a1_answer", "a2_answer",
                                           "b1_answer", "b2_answer",
                                           "c1_answer", "c2_answer",
                                           "sf1", "sf2", "sf3",
                                           "sh1", "sh2", "sh3", "psa"
)]), ]


# 3. Create a long format dataset ####

###The survey asked three sets of two questions. The first question asked the
#willingness to accept a decision, and the second question asked the willingness
#to accept a decision after being informed that one or two other tiers of
#government supported the decisions. Each set of two questions was asked for the
#M (closing a kindergarten in your municipality), C (closing an upper secondary school in your municipality),
#N (closing a department at a university college). In the NCP dataset,
#respondent answers appear in six columns/variables. To be able to compare the
#impact of a respondent's preference for subnational tier towards the M, C and N
#tier one need to cluster the answers to the 1st and 2nd question by respondent.
#This means that the dataset needs to be transformed from a wide-format to a
#long-format whereby the respondents answers appear in two columns/variables, a
#new variable 'tier' with three values is created, and three tiers are clustered for each respondent.##


long_data <- pivot_longer(my_df, cols = starts_with("trtmnt"), names_to = "question",
                          values_to = "treatment")

long_data$first_answer_original <- ifelse(long_data$question == "trtmnt_A",
                                     long_data$a1_answer,
                                     long_data$b1_answer)

long_data$first_answer_original <- ifelse(long_data$question == "trtmnt_C",
                                     long_data$c1_answer,
                                     long_data$first_answer_original)


long_data$decision_maker <- NA
long_data$decision_maker[long_data$question == "trtmnt_A"] <- "M"
long_data$decision_maker[long_data$question == "trtmnt_B"] <- "C"
long_data$decision_maker[long_data$question == "trtmnt_C"] <- "N"

long_data$second_answer_original <- ifelse(long_data$question == "trtmnt_A",
                                  long_data$a2_answer,
                                  long_data$b2_answer)

long_data$second_answer_original <- ifelse(long_data$question == "trtmnt_C",
                                      long_data$c2_answer,
                                      long_data$second_answer_original)


# 4. Recoding the dependent variables for the main analysis ####

# We merge answer categories ‘to a large extent’ (= 4) and ‘to a very large extent’ (= 5)
# because of the low share of respondents in the latter category (see Figure 2 in the main text).
# See Appendix D below for the results for the unmerged answer categories 

# Make First Answer 4 Categories (Merge 4+5):
long_data$first_answer <- NA
long_data$first_answer[long_data$first_answer_original == 1] <- 1
long_data$first_answer[long_data$first_answer_original == 2] <- 2
long_data$first_answer[long_data$first_answer_original == 3] <- 3
long_data$first_answer[long_data$first_answer_original >= 4] <- 4

#### Make Second Answer 4 Categories (Merge 4+5):
long_data$second_answer <- NA
long_data$second_answer[long_data$second_answer_original == 1] <- 1
long_data$second_answer[long_data$second_answer_original == 2] <- 2
long_data$second_answer[long_data$second_answer_original == 3] <- 3
long_data$second_answer[long_data$second_answer_original >= 4] <- 4

#### Change variable categorization from continuous to ordinal:

long_data$first_answer_factor <- as.factor(long_data$first_answer)


# Correct the class and the order of variables:
long_data$responseid <- as.factor(long_data$responseid) #Individuals

long_data$tier_n <- factor(long_data$decision_maker,
                         levels = c("N", "M", "C"))

long_data$tier_m <- factor(long_data$decision_maker,
                         levels = c("M", "C", "N"))

long_data$tier_c <- factor(long_data$decision_maker,
                         levels = c("C", "M", "N"))

long_data$Support <- factor(long_data$treatment,
                          levels = c("N", "M", "C", "M+C", "M+N", "C+N"),
                          labels = c("National Government Supports",
                                     "Respondent's Municipality Supports",
                                     "Respondent's County Supports",
                                     "Municipality & County Support",
                                     "Municipality & National Government Support",
                                     "County & National Government Support"))

long_data$Support_c <- factor(long_data$treatment,
                            levels = c("C", "M", "N", "M+C", "M+N", "C+N"),
                            labels = c("Respondent's County Supports",
                                       "Respondent's Municipality Supports",
                                       "National Government Supports",
                                       "Municipality & County Support",
                                       "Municipality & National Government Support",
                                       "County & National Government Support"))

long_data$support_m <- factor(long_data$treatment,
                            levels = c("M", "C", "N", "M+C", "M+N",
                                       "C+N"),
                            labels = c("Respondent's Municipality Supports",
                                       "Respondent's County Supports",
                                       "National Government Supports",
                                       "Municipality & County Support",
                                       "Municipality & National Government Support",
                                       "County & National Government Support"))


long_vars <- c("responseid", "decision_maker", "first_answer", 'first_answer_factor',
               'first_answer_original', "treatment", 'second_answer',
               'second_answer_original', 'chng_A_ord', 'chng_B_ord', 'chng_C_ord',
               'tier_m', 'tier_c', 'tier_n', 'Support', 'Support_c', 'support_m',
               "sf1", 'sf2', 'sf3', 'sh1', 'sh2', 'sh3', 'sf_sum', 'sh_sum',
               'sf_intvl', 'sh_intvl', 'sf_dum', 'sh_dum', 'psa')


dataset <- long_data[long_vars]

# Save the dataset as an Rdata file:
saveRDS(dataset, file = "Alica_Schakel_2025_JPP_replication.rds")

# Save the dataset as a Stata data file:
write.dta(dataset, "Alica_Schakel_2025_JPP_replication.dta")


# Appendix F presents the results of a random-effects model with random
# effects for municipalities and an additional eight individual level control variables.
# These variables have been retrieved from five other NCP surveys:
# Norwegian Citizen Panel Round 11, Round 14, Round 16, Round 17, Round 18.
# They can be downloaded here https://surveybanken.sikt.no/en/series/ed271b1c-2595-47e4-8c97-3fcc00f02368

# IMPORTANT: Respondent IDs are anonymised and the datasets can only be merged
# working in a shielded/protected environment because of GDPR-rules. We used the
# SAFE-environment of the University of Bergen. The merging can be replicated in
# the SAFE-environment (syntax can be provided by Berkay Alica). We have an
# anonymised version of the dataset entitled "Alica_Schakel_2025_JPP_RE_replication_dataset.rds"

# END ####