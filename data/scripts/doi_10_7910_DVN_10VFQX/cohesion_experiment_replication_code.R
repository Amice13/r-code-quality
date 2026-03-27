##### Code for experimental analysis in Section 4 and Appendices D-F. #####
#install.packages(c("readr","texreg")) # If packages not yet installed
# Load packages
library(readr)
library(texreg)

### Load South Africa survey data (alter file path as needed)
sadata <- read_csv("cohesion_experiment_replication_data.csv")

# Delete rows of labels and test data
sadata <- sadata[-c(1:21),]

### Clean variables ###
# Treatment indicator: 1 = cohesion treatment; 0 = diversity treatment
sadata$cohesion <- NA
sadata$cohesion[sadata$Block2_DO == "Q2C|Q2O"] <- 1
sadata$cohesion[sadata$Block2_DO == "Q2D|Q2O"] <- 0
sadata$cohesion[sadata$Block1_DO == "Q1C|Q1O"] <- 1
sadata$cohesion[sadata$Block1_DO == "Q1D|Q1O"] <- 0
sadata$cohesion[sadata$Block3_DO == "Q3C|Q3O"] <- 1
sadata$cohesion[sadata$Block3_DO == "Q3D|Q3O"] <- 0

# Issue depicted in the vignette
sadata$issue <- NA
sadata$issue[!is.na(sadata$Q2O)] <- 1 #education
sadata$issue[!is.na(sadata$Q1O)] <- 2 #jobs
sadata$issue[!is.na(sadata$Q3O)] <- 3 #environment

# Level of support for protesters; repeat for each issue
sadata$support <- NA
sadata$support[sadata$issue == 1 & sadata$Q2O == "I would not vote for any tax increase, because I do not support the protesters' demands."] <- 0
sadata$support[sadata$issue == 1 & sadata$Q2O == "I support the protesters' demands, but I would not vote for a tax increase."] <- 1
sadata$support[sadata$issue == 1 & sadata$Q2O == "I would vote to approve a tax increase of one percent."] <- 2
sadata$support[sadata$issue == 1 & sadata$Q2O == "I would vote to approve a tax increase of more than one percent [enter percent]."] <- 3

sadata$support[sadata$issue == 2 & sadata$Q1O == "I would not vote for any tax increase, because I do not support the protesters' demands."] <- 0
sadata$support[sadata$issue == 2 & sadata$Q1O == "I support the protesters' demands, but I would not vote for a tax increase."] <- 1
sadata$support[sadata$issue == 2 & sadata$Q1O == "I would vote to approve a tax increase of one percent."] <- 2
sadata$support[sadata$issue == 2 & sadata$Q1O == "I would vote to approve a tax increase of more than one percent [enter percent]."] <- 3

sadata$support[sadata$issue == 3 & sadata$Q3O == "I would not vote for any tax increase, because I do not support the protesters' demands."] <- 0
sadata$support[sadata$issue == 3 & sadata$Q3O == "I support the protesters' demands, but I would not vote for a tax increase."] <- 1
sadata$support[sadata$issue == 3 & sadata$Q3O == "I would vote to approve a tax increase of one percent."] <- 2
sadata$support[sadata$issue == 3 & sadata$Q3O == "I would vote to approve a tax increase of more than one percent [enter percent]."] <- 3

# Outcome variable: would or would not vote to grant protesters concession
sadata$voteyes <- NA
sadata$voteyes[sadata$support == 0] <- 0
sadata$voteyes[sadata$support == 1] <- 0
sadata$voteyes[sadata$support == 2] <- 1
sadata$voteyes[sadata$support == 3] <- 1

## Outcome variable: remembered demands or not
# First, which issue did the respondent recall from the vignette?
sadata$recall <- NA
sadata$recall[sadata$Q30 == "Education"] <- 1
sadata$recall[sadata$Q30 == "Unemployment"] <- 2
sadata$recall[sadata$Q30 == "Environmental protection"] <- 3
# Second, did that issue match the issue actually depicted? (i.e., Did respondent recall correctly?)
sadata$recallcorrect <- NA
sadata$recallcorrect[sadata$issue == sadata$recall] <- 1
sadata$recallcorrect[sadata$issue != sadata$recall] <- 0

## Demographics
# Respondent age
sadata$age <- NA
sadata$age <- 2021 - as.numeric(sadata$Q33_1)
# Respondent gender
sadata$male <- NA
sadata$male[sadata$Q34 == "Male"] <- 1
sadata$male[sadata$Q34 == "Female"] <- 0
sadata$male[sadata$Q34 == "Neither"] <- 0
# Respondent urban vs. rural residence 
sadata$urban <- NA
sadata$urban[sadata$Q36 == "Urban"] <- 1
sadata$urban[sadata$Q36 == "Rural"] <- 0
sadata$urban[sadata$Q36 == "Neither"] <- 0
# Respondent income group
sadata$income <- NA
sadata$income[sadata$Q38 == "1 (lowest income group)"] <- 1
sadata$income[sadata$Q38 == "2"] <- 2
sadata$income[sadata$Q38 == "3"] <- 3
sadata$income[sadata$Q38 == "4"] <- 4
sadata$income[sadata$Q38 == "5"] <- 5
sadata$income[sadata$Q38 == "6"] <- 6
sadata$income[sadata$Q38 == "7"] <- 7
sadata$income[sadata$Q38 == "8"] <- 8
sadata$income[sadata$Q38 == "9"] <- 9
sadata$income[sadata$Q38 == "10 (highest income group)"] <- 10
# Respondent race
sadata$race <- NA
sadata$race[sadata$Q39 == "Black"] <- 4
sadata$race[sadata$Q39 == "White"] <- 3
sadata$race[sadata$Q39 == "Coloured"] <- 2
sadata$race[sadata$Q39 == "Asian"] <- 1
sadata$race[sadata$Q39 == "Other"] <- 0
# Respondent education level
sadata$educ <- NA
sadata$educ[sadata$Q40 == "No formal schooling"] <- 0 
sadata$educ[sadata$Q40 == "Informal schooling only"] <- 1
sadata$educ[sadata$Q40 == "Some primary schooling"] <- 2
sadata$educ[sadata$Q40 == "Primary school completed"] <- 3
sadata$educ[sadata$Q40 == "Some secondary school"] <- 4
sadata$educ[sadata$Q40 == "Secondary school completed"] <- 5
sadata$educ[sadata$Q40 == "Some university"] <- 6
sadata$educ[sadata$Q40 == "University completed"] <- 7
sadata$educ[sadata$Q40 == "Post-graduate"] <- 8

## Test H1: cohesion treatment -> vote to fund protester demands
library(texreg)
mod1 <- glm(voteyes ~ cohesion, data = sadata, family = "binomial") # Without controls
mod2 <- glm(voteyes ~ cohesion + age + male + as.factor(race) + educ + income, data = sadata, family = "binomial") # With controls (first result in Fig. 4)
screenreg(list(mod1,mod2))
# For LaTeX code, replace "screenreg" with "texreg" in command above.
# Calculate 95% confidence interval using model 2, for Fig. 4 (see LaTeX template, "latex_plots.tex", for reproducing figure)
1.96*0.161645 # 0.379141 +- 0.3168242

## Test H2: cohesion treatment -> better recall of issue
mod3 <- glm(recallcorrect ~ cohesion, data = sadata, family = "binomial") # Without controls
mod4 <- glm(recallcorrect ~ cohesion + age + male + as.factor(race) + educ + income, data = sadata, family = "binomial") # With controls (second result in Fig. 4)
screenreg(list(mod3,mod4), custom.model.names = c("Model 3","Model4"))
# Calculate 95% confidence interval using model 4, for Fig. 4 (see LaTeX template, "latex_plots.tex", for reproducing figure)
1.96*0.146250 # 0.036880 +- 0.28665

### Predict probabilities in Fig. 5 (see LaTeX template, "latex_plots.tex", for reproducing figure)
# Set values to diverse message condition and modal demographics
newdata_diverse <- data.frame(cohesion = 0, age = 38.16, male = 1, race = 4, educ = 5, income = 5) #setting cohesion = 0
# Predicted probability of voting for tax increase under diverse condition
set.seed(123)
predict(mod2, newdata_diverse, type = "response", se.fit = TRUE)
# Predicted probability of recalling demands under diverse condition
set.seed(123)
predict(mod4, newdata_diverse, type = "response", se.fit = TRUE)

# Set values to cohesive message condition and modal demographics
newdata_cohesive <- data.frame(cohesion = 1, age = 38.16, male = 1, race = 4, educ = 5, income = 5) #setting cohesion = 1
# Predicted probability of recalling demands under cohesive condition
set.seed(123)
predict(mod2, newdata_cohesive, type = "response", se.fit = TRUE)
# Predicted probability of recalling demands under cohesive condition
set.seed(123)
predict(mod4, newdata_cohesive, type = "response", se.fit = TRUE)

##### Code for Analyses in the Supplemental Materials #####

# Sample Characteristics (Appendix D)
prop.table(table(sadata$male))
prop.table(table(sadata$race)) # 0=Other; 1=Asian; 2=Coloured; 3=White; 4=Black
prop.table(table(sadata$urban))
# Code and summarize age brackets
sadata$age15to25 <- NA
sadata$age15to25 <- 0
sadata$age15to25[sadata$age >= 15 & sadata$age <= 25] <- 1
prop.table(table(sadata$age15to25))
sadata$age26to35 <- NA
sadata$age26to35 <- 0
sadata$age26to35[sadata$age >= 26 & sadata$age <= 35] <- 1
prop.table(table(sadata$age26to35))
sadata$age36to45 <- NA
sadata$age36to45 <- 0
sadata$age36to45[sadata$age >= 36 & sadata$age <= 45] <- 1
prop.table(table(sadata$age36to45))
sadata$age46to55 <- NA
sadata$age46to55 <- 0
sadata$age46to55[sadata$age >= 46 & sadata$age <= 55] <- 1
prop.table(table(sadata$age46to55))
sadata$age56plus <- NA
sadata$age56plus <- 0
sadata$age56plus[sadata$age >= 56] <- 1
prop.table(table(sadata$age56plus))

# Balance Tests (Appendix E)
mod_balance <- glm(cohesion ~ + age + male + as.factor(race) + educ + income, data = sadata, family = "binomial")
screenreg(mod_balance,
       custom.coef.names = c("Intercept","Age","Male","Asian","Coloured","White","Black","Education","Income"),
)

# Tables showing robustness to controls (Appendix F)
screenreg(list(mod1,mod2,mod3,mod4),
       custom.coef.names = c("Intercept","Cohesive Treatment","Age","Male","Asian","Coloured","White","Black","Education","Income"),
       stars = c(.01,.05,.10)
)



