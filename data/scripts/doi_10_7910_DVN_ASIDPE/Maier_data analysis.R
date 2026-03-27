## -----------------------------------------------------------------------------
## R script for analyzing clean Mcheck survey data collected in May/June 2022 
## Survey experiment on new Mcheck label by the Swiss retailer Migros
## Created July 2022 by Maiken Maier
## Last modified August 2023 by Maiken Maier
## -----------------------------------------------------------------------------

######################################################
## Loading Packages
######################################################
library(readxl)
library(tidyverse)
library(writexl)
library(dplyr)
library(naniar)
library(psych)
library(jtools)
library(sandwich)
library(lmtest)
library(ggstance)
library(broom.mixed)
library(ggpubr)
library(effsize)
library(stats) #contains p.adjust formula

######################################################
## Loading Data
######################################################
data <- read_excel("mcheck_articleI_replication data.xlsx")

################################################################
## Descriptive statistics
################################################################
## Median survey completion time
summary(data$`Duration (in seconds)`)/60 #21.37 minutes

#Average duration of survey items until DVs
summary(data$duration_first_survey_part)/60 #7.54 minutes

## Amount of respondents in control and treatment group
table(data$Group)

## Demographics
summary(data$age)
table(data$agexgender)
table(data$canton)
table(data$education)
table(data$income)
table(data$Q_Language)

## Perception of labels
summary(data$label_opinion_index)
table(data$label_opinion_1)/2372
table(data$label_opinion_2)/2372
table(data$label_opinion_3)/2372
table(data$label_opinion_4)/2372
table(data$label_opinion_5)/2372
table(data$label_opinion_6)/2372
table(data$label_opinion_7)/2372 

## Label understanding
table(data$label_opinion_6)/2372

## Label confusion
table(data$label_opinion_7)/2372

## Prior awareness of MCheck label
table(data$label_awareness_11)/2372

## Shopping criteria
summary(data$`shopping_criteria _1`)
summary(data$`shopping_criteria _2`)
summary(data$`shopping_criteria _3`)
summary(data$`shopping_criteria _4`)
summary(data$`shopping_criteria _5`)
summary(data$`shopping_criteria _6`)
summary(data$`shopping_criteria _7`)
summary(data$`shopping_criteria _8`)
summary(data$`shopping_criteria _9`)
summary(data$`shopping_criteria _10`)
summary(data$`shopping_criteria _11`)
summary(data$`shopping_criteria _12`)
summary(data$`shopping_criteria _13`)
summary(data$`shopping_criteria _14`)
summary(data$`shopping_criteria _15`)

# Price
table(data$`shopping_criteria _3`)/2372

# Ease of preparation
table(data$`shopping_criteria _13`)/2372

# Popularity of the brand
table(data$`shopping_criteria _14`)/2372

# Habit
table(data$`shopping_criteria _15`)/2372

## Frequency of shopping at Migros
table(data$supermarket_choice_1)/2372

## Prior knowledge
table(data$prior_knowledge)/2372

##MCheck label awareness
table(data$label_awareness_11)/2372

## Dependent variables
summary(data$attitudes_1)
table(data$attitudes_1)/2372
sd(data$attitudes_1)
summary(data$control_climate_2)
table(data$control_climate_2)/2372
sd(data$control_climate_2)
summary(data$control_climate_1)
sd(data$control_climate_1)
summary(data$`social_norms_1.1 _1`)
sd(data$`social_norms_1.1 _1`)
summary(data$`social_norms_1.1 _2`)
sd(data$`social_norms_1.1 _2`)
summary(data$`social_norms_1.1 _3`)
sd(data$`social_norms_1.1 _3`)
summary(data$`social_norms_1.1 _4`)
sd(data$`social_norms_1.1 _4`)
summary(data$intention_climate)
sd(data$intention_climate)
summary(data$policy_support_1.1)
sd(data$policy_support_1.1)
summary(data$policy_support_2.4_1)
sd(data$policy_support_2.4_1)
summary(data$policy_support_2.4_2)
sd(data$policy_support_2.4_2)
summary(data$policy_support_2.4_3)
sd(data$policy_support_2.4_3)
summary(data$policy_support_2.4_4)
sd(data$policy_support_2.4_4)
summary(data$policy_support_2.4_5)
sd(data$policy_support_2.4_5)
summary(data$policy_support_2.4_6)
sd(data$policy_support_2.4_6)
summary(data$policy_support_2.4_7)
sd(data$policy_support_2.4_7)
summary(data$gov_regulation)
sd(data$gov_regulation)
summary(data$wtp)
sd(data$wtp)

#####################################################################################
## MLS simple regression models
#####################################################################################
## ---------------------------------------------------------------------
## DV1: Attitude
## ---------------------------------------------------------------------
## Attitude towards importance of co2 impact of foods
lm1 <- glm(attitudes_1 ~ Group + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income  + label_awareness_11, data = data)
summary(lm1)
lm1a <- glm(attitudes_1 ~ Group*label_awareness_11 + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income, data = data)
summary(lm1a)
#Test whether we need robust SEs
bptest(lm1) #test shows that if we can reject the null that the variance of the residuals is constant, there is heteroscedasticity
coeftest(lm1, vcov=sandwich)
coefci(lm1)
cohen.d(data$attitudes_1 ~ data$Group)

##Wald test to check model fit when adding variables
waldtest(lm1, lm1a, vcov=sandwich)

## ---------------------------------------------------------------------
## DV2a: Perceived behavioral control - perceived ease
## ---------------------------------------------------------------------
## Perceived ease of paying attention to co2 impact while food shopping
lm2 <- glm(control_climate_2 ~ Group + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income  + label_awareness_11, data = data)
coeftest(lm2, vcov. = sandwich)
coefci(lm2)
lm2a <- glm(control_climate_2 ~ Group*label_awareness_11 + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income, data = data)
coeftest(lm2a, vcov. = sandwich)
coefci(lm2a)
cohen.d(data$control_climate_2 ~ data$Group)

## ---------------------------------------------------------------------
## DV2b: Perceived behavioral control - perceived self efficacy
## ---------------------------------------------------------------------
## Perceived behavioral control over paying attention to co2 impact while food shopping
lm3 <- glm(control_climate_1 ~ Group + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income  + label_awareness_11, data = data)
coeftest(lm3, vcov. = sandwich)
coefci(lm3)
lm3a <- glm(control_climate_1 ~ Group*label_awareness_11 + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income, data = data)
coeftest(lm3a, vcov. = sandwich)
cohen.d(data$control_climate_1 ~ data$Group)

## ---------------------------------------------------------------------
## DV3: Perceived social norms
## ---------------------------------------------------------------------
## Perceived social norms regarding co2 impact of food products
# 1. Family
lm4 <- glm(`social_norms_1.1 _1` ~ Group + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income  + label_awareness_11, data = data)
coeftest(lm4, vcov. = sandwich)
lm4a <- glm(`social_norms_1.1 _1` ~ Group*label_awareness_11 + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income, data = data)
coeftest(lm4a, vcov. = sandwich)
# 2. Friends
lm5 <- glm(`social_norms_1.1 _2` ~ Group + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income  + label_awareness_11, data = data)
coeftest(lm5, vcov. = sandwich)
lm5a <- glm(`social_norms_1.1 _2` ~ Group*label_awareness_11 + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income, data = data)
coeftest(lm5a, vcov. = sandwich)
cohen.d(data$`social_norms_1.1 _2` ~ data$Group)
# 3. Coworkers
lm6 <- glm(`social_norms_1.1 _3` ~ Group + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income  + label_awareness_11, data = data)
coeftest(lm6, vcov. = sandwich)
coefci(lm6)
lm6a <- glm(`social_norms_1.1 _3` ~ Group*label_awareness_11 + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income, data = data)
coeftest(lm6a, vcov. = sandwich)
cohen.d(data$`social_norms_1.1 _3` ~ data$Group)
# 4. Majority of Swiss citizens
lm7 <- glm(`social_norms_1.1 _4` ~ Group + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income  + label_awareness_11, data = data)
coeftest(lm7, vcov. = sandwich)
lm7a <- glm(`social_norms_1.1 _4` ~ Group*label_awareness_11 + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income, data = data)
coeftest(lm7a, vcov. = sandwich)
cohen.d(data$`social_norms_1.1 _4` ~ data$Group)
#Index perceived social norms regarding co2 impact of foods
lm8 <- glm(social_norms_1.1_index ~ Group + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income  + label_awareness_11, data = data)
coeftest(lm8, vcov. = sandwich)
lm8a <- glm(social_norms_1.1_index ~ Group*label_awareness_11 + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income, data = data)
coeftest(lm8a, vcov. = sandwich)

## ---------------------------------------------------------------------
## DV4: Purchasing intentions
## ---------------------------------------------------------------------
## Intention to consider co2 impact of foods purchased
lm9 <- glm(intention_climate ~ Group + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income  + label_awareness_11, data = data)
coeftest(lm9, vcov. = sandwich)
coefci(lm9)
lm9a <- glm(intention_climate ~ Group*label_awareness_11 + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income, data = data)
coeftest(lm9a, vcov. = sandwich)
cohen.d(data$intention_climate ~ data$Group)

## ---------------------------------------------------------------------
## DV5a: General support of policy measures to reduce co2 emissions
## ---------------------------------------------------------------------
lm10 <- glm(policy_support_1.1 ~ Group + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income  + label_awareness_11, data = data)
coeftest(lm10, vcov. = sandwich)
lm10a <- glm(policy_support_1.1 ~ Group*label_awareness_11 + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income, data = data)
coeftest(lm10a, vcov. = sandwich)

## ---------------------------------------------------------------------
## DV5b: Demand for specific instruments to reduce co2 emissions
## ---------------------------------------------------------------------
## Mandatory co2 label
lm11 <- glm(policy_support_2.4_1 ~ Group + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income  + label_awareness_11, data = data)
coeftest(lm11, vcov. = sandwich)
coefci(lm11)
lm11a <- glm(policy_support_2.4_1 ~ Group*label_awareness_11 + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income, data = data)
coeftest(lm11a, vcov. = sandwich)
cohen.d(data$policy_support_2.4_1 ~ data$Group)
## Meat taxes
lm12 <- glm(policy_support_2.4_2 ~ Group + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income  + label_awareness_11, data = data)
coeftest(lm12, vcov. = sandwich)
lm12a <- glm(policy_support_2.4_2 ~ Group*label_awareness_11 + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income, data = data)
coeftest(lm12a, vcov. = sandwich)
## At least 50% meatfree meals in public cafeterias
lm13 <- glm(policy_support_2.4_3 ~ Group + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income  + label_awareness_11, data = data)
coeftest(lm13, vcov. = sandwich)
lm13a <- glm(policy_support_2.4_3 ~ Group*label_awareness_11 + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income, data = data)
coeftest(lm13a, vcov. = sandwich)
## Subsidies for plant-based foods
lm14 <- glm(policy_support_2.4_4 ~ Group + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income  + label_awareness_11, data = data)
coeftest(lm14, vcov. = sandwich)
lm14a <- glm(policy_support_2.4_4 ~ Group*label_awareness_11 + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income, data = data)
coeftest(lm14a, vcov. = sandwich)
## Support of meat substitute products 
lm15 <- glm(policy_support_2.4_5 ~ Group + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income  + label_awareness_11, data = data)
coeftest(lm15, vcov. = sandwich)
lm15a <- glm(policy_support_2.4_5 ~ Group*label_awareness_11 + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income  + label_awareness_11, data = data)
coeftest(lm15a, vcov. = sandwich)
## Information and education campaign promoting meat reduction
lm16 <- glm(policy_support_2.4_6 ~ Group + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income  + label_awareness_11, data = data)
coeftest(lm16, vcov. = sandwich)
lm16a <- glm(policy_support_2.4_6 ~ Group*label_awareness_11 + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income, data = data)
coeftest(lm16a, vcov. = sandwich)
## Reduction of subsidies for meat and animal feed producers
lm17 <- glm(policy_support_2.4_7 ~ Group + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income  + label_awareness_11, data = data)
coeftest(lm17, vcov. = sandwich)
lm17a <- glm(policy_support_2.4_7 ~ Group*label_awareness_11 + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income, data = data)
coeftest(lm17a, vcov. = sandwich)

## Index of instruments to reduce food related co2 emissions
lm18 <- glm(policy_support_2.4_index ~ Group + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income  + label_awareness_11, data = data)
coeftest(lm18, vcov. = sandwich)
lm18a <- glm(policy_support_2.4_index ~ Group*label_awareness_11 + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income, data = data)
coeftest(lm18a, vcov. = sandwich)

## ---------------------------------------------------------------------
## DV5c: Demand for more governmental regulation in food sector
## ---------------------------------------------------------------------
lm19 <- glm(gov_regulation ~ Group + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income  + label_awareness_11, data = data)
coeftest(lm19, vcov. = sandwich)
lm19a <- glm(gov_regulation ~ Group*label_awareness_11 + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income, data = data)
coeftest(lm19a, vcov. = sandwich)

## ---------------------------------------------------------------------
## DV5d: Support of increasing taxes on meat (willingness to pay for meat tax)
## ---------------------------------------------------------------------
lm20 <- glm(wtp ~ Group + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income  + label_awareness_11, data = data)
coeftest(lm20, vcov. = sandwich)
lm20a <- glm(wtp ~ Group*label_awareness_11 + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income, data = data)
coeftest(lm20a, vcov. = sandwich)

## ---------------------------------------------------------------------
## Adjust p-values for multiple comparisons
## ---------------------------------------------------------------------
#Adjust p-values for multiple comparisons among market acceptance DVs
p_values1 <- c(0.0000, 0.0000, 0.0008, 0.1255, 0.0551, 0.0146, 0.0948, 0.0144, 0.0061)
p.adjust(p_values1, method = "BH")

#Adjust p-values for multiple comparisons among socio-political acceptance DVs
p_values2 <- c(0.648, 0.0008, 0.29, 0.693, 0.5263, 0.1764, 0.9, 0.8315, 0.3766)
p.adjust(p_values2, method = "BH")

#####################################################################################
## Manipulation check
#####################################################################################
##Impact of food labels, such as Mcheck, on purchases
mc1 <- glm(intentions_mcheck ~ Group + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income  + label_awareness_11, data = data)
coeftest(mc1, vcov. = sandwich)
coefci(mc1)
mc1a <- glm(intentions_mcheck ~ Group*label_awareness_11 + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income, data = data)
coeftest(mc1a, vcov. = sandwich)
cohen.d(data$intentions_mcheck ~ data$Group)

## Perception of Migros
#Social responsibility
mc2 <- glm(`migros_perception _1` ~ Group + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income  + label_awareness_11, data = data)
coeftest(mc2, vcov. = sandwich)
#Environmental responsibility
mc3 <- glm(`migros_perception _2` ~ Group + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income  + label_awareness_11, data = data)
coeftest(mc3, vcov. = sandwich)
#Contribution to sustainable diet
mc4 <- glm(`migros_perception _4` ~ Group + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income  + label_awareness_11, data = data)
coeftest(mc4, vcov. = sandwich)
#Information of customers regarding impact of their consumption on people and the environment
mc5 <- glm(`migros_perception _5` ~ Group + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income  + label_awareness_11, data = data)
coeftest(mc5, vcov. = sandwich)
coefci(mc5)
mc5a <- glm(`migros_perception _5` ~ Group*label_awareness_11 + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income, data = data)
coeftest(mc5a, vcov. = sandwich)
cohen.d(data$`migros_perception _5` ~ data$Group)

## Effectiveness of private labeling initiatives
#Effectiveness regarding more sustainable food purchasing
mc6 <- glm(initiative_effective_1 ~ Group + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income  + label_awareness_11, data = data)
coeftest(mc6, vcov. = sandwich)
coefci(mc6)
mc6a <- glm(initiative_effective_1 ~ Group*label_awareness_11 + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income, data = data)
coeftest(mc6a, vcov. = sandwich)
cohen.d(data$initiative_effective_1 ~ data$Group)

## Credibility of private labeling initiatives regarding independent rating of products
mc7 <- glm(initiative_credible ~ Group + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income  + label_awareness_11, data = data)
coeftest(mc7, vcov. = sandwich)
coefci(mc7)
mc7a <- glm(initiative_credible ~ Group*label_awareness_11 + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income, data = data)
coeftest(mc7a, vcov. = sandwich)
cohen.d(data$initiative_credible ~ data$Group)

## Post-survey Mcheck awareness
mc8 <- glm(mcheck_awareness_1 ~ Group + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income  + label_awareness_11, data = data)
coeftest(mc8, vcov. = sandwich)
coefci(mc8)
mc8a <- glm(mcheck_awareness_1 ~ Group*label_awareness_11 + age + gender + education + diet_meat + ssc + tsc + `shopping_criteria _3` + `shopping_criteria _13` + `shopping_criteria _14` + `shopping_criteria _15` + label_opinion_index + label_opinion_6 + label_opinion_7 + prior_knowledge + supermarket_choice_1 + left_right + adults + children + urban_rural + income, data = data)
coeftest(mc8a, vcov. = sandwich)
cohen.d(data$mcheck_awareness_1 ~ data$Group)

################################################################
## Main treatment effects
################################################################
library(stargazer)

## ---------------------------------------------------------------------
## Regression tables main effects
## ---------------------------------------------------------------------
## Regression tables market acceptance
lm1$rse <-sqrt(diag(vcovHC(lm1, type="HC2")))
lm2$rse <-sqrt(diag(vcovHC(lm2, type="HC2")))
lm3$rse <-sqrt(diag(vcovHC(lm3, type="HC2")))
lm4$rse <-sqrt(diag(vcovHC(lm4, type="HC2")))
lm5$rse <-sqrt(diag(vcovHC(lm5, type="HC2")))
lm6$rse <-sqrt(diag(vcovHC(lm6, type="HC2")))
lm7$rse <-sqrt(diag(vcovHC(lm7, type="HC2")))
lm9$rse <-sqrt(diag(vcovHC(lm9, type="HC2")))
stargazer(lm1,lm2, lm3, lm4, lm5, lm6, lm7,lm9,
          title = "Market acceptance (DVs 1 - 4)", 
          dep.var.labels = c("Attitude", "Perceived ease", "Perceived self-efficacy", "Social norms - Family", "Social norms - Friends", "Social norms - Coworkers", "Social Norms - Swiss population", "Behavioral intentions"), 
          covariate.labels = c( "Information treatment", "age", "gender", "education", "diet", "ssc index", "tsc index","price", "ease of preparation", "popularity of the brand", "habit", "label opinion index", "label understanding", "label confusion", "prior knowledge", "Migros shopping frequency", "left/right placement", "adults", "children", "urban/rural", "income", "prior Mcheck awareness"),
          se=list(lm1$rse, lm2$rse, lm3$rse, lm4$rse, lm5$rse, lm6$rse, lm7$rse, lm9$rse),  
          align= TRUE, header = FALSE, type = "html", no.space = TRUE, out = "RT_market acceptance.html" )

## Regression tables socio-political acceptance
#Regression table general policy support, more gov regulation, wtp for meat tax
lm10$rse <-sqrt(diag(vcovHC(lm10, type="HC2")))
lm19$rse <-sqrt(diag(vcovHC(lm19, type="HC2")))
lm20$rse <-sqrt(diag(vcovHC(lm20, type="HC2")))
stargazer(lm10, lm19, lm20,
          title = "Socio-political acceptance (DV5a, 5c and 5d)", 
          dep.var.labels = c("General support for policies to reduce food-related CO2 emissions", "General support for more gov. regulation in the food sector", "Willingness to pay for meat tax"), 
          covariate.labels = c( "Information treatment", "age", "gender", "education", "diet", "ssc index", "tsc index","price", "ease of preparation", "popularity of the brand", "habit", "label opinion index", "label understanding", "label confusion", "prior knowledge", "Migros shopping frequency", "left/right placement", "adults", "children", "urban/rural", "income", "prior Mcheck awareness"),
          se=list(lm10$rse, lm19$rse, lm20$rse),  
          align= TRUE, header = FALSE, type = "html", no.space = TRUE, out = "RT_general policy support.html" )

#Regression table policy instrument support effects
lm11$rse <-sqrt(diag(vcovHC(lm11, type="HC2")))
lm12$rse <-sqrt(diag(vcovHC(lm12, type="HC2")))
lm13$rse <-sqrt(diag(vcovHC(lm13, type="HC2")))
lm14$rse <-sqrt(diag(vcovHC(lm14, type="HC2")))
lm15$rse <-sqrt(diag(vcovHC(lm15, type="HC2")))
lm16$rse <-sqrt(diag(vcovHC(lm16, type="HC2")))
lm17$rse <-sqrt(diag(vcovHC(lm17, type="HC2")))
stargazer(lm11,lm12, lm13, lm14, lm15, lm16, lm17,
          title = "Socio-political acceptance (DV5b - 1-7)", 
          dep.var.labels = c("Mandatory CO2 label", "Meat tax", "Min. 50% meat free cafeteria dishes", "Subsidies for plant-based foods", "Support for meat substitutes", "Information/education campaign", "Reduce subsidies for meat producers"), 
          covariate.labels = c( "Information treatment", "age", "gender", "education", "diet", "ssc index", "tsc index","price", "ease of preparation", "popularity of the brand", "habit", "label opinion index", "label understanding", "label confusion", "prior knowledge", "Migros shopping frequency", "left/right placement", "adults", "children", "urban/rural", "income", "prior Mcheck awareness"),
          se=list(lm11$rse, lm12$rse, lm13$rse, lm14$rse, lm15$rse, lm16$rse, lm17$rse),  
          align= TRUE, header = FALSE, type = "html", no.space = TRUE, out = "RT_specific policy support.html" )

## ---------------------------------------------------------------------
## Regression tables interaction effects
## ---------------------------------------------------------------------
## Regression tables market acceptance
lm1a$rse <-sqrt(diag(vcovHC(lm1a, type="HC2")))
lm2a$rse <-sqrt(diag(vcovHC(lm2a, type="HC2")))
lm3a$rse <-sqrt(diag(vcovHC(lm3a, type="HC2")))
lm4a$rse <-sqrt(diag(vcovHC(lm4a, type="HC2")))
lm5a$rse <-sqrt(diag(vcovHC(lm5a, type="HC2")))
lm6a$rse <-sqrt(diag(vcovHC(lm6a, type="HC2")))
lm7a$rse <-sqrt(diag(vcovHC(lm7a, type="HC2")))
lm9a$rse <-sqrt(diag(vcovHC(lm9a, type="HC2")))
stargazer(lm1a,lm2a,lm3a,lm4a,lm5a,lm6a,lm7a,lm9a,
          title = "Market acceptance (DVs 1 - 4)", 
          dep.var.labels = c("Attitude", "Perceived ease", "Perceived self-efficacy", "Social norms - Family", "Social norms - Friends", "Social norms - Coworkers", "Social Norms - Swiss population", "Behavioral intentions"), 
          covariate.labels = c( "Information treatment", "prior Mcheck awareness", "age", "gender", "education", "diet", "ssc index", "tsc index","price", "ease of preparation", "popularity of the brand", "habit", "label opinion index", "label understanding", "label confusion", "prior knowledge", "Migros shopping frequency", "left/right placement", "adults", "children", "urban/rural", "income", "Information treatment * prior Mcheck awareness"),
          se=list(lm1a$rse, lm2a$rse, lm3a$rse, lm4a$rse, lm5a$rse, lm6a$rse, lm7a$rse, lm9a$rse),  
          align= TRUE, header = FALSE, type = "html", no.space = TRUE, out = "RT_market acceptance_interaction.html" )

## Regression tables socio-political acceptance
#Regression table general policy support, more gov regulation, wtp for meat tax
lm10a$rse <-sqrt(diag(vcovHC(lm10a, type="HC2")))
lm19a$rse <-sqrt(diag(vcovHC(lm19a, type="HC2")))
lm20a$rse <-sqrt(diag(vcovHC(lm20a, type="HC2")))
stargazer(lm10a, lm19a, lm20a,
          title = "Socio-political acceptance (DV5a, 5c and 5d)", 
          dep.var.labels = c("General support for policies to reduce food-related CO2 emissions", "General support for more gov. regulation in the food sector", "Willingness to pay for meat tax"), 
          covariate.labels = c( "Information treatment", "prior Mcheck awareness", "age", "gender", "education", "diet", "ssc index", "tsc index","price", "ease of preparation", "popularity of the brand", "habit", "label opinion index", "label understanding", "label confusion", "prior knowledge", "Migros shopping frequency", "left/right placement", "adults", "children", "urban/rural", "income", "Information treatment * prior Mcheck awareness"),
          se=list(lm10a$rse, lm19a$rse, lm20a$rse),  
          align= TRUE, header = FALSE, type = "html", no.space = TRUE, out = "RT_general policy support_interaction.html" )

#Regression table policy instrument support effects
lm11a$rse <-sqrt(diag(vcovHC(lm11a, type="HC2")))
lm12a$rse <-sqrt(diag(vcovHC(lm12a, type="HC2")))
lm13a$rse <-sqrt(diag(vcovHC(lm13a, type="HC2")))
lm14a$rse <-sqrt(diag(vcovHC(lm14a, type="HC2")))
lm15a$rse <-sqrt(diag(vcovHC(lm15a, type="HC2")))
lm16a$rse <-sqrt(diag(vcovHC(lm16a, type="HC2")))
lm17a$rse <-sqrt(diag(vcovHC(lm17a, type="HC2")))
stargazer(lm11a,lm12a, lm13a, lm14a, lm15a, lm16a, lm17a,
          title = "Socio-political acceptance (DV5b - 1-7)", 
          dep.var.labels = c("Mandatory CO2 label", "Meat tax", "Min. 50% meat free cafeteria dishes", "Subsidies for plant-based foods", "Support for meat substitutes", "Information/education campaign", "Reduce subsidies for meat producers"), 
          covariate.labels = c( "Information treatment", "prior Mcheck awareness", "age", "gender", "education", "diet", "ssc index", "tsc index","price", "ease of preparation", "popularity of the brand", "habit", "label opinion index", "label understanding", "label confusion", "prior knowledge", "Migros shopping frequency", "left/right placement", "adults", "children", "urban/rural", "income", "Information treatment * prior Mcheck awareness"),
          se=list(lm11a$rse, lm12a$rse, lm13a$rse, lm14a$rse, lm15a$rse, lm16a$rse, lm17a$rse),  
          align= TRUE, header = FALSE, type = "html", no.space = TRUE, out = "RT_specific policy support_interaction.html" )

################################################################
## Manipulation check results
################################################################
#Regression table mcheck intention, awareness, credibility and effectiveness
mc1$rse <-sqrt(diag(vcovHC(mc1, type="HC2")))
mc6$rse <-sqrt(diag(vcovHC(mc6, type="HC2")))
mc7$rse <-sqrt(diag(vcovHC(mc7, type="HC2")))
mc8$rse <-sqrt(diag(vcovHC(mc8, type="HC2")))
stargazer(mc1, mc6, mc7, mc8,
          title = "Manipulation check", 
          dep.var.labels = c("Intention to consider Mcheck label when shopping", "Perceived effectiveness of voluntary labeling initiatives", "Perceived credibility of voluntary labeling initiatives", "Awareness of Mcheck label"), 
          covariate.labels = c( "Information treatment", "age", "gender", "education", "diet", "ssc index", "tsc index","price", "ease of preparation", "popularity of the brand", "habit", "label opinion index", "label understanding", "label confusion", "prior knowledge", "Migros shopping frequency", "left/right placement", "adults", "children", "urban/rural", "income", "prior Mcheck awareness"),
          se=list(mc1$rse, mc6$rse, mc7$rse, mc8$rse),  
          align= TRUE, header = FALSE, type = "html", no.space = TRUE, out = "RT_manipulation check_1.html" )
#Regression table Migros perception
mc2$rse <-sqrt(diag(vcovHC(mc2, type="HC2")))
mc3$rse <-sqrt(diag(vcovHC(mc3, type="HC2")))
mc4$rse <-sqrt(diag(vcovHC(mc4, type="HC2")))
mc5$rse <-sqrt(diag(vcovHC(mc5, type="HC2")))
stargazer(mc2, mc3, mc4, mc5,
          title = "Manipulation check - Perception of Migros", 
          dep.var.labels = c("Socially responsible", "Environmentally responsible", "Contributes to sustainable diets", "Informs customers on the impact of their consumption on people and the environment"), 
          covariate.labels = c( "Information treatment", "age", "gender", "education", "diet", "ssc index", "tsc index","price", "ease of preparation", "popularity of the brand", "habit", "label opinion index", "label understanding", "label confusion", "prior knowledge", "Migros shopping frequency", "left/right placement", "adults", "children", "urban/rural", "income", "prior Mcheck awareness"),
          se=list(mc1$rse, mc6$rse, mc7$rse, mc8$rse),  
          align= TRUE, header = FALSE, type = "html", no.space = TRUE, out = "RT_manipulation check_2.html" )

################################################################
## Randomization Tests
################################################################
## ---------------------------------------------------------------------
## Balance checks overall dataset
## ---------------------------------------------------------------------
bc_gender <- lm(gender ~ Group, data = data)
summary(bc_gender)

bc_age <- lm(age ~ Group, data = data)
summary(bc_age)

bc_education <- lm(education ~ Group, data = data)
summary(bc_education)

bc_citizenship <- lm(citizenship ~ Group, data = data)
summary(bc_citizenship)

bc_canton <- lm(canton ~ Group, data = data)
summary(bc_canton)

bc_income <- lm(income ~ Group, data = data)
summary(bc_income)

bc_parties <- lm(parties ~ Group, data = data)
summary(bc_parties)

bc_gov_intervention <- lm(gov_intervention ~ Group, data = data)
summary(bc_gov_intervention)

bc_economic_regulation <- lm(economic_regulation ~ Group, data = data)
summary(bc_economic_regulation)

bc_left_right <- lm(left_right ~ Group, data = data)
summary(bc_left_right)

bc_diet <- lm(diet_meat ~ Group, data = data)
summary(bc_diet)

bc_substitute_consumption <- lm(substitute_consum ~ Group, data = data)
summary(bc_substitute_consumption)

bc_urban_rural <- lm(urban_rural ~ Group, data = data)
summary(bc_urban_rural)

#sustainable shopping criteria index
bc_ssc <- lm(ssc ~ Group, data = data)
summary(bc_ssc)

#taste and safety shopping criteria index
bc_tsc <- lm(tsc ~ Group, data = data)
summary(bc_tsc)

bc_price <- lm(`shopping_criteria _3` ~ Group, data = data)
summary(bc_price)

bc_preparation_ease <- lm(`shopping_criteria _13` ~ Group, data = data)
summary(bc_preparation_ease)

bc_brand_popularity <- lm(`shopping_criteria _14` ~ Group, data = data)
summary(bc_brand_popularity)

bc_habit <- lm(`shopping_criteria _15` ~ Group, data = data)
summary(bc_habit)

bc_label_opinion_index <- lm(label_opinion_index ~ Group, data = data)
summary(bc_label_opinion_index)

bc_label_understanding <- lm(label_opinion_6 ~ Group, data = data)
summary(bc_label_understanding)

bc_label_information <- lm(label_opinion_7 ~ Group, data = data)
summary(bc_label_information)

bc_shopping_list <- lm(label_opinion_8 ~ Group, data = data)
summary(bc_shopping_list)

bc_spontaneous_shopping <- lm(label_opinion_9 ~ Group, data = data)
summary(bc_spontaneous_shopping)

bc_food_shopping <- lm(food_shopping ~ Group, data = data)
summary(bc_food_shopping)

bc_eating_out <- lm(eating_out ~ Group, data = data)
summary(bc_eating_out)

bc_supermarket_choice_migros <- lm(supermarket_choice_1 ~ Group, data = data)
summary(bc_supermarket_choice_migros)

bc_mcheck_awareness <- lm(label_awareness_11 ~ Group, data = data)
summary(bc_mcheck_awareness)

bc_prior_knowledge <- lm(prior_knowledge ~ Group, data = data)
summary(bc_prior_knowledge)

################################################################
## Balance check results
################################################################
## Regression tables balance checks
#Demographics
bc_age$rse <-sqrt(diag(vcovHC(bc_age, type="HC2")))
bc_gender$rse <-sqrt(diag(vcovHC(bc_gender, type="HC2")))
bc_education$rse <-sqrt(diag(vcovHC(bc_education, type="HC2")))
bc_canton$rse <-sqrt(diag(vcovHC(bc_canton, type="HC2")))
bc_citizenship$rse <-sqrt(diag(vcovHC(bc_citizenship, type="HC2")))
bc_income$rse <-sqrt(diag(vcovHC(bc_income, type="HC2")))
bc_urban_rural$rse <-sqrt(diag(vcovHC(bc_urban_rural, type="HC2")))
stargazer(bc_age, bc_gender, bc_education, bc_canton, bc_citizenship, bc_income,
          title = "Balance check - demographic variables", 
          dep.var.labels = c("age", "gender", "education", "canton of residency", "Swiss residency", "income", "urban/rural"), 
          covariate.labels = c( "Information treatment"),
          se=list(bc_age$rse, bc_gender$rse, bc_education$rse, bc_canton$rse, bc_citizenship$rse, bc_income$rse, bc_urban_rural$rse),  
          align= TRUE, header = FALSE, type = "html", no.space = TRUE, out = "RT_balance checks - demographics.html" )

#Political variables
bc_left_right$rse <-sqrt(diag(vcovHC(bc_age, type="HC2")))
bc_parties$rse <-sqrt(diag(vcovHC(bc_gender, type="HC2")))
bc_gov_intervention$rse <-sqrt(diag(vcovHC(bc_education, type="HC2")))
stargazer(bc_left_right, bc_parties, bc_gov_intervention,
          title = "Balance check - political variables", 
          dep.var.labels = c("Left/right political self placement", "Political party support", "General preference for governmental intervention"), 
          covariate.labels = c( "Information treatment"),
          se=list(bc_left_right$rse, bc_parties$rse, bc_gov_intervention$rse),  
          align= TRUE, header = FALSE, type = "html", no.space = TRUE, out = "RT_balance checks - political variables.html" )

