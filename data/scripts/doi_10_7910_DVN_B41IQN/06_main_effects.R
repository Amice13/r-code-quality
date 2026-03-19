### ------------------------------------------------------------
# Thesis Survey Experiment
# 05 Main Effects
# Friedrich Püttmann
# October 2023
### ------------------------------------------------------------

## install packages -----------------------------------------------

getOption("max.print")
options(max.print = 200)
install.packages("car") 

## load packages -----------------------------------------------

library(tidyverse)
library(pollster)
library(MASS)
library(magrittr)
library(car)
library(dplyr)

## load dataset ------------------------------------------------

data_exp <- read_rds("02_data/03_data_clean.rds")

data_treat <- subset(data_exp, treat == "yes")
data_control <- subset(data_exp, treat == "no")

data_turks <- subset(data_exp, eth_self == "Turkish")
data_kurds <- subset(data_exp, eth_self == "Kurdish")
data_arabs <- subset(data_exp, eth_self == "Arab")

data_relcon <- subset(data_exp, lifestyle == "religious conservative")

view(data_exp)
view(data_treat)
view(data_control)

## inspect important control variables -------------------------

# gender
table(data_exp$gender)
# age
table(data_exp$age)
# education
table(data_exp$edu)
# education by group
table(data_exp$edu_group)
# job 
table(data_exp$job)
# job 
table(data_exp$job_group)
# party choice today by group
table(data_exp$vote_today_group)
# Syrian presence
table(data_exp$syr_presence)
# Syrian encounter
table(data_exp$syr_encounter)
# Syrian partner
table(data_exp$Soru1301)
# Syrian relative
table(data_exp$Soru1302)
# Absence of any Syrian acquaintance
table(data_exp$Soru1308)
# Absence of any loss after earthquake
table(data_exp$Soru805)
# Religion/Sect
table(data_exp$sect)


## create tables with weights ----------------------------------

# GENERAL

# gender

table(data_exp$gender)
topline(df = data_exp, variable = gender, weight = weight_siyasi)
crosstab(df = data_exp, x = lifestyle, y = gender, weight = weight_siyasi)

# religion 

table(data_exp$sect)
topline(df = data_exp, variable = sect, weight = weight_siyasi)
crosstab(df = data_exp, x = lifestyle, y = sect, weight = weight_siyasi)
crosstab(df = data_exp, x = eth_self, y = sect, weight = weight_siyasi)
crosstab(df = data_exp, x = sect, y = eth_self, weight = weight_siyasi)

# preference in refugee - general

  # complete data
table(data_exp$prefref_gen)
topline(df = data_exp, variable = prefref_gen, weight = weight_siyasi)
crosstab(df = data_exp, x = lifestyle, y = prefref_gen, weight = weight_siyasi)
crosstab(df = data_exp, x = vote_may_group, y = prefref_gen, weight = weight_siyasi)

# preference in refugee - educated and employed

  # full sample
table(data_exp$prefref_eduemp)
topline(df = data_exp, variable = prefref_eduemp, weight = weight_siyasi)
crosstab(df = data_exp, x = lifestyle, y = prefref_eduemp, weight = weight_siyasi)
crosstab(df = data_exp, x = vote_may_group, y = prefref_eduemp, weight = weight_siyasi)


# willingness to accept long-term stay with EU help

  # full sample
  # table absolute numbers
table(data_exp$longterm_stay)
  # table percentages
original_table <- table(data_exp$longterm_stay)
total_count <- sum(table(data_exp$longterm_stay))
percentages <- (original_table / total_count) * 100
print(percentages)
  # table percentages and weights
topline(df = data_exp, variable = longterm_stay, weight = weight_siyasi)
  # cross tabulations
crosstab(df = data_exp, x = lifestyle, y = longterm_stay, weight = weight_siyasi)
crosstab(df = data_exp, x = vote_may_group, y = longterm_stay, weight = weight_siyasi)

# refugee policy is an important topic

  # full sample
table(data_exp$refpol_imp)
topline(df = data_exp, variable = refpol_imp, weight = weight_siyasi)
crosstab(df = data_exp, x = lifestyle, y = refpol_imp, weight = weight_siyasi)
crosstab(df = data_exp, x = vote_may_group, y = refpol_imp, weight = weight_siyasi)

# refugee policy is more important than the issue of inflation

  # full sample
table(data_exp$refpol_moreimp)
topline(df = data_exp, variable = refpol_moreimp, weight = weight_siyasi)
crosstab(df = data_exp, x = lifestyle, y = refpol_moreimp, weight = weight_siyasi)
crosstab(df = data_exp, x = vote_may_group, y = refpol_moreimp, weight = weight_siyasi)

# description of Syrian refugees
table(data_exp$syr_descrip)
topline(df = data_exp, variable = syr_descrip, weight = weight_siyasi)
crosstab(df = data_exp, x = lifestyle, y = syr_descrip, weight = weight_siyasi)
crosstab(df = data_exp, x = eth_self, y = syr_descrip, weight = weight_siyasi)
crosstab(df = data_exp, x = vote_may_group, y = syr_descrip, weight = weight_siyasi)



# SOCIAL BOUNDARIES

# talking to Syrians

  # full sample
table(data_exp$syr_talking)
topline(df = data_exp, variable = syr_talking, weight = weight_siyasi)
crosstab(df = data_exp, x = lifestyle, y = syr_talking, weight = weight_siyasi)
crosstab(df = data_exp, x = eth_self, y = syr_talking, weight = weight_siyasi)

  # treatment group only
table(data_treat$syr_talking)
topline(df = data_treat, variable = syr_talking, weight = weight_siyasi)
crosstab(df = data_treat, x = lifestyle, y = syr_talking, weight = weight_siyasi)
crosstab(df = data_treat, x = eth_self, y = syr_talking, weight = weight_siyasi)

  # control group only
table(data_control$syr_talking)
topline(df = data_control, variable = syr_talking, weight = weight_siyasi)
crosstab(df = data_control, x = lifestyle, y = syr_talking, weight = weight_siyasi)
crosstab(df = data_control, x = eth_self, y = syr_talking, weight = weight_siyasi)


# Syrian friend

  # complete data
table(data_exp$syr_friend)
topline(df = data_exp, variable = syr_friend, weight = weight_siyasi)
crosstab(df = data_exp, x = lifestyle, y = syr_friend, weight = weight_siyasi)
crosstab(df = data_exp, x = eth_self, y = syr_friend, weight = weight_siyasi)
crosstab(df = data_exp, x = rel_prac, y = syr_friend, weight = weight_siyasi)

  # treatment group only
table(data_treat$syr_talking)
topline(df = data_treat, variable = syr_friend, weight = weight_siyasi)
crosstab(df = data_treat, x = lifestyle, y = syr_friend, weight = weight_siyasi)
crosstab(df = data_treat, x = eth_self, y = syr_friend, weight = weight_siyasi)

  # control group only
table(data_control$syr_talking)
topline(df = data_control, variable = syr_friend, weight = weight_siyasi)
crosstab(df = data_control, x = lifestyle, y = syr_friend, weight = weight_siyasi)
crosstab(df = data_control, x = eth_self, y = syr_friend, weight = weight_siyasi)

# socialising with Syrians

  # complete data
table(data_exp$syr_socialising)
topline(df = data_exp, variable = syr_socialising, weight = weight_siyasi)
crosstab(df = data_exp, x = lifestyle, y = syr_socialising, weight = weight_siyasi)
crosstab(df = data_exp, x = rel_prac, y = syr_socialising, weight = weight_siyasi)
crosstab(df = data_exp, x = eth_self, y = syr_socialising, weight = weight_siyasi)

data_exp_norelative <- data_exp %>% filter(Soru1302 == "no")
crosstab(df = data_exp_norelative, x = eth_self, y = syr_socialising, weight = weight_siyasi)

  # treatment group only
table(data_treat$syr_talking)
topline(df = data_treat, variable = syr_socialising, weight = weight_siyasi)
crosstab(df = data_treat, x = lifestyle, y = syr_socialising, weight = weight_siyasi)
crosstab(df = data_treat, x = eth_self, y = syr_socialising, weight = weight_siyasi)

  # control group only
table(data_control$syr_talking)
topline(df = data_control, variable = syr_socialising, weight = weight_siyasi)
crosstab(df = data_control, x = lifestyle, y = syr_socialising, weight = weight_siyasi)
crosstab(df = data_control, x = eth_self, y = syr_socialising, weight = weight_siyasi)

# staying away from Syrians

  # complete data
table(data_exp$try_stayaway)
topline(df = data_exp, variable = try_stayaway, weight = weight_siyasi)
crosstab(df = data_exp, x = lifestyle, y = try_stayaway, weight = weight_siyasi)
crosstab(df = data_exp, x = eth_self, y = try_stayaway, weight = weight_siyasi)

  # treatment group only
table(data_treat$try_stayaway)
topline(df = data_treat, variable = try_stayaway, weight = weight_siyasi)
crosstab(df = data_treat, x = lifestyle, y = try_stayaway, weight = weight_siyasi)
crosstab(df = data_treat, x = eth_self, y = try_stayaway, weight = weight_siyasi)

  # control group only
table(data_control$try_stayaway)
topline(df = data_control, variable = try_stayaway, weight = weight_siyasi)
crosstab(df = data_control, x = lifestyle, y = try_stayaway, weight = weight_siyasi)
crosstab(df = data_control, x = eth_self, y = try_stayaway, weight = weight_siyasi)

# Soru901: Acceptance in the same country 

    # complete data
table(data_exp$Soru901)
topline(df = data_exp, variable = Soru901, weight = weight_siyasi)
crosstab(df = data_exp, x = lifestyle, y = Soru901, weight = weight_siyasi)
crosstab(df = data_exp, x = eth_self, y = Soru901, weight = weight_siyasi)

# Soru902: Acceptance in the same city

    # complete data
table(data_exp$Soru902)
topline(df = data_exp, variable = Soru902, weight = weight_siyasi)
crosstab(df = data_exp, x = lifestyle, y = Soru902, weight = weight_siyasi)
crosstab(df = data_exp, x = eth_self, y = Soru902, weight = weight_siyasi)

# Soru903: Acceptance in the same neighbourhood, workplace or school

    # complete data
table(data_exp$Soru903)
topline(df = data_exp, variable = Soru903, weight = weight_siyasi)
crosstab(df = data_exp, x = lifestyle, y = Soru903, weight = weight_siyasi)
crosstab(df = data_exp, x = eth_self, y = Soru903, weight = weight_siyasi)

# Soru904: Acceptance in the same apartment building, as a neighbour or in one's friends group

    # complete data
table(data_exp$Soru904)
topline(df = data_exp, variable = Soru904, weight = weight_siyasi)
crosstab(df = data_exp, x = lifestyle, y = Soru904, weight = weight_siyasi)
crosstab(df = data_exp, x = eth_self, y = Soru904, weight = weight_siyasi)

# Soru905: Acceptance in the same house or family

    # complete data
table(data_exp$Soru905)
topline(df = data_exp, variable = Soru905, weight = weight_siyasi)
crosstab(df = data_exp, x = lifestyle, y = Soru905, weight = weight_siyasi)
crosstab(df = data_exp, x = eth_self, y = Soru905, weight = weight_siyasi)



# SYMBOLIC BOUNDARIES

# religious brotherhood

  # complete data
table(data_exp$rel_bro)
topline(df = data_exp, variable = rel_bro, weight = weight_siyasi)
crosstab(df = data_exp, x = lifestyle, y = rel_bro, weight = weight_siyasi)
crosstab(df = data_exp, x = rel_prac, y = rel_bro, weight = weight_siyasi)
crosstab(df = data_exp, x = eth_self, y = rel_bro, weight = weight_siyasi)

  # treatment group only
crosstab(df = data_treat, x = lifestyle, y = rel_bro, weight = weight_siyasi)
crosstab(df = data_treat, x = rel_prac, y = rel_bro, weight = weight_siyasi)
crosstab(df = data_treat, x = eth_self, y = rel_bro, weight = weight_siyasi)

  # control group only
crosstab(df = data_control, x = lifestyle, y = rel_bro, weight = weight_siyasi)
crosstab(df = data_control, x = rel_prac, y = rel_bro, weight = weight_siyasi)
crosstab(df = data_control, x = eth_self, y = rel_bro, weight = weight_siyasi)


# ethnic brotherhood

  # complete data
table(data_exp$eth_bro)
topline(df = data_exp, variable = eth_bro, weight = weight_siyasi)
crosstab(df = data_exp, x = eth_self, y = eth_bro, weight = weight_siyasi)

  # treatment group only
crosstab(df = data_treat, x = lifestyle, y = eth_bro, weight = weight_siyasi)
crosstab(df = data_treat, x = rel_prac, y = eth_bro, weight = weight_siyasi)
crosstab(df = data_treat, x = eth_self, y = eth_bro, weight = weight_siyasi)

  # control group only
crosstab(df = data_control, x = lifestyle, y = eth_bro, weight = weight_siyasi)
crosstab(df = data_control, x = rel_prac, y = eth_bro, weight = weight_siyasi)
crosstab(df = data_control, x = eth_self, y = eth_bro, weight = weight_siyasi)

# part of society - today

  # complete data
table(data_exp$partsoc_today)
topline(df = data_exp, variable = partsoc_today, weight = weight_siyasi)
crosstab(df = data_exp, x = lifestyle, y = partsoc_today, weight = weight_siyasi)
crosstab(df = data_exp, x = eth_self, y = partsoc_today, weight = weight_siyasi)

  # treatment group only
crosstab(df = data_treat, x = lifestyle, y = partsoc_today, weight = weight_siyasi)
crosstab(df = data_treat, x = rel_prac, y = partsoc_today, weight = weight_siyasi)
crosstab(df = data_treat, x = eth_self, y = partsoc_today, weight = weight_siyasi)

  # control group only
crosstab(df = data_control, x = lifestyle, y = partsoc_today, weight = weight_siyasi)
crosstab(df = data_control, x = rel_prac, y = partsoc_today, weight = weight_siyasi)
crosstab(df = data_control, x = eth_self, y = partsoc_today, weight = weight_siyasi)

# part of society - tomorrow

  # complete data
table(data_exp$partsoc_tomo)
topline(df = data_exp, variable = partsoc_tomp, weight = weight_siyasi)
crosstab(df = data_exp, x = lifestyle, y = partsoc_tomo, weight = weight_siyasi)

crosstab(df = data_exp, x = eth_self, y = partsoc_tomo, weight = weight_siyasi)

# part of society - should

  # complete data
table(data_exp$partsoc_should)
topline(df = data_exp, variable = partsoc_should, weight = weight_siyasi)
crosstab(df = data_exp, x = lifestyle, y = partsoc_should, weight = weight_siyasi)

crosstab(df = data_exp, x = rel_prac, y = partsoc_should, weight = weight_siyasi)

crosstab(df = data_exp, x = eth_self, y = partsoc_should, weight = weight_siyasi)

# turkish citizenship

  # complete data
table(data_exp$citizenship)
topline(df = data_exp, variable = citizenship, weight = weight_siyasi)
crosstab(df = data_exp, x = lifestyle, y = citizenship, weight = weight_siyasi)

crosstab(df = data_exp, x = rel_prac, y = citizenship, weight = weight_siyasi)

crosstab(df = data_exp, x = eth_self, y = citizenship, weight = weight_siyasi)







## create tables without weights -------------------------------

# manipulation check

table(data_treat$manip_check)
table_manip_check <- table(data_treat$manip_check)        
percentage_table <- prop.table(table_manip_check) * 100     
percentage_table

table(data_control$manip_check)
table_manip_check <- table(data_control$manip_check)        
percentage_table <- prop.table(table_manip_check) * 100     
percentage_table


twoway_abs <- table(data_exp$treat, data_exp$manip_check)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- data_exp |>
  select(treat, manip_check) |>
  table() |>
  chisq.test()
res_chisqt

# preference in refugee - general

table(data_exp$prefref_gen)

twoway_abs <- table(data_exp$lifestyle, data_exp$prefref_gen)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- data_exp |>
  select(lifestyle, prefref_gen) |>
  table() |>
  chisq.test()
res_chisqt

# Talking to Syrians

  # by lifestyle

table(data_exp$syr_talking)

twoway_abs <- table(data_exp$lifestyle, data_exp$syr_talking)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

  # by ethnicity

table(data_exp$syr_talking)

twoway_abs <- table(data_exp$eth_self, data_exp$syr_talking)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)


# Socialising with Syrians

table(data_exp$syr_socialising)

twoway_abs <- table(data_exp$lifestyle, data_exp$syr_socialising)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

# Syrian friend

table(data_exp$syr_friend)

twoway_abs <- table(data_exp$lifestyle, data_exp$syr_friend)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

# Cultural similarity

table(data_exp$cultsim)

twoway_abs <- table(data_exp$lifestyle, data_exp$cultsim)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

# Religious similarity

table(data_exp$relsim)

twoway_abs <- table(data_exp$lifestyle, data_exp$relsim)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

# Acceptance in the same country  (Soru 901) 

  # by lifestyle

table(data_exp$Soru901)

twoway_abs <- table(data_exp$lifestyle, data_exp$Soru901)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

  # by ethnicity

table(data_exp$Soru901)

twoway_abs <- table(data_exp$eth_self, data_exp$Soru901)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)


# Acceptance in the same city  (Soru 902) 

  # by lifestyle

table(data_exp$Soru902)

twoway_abs <- table(data_exp$lifestyle, data_exp$Soru902)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

  # by ethnicity

table(data_exp$Soru902)

twoway_abs <- table(data_exp$eth_self, data_exp$Soru902)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

# Acceptance in the same neighbourhood, workplace or school  (Soru 903) 

  # by lifestyle

table(data_exp$Soru903)

twoway_abs <- table(data_exp$lifestyle, data_exp$Soru903)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

  # by ethnicity

table(data_exp$Soru903)

twoway_abs <- table(data_exp$eth_self, data_exp$Soru903)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)


# Acceptance in the same apartment block or as a neighbour (Soru 904) 

  # by lifestyle

table(data_exp$Soru904)

twoway_abs <- table(data_exp$lifestyle, data_exp$Soru904)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

  # by ethnicity

table(data_exp$Soru904)

twoway_abs <- table(data_exp$eth_self, data_exp$Soru904)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)


# Acceptance at home or in the family (Soru 905) 

  # by lifestyle

table(data_exp$Soru905)

twoway_abs <- table(data_exp$lifestyle, data_exp$Soru905)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

  # by ethnicity 

table(data_exp$Soru905)

twoway_abs <- table(data_exp$eth_self, data_exp$Soru905)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)








## estimate regressions --------------------------------------------


#### SOCIAL BOUNDARIES ----------------------  

## Staying away from Syrians ##

# full sample

table(data_exp$try_stayaway)

# by lifestyle

      # display table (try_stayaway)
twoway_abs <- table(data_exp$lifestyle, data_exp$try_stayaway)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

      # logistic regression (try_stayaway)
res_logit_bas <- glm(try_stayaway ~ lifestyle, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with controls (gender + age)
res_logit_bas <- glm(try_stayaway ~ lifestyle + gender + age, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (except family relations + sect)
res_logit_bas <- glm(try_stayaway ~ lifestyle + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (except social contact/family relations)
res_logit_bas <- glm(try_stayaway ~ lifestyle + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (no Syrian connection at all)
res_logit_bas <- glm(try_stayaway ~ lifestyle + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + Soru1308 + sect, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

vif(res_logit_bas)

      # new
res_logit_bas <- lm(try_stayaway ~ lifestyle + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + Soru1308 + sect, data = data_exp)
summary(res_logit_bas)

vif(res_logit_bas)

      # with all controls (inherent Syrian contact)
res_logit_bas <- glm(try_stayaway ~ lifestyle + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + syr_con2 + sect, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls - party vote
res_logit_bas <- glm(try_stayaway ~ lifestyle + gender + age + eth_self + edu_group + job_group + syr_presence + syr_encounter + Soru805 + Soru1308 + sect, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls + income
res_logit_bas <- glm(try_stayaway ~ lifestyle + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + Soru1308 + sect + income1, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as religious practice
res_logit_bas <- glm(try_stayaway ~ rel_prac, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as religious practice + sect
res_logit_bas <- glm(try_stayaway ~ rel_prac + sect, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as religious practice with controls
res_logit_bas <- glm(try_stayaway ~ rel_prac + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + syr_con2 + sect, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as head cover
res_logit_bas <- glm(try_stayaway ~ head_cov, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as head cover with controls
res_logit_bas <- glm(try_stayaway ~ head_cov + gender + age + eth_self + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# by ethnicity

# display table (try_stayaway)
twoway_abs <- table(data_exp$eth_self, data_exp$try_stayaway)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

      # logistic regression (try_stayaway)
res_logit_bas <- glm(try_stayaway ~ eth_self, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with controls (gender + age)
res_logit_bas <- glm(try_stayaway ~ eth_self + gender + age, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (except family relations + sect)
res_logit_bas <- glm(try_stayaway ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (non Syrian connection at all)
res_logit_bas <- glm(try_stayaway ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

     # with all controls (Syrian relative)
res_logit_bas <- glm(try_stayaway ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + Soru1302 , family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (inherent Syrian social contact)
res_logit_bas <- glm(try_stayaway ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # mother tongue as measurement of ethnicity
res_logit_bas <- glm(try_stayaway ~ language, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # mother tongue as measurement of ethnicity + controls
res_logit_bas <- glm(try_stayaway ~ language + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)



## Talking to Syrians ##

# full sample

# recode into binary (syr_talking_ever)
data_exp <- data_exp %>% 
  mutate(syr_talking_ever = recode(syr_talking, "Never" = "No", "Sometimes" = "Yes", "Often" = "Yes", "All the time" = "Yes"))
table(data_exp$syr_talking_ever)

  # by lifestyle

table(data_exp$syr_talking)

res_ologit_bas <- polr(syr_talking ~ lifestyle, data = data_exp)
summary(res_ologit_bas)

      # display table (syr_talking_ever)
twoway_abs <- table(data_exp$lifestyle, data_exp$syr_talking_ever)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

      # logistic regression (syr_talking_ever)
res_logit_bas <- glm(syr_talking_ever ~ lifestyle, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (except family relations + sect)
res_logit_bas <- glm(syr_talking_ever ~ lifestyle + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

    # with all controls (except Syrian social contact/relative)
res_logit_bas <- glm(syr_talking_ever ~ lifestyle + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

     # with all controls (no Syrian connection at all)
res_logit_bas <- glm(syr_talking_ever ~ lifestyle + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

    # with all controls (Syrian relative)
res_logit_bas <- glm(syr_talking_ever ~ lifestyle + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + Soru1302, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

    # with all controls (inherent Syrian contact)
res_logit_bas <- glm(syr_talking_ever ~ lifestyle + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

    # with all controls - party vote
res_logit_bas <- glm(syr_talking_ever ~ lifestyle + gender + age + eth_self + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (except family relations) + ethnicity as language
res_logit_bas <- glm(syr_talking_ever ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls + ethnicity as language 
res_logit_bas <- glm(syr_talking_ever ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as religious practice 
res_logit_bas <- glm(syr_talking_ever ~ rel_prac, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as religious practice with controls
res_logit_bas <- glm(syr_talking_ever ~ rel_prac + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as head cover
res_logit_bas <- glm(syr_talking_ever ~ head_cov, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as head cover + female
res_logit_bas <- glm(syr_talking_ever ~ head_cov + gender, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as head cover with controls
res_logit_bas <- glm(syr_talking_ever ~ head_cov + gender + age + eth_self + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

  # by ethnicity

      # display table (syr_talking_ever)
twoway_abs <- table(data_exp$eth_self, data_exp$syr_talking_ever)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

      # logistic regression (syr_talking_ever)
res_logit_bas <- glm(syr_talking_ever ~ eth_self, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with controls (gender + age)
res_logit_bas <- glm(syr_talking_ever ~ eth_self + gender + age, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (except family relations + sect)
res_logit_bas <- glm(syr_talking_ever ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (except family relations)
res_logit_bas <- glm(syr_talking_ever ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (inherent Syrian contact)
res_logit_bas <- glm(syr_talking_ever ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # same model but with mother tongue as measurement of ethnicity
res_logit_bas <- glm(syr_talking_ever ~ language + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # base model with mother tongue as measurement of ethnicity
res_logit_bas <- glm(syr_talking_ever ~ language, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)



## Socialising with Syrians ## 

# full sample

table(data_exp$syr_socialising)

# recode into binary (syr_socialising_ever)
data_exp <- data_exp %>% 
  mutate(syr_socialising_ever = recode(syr_socialising, "Never" = "No", "Sometimes" = "Yes", "Often" = "Yes", "All the time" = "Yes"))
table(data_exp$syr_socialising_ever)

# by lifestyle

# display table (syr_socialising_ever)
twoway_abs <- table(data_exp$lifestyle, data_exp$syr_socialising_ever)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

# logistic regression (syr_socialising_ever)
res_logit_bas <- glm(syr_socialising_ever ~ lifestyle, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# controlling for sect
res_logit_bas <- glm(syr_socialising_ever ~ lifestyle + sect, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# with all controls (except family relations + sect)
res_logit_bas <- glm(syr_socialising_ever ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# with all controls (except family relations)
res_logit_bas <- glm(syr_socialising_ever ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# with all controls (no Syrian connection at all)
res_logit_bas <- glm(syr_socialising_ever ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# with all controls (inherent Syrian contact)
res_logit_bas <- glm(syr_socialising_ever ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# with all controls + Bölge
res_logit_bas <- glm(syr_socialising_ever ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + Soru1308 + Bölge, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# with religious identity as religious practice 
res_logit_bas <- glm(syr_socialising_ever ~ rel_prac, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# with religious identity as religious practice + sect
res_logit_bas <- glm(syr_socialising_ever ~ rel_prac + sect, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# with religious identity as religious practice with controls
res_logit_bas <- glm(syr_socialising_ever ~ rel_prac + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# with religious identity as head cover
res_logit_bas <- glm(syr_socialising_ever ~ head_cov, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# with religious identity as head cover with controls
res_logit_bas <- glm(syr_socialising_ever ~ head_cov + gender + age + eth_self + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# by ethnicity

# display table (syr_socialising_ever)
twoway_abs <- table(data_exp$eth_self, data_exp$syr_socialising_ever)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

# logistic regression (syr_socialising_ever)
res_logit_bas <- glm(syr_socialising_ever ~ eth_self, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# with controls (gender + age)
res_logit_bas <- glm(syr_socialising_ever ~ eth_self + gender + age, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# with all controls (except family relations + sect)
res_logit_bas <- glm(syr_socialising_ever ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# with all controls (except family relations)
res_logit_bas <- glm(syr_socialising_ever ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# with all controls (inherent Syrian contact)
res_logit_bas <- glm(syr_socialising_ever ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# with mother tongue as measurement of ethnicity
res_logit_bas <- glm(syr_socialising_ever ~ language, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# with mother tongue as measurement of ethnicity + controls
res_logit_bas <- glm(syr_socialising_ever ~ language + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)



## Having a Syrian friend ## 

# full sample

table(data_exp$syr_friend)

# by lifestyle

      # display table (syr_friend)
twoway_abs <- table(data_exp$lifestyle, data_exp$syr_friend)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

      # logistic regression (syr_friend)
res_logit_bas <- glm(syr_friend ~ lifestyle, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (except family relations + sect)
res_logit_bas <- glm(syr_friend ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (except family relations)
res_logit_bas <- glm(syr_friend ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (no Syrian connection at all)
res_logit_bas <- glm(syr_friend ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (inherent Syrian contact)
res_logit_bas <- glm(syr_friend ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as religious practice 
res_logit_bas <- glm(syr_friend ~ rel_prac, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as religious practice with controls (except family relations + sect)
res_logit_bas <- glm(syr_friend ~ rel_prac + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as religious practice with controls (except family relations)
res_logit_bas <- glm(syr_friend ~ rel_prac + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as religious practice with controls
res_logit_bas <- glm(syr_friend ~ rel_prac + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as head cover
res_logit_bas <- glm(syr_friend ~ head_cov, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as head cover with controls
res_logit_bas <- glm(syr_friend ~ head_cov + gender + age + eth_self + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# by ethnicity

      # display table (syr_friend)
twoway_abs <- table(data_exp$eth_self, data_exp$syr_friend)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

      # logistic regression (syr_friend)
res_logit_bas <- glm(syr_friend ~ eth_self, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with controls (gender + age)
res_logit_bas <- glm(syr_friend ~ eth_self + gender + age, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (except family relations + sect)
res_logit_bas <- glm(syr_friend ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (except family relations)
res_logit_bas <- glm(syr_friend ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (inherent Syrian contact)
res_logit_bas <- glm(syr_friend ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # same model but with mother tongue as measurement of ethnicity
res_logit_bas <- glm(syr_friend ~ language + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # same model but with mother tongue as measurement of ethnicity (without sect)
res_logit_bas <- glm(syr_friend ~ language + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # base model with mother tongue as measurement of ethnicity
res_logit_bas <- glm(syr_friend ~ language, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)


## SYMBLIC BOUNDARIES - BEFORE TREATMENT -------------------


## Acceptance in the same country  (Soru 901) ##

# full sample

table(data_exp$Soru901)

# by lifestyle

# display table (Soru901)
twoway_abs <- table(data_exp$lifestyle, data_exp$Soru901)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

      # logistic regression (Soru901)
res_logit_bas <- glm(Soru901 ~ lifestyle, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with controls (gender + age)
res_logit_bas <- glm(Soru901 ~ lifestyle + gender + age, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls 
res_logit_bas <- glm(Soru901 ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as religious practice 
res_logit_bas <- glm(Soru901 ~ rel_prac, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as religious practice with controls
res_logit_bas <- glm(Soru901 ~ rel_prac + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as head cover
res_logit_bas <- glm(Soru901 ~ head_cov, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as head cover with controls
res_logit_bas <- glm(Soru901 ~ head_cov + gender + age + eth_self + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# by ethnicity

# display table (Soru901)
twoway_abs <- table(data_exp$eth_self, data_exp$Soru901)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

      # logistic regression (eth_self)
res_logit_bas <- glm(Soru901 ~ eth_self, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with controls (gender + age)
res_logit_bas <- glm(Soru901 ~ eth_self + gender + age, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (except family relations + sect)
res_logit_bas <- glm(Soru901 ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (except family relations)
res_logit_bas <- glm(Soru901 ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls 
res_logit_bas <- glm(Soru901 ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # same model but with mother tongue as measurement of ethnicity
res_logit_bas <- glm(Soru901 ~ language + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # base model with mother tongue as measurement of ethnicity
res_logit_bas <- glm(Soru901 ~ language, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

.....

## Acceptance in the same city  (Soru 902) ##

# full sample

table(data_exp$Soru902)

# by lifestyle

# display table (Soru 902)
twoway_abs <- table(data_exp$lifestyle, data_exp$Soru902)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

# logistic regression (Soru902)
res_logit_bas <- glm(Soru902 ~ lifestyle, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with controls (gender + age)
res_logit_bas <- glm(Soru902 ~ lifestyle + gender + age, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (except family relations + sect)
res_logit_bas <- glm(Soru902 ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (except family relations)
res_logit_bas <- glm(Soru902 ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls 
res_logit_bas <- glm(Soru902 ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as religious practice 
res_logit_bas <- glm(Soru902 ~ rel_prac, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as religious practice with controls
res_logit_bas <- glm(Soru902 ~ rel_prac + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as head cover
res_logit_bas <- glm(Soru902 ~ head_cov, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as head cover with controls
res_logit_bas <- glm(Soru902 ~ head_cov + gender + age + eth_self + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# by ethnicity

# display table (Soru902)
twoway_abs <- table(data_exp$eth_self, data_exp$Soru902)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

# logistic regression (eth_self)
res_logit_bas <- glm(Soru902 ~ eth_self, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with controls (gender + age)
res_logit_bas <- glm(Soru902 ~ eth_self + gender + age, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (except family relations + sect)
res_logit_bas <- glm(Soru902 ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (except family relations)
res_logit_bas <- glm(Soru902 ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls 
res_logit_bas <- glm(Soru902 ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # same model but with mother tongue as measurement of ethnicity
res_logit_bas <- glm(Soru902 ~ language + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # base model with mother tongue as measurement of ethnicity
res_logit_bas <- glm(Soru902 ~ language, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

.......

## Acceptance in the same neighbourhood, workplace or school (Soru 903) ##

# full sample

table(data_exp$Soru903)

# by lifestyle

# display table (Soru903)
twoway_abs <- table(data_exp$lifestyle, data_exp$Soru903)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

      # logistic regression (Soru903)
res_logit_bas <- glm(Soru903 ~ lifestyle, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with controls (gender + age)
res_logit_bas <- glm(Soru903 ~ lifestyle + gender + age, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (except family relations & sect)
res_logit_bas <- glm(Soru903 ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (except family relations)
res_logit_bas <- glm(Soru903 ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls 
res_logit_bas <- glm(Soru903 ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as religious practice 
res_logit_bas <- glm(Soru903 ~ rel_prac, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as religious practice with controls
res_logit_bas <- glm(Soru903 ~ rel_prac + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as head cover
res_logit_bas <- glm(Soru903 ~ head_cov, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as head cover with controls
res_logit_bas <- glm(Soru903 ~ head_cov + gender + age + eth_self + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# by ethnicity

# display table (Soru903)
twoway_abs <- table(data_exp$eth_self, data_exp$Soru903)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

      # logistic regression (eth_self)
res_logit_bas <- glm(Soru903 ~ eth_self, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with controls (gender + age)
res_logit_bas <- glm(Soru903 ~ eth_self + gender + age, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (except family relations + sect)
res_logit_bas <- glm(Soru903 ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (except family relations)
res_logit_bas <- glm(Soru903 ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls 
res_logit_bas <- glm(Soru903 ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # base model with mother tongue as measurement of ethnicity
res_logit_bas <- glm(Soru903 ~ language, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with mother tongue as measurement of ethnicity + all controls (except acquaintance + sect)
res_logit_bas <- glm(Soru903 ~ language + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with mother tongue as measurement of ethnicity + all controls (except acquaintance)
res_logit_bas <- glm(Soru903 ~ language + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with mother tongue as measurement of ethnicity + all controls
res_logit_bas <- glm(Soru903 ~ language + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)
.......


## Acceptance as neighbour, in one's apartment building or in one's friends' circle (Soru 904) ##

# full sample

table(data_exp$Soru904)

# by lifestyle

# display table (Soru904)
twoway_abs <- table(data_exp$lifestyle, data_exp$Soru904)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

# logistic regression (Soru904)
res_logit_bas <- glm(Soru904 ~ lifestyle, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with controls (gender + age)
res_logit_bas <- glm(Soru904 ~ lifestyle + gender + age, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (except family relations + sect)
res_logit_bas <- glm(Soru904 ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (except family relations)
res_logit_bas <- glm(Soru904 ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls 
res_logit_bas <- glm(Soru904 ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as religious practice 
res_logit_bas <- glm(Soru904 ~ rel_prac, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as religious practice with controls (w/o sect)
res_logit_bas <- glm(Soru904 ~ rel_prac + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as religious practice with controls
res_logit_bas <- glm(Soru904 ~ rel_prac + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as head cover
res_logit_bas <- glm(Soru904 ~ head_cov, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as head cover with controls
res_logit_bas <- glm(Soru904 ~ head_cov + gender + age + eth_self + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# by ethnicity

# display table (Soru904)
twoway_abs <- table(data_exp$eth_self, data_exp$Soru904)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

# logistic regression (eth_self)
res_logit_bas <- glm(Soru904 ~ eth_self, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with controls (gender + age)
res_logit_bas <- glm(Soru904 ~ eth_self + gender + age, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (except family relations + sect)
res_logit_bas <- glm(Soru904 ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (except family relations)
res_logit_bas <- glm(Soru904 ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls 
res_logit_bas <- glm(Soru904 ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # base model with mother tongue as measurement of ethnicity
res_logit_bas <- glm(Soru904 ~ language, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with mother tongue as measurement of ethnicity + all controls (except acquaintance + sect)
res_logit_bas <- glm(Soru904 ~ language + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with mother tongue as measurement of ethnicity + all controls (except acquaintance)
res_logit_bas <- glm(Soru904 ~ language + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with mother tongue as measurement of ethnicity + all controls
res_logit_bas <- glm(Soru904 ~ language + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

.......

## Acceptance in the same family or home (Soru 905) ##

# full sample

table(data_exp$Soru905)

# by lifestyle

# display table (Soru905)
twoway_abs <- table(data_exp$lifestyle, data_exp$Soru905)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

      # logistic regression (Soru905)
res_logit_bas <- glm(Soru905 ~ lifestyle, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with controls (gender + age)
res_logit_bas <- glm(Soru905 ~ lifestyle + gender + age, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (except family relations + sect)
res_logit_bas <- glm(Soru905 ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (except family relations)
res_logit_bas <- glm(Soru905 ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls 
res_logit_bas <- glm(Soru905 ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as religious practice 
res_logit_bas <- glm(Soru905 ~ rel_prac, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as religious practice with controls
res_logit_bas <- glm(Soru905 ~ rel_prac + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as head cover
res_logit_bas <- glm(Soru905 ~ head_cov, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as head cover with controls
res_logit_bas <- glm(Soru905 ~ head_cov + gender + age + eth_self + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# by ethnicity

# display table (Soru905)
twoway_abs <- table(data_exp$eth_self, data_exp$Soru905)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

# logistic regression (eth_self)
res_logit_bas <- glm(Soru905 ~ eth_self, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)



# by ethnicity

# display table (syrref_eth)
twoway_abs <- table(data_exp$eth_self, data_exp$syrref_eth)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

#  recoding into binary

  # prefer Arab
data_exp <- data_exp %>% 
  mutate(syrref_prefarab = recode(syrref_eth, "Doesn't matter"="No", "Prefer Arab"="Yes", "Prefer Kurdish"="No"))
table(data_exp$syrref_prefarab)

  # prefer Kurdish
data_exp <- data_exp %>% 
  mutate(syrref_prefkurd = recode(syrref_eth, "Doesn't matter"="No", "Prefer Arab"="No", "Prefer Kurdish"="Yes"))
table(data_exp$syrref_prefkurd)

# regressions for prefer Arab

      # logistic regression (syrref_prefarab)
res_logit_bas <- glm(syrref_prefarab ~ eth_self, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with controls (gender + age)
res_logit_bas <- glm(syrref_prefarab ~ eth_self + gender + age, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (except family relations + sect)
res_logit_bas <- glm(syrref_prefarab ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (except family relations)
res_logit_bas <- glm(syrref_prefarab ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls 
res_logit_bas <- glm(syrref_prefarab ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # same model but with mother tongue as measurement of ethnicity
res_logit_bas <- glm(syrref_prefarab ~ language + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # same model but with mother tongue as measurement of ethnicity (without sect)
res_logit_bas <- glm(syrref_prefarab ~ language + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # base model with mother tongue as measurement of ethnicity
res_logit_bas <- glm(syrref_prefarab ~ language, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)





## Cultural Similarity ##

# full sample

table(data_exp$cultsim)

# recode into binary (cultsim_any)
data_exp <- data_exp %>% 
  mutate(cultsim_any = recode(cultsim, `1` = 0, `2` = 0, `3` = 0, `4` = 1, `5` = 1))
data_exp <- data_exp %>% 
mutate(cultsim_any = factor(
  cultsim_any, levels = c(0, 1), 
         labels = c("Not similar/Undecided", "Similar/Very similar")
         ))
table(data_exp$cultsim_any)

topline(df = data_exp, variable = cultsim_any, weight = weight_siyasi)
crosstab(df = data_exp, x = lifestyle, y = cultsim_any, weight = weight_siyasi)
crosstab(df = data_exp, x = eth_self, y = cultsim_any, weight = weight_siyasi)

# by lifestyle

      # display table (cultsim_any)
twoway_abs <- table(data_exp$lifestyle, data_exp$cultsim_any)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

      # logistic regression (cultsim_any)
res_logit_bas <- glm(cultsim_any ~ lifestyle, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (except family relations + sect)
res_logit_bas <- glm(cultsim_any ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (except family relations)
res_logit_bas <- glm(cultsim_any ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls 
res_logit_bas <- glm(cultsim_any ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls + Bölge
res_logit_bas <- glm(cultsim_any ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + Soru1308 + Bölge, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls & with ethnicity as eth_self
res_logit_bas <- glm(cultsim_any ~ lifestyle + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as religious practice 
res_logit_bas <- glm(cultsim_any ~ rel_prac, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as religious practice + sect
res_logit_bas <- glm(cultsim_any ~ rel_prac + sect, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as religious practice with controls
res_logit_bas <- glm(cultsim_any ~ rel_prac + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as head cover
res_logit_bas <- glm(cultsim_any ~ head_cov + sect, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as head cover with controls
res_logit_bas <- glm(cultsim_any ~ head_cov + gender + age + eth_self + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# by ethnicity

    # display table (cultsim_any)
twoway_abs <- table(data_exp$eth_self, data_exp$cultsim_any)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

      # logistic regression (cultsim_any)
res_logit_bas <- glm(cultsim_any ~ eth_self, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with controls (gender + age)
res_logit_bas <- glm(cultsim_any ~ eth_self + gender + age, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with controls (gender + age + sect)
res_logit_bas <- glm(cultsim_any ~ eth_self + gender + age + sect, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (except family relations etc)
res_logit_bas <- glm(cultsim_any ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls 
res_logit_bas <- glm(cultsim_any ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # ethnicity as mother tongue
res_logit_bas <- glm(cultsim_any ~ language, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # ethnicity as mother tongue + controls
res_logit_bas <- glm(cultsim_any ~ language + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)





## Religious Similarity ##

# full sample

table(data_exp$relsim)

# recode into binary (relsim_any)
data_exp <- data_exp %>% 
  mutate(relsim_any = recode(relsim, `1` = 0, `2` = 0, `3` = 0, `4` = 1, `5` = 1))
table(data_exp$relsim_any)

original_table <- table(data_exp$relsim_any)
total_count <- sum(table(data_exp$relsim_any))
percentages <- (original_table / total_count) * 100
print(percentages)

# by lifestyle

      # display table (relsim_any)
twoway_abs <- table(data_exp$lifestyle, data_exp$relsim_any)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

      # logistic regression (relsim_any)
res_logit_bas <- glm(relsim_any ~ lifestyle, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (except family relations)
res_logit_bas <- glm(relsim_any ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls 
res_logit_bas <- glm(relsim_any ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls + Bölge
res_logit_bas <- glm(relsim_any ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + Soru1308 + Bölge, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls & with ethnicity as eth_self
res_logit_bas <- glm(relsim_any ~ lifestyle + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as religious practice 
res_logit_bas <- glm(relsim_any ~ rel_prac, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as religious practice with controls (except family relations)
res_logit_bas <- glm(relsim_any ~ rel_prac + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as religious practice with all controls
res_logit_bas <- glm(relsim_any ~ rel_prac + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as head coverage
res_logit_bas <- glm(relsim_any ~ head_cov, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with religious identity as head coverage with controls
res_logit_bas <- glm(relsim_any ~ head_cov + gender + age + eth_self + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# by ethnicity

# display table (relsim_any)
twoway_abs <- table(data_exp$eth_self, data_exp$relsim_any)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

      # logistic regression (relsim_any)
res_logit_bas <- glm(relsim_any ~ eth_self, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with controls (gender + age)
res_logit_bas <- glm(relsim_any ~ eth_self + gender + age, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls (except family relations etc)
res_logit_bas <- glm(relsim_any ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # with all controls 
res_logit_bas <- glm(relsim_any ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # same model but with mother tongue as measurement of ethnicity
res_logit_bas <- glm(relsim_any ~ language + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

      # base model with mother tongue as measurement of ethnicity
res_logit_bas <- glm(relsim_any ~ language, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)









#### SYMBOLIC BOUNDARIES - AFTER TREATMENT ---------------------  


## PART OF SOCIETY - TODAY (Soru 2301) ## --------

# full sample

  # baseline model

res_ols_bas <- data_exp %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols_bas)

  # extended model

res_ols_ext <- data_exp %>%
  lm(partsoc_today ~ treat + gender + age + lifestyle, data = .)
summary(res_ols_ext)

# by gender

table(data_exp$gender)

res_ols_mal <- data_exp %>%
  filter(gender == "male") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols_mal)

res_ols_fem <- data_exp  %>%
  filter(gender == "female") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols_fem)

# by age

summary(data_exp$age)

res_ols_age <- data_exp %>%
  lm(partsoc_today ~ treat * age, data = .)
summary(res_ols_age)

  # consider using age groups as well.

# by lifestyle

table(data_exp$lifestyle)

res_ols_mod <- data_exp  %>%
  filter(lifestyle == "modern") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols_mod)

res_ols_trac <- data_exp  %>%
  filter(lifestyle == "traditional conservative") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols_trac)

res_ols_relc <- data_exp  %>%
  filter(lifestyle == "religious conservative") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols_relc)

# by ethnic self-identification

table(data_exp$eth_self)

res_ols_mod <- data_exp  %>%
  filter(eth_self == "Turkish") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols_mod)

res_ols_trac <- data_exp  %>%
  filter(eth_self == "Kurdish") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols_trac)

res_ols_relc <- data_exp  %>%
  filter(eth_self == "Arab") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols_relc)

# by lifestyle and gender

twoway_abs <- table(data_exp$lifestyle, data_exp$gender)
twoway_abs

twoway_sha_row <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_sha_row

twoway_sha_col <- twoway_abs |>
  prop.table(margin = 2) |>
  round(digits = 2)
twoway_sha_col

  #

res_ols_mod_mal <- data_exp  %>%
  filter(lifestyle == "modern" & gender == "male") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols_mod_mal)

res_ols_mod_fem <- data_exp  %>%
  filter(lifestyle == "modern" & gender == "female") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols_mod_fem)

res_ols_trac_mal <- data_exp  %>%
  filter(lifestyle == "traditional conservative" & gender == "male") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols_trac_mal)

res_ols_trac_fem <- data_exp  %>%
  filter(lifestyle == "traditional conservative" & gender == "female") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols_trac_fem)

res_ols_relc_mal <- data_exp  %>%
  filter(lifestyle == "religious conservative" & gender == "male") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols_relc_mal)

res_ols_relc_fem <- data_exp  %>%
  filter(lifestyle == "religious conservative" & gender == "female") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols_relc_fem)


# by lifestyle & ethnic self-identification

res_ols_mod_turk <- data_exp  %>%
  filter(lifestyle == "modern" & eth_self == "Turkish") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols_mod_turk)

res_ols_mod_kurd <- data_exp  %>%
  filter(lifestyle == "modern" & eth_self == "Kurdish") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols_mod_kurd)

res_ols_mod_arab <- data_exp  %>%
  filter(lifestyle == "modern" & eth_self == "Arab") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols_mod_arab)

#

res_ols_trac_turk <- data_exp  %>%
  filter(lifestyle == "traditional conservative" & eth_self == "Turkish") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols_trac_turk)

res_ols_trac_kurd <- data_exp  %>%
  filter(lifestyle == "traditional conservative" & eth_self == "Kurdish") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols_trac_kurd)

res_ols_trac_arab <- data_exp  %>%
  filter(lifestyle == "traditional conservative" & eth_self == "Arab") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols_trac_arab)

#

res_ols_relc_turk <- data_exp  %>%
  filter(lifestyle == "religious conservative" & eth_self == "Turkish") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols_relc_turk)

res_ols_relc_kurd <- data_exp  %>%
  filter(lifestyle == "religious conservative" & eth_self == "Kurdish") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols_relc_kurd)

res_ols_relc_arab <- data_exp  %>%
  filter(lifestyle == "religious conservative" & eth_self == "Arab") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols_relc_arab)



# by lifestyle, ethnic self-identification & sect (only for Sunni Muslims)

res_ols_mod_turk <- data_exp  %>%
  filter(lifestyle == "modern" & eth_self == "Turkish" & sect == "Sunni") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols_mod_turk)

res_ols_mod_kurd <- data_exp  %>%
  filter(lifestyle == "modern" & eth_self == "Kurdish" & sect == "Sunni") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols_mod_kurd)

        ## no modern Sunni Arabs in the sample

#

res_ols_trac_turk <- data_exp  %>%
  filter(lifestyle == "traditional conservative" & eth_self == "Turkish" & sect == "Sunni") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols_trac_turk)

res_ols_trac_kurd <- data_exp  %>%
  filter(lifestyle == "traditional conservative" & eth_self == "Kurdish" & sect == "Sunni") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols_trac_kurd)

res_ols_trac_arab <- data_exp  %>%
  filter(lifestyle == "traditional conservative" & eth_self == "Arab" & sect == "Sunni") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols_trac_arab)

#

res_ols_relc_turk <- data_exp  %>%
  filter(lifestyle == "religious conservative" & eth_self == "Turkish" & sect == "Sunni") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols_relc_turk)

res_ols_relc_kurd <- data_exp  %>%
  filter(lifestyle == "religious conservative" & eth_self == "Kurdish" & sect == "Sunni") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols_relc_kurd)

res_ols_relc_arab <- data_exp  %>%
  filter(lifestyle == "religious conservative" & eth_self == "Arab" & sect == "Sunni") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols_relc_arab)



## PART OF SOCIETY - TOMORROW (Soru 2302) #######################

# full sample

# baseline model

res_ols_bas <- data_exp %>%
  lm(partsoc_tomo ~ treat, data = .)
summary(res_ols_bas)

# by lifestyle

table(data_exp$lifestyle)

res_ols_mod <- data_exp  %>%
  filter(lifestyle == "modern") %>%
  lm(partsoc_tomo ~ treat, data = .)
summary(res_ols_mod)

res_ols_trac <- data_exp  %>%
  filter(lifestyle == "traditional conservative") %>%
  lm(partsoc_tomo ~ treat, data = .)
summary(res_ols_trac)

res_ols_relc <- data_exp  %>%
  filter(lifestyle == "religious conservative") %>%
  lm(partsoc_tomo ~ treat, data = .)
summary(res_ols_relc)

# by ethnic self-identification

table(data_exp$eth_self)

res_ols_mod <- data_exp  %>%
  filter(eth_self == "Turkish") %>%
  lm(partsoc_tomo ~ treat, data = .)
summary(res_ols_mod)

res_ols_trac <- data_exp  %>%
  filter(eth_self == "Kurdish") %>%
  lm(partsoc_tomo ~ treat, data = .)
summary(res_ols_trac)

res_ols_relc <- data_exp  %>%
  filter(eth_self == "Arab") %>%
  lm(partsoc_tomo ~ treat, data = .)
summary(res_ols_relc)


# by lifestyle & ethnic self-identification

res_ols_mod_turk <- data_exp  %>%
  filter(lifestyle == "modern" & eth_self == "Turkish") %>%
  lm(partsoc_tomo ~ treat, data = .)
summary(res_ols_mod_turk)

res_ols_mod_kurd <- data_exp  %>%
  filter(lifestyle == "modern" & eth_self == "Kurdish") %>%
  lm(partsoc_tomo ~ treat, data = .)
summary(res_ols_mod_kurd)

res_ols_mod_arab <- data_exp  %>%
  filter(lifestyle == "modern" & eth_self == "Arab") %>%
  lm(partsoc_tomo ~ treat, data = .)
summary(res_ols_mod_arab)

#

res_ols_trac_turk <- data_exp  %>%
  filter(lifestyle == "traditional conservative" & eth_self == "Turkish") %>%
  lm(partsoc_tomo ~ treat, data = .)
summary(res_ols_trac_turk)

res_ols_trac_kurd <- data_exp  %>%
  filter(lifestyle == "traditional conservative" & eth_self == "Kurdish") %>%
  lm(partsoc_tomo ~ treat, data = .)
summary(res_ols_trac_kurd)

res_ols_trac_arab <- data_exp  %>%
  filter(lifestyle == "traditional conservative" & eth_self == "Arab") %>%
  lm(partsoc_tomo ~ treat, data = .)
summary(res_ols_trac_arab)

#

res_ols_relc_turk <- data_exp  %>%
  filter(lifestyle == "religious conservative" & eth_self == "Turkish") %>%
  lm(partsoc_tomo ~ treat, data = .)
summary(res_ols_relc_turk)

res_ols_relc_kurd <- data_exp  %>%
  filter(lifestyle == "religious conservative" & eth_self == "Kurdish") %>%
  lm(partsoc_tomo ~ treat, data = .)
summary(res_ols_relc_kurd)

res_ols_relc_arab <- data_exp  %>%
  filter(lifestyle == "religious conservative" & eth_self == "Arab") %>%
  lm(partsoc_tomo ~ treat, data = .)
summary(res_ols_relc_arab)




## PART OF SOCIETY - SHOULD (Soru 2303) #######################

# full sample

# baseline model

res_ols_bas <- data_exp %>%
  lm(partsoc_should ~ treat, data = .)
summary(res_ols_bas)

# by lifestyle

table(data_exp$lifestyle)

res_ols_mod <- data_exp  %>%
  filter(lifestyle == "modern") %>%
  lm(partsoc_should ~ treat, data = .)
summary(res_ols_mod)

res_ols_trac <- data_exp  %>%
  filter(lifestyle == "traditional conservative") %>%
  lm(partsoc_should ~ treat, data = .)
summary(res_ols_trac)

res_ols_relc <- data_exp  %>%
  filter(lifestyle == "religious conservative") %>%
  lm(partsoc_should ~ treat, data = .)
summary(res_ols_relc)

# by ethnic self-identification

table(data_exp$eth_self)

res_ols_turk <- data_exp  %>%
  filter(eth_self == "Turkish") %>%
  lm(partsoc_should ~ treat, data = .)
summary(res_ols_turk)

res_ols_kurd <- data_exp  %>%
  filter(eth_self == "Kurdish") %>%
  lm(partsoc_should ~ treat, data = .)
summary(res_ols_kurd)

res_ols_arab <- data_exp  %>%
  filter(eth_self == "Arab") %>%
  lm(partsoc_should ~ treat, data = .)
summary(res_ols_arab)


# by lifestyle & ethnic self-identification

res_ols_mod_turk <- data_exp  %>%
  filter(lifestyle == "modern" & eth_self == "Turkish") %>%
  lm(partsoc_should ~ treat, data = .)
summary(res_ols_mod_turk)

res_ols_mod_kurd <- data_exp  %>%
  filter(lifestyle == "modern" & eth_self == "Kurdish") %>%
  lm(partsoc_should ~ treat, data = .)
summary(res_ols_mod_kurd)

res_ols_mod_arab <- data_exp  %>%
  filter(lifestyle == "modern" & eth_self == "Arab") %>%
  lm(partsoc_should ~ treat, data = .)
summary(res_ols_mod_arab)

#

res_ols_trac_turk <- data_exp  %>%
  filter(lifestyle == "traditional conservative" & eth_self == "Turkish") %>%
  lm(partsoc_should ~ treat, data = .)
summary(res_ols_trac_turk)

res_ols_trac_kurd <- data_exp  %>%
  filter(lifestyle == "traditional conservative" & eth_self == "Kurdish") %>%
  lm(partsoc_should ~ treat, data = .)
summary(res_ols_trac_kurd)

res_ols_trac_arab <- data_exp  %>%
  filter(lifestyle == "traditional conservative" & eth_self == "Arab") %>%
  lm(partsoc_should ~ treat, data = .)
summary(res_ols_trac_arab)

#

res_ols_relc_turk <- data_exp  %>%
  filter(lifestyle == "religious conservative" & eth_self == "Turkish") %>%
  lm(partsoc_should ~ treat, data = .)
summary(res_ols_relc_turk)

res_ols_relc_kurd <- data_exp  %>%
  filter(lifestyle == "religious conservative" & eth_self == "Kurdish") %>%
  lm(partsoc_should ~ treat, data = .)
summary(res_ols_relc_kurd)

res_ols_relc_arab <- data_exp  %>%
  filter(lifestyle == "religious conservative" & eth_self == "Arab") %>%
  lm(partsoc_should ~ treat, data = .)
summary(res_ols_relc_arab)






## TURKISH CITIZENSHIP (Soru 2304) #######################

# full sample

# baseline model

res_ols_bas <- data_exp %>%
  lm(citizenship ~ treat, data = .)
summary(res_ols_bas)

# by lifestyle

table(data_exp$lifestyle)

res_ols_mod <- data_exp  %>%
  filter(lifestyle == "modern") %>%
  lm(citizenship ~ treat, data = .)
summary(res_ols_mod)

res_ols_trac <- data_exp  %>%
  filter(lifestyle == "traditional conservative") %>%
  lm(citizenship ~ treat, data = .)
summary(res_ols_trac)

res_ols_relc <- data_exp  %>%
  filter(lifestyle == "religious conservative") %>%
  lm(citizenship ~ treat, data = .)
summary(res_ols_relc)

# by ethnic self-identification

table(data_exp$eth_self)

res_ols_mod <- data_exp  %>%
  filter(eth_self == "Turkish") %>%
  lm(citizenship ~ treat, data = .)
summary(res_ols_mod)

res_ols_trac <- data_exp  %>%
  filter(eth_self == "Kurdish") %>%
  lm(citizenship ~ treat, data = .)
summary(res_ols_trac)

res_ols_relc <- data_exp  %>%
  filter(eth_self == "Arab") %>%
  lm(citizenship ~ treat, data = .)
summary(res_ols_relc)

# by lifestyle + ethnic self-identification

res_ols_mod_turk <- data_exp  %>%
  filter(lifestyle == "modern" & eth_self == "Turkish") %>%
  lm(citizenship ~ treat, data = .)
summary(res_ols_mod_turk)

res_ols_mod_kurd <- data_exp  %>%
  filter(lifestyle == "modern" & eth_self == "Kurdish") %>%
  lm(citizenship ~ treat, data = .)
summary(res_ols_mod_kurd)

res_ols_mod_arab <- data_exp  %>%
  filter(lifestyle == "modern" & eth_self == "Arab") %>%
  lm(citizenship ~ treat, data = .)
summary(res_ols_mod_arab)

#

res_ols_trac_turk <- data_exp  %>%
  filter(lifestyle == "traditional conservative" & eth_self == "Turkish") %>%
  lm(citizenship ~ treat, data = .)
summary(res_ols_trac_turk)

res_ols_trac_kurd <- data_exp  %>%
  filter(lifestyle == "traditional conservative" & eth_self == "Kurdish") %>%
  lm(citizenship ~ treat, data = .)
summary(res_ols_trac_kurd)

res_ols_trac_arab <- data_exp  %>%
  filter(lifestyle == "traditional conservative" & eth_self == "Arab") %>%
  lm(citizenship ~ treat, data = .)
summary(res_ols_trac_arab)

#

res_ols_relc_turk <- data_exp  %>%
  filter(lifestyle == "religious conservative" & eth_self == "Turkish") %>%
  lm(citizenship ~ treat, data = .)
summary(res_ols_relc_turk)

res_ols_relc_kurd <- data_exp  %>%
  filter(lifestyle == "religious conservative" & eth_self == "Kurdish") %>%
  lm(citizenship ~ treat, data = .)
summary(res_ols_relc_kurd)

res_ols_relc_arab <- data_exp  %>%
  filter(lifestyle == "religious conservative" & eth_self == "Arab") %>%
  lm(citizenship ~ treat, data = .)
summary(res_ols_relc_arab)



## RELIGIOUS BROTHERHOOD (Soru 24) #######################

# full sample

# tables

table(data_exp$rel_bro)

twoway_abs <- table(data_exp$treat, data_exp$rel_bro)
twoway_abs

twoway_sha_row <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_sha_row

twoway_sha_col <- twoway_abs |>
  prop.table(margin = 2) |>
  round(digits = 2)
twoway_sha_col

# baseline model

res_logit_bas <- data_exp %>%
  glm(rel_bro ~ treat, family = binomial(link = "logit"), data = .)
summary(res_logit_bas)

# by lifestyle + ethnic self-identification

res_logit_mod_turk <- data_exp  %>%
  filter(lifestyle == "modern" & eth_self == "Turkish") %>%
  glm(rel_bro ~ treat, family = binomial(link = "logit"), data = .)
summary(res_logit_mod_turk)

res_logit_mod_kurd <- data_exp  %>%
  filter(lifestyle == "modern" & eth_self == "Kurdish") %>%
  glm(rel_bro ~ treat, family = binomial(link = "logit"), data = .)
summary(res_logit_mod_kurd)

res_logit_mod_arab <- data_exp  %>%
  filter(lifestyle == "modern" & eth_self == "Arab") %>%
  glm(rel_bro ~ treat, family = binomial(link = "logit"), data = .)
summary(res_logit_mod_arab)

#

res_logit_trac_turk <- data_exp  %>%
  filter(lifestyle == "traditional conservative" & eth_self == "Turkish") %>%
  glm(rel_bro ~ treat, family = binomial(link = "logit"), data = .)
summary(res_logit_trac_turk)

res_logit_trac_kurd <- data_exp  %>%
  filter(lifestyle == "traditional conservative" & eth_self == "Kurdish") %>%
  glm(rel_bro ~ treat, family = binomial(link = "logit"), data = .)
summary(res_logit_trac_kurd)

res_logit_trac_arab <- data_exp  %>%
  filter(lifestyle == "traditional conservative" & eth_self == "Arab") %>%
  glm(rel_bro ~ treat, family = binomial(link = "logit"), data = .)
summary(res_logit_trac_arab)

#

res_logit_relc_turk <- data_exp  %>%
  filter(lifestyle == "religious conservative" & eth_self == "Turkish") %>%
  glm(rel_bro ~ treat, family = binomial(link = "logit"), data = .)
summary(res_logit_relc_turk)

res_logit_relc_kurd <- data_exp  %>%
  filter(lifestyle == "religious conservative" & eth_self == "Kurdish") %>%
  glm(rel_bro ~ treat, family = binomial(link = "logit"), data = .)
summary(res_logit_relc_kurd)

res_logit_relc_arab <- data_exp  %>%
  filter(lifestyle == "religious conservative" & eth_self == "Arab") %>%
  glm(rel_bro ~ treat, family = binomial(link = "logit"), data = .)
summary(res_logit_relc_arab)



## ETHNIC BROTHERHOOD (Soru 25) #######################

# full sample

# tables

table(data_exp$eth_bro)

twoway_abs <- table(data_exp$treat, data_exp$eth_bro)
twoway_abs

twoway_sha_row <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_sha_row

twoway_sha_col <- twoway_abs |>
  prop.table(margin = 2) |>
  round(digits = 2)
twoway_sha_col

# baseline model

res_logit_bas <- data_exp %>%
  glm(eth_bro ~ treat, family = binomial(link = "logit"), data = .)
summary(res_logit_bas)

# by lifestyle + ethnic self-identification

res_logit_mod_turk <- data_exp  %>%
  filter(lifestyle == "modern" & eth_self == "Turkish") %>%
  glm(eth_bro ~ treat, family = binomial(link = "logit"), data = .)
summary(res_logit_mod_turk)

res_logit_mod_kurd <- data_exp  %>%
  filter(lifestyle == "modern" & eth_self == "Kurdish") %>%
  glm(eth_bro ~ treat, family = binomial(link = "logit"), data = .)
summary(res_logit_mod_kurd)

res_logit_mod_arab <- data_exp  %>%
  filter(lifestyle == "modern" & eth_self == "Arab") %>%
  glm(eth_bro ~ treat, family = binomial(link = "logit"), data = .)
summary(res_logit_mod_arab)

#

res_logit_trac_turk <- data_exp  %>%
  filter(lifestyle == "traditional conservative" & eth_self == "Turkish") %>%
  glm(eth_bro ~ treat, family = binomial(link = "logit"), data = .)
summary(res_logit_trac_turk)

res_logit_trac_kurd <- data_exp  %>%
  filter(lifestyle == "traditional conservative" & eth_self == "Kurdish") %>%
  glm(eth_bro ~ treat, family = binomial(link = "logit"), data = .)
summary(res_logit_trac_kurd)

res_logit_trac_arab <- data_exp  %>%
  filter(lifestyle == "traditional conservative" & eth_self == "Arab") %>%
  glm(eth_bro ~ treat, family = binomial(link = "logit"), data = .)
summary(res_logit_trac_arab)

#

res_logit_relc_turk <- data_exp  %>%
  filter(lifestyle == "religious conservative" & eth_self == "Turkish") %>%
  glm(eth_bro ~ treat, family = binomial(link = "logit"), data = .)
summary(res_logit_relc_turk)

res_logit_relc_kurd <- data_exp  %>%
  filter(lifestyle == "religious conservative" & eth_self == "Kurdish") %>%
  glm(eth_bro ~ treat, family = binomial(link = "logit"), data = .)
summary(res_logit_relc_kurd)

res_logit_relc_arab <- data_exp  %>%
  filter(lifestyle == "religious conservative" & eth_self == "Arab") %>%
  glm(eth_bro ~ treat, family = binomial(link = "logit"), data = .)
summary(res_logit_relc_arab)


## LONG-TERM STAY #######################

# full sample

# tables

table(data_exp$longterm_stay)

twoway_abs <- table(data_exp$treat, data_exp$longterm_stay)
twoway_abs

twoway_sha_row <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_sha_row

twoway_sha_col <- twoway_abs |>
  prop.table(margin = 2) |>
  round(digits = 2)
twoway_sha_col

# baseline model

res_logit_bas <- data_exp %>%
  glm(longterm_stay ~ treat, family = binomial(link = "logit"), data = .)
summary(res_logit_bas)

# by lifestyle + ethnic self-identification

res_logit_mod_turk <- data_exp  %>%
  filter(lifestyle == "modern" & eth_self == "Turkish") %>%
  glm(longterm_stay ~ treat, family = binomial(link = "logit"), data = .)
summary(res_logit_mod_turk)

res_logit_mod_kurd <- data_exp  %>%
  filter(lifestyle == "modern" & eth_self == "Kurdish") %>%
  glm(longterm_stay ~ treat, family = binomial(link = "logit"), data = .)
summary(res_logit_mod_kurd)

res_logit_mod_arab <- data_exp  %>%
  filter(lifestyle == "modern" & eth_self == "Arab") %>%
  glm(longterm_stay ~ treat, family = binomial(link = "logit"), data = .)
summary(res_logit_mod_arab)

#

res_logit_trac_turk <- data_exp  %>%
  filter(lifestyle == "traditional conservative" & eth_self == "Turkish") %>%
  glm(longterm_stay ~ treat, family = binomial(link = "logit"), data = .)
summary(res_logit_trac_turk)

res_logit_trac_kurd <- data_exp  %>%
  filter(lifestyle == "traditional conservative" & eth_self == "Kurdish") %>%
  glm(longterm_stay ~ treat, family = binomial(link = "logit"), data = .)
summary(res_logit_trac_kurd)

res_logit_trac_arab <- data_exp  %>%
  filter(lifestyle == "traditional conservative" & eth_self == "Arab") %>%
  glm(longterm_stay ~ treat, family = binomial(link = "logit"), data = .)
summary(res_logit_trac_arab)

#

res_logit_relc_turk <- data_exp  %>%
  filter(lifestyle == "religious conservative" & eth_self == "Turkish") %>%
  glm(longterm_stay ~ treat, family = binomial(link = "logit"), data = .)
summary(res_logit_relc_turk)

res_logit_relc_kurd <- data_exp  %>%
  filter(lifestyle == "religious conservative" & eth_self == "Kurdish") %>%
  glm(longterm_stay ~ treat, family = binomial(link = "logit"), data = .)
summary(res_logit_relc_kurd)

res_logit_relc_arab <- data_exp  %>%
  filter(lifestyle == "religious conservative" & eth_self == "Arab") %>%
  glm(longterm_stay ~ treat, family = binomial(link = "logit"), data = .)
summary(res_logit_relc_arab)





## estimate mann-whitney u test --------------------------------

res_mwu <- data_exp %>%
  wilcox.test(partsoc_today ~ treat, data = .
    ,exact = FALSE, correct = FALSE, conf.int = FALSE)
res_mwu
