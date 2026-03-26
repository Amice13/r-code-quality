### ------------------------------------------------------------
# Thesis Survey Experiment
# 04 Randomisation Checks
# Friedrich Püttmann
# October 2023
### ------------------------------------------------------------

## load packages -----------------------------------------------

library(tidyverse)
library(rstatix)

## load dataset ------------------------------------------------

data_exp <- read_rds("02_data/03_data_clean.rds")

## check randomization via individual variables ----------------

table(data_exp$treat)

# gender

summary(data_exp$gender) 
total_count <- sum(!is.na(data_exp$gender))
print(total_count)

table(data_exp$gender) 
table_gender <- table(data_exp$gender)       
percentage_table <- prop.table(table_gender) * 100    
percentage_table

twoway_abs <- table(data_exp$treat, data_exp$gender)
twoway_abs

twoway_sha <- twoway_abs |>
    prop.table(margin = 1) |>
    round(digits = 2)
twoway_sha

res_chisqt <- data_exp |>
  select(treat, gender) |>
  table() |>
  chisq.test()
res_chisqt

# age

summary(data_exp$age) 
total_count <- sum(!is.na(data_exp$age))
print(total_count)

summary(data_exp$age)

group_av <- data_exp |>
  group_by(treat) |>
  summarize(mean_value = mean(age, na.rm = TRUE))
group_av

res_ttest <- data_exp |>
  t_test(age ~ treat) |>
  add_significance()
res_ttest

# lifestyle

summary(data_exp$lifestyle) 
total_count <- sum(!is.na(data_exp$lifestyle))
print(total_count)

table(data_exp$lifestyle) 
table_lifestyle <- table(data_exp$lifestyle)       
percentage_table <- prop.table(table_lifestyle) * 100    
percentage_table


twoway_abs <- table(data_exp$treat, data_exp$lifestyle)
twoway_abs

twoway_sha <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_sha

res_chisqt <- data_exp |>
  select(lifestyle, treat) |>
  table() |>
  chisq.test()
res_chisqt

##

twoway_abs <- table(data_exp$lifestyle, data_exp$treat)
twoway_abs

twoway_sha <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_sha

res_chisqt <- data_exp |>
  select(treat, lifestyle) |>
  table() |>
  chisq.test()
res_chisqt

# vote today (grouped)

summary(data_exp$vote_today_group) 
total_count <- sum(!is.na(data_exp$vote_today_group))
print(total_count)

 
table(data_exp$vote_today_group)
vote_table <- table(data_exp$vote_today_group)
vote_percentage <- prop.table(vote_table) * 100
print(vote_percentage)


twoway_abs <- table(data_exp$vote_today_group, data_exp$treat)
twoway_abs

twoway_sha <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_sha

res_chisqt <- data_exp |>
  select(vote_today_group, treat) |>
  table() |>
  chisq.test()
res_chisqt

## 

twoway_abs <- table(data_exp$treat, data_exp$vote_today_group)
twoway_abs

twoway_sha <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_sha

res_chisqt <- data_exp |>
  select(treat, vote_today_group) |>
  table() |>
  chisq.test()
res_chisqt

# education (grouped)

summary(data_exp$edu_group) 
total_count <- sum(!is.na(data_exp$edu_group))
print(total_count)

table(data_exp$edu_group)
edu_table <- table(data_exp$edu_group)
edu_percentage <- prop.table(edu_table) * 100
edu_percentage


twoway_abs <- table(data_exp$treat, data_exp$edu_group)
twoway_abs

twoway_sha <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_sha

res_chisqt <- data_exp |>
  select(edu_group, treat) |>
  table() |>
  chisq.test()
res_chisqt

# job (grouped)

summary(data_exp$job_group) 
total_count <- sum(!is.na(data_exp$job_group))
print(total_count)

table(data_exp$job_group)
job_group_table <- table(data_exp$job_group)
job_group_percent <- prop.table(job_group_table) * 100
job_group_percent


twoway_abs <- table(data_exp$treat, data_exp$job_group)
twoway_abs

twoway_sha <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_sha

res_chisqt <- data_exp |>
  select(job_group, treat) |>
  table() |>
  chisq.test()
res_chisqt

# syrian presence

summary(data_exp$syr_presence) 
total_count <- sum(!is.na(data_exp$syr_presence))
print(total_count)

table(data_exp$syr_presence) 
table_syr_presence <- table(data_exp$syr_presence)       
percentage_table <- prop.table(table_syr_presence) * 100    
percentage_table

twoway_abs <- table(data_exp$treat, data_exp$syr_presence)
twoway_abs

twoway_sha <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_sha

res_chisqt <- data_exp |>
  select(syr_presence, treat) |>
  table() |>
  chisq.test()
res_chisqt

# syrian encounter

summary(data_exp$syr_encounter) 
total_count <- sum(!is.na(data_exp$syr_encounter))
print(total_count)

table(data_exp$syr_encounter) 
table_syr_encounter <- table(data_exp$syr_encounter)       
percentage_table <- prop.table(table_syr_encounter) * 100    
percentage_table

twoway_abs <- table(data_exp$treat, data_exp$syr_encounter)
twoway_abs

twoway_sha <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_sha

res_chisqt <- data_exp |>
  select(syr_encounter, treat) |>
  table() |>
  chisq.test()
res_chisqt

# syrian friend

summary(data_exp$syr_friend) 
total_count <- sum(!is.na(data_exp$syr_friend))
print(total_count)

table(data_exp$syr_friend) 
table_syr_friend <- table(data_exp$syr_friend)       
percentage_table <- prop.table(table_syr_friend) * 100    
percentage_table

twoway_abs <- table(data_exp$treat, data_exp$syr_friend)
twoway_abs

twoway_sha <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_sha

res_chisqt <- data_exp |>
  select(syr_friend, treat) |>
  table() |>
  chisq.test()
res_chisqt

# ethnic self-identification

summary(data_exp$eth_self) 
total_count <- sum(!is.na(data_exp$eth_self))
print(total_count)

table(data_exp$eth_self)
table_exp <- table(data_exp$eth_self)
percentage_table <- prop.table(table_exp) * 100
percentage_table

twoway_abs <- table(data_exp$treat, data_exp$eth_self)
twoway_abs

twoway_sha <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_sha

res_chisqt <- data_exp |>
  select(eth_self, treat) |>
  table() |>
  chisq.test()
res_chisqt

# language

summary(data_exp$language) 
total_count <- sum(!is.na(data_exp$language))
print(total_count)

table(data_exp$language)
table_language <- table(data_exp$language)
percent_language <- prop.table(table_language) * 100
percent_language


twoway_abs <- table(data_exp$treat, data_exp$language)
twoway_abs

twoway_sha <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_sha

res_chisqt <- data_exp |>
  select(treat, language) |>
  table() |>
  chisq.test()
res_chisqt

# religious practice

summary(data_exp$rel_prac) 
total_count <- sum(!is.na(data_exp$rel_prac))
print(total_count)

table(data_exp$rel_prac)
table_rel_prac <- table(data_exp$rel_prac)      
percentage_table <- prop.table(table_rel_prac) * 100   
percentage_table

twoway_abs <- table(data_exp$treat, data_exp$rel_prac)
twoway_abs

twoway_sha <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_sha

res_chisqt <- data_exp |>
  select(rel_prac, treat) |>
  table() |>
  chisq.test()
res_chisqt

# head covering

summary(data_exp$head_cov) 
total_count <- sum(!is.na(data_exp$head_cov))
print(total_count)

table(data_exp$head_cov) 
table_head_cov <- table(data_exp$head_cov)       
percentage_table <- prop.table(table_head_cov) * 100    
percentage_table

twoway_abs <- table(data_exp$treat, data_exp$head_cov)
twoway_abs

twoway_sha <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_sha

res_chisqt <- data_exp |>
  select(head_cov, treat) |>
  table() |>
  chisq.test()
res_chisqt

# sect

summary(data_exp$sect) 
total_count <- sum(!is.na(data_exp$sect))
print(total_count)

table(data_exp$sect) 
table_sect <- table(data_exp$sect)       
percentage_table <- prop.table(table_sect) * 100    
percentage_table

twoway_abs <- table(data_exp$treat, data_exp$sect)
twoway_abs

twoway_sha <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_sha

res_chisqt <- data_exp |>
  select(sect, treat) |>
  table() |>
  chisq.test()
res_chisqt

# income (grouping 1)

summary(data_exp$income1) 
total_count <- sum(!is.na(data_exp$income1))
print(total_count)

table(data_exp$income1)
income_table <- table(data_exp$income1)
income_percentage <- prop.table(income_table) * 100
income_percentage

twoway_abs <- table(data_exp$treat, data_exp$income1)
twoway_abs

twoway_sha <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_sha

res_chisqt <- data_exp |>
  select(income1, treat) |>
  table() |>
  chisq.test()
res_chisqt

## Clear environment

rm(list = ls())
