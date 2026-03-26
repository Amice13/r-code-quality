#################################
## Replication Code for Carolyn Barnett, "Women's Rights 
## and Misperceived Gender Norms Under Authoritarianism"
## 05/31/2023

# Run using R version 4.2.0 (2022-04-22) 
# in RStudio version 2023.03.0+386

library(tidyverse) # version 1.3.1
library(readstata13) # version 0.10.0
library(RColorBrewer) # version 1.1-3
library(broom) # version 0.8.0
library(patchwork) # version 1.1.1
library(modelsummary) # version 1.3.0
library(stargazer) # version 5.2.3
library(xtable) # version 1.8-4
library(pollster) # version 0.1.3
library(weights) # version 1.0.4

## Load data ---------------------------------

# Original surveys - data included in replication files
d <- readRDS("tel_data_cps_rep.rds")
o <- readRDS("online_data_cps_rep.rds")

# Comparison data for Appendix Tables 3 and 16
# Data files must be obtained from original open source websites

# Arab Barometer Wave 5 (used in Appendix Tables 3 and 16)
# Data must be downloaded from Arab Barometer website
ab5 <- readstata13::read.dta13("ABV_Crossectional_Data_Release_ENG.dta")

# Arab Barometer Wave 6 (used in Appendix Table 16)
# Data must be downloaded from Arab Barometer website
# Note: Wave 6 Part 2 contained no gender-related questions
ab6_1 <- readstata13::read.dta13("Arab_Barometer_Wave_6_Part_1_ENG_RELEASE.dta")
ab6_3 <- readstata13::read.dta13("Arab_Barometer_Wave_6_Part_3_ENG_RELEASE.dta")

# World Values Survey - Morocco Wave 7 (used in Appendix Table 16)
# Data must be downloaded from World Values Survey website
mor_wvs <- readstata13::read.dta13("WVS_Wave_7_Morocco_Stata_v3.0.dta")


## Common plot parameters -------------------------------------------

theme_cb <- function () { 
  theme_bw(base_size=12, base_family="") %+replace% 
    theme(
      panel.background=element_blank(),
      legend.background=element_rect(fill="transparent", colour=NA),
      legend.key=element_rect(fill="transparent", colour=NA),
      axis.text.x=element_text(size=10),
      axis.title.x=element_text(size=12,
                                margin = margin(t = 10, r = 0, b = 0, l = 0)),
      axis.text.y=element_text(size=10), 
      axis.title.y=element_text(size=12, angle=90,
                                margin = margin(t = 0, r = 10, b = 0, l = 0)),
      plot.title=element_text(size=18, color="black",
                              margin = margin(t = 0, r = 0, b = 10, l = 0)),
      plot.subtitle=element_text(size=14)
    )
}

## Custom Functions -------------------------------------------

prop_se <- function(prop, N){
  1.96 * sqrt((prop * (1 - prop)) / N)
}

likert_se <- function(responses, N){
  1.96 * sqrt(var(responses, na.rm = T) / N)
}

## Common Input for Tables ---------------------

# Coefficient Map
cm_gaps <- c(
  "pa_mean_2" = "PA Score",
  "pa_mean" = "PA Score",
  "female" = "Female",
  "male" = "Male",
  "factor(educat3)2" = "Prep./Sec. Ed.",
  "factor(educat3)3" = "Higher Ed.",
  "factor(agecat5)2" = "25-34",
  "factor(agecat5)3" = "35-44",
  "factor(agecat5)4" = "45-54",
  "factor(agecat5)5" = "55+",
  "factor(agecat)2" = "25-34",
  "factor(agecat)3" = "35-44",
  "factor(agecat)4" = "45-54",
  "factor(agecat)5" = "55+",
  "quran" = "Qur'an",
  "religiosity" = "Religiosity",
  "urban" = "Urban",
  "income_perception" = "Subj. SES",
  "married" = "Married" ,
  "si_worry" = "Sensitivity",
  "interviewer_female" = "Enum. Fem.",
  "male:interviewer_female" = "Enum. Fem. * Male"
)

# Global option for tables: tidymodels: broom 
options(modelsummary_get = "broom")

# Goodness-of-Fit Maps
gm_lm <- c(
  "nobs", 
  "r.squared", 
  "adj.r.squared",
  "statistic"
)

gm_log <- c(
  "nobs", 
  "logLik", 
  "AIC"
)

## Figure 1: Personal Attitudes vs. Perceived Norms (Telephone Survey) ------

pd <- d %>%
  # dichotomize into more vs. less progressive responses
  mutate(across(.cols = c(men_jobs_pa, women_auth_pa, housework_pa,
                          men_last_pa, underage_pa, inheritance_pa,
                          men_jobs_pn, women_auth_pn, housework_pn,
                          men_last_pn, underage_pn, inheritance_pn),
                ~case_when(
                  .x %in% c(2,3) ~ 1,
                  .x %in% c(0,1) ~ 0,
                  TRUE ~ NA_real_
                ))) %>%
  # for each variable, calculate the weighted mean of progressive responses
  dplyr::summarize(across(.cols = c(men_jobs_pa, women_auth_pa, housework_pa,
                                    men_last_pa, underage_pa, inheritance_pa,
                                    men_jobs_pn, women_auth_pn, housework_pn,
                                    men_last_pn, underage_pn, inheritance_pn),
                          ~weighted.mean(.x, w = wt, na.rm = T),
                          .names = "mean_{.col}")) %>%
  # pivot
  pivot_longer(cols = everything()) %>%
  # calculate margin of error of proportion, lower and upper CI
  mutate(me = prop_se(prop = value, N = 1302), 
         lower = value - me,
         upper = value + me) %>%
  # prep for plotting
  mutate(Response = c(rep("Attitudes", 6), 
                      rep("Perceived Norms", 6)),
         Question = rep(c("Men Jobs", "Women Auth.", "Housework", 
                          "Last Word", "Underage", "Inheritance"), 2)) %>%
  arrange(value) 

ggplot(pd, aes(x = reorder(Question, value), 
               y = value,
               group = Response,
               fill = Response,
               color = Response)) + 
  geom_col(width = 0.5,
           position = position_dodge(0.6)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(width = 0.6),
                width = 0.2,
                color = "black") +
  scale_y_continuous(limits = c(0,1),
                     labels = scales::percent_format()) +
  labs(x = "Item", 
       y = "Proportion with Egalitarian View") +
  scale_color_grey(start = 0.3, end = 0.7) +
  scale_fill_grey(start = 0.3, end = 0.7) +
  theme_cb() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 10))

ggsave("pa_vs_pn_all_items_weighted.png",
       width = 6.25,
       height = 4.5)

# Save attitudes for inclusion in online survey graph
tel_att_prop <- pd %>%
  filter(Response == "Attitudes") %>%
  mutate(Survey = "Telephone")

# Percentage gaps reported in text (PA minus PN)
# Inheritance
pd$value[2] - pd$value[1] # 0.018

# Men jobs
pd$value[4] - pd$value[3] # 0.008

# Last word
pd$value[6] - pd$value[5] # 0.065

# Underage
pd$value[10] - pd$value[8] # 0.081

# Housework
pd$value[11] - pd$value[7] # 0.209

# Women auth
pd$value[12] - pd$value[9] # 0.118

## Figure 2: Personal Attitudes vs. Perceived Norms (Online Survey) ------

pd <- o %>%
  # dichotomize into more vs. less progressive responses
  mutate(across(.cols = c(pa_men_jobs, pa_women_power,
                          pa_housework, pa_final_say,
                          pn_men_jobs, pn_women_power,
                          pn_housework, pn_final_say),
                ~case_when(
                  .x %in% c(2,3) ~ 1,
                  .x %in% c(0,1) ~ 0,
                  TRUE ~ NA_real_
                ))) %>%
  # for each variable, calculate the weighted mean of progressive responses
  dplyr::summarize(across(.cols = c(pa_men_jobs, pa_women_power,
                                    pa_housework, pa_final_say,
                                    pn_men_jobs, pn_women_power,
                                    pn_housework, pn_final_say),
                          ~weighted.mean(.x, w = wt, na.rm = T),
                          .names = "mean_{.col}")) %>%
  # pivot
  pivot_longer(cols = everything()) %>%
  # calculate margin of error of proportion, lower and upper CI
  mutate(me = prop_se(prop = value, N = 1291), 
         lower = value - me,
         upper = value + me) %>%
  # prep for plotting
  mutate(Response = c(rep("Attitudes",4),
                      rep("Perceived Norms", 4)),
         Question = rep(c("Men Jobs", "Women Authority",
                          "Housework", "Last Word"), 2)) %>%
  arrange(value)

# Merge in telephone actual means
pd <- pd %>%
  mutate(Survey = "Online") %>%
  bind_rows(tel_att_prop) %>%
  filter(!(Question %in% c("Inheritance", "Underage")))

pd$Question[pd$Question == "Women Auth."] <- "Women Authority"

pd <- pd %>%
  mutate(Response = paste(Response, Survey, sep = " "))

pd$Response <- fct_relevel(pd$Response, "Perceived Norms Online", 
                           after = 1)

ggplot(pd, aes(x = reorder(Question, value), y = value,
               group = Response,
               fill = Response,
               color = Response)) +
  geom_col(width = 0.5,
           position = position_dodge(0.6)) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.6),
                width = 0.2,
                color = "black") +
  scale_y_continuous(limits = c(0,1),
                     labels = scales::percent_format()) +
  labs(x = "Item", y = "Proportion with Egalitarian View") +
  scale_color_grey(start = 0.3, end = 0.7) +
  scale_fill_grey(start = 0.3, end = 0.7) +
  theme_cb() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 10))

ggsave("pa_vs_pn_all_items_weighted_online.png",
       width = 6.3,
       height = 3.8)


# Percentage gaps reported in text
# Men jobs
pd$value[3] - pd$value[1] # 0.16

# Last word
pd$value[5] - pd$value[2] # 0.27

# Housework
pd$value[7] - pd$value[4] # 0.356

# Women auth
pd$value[8] - pd$value[6] # 0.210


## In Text: T-tests for difference in proportions --------------------

# NOTE: Table ommitted, but results reported in text
# Results calculated only for telephone survey; 
# visible in Figure 2 above that all differences in the online
# survey are significant at p < 0.001

# Telephone
td <- d %>%
  # dichotomize into more vs. less progressive responses
  mutate(across(.cols = c(men_jobs_pa, women_auth_pa, housework_pa,
                          men_last_pa, underage_pa, inheritance_pa,
                          men_jobs_pn, women_auth_pn, housework_pn,
                          men_last_pn, underage_pn, inheritance_pn),
                ~case_when(
                  .x %in% c(2,3) ~ 1,
                  .x %in% c(0,1) ~ 0,
                  TRUE ~ NA_real_
                )))

summary(td$men_jobs_pa)
summary(d$men_jobs_pa)

set.seed(20578) # set seed for bootstrapping
# weighted t-tests with bootstrapped standard errors
t1 <- wtd.t.test(td$inheritance_pa, td$inheritance_pn,
                 weight = td$wt, bootse = T, drops = F)
t2 <- wtd.t.test(td$men_jobs_pa, td$men_jobs_pn,
                 weight = td$wt, bootse = T, drops = F)
t3 <- wtd.t.test(td$men_last_pa, td$men_last_pn,
                 weight = td$wt, bootse = T, drops = F)
t4 <- wtd.t.test(td$underage_pa, td$underage_pn,
                 weight = td$wt, bootse = T, drops = F)
t5 <- wtd.t.test(td$housework_pa, td$housework_pn,
                 weight = td$wt, bootse = T, drops = F)
t6 <- wtd.t.test(td$women_auth_pa, td$women_auth_pn,
                 weight = td$wt, bootse = T, drops = F)

# table of results
t_table <- data.frame(matrix(nrow = 6, ncol = 4))
names(t_table) <- c("Question", "PA", "PN", "p-value")
t_table$Question <- c("Inheritance", "Men Jobs", "Last Word",
                      "Underage", "Housework", "Women Auth.")
t_table$PA <- c(t1$additional[2], t2$additional[2], t3$additional[2],
                t4$additional[2], t5$additional[2], t6$additional[2])
t_table$PN <- c(t1$additional[3], t2$additional[3], t3$additional[3],
                t4$additional[3], t5$additional[3], t6$additional[3])
t_table$`p-value` <- c(t1$coefficients[3], t2$coefficients[3], t3$coefficients[3],
                       t4$coefficients[3], t5$coefficients[3], t6$coefficients[3])

t_table <- t_table %>%
  mutate(diff = PA - PN,
         p_val = round(`p-value`, 2))

t_table

## Figure 3: Predicted Probability of Misperceiving --------------------

# Run models w/ binary DV
pp_model <- as.formula(overestimate ~ 
                         factor(agecat5) + urban + 
                         male + factor(educat3) + 
                         income_perception + 
                         quran + married)

mmis_t_bin <- glm(pp_model, data = d, 
                  weights = wt,
                  family = "binomial") # warning due to weights

pp_model <- as.formula(overestimate ~ 
                         factor(agecat5) + urban + 
                         male + factor(educat3) + 
                         income_perception + 
                         religiosity + married)

mmis_o_bin <- glm(pp_model, data = o, 
                  weights = wt,
                  family = "binomial")

# Predicted probabilities at mean values, rounded
# Telephone
newdata_t <- with(d, data.frame(male = c(0,1),
                              agecat5 = 3,
                              urban = 1,
                              educat3 = 2,
                              income_perception = 2,
                              quran = 3,
                              married = 0
                              ))

pred_t <- predict(mmis_t_bin, 
                  newdata_t, 
                  type="response",
                  se.fit = T)

# Online
newdata_o <- with(o, data.frame(male = c(0,1),
                                agecat5 = 3,
                                urban = 1,
                                educat3 = 2,
                                income_perception = 2,
                                religiosity = 2,
                                married = 0
))

pred_o <- predict(mmis_o_bin, 
                  newdata_o, 
                  type="response",
                  se.fit = T)

# Plot results
ests <- c(pred_t[[1]], pred_o[[1]])
ses <- c(pred_t[[2]], pred_o[[2]])

pd <- data.frame(cbind(ests, ses))

names(pd) <- c("Estimate", "SE")
pd <- pd %>%
  mutate(
    Gender = rep(c("Female", "Male"), 2),
    Survey = c(rep("Telephone", 2),
               rep("Online", 2)),
    lower = Estimate - 1.96 * SE,
    upper = Estimate + 1.96 * SE
  )


ggplot(pd, aes(x = Survey, y = Estimate,
               group = Gender,
               shape = Gender)) +
  geom_point(position = position_dodge(width = 0.6),
             size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(width = 0.6),
                width = 0.2,
                color = "black") +
  geom_hline(yintercept = 0.5, linetype = 3, color = "black") +
  scale_y_continuous(limits = c(0.25, 1)) +
  labs(y = "Predicted Probability") +
  theme_cb()

ggsave("predicted_probs.png",
       width = 4.8,
       height = 3.8)

pd # to view estimates cited in text

## Appendix Table 1: Telephone Survey Quotas ---------------------

tq <- readxl::read_xlsx("telephone_survey_quotas.xlsx")

print(xtable(tq), 
      include.rownames = F)

## Appendix Table 2: Online Survey Regional Distribution ------------

regions <- data.frame(table(o$region_name))
names(regions) <- c("Region", "Count")

regions$Percent <- round(regions$Count / 1291, digits = 2)

print(xtable(regions), include.rownames = F)


## Appendix Table 3: Survey Demographics ---------------------------

# Establish data frame for storing data 

dem_tab <- data.frame(matrix(nrow = 27, ncol = 5))
names(dem_tab) <- c("Attribute",
                    "Category",
                    "AB W5", 
                    "Telephone",
                    "Online")

dem_tab$Attribute <- c("Gender", "",
                       "",
                       "Age", "","","","",
                       "",
                       "Status", "","","","",
                       "",
                       "Work Sector", "if Employed", "",
                       "",
                       "Economic", "Security", "", "",
                       "",
                       "Marital", "Status", "")

dem_tab$Category <- c("Male", "Female", # 1-2
                      "",
                      "18-24", "25-34","35-44","45-54","55+", #4-8
                      "",
                      "Employed", 
                      "Housewife","Student",
                      "Unemployed", "Retired", # 10-14
                      "",
                      "Public", "Private", 
                      "Other / NA", # 16-18
                      "",
                      "Good and Can Save", 
                      "No Difficulties", 
                      "Some Difficulties", 
                      "Major Difficulties", #20-23
                      "",
                      "Single Never Married", 
                      "Currently Married", 
                      "Other" ) # 25-27

## Telephone survey
n_tel <- 1302

# Gender - all women
prop.table(table(d$gender))
dem_tab$Telephone[1:2] <- c(0.49, 0.51)

# Age
prop.table(table(d$agecat5))

dem_tab$Telephone[4:8] <- prop.table(table(d$agecat5))

# Employed
prop.table(table(d$emp_status))

dem_tab$Telephone[10] <- (table(d$emp_status)[1] + table(d$emp_status)[2]) / n_tel # employed
dem_tab$Telephone[11] <- table(d$emp_status)[3] / n_tel # housewife
dem_tab$Telephone[12] <- table(d$emp_status)[4] / n_tel # student
dem_tab$Telephone[13] <- table(d$emp_status)[5] / n_tel # unemployed
dem_tab$Telephone[14] <- table(d$emp_status)[6] / n_tel # retired

# Employment sector
prop.table(table(d$sector))

dem_tab$Telephone[16:17] <- prop.table(table(d$sector))[1:2]
dem_tab$Telephone[18] <- prop.table(table(d$sector))[3] +
  prop.table(table(d$sector))[4]

# Economic security
prop.table(table(d$income_perception))

dem_tab$Telephone[20:23] <- rev(prop.table(table(d$income_perception)))

# Marital status
prop.table(table(d$marital_status))

# single never married
dem_tab$Telephone[25] <- prop.table(table(d$marital_status))[1] 
# married
dem_tab$Telephone[26] <- prop.table(table(d$marital_status))[2] 
# other
dem_tab$Telephone[27] <- 1 - (dem_tab$Telephone[25] + 
                                dem_tab$Telephone[26])

## Online survey
n_online <- 1291

# Gender - all women
prop.table(table(o$gender))
dem_tab$Online[1:2] <- c(0.500, 0.500)

# Age
prop.table(table(o$agecat5))

dem_tab$Online[4:8] <- prop.table(table(o$agecat5))

# Employed
# note - response IDs are different from telephone survey
prop.table(table(o$work_status))

dem_tab$Online[10] <- (table(o$work_status)[1] + table(o$work_status)[2]) / n_online # employed
dem_tab$Online[11] <- table(o$work_status)[4] / n_online # housewife
dem_tab$Online[12] <- table(o$work_status)[3] / n_online # student
dem_tab$Online[13] <- table(o$work_status)[6] / n_online # unemployed
dem_tab$Online[14] <- table(o$work_status)[5] / n_online # retired

# Economic security
prop.table(table(o$income_perception))

dem_tab$Online[20:23] <- rev(prop.table(table(o$income_perception)))

# Marital status
prop.table(table(o$marital_status))

# single never married
dem_tab$Online[25] <- prop.table(table(o$marital_status))[1] 
# married
dem_tab$Online[26] <- prop.table(table(o$marital_status))[2] 
# other
dem_tab$Online[27] <- 1 - (dem_tab$Online[25] + 
                             dem_tab$Online[26])

# Employment sector - NOT ASKED
dem_tab$Online[16:17] <- c("NA", "NA")


## AB5 Morocco
n_ab5 <- 2400

# Select Morocco 
mor <- ab5 %>% filter(country == "Morocco")
# Note: all stats are from *unweighted* sample

# Gender 
prop.table(table(mor$Q1002)) 

dem_tab$`AB W5`[1:2] <- c(0.5, 0.5)

# Age group 
# age - need to remove 9999 values and combine
summary(mor$Q1001) # what year were you born (alread recoded to age values)
mor <- mor %>% 
  mutate(age = case_when(
    !is.na(Q1001) & Q1001 != 99999 ~ as.double(Q1001),
    TRUE ~ NA_real_
  ))
summary(mor$age)

# age_group
mor$age_group <- case_when(
  mor$age %in% c(17:24) ~ 1,
  mor$age %in% c(25:34) ~ 2,
  mor$age %in% c(35:44) ~ 3,
  mor$age %in% c(45:54) ~ 4, 
  mor$age %in% c(55:95) ~ 5, 
  TRUE ~ NA_real_
)
prop.table(table(mor$age_group))

dem_tab$`AB W5`[4:8] <- prop.table(table(mor$age_group))

# Employed
prop.table(table(mor$Q1005))

dem_tab$`AB W5`[10] <- (309 + 394) / n_ab5 # employed
dem_tab$`AB W5`[11] <- 679 / n_ab5 # housewife 
dem_tab$`AB W5`[12] <- 308 / n_ab5 # student
dem_tab$`AB W5`[13] <- 422 / n_ab5 # unemployed
dem_tab$`AB W5`[14] <- 132 / n_ab5  # retired

# Work sector 
prop.table(table(mor$Q1006A))

dem_tab$`AB W5`[16] <- prop.table(table(mor$Q1006A))[1] # public
dem_tab$`AB W5`[17] <- prop.table(table(mor$Q1006A))[2] # private
dem_tab$`AB W5`[18] <- prop.table(table(mor$Q1006A))[3] # other

# Economic security
prop.table(table(mor$Q1016))

dem_tab$`AB W5`[20:23] <- prop.table(table(mor$Q1016))[1:4] 

# Marital status 
prop.table(table(mor$Q1010))

# single never married
dem_tab$`AB W5`[25] <- prop.table(table(mor$Q1010))[1] 
# married
dem_tab$`AB W5`[26] <- prop.table(table(mor$Q1010))[4] 
# other
dem_tab$`AB W5`[27] <- 1 - (dem_tab$`AB W5`[25] + 
                              dem_tab$`AB W5`[26]) 

## Create Table
str(dem_tab)
dem_tab$Online <- as.numeric(dem_tab$Online)

## Round all numbers to 3 digits
dt <- dem_tab %>%
  purrr::modify_if(~is.numeric(.), ~round(., 3))

## Add "N" for each sample in a new row
dt <- data.frame(rbind(dt,
                       c("N", "", 
                         n_ab5, n_tel, n_online)))

# Print
print(xtable::xtable(dt), include.rownames = F)


## Appendix Table 4: Summary of Interviews Conducted ------------

# Table created directly in Latex with relevant information

## Appendix Table 5: Focus Groups ----------------------

# Table created directly in Latex with relevant information

## Appendix Table 6: Missing Data -----------------

summary(d$pa_mean_2) # 3 NAs (0.2 %)
summary(d$pn_mean_2) # 61 NAs (4.7 %)

# Missing values do not exceed 9% for any individual item
summary(d$men_jobs_pa) # 12 NA (0.9 %)
summary(d$men_jobs_pn) # 70 NA (5.4 %)

summary(d$women_auth_pa) # 31 NA (2.4 %)
summary(d$women_auth_pn) # 114 NA (8.8 %)

summary(d$housework_pa) # 7 NA (0.5 %)
summary(d$housework_pn) # 87 NA (6.7 %)

summary(d$men_last_pa) # 14 NA (1.1 %)
summary(d$men_last_pn) # 90 NA (6.9 %)

summary(d$underage_pa) # 36 NA (2.8 %)
summary(d$underage_pn) # 107 NA (8.2 %)

summary(d$inheritance_pa) # 50 NA (3.8 %)
summary(d$inheritance_pn) # 101 NA (7.8 %)

# Calculating NA responses per respondent
na_counts <- d %>%
  mutate(across(.cols = c(men_jobs_pa, women_auth_pa, housework_pa,
                          men_last_pa, underage_pa, inheritance_pa,
                          men_jobs_pn, women_auth_pn, housework_pn,
                          men_last_pn, underage_pn, inheritance_pn),
                ~ifelse(is.na(.x), 1, 0))) %>%
  rowwise() %>%
  mutate(na_total_pa = sum(men_jobs_pa, women_auth_pa, housework_pa,
                           men_last_pa, underage_pa, inheritance_pa),
         na_total_pn = sum(men_jobs_pn, women_auth_pn, housework_pn,
                           men_last_pn, underage_pn, inheritance_pn),
         na_any_pa = ifelse(na_total_pa >= 1, 1, 0),
         na_any_pn = ifelse(na_total_pn >= 1, 1, 0),
         na_out_pa = ifelse(na_total_pa >= 3, 1, 0),
         na_out_pn = ifelse(na_total_pn >= 3, 1, 0)) %>%
  select(na_total_pa, na_total_pn, 
         na_any_pa, na_any_pn, 
         na_out_pa, na_out_pn)

# Count of NA responses on PA questions by respondent
table(na_counts$na_total_pa)
#   0    1    2    3    4 
#1179  100   20    2    1   # 3 omitted total for having >= 3 NAs

# of those included, 120 were missing 1 or 2
100 / 1302 # 7.6 %
20 / 1302 # 1.5 %

# Count of NA responses on PN questions by respondent
table(na_counts$na_total_pn)
#0   1   2   3   4   5   6 
#990 186  65  25  14  10  12 # 61 omitted total

# of those included, 251 were missing one or two
186 / 1302 # 14.3 %
65 / 1302 # 5.0 %

## Create table for appendix
na_table <- d %>%
  select(pa_mean_2, pn_mean_2,
         men_jobs_pa, women_auth_pa, housework_pa,
         men_last_pa, underage_pa, inheritance_pa,
         men_jobs_pn, women_auth_pn, housework_pn,
         men_last_pn, underage_pn, inheritance_pn
  ) %>%
  dplyr::summarize(across(.cols = everything(),
                   ~sum(is.na(.x)))) %>%
  pivot_longer(cols = everything()) %>%
  mutate(
    Group = case_when(
      str_detect(name, "men_jobs_pa") ~ "Personal Attitudes",
      str_detect(name, "men_jobs_pn") ~ "Perceived Norms",
      str_detect(name, "pa_mean_2") ~ "PA Mean",
      str_detect(name, "pn_mean_2") ~ "PN Mean"
    ),
    Issue = case_when(
      str_detect(name, "men_jobs") ~ "Men Jobs",
      str_detect(name, "women_auth") ~ "Women Auth.",
      str_detect(name, "housework") ~ "Housework",
      str_detect(name, "men_last") ~ "Final Say",
      str_detect(name, "underage") ~ "Underage",
      str_detect(name, "inheritance") ~ "Inheritance"
    ),
    Percent = value / 1302
  ) %>%
  rename(Count = value) %>%
  select(Group, Issue, Count, Percent)

print(xtable(na_table), include.rownames = F)

## Appendix Figures 1-2: Personal Attitude vs. Perceived Norm Scores ------

# Telephone survey
ggplot(d, aes(x = pa_mean_2, 
              y = pn_mean_2)) +
  geom_jitter(width=0.25,
              color = "gray") +
  geom_smooth(method = "lm", color = "black") +
  geom_abline(slope = 1, intercept = c(0,0),
              linetype = 3) +
  labs(x = "Mean Personal Attitude (Telephone)",
       y = "Mean Perceived Norm") + 
  scale_y_continuous(limits = c(0,3),
                     labels = c(0:3)) +
  annotate(geom = "text", x = 0.5, y = 2.5,
           label = "Self More Conservative") +
  annotate(geom = "text", x = 2.5, y = 0.25,
           label = "Self More Progressive") +
  theme_cb() 

ggsave("scatter_telephone.png",
       width = 6,
       height = 4.25)

# Online survey
ggplot(o, aes(x = pa_mean, 
              y = pn_mean)) +
  geom_jitter(width=0.25, color = "gray") +
  geom_smooth(method = "lm", color = "black") +
  geom_abline(slope = 1, intercept = c(0,0),
              linetype = 3) +
  labs(x = "Mean Personal Attitude (Online)",
       y = "Mean Perceived Norm") + 
  scale_y_continuous(limits = c(0,3.1),
                     labels = c(0:3)) +
  annotate(geom = "text", x = 0.5, y = 2.5,
           label = "Self More Conservative") +
  annotate(geom = "text", x = 2.8, y = 0.1,
           label = "Self More Progressive") +
  theme_cb() 

ggsave("scatter_online.png",
       width = 6,
       height = 4.25)

## Appendix Figure 3: Distribution of Misperception Score -----------

mhist1 <- ggplot(d, aes(x = misperception_score)) +
  geom_histogram(bins = 15, color = "gray", fill = "white") +
  geom_vline(xintercept = 0, color = "black", linetype = 3) + 
  geom_vline(xintercept = -0.1336, color = "red", linetype = 2) + 
  labs(x = "", 
       y = "Count",
       title = "Telephone") +
  scale_x_continuous(limits = c(-1.8, 1.8)) + 
  scale_y_continuous(limits = c(0, 450)) +
  theme_cb() 
mhist1

mhist2 <- ggplot(o, aes(x = misperception_score)) +
  geom_histogram(bins = 15, color = "gray", fill = "white") +
  geom_vline(xintercept = 0, color = "black", linetype = 3) + 
  geom_vline(xintercept = -0.4154, color = "red", linetype = 2) + 
  labs(x = "Misperception Gap", 
       y = "Count",
       title = "Online") +
  scale_x_continuous(limits = c(-1.8, 1.8)) + 
  scale_y_continuous(limits = c(0, 300)) +
  theme_cb() 
mhist2

mhist1 / mhist2
ggsave("misperception_gap_hists.png",
       width = 5,
       height = 4)

## Appendix Table 7: Telephone Survey Summary Statistics -----------------

d_reg_summary <- d %>%
  dplyr::select(pa_mean_2,
                pn_mean_2,
                misperception_score,
                agecat5, urban, 
                male, educat3, 
                income_perception, 
                quran, married
  ) 
d_reg_summary <- as.data.frame(d_reg_summary)

stargazer(d_reg_summary,
          summary.stat = c("n", "mean", "sd", "min", "max"),
          covariate.labels = c(
            "PA Mean", "PN Mean", "Misperception Score",
            "Age Category", "Urban", 
            "Male", "Ed. Category",
            "Subj. SES", "Qur'an", "Married"
          ))

## Appendix Table 8: Online Survey Summary Statistics -----------------

o_reg_summary <- o %>%
  dplyr::select(pa_mean,
                pn_mean,
                misperception_score,
                agecat5, urban, 
                male, educat3, 
                income_perception, religiosity, married
  ) 
o_reg_summary <- as.data.frame(o_reg_summary)

stargazer(o_reg_summary,
          summary.stat = c("n", "mean", "sd", "min", "max"),
          covariate.labels = c(
            "PA Mean", "PN Mean", 
            "Misperception Score",
            "Age Category", "Urban", 
            "Male", "Ed. Category",
            "Subj. SES", "Religiosity", 
            "Married"
          ))


## Appendix Table 9: Correlates of PA and PN (Telephone) ---------------------

# DV = PA, gender and education only
pa_model <- as.formula(pa_mean_2 ~  
                          male + factor(educat3))
mpa_t_ge <- lm(pa_model, data = d, weights = wt)

# DV = PA, all covariates
pa_model <- as.formula(pa_mean_2 ~  
                          factor(agecat5) + urban + 
                          male + factor(educat3) + 
                          income_perception + quran + 
                          married)
mpa_t <- lm(pa_model, data = d, weights = wt)

# DV = PN, gender and education only 
pn_model <- as.formula(pn_mean_2 ~  
                          male + factor(educat3))
mpn_t_ge <- lm(pn_model, data = d, weights = wt)

# DV = PN, all covariates
pn_model <- as.formula(pn_mean_2 ~  
                          factor(agecat5) + urban + 
                          male + factor(educat3) + 
                          income_perception + quran + 
                          married)
mpn_t <- lm(pn_model, data = d, weights = wt)

modelsummary(list(
  "PA" = mpa_t_ge,
  "PA" = mpa_t,
  "PN" = mpn_t_ge,
  "PN" = mpn_t
),
stars = T,
coef_map = cm_gaps,
metrics = "rmse", 
gof_map = gm_lm,
output = "latex")

## Appendix Table 10: Correlates of PA and PN (Online) -----------------

# DV = PA, gender and education only
pa_model <- as.formula(pa_mean ~  
                          male + factor(educat3))
mpa_o_ge <- lm(pa_model, data = o, weights = wt)

# DV = PA, all covariates
pa_model <- as.formula(pa_mean ~ 
                          factor(agecat5) + urban + 
                          male + factor(educat3) + 
                          income_perception + 
                          religiosity + married)
mpa_o <- lm(pa_model, data = o, weights = wt)

# DV = PN, gender and education only
pn_model <- as.formula(pn_mean ~ 
                          male + factor(educat3))
mpn_o_ge <- lm(pn_model, data = o, weights = wt)

# DV = PN, all covariates
pn_model <- as.formula(pn_mean ~ 
                          factor(agecat5) + urban + 
                          male + factor(educat3) + 
                          income_perception + 
                          religiosity + married)
mpn_o <- lm(pn_model, data = o, weights = wt)

modelsummary(list(
  "PA" = mpa_o_ge,
  "PA" = mpa_o,
  "PN" = mpn_o_ge,
  "PN" = mpn_o
),
stars = T,
coef_map = cm_gaps,
metrics = "rmse", 
gof_map = gm_lm,
output = "latex")

## Appendix Table 11: Correlates of Misperception Gap (OLS) ------------------

mis_model <- as.formula(misperception_score ~ 
                          factor(agecat5) + urban + 
                          male + factor(educat3) + 
                          income_perception + quran + married)
mmis_t <- lm(mis_model, data = d, weights = wt)

mmis_t_abs <- lm(update(mis_model, abs(misperception_score) ~ .), 
                 data = d, weights = wt)

mis_model <- as.formula(misperception_score ~ 
                          factor(agecat5) + urban + 
                          male + factor(educat3) + 
                          income_perception + religiosity + married)
mmis_o <- lm(mis_model, data = o, weights = wt)

mmis_o_abs <- lm(update(mis_model, abs(misperception_score) ~ .), 
                 data = o, weights = wt)

modelsummary(list(
  "Misperception" = mmis_t,
  "Abs. Val. MP" = mmis_t_abs,
  "Misperception" = mmis_o,
  "Abs. Val. MP" = mmis_o_abs
),
stars = T,
coef_map = cm_gaps,
metrics = "rmse", 
gof_map = gm_lm,
output = "latex")

## Appendix Table 12: Correlates of Misperception (Logistic Regression) ------------

# Note: models were estimated above in section creating 
# main text Figure 3 displaying predicted probabilities

modelsummary(list(
  "Telephone" = mmis_t_bin,
  "Online" = mmis_o_bin
),
stars = T,
coef_map = cm_gaps,
metrics = "rmse", 
gof_map = gm_log,
output = "latex")


## Appendix Figure 4: PA vs PN by Mean Response (Telephone) -------------

pd <- d %>%
  # for each variable, calculate the weighted mean of responses
  dplyr::summarize(across(.cols = c(men_jobs_pa, women_auth_pa, housework_pa,
                                    men_last_pa, underage_pa, inheritance_pa,
                                    men_jobs_pn, women_auth_pn, housework_pn,
                                    men_last_pn, underage_pn, inheritance_pn),
                          ~weighted.mean(.x, w = wt, na.rm = T),
                          .names = "mean_{.col}"),
                   across(.cols = c(men_jobs_pa, women_auth_pa, housework_pa,
                                    men_last_pa, underage_pa, inheritance_pa,
                                    men_jobs_pn, women_auth_pn, housework_pn,
                                    men_last_pn, underage_pn, inheritance_pn),
                          ~likert_se(responses = .x, N = 1302),
                          .names = "margin_{.col}")) %>%
  # pivot
  pivot_longer(cols = contains("mean"),
               names_prefix = "mean_") %>%
  pivot_longer(cols = contains("margin"),
               names_to = "variable",
               values_to = "me",
               names_prefix = "margin_") %>%
  # only keep rows where the variables match
  filter(name == variable) %>%
  select(-variable) %>%
  # calculate lower and upper CI
  mutate(lower = value - me,
         upper = value + me) %>%
  # prep for plotting
  mutate(Response = c(rep("Attitudes", 6), 
                      rep("Perceived Norms", 6)),
         Question = rep(c("Men Jobs", "Women Auth.", "Housework", 
                          "Last Word", "Underage", "Inheritance"), 2)) %>%
  arrange(value) 

ggplot(pd, aes(x = reorder(Question, value), y = value,
               group = Response,
               fill = Response,
               color = Response)) + 
  geom_col(width = 0.5,
           position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(width = 0.6),
                width = 0.2,
                color = "black") +
  scale_y_continuous(limits = c(0,3)) +
  labs(x = "Item", y = "Mean Response"
  ) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  theme_cb() +
  theme(legend.position = "bottom")

ggsave("pa_vs_pn_all_items_weighted_MR.png",
       width = 6.25,
       height = 4.5)

# Save attitudes for inclusion in online survey graph
tel_att_mean <- pd %>%
  filter(Response == "Attitudes") %>%
  mutate(Survey = "Telephone")


## Appendix Figure 5: PA vs PN by Mean Response (Online) -------------

pd <- o %>%
  # for each variable, calculate the weighted mean of progressive responses
  dplyr::summarize(across(.cols = c(pa_men_jobs, pa_women_power, 
                                    pa_housework, pa_final_say,
                                    pn_men_jobs, pn_women_power, 
                                    pn_housework, pn_final_say),
                          ~weighted.mean(.x, w = wt, na.rm = T),
                          .names = "mean_{.col}"),
                   across(.cols = c(pa_men_jobs, pa_women_power, 
                                    pa_housework, pa_final_say,
                                    pn_men_jobs, pn_women_power, 
                                    pn_housework, pn_final_say),
                          ~likert_se(responses = .x, N = 1291),
                          .names = "margin_{.col}")) %>%
  # pivot
  pivot_longer(cols = contains("mean"),
               names_prefix = "mean_") %>%
  pivot_longer(cols = contains("margin"),
               names_to = "variable",
               values_to = "me",
               names_prefix = "margin_") %>%
  # only keep rows where the variables match
  filter(name == variable) %>%
  select(-variable) %>%
  # calculate lower and upper CI
  mutate(lower = value - me,
         upper = value + me) %>%
  # prep for plotting
  mutate(Response = c(rep("Attitudes",4), 
                      rep("Perceived Norms", 4)),
         Question = rep(c("Men Jobs", "Women Authority", 
                          "Housework", "Last Word"), 2)) %>%
  arrange(value) 

# Merge in telephone actual means
pd <- pd %>%
  mutate(Survey = "Online") %>%
  bind_rows(tel_att_mean) %>%
  filter(!(Question %in% c("Inheritance", "Underage")))

pd$Question[pd$Question == "Women Auth."] <- "Women Authority"

pd <- pd %>%
  mutate(Response = paste(Response, Survey, sep = " "))

pd$Response <- fct_relevel(pd$Response, "Perceived Norms Online", 
                           after = 1)

ggplot(pd, aes(x = reorder(Question, value), y = value,
               group = Response,
               fill = Response,
               color = Response)) + 
  geom_col(width = 0.5,
           position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(width = 0.6),
                width = 0.2,
                color = "black") +
  scale_y_continuous(limits = c(0,3)) +
  labs(x = "Item", y = "Mean Response") +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  theme_cb() +
  theme(legend.position = "bottom")

ggsave("pa_vs_pn_all_items_weighted_online_MR.png",
       width = 6.3,
       height = 3.8)


## Functions to Make Misperception-by-Demographic Figures -------

# NOTE: The appendix uses these functions to create Figures 6 & 7.
# Similar code creates graphs breaking down responses by other
# demographic sub-categories. This code + results are available 
# upon request.

# Function to gather PA proportions for misperception graphs
gather_props <- function(data, var1, value, value_name){
  temp <- data %>% filter({{var1}} == value)
  prop_mw <- weighted.mean(temp$men_jobs_pa %in% c(2:3), 
                           w = temp$wt, na.rm = T)
  prop_wa <- weighted.mean(temp$women_auth_pa %in% c(2:3), 
                           w = temp$wt, na.rm = T)
  prop_ho <- weighted.mean(temp$housework_pa %in% c(2:3), 
                           w = temp$wt, na.rm = T)
  prop_ml <- weighted.mean(temp$men_last_pa %in% c(2:3), 
                           w = temp$wt, na.rm = T)
  prop_ua <- weighted.mean(temp$underage_pa %in% c(2:3), 
                           w = temp$wt, na.rm = T)
  prop_in <- weighted.mean(temp$inheritance_pa %in% c(2:3), 
                           w = temp$wt, na.rm = T)
  out_df <- data.frame(
    Value = rep(value_name, 6),
    Question = c("Men Jobs", "Women Auth.", "Housework",
                 "Last Word", "Underage", "Inheritance"),
    Measure = "PA",
    value = c(prop_mw, prop_wa, prop_ho,
              prop_ml, prop_ua, prop_in)
  )
  return(out_df)
}

# test
gather_props(d, educat3, 1, "Lower")

# Function to gather PN proportions
gather_props_pn <- function(data, var1, value, value_name){
  temp <- data %>% filter({{var1}} == value)
  prop_mw <- weighted.mean(temp$men_jobs_pn %in% c(2:3), 
                           w = temp$wt, na.rm = T)
  prop_wa <- weighted.mean(temp$women_auth_pn %in% c(2:3), 
                           w = temp$wt, na.rm = T)
  prop_ho <- weighted.mean(temp$housework_pn %in% c(2:3), 
                           w = temp$wt, na.rm = T)
  prop_ml <- weighted.mean(temp$men_last_pn %in% c(2:3), 
                           w = temp$wt, na.rm = T)
  prop_ua <- weighted.mean(temp$underage_pn %in% c(2:3), 
                           w = temp$wt, na.rm = T)
  prop_in <- weighted.mean(temp$inheritance_pn %in% c(2:3), 
                           w = temp$wt, na.rm = T)
  out_df <- data.frame(
    Value = rep(value_name, 6),
    Question = c("Men Jobs", "Women Auth.", "Housework",
                 "Last Word", "Underage", "Inheritance"),
    Measure = "PN",
    value = c(prop_mw, prop_wa, prop_ho,
              prop_ml, prop_ua, prop_in)
  )
  return(out_df)
}

# test
gather_props_pn(d, educat3, 1, "Lower")


# Function to gather progressive proportions from online survey
gather_props_ol <- function(data, var1, value, value_name){
  temp <- data %>% filter({{var1}} == value)
  prop_mw <- weighted.mean(temp$pa_men_jobs %in% c(2:3), 
                           w = temp$wt, na.rm = T)
  prop_wa <- weighted.mean(temp$pa_women_power %in% c(2:3), 
                           w = temp$wt, na.rm = T)
  prop_ho <- weighted.mean(temp$pa_housework %in% c(2:3), 
                           w = temp$wt, na.rm = T)
  prop_ml <- weighted.mean(temp$pa_final_say %in% c(2:3), 
                           w = temp$wt, na.rm = T)
  out_df <- data.frame(
    Value = rep(value_name, 4),
    Question = c("Men Jobs", "Women Auth.", "Housework",
                 "Last Word"),
    Measure = "PA",
    value = c(prop_mw, prop_wa, prop_ho,
              prop_ml)
  )
  return(out_df)
}

gather_props_ol(o, educat3, 1, "Lower")

# Function to gather PN proportions from online survey
gather_props_pn_ol <- function(data, var1, value, value_name){
  temp <- data %>% filter({{var1}} == value)
  prop_mw <- weighted.mean(temp$pn_men_jobs %in% c(2:3), 
                           w = temp$wt, na.rm = T)
  prop_wa <- weighted.mean(temp$pn_women_power %in% c(2:3), 
                           w = temp$wt, na.rm = T)
  prop_ho <- weighted.mean(temp$pn_housework %in% c(2:3), 
                           w = temp$wt, na.rm = T)
  prop_ml <- weighted.mean(temp$pn_final_say %in% c(2:3), 
                           w = temp$wt, na.rm = T)
  out_df <- data.frame(
    Value = rep(value_name, 4),
    Question = c("Men Jobs", "Women Auth.", "Housework",
                 "Last Word"),
    Measure = "PN",
    value = c(prop_mw, prop_wa, prop_ho,
              prop_ml)
  )
  return(out_df)
}

gather_props_pn_ol(o, educat3, 1, "Lower")

## Appendix Figure 6: Misperception by Gender (Telephone) ---------

# Proportion w/ progressive view, weighted - female 
fem_prop_pa <- gather_props(d, female, 1, "Female")
fem_prop_pn <- gather_props_pn(d, female, 1, "Female")

# Proportion w/ progressive view, weighted - male
male_prop_pa <- gather_props(d, female, 0, "Male")
male_prop_pn <- gather_props_pn(d, female, 0, "Male")

# Overall weighted mean
wtd_means <- d %>%
  ungroup() %>%
  dplyr::summarize(mean_mj_pa = weighted.mean(men_jobs_pa %in% c(2:3), w = d$wt, na.rm = T),
                   mean_wa_pa = weighted.mean(women_auth_pa %in% c(2:3), w = d$wt, na.rm = T),
                   mean_ho_pa = weighted.mean(housework_pa %in% c(2:3), w = d$wt, na.rm = T),
                   mean_ml_pa = weighted.mean(men_last_pa %in% c(2:3), w = d$wt, na.rm = T),
                   mean_ua_pa = weighted.mean(underage_pa %in% c(2:3), w = d$wt, na.rm = T),
                   mean_in_pa = weighted.mean(inheritance_pa %in% c(2:3), w = d$wt, na.rm = T)) %>%
  pivot_longer(cols = everything()) 

wtd_means <- wtd_means %>%
  bind_rows(wtd_means) %>%
  mutate(
    Value = c(rep("Female", 6),
              rep("Male", 6)),
    Question = rep(c("Men Jobs", "Women Auth.", 
                     "Housework", "Last Word", 
                     "Underage", "Inheritance"), 2)
  ) %>%
  rename(sm = value)

pd <- bind_rows(fem_prop_pa, fem_prop_pn,
                male_prop_pa, male_prop_pn) %>% 
  mutate(me = prop_se(prop = value, 
                      N = ifelse(Value == "Female", 664, 638))) %>%
  left_join(y = wtd_means %>% select(-name), 
            by = c("Value", "Question")) %>%
  mutate(
    lower = value - me,
    upper = value + me
  )

pd$Value <- factor(pd$Value)
pd$Value <- fct_relevel(pd$Value, "Female", "Male")

pd$Measure <- factor(pd$Measure)
pd$Measure <- fct_relevel(pd$Measure, "PA", "PN")

ggplot(pd, aes(x = Value,
               y = value,
               group = Measure,
               fill = Measure,
               color = Measure)) +
  facet_wrap(~Question, ncol = 2) +
  geom_col(width = 0.5,
           position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(width = 0.6),
                width = 0.2,
                color = "black") +
  geom_hline(aes(yintercept = sm),
             linetype = 3, size = 0.5) +
  labs(y = "Proportion with Egalitarian View",
       x = "Gender") +
  scale_y_continuous(limits = c(0,1),
                     labels = scales::percent_format()) + 
  scale_fill_brewer(palette = "Paired") +
  scale_color_brewer(palette = "Paired") + 
  theme_cb() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

ggsave("misperception_by_gen_prop.png",
       width = 4.5,
       height = 5)


## Appendix Figure 7: Misperception by Gender (Online) ---------

# Proportion w/ progressive view, weighted - female 
fem_prop_pa <- gather_props_ol(o, female, 1, "Female")
fem_prop_pn <- gather_props_pn_ol(o, female, 1, "Female")

# Proportion w/ progressive view, weighted - male
male_prop_pa <- gather_props_ol(o, female, 0, "Male")
male_prop_pn <- gather_props_pn_ol(o, female, 0, "Male")

# Overall weighted mean
wtd_means <- o %>%
  ungroup() %>%
  dplyr::summarize(mean_mj_pa = weighted.mean(pa_men_jobs %in% c(2:3), w = o$wt, na.rm = T),
                   mean_wa_pa = weighted.mean(pa_women_power %in% c(2:3), w = o$wt, na.rm = T),
                   mean_ho_pa = weighted.mean(pa_housework %in% c(2:3), w = o$wt, na.rm = T),
                   mean_ml_pa = weighted.mean(pa_final_say %in% c(2:3), w = o$wt, na.rm = T)) %>%
  pivot_longer(cols = everything()) 

wtd_means <- wtd_means %>%
  bind_rows(wtd_means) %>%
  mutate(
    Value = c(rep("Female", 4),
              rep("Male", 4)),
    Question = rep(c("Men Jobs", "Women Auth.", 
                     "Housework", "Last Word"), 2)
  ) %>%
  rename(sm = value)

pd <- bind_rows(fem_prop_pa, fem_prop_pn,
                male_prop_pa, male_prop_pn) %>% 
  mutate(me = prop_se(prop = value, 
                      N = ifelse(Value == "Female", 645, 646))) %>%
  left_join(y = wtd_means %>% select(-name), 
            by = c("Value", "Question")) %>%
  mutate(
    lower = value - me,
    upper = value + me
  )

pd$Value <- factor(pd$Value)
pd$Value <- fct_relevel(pd$Value, "Female", "Male")

pd$Measure <- factor(pd$Measure)
pd$Measure <- fct_relevel(pd$Measure, "PA", "PN")

ggplot(pd, aes(x = Value,
               y = value,
               group = Measure,
               fill = Measure,
               color = Measure)) +
  facet_wrap(~Question, ncol = 2) +
  geom_col(width = 0.5,
           position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(width = 0.6),
                width = 0.2,
                color = "black") +
  geom_hline(aes(yintercept = sm),
             linetype = 3, size = 0.5) +
  labs(y = "Proportion with Egalitarian View",
       x = "Gender") +
  scale_y_continuous(limits = c(0,1),
                     labels = scales::percent_format()) + 
  scale_fill_brewer(palette = "Paired") +
  scale_color_brewer(palette = "Paired") +  
  theme_cb() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

ggsave("misperception_by_gen_prop_ol.png",
       width = 4.5,
       height = 4.25)


## Appendix Table 10: PA First vs PN First -------------------

# Kolmogorov-Smirnov Test for overall PA and PN scores
k1 <- ks.test(o$pa_mean[o$pn_first == 1],
              o$pa_mean[o$pn_first == 0]) # p = 0.928
k1

k2 <- ks.test(o$pn_mean[o$pn_first == 1],
              o$pn_mean[o$pn_first == 0]) # p = 1
k2

# For individual questions
# Men jobs
k3 <- ks.test(o$pa_men_jobs[o$pn_first == 1],
              o$pa_men_jobs[o$pn_first == 0]) # p = 0.55
k3

k4 <- ks.test(o$pn_men_jobs[o$pn_first == 1],
              o$pn_men_jobs[o$pn_first == 0]) # p = 0.17
k4

# Women authority
k5 <- ks.test(o$pa_women_power[o$pn_first == 1],
              o$pa_women_power[o$pn_first == 0]) # p = 1
k5

k6 <- ks.test(o$pn_women_power[o$pn_first == 1],
              o$pn_women_power[o$pn_first == 0]) # p = 1
k6

# Housework
k7 <- ks.test(o$pa_housework[o$pn_first == 1],
              o$pa_housework[o$pn_first == 0]) # p = 0.32
k7

k8 <- ks.test(o$pn_housework[o$pn_first == 1],
              o$pn_housework[o$pn_first == 0]) # p = 0.54
k8

# Final say
k9 <- ks.test(o$pa_final_say[o$pn_first == 1],
              o$pa_final_say[o$pn_first == 0]) # p = 0.94
k9

k10 <- ks.test(o$pn_final_say[o$pn_first == 1],
               o$pn_final_say[o$pn_first == 0]) # p = 1
k10

## Table
k_table <- data.frame(matrix(nrow = 10, ncol = 4))
names(k_table) <- c("Question", "Mean (PA First)", 
                    "Mean (PN First)", "KS Test p-value")

k_table$Question <- c("Mean PA", "Mean PN", 
                      "Men Jobs PA", "Men Jobs PN",
                      "Women Authority PA", 
                      "Women Authority PN",
                      "Housework PA", "Housework PN", 
                      "Last Word PA", "Last Word PN")

k_table$`Mean (PA First)` <- c(
  mean(o$pa_mean[o$pn_first == 0], na.rm = T),
  mean(o$pn_mean[o$pn_first == 0], na.rm = T),
  mean(o$pa_men_jobs[o$pn_first == 0], na.rm = T),
  mean(o$pn_men_jobs[o$pn_first == 0], na.rm = T),
  mean(o$pa_women_power[o$pn_first == 0], na.rm = T),
  mean(o$pn_women_power[o$pn_first == 0], na.rm = T),
  mean(o$pa_housework[o$pn_first == 0], na.rm = T),
  mean(o$pn_housework[o$pn_first == 0], na.rm = T),
  mean(o$pa_final_say[o$pn_first == 0], na.rm = T),
  mean(o$pn_final_say[o$pn_first == 0], na.rm = T)
)

k_table$`Mean (PN First)` <- c(
  mean(o$pa_mean[o$pn_first == 1], na.rm = T),
  mean(o$pn_mean[o$pn_first == 1], na.rm = T),
  mean(o$pa_men_jobs[o$pn_first == 1], na.rm = T),
  mean(o$pn_men_jobs[o$pn_first == 1], na.rm = T),
  mean(o$pa_women_power[o$pn_first == 1], na.rm = T),
  mean(o$pn_women_power[o$pn_first == 1], na.rm = T),
  mean(o$pa_housework[o$pn_first == 1], na.rm = T),
  mean(o$pn_housework[o$pn_first == 1], na.rm = T),
  mean(o$pa_final_say[o$pn_first == 1], na.rm = T),
  mean(o$pn_final_say[o$pn_first == 1], na.rm = T)
)

k_table$`KS Test p-value` <- c(
  k1$p.value, 
  k2$p.value, 
  k3$p.value, 
  k4$p.value, 
  k5$p.value, 
  k6$p.value, 
  k7$p.value, 
  k8$p.value, 
  k9$p.value, 
  k10$p.value
)

## Round all numbers to 3 digits
kt <- k_table %>%
  purrr::modify_if(~is.numeric(.), ~round(., 3))

## Print
print(xtable(kt), include.rownames = F)



## Appendix Figure 8: PA vs. PN by Enumerator Gender --------

table(d$interviewer_gender)
enum_male <- 284
enum_fem <- 1018

pd <- d %>%
  # dichotomize into more vs. less progressive responses
  mutate(across(.cols = c(men_jobs_pa, women_auth_pa, housework_pa,
                          men_last_pa, underage_pa, inheritance_pa,
                          men_jobs_pn, women_auth_pn, housework_pn,
                          men_last_pn, underage_pn, inheritance_pn),
                ~case_when(
                  .x %in% c(2,3) ~ 1,
                  .x %in% c(0,1) ~ 0,
                  TRUE ~ NA_real_
                ))) %>%
  # group by interviewer gender
  group_by(interviewer_female) %>%
  # for each variable, calculate the weighted mean of progressive responses
  dplyr::summarize(across(.cols = c(men_jobs_pa, women_auth_pa, housework_pa,
                                    men_last_pa, underage_pa, inheritance_pa,
                                    men_jobs_pn, women_auth_pn, housework_pn,
                                    men_last_pn, underage_pn, inheritance_pn),
                          ~weighted.mean(.x, w = wt, na.rm = T),
                          .names = "mean_{.col}")) %>%
  # pivot
  pivot_longer(cols = -interviewer_female) %>%
  # calculate margin of error of proportion, lower and upper CI
  mutate(me = prop_se(prop = value, 
                      N = ifelse(interviewer_female == 1, 
                                 enum_fem,
                                 enum_male)), 
         lower = value - me,
         upper = value + me) %>%
  # prep for plotting
  mutate(Response = rep(c(rep("Attitudes", 6), 
                          rep("Perceived Norms", 6)), 2),
         Question = rep(c("Men Jobs", "Women Auth.", "Housework", 
                          "Last Word", "Underage", "Inheritance"), 4),
         Enumerator = c(rep("ME", 12), rep("FE", 12)),
         Group = paste(Enumerator, Response, sep = " ")) %>%
  arrange(value) 

ggplot(pd, aes(x = reorder(Question, value), y = value,
               group = Group,
               fill = Group,
               color = Group)) + 
  geom_col(width = 0.5,
           position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(width = 0.6),
                width = 0.2,
                color = "black") +
  scale_y_continuous(limits = c(0,1),
                     labels = scales::percent_format()) +
  labs(x = "Item", y = "Proportion with Progressive View") +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  theme_cb() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 10)) +
  guides(col = guide_legend(nrow = 2, byrow = TRUE))

ggsave("pa_vs_pn_all_items_weighted_ig.png",
       width = 6.25,
       height = 4.5)

## Appendix Table 14: Correlates of PA, PN, and Misperception with Enumerator Gender ---------------------

# DV = PA
pa_model <- as.formula(pa_mean_2 ~  
                          factor(agecat5) + urban + 
                          male + factor(educat3) + 
                          income_perception + quran + married +
                          interviewer_female)
mpa_t_ig <- lm(pa_model, data = d, weights = wt)

# DV = PN
pn_model <- as.formula(pn_mean_2 ~  
                          factor(agecat5) + urban + 
                          male + factor(educat3) + 
                          income_perception + quran + married +
                          interviewer_female)
mpn_t_ig <- lm(pn_model, data = d, weights = wt)

# DV = Misperception Score
mis_model <- as.formula(misperception_score ~ 
                          factor(agecat5) + urban + 
                          male + factor(educat3) + 
                          income_perception + quran + 
                          married + interviewer_female)
mmis_t_ig <- lm(mis_model, data = d, weights = wt)

# DV = Absolute value of misperception gap
abs_model <- as.formula(abs(misperception_score) ~ 
                          factor(agecat5) + urban + 
                          male + factor(educat3) + 
                          income_perception + quran + 
                          married + interviewer_female)
mmis_t_abs_ig <- lm(abs_model, data = d, weights = wt)


modelsummary(list(
  "PA" = mpa_t_ig,
  "PN" = mpn_t_ig,
  "Misperception" = mmis_t_ig,
  "Abs. Val. MP" = mmis_t_abs_ig
),
stars = T,
coef_map = cm_gaps,
metrics = "rmse", 
gof_map = gm_lm,
output = "latex")

## Appendix Table 15: Correlates of PA, PN, and Misperception with Enumerator-Respondent Interaction ---------------------

# DV = PA
pa_model <- as.formula(pa_mean_2 ~  
                          factor(agecat5) + urban + 
                          factor(educat3) + 
                          income_perception + quran + 
                          married +
                          male * interviewer_female)
mpa_t_ig_rp <- lm(pa_model, data = d, weights = wt)

# DV = PN
pn_model <- as.formula(pn_mean_2 ~  
                          factor(agecat5) + urban + 
                          factor(educat3) + 
                          income_perception + quran + 
                          married +
                          male * interviewer_female)
mpn_t_ig_rp <- lm(pn_model, data = d, weights = wt)

# DV = Misperception gap
mis_model <- as.formula(misperception_score ~ 
                          factor(agecat5) + urban + 
                          male + factor(educat3) + 
                          income_perception + quran + 
                          married +
                          interviewer_female * male)
mmis_t_ig_rp <- lm(mis_model, data = d, weights = wt)

# DV = Absolute value of misperception gap
abs_model <- as.formula(abs(misperception_score) ~ 
                          factor(agecat5) + urban + 
                          male + factor(educat3) + 
                          income_perception + quran + 
                          married +
                          interviewer_female * male)
mmis_t_abs_ig_rp <- lm(abs_model, data = d, weights = wt)

modelsummary(list(
  "PA" = mpa_t_ig_rp,
  "PN" = mpn_t_ig_rp,
  "Misperception" = mmis_t_ig_rp,
  "Abs. Val. MP" = mmis_t_abs_ig_rp
),
stars = T,
coef_map = cm_gaps,
metrics = "rmse", 
gof_map = gm_lm,
output = "latex")


## Appendix Table 16: Survey Response Comparisons ---------------------

### Save comparable percents from tel survey -------------
# Recall already coded so that higher values = more egalitarian views
t1 <- topline(d, men_jobs_pa, weight = wt)
t2 <- topline(d, inheritance_pa, weight = wt)
t3 <- topline(d, men_last_pa, weight = wt)
t4 <- topline(d, women_auth_pa, weight = wt)

tel_percents <- tibble(
  percents = c(t1$Percent[3] + t1$Percent[4],
               t2$Percent[3] + t2$Percent[4],
               t3$Percent[3] + t3$Percent[4],
               t4$Percent[3] + t4$Percent[4]
  ),
  variables = c("men_jobs", "inheritance", 
                "men_last", "women_auth")
)

### Save comparable percents from online survey -------------
# Recall already coded so that higher values = more egalitarian views
o1 <- topline(o, pa_men_jobs, weight = wt)
o2 <- topline(o, pa_final_say, weight = wt)
o3 <- topline(o, pa_women_power, weight = wt)

ol_percents <- tibble(
  percents = c(o1$Percent[3] + o1$Percent[4],
               o2$Percent[3] + o2$Percent[4],
               o3$Percent[3] + o3$Percent[4]
  ),
  variables = c("men_jobs", "men_last", 
                "women_auth")
)

### Calculate percentages from AB Wave 5 --------------

ab5_mor <- ab5 %>%
  filter(country == "Morocco") 

# final say
b1 <- topline(ab5_mor, Q601_18, weight = wt)

# inheritance 1 - egal
b2 <- topline(ab5_mor, Q601_9, weight = wt)

# inheritance 2 - egal
b3 <- topline(ab5_mor, Q601_9A, weight = wt)

# woman pm - egal
b4 <- topline(ab5_mor, Q601_1, weight = wt)

# quotas - egal
b5 <- topline(ab5_mor, Q601A, weight = wt)


ab5_percents <- c(
  b1$Percent[3] + b1$Percent[4], # final say
  b2$`Valid Percent`[1] + b2$`Valid Percent`[2], # inheritance 1
  b3$`Valid Percent`[1] + b3$`Valid Percent`[2], # inheritance 2
  b4$Percent[1] + b4$Percent[2], # woman PM
  b5$Percent[1] + b5$Percent[2] # quotas
)

ab5_percents <- data.frame(ab5_percents)
ab5_percents$variables <- c(
  "final_say", "inheritance_share", 
  "inheritance_rights", 
  "woman_pm", "quotas"
)


### Calculate percentages from AB Wave 6 ----------------

mor6_1 <- ab6_1 %>%
  filter(COUNTRY == 13) %>%
  select(Q601_18, WT)

mor6_3 <- ab6_3 %>%
  filter(COUNTRY == 13) %>%
  select(Q601_1, Q601_18, WT)

# Woman can be PM (Part 3 only)
a1 <- topline(df = mor6_3, variable = Q601_1, weight = WT)

# Man final say at home (Part 1)
a2a <- topline(df = mor6_1, variable = Q601_18, weight = WT)

# MMan final say at home (Part 3)
a2b <- topline(df = mor6_3, variable = Q601_18, weight = WT)

ab6_percents <- c(
  a1$Percent[1] + a1$Percent[2],
  a2a$Percent[3] + a2a$Percent[4],
  a2b$Percent[3] + a2b$Percent[4]
)

ab6_percents <- data.frame(ab6_percents)
ab6_percents$variables <- c(
  "woman_pm", "final_say_1", 
  "final_say_2"
)


### Calculate percentages from WVS Wave 7 ---------------

wd <- mor_wvs %>%
  select(Q33_3, 
         W_WEIGHT) %>%
  rename(men_jobs = Q33_3,
         wt = W_WEIGHT)

w1 <- topline(wd, men_jobs, weight = wt)

wvs_results <- c(
  w1$Percent[2]
)

wvs_results <- data.frame(wvs_results)
wvs_results$variable <- c(
  "men_jobs"
)


### Build the table ------------------------------------
pcts_tab <- data.frame(matrix(nrow = 12, ncol = 6))
names(pcts_tab) <- c("Question",
                     "Telephone",
                     "Online",
                     "AB W5", 
                     "AB W6", 
                     "WVS W7"
)

pcts_tab$Question <- c("Men Jobs", 
                       "",
                       
                       "Men Final Say", 
                       "",
                       
                       "Women Authority",
                       "Woman PM",
                       "Quotas", 
                       "",
                       
                       "Inheritance (Keep)", 
                       "Inheritance (Shares)", 
                       "Inheritance (Rights)", 
                       "")

pcts_tab$Telephone <- c(tel_percents$percents[1], "",
                        tel_percents$percents[3], "", 
                        tel_percents$percents[4], "", "", "",
                        tel_percents$percents[2], "", "", "")

pcts_tab$Online <- c(ol_percents$percents[1], "",
                     ol_percents$percents[2], "", 
                     ol_percents$percents[3], "", "", "",
                     "", "", "", "")

pcts_tab$`AB W5` <- c("", "", 
                      
                      ab5_percents$ab5_percents[1], "",
                      
                      "", 
                      ab5_percents$ab5_percents[4], 
                      ab5_percents$ab5_percents[5], 
                      "",
                      
                      "", 
                      ab5_percents$ab5_percents[2],
                      ab5_percents$ab5_percents[3],
                      ""
)


pcts_tab$`AB W6` <- c("", "",
                      
                      (ab6_percents$ab6_percents[2] + 
                         ab6_percents$ab6_percents[3]) / 2, "",
                      
                      "", ab6_percents$ab6_percents[1],
                      "", "",
                      
                      "", "", "", "")

pcts_tab$`WVS W7` <- c(wvs_results$wvs_results[1], "",
                       
                       "", "", 
                       
                       "", "", "", "",
                       
                       "", "", "", "")

pcts_tab <- pcts_tab %>%
  mutate(across(.cols = 2:6, 
                ~round(as.numeric(.x), digits = 2)))

print(xtable(pcts_tab), include.rownames = F)

## Appendix Table 1y: Correlates of Misperception Gap with SI Worry Control ---------------------

mis_model <- as.formula(misperception_score ~ 
                          factor(agecat5) + urban + 
                          male + factor(educat3) + 
                          income_perception + quran + 
                          married + si_worry)
mmis_t_si <- lm(mis_model, data = d, weights = wt)

mmis_t_abs_si <- lm(update(mis_model, abs(misperception_score) ~ .), 
                    data = d, weights = wt)

mis_model <- as.formula(misperception_score ~ 
                          factor(agecat5) + urban + 
                          male + factor(educat3) + 
                          income_perception + 
                          religiosity + married +
                          si_worry)
mmis_o_si <- lm(mis_model, data = o, weights = wt)

mmis_o_abs_si <- lm(update(mis_model, abs(misperception_score) ~ .), 
                    data = o, weights = wt)

modelsummary(list(
  "Misperception" = mmis_t_si,
  "Abs. Val. MP" = mmis_t_abs_si,
  "Misperception" = mmis_o_si,
  "Abs. Val. MP" = mmis_o_abs_si
),
stars = T,
coef_map = cm_gaps,
metrics = "rmse",
gof_map = gm_lm,
output = "latex")

## Appendix Table 18: PA and PN with SI Worry Control (Telephone) ---------------------

pa_model <- as.formula(pa_mean_2 ~  
                          male + factor(educat3) +
                          si_worry)
mpa_t_ge_si <- lm(pa_model, data = d, weights = wt)

pa_model <- as.formula(pa_mean_2 ~  
                          factor(agecat5) + urban + 
                          male + factor(educat3) + 
                          income_perception + quran + 
                          married + si_worry)
mpa_t_si <- lm(pa_model, data = d, weights = wt)

pn_model <- as.formula(pn_mean_2 ~  
                          male + factor(educat3) +
                          si_worry)
mpn_t_ge_si <- lm(pn_model, data = d, weights = wt)

pn_model <- as.formula(pn_mean_2 ~  
                          factor(agecat5) + urban + 
                          male + factor(educat3) + 
                          income_perception + quran + 
                          married + si_worry)
mpn_t_si <- lm(pn_model, data = d, weights = wt)


modelsummary(list(
  "PA" = mpa_t_ge_si,
  "PA" = mpa_t_si,
  "PN" = mpn_t_ge_si,
  "PN" = mpn_t_si
),
stars = T,
coef_map = cm_gaps,
metrics = "rmse",
gof_map = gm_lm,
output = "latex")

## Appendix Table 19: PA and PN with SI Worry Control (Online) ---------------

pa_model <- as.formula(pa_mean ~  
                          male + factor(educat3) +
                          si_worry)
mpa_o_ge_si <- lm(pa_model, data = o, weights = wt)

pa_model <- as.formula(pa_mean ~ 
                          factor(agecat5) + urban + 
                          male + factor(educat3) + 
                          income_perception + 
                          religiosity + married + 
                          si_worry)
mpa_o_si <- lm(pa_model, data = o, weights = wt)

pn_model <- as.formula(pn_mean ~  
                          male + factor(educat3) +
                          si_worry)
mpn_o_ge_si <- lm(pn_model, data = o, weights = wt)

pn_model <- as.formula(pn_mean ~ 
                          factor(agecat5) + urban + 
                          male + factor(educat3) + 
                          income_perception + 
                          religiosity + married + 
                          si_worry)
mpn_o_si <- lm(pn_model, data = o, weights = wt)


modelsummary(list(
  "PA" = mpa_o_ge_si,
  "PA" = mpa_o_si,
  "PN" = mpn_o_ge_si,
  "PN" = mpn_o_si
),
stars = T,
coef_map = cm_gaps,
metrics = "rmse",
gof_map = gm_lm,
output = "latex")

## Appendix Table 20: Online Survey Results with Alternate Misperception Measure ---------------------

mis_alt_model <- as.formula(misperception_score_alt ~ 
                              factor(agecat5) + urban + 
                              male + factor(educat3) + 
                              income_perception + 
                              religiosity + married)
mmis_o_x <- lm(mis_alt_model, data = o, weights = wt)

mmis_o_abs_x <- lm(update(mis_alt_model, abs(misperception_score_alt) ~ .), 
                   data = o, weights = wt)

mmis_o_si_x <- lm(update(mis_alt_model, misperception_score_alt ~ . + si_worry), 
                  data = o, weights = wt)

mmis_o_abs_si_x <- lm(update(mis_alt_model, abs(misperception_score_alt) ~ . + si_worry), 
                      data = o, weights = wt)

modelsummary(list(
  "Misperception" = mmis_o_x,
  "Abs. Val. MP" = mmis_o_abs_x,
  "Misperception" = mmis_o_si_x,
  "Abs. Val. MP" = mmis_o_abs_si_x
),
stars = T,
coef_map = cm_gaps,
metrics = "rmse",
gof_map = gm_lm,
output = "latex")


























