################################################################################
#Replication Materials for
#Explanation Giving Promotes Democratic Satisfaction Regardless of Respondent Partisanship
#
#Syntax produces:
#Figure OF1
#The Stata syntax file 'appendixF.do' needs to be run first to produce
#the estimates that are plotted below
################################################################################

rm(list = ls())

library(ggpubr)
library(cowplot)
library(rio)
library(tidyverse)
library(ggstance)

##############################################
#Explanations (Micro) Interaction
##############################################

##Function
dy_func1 <- function(x) {
  x %>%
    rename(
      ame = `_margin`, 
      lower = `_ci_lb`, 
      upper = `_ci_ub`, 
      mod_nmbr = `_at`, 
      mod_value = `_at2`, 
    ) %>%
    select(ame, lower, upper, mod_nmbr, mod_value)
}

###Dem Satis: ESS Explanation x Education
demsatis_explain_educ <- import("dem_satis_explain_educ.dta") %>%
  dy_func1() %>%
  mutate(iv = "Explanations (ESS)", 
         outcome = "Dem. Satisfaction", 
         moderator = "Education")

###Dem Satis: ESS Explanation x Interest
demsatis_explain_interest <- import("dem_satis_explain_interest.dta") %>%
  dy_func1() %>%
  mutate(iv = "Explanations (ESS)", 
         outcome = "Dem. Satisfaction", 
         moderator = "Interest")

###Gov Satis: ESS Explanation x Education
govsatis_explain_educ <- import("gov_satis_explain_educ.dta") %>%
  dy_func1() %>%
  mutate(iv = "Explanations (ESS)", 
         outcome = "Gov. Satisfaction", 
         moderator = "Education")

###Gov Satis: ESS Explanation x Interest
govsatis_explain_interest <- import("gov_satis_explain_interest.dta") %>%
  dy_func1() %>%
  mutate(iv = "Explanations (ESS)", 
         outcome = "Gov. Satisfaction", 
         moderator = "Interest")

##############################################
#Explanations (Macro) Interaction
##############################################

###Dem Satis: Vdem Explanation x Education
demsatis_reason_educ <- import("dem_satis_reason_educ.dta") %>%
  dy_func1() %>%
  mutate(iv = "Explanations (VDem)", 
         outcome = "Dem. Satisfaction", 
         moderator = "Education")

###Dem Satis: Vdem Explanation x Interest
demsatis_reason_interest <- import("dem_satis_reason_interest.dta") %>%
  dy_func1() %>%
  mutate(iv = "Explanations (VDem)", 
         outcome = "Dem. Satisfaction", 
         moderator = "Interest")

###Gov Satis: Vdem Explanation x Education
govsatis_reason_educ <- import("gov_satis_reason_educ.dta") %>%
  dy_func1() %>%
  mutate(iv = "Explanations (VDem)", 
         outcome = "Gov. Satisfaction", 
         moderator = "Education")

###Gov Satis: Vdem Explanation x Interest
govsatis_reason_interest <- import("gov_satis_reason_interest.dta") %>%
  dy_func1() %>%
  mutate(iv = "Explanations (VDem)", 
         outcome = "Gov. Satisfaction", 
         moderator = "Interest")


##############################################
#Combine
##############################################
dfs = sapply(.GlobalEnv, is.data.frame) 
comb <- do.call(rbind, mget(names(dfs)[dfs]))

##############################################
#Figure
##############################################

ggplot(comb, aes(x=mod_value, y=ame, linetype = moderator, shape = moderator)) + 
  geom_pointrange(aes(ymin=lower, ymax=upper), 
                  position = position_dodge(width=0.2)) + 
  geom_line(position = position_dodge(width=0.2)) + 
  facet_grid(iv ~ outcome) + 
  theme_bw(14) + 
  geom_hline(yintercept=0, linetype="dashed", color="red") + 
  labs(y = "AME of Explanation Variable", 
       x = "Value of Moderator", 
       shape = "Moderator:", 
       linetype = "Moderator:")

ggsave("figure_of1.png", 
       height=8, width=14)

#######################
#Remove data objects
#######################
rm(list = ls())
