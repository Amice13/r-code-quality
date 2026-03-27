################################################################################
#Replication Materials for
#Explanation Giving Promotes Democratic Satisfaction Regardless of Respondent Partisanship
#
#Syntax produces:
#Figures OA6 & OA&
#The Stata syntax file 'main_table1_table2.do' needs to be run first to produce
#the estimates that are plotted below
################################################################################

library(ggpubr)
library(cowplot)
library(rio)
library(tidyverse)
library(ggstance)

rm(list = ls())

################
#Functions
################

dy_func1 <- function(x) {
  x %>%
    dplyr::rename(
      ame = `_margin`, 
      lower = `_ci_lb`, 
      upper = `_ci_ub`, 
      predictor = `_deriv`, 
      pid = `_by1`) %>%
    mutate(pid = factor(pid, 
                        levels=c(1,2,3), 
                        labels=c("Co-Partisan",
                                 "Opposing Partisan", 
                                 "Non-Partisan"))) %>%
    select(ame, lower, upper, pid)
}


################
#V-Dem
################

####Government Satisfaction

reason_govsatis1 <- import("gov_satis_reason_dydx_m3_cluster1.dta") %>%
  dy_func1() %>%
  mutate(outcome = "Gov. Satisfaction", 
         predictor = "Explanations (V-Dem)", 
         model = "Model 1")

reason_govsatis2 <- import("gov_satis_reason_dydx_m3_cluster2.dta") %>%
  dy_func1() %>%
  mutate(outcome = "Gov. Satisfaction", 
         predictor = "Explanations (V-Dem)", 
         model = "Model 2")

reason_govsatis3 <- import("gov_satis_reason_dydx_m3_cluster3.dta") %>%
  dy_func1() %>%
  mutate(outcome = "Gov. Satisfaction", 
         predictor = "Explanations (V-Dem)", 
         model = "Model 3")

reason_govsatis4 <- import("gov_satis_reason_dydx_m3_cluster4.dta") %>%
  dy_func1() %>%
  mutate(outcome = "Gov. Satisfaction", 
         predictor = "Explanations (V-Dem)", 
         model = "Model 4")


####Democratic Satisfaction

reason_demsatis1 <- import("dem_satis_reason_dydx_m3_cluster1.dta") %>%
  dy_func1() %>%
  mutate(outcome = "Dem. Satisfaction", 
         predictor = "Explanations (V-Dem)", 
         model = "Model 1")

reason_demsatis2 <- import("dem_satis_reason_dydx_m3_cluster2.dta") %>%
  dy_func1() %>%
  mutate(outcome = "Dem. Satisfaction", 
         predictor = "Explanations (V-Dem)", 
         model = "Model 2")

reason_demsatis3 <- import("dem_satis_reason_dydx_m3_cluster3.dta") %>%
  dy_func1() %>%
  mutate(outcome = "Dem. Satisfaction", 
         predictor = "Explanations (V-Dem)", 
         model = "Model 3")

reason_demsatis4 <- import("dem_satis_reason_dydx_m3_cluster4.dta") %>%
  dy_func1() %>%
  mutate(outcome = "Dem. Satisfaction", 
         predictor = "Explanations (V-Dem)", 
         model = "Model 4")

################
#ESS
################

####Government Satisfaction

explain_govsatis3 <- import("gov_satis_explain_dydx_m3_cluster3.dta") %>%
  dy_func1() %>%
  mutate(outcome = "Gov. Satisfaction", 
         predictor = "Explanations (ESS)", 
         model = "Model 3")

explain_govsatis4 <- import("gov_satis_explain_dydx_m3_cluster4.dta") %>%
  dy_func1() %>%
  mutate(outcome = "Gov. Satisfaction", 
         predictor = "Explanations (ESS)", 
         model = "Model 4")

####Democratic Satisfaction

explain_demsatis3 <- import("dem_satis_explain_dydx_m3_cluster3.dta") %>%
  dy_func1() %>%
  mutate(outcome = "Dem. Satisfaction", 
         predictor = "Explanations (ESS)", 
         model = "Model 3")

explain_demsatis4 <- import("dem_satis_explain_dydx_m3_cluster4.dta") %>%
  dy_func1() %>%
  mutate(outcome = "Dem. Satisfaction", 
         predictor = "Explanations (ESS)", 
         model = "Model 4")


################
#Combine
################

dfs <- Filter(function(x) is(x, "data.frame"), mget(ls()))
ames <- do.call(rbind, dfs)

################
#Figures
################

##Figure OA6
ames %>% 
  filter(outcome == "Gov. Satisfaction") %>%
  ggplot(aes(x=ame, y=pid)) + 
  geom_pointrange(aes(xmin=lower, xmax=upper)) + 
  facet_grid(predictor ~ model) + 
  geom_vline(xintercept=0, linetype="dashed", color="red") + 
  geom_text(aes(label = round(ame,2)), vjust = -0.45) + 
  theme_bw(14) + 
  labs(x = "AME for Explanation Variable", 
       y = "Respondent Partisanship", 
       title = "Outcome = Government Satisfaction")

ggsave("figure_oa6.png", 
       height=8, width=12)

##Figure OA&
ames %>% 
  filter(outcome == "Dem. Satisfaction") %>%
  ggplot(aes(x=ame, y=pid)) + 
  geom_pointrange(aes(xmin=lower, xmax=upper)) + 
  facet_grid(predictor ~ model) + 
  geom_vline(xintercept=0, linetype="dashed", color="red") + 
  geom_text(aes(label = round(ame,2)), vjust = -0.45) + 
  theme_bw(14) + 
  labs(x = "AME for Explanation Variable", 
       y = "Respondent Partisanship", 
       title = "Outcome = Democratic Satisfaction")

ggsave("figure_oa7.png", 
       height=8, width=12)

#######################
#Remove data objects
#######################
rm(list = ls())

