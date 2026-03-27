################################################################################
#Replication Materials for
#Explanation Giving Promotes Democratic Satisfaction Regardless of Respondent Partisanship
#
#Syntax produces:
#Figure OE1
#The Stata syntax file 'appendixE.do' needs to be run first to produce
#the estimates that are plotted below
################################################################################

rm(list = ls())

library(ggpubr)
library(cowplot)
library(rio)
library(tidyverse)
library(ggstance)

##############################################
#Marginal Effects
##############################################

dy_func1 <- function(x) {
  x %>%
    rename(
      ame = `_margin`, 
      lower = `_ci_lb`, 
      upper = `_ci_ub`)  %>%
    mutate(
      iv = ifelse(`_deriv` == 1, "Explanations (ESS)", "Explanations (VDem)"), 
      moderator = ifelse(`_by1` == 0, "Coalition", "Single Party")) %>%
    select(ame, lower, upper, iv, moderator)
}

demsatis_dydx <- import("dem_satis_both_dydx_coalition.dta") %>%
  dy_func1() %>%
  mutate(outcome = "Dem. Satisfaction")

govsatis_dydx <- import("gov_satis_both_dydx_coalition.dta") %>%
  dy_func1() %>%
  mutate(outcome = "Gov. Satisfaction")

comb_dydx <- bind_rows(demsatis_dydx, govsatis_dydx)

##Plot
p1 <- ggplot(comb_dydx, aes(x=ame, y=iv, shape=moderator)) + 
  geom_pointrangeh(aes(xmin=lower, xmax=upper), 
                   position = position_dodge2v(height = 0.2)) +
  facet_grid(~ outcome) + 
  theme_bw(14) + 
  labs(y = NULL, x = "AME for Explanations Variable", 
       shape = "Moderator") + 
  geom_vline(xintercept=0, linetype="dashed", color="red")

##############################################
#Predicted Probabilities
##############################################

pfunc <- function(x) {
  x %>%
    rename(
      prediction = `_margin`, 
      lower = `_ci_lb`, 
      upper = `_ci_ub`
    ) %>%
    mutate(
      moderator = ifelse(`_by1` == 0, "Coalition", "Single Party")) %>%
    select(prediction, lower, upper, moderator, `_at1`, `_at3`)
}

#Dem Satisfaction; Explanations (ESS)
dsatis_pred_explain <- import("dem_satis_explain_pred__both_coalition.dta") %>%
  pfunc() %>%
  mutate(outcome = "Dem. Satisfaction", 
         iv = "Explanations (ESS)") %>%
  rename(iv_value = `_at1`) %>%
  select(-`_at3`)

#Dem Satisfaction: Explanations (VDem)
dsatis_pred_reason <- import("dem_satis_reason_pred_both_coalition.dta") %>%
  pfunc() %>%
  mutate(outcome = "Dem. Satisfaction",
         iv = "Explanations (VDem)") %>%
  rename(iv_value = `_at3`) %>%
  select(-`_at1`)

#Government Satisfaction: Explanations (Vdem)
gsatis_pred_reason <- import("gov_satis_reason_pred_both_coalition.dta") %>%
  pfunc() %>%
  mutate(outcome = "Gov. Satisfaction",
         iv = "Explanations (VDem)") %>%
  rename(iv_value = `_at3`) %>%
  select(-`_at1`)

#Government Satisfaction: Explanations (ESS)
gsatis_pred_explain <- import("gov_satis_explain_pred__both_coalition.dta") %>%
  pfunc() %>%
  mutate(outcome = "Gov. Satisfaction", 
         iv = "Explanations (ESS)") %>%
  rename(iv_value = `_at1`) %>%
  select(-`_at3`)


pred_comb <- bind_rows(gsatis_pred_reason, gsatis_pred_explain, 
                       dsatis_pred_reason, dsatis_pred_explain)


p2 <- ggplot(pred_comb, aes(x=iv_value, y=prediction, shape=moderator, linetype=moderator)) + 
  geom_pointrange(aes(ymin=lower, ymax=upper), 
                  position = position_dodge(width=0.1)) + 
  geom_line() + 
  facet_grid(iv ~ outcome) + 
  theme_bw(14) + 
  labs(shape = "Moderator", 
       linetype = "Moderator", 
       y = "Predicted Value", 
       x = "Value of Explanations Measure")

##############################################
#Combined Figure
##############################################

ggarrange(p1, p2, labels=c("A", "B"), common.legend = T)

ggsave("figure_oe1.png", height=8, width=14)


#######################
#Remove data objects
#######################
rm(list = ls())

