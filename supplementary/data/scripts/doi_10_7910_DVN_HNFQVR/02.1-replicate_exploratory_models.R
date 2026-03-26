# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Replicate exploratory models for paper according to PAP.
# 
# Author: Soubhik Barari
# 
# Environment:
# - must use R 3.6
# 
# Input:
# - PAI-Misinfo-Feb2021.RData
#
# Output:
# - PAI-Misinfo-Feb2021_explore.RData
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#####------------------------------------------------------#
##### Pre-amble ####
#####------------------------------------------------------#

rm(list=ls())

library(broom)
library(stargazer)
library(ggplot2)
library(tidyverse)

load("PAI-Misinfo-Feb2021.RData")

agree_levels <- c("Strongly disagree","Somewhat disagree","Neither agree nor disagree","Somewhat agree","Strongly agree")
approve_levels <- c("Strongly disapprove","Somewhat disapprove","Neither approve nor disapprove","Somewhat approve","Strongly approve")
freq_levels <- c("Not Sure", "Never", "Rarely", "Sometimes", "Often", "Always")
freq2_levels <- c("Not sure", "Never", "Less often than every few weeks", "Every few weeks at most", "A few times a week", "About once a day", "Several times a day")

#####------------------------------------------------------#
##### RQ1 (Partisanship) -- disaggregate into interventions ####
#####------------------------------------------------------#

## H1.1: Stronger Republican self-identification predicts decreased
##       support for veracity labels and removal interventions.

## labels
h1.1.mod.lab2pt <- lm(labels_approve ~ demog_PID_2pt, 
				data=dat)
summary(h1.1.mod.lab2pt)
h1.1.df.lab2pt <- tidy(h1.1.mod.lab2pt)


h1.1.mod.lab6pt <- lm(labels_approve ~ demog_PID_6pt, 
				data=dat)
summary(h1.1.mod.lab6pt)
h1.1.df.lab6pt <- tidy(h1.1.mod.lab6pt)


h1.1.mod.lab2pt.adj <- lm(labels_approve ~ demog_PID_2pt + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, 
				data=dat)
summary(h1.1.mod.lab2pt.adj)
h1.1.df.lab2pt.adj <- tidy(h1.1.mod.lab2pt.adj)


h1.1.mod.lab6pt.adj <- lm(labels_approve ~ demog_PID_6pt + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, 
				data=dat)
summary(h1.1.mod.lab6pt.adj)
h1.1.df.lab6pt.adj <- tidy(h1.1.mod.lab6pt.adj)

## remove
h1.1.mod.rem2pt <- lm(remove_approve ~ demog_PID_2pt, 
				data=dat)
summary(h1.1.mod.rem2pt)
h1.1.df.rem2pt <- tidy(h1.1.mod.lab2pt)


h1.1.mod.rem6pt <- lm(remove_approve ~ demog_PID_6pt, 
				data=dat)
summary(h1.1.mod.rem6pt)
h1.1.df.rem6pt <- tidy(h1.1.mod.rem6pt)


h1.1.mod.rem2pt.adj <- lm(remove_approve ~ demog_PID_2pt + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, 
				data=dat)
summary(h1.1.mod.rem2pt.adj)
h1.1.df.rem2pt.adj <- tidy(h1.1.mod.rem2pt.adj)


h1.1.mod.rem6pt.adj <- lm(remove_approve ~ demog_PID_6pt + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, 
				data=dat)
summary(h1.1.mod.rem6pt.adj)
h1.1.df.rem6pt.adj <- tidy(h1.1.mod.rem6pt.adj)

## downrank
h1.1.mod.rank2pt <- lm(downrank_approve ~ demog_PID_2pt, 
				data=dat)
summary(h1.1.mod.rank2pt)
h1.1.df.rank2pt <- tidy(h1.1.mod.rank2pt)


h1.1.mod.rank6pt <- lm(downrank_approve ~ demog_PID_6pt, 
				data=dat)
summary(h1.1.mod.rank6pt)
h1.1.df.rank6pt <- tidy(h1.1.mod.rank6pt)


h1.1.mod.rank2pt.adj <- lm(downrank_approve ~ demog_PID_2pt + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, 
				data=dat)
summary(h1.1.mod.rank2pt.adj)
h1.1.df.rank2pt.adj <- tidy(h1.1.mod.rank2pt.adj)


h1.1.mod.rank6pt.adj <- lm(downrank_approve ~ demog_PID_6pt + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, 
				data=dat)
summary(h1.1.mod.rank6pt.adj)
h1.1.df.rank6pt.adj <- tidy(h1.1.mod.rank6pt.adj)

## evaluation
h1.1.mod.correct6pt <- lm(labels_eval_1 ~ demog_PID_6pt, data=dat)
summary(h1.1.mod.correct6pt)
h1.1.df.correct6pt <- tidy(h1.1.mod.correct6pt)

h1.1.mod.incorrect6pt <- lm(labels_eval_2 ~ demog_PID_6pt, data=dat)
summary(h1.1.mod.incorrect6pt)
h1.1.df.incorrect6pt <- tidy(h1.1.mod.incorrect6pt)

h1.1.mod.informyou6pt <- lm(labels_eval_3 ~ demog_PID_6pt, data=dat)
summary(h1.1.mod.informyou6pt)
h1.1.df.informyou6pt <- tidy(h1.1.mod.informyou6pt)

h1.1.mod.informothers6pt <- lm(labels_eval_4 ~ demog_PID_6pt, data=dat)
summary(h1.1.mod.informothers6pt)
h1.1.df.informothers6pt <- tidy(h1.1.mod.informothers6pt)

h1.1.mod.correct6pt.adj <- lm(labels_eval_1 ~ demog_PID_6pt + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, data=dat)
summary(h1.1.mod.correct6pt.adj)
h1.1.df.correct6pt.adj <- tidy(h1.1.mod.correct6pt.adj)

h1.1.mod.incorrect6pt.adj <- lm(labels_eval_2 ~ demog_PID_6pt + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, data=dat)
summary(h1.1.mod.incorrect6pt.adj)
h1.1.df.incorrect6pt.adj <- tidy(h1.1.mod.incorrect6pt.adj)

h1.1.mod.informyou6pt.adj <- lm(labels_eval_3 ~ demog_PID_6pt + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, data=dat)
summary(h1.1.mod.informyou6pt.adj)
h1.1.df.informyou6pt.adj <- tidy(h1.1.mod.informyou6pt.adj)

h1.1.mod.informothers6pt.adj <- lm(labels_eval_4 ~ demog_PID_6pt + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, data=dat)
summary(h1.1.mod.informothers6pt.adj)
h1.1.df.informothers6pt.adj <- tidy(h1.1.mod.informothers6pt.adj)


## other parts
h1.1.mod.falsepos6pt <- lm(falsepos_approve ~ demog_PID_6pt, data=dat)
summary(h1.1.mod.falsepos6pt)
h1.1.df.falsepos6pt <- tidy(h1.1.mod.falsepos6pt)

h1.1.mod.type6pt <- lm(labels_approve_type ~ demog_PID_6pt, data=dat)
summary(h1.1.mod.type6pt)
h1.1.df.type6pt <- tidy(h1.1.mod.type6pt)

h1.1.mod.falsepos6pt.adj <- lm(falsepos_approve ~ demog_PID_6pt + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, data=dat)
summary(h1.1.mod.falsepos6pt.adj)
h1.1.df.falsepos6pt.adj <- tidy(h1.1.mod.falsepos6pt.adj)

h1.1.mod.type6pt.adj <- lm(labels_approve_type ~ demog_PID_6pt + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, data=dat)
summary(h1.1.mod.type6pt.adj)
h1.1.df.type6pt.adj <- tidy(h1.1.mod.type6pt.adj)

#####------------------------------------------------------#
##### RQ2 (Institutional Trust) -- disaggregate institutions ####
#####------------------------------------------------------#

## H2.1:	Trust in American institutions (e.g. government, social media companies, academia) 
##          predicts increased support for interventions.

h2.1.mod.idxinst1 <- lm(support_idx ~ confid_inst_academics, data=dat)
summary(h2.1.mod.idxinst1)
h2.1.df.idxinst1 <- tidy(h2.1.mod.idxinst1)

h2.1.mod.idxinst2 <- lm(support_idx ~ confid_inst_electoffic, data=dat)
summary(h2.1.mod.idxinst2)
h2.1.df.idxinst2 <- tidy(h2.1.mod.idxinst2)

h2.1.mod.idxinst3 <- lm(support_idx ~ confid_inst_factcheckers, data=dat)
summary(h2.1.mod.idxinst3)
h2.1.df.idxinst3 <- tidy(h2.1.mod.idxinst3)

h2.1.mod.idxinst4 <- lm(support_idx ~ confid_inst_localnews, data=dat)
summary(h2.1.mod.idxinst4)
h2.1.df.idxinst4 <- tidy(h2.1.mod.idxinst4)

h2.1.mod.idxinst5 <- lm(support_idx ~ confid_inst_msm, data=dat)
summary(h2.1.mod.idxinst5)
h2.1.df.idxinst5 <- tidy(h2.1.mod.idxinst5)

h2.1.mod.idxinst6 <- lm(support_idx ~ confid_inst_pplfollow, data=dat)
summary(h2.1.mod.idxinst6)
h2.1.df.idxinst6 <- tidy(h2.1.mod.idxinst6)

h2.1.mod.idxinst7 <- lm(support_idx ~ confid_inst_supremecourt, data=dat)
summary(h2.1.mod.idxinst7)
h2.1.df.idxinst7 <- tidy(h2.1.mod.idxinst7)

h2.1.mod.idxinst8 <- lm(support_idx ~ confid_inst_socmedcompanies, data=dat)
summary(h2.1.mod.idxinst8)
h2.1.df.idxinst8 <- tidy(h2.1.mod.idxinst8)

h2.1.mod.idxinst.adj <- lm(support_idx ~ confid_inst_academics + confid_inst_electoffic + confid_inst_factcheckers + confid_inst_localnews + confid_inst_msm + confid_inst_pplfollow + confid_inst_supremecourt + confid_inst_socmedcompanies + demog_PID + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, data=dat)
summary(h2.1.mod.idxinst.adj)
h2.1.df.idxinst.adj <- tidy(h2.1.mod.idxinst.adj)

h2.1.mod.idxinst.adj2 <- lm(support_idx ~ confid_inst_academics + confid_inst_electoffic + confid_inst_factcheckers + confid_inst_localnews + confid_inst_msm + confid_inst_pplfollow + confid_inst_supremecourt + confid_inst_socmedcompanies + demog_PID + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, data=dat)
summary(h2.1.mod.idxinst.adj2)
h2.1.df.idxinst.adj2 <- tidy(h2.1.mod.idxinst.adj2)


## H2.2:	Trust in institutions predicts decreased support for interventions 
##          by platform users.

h2.1.mod.usersinst1 <- lm(shoulddecide_users ~ confid_inst_academics, data=dat)
summary(h2.1.mod.usersinst1)
h2.1.df.usersinst1 <- tidy(h2.1.mod.usersinst1)

h2.1.mod.usersinst2 <- lm(shoulddecide_users ~ confid_inst_electoffic, data=dat)
summary(h2.1.mod.usersinst2)
h2.1.df.usersinst2 <- tidy(h2.1.mod.usersinst2)

h2.1.mod.usersinst3 <- lm(shoulddecide_users ~ confid_inst_factcheckers, data=dat)
summary(h2.1.mod.usersinst3)
h2.1.df.usersinst3 <- tidy(h2.1.mod.usersinst3)

h2.1.mod.usersinst4 <- lm(shoulddecide_users ~ confid_inst_localnews, data=dat)
summary(h2.1.mod.usersinst4)
h2.1.df.usersinst4 <- tidy(h2.1.mod.usersinst4)

h2.1.mod.usersinst5 <- lm(shoulddecide_users ~ confid_inst_msm, data=dat)
summary(h2.1.mod.usersinst5)
h2.1.df.usersinst5 <- tidy(h2.1.mod.usersinst5)

h2.1.mod.usersinst6 <- lm(shoulddecide_users ~ confid_inst_pplfollow, data=dat)
summary(h2.1.mod.usersinst6)
h2.1.df.usersinst6 <- tidy(h2.1.mod.usersinst6)

h2.1.mod.usersinst7 <- lm(shoulddecide_users ~ confid_inst_supremecourt, data=dat)
summary(h2.1.mod.usersinst7)
h2.1.df.usersinst7 <- tidy(h2.1.mod.usersinst7)

h2.1.mod.usersinst8 <- lm(shoulddecide_users ~ confid_inst_socmedcompanies, data=dat)
summary(h2.1.mod.usersinst8)
h2.1.df.usersinst8 <- tidy(h2.1.mod.usersinst8)

h2.1.mod.usersinst.adj <- lm(shoulddecide_users ~ confid_inst_academics + confid_inst_electoffic + confid_inst_factcheckers + confid_inst_localnews + confid_inst_msm + confid_inst_pplfollow + confid_inst_supremecourt + confid_inst_socmedcompanies + demog_PID + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, data=dat)
summary(h2.1.mod.usersinst.adj)
h2.1.df.usersinst.adj <- tidy(h2.1.mod.usersinst.adj)

h2.1.mod.usersinst.adj2 <- lm(shoulddecide_users ~ confid_inst_academics + confid_inst_electoffic + confid_inst_factcheckers + confid_inst_localnews + confid_inst_msm + confid_inst_pplfollow + confid_inst_supremecourt + confid_inst_socmedcompanies + demog_PID + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, data=dat)
summary(h2.1.mod.usersinst.adj2)
h2.1.df.usersinst.adj2 <- tidy(h2.1.mod.usersinst.adj2)


h2.1.mod.algosinst1 <- lm(shoulddecide_algos ~ confid_inst_academics, data=dat)
summary(h2.1.mod.algosinst1)
h2.1.df.algosinst1 <- tidy(h2.1.mod.algosinst1)

h2.1.mod.algosinst2 <- lm(shoulddecide_algos ~ confid_inst_electoffic, data=dat)
summary(h2.1.mod.algosinst2)
h2.1.df.algosinst2 <- tidy(h2.1.mod.algosinst2)

h2.1.mod.algosinst3 <- lm(shoulddecide_algos ~ confid_inst_factcheckers, data=dat)
summary(h2.1.mod.algosinst3)
h2.1.df.algosinst3 <- tidy(h2.1.mod.algosinst3)

h2.1.mod.algosinst4 <- lm(shoulddecide_algos ~ confid_inst_localnews, data=dat)
summary(h2.1.mod.algosinst4)
h2.1.df.algosinst4 <- tidy(h2.1.mod.algosinst4)

h2.1.mod.algosinst5 <- lm(shoulddecide_algos ~ confid_inst_msm, data=dat)
summary(h2.1.mod.algosinst5)
h2.1.df.algosinst5 <- tidy(h2.1.mod.algosinst5)

h2.1.mod.algosinst6 <- lm(shoulddecide_algos ~ confid_inst_pplfollow, data=dat)
summary(h2.1.mod.algosinst6)
h2.1.df.algosinst6 <- tidy(h2.1.mod.algosinst6)

h2.1.mod.algosinst7 <- lm(shoulddecide_algos ~ confid_inst_supremecourt, data=dat)
summary(h2.1.mod.algosinst7)
h2.1.df.algosinst7 <- tidy(h2.1.mod.algosinst7)

h2.1.mod.algosinst8 <- lm(shoulddecide_algos ~ confid_inst_socmedcompanies, data=dat)
summary(h2.1.mod.algosinst8)
h2.1.df.algosinst8 <- tidy(h2.1.mod.algosinst8)

h2.1.mod.algosinst.adj <- lm(shoulddecide_algos ~ confid_inst_academics + confid_inst_electoffic + confid_inst_factcheckers + confid_inst_localnews + confid_inst_msm + confid_inst_pplfollow + confid_inst_supremecourt + confid_inst_socmedcompanies + demog_PID + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, data=dat)
summary(h2.1.mod.algosinst.adj)
h2.1.df.algosinst.adj <- tidy(h2.1.mod.algosinst.adj)

h2.1.mod.algosinst.adj2 <- lm(shoulddecide_algos ~ confid_inst_academics + confid_inst_electoffic + confid_inst_factcheckers + confid_inst_localnews + confid_inst_msm + confid_inst_pplfollow + confid_inst_supremecourt + confid_inst_socmedcompanies + demog_PID + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, data=dat)
summary(h2.1.mod.algosinst.adj2)
h2.1.df.algosinst.adj2 <- tidy(h2.1.mod.algosinst.adj2)

#####------------------------------------------------------#
##### RQ4 (Experience) -- disaggregate support ####
#####------------------------------------------------------#

##TODO

#####------------------------------------------------------#
##### Other ####
#####------------------------------------------------------#

## marginal fx of PID on encounters 
mod.fb1 <- lm(labels_freq_FB ~ demog_PID_6pt, 
   data = dat %>% mutate(labels_freq_FB=scale(as.numeric(factor(labels_freq_FB, levels=freq2_levels)))))
mod.fb2 <- lm(labels_freq_FB ~ demog_PID_6pt  + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, 
   data = dat %>% mutate(labels_freq_FB=scale(as.numeric(factor(labels_freq_FB, levels=freq2_levels)))))

mod.ig1 <- lm(labels_freq_IG ~ demog_PID_6pt, 
   data = dat %>% mutate(labels_freq_IG=scale(as.numeric(factor(labels_freq_IG, levels=freq2_levels)))))
mod.ig2 <- lm(labels_freq_IG ~ demog_PID_6pt + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, 
   data = dat %>% mutate(labels_freq_IG=scale(as.numeric(factor(labels_freq_IG, levels=freq2_levels)))))

mod.yt1 <- lm(labels_freq_YT ~ demog_PID_6pt, 
   data = dat %>% mutate(labels_freq_YT=scale(as.numeric(factor(labels_freq_YT, levels=freq2_levels)))))
mod.yt2 <- lm(labels_freq_YT ~ demog_PID_6pt + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, 
   data = dat %>% mutate(labels_freq_YT=scale(as.numeric(factor(labels_freq_YT, levels=freq2_levels)))))

mod.tk1 <- lm(labels_freq_TK ~ demog_PID_6pt, 
   data = dat %>% mutate(labels_freq_TK=scale(as.numeric(factor(labels_freq_TK, levels=freq2_levels)))))
mod.tk2 <- lm(labels_freq_TK ~ demog_PID_6pt + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, 
   data = dat %>% mutate(labels_freq_TK=scale(as.numeric(factor(labels_freq_TK, levels=freq2_levels)))))

mod.tw1 <- lm(labels_freq_TW ~ demog_PID_6pt, 
   data = dat %>% mutate(labels_freq_TW=scale(as.numeric(factor(labels_freq_TW, levels=freq2_levels)))))
mod.tw2 <- lm(labels_freq_TW ~ demog_PID_6pt + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, 
   data = dat %>% mutate(labels_freq_TW=scale(as.numeric(factor(labels_freq_TW, levels=freq2_levels)))))

## marginal fx of PID on specific support
mod.lab1 <- lm(labels_approve ~ demog_PID_6pt, 
   data = dat %>% mutate(labels_approve = scale(labels_approve)))
mod.lab2 <- lm(labels_approve ~ demog_PID_6pt + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, 
   data = dat %>% mutate(labels_approve = scale(labels_approve)))

mod.rem1 <- lm(remove_approve ~ demog_PID_6pt, 
   data = dat %>% mutate(remove_approve = scale(remove_approve)))
mod.rem2 <- lm(remove_approve ~ demog_PID_6pt + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, 
   data = dat %>% mutate(remove_approve = scale(remove_approve)))

mod.dr1 <- lm(downrank_approve ~ demog_PID_6pt, 
   data = dat %>% mutate(downrank_approve = scale(downrank_approve)))
mod.dr2 <- lm(downrank_approve ~ demog_PID_6pt + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, 
   data = dat %>% mutate(downrank_approve = scale(downrank_approve)))


#####------------------------------------------------------#
##### Save ####
#####------------------------------------------------------#

save.image("PAI-Misinfo-Feb2021_explore.RData")

