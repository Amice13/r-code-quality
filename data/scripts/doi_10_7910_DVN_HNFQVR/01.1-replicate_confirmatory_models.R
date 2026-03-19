# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Replicate confirmatory models for paper according to PAP.
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
# - PAI-Misinfo-Feb2021_confirm.RData
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## >>> Correction from PAP: using two-sided tests

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

dat$bias_current1 <- scale(dat$bias_current1)
dat$bias_current2 <- scale(dat$bias_current2)
dat$bias_current3 <- scale(dat$bias_current3)

dat$support_idx <- scale(dat$support_idx)
dat$support_ambiv <- scale(dat$support_ambiv)
dat$shoulddecide_algos <- scale(dat$shoulddecide_algos)
dat$shoulddecide_users <- scale(dat$shoulddecide_users)

dat$inst_trust <- scale(dat$inst_trust)

#####------------------------------------------------------#
##### RQ1 (Partisanship) ####
#####------------------------------------------------------#

## H1.1: Stronger Republican self-identification predicts decreased
##       support for veracity labels and removal interventions.

## support
h1.1.mod.idx2pt <- lm(support_idx ~ demog_PID_2pt, 
				data=dat)
summary(h1.1.mod.idx2pt)
h1.1.df.idx2pt <- tidy(h1.1.mod.idx2pt)


h1.1.mod.idx6pt <- lm(support_idx ~ demog_PID_6pt, 
				data=dat)
summary(h1.1.mod.idx6pt)
h1.1.df.idx6pt <- tidy(h1.1.mod.idx6pt)


h1.1.mod.idx2pt.adj <- lm(support_idx ~ demog_PID_2pt + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, 
				data=dat)
summary(h1.1.mod.idx2pt.adj)
h1.1.df.idx2pt.adj <- tidy(h1.1.mod.idx2pt.adj)


h1.1.mod.idx6pt.adj <- lm(support_idx ~ demog_PID_6pt + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, 
				data=dat)
summary(h1.1.mod.idx6pt.adj)
h1.1.df.idx6pt.adj <- tidy(h1.1.mod.idx6pt.adj)


# H1.2: Stronger Republican self-identification predicts belief 
#       that interventions are biased by ideological lean of content.

## >>> Correction from PAP: disaggregating into separate 
## >>> since aggregate idx not super interpretable

###general bias
h1.2.mod.bias2pt1 <- lm(bias_current1 ~ demog_PID_2pt, 
					   data=dat)
summary(h1.2.mod.bias2pt1)
h1.2.df.bias2pt1 <- tidy(h1.2.mod.bias2pt1)


h1.2.mod.bias6pt1 <- lm(bias_current1 ~ demog_PID_6pt, 
					   data=dat)
summary(h1.2.mod.bias6pt1)
h1.2.df.bias6pt1 <- tidy(h1.2.mod.bias6pt1)


h1.2.mod.bias2pt1.adj <- lm(bias_current1 ~ demog_PID_2pt + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, 
						   data=dat)
summary(h1.2.mod.bias2pt1.adj)
h1.2.df.bias2pt1.adj <- tidy(h1.2.mod.bias2pt1.adj)


h1.2.mod.bias6pt1.adj <- lm(bias_current1 ~ demog_PID_6pt + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, 
						   data=dat)
summary(h1.2.mod.bias6pt1.adj)
h1.2.df.bias6pt1.adj <- tidy(h1.2.mod.bias6pt1.adj)


###bias against conservatives
h1.2.mod.bias2pt2 <- lm(bias_current2 ~ demog_PID_2pt, 
					   data=dat)
summary(h1.2.mod.bias2pt2)
h1.2.df.bias2pt2 <- tidy(h1.2.mod.bias2pt2)


h1.2.mod.bias6pt2 <- lm(bias_current2 ~ demog_PID_6pt, 
					   data=dat)
summary(h1.2.mod.bias6pt2)
h1.2.df.bias6pt2 <- tidy(h1.2.mod.bias6pt2)


h1.2.mod.bias2pt2.adj <- lm(bias_current2 ~ demog_PID_2pt + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, 
						   data=dat)
summary(h1.2.mod.bias2pt2.adj)
h1.2.df.bias2pt2.adj <- tidy(h1.2.mod.bias2pt2.adj)


h1.2.mod.bias6pt2.adj <- lm(bias_current2 ~ demog_PID_6pt + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, 
						   data=dat)
summary(h1.2.mod.bias6pt2.adj)
h1.2.df.bias6pt2.adj <- tidy(h1.2.mod.bias6pt2.adj)


###bias against liberals
h1.2.mod.bias2pt3 <- lm(bias_current3 ~ demog_PID_2pt, 
					   data=dat)
summary(h1.2.mod.bias2pt3)
h1.2.df.bias2pt3 <- tidy(h1.2.mod.bias2pt3)


h1.2.mod.bias6pt3 <- lm(bias_current3 ~ demog_PID_6pt, 
					   data=dat)
summary(h1.2.mod.bias6pt3)
h1.2.df.bias6pt3 <- tidy(h1.2.mod.bias6pt3)


h1.2.mod.bias2pt3.adj <- lm(bias_current3 ~ demog_PID_2pt + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, 
						   data=dat)
summary(h1.2.mod.bias2pt3.adj)
h1.2.df.bias2pt3.adj <- tidy(h1.2.mod.bias2pt3.adj)


h1.2.mod.bias6pt3.adj <- lm(bias_current3 ~ demog_PID_6pt + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, 
						   data=dat)
summary(h1.2.mod.bias6pt3.adj)
h1.2.df.bias6pt3.adj <- tidy(h1.2.mod.bias6pt3.adj)



#####------------------------------------------------------#
##### RQ2 (Institutional Trust) ####
#####------------------------------------------------------#


## H2.1:	Trust in American institutions (e.g. government, social media companies, academia) 
##          predicts increased support for interventions.

h2.1.mod.idxinst <- lm(support_idx ~ inst_trust, 
				data=dat)
summary(h2.1.mod.idxinst)
h2.1.df.idxinst <- tidy(h2.1.mod.idxinst)


h2.1.mod.idxinst.adj <- lm(support_idx ~ inst_trust + demog_PID + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, 
				data=dat)
summary(h2.1.mod.idxinst.adj)
h2.1.df.idxinst.adj <- tidy(h2.1.mod.idxinst.adj)

h2.1.mod.idxinst.adj.D <- lm(support_idx ~ inst_trust + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, 
				data=dat[dat$demog_PID == "Democrat",])
summary(h2.1.mod.idxinst.adj.D)
h2.1.df.idxinst.adj.D <- tidy(h2.1.mod.idxinst.adj.D)

h2.1.mod.idxinst.adj.R <- lm(support_idx ~ inst_trust + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, 
				data=dat[dat$demog_PID == "Republican",])
summary(h2.1.mod.idxinst.adj.R)
h2.1.df.idxinst.adj.R <- tidy(h2.1.mod.idxinst.adj.R)



## H2.2:	Trust in institutions predicts decreased support for interventions 
##          by platform users.

h2.2.mod.usersinst <- lm(shoulddecide_users ~ inst_trust, 
				data=dat)
summary(h2.2.mod.usersinst)
h2.2.df.usersinst <- tidy(h2.2.mod.usersinst)


h2.2.mod.usersinst.adj <- lm(shoulddecide_users ~ inst_trust + demog_PID + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, 
				data=dat)
summary(h2.2.mod.usersinst.adj)
h2.2.df.usersinst.adj <- tidy(h2.2.mod.usersinst.adj)

h2.2.mod.usersinst.adj.D <- lm(shoulddecide_users ~ inst_trust + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, 
				data=dat[dat$demog_PID == "Democrat",])
summary(h2.2.mod.usersinst.adj.D)
h2.2.df.usersinst.adj.D <- tidy(h2.2.mod.usersinst.adj.D)

h2.2.mod.usersinst.adj.R <- lm(shoulddecide_users ~ inst_trust + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, 
				data=dat[dat$demog_PID == "Republican",])
summary(h2.2.mod.usersinst.adj.R)
h2.2.df.usersinst.adj.R <- tidy(h2.2.mod.usersinst.adj.R)



## H2.3:	Distrust in institutions predicts increased support for interventions 
##			by algorithms.

h2.3.mod.algosinst <- lm(shoulddecide_algos ~ inst_trust, 
				data=dat)
summary(h2.3.mod.algosinst)
h2.3.df.algosinst <- tidy(h2.3.mod.algosinst)


h2.3.mod.algosinst.adj <- lm(shoulddecide_algos ~ inst_trust + demog_PID + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, 
				data=dat)
summary(h2.3.mod.algosinst.adj)
h2.3.df.algosinst.adj <- tidy(h2.3.mod.algosinst.adj)

h2.3.mod.algosinst.adj.D <- lm(shoulddecide_algos ~ inst_trust + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, 
				data=dat[dat$demog_PID == "Democrat",])
summary(h2.3.mod.algosinst.adj.D)
h2.3.df.algosinst.adj.D <- tidy(h2.3.mod.algosinst.adj.D)

h2.3.mod.algosinst.adj.R <- lm(shoulddecide_algos ~ inst_trust + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian, 
				data=dat[dat$demog_PID == "Republican",])
summary(h2.3.mod.algosinst.adj.R)
h2.3.df.algosinst.adj.R <- tidy(h2.3.mod.algosinst.adj.R)


#####------------------------------------------------------#
##### RQ3 (Media Preference) ####
#####------------------------------------------------------#

## H3.1:	Preference for conservative-slanting news outlets predicts 
##			decreased support for interventions.
h3.1.mod.idxcons <- lm(support_idx ~ I(news_slants_avg > 0),
					   data = dat)
summary(h3.1.mod.idxcons)
h3.1.df.idxcons <- tidy(h3.1.mod.idxcons)

h3.1.mod.idxcons2 <- lm(support_idx ~ news_slants_avg,
					   data = dat)
summary(h3.1.mod.idxcons2)
h3.1.df.idxcons2 <- tidy(h3.1.mod.idxcons2)


h3.1.mod.idxcons.adj <- lm(support_idx ~ I(news_slants_avg > 0) + demog_PID + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian,
					   data = dat)
summary(h3.1.mod.idxcons.adj)
h3.1.df.idxcons.adj <- tidy(h3.1.mod.idxcons.adj)

h3.1.mod.idxcons.adj2 <- lm(support_idx ~ news_slants_avg + demog_PID + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian,
					   data = dat)
summary(h3.1.mod.idxcons.adj2)
h3.1.df.idxcons.adj2 <- tidy(h3.1.mod.idxcons.adj2)

## H3.2:	Decreased engagement in news media consumption overall 
##			predicts ambivalence or indifference about interventions.
h3.2.mod.idxnews <- lm(support_ambiv ~ follow_news,
					   data = dat)
summary(h3.2.mod.idxnews)
h3.2.df.idxnews <- tidy(h3.2.mod.idxnews)


h3.2.mod.idxnews.adj <- lm(support_ambiv ~ follow_news + demog_PID + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian,
					   data = dat)
summary(h3.2.mod.idxnews.adj)
h3.2.df.idxnews.adj <- tidy(h3.2.mod.idxnews.adj)


h3.2.mod.idxnews2 <- lm(support_ambiv ~ news_freq,
					   data = dat)
summary(h3.2.mod.idxnews2)
h3.2.df.idxnews2 <- tidy(h3.2.mod.idxnews2)


h3.2.mod.idxnews2.adj <- lm(support_ambiv ~ news_freq + demog_PID + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian,
					   data = dat)
summary(h3.2.mod.idxnews2.adj)
h3.2.df.idxnews2.adj <- tidy(h3.2.mod.idxnews2.adj)


#####------------------------------------------------------#
##### RQ4 (Experience) ####
#####------------------------------------------------------#

## H4.1:	Exposure to interventions on congenial content predicts decreased 
##			support for interventions.
h4.1.mod.moder_good <- lm(support_idx ~ moder_good,
						 data = dat)
summary(h4.1.mod.moder_good)
h4.1.df.moder_good <- tidy(h4.1.mod.moder_good)

h4.1.mod.moder_good.adj <- lm(support_idx ~ moder_good + moder_bad + moder_you + moder_others + follow_disagree + demog_PID + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian,
								 data = dat)
summary(h4.1.mod.moder_good.adj)
h4.1.df.moder_good.adj <- tidy(h4.1.mod.moder_good.adj)

h4.1.mod.moder_good.adj.D <- lm(support_idx ~ moder_good + moder_bad + moder_you + moder_others + follow_disagree + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian,
								 data = dat[dat$demog_PID == "Democrat",])
summary(h4.1.mod.moder_good.adj.D)
h4.1.df.moder_good.adj.D <- tidy(h4.1.mod.moder_good.adj.D)

h4.1.mod.moder_good.adj.R <- lm(support_idx ~ moder_good + moder_bad + moder_you + moder_others + follow_disagree + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian,
								 data = dat[dat$demog_PID == "Republican",])
summary(h4.1.mod.moder_good.adj.R)
h4.1.df.moder_good.adj.R <- tidy(h4.1.mod.moder_good.adj.R)

h4.1.mod.moder_bad <- lm(support_idx ~ moder_bad,
						 data = dat)
summary(h4.1.mod.moder_bad)
h4.1.df.moder_bad <- tidy(h4.1.mod.moder_bad)


h4.1.mod.moder_bad.adj <- lm(support_idx ~ moder_bad + moder_good + moder_you + moder_others + follow_disagree + demog_PID + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian,
								 data = dat)
summary(h4.1.mod.moder_bad.adj)
h4.1.df.moder_bad.adj <- tidy(h4.1.mod.moder_bad.adj)
cor(dat$moder_bad, dat$moder_good, use="pairwise.complete.obs") ##0.52
cor(dat$moder_bad, dat$moder_you, use="pairwise.complete.obs") ##0.58


## H4.2: 	Receiving interventions predicts decreased support for interventions.
h4.2.mod.moder_you <- lm(support_idx ~ moder_you,
						 data = dat)
summary(h4.2.mod.moder_you)
h4.2.df.moder_you <- tidy(h4.2.mod.moder_you)


h4.2.mod.moder_you.adj <- lm(support_idx ~ moder_you + moder_bad + moder_good + moder_others + follow_disagree + demog_PID + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian,
						 data = dat)
summary(h4.2.mod.moder_you.adj)
h4.2.df.moder_you.adj <- tidy(h4.2.mod.moder_you.adj)


## H4.3:	Usage of interventions on others (e.g. muting, blocking) predicts 
##			increased support for interventions.
h4.3.mod.moder_others <- lm(support_idx ~ moder_others,
						 data = dat)
summary(h4.3.mod.moder_others)
h4.3.df.moder_others <- tidy(h4.3.mod.moder_others)


h4.3.mod.moder_others.adj <- lm(support_idx ~ moder_others + moder_bad + moder_good + moder_you + follow_disagree + demog_PID + demog_agegroup + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian,
						 data = dat)
summary(h4.3.mod.moder_others.adj)
h4.3.df.moder_others.adj <- tidy(h4.3.mod.moder_others.adj)


## H4.4:	Greater extensive margin of online platform usage predicts decreased 
##			support for interventions.
h4.4.mod.usage_n <- lm(support_idx ~ usage_n,
						 data = dat)
summary(h4.4.mod.usage_n)
h4.4.df.usage_n <- tidy(h4.4.mod.usage_n)


h4.4.mod.usage_n.adj <- lm(support_idx ~ usage_n + demog_PID + demog_agegroup + follow_disagree + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian,
						 data = dat)
summary(h4.4.mod.usage_n.adj)
h4.4.df.usage_n.adj <- tidy(h4.4.mod.usage_n.adj)

h4.4.mod.usage_n.adj.D <- lm(support_idx ~ usage_n + demog_agegroup + follow_disagree + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian,
						 data = dat[dat$demog_PID == "Democrat",])
summary(h4.4.mod.usage_n.adj.D)
h4.4.df.usage_n.adj.D <- tidy(h4.4.mod.usage_n.adj.D)

h4.4.mod.usage_n.adj.R <- lm(support_idx ~ usage_n + demog_agegroup + follow_disagree + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian,
						 data = dat[dat$demog_PID == "Republican",])
summary(h4.4.mod.usage_n.adj.R)
h4.4.df.usage_n.adj.R <- tidy(h4.4.mod.usage_n.adj.R)


## H4.5:	Greater intensive margin of online platform usage predicts decreased 
##			support for interventions.
h4.5.mod.usage_avg <- lm(support_idx ~ usage_avg, data = dat)
summary(h4.5.mod.usage_avg)
h4.5.df.usage_avg <- tidy(h4.5.mod.usage_avg)


h4.5.mod.usage_avg.adj <- lm(support_idx ~ usage_avg + demog_PID + demog_agegroup + follow_disagree + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian,
						 data = dat)
summary(h4.5.mod.usage_avg.adj)
h4.5.df.usage_avg.adj <- tidy(h4.5.mod.usage_avg.adj)

h4.5.mod.usage_avg.adj.D <- lm(support_idx ~ usage_avg + demog_agegroup + follow_disagree + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian,
						 data = dat[dat$demog_PID == "Democrat",])
summary(h4.5.mod.usage_avg.adj.D)
h4.5.df.usage_avg.adj.D <- tidy(h4.5.mod.usage_avg.adj.D)

h4.5.mod.usage_avg.adj.R <- lm(support_idx ~ usage_avg + demog_agegroup + follow_disagree + demog_gender + demog_income + demog_diglit + demog_race_white + demog_race_black + demog_race_asian,
						 data = dat[dat$demog_PID == "Republican",])
summary(h4.5.mod.usage_avg.adj.R)
h4.5.df.usage_avg.adj.R <- tidy(h4.5.mod.usage_avg.adj.R)


#####------------------------------------------------------#
##### Save ####
#####------------------------------------------------------#

save.image("PAI-Misinfo-Feb2021_confirm.RData")

