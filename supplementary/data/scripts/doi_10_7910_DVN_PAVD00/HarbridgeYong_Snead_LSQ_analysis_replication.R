##HarbridgeYong_Snead_LSQ_analysis_replication.R
##Replication code for LSQ article "Interest Groups and Intra-Party Conflict on Scored Votes"

##libraries used
library(tidyverse)
library(ggplot2)
library(car) ##note that blocks recode from dplyr
library(lmtest)
library(sandwich)
library(modelsummary)
library(flextable)
library(logistf)

##Working Directory (set to where data is stored)
setwd("C:/Users/lmh735/OneDrive - Northwestern University/_FromNUBox/Interest Groups and Party Conflict Spring 2020/Drafts/LSQ Submission/RandR/RandR_submission/RandR_round2/Replication Files LSQ")
##data
data<- read.csv("HarbridgeYong_Snead_LSQ_data.csv", header=TRUE)
names(data)


########################################
## Some notes on key variables        ##
########################################
##See Online Supplemental Appendix for more details on variable descriptions and variable sources

##Variables about interest group agreement or disagreement:
##When leadership disagree, number of interest groups agreeing or disagreeing with leadership should be NA
##Recodes num_ig_l_agree_maj [min] and num_ig_l_disagree_maj [min] to NA if leadership_agrees_maj [min] is 0
##number of groups agreeing
summary(as.factor(data$num_ig_l_agree_NA_maj))
summary(as.factor(data$num_ig_l_agree_NA_min))
summary(data$num_ig_l_agree_NA_maj)
summary(data$num_ig_l_agree_NA_min)

##number of groups disagreeing
summary(as.factor(data$num_ig_l_disagree_NA_maj))
summary(as.factor(data$num_ig_l_disagree_NA_min))
summary(data$num_ig_l_disagree_NA_maj)
summary(data$num_ig_l_disagree_NA_min)

##binary of 1+ aligned groups disagreed
summary(as.factor(data$one_plus_ig_l_disagree_NA_maj)) ##footnote 12
summary(as.factor(data$one_plus_ig_l_disagree_NA_min))

##binary of 1+ aligned groups agreeing
summary(as.factor(data$one_plus_ig_l_agree_NA_maj))
summary(as.factor(data$one_plus_ig_l_agree_NA_min))

#############
##majortopic - corrects two errors: one with 2005 that should be 20 and one with 2000 that should be 14
summary(as.factor(data$majortopic))

#############
##Extremity of NOMINATE
#data$ext_nominate<- ifelse(data$nominate_dim1 < 0, (-1*data$nominate_dim1), data$nominate_dim1)
summary(data$nominate_dim1)
summary(data$ext_nominate)
##Note that in constructing this measure, this assumes no Democrats have a score > 0 and no Republicans have a score < 0. 
##Ind (Sanders and King caucus with Dems and have nominate scores < 0 ). 
##2 obs where Democratic House members have NOMINATE score > 0
## Bobby Bright (111th House (0.067))
## Walt Minnick (111th House (0.088))
## However, neither is sponsor of the bills that receive a scored vote in our sample


#####################################
##     Descriptive Statistics      ##
#####################################
##Appendix Tables C1 and C2

###################
##Majority Party
###################   
##NOTE: vars (except leadership agreement) should also subset on leaders agreeing (since analysis does)
##NOTE: convert proportion to percent

##percent passage votes
summary(data$vote_passage[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1]) 
length(na.omit(data$vote_passage[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1]))
sd(data$vote_passage[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1], na.rm=TRUE)

##percent passage_constrained
summary(data$vote_passage_constrained[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1]) 
length(na.omit(data$vote_passage_constrained[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1]))
sd(data$vote_passage_constrained[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1], na.rm=TRUE)

##percent other category votes
summary(data$vote_other[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1])
length(na.omit(data$vote_other[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1]))
sd(data$vote_other[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1], na.rm=TRUE)

##Extremity of NOMINATE
summary(data$ext_nominate[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1])
length(na.omit(data$ext_nominate[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1]))
sd(data$ext_nominate[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1], na.rm=TRUE)
##Note: missing data when unable to merge. Mostly presidential nominations
summary(as.factor(data$billtype1_lower_correct[is.na(data$ext_nominate) & data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1]))

##important bill indicator
summary(data$ImpBill[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1])
length(na.omit(data$ImpBill[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1]))
sd(data$ImpBill[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1], na.rm=TRUE)

##CQ story
summary(data$cq_story[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1])
length(na.omit(data$cq_story[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1]))
sd(data$cq_story[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1], na.rm=TRUE)

##chamber
summary(data$senate[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1])
length(na.omit(data$senate[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1]))
sd(data$senate[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1], na.rm=TRUE)

##divided congress
summary(data$divided_cong[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1])
length(na.omit(data$divided_cong[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1]))
sd(data$divided_cong[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1], na.rm=TRUE)

##Rate of leadership agreement (majority) - note that both not voting is in 0, as is yay/nay and yay or nay/not voting
summary(data$leadership_agrees_maj[data$num_ig_aligned_maj>0])
length(na.omit(data$leadership_agrees_maj[data$num_ig_aligned_maj>0]))
sd(data$leadership_agrees_maj[data$num_ig_aligned_maj>0], na.rm=TRUE)
##Number where both leaders in party did not vote (majority)
summary(as.factor(data$leadership_vote_cat_maj[data$num_ig_aligned_maj>0])) ##footnote 6

##Leadership agreement included in first measure, built in by construction on others
##summary of number of aligned groups scoring vote (majority)
summary(data$num_ig_aligned_maj[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1])
length(na.omit(data$num_ig_aligned_maj[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1]))
sd(data$num_ig_aligned_maj[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1], na.rm=TRUE)

##summary of number of aligned groups that agree with leadership in scoring (majority) - note that these are NA if leadership disagrees internally
summary(data$num_ig_l_agree_NA_maj[data$num_ig_aligned_maj>0]) ##also in text
length(na.omit(data$num_ig_l_agree_NA_maj[data$num_ig_aligned_maj>0]))
sd(data$num_ig_l_agree_NA_maj[data$num_ig_aligned_maj>0], na.rm=TRUE)

##summary of number of aligned groups that disagree with leadership in scoring (majority) - note that these are NA if leadership disagrees internally
summary(data$num_ig_l_disagree_NA_maj[data$num_ig_aligned_maj>0]) ##also in text
length(na.omit(data$num_ig_l_disagree_NA_maj[data$num_ig_aligned_maj>0]))
sd(data$num_ig_l_disagree_NA_maj[data$num_ig_aligned_maj>0], na.rm=TRUE)

##summary of 1+ groups disagreeing (majority)
summary(data$one_plus_ig_l_disagree_NA_maj[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1])
length(na.omit(data$one_plus_ig_l_disagree_NA_maj[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1]))
sd(data$one_plus_ig_l_disagree_NA_maj[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1], na.rm=TRUE)

#Ryan committee cohesion
summary(data$maj_intra_agree_mean[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1])
length(na.omit(data$maj_intra_agree_mean[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1]))
sd(data$maj_intra_agree_mean[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1], na.rm=TRUE)

##voting cohesion
summary(data$vote_leader_pct_maj[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1])
length(na.omit(data$vote_leader_pct_maj[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1])) 
sd(data$vote_leader_pct_maj[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1], na.rm=TRUE)

##majortopic codes (for IG) (Table C2)
summary(as.factor(data$majortopic_ig[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1]))
length(na.omit(as.factor(data$majortopic_ig[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1])))
summary(as.factor(data$majortopic_ig[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1]))/
  length(na.omit(as.factor(data$majortopic_ig[data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1])))

##################
##Minority Party
##################
##percent passage votes
summary(data$vote_passage[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1]) 
length(na.omit(data$vote_passage[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1]))
sd(data$vote_passage[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1], na.rm=TRUE)

##percent passage_constrained
summary(data$vote_passage_constrained[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1]) 
length(na.omit(data$vote_passage_constrained[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1]))
sd(data$vote_passage_constrained[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1], na.rm=TRUE)

##percent other category votes
summary(data$vote_other[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1])
length(na.omit(data$vote_other[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1]))
sd(data$vote_other[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1], na.rm=TRUE)

##Extremity of NOMINATE
summary(data$ext_nominate[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1])
length(na.omit(data$ext_nominate[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1]))
sd(data$ext_nominate[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1], na.rm=TRUE)
##missing data when unable to merge. Mostly presidential nominations
summary(as.factor(data$billtype1_lower_correct[is.na(data$ext_nominate) & data$num_ig_aligned_min>0 & data$leadership_agrees_min==1]))

##important bill indicator
summary(data$ImpBill[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1])
length(na.omit(data$ImpBill[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1]))
sd(data$ImpBill[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1], na.rm=TRUE)

##CQ story
summary(data$cq_story[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1])
length(na.omit(data$cq_story[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1]))
sd(data$cq_story[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1], na.rm=TRUE)

##chamber
summary(data$senate[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1])
length(na.omit(data$senate[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1]))
sd(data$senate[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1], na.rm=TRUE)

##divided congress
summary(data$divided_cong[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1])
length(na.omit(data$divided_cong[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1]))
sd(data$divided_cong[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1], na.rm=TRUE)

##Rate of leadership agreement (minority) - note that both not voting is in 0, as is yay/nay and yay or nay/not voting
summary(data$leadership_agrees_min[data$num_ig_aligned_min>0])
length(na.omit(data$leadership_agrees_min[data$num_ig_aligned_min>0]))
sd(data$leadership_agrees_min[data$num_ig_aligned_min>0], na.rm=TRUE)
##Number where both leaders in party did not vote (minority)
summary(as.factor(data$leadership_vote_cat_min[data$num_ig_aligned_min>0])) ##footnote 6

##Leadership agreement included in first measure, built in by construction on others
##summary of number of aligned groups scoring vote (minority)
summary(data$num_ig_aligned_min[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1])
length(na.omit(data$num_ig_aligned_min[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1]))
sd(data$num_ig_aligned_min[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1], na.rm=TRUE)

##summary of number of aligned groups that agree with leadership in scoring (minority) - note that these are NA if leadership disagrees internally
summary(data$num_ig_l_agree_NA_min[data$num_ig_aligned_min>0]) ##also in text
length(na.omit(data$num_ig_l_agree_NA_min[data$num_ig_aligned_min>0]))
sd(data$num_ig_l_agree_NA_min[data$num_ig_aligned_min>0], na.rm=TRUE)

##summary of number of aligned groups that disagree with leadership in scoring (minority) - note that these are NA if leadership disagrees internally
summary(data$num_ig_l_disagree_NA_min[data$num_ig_aligned_min>0]) ##also in text
length(na.omit(data$num_ig_l_disagree_NA_min[data$num_ig_aligned_min>0]))
sd(data$num_ig_l_disagree_NA_min[data$num_ig_aligned_min>0], na.rm=TRUE)

##summary of 1+ groups disagreeing (minority)
summary(data$one_plus_ig_l_disagree_NA_min[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1])
length(na.omit(data$one_plus_ig_l_disagree_NA_min[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1]))
sd(data$one_plus_ig_l_disagree_NA_min[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1], na.rm=TRUE)

#Ryan committee cohesion
summary(data$min_intra_agree_mean[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1])
length(na.omit(data$min_intra_agree_mean[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1]))
sd(data$min_intra_agree_mean[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1], na.rm=TRUE)

##voting cohesion
summary(data$vote_leader_pct_min[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1])
length(na.omit(data$vote_leader_pct_min[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1]))
sd(data$vote_leader_pct_min[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1], na.rm=TRUE)

##majortopic codes (for IG) (Table C2)
summary(as.factor(data$majortopic_ig[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1]))
length(na.omit(as.factor(data$majortopic_ig[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1])))
summary(as.factor(data$majortopic_ig[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1]))/
  length(na.omit(as.factor(data$majortopic_ig[data$num_ig_aligned_min>0 & data$leadership_agrees_min==1])))


#####################################
##   Descriptive patterns          ##
#####################################
##How often are groups in our data scoring in opposition to each other (e.g., some agree with leader and others disagree)?
##Majority (Table C4)
cross_press_maj<- data %>%
  select(num_ig_aligned_maj, num_ig_l_agree_NA_maj, num_ig_l_disagree_NA_maj) %>%
  filter(num_ig_aligned_maj>0) %>%
  count(num_ig_l_agree_NA_maj, num_ig_l_disagree_NA_maj) %>%
  pivot_wider(names_from = num_ig_l_disagree_NA_maj, values_from = n, values_fill = 0)
cross_press_maj

##Minority (Table C5)
cross_press_min<- data %>%
  select(num_ig_aligned_min, num_ig_l_agree_NA_min, num_ig_l_disagree_NA_min) %>%
  filter(num_ig_aligned_min>0) %>%
  count(num_ig_l_agree_NA_min, num_ig_l_disagree_NA_min) %>%
  pivot_wider(names_from = num_ig_l_disagree_NA_min, values_from = n, values_fill = 0)
cross_press_min

##Patterns within the votes where the leadership disagreed internally (Table C3) - column 1 with agreement is copied from Table C1
##Majority
leader_dis_scored_maj<- data %>%
  select(num_ig_aligned_maj, leadership_agrees_maj, ##can't look at agree/disagree because can't if leadership doesn't have party, just look at # groups scoring
         major_macro, major_civil, major_labor, major_educ, major_enviro, major_immig, major_social, major_dcomm) %>%
  filter(leadership_agrees_maj==0) %>%
  filter(num_ig_aligned_maj>0) %>%
  summarize(min_score = min(num_ig_aligned_maj, na.rm=TRUE),
            max_score = max(num_ig_aligned_maj, na.rm=TRUE),
            mean_score = mean(num_ig_aligned_maj, na.rm=TRUE),
            sd_score = sd(num_ig_aligned_maj, na.rm=TRUE),
            mean_macro = mean(major_macro, na.rm=TRUE),
            mean_civil = mean(major_civil, na.rm=TRUE),
            mean_labor = mean(major_labor, na.rm=TRUE),
            mean_educ = mean(major_educ, na.rm=TRUE),
            mean_enviro = mean(major_enviro, na.rm=TRUE),
            mean_immig = mean(major_immig, na.rm=TRUE),
            mean_social = mean(major_social, na.rm=TRUE),
            mean_dcomm = mean(major_dcomm, na.rm=TRUE))
leader_dis_scored_maj

##Minority
leader_dis_scored_min<- data %>%
  select(num_ig_aligned_min, leadership_agrees_min, ##can't look at agree/disagree because can't if leadership doesn't have party, just look at # groups scoring
         major_macro, major_civil, major_labor, major_educ, major_enviro, major_immig, major_social, major_dcomm) %>%
  filter(leadership_agrees_min==0) %>%
  filter(num_ig_aligned_min>0) %>%
  summarize(min_score = min(num_ig_aligned_min, na.rm=TRUE),
            max_score = max(num_ig_aligned_min, na.rm=TRUE),
            mean_score = mean(num_ig_aligned_min, na.rm=TRUE),
            sd_score = sd(num_ig_aligned_min, na.rm=TRUE),
            mean_macro = mean(major_macro, na.rm=TRUE),
            mean_civil = mean(major_civil, na.rm=TRUE),
            mean_labor = mean(major_labor, na.rm=TRUE),
            mean_educ = mean(major_educ, na.rm=TRUE),
            mean_enviro = mean(major_enviro, na.rm=TRUE),
            mean_immig = mean(major_immig, na.rm=TRUE),
            mean_social = mean(major_social, na.rm=TRUE),
            mean_dcomm = mean(major_dcomm, na.rm=TRUE))
leader_dis_scored_min


##Patterns of disagreement by individual groups among votes where they took a position (Table C8)
##Democratic groups
summary(as.factor(data$aauw_pref_l_agree_h_disagree[data$aauw_pos==1]))
summary(as.factor(data$ada_pref_l_agree_h_disagree[data$ada_pos==1]))
summary(as.factor(data$afl_pref_l_agree_h_disagree[data$afl_pos==1]))
summary(as.factor(data$nea_pref_l_agree_h_disagree[data$nea_pos==1]))
summary(as.factor(data$lcv_pref_l_agree_h_disagree[data$lcv_pos==1]))
summary(as.factor(data$naacp_pref_l_agree_h_disagree[data$naacp_pos==1]))
summary(as.factor(data$ara_pref_l_agree_h_disagree[data$ara_pos==1]))
summary(as.factor(data$aauw_pref_l_agree_s_disagree[data$aauw_pos==1]))
summary(as.factor(data$ada_pref_l_agree_s_disagree[data$ada_pos==1]))
summary(as.factor(data$afl_pref_l_agree_s_disagree[data$afl_pos==1]))
summary(as.factor(data$nea_pref_l_agree_s_disagree[data$nea_pos==1]))
summary(as.factor(data$lcv_pref_l_agree_s_disagree[data$lcv_pos==1]))
summary(as.factor(data$naacp_pref_l_agree_s_disagree[data$naacp_pos==1]))
summary(as.factor(data$ara_pref_l_agree_s_disagree[data$ara_pos==1]))

##Republican groups
summary(as.factor(data$acu_pref_l_agree_h_disagree[data$acu_pos==1]))
summary(as.factor(data$nfib_pref_l_agree_h_disagree[data$nfib_pos==1]))
summary(as.factor(data$frc_pref_l_agree_h_disagree[data$frc_pos==1]))
summary(as.factor(data$fw_pref_l_agree_h_disagree[data$fw_pos==1]))
summary(as.factor(data$cg_pref_l_agree_h_disagree[data$cg_pos==1]))
summary(as.factor(data$cc_pref_l_agree_h_disagree[data$cc_pos==1]))
summary(as.factor(data$fair_pref_l_agree_h_disagree[data$fair_pos==1]))
summary(as.factor(data$acu_pref_l_agree_s_disagree[data$acu_pos==1]))
summary(as.factor(data$nfib_pref_l_agree_s_disagree[data$nfib_pos==1]))
summary(as.factor(data$frc_pref_l_agree_s_disagree[data$frc_pos==1]))
summary(as.factor(data$fw_pref_l_agree_s_disagree[data$fw_pos==1]))
summary(as.factor(data$cg_pref_l_agree_s_disagree[data$cg_pos==1]))
summary(as.factor(data$cc_pref_l_agree_s_disagree[data$cc_pos==1]))
summary(as.factor(data$fair_pref_l_agree_s_disagree[data$fair_pos==1]))

##because 0 above can be agreement with leader or leader disagrees, check for agreement
##Democratic groups
summary(as.factor(data$aauw_pref_l_agree_h[data$aauw_pos==1]))
summary(as.factor(data$ada_pref_l_agree_h[data$ada_pos==1]))
summary(as.factor(data$afl_pref_l_agree_h[data$afl_pos==1]))
summary(as.factor(data$nea_pref_l_agree_h[data$nea_pos==1]))
summary(as.factor(data$lcv_pref_l_agree_h[data$lcv_pos==1]))
summary(as.factor(data$naacp_pref_l_agree_h[data$naacp_pos==1]))
summary(as.factor(data$ara_pref_l_agree_h[data$ara_pos==1]))
summary(as.factor(data$aauw_pref_l_agree_s[data$aauw_pos==1]))
summary(as.factor(data$ada_pref_l_agree_s[data$ada_pos==1]))
summary(as.factor(data$afl_pref_l_agree_s[data$afl_pos==1]))
summary(as.factor(data$nea_pref_l_agree_s[data$nea_pos==1]))
summary(as.factor(data$lcv_pref_l_agree_s[data$lcv_pos==1]))
summary(as.factor(data$naacp_pref_l_agree_s[data$naacp_pos==1]))
summary(as.factor(data$ara_pref_l_agree_s[data$ara_pos==1]))

##Republican groups
summary(as.factor(data$acu_pref_l_agree_h[data$acu_pos==1]))
summary(as.factor(data$nfib_pref_l_agree_h[data$nfib_pos==1]))
summary(as.factor(data$frc_pref_l_agree_h[data$frc_pos==1]))
summary(as.factor(data$fw_pref_l_agree_h[data$fw_pos==1]))
summary(as.factor(data$cg_pref_l_agree_h[data$cg_pos==1]))
summary(as.factor(data$cc_pref_l_agree_h[data$cc_pos==1]))
summary(as.factor(data$fair_pref_l_agree_h[data$fair_pos==1]))
summary(as.factor(data$acu_pref_l_agree_s[data$acu_pos==1]))
summary(as.factor(data$nfib_pref_l_agree_s[data$nfib_pos==1]))
summary(as.factor(data$frc_pref_l_agree_s[data$frc_pos==1]))
summary(as.factor(data$fw_pref_l_agree_s[data$fw_pos==1]))
summary(as.factor(data$cg_pref_l_agree_s[data$cg_pos==1]))
summary(as.factor(data$cc_pref_l_agree_s[data$cc_pos==1]))
summary(as.factor(data$fair_pref_l_agree_s[data$fair_pos==1]))

##combining H and S - Disagree
##Democratic groups
summary(as.factor(data$aauw_pref_l_agree_disagree[data$aauw_pos==1 & data$leadership_agrees_dem==1]))
summary(as.factor(data$ada_pref_l_agree_disagree[data$ada_pos==1 & data$leadership_agrees_dem==1]))
summary(as.factor(data$afl_pref_l_agree_disagree[data$afl_pos==1 & data$leadership_agrees_dem==1]))
summary(as.factor(data$nea_pref_l_agree_disagree[data$nea_pos==1 & data$leadership_agrees_dem==1]))
summary(as.factor(data$lcv_pref_l_agree_disagree[data$lcv_pos==1 & data$leadership_agrees_dem==1]))
summary(as.factor(data$naacp_pref_l_agree_disagree[data$naacp_pos==1 & data$leadership_agrees_dem==1]))
summary(as.factor(data$ara_pref_l_agree_disagree[data$ara_pos==1 & data$leadership_agrees_dem==1]))

##Republican groups
summary(as.factor(data$acu_pref_l_agree_disagree[data$acu_pos==1 & data$leadership_agrees_rep==1]))
summary(as.factor(data$nfib_pref_l_agree_disagree[data$nfib_pos==1 & data$leadership_agrees_rep==1]))
summary(as.factor(data$frc_pref_l_agree_disagree[data$frc_pos==1 & data$leadership_agrees_rep==1]))
summary(as.factor(data$fw_pref_l_agree_disagree[data$fw_pos==1 & data$leadership_agrees_rep==1]))
summary(as.factor(data$cg_pref_l_agree_disagree[data$cg_pos==1 & data$leadership_agrees_rep==1]))
summary(as.factor(data$cc_pref_l_agree_disagree[data$cc_pos==1 & data$leadership_agrees_rep==1]))
summary(as.factor(data$fair_pref_l_agree_disagree[data$fair_pos==1 & data$leadership_agrees_rep==1]))


#####################################
##  Frequency of disagreement      ##
#####################################
#t-tests of difference (Table 1)
##with raw count of number that disagree SUBSET on just those votes scored by 1+ group in that party
t.test(data$num_ig_l_disagree_NA_maj[data$num_ig_aligned_maj>0], 
       data$num_ig_l_disagree_NA_min[data$num_ig_aligned_min>0]) ##mean higher among majority than minority
##with raw count of number that disagree SUBSET on just those votes scored by 1+ group in both parties
t.test(data$num_ig_l_disagree_NA_maj[data$num_ig_aligned_maj>0 & data$num_ig_aligned_min>0],
       data$num_ig_l_disagree_NA_min[data$num_ig_aligned_maj>0 & data$num_ig_aligned_min>0]) ##mean higher among majority than minority

##by construction, if leadership disagreed, counts are NA

##Using dummy variable SUBSET on just those votes scored by 1+ group in that party
t.test(data$one_plus_ig_l_disagree_NA_maj[data$num_ig_aligned_maj>0], 
       data$one_plus_ig_l_disagree_NA_min[data$num_ig_aligned_min>0]) ##mean higher among majority than minority
##Using dummy variable SUBSET on just those votes scored by 1+ group in both parties
t.test(data$one_plus_ig_l_disagree_NA_maj[data$num_ig_aligned_maj>0 & data$num_ig_aligned_min>0], 
       data$one_plus_ig_l_disagree_NA_min[data$num_ig_aligned_maj>0 & data$num_ig_aligned_min>0]) 

##N for each (excludes leadership lack of agreement by construction)
length(na.omit(data$num_ig_l_disagree_NA_maj[data$num_ig_aligned_maj>0]))
length(na.omit(data$num_ig_l_disagree_NA_min[data$num_ig_aligned_min>0]))
length(na.omit(data$num_ig_l_disagree_NA_maj[data$num_ig_aligned_maj>0 & data$num_ig_aligned_min>0]))
length(na.omit(data$num_ig_l_disagree_NA_min[data$num_ig_aligned_maj>0 & data$num_ig_aligned_min>0]))
length(na.omit(data$one_plus_ig_l_disagree_NA_maj[data$num_ig_aligned_maj>0]))
length(na.omit(data$one_plus_ig_l_disagree_NA_min[data$num_ig_aligned_min>0]))
length(na.omit(data$one_plus_ig_l_disagree_NA_maj[data$num_ig_aligned_maj>0 & data$num_ig_aligned_min>0]))
length(na.omit(data$one_plus_ig_l_disagree_NA_min[data$num_ig_aligned_maj>0 & data$num_ig_aligned_min>0]))


#####################
##majority vs. minority, by party (Table E6)
##Democratic aligned (when Dem is majority vs. Dem is minority)
##with raw count of number that disagree SUBSET on just those votes scored by 1+ group in that party
t.test(data$num_ig_l_disagree_NA_maj[data$num_ig_aligned_maj>0 & data$majority=="Dem"], 
       data$num_ig_l_disagree_NA_min[data$num_ig_aligned_min>0 & data$majority=="GOP"]) ##mean higher among minority
##Using dummy variable SUBSET on just those votes scored by 1+ group in that party
t.test(data$one_plus_ig_l_disagree_NA_maj[data$num_ig_aligned_maj>0 & data$majority=="Dem"], 
       data$one_plus_ig_l_disagree_NA_min[data$num_ig_aligned_min>0 & data$majority=="GOP"]) ##mean higher among minority

##N for each (excludes leadership lack of agreement by construction)
length(na.omit(data$num_ig_l_disagree_NA_maj[data$num_ig_aligned_maj>0 & data$majority=="Dem"]))
length(na.omit(data$num_ig_l_disagree_NA_min[data$num_ig_aligned_min>0 & data$majority=="GOP"]))
length(na.omit(data$one_plus_ig_l_disagree_NA_maj[data$num_ig_aligned_maj>0 & data$majority=="Dem"]))
length(na.omit(data$one_plus_ig_l_disagree_NA_min[data$num_ig_aligned_min>0 & data$majority=="GOP"]))

##Republican aligned (when GOP is majority vs. GOP is minority)
##with raw count of number that disagree SUBSET on just those votes scored by 1+ group in that party
t.test(data$num_ig_l_disagree_NA_maj[data$num_ig_aligned_maj>0 & data$majority=="GOP"], 
       data$num_ig_l_disagree_NA_min[data$num_ig_aligned_min>0 & data$majority=="Dem"]) ##mean higher among majority than minority
##Using dummy variable SUBSET on just those votes scored by 1+ group in that party
t.test(data$one_plus_ig_l_disagree_NA_maj[data$num_ig_aligned_maj>0 & data$majority=="GOP"], 
       data$one_plus_ig_l_disagree_NA_min[data$num_ig_aligned_min>0 & data$majority=="Dem"]) ##mean higher among majority than minority

##N for each (excludes leadership lack of agreement by construction)
length(na.omit(data$num_ig_l_disagree_NA_maj[data$num_ig_aligned_maj>0 & data$majority=="GOP"]))
length(na.omit(data$num_ig_l_disagree_NA_min[data$num_ig_aligned_min>0 & data$majority=="Dem"]))
length(na.omit(data$one_plus_ig_l_disagree_NA_maj[data$num_ig_aligned_maj>0 & data$majority=="GOP"]))
length(na.omit(data$one_plus_ig_l_disagree_NA_min[data$num_ig_aligned_min>0 & data$majority=="Dem"]))

##Check with other measure - matches
t.test(data$one_plus_ig_l_disagree_NA_dem[data$num_ig_lib>0 & data$leadership_agrees_dem==1 & data$majority=="Dem"], ##Dem majority
       data$one_plus_ig_l_disagree_NA_dem[data$num_ig_lib>0 & data$leadership_agrees_dem==1 & data$majority=="GOP"]) ##Dem minority
t.test(data$one_plus_ig_l_disagree_NA_rep[data$num_ig_cons>0 & data$leadership_agrees_rep==1 & data$majority=="GOP"], ##GOP majority
       data$one_plus_ig_l_disagree_NA_rep[data$num_ig_cons>0 & data$leadership_agrees_rep==1 & data$majority=="Dem"]) ##GOP minority

##what percent had 1+ IG disagreement
summary(data$one_plus_ig_l_disagree_NA_maj[data$num_ig_aligned_maj>0])
summary(data$one_plus_ig_l_disagree_NA_min[data$num_ig_aligned_min>0])

##what percent had 1+ IG in agreement
summary(data$one_plus_ig_l_agree_NA_maj[data$num_ig_aligned_maj>0])
summary(data$one_plus_ig_l_agree_NA_min[data$num_ig_aligned_min>0])

##pool across majority or minority scored (in text)
summary(data$one_plus_ig_l_agree_NA_pool[data$num_ig_aligned_maj>0 | data$num_ig_aligned_min>0])
summary(data$one_plus_ig_l_disagree_NA_pool[data$num_ig_aligned_maj>0 | data$num_ig_aligned_min>0])

######################################
##Frequency of scored votes
#Note, with all data, 0 if none of groups we gathered data for scored the group
summary(as.factor(data$num_ig_l_disagree_NA_maj))
summary(as.factor(data$num_ig_l_disagree_NA_min))
##Among those where 1+ group scored (in text)
summary(as.factor(data$num_ig_l_disagree_NA_maj[data$num_ig_aligned_maj>0]))
summary(as.factor(data$num_ig_l_disagree_NA_min[data$num_ig_aligned_min>0]))

##Very few with 2 or 3 groups. Most have 0 or 1.

##check raw N of IG-party disagreement (footnote 12)
table(data$one_plus_ig_l_disagree_NA_maj[data$num_ig_aligned_maj>0]) ##180 1, 1071 0
table(data$one_plus_ig_l_disagree_NA_min[data$num_ig_aligned_min>0]) ##135 1, 1295 0

##among both scored (footnote 12)
##check raw N of IG-party disagreement
table(data$one_plus_ig_l_disagree_NA_maj[data$num_ig_aligned_maj>0 & data$num_ig_aligned_min>0]) ##53 1, 520 0
table(data$one_plus_ig_l_disagree_NA_min[data$num_ig_aligned_maj>0 & data$num_ig_aligned_min>0]) ##38 1, 553 0

##########################
##Create dataframes that include relevant subsets (because logistf doesn't recognize the subset command)
##create data where 1+ group aligned with majority and separate for 1+ aligned with minority scored the vote
data.scored.maj<- subset(data, data$num_ig_aligned_maj>0) ##Note that different set of votes for majority and minority
data.scored.min<- subset(data, data$num_ig_aligned_min>0) 

##Overlap of scored votes
data.scored.overlap<- subset(data, data$num_ig_aligned_min>0 & data$num_ig_aligned_maj>0)

##########################
##MAJORITY PARTY

##Logistic analysis of did any groups disagree with the leadership (since max of 3 and most important variation between 0 and 1+)
##Using Firth's bias correction

##subset on votes scored by 1+ group in that party
##with rare events correction (Table 2.1)
ig_disagree_logistf_maj_2<- logistf(one_plus_ig_l_disagree_NA_maj ~ vote_passage + vote_passage_constrained + vote_other + chamber_cat + divided_cong +
                                ext_nominate + ImpBill + as.factor(majortopic_ig), 
                              data=data.scored.maj)
summary(ig_disagree_logistf_maj_2)

##with rare events correction - FLIC (with intercept correction) (footnote robustness)
ig_disagree_flic_maj_2<- flic(one_plus_ig_l_disagree_NA_maj ~ vote_passage + vote_passage_constrained + vote_other + chamber_cat + divided_cong +
                                      ext_nominate + ImpBill + as.factor(majortopic_ig), 
                                    data=data.scored.maj)
summary(ig_disagree_flic_maj_2)

##subset on votes scored by MAJORITY AND MINORITY aligned (Table 2.2)
##With rare events correction
ig_disagree_logistf_maj_3<- logistf(one_plus_ig_l_disagree_NA_maj ~ vote_passage + vote_passage_constrained + vote_other + chamber_cat + divided_cong +
                                ext_nominate + ImpBill #+ as.factor(majortopic_ig)
                              , 
                              data=data.scored.overlap)
summary(ig_disagree_logistf_maj_3)

#####################################################
#####################################################
##MINORITY PARTY

##Logistic analysis of did any groups disagree with the leadership (since max of 3 and most important variation between 0 and 1+)
##subset on votes scored by 1+ group (Table 2.3)
##with rare events correction
ig_disagree_logistf_min_2<- logistf(one_plus_ig_l_disagree_NA_min ~ vote_passage + vote_passage_constrained + vote_other + chamber_cat + divided_cong +
                                ext_nominate + ImpBill + as.factor(majortopic_ig), 
                              data=data.scored.min)
summary(ig_disagree_logistf_min_2) 

##with rare events correction - FLIC (with intercept correction) (footnote robustness)
ig_disagree_flic_min_2<- flic(one_plus_ig_l_disagree_NA_min ~ vote_passage + vote_passage_constrained + vote_other + chamber_cat + divided_cong +
                                ext_nominate + ImpBill + as.factor(majortopic_ig), 
                              data=data.scored.min)
summary(ig_disagree_flic_min_2)

##subset on votes scored by Majority AND Minority aligned
##with rare events correction (Table 2.4)
ig_disagree_logistf_min_3<- logistf(one_plus_ig_l_disagree_NA_min ~ vote_passage + vote_passage_constrained + vote_other + chamber_cat + divided_cong +
                                ext_nominate + ImpBill #+ as.factor(majortopic_ig)
                              , 
                              data=data.scored.overlap)
summary(ig_disagree_logistf_min_3) 

##############################
##Robustness checks with CQ story measure through 114th (Table E1)
##check count of 1's
table(data$one_plus_ig_l_disagree_NA_maj[!is.na(data$cq_story) & data$num_ig_aligned_maj>0]) ##133 1
table(data$one_plus_ig_l_disagree_NA_min[!is.na(data$cq_story) & data$num_ig_aligned_min>0]) ##123 1

table(data$one_plus_ig_l_disagree_NA_maj[!is.na(data$cq_story) & data$num_ig_aligned_maj>0 & data$num_ig_aligned_min>0]) ##45 1
table(data$one_plus_ig_l_disagree_NA_min[!is.na(data$cq_story) & data$num_ig_aligned_maj>0 & data$num_ig_aligned_min>0]) ##37 1
##Need rare events correction

##Majority
##subset on votes scored by 1+ group in that party
##rare events correction (Table E1.1)
ig_disagree_logistf_maj_2_cq<- logistf(one_plus_ig_l_disagree_NA_maj ~ vote_passage + vote_passage_constrained + vote_other + chamber_cat + divided_cong +
                                   ext_nominate + ImpBill + as.factor(majortopic_ig) + cq_story, 
                                 data=data.scored.maj) ##subset itself doesn't work but NA on cq_story in 115th
summary(ig_disagree_logistf_maj_2_cq) 

##subset on votes scored by MAJORITY AND MINORITY aligned (Table E1.2)
##rare events correction
ig_disagree_logistf_maj_3_cq<- logistf(one_plus_ig_l_disagree_NA_maj ~ vote_passage + vote_passage_constrained + vote_other + chamber_cat + divided_cong +
                                   ext_nominate + ImpBill + #as.factor(majortopic_ig) + 
                                   cq_story, 
                                 data=data.scored.overlap)
summary(ig_disagree_logistf_maj_3_cq)

##Minority
##subset on votes scored by 1+ group (Table E1.3)
##rare events correction
ig_disagree_logistf_min_2_cq<- logistf(one_plus_ig_l_disagree_NA_min ~ vote_passage + vote_passage_constrained + vote_other + chamber_cat + divided_cong +
                                   ext_nominate + ImpBill + as.factor(majortopic_ig) + cq_story, 
                                 data=data.scored.min)
summary(ig_disagree_logistf_min_2_cq) 

##subset on votes scored by Majority AND Minority aligned (Table E1.4)
##rare events correction
ig_disagree_logistf_min_3_cq<- logistf(one_plus_ig_l_disagree_NA_min ~ vote_passage + vote_passage_constrained + vote_other + chamber_cat + divided_cong +
                                   ext_nominate + ImpBill + #as.factor(majortopic_ig) + 
                                   cq_story, 
                                 data=data.scored.overlap)
summary(ig_disagree_logistf_min_3_cq) 


########################################################
## Relationship between disagreement and party voting ##
########################################################
##Table 3

##MAJORITY PARTY
##Dummy variable for any groups disagree

##subset on votes scored by 1+ group (Table 3.1)
party_unity_ols_maj_5<- lm(vote_leader_pct_maj ~ as.factor(one_plus_ig_l_disagree_NA_maj) + num_ig_l_agree_NA_maj + 
                             vote_passage + vote_passage_constrained + vote_other + chamber_cat + divided_cong +
                             ext_nominate + ImpBill + as.factor(majortopic_ig), 
                       data=data,
                       subset=c(data$num_ig_aligned_maj>0))
summary(party_unity_ols_maj_5)

##subset on votes scored by 1+ group Majority AND Minority (Table 3.2)
party_unity_ols_maj_6<- lm(vote_leader_pct_maj ~ as.factor(one_plus_ig_l_disagree_NA_maj) + num_ig_l_agree_NA_maj + 
                             vote_passage + vote_passage_constrained + vote_other + chamber_cat + divided_cong +
                             ext_nominate + ImpBill + as.factor(majortopic_ig),
                           data=data,
                           subset=c(data$num_ig_aligned_maj>0 & data$num_ig_aligned_min >0))
summary(party_unity_ols_maj_6)

############################
##MINORITY PARTY

##Dummy variable for any groups disagree 
##subset on votes scored by 1+ group (Table 3.3)
party_unity_ols_min_5<- lm(vote_leader_pct_min ~ as.factor(one_plus_ig_l_disagree_NA_min) + num_ig_l_agree_NA_min + 
                             vote_passage + vote_passage_constrained + vote_other + chamber_cat + divided_cong +
                             ext_nominate + ImpBill + as.factor(majortopic_ig), 
                       data=data,
                       subset=c(data$num_ig_aligned_min>0))
summary(party_unity_ols_min_5)

##subset on votes scored by 1+ group majority AND minority (Table 3.4)
party_unity_ols_min_6<- lm(vote_leader_pct_min ~ as.factor(one_plus_ig_l_disagree_NA_min) + num_ig_l_agree_NA_min + 
                             vote_passage + vote_passage_constrained + vote_other + chamber_cat + divided_cong +
                             ext_nominate + ImpBill + as.factor(majortopic_ig),
                           data=data,
                           subset=c(data$num_ig_aligned_min>0 & data$num_ig_aligned_maj>0))
summary(party_unity_ols_min_6)

############################
##Robustness check with cq_story measure through 114th (Table E3)
##Majority
##subset on votes scored by 1+ group (Table E3.1)
party_unity_ols_maj_5_cq<- lm(vote_leader_pct_maj ~ as.factor(one_plus_ig_l_disagree_NA_maj) + num_ig_l_agree_NA_maj + 
                             vote_passage + vote_passage_constrained + vote_other + chamber_cat + divided_cong +
                             ext_nominate + ImpBill + as.factor(majortopic_ig) + cq_story, 
                           data=data,
                           subset=c(data$num_ig_aligned_maj>0 & data$cong<115))
summary(party_unity_ols_maj_5_cq)

##subset on votes scored by 1+ group Majority AND Minority (Table E3.2)
party_unity_ols_maj_6_cq<- lm(vote_leader_pct_maj ~ as.factor(one_plus_ig_l_disagree_NA_maj) + num_ig_l_agree_NA_maj + 
                             vote_passage + vote_passage_constrained + vote_other + chamber_cat + divided_cong +
                             ext_nominate + ImpBill + as.factor(majortopic_ig) + cq_story,
                           data=data,
                           subset=c(data$num_ig_aligned_maj>0 & data$num_ig_aligned_min >0 & data$cong<115))
summary(party_unity_ols_maj_6_cq)

##Minority
##subset on votes scored by 1+ group (Table E3.3)
party_unity_ols_min_5_cq<- lm(vote_leader_pct_min ~ as.factor(one_plus_ig_l_disagree_NA_min) + num_ig_l_agree_NA_min + 
                             vote_passage + vote_passage_constrained + vote_other + chamber_cat + divided_cong +
                             ext_nominate + ImpBill + as.factor(majortopic_ig) + cq_story, 
                           data=data,
                           subset=c(data$num_ig_aligned_min>0 & data$cong<115))
summary(party_unity_ols_min_5_cq)

##subset on votes scored by 1+ group majority AND minority (Table E3.4)
party_unity_ols_min_6_cq<- lm(vote_leader_pct_min ~ as.factor(one_plus_ig_l_disagree_NA_min) + num_ig_l_agree_NA_min + 
                             vote_passage + vote_passage_constrained + vote_other + chamber_cat + divided_cong +
                             ext_nominate + ImpBill + as.factor(majortopic_ig) + cq_story,
                           data=data,
                           subset=c(data$num_ig_aligned_min>0 & data$num_ig_aligned_maj>0 & data$cong<115))
summary(party_unity_ols_min_6_cq)

############################
##Robustness check with Ryan intraparty agreement in committee measure through 114th House only (Table E5)
##Majority
##subset on votes scored by 1+ group (Table E5.1)
party_unity_ols_maj_5_comm<- lm(vote_leader_pct_maj ~ as.factor(one_plus_ig_l_disagree_NA_maj) + num_ig_l_agree_NA_maj + 
                                vote_passage + vote_passage_constrained + vote_other + divided_cong +
                                ext_nominate + as.factor(majortopic_ig) + maj_intra_agree_mean, 
                              data=data,
                              subset=c(data$num_ig_aligned_maj>0 & data$cong<115))
summary(party_unity_ols_maj_5_comm)

##subset on votes scored by 1+ group majority AND minority Table E5.2)
party_unity_ols_maj_6_comm<- lm(vote_leader_pct_maj ~ as.factor(one_plus_ig_l_disagree_NA_maj) + num_ig_l_agree_NA_maj + 
                                  vote_passage + vote_passage_constrained + vote_other + divided_cong +
                                  ext_nominate + as.factor(majortopic_ig) + maj_intra_agree_mean, 
                                data=data,
                                subset=c(data$num_ig_aligned_min>0 & data$num_ig_aligned_maj>0 & data$cong<115))
summary(party_unity_ols_maj_6_comm)

##Minority
##subset on votes scored by 1+ group (Table E3.3)
party_unity_ols_min_5_comm<- lm(vote_leader_pct_min ~ as.factor(one_plus_ig_l_disagree_NA_min) + num_ig_l_agree_NA_min + 
                                  vote_passage + vote_passage_constrained + vote_other + divided_cong +
                                  ext_nominate + as.factor(majortopic_ig) + min_intra_agree_mean, 
                                data=data,
                                subset=c(data$num_ig_aligned_min>0 & data$cong<115))
summary(party_unity_ols_min_5_comm)

##subset on votes scored by 1+ group majority AND minority (Table E5.4)
party_unity_ols_min_6_comm<- lm(vote_leader_pct_min ~ as.factor(one_plus_ig_l_disagree_NA_min) + num_ig_l_agree_NA_min + 
                                  vote_passage + vote_passage_constrained + vote_other + divided_cong +
                                  ext_nominate + as.factor(majortopic_ig) + min_intra_agree_mean, 
                                data=data,
                                subset=c(data$num_ig_aligned_maj>0 & data$num_ig_aligned_min>0 & data$cong<115))
summary(party_unity_ols_min_6_comm)

##########################
##With indicators for each group (Table E4)
##Majority
##subset on votes scored by 1+ group (Table E4.1)
party_unity_ols_maj_5_group<- lm(vote_leader_pct_maj ~ aauw_pref_l_agree_disagree_maj + ##dem
                                   ada_pref_l_agree_disagree_maj +  
                                   afl_pref_l_agree_disagree_maj +
                                   nea_pref_l_agree_disagree_maj +
                                   lcv_pref_l_agree_disagree_maj +
                                   naacp_pref_l_agree_disagree_maj +
                                   ara_pref_l_agree_disagree_maj +
                                   acu_pref_l_agree_disagree_maj + ##GOP
                                   nfib_pref_l_agree_disagree_maj +
                                   frc_pref_l_agree_disagree_maj +
                                   fw_pref_l_agree_disagree_maj +
                                   cg_pref_l_agree_disagree_maj +
                                   cc_pref_l_agree_disagree_maj +
                                   fair_pref_l_agree_disagree_maj +
                                 num_ig_l_agree_NA_maj + 
                             vote_passage + vote_passage_constrained + vote_other + chamber_cat + divided_cong +
                             ext_nominate + ImpBill, 
                           data=data,
                           subset=c(data$num_ig_aligned_maj>0 & data$leadership_agrees_maj==1))
summary(party_unity_ols_maj_5_group)

##Minority
##subset on votes scored by 1+ group (Table E4.2)
party_unity_ols_min_5_group<- lm(vote_leader_pct_min ~ aauw_pref_l_agree_disagree_min + ##dem
                                   ada_pref_l_agree_disagree_min +  
                                   afl_pref_l_agree_disagree_min +
                                   nea_pref_l_agree_disagree_min +
                                   lcv_pref_l_agree_disagree_min +
                                   naacp_pref_l_agree_disagree_min +
                                   ara_pref_l_agree_disagree_min +
                                   acu_pref_l_agree_disagree_min + ##GOP
                                   nfib_pref_l_agree_disagree_min +
                                   frc_pref_l_agree_disagree_min +
                                   fw_pref_l_agree_disagree_min +
                                   cg_pref_l_agree_disagree_min +
                                   cc_pref_l_agree_disagree_min +
                                   fair_pref_l_agree_disagree_min +
                                   num_ig_l_agree_NA_min + 
                                   vote_passage + vote_passage_constrained + vote_other + chamber_cat + divided_cong +
                                   ext_nominate + ImpBill, 
                                 data=data,
                                 subset=c(data$num_ig_aligned_min>0 & data$leadership_agrees_min==1))
summary(party_unity_ols_min_5_group)


#############################################
##Analysis with indicators for each group, dropping major topic code
##subset on votes scored by 1+ group
##By party rather than majority/minority (num_ig_lib, and num_ig_cons)
##limitation is that very few observations for each group have a 1 for disagreement
##combining H and S

######################
##initial model specification by party, add variable for majority (Table E8)
##subset on votes scored by 1+ group
##Democrats (Table E8.1)
party_unity_ols_dem_5<- lm(vote_leader_pct_dem ~ as.factor(one_plus_ig_l_disagree_NA_dem) + num_ig_l_agree_NA_dem + 
                            vote_passage + vote_passage_constrained + vote_other + chamber_cat + divided_cong +
                            ext_nominate + ImpBill + as.factor(majortopic_ig) + majority, 
                           data=data,
                           subset=c(data$num_ig_lib>0 & data$leadership_agrees_dem==1))
summary(party_unity_ols_dem_5)

##Republicans (Table E8.2)
party_unity_ols_rep_5<- lm(vote_leader_pct_rep ~ as.factor(one_plus_ig_l_disagree_NA_rep) + num_ig_l_agree_NA_rep + 
                             vote_passage + vote_passage_constrained + vote_other + chamber_cat + divided_cong +
                             ext_nominate + ImpBill + as.factor(majortopic_ig) + majority, 
                           data=data,
                           subset=c(data$num_ig_cons>0 & data$leadership_agrees_rep==1))
summary(party_unity_ols_rep_5)

######################
##Indicators by group

###Models with indicators for disagreement but no control for agreement
##Democrats (Table E8.3)
party_unity_ols_dem_5_group<- lm(vote_leader_pct_dem ~ aauw_pref_l_agree_disagree + ada_pref_l_agree_disagree +
                                   afl_pref_l_agree_disagree + nea_pref_l_agree_disagree +
                                   lcv_pref_l_agree_disagree + naacp_pref_l_agree_disagree + 
                                   ara_pref_l_agree_disagree +
                                   num_ig_l_agree_dem +
                                   vote_passage + vote_passage_constrained + vote_other + 
                                   chamber_cat  + divided_cong + 
                                   ext_nominate + ImpBill + majority, 
                                 data=data,
                                 subset=c(data$num_ig_lib>0 & data$leadership_agrees_dem==1))
summary(party_unity_ols_dem_5_group)

##Republicans (Table E8.4)
party_unity_ols_rep_5_group<- lm(vote_leader_pct_rep ~ acu_pref_l_agree_disagree + nfib_pref_l_agree_disagree +
                                   frc_pref_l_agree_disagree + fw_pref_l_agree_disagree +
                                   cg_pref_l_agree_disagree + cc_pref_l_agree_disagree + fair_pref_l_agree_disagree +
                                   num_ig_l_agree_rep +
                                   vote_passage + vote_passage_constrained + vote_other + 
                                   chamber_cat  + divided_cong + 
                                   ext_nominate + ImpBill + majority, 
                                 data=data,
                                 subset=c(data$num_ig_cons>0 & data$leadership_agrees_rep==1))
summary(party_unity_ols_rep_5_group)


#############################################
#############################################
##Bootstrap for figure
##Use subsets created above of scored maj, scored min, or scored both

##Overlap of scored votes
dim(data.scored.maj) ##1435
dim(data.scored.min) ##1620
dim(data.scored.overlap) #639

######################################
##DV: 1+ interest groups disagree with party leaders (Figure 1)
##subset model to only cases where that party scores vote
##simulation run separately for majority and minority, with rare events correction
##Since just interested in whether predicted probability of 1+ groups disagreeing is greater in majority party, not using new data (just predict off each simulation)
ig_disagree_boot<- matrix(nrow=1000, ncol=3) ##create empty matrix to hold results  
for (i in 1:1000){ ##repeat 1000 times
  ig_disagree_simcases_maj<- sample(1:nrow(data.scored.maj), replace=TRUE)
  ig_disagree_sim_maj<- data.scored.maj[ig_disagree_simcases_maj, ]
  ig_disagree_model_maj<- logistf(one_plus_ig_l_disagree_NA_maj ~ vote_passage + vote_passage_constrained + vote_other + chamber_cat + divided_cong +
                                ext_nominate + ImpBill + as.factor(majortopic_ig),
                             data=ig_disagree_sim_maj)
  ig_disagree_simcases_min<- sample(1:nrow(data.scored.min), replace=TRUE)
  ig_disagree_sim_min<- data.scored.min[ig_disagree_simcases_min, ]
  ig_disagree_model_min<- logistf(one_plus_ig_l_disagree_NA_min ~ vote_passage + vote_passage_constrained + vote_other + chamber_cat + divided_cong +
                                ext_nominate + ImpBill + as.factor(majortopic_ig),
                             data=ig_disagree_sim_min)
  pred_maj<-  predict(ig_disagree_model_maj,
                       type = "response")
  ig_disagree_boot[i,1]<- mean(pred_maj, na.rm=TRUE)
  pred_min<-  predict(ig_disagree_model_min,
                       type = "response")
  ig_disagree_boot[i,2]<- mean(pred_min, na.rm=TRUE)
  ig_disagree_boot[i,3]<- ig_disagree_boot[i,1] - ig_disagree_boot[i,2] ##difference between maj and min
}

summary(ig_disagree_boot)
ig_disagree_boot<- as.data.frame(ig_disagree_boot)
names(ig_disagree_boot)<- c("maj_1plus_prob", "min_1plus_prob", "diff_maj_min")
summary(ig_disagree_boot)

##order of variables in resulting data frame are: 
#predicted probability of 1+ groups disagreeing with leader for majority (controlling for variables as they are observed)
#predicted probability of 1+ groups disagreeing with leader for minority (controlling for variables as they are observed)
#difference between predicted probabily for majority - minority

##calc mean and 95% confidence interval by group
ig_disagee_mean_maj=mean(ig_disagree_boot$maj_1plus_prob)
ig_disagee_lower_maj=quantile(ig_disagree_boot$maj_1plus_prob, probs=0.025)
ig_disagee_upper_maj=quantile(ig_disagree_boot$maj_1plus_prob, probs=0.975)
ig_disagee_mean_maj
ig_disagee_lower_maj
ig_disagee_upper_maj
ig_disagee_mean_min=mean(ig_disagree_boot$min_1plus_prob)
ig_disagee_lower_min=quantile(ig_disagree_boot$min_1plus_prob, probs=0.025)
ig_disagee_upper_min=quantile(ig_disagree_boot$min_1plus_prob, probs=0.975)
ig_disagee_mean_min
ig_disagee_lower_min
ig_disagee_upper_min
ig_disagee_mean_diff=mean(ig_disagree_boot$diff_maj_min)
ig_disagee_lower_diff=quantile(ig_disagree_boot$diff_maj_min, probs=0.025)
ig_disagee_upper_diff=quantile(ig_disagree_boot$diff_maj_min, probs=0.975)
ig_disagee_mean_diff
ig_disagee_lower_diff
ig_disagee_upper_diff
##Note that because of bootstrap, numbers may differ slightly from iteration reported in paper

##Plot majority and minority
ig_disagree_plot<- data.frame(matrix(c(ig_disagee_mean_maj, ig_disagee_lower_maj, ig_disagee_upper_maj,
                                       ig_disagee_mean_min, ig_disagee_lower_min, ig_disagee_upper_min),
                                     nrow=2, ncol=3, byrow=TRUE))
ig_disagree_plot
names(ig_disagree_plot)<- c("treatment", "lower", "upper")
ig_disagree_plot$group<- c("Majority", "Minority")
##set group order
ig_disagree_plot$group.order<- factor(ig_disagree_plot$group, as.character(ig_disagree_plot$group))

##Plot
dodge <- position_dodge(width = 0.9)
ig_disagree_plot_output<- ggplot(data = ig_disagree_plot, aes(x = group.order, y = treatment)) + 
  geom_point(stat = "identity", position = dodge, size=4) +
  geom_errorbar(data=ig_disagree_plot, mapping=aes(ymin = lower, ymax = upper), position = dodge, width = 0.25) +
  labs(y="Predicted Probability") +
  coord_cartesian(ylim=c(0,0.2)) +
  theme(axis.ticks.x=element_blank(), axis.title.x=element_blank()) +
  theme(axis.text=element_text(size=12), axis.title.y = element_text(size = 12),
        plot.title = element_text(size=12))
ggsave("Fig1.png", ig_disagree_plot_output, height=5, width=6)

######################################
##Bootstrap on just 639 votes with majority and minority aligned interest groups scoring (Figure E1, footnote 17)
######################################
##DV: 1+ interest groups disagree with party leaders, with rare events correction
##Since just interested in whether predicted probability of 1+ groups disagreeing is greater in majority party, not using new data (just predict off each simulation)
ig_disagree_overlap_boot<- matrix(nrow=1000, ncol=3) ##create empty matrix to hold results  
for (i in 1:1000){ ##repeat 1000 times
  ig_disagree_overlap_simcases<- sample(1:nrow(data.scored.overlap), replace=TRUE)
  ig_disagree_overlap_sim<- data.scored.overlap[ig_disagree_overlap_simcases, ]
  ig_disagree_overlap_model_maj<- logistf(one_plus_ig_l_disagree_NA_maj ~ vote_passage + vote_passage_constrained + #vote_other + 
                                        chamber_cat + divided_cong +
                                        ext_nominate + ImpBill #+ as.factor(majortopic_ig)
                                      , 
                              data=ig_disagree_overlap_sim)
  ig_disagree_overlap_model_min<- logistf(one_plus_ig_l_disagree_NA_min ~  vote_passage + vote_passage_constrained + #vote_other + 
                                        chamber_cat + divided_cong +
                                        ext_nominate + ImpBill #+ as.factor(majortopic_ig)
                                      , 
                              data=ig_disagree_overlap_sim)
  pred_overlap_maj<-  predict(ig_disagree_overlap_model_maj,
                      type = "response")
  ig_disagree_overlap_boot[i,1]<- mean(pred_overlap_maj, na.rm=TRUE)
  pred_overlap_min<-  predict(ig_disagree_overlap_model_min,
                      type = "response")
  ig_disagree_overlap_boot[i,2]<- mean(pred_overlap_min, na.rm=TRUE)
  ig_disagree_overlap_boot[i,3]<- ig_disagree_overlap_boot[i,1] - ig_disagree_overlap_boot[i,2] ##difference between maj and min
}

summary(ig_disagree_overlap_boot)
ig_disagree_overlap_boot<- as.data.frame(ig_disagree_overlap_boot)
names(ig_disagree_overlap_boot)<- c("maj_1plus_prob", "min_1plus_prob", "diff_maj_min")
summary(ig_disagree_overlap_boot)

##order of variables in resulting data frame are: 
#predicted probability of 1+ groups disagreeing with leader for majority (controlling for variables as they are observed)
#predicted probability of 1+ groups disagreeing with leader for minority (controlling for variables as they are observed)
##difference in predicted probability for majority - minority

##calc mean and 95% confidence interval by group (footnote)
ig_disagee_overlap_mean_maj=mean(ig_disagree_overlap_boot$maj_1plus_prob)
ig_disagee_overlap_lower_maj=quantile(ig_disagree_overlap_boot$maj_1plus_prob, probs=0.025)
ig_disagee_overlap_upper_maj=quantile(ig_disagree_overlap_boot$maj_1plus_prob, probs=0.975)
ig_disagee_overlap_mean_maj
ig_disagee_overlap_lower_maj
ig_disagee_overlap_upper_maj
ig_disagee_overlap_mean_min=mean(ig_disagree_overlap_boot$min_1plus_prob)
ig_disagee_overlap_lower_min=quantile(ig_disagree_overlap_boot$min_1plus_prob, probs=0.025)
ig_disagee_overlap_upper_min=quantile(ig_disagree_overlap_boot$min_1plus_prob, probs=0.975)
ig_disagee_overlap_mean_min
ig_disagee_overlap_lower_min
ig_disagee_overlap_upper_min
ig_disagee_overlap_mean_diff=mean(ig_disagree_overlap_boot$diff_maj_min)
ig_disagee_overlap_lower_diff=quantile(ig_disagree_overlap_boot$diff_maj_min, probs=0.025)
ig_disagee_overlap_upper_diff=quantile(ig_disagree_overlap_boot$diff_maj_min, probs=0.975)
ig_disagee_overlap_mean_diff
ig_disagee_overlap_lower_diff
ig_disagee_overlap_upper_diff
##Plot majority and minority
ig_disagree_overlap_plot<- data.frame(matrix(c(ig_disagee_overlap_mean_maj, ig_disagee_overlap_lower_maj, ig_disagee_overlap_upper_maj,
                                       ig_disagee_overlap_mean_min, ig_disagee_overlap_lower_min, ig_disagee_overlap_upper_min),
                                     nrow=2, ncol=3, byrow=TRUE))
ig_disagree_overlap_plot
names(ig_disagree_overlap_plot)<- c("treatment", "lower", "upper")
ig_disagree_overlap_plot$group<- c("Majority", "Minority")
##set group order
ig_disagree_overlap_plot$group.order<- factor(ig_disagree_overlap_plot$group, as.character(ig_disagree_overlap_plot$group))

##Plot
dodge <- position_dodge(width = 0.9)
ig_disagree_overlap_plot_output<- ggplot(data = ig_disagree_overlap_plot, aes(x = group.order, y = treatment)) + 
  geom_point(stat = "identity", position = dodge, size=4) +
  geom_errorbar(data=ig_disagree_overlap_plot, mapping=aes(ymin = lower, ymax = upper), position = dodge, width = 0.25) +
  labs(y="Predicted Probability") +
  coord_cartesian(ylim=c(0,0.15)) +
  theme(axis.ticks.x=element_blank(), axis.title.x=element_blank()) +
  theme(axis.text=element_text(size=12), axis.title.y = element_text(size = 12),
        plot.title = element_text(size=12))
ggsave("FigE1.png", ig_disagree_overlap_plot_output, height=5, width=6)


#############################################
#############################################
##Output regressions

source("regtable.R") ##Using for some and using R package for others, where regtable doesn't handle specification

##Set working directory for all output (same as above unless otherwise specified)

##output models
##DV1: Did 1+ group disagree
##Dv2: voting cohesion

##DV1 with rare events correction (Table 2)
dv1_logistf<- list(
  "Majority, Scored Votes"=ig_disagree_logistf_maj_2,
  "Majority, Both Scored Votes"=ig_disagree_logistf_maj_3, 
  "Minority, Scored Votes"=ig_disagree_logistf_min_2,
  "Minority, Both Scored Votes"=ig_disagree_logistf_min_3
)
modelsummary(dv1_logistf, 
             estimate = "{estimate}{stars}",
             coef_rename = c("(Intercept)" = "Constant",
                             "party_controlminority"= "Minority Party",
                             "intra_agree"= "Mean Intra-Party Agreement on Committee",
                             "vote_other"= "Other Vote",
                             "vote_passage"= "Passage Vote",
                             "vote_passage_constrained"= "Passage Vote (Constrained)",
                             "chamber_catSenate"= "Senate",
                             "divided_cong"= "Divided Congress",
                             "ext_nominate"= "Extremity NOMINATE Sponsor",
                             "ImpBill"= "Important Bills (not minor)",
                             "as.factor(majortopic_ig)1"= "Macroeconomics",
                             "as.factor(majortopic_ig)2"= "Civil Rights",
                             "as.factor(majortopic_ig)5"= "Labor",
                             "as.factor(majortopic_ig)6"= "Education",
                             "as.factor(majortopic_ig)7"= "Environment",
                             "as.factor(majortopic_ig)9"= "Immigration",
                             "as.factor(majortopic_ig)13"= "Social Welfare",
                             "as.factor(majortopic_ig)15"= "Domestic Commerce and Small Bus"),
             output = "Table2.docx")

##DV2: Pct voting with party leadership, binary measure of number of IG disagreeing (Table 3)
outtable.rtf(list("Majority, Scored Votes"=party_unity_ols_maj_5,
                  "Majority, Both Scored Votes"=party_unity_ols_maj_6, 
                  "Minority, Scored Votes"=party_unity_ols_min_5,
                  "Minority, Both Scored Votes"=party_unity_ols_min_6), 
             replacelist=list(c("(Intercept)", "Constant"),
                              c("as.factor(one_plus_ig_l_disagree_NA_maj)1", "1+ Aligned IG Disagreeing with Leaders"),
                              c("num_ig_l_agree_NA_maj", "Number Aligned IG Agreeing with Leaders"),
                              c("as.factor(one_plus_ig_l_disagree_NA_min)1", "1+ Aligned IG Disagreeing with Leaders"),
                              c("num_ig_l_agree_NA_min", "Number Aligned IG Agreeing with Leaders"),
                              c("vote_other", "Other Vote"),
                              c("vote_passage", "Passage Vote"),
                              c("vote_passage_constrained", "Passage Vote (Constrained)"),
                              c("chamber_catSenate", "Senate"),
                              c("divided_cong", "Divided Congress"),
                              c("ext_nominate", "Extremity NOMINATE Sponsor"),
                              c("ImpBill", "Important Bills (not minor)"),
                              c("ImpBill", "Important Bills (not minor)"),
                              c("as.factor(majortopic_ig)1", "Macroeconomics"),
                              c("as.factor(majortopic_ig)2", "Civil Rights"),
                              c("as.factor(majortopic_ig)5", "Labor"),
                              c("as.factor(majortopic_ig)6", "Education"),
                              c("as.factor(majortopic_ig)7", "Environment"),
                              c("as.factor(majortopic_ig)9", "Immigration"),
                              c("as.factor(majortopic_ig)13", "Social Welfare"),
                              c("as.factor(majortopic_ig)15", "Domestic Commerce and Small Bus")),
             p.levels =c(0.10,0.05,0.01,0.001),
             scientific = 5,
             digits = 3,
             p.levels.labels=c("^", "*","**","***"),
             "Table3.rtf")

##Models with robustness checks of CQ story through 114th
##DV1 with CQ, rare events correction (Table E1)
dv1_logistf_cq<- list(
  "Majority, Scored Votes"=ig_disagree_logistf_maj_2_cq,
  "Majority, Both Scored Votes"=ig_disagree_logistf_maj_3_cq, 
  "Minority, Scored Votes"=ig_disagree_logistf_min_2_cq,
  "Minority, Both Scored Votes"=ig_disagree_logistf_min_3_cq
)
modelsummary(dv1_logistf_cq, 
             estimate = "{estimate}{stars}",
             coef_rename = c("(Intercept)" = "Constant",
                             "party_controlminority"= "Minority Party",
                             "intra_agree"= "Mean Intra-Party Agreement on Committee",
                             "vote_other"= "Other Vote",
                             "vote_passage"= "Passage Vote",
                             "vote_passage_constrained"= "Passage Vote (Constrained)",
                             "chamber_catSenate"= "Senate",
                             "divided_cong"= "Divided Congress",
                             "ext_nominate"= "Extremity NOMINATE Sponsor",
                             "ImpBill"= "Important Bills (not minor)",
                             "as.factor(majortopic_ig)1"= "Macroeconomics",
                             "as.factor(majortopic_ig)2"= "Civil Rights",
                             "as.factor(majortopic_ig)5"= "Labor",
                             "as.factor(majortopic_ig)6"= "Education",
                             "as.factor(majortopic_ig)7"= "Environment",
                             "as.factor(majortopic_ig)9"= "Immigration",
                             "as.factor(majortopic_ig)13"= "Social Welfare",
                             "as.factor(majortopic_ig)15"= "Domestic Commerce and Small Bus",
                             "cq_story"= "CQ Story"),
             output = "TableE1.docx")

##DV2: Pct voting with party leadership, binary measure of number of IG disagreeing (Table E3)
outtable.rtf(list("Majority, Scored Votes"=party_unity_ols_maj_5_cq,
                  "Majority, Both Scored Votes"=party_unity_ols_maj_6_cq, 
                  "Minority, Scored Votes"=party_unity_ols_min_5_cq,
                  "Minority, Both Scored Votes"=party_unity_ols_min_6_cq), 
             replacelist=list(c("(Intercept)", "Constant"),
                              c("as.factor(one_plus_ig_l_disagree_NA_maj)1", "1+ Aligned IG Disagreeing with Leaders"),
                              c("num_ig_l_agree_NA_maj", "Number Aligned IG Agreeing with Leaders"),
                              c("as.factor(one_plus_ig_l_disagree_NA_min)1", "1+ Aligned IG Disagreeing with Leaders"),
                              c("num_ig_l_agree_NA_min", "Number Aligned IG Agreeing with Leaders"),
                              c("vote_other", "Other Vote"),
                              c("vote_passage", "Passage Vote"),
                              c("vote_passage_constrained", "Passage Vote (Constrained)"),
                              c("chamber_catSenate", "Senate"),
                              c("divided_cong", "Divided Congress"),
                              c("ext_nominate", "Extremity NOMINATE Sponsor"),
                              c("ImpBill", "Important Bills (not minor)"),
                              c("ImpBill", "Important Bills (not minor)"),
                              c("as.factor(majortopic_ig)1", "Macroeconomics"),
                              c("as.factor(majortopic_ig)2", "Civil Rights"),
                              c("as.factor(majortopic_ig)5", "Labor"),
                              c("as.factor(majortopic_ig)6", "Education"),
                              c("as.factor(majortopic_ig)7", "Environment"),
                              c("as.factor(majortopic_ig)9", "Immigration"),
                              c("as.factor(majortopic_ig)13", "Social Welfare"),
                              c("as.factor(majortopic_ig)15", "Domestic Commerce and Small Bus"),
                              c("cq_story", "CQ Story")),
             p.levels =c(0.10,0.05,0.01,0.001),
             scientific = 5,
             digits = 3,
             p.levels.labels=c("^", "*","**","***"),
             "TableE3.rtf")

#################
##Models with robustness checks of Ryan House committee vote through 114th (Table E5)
##DV2: Pct voting with party leadership, binary measure of number of IG disagreeing
outtable.rtf(list("Majority, Scored Votes"=party_unity_ols_maj_5_comm,
                  "Majority, Both Scored Votes"=party_unity_ols_maj_6_comm, 
                  "Minority, Scored Votes"=party_unity_ols_min_5_comm,
                  "Minority, Both Scored Votes"=party_unity_ols_min_6_comm), 
             replacelist=list(c("(Intercept)", "Constant"),
                              c("as.factor(one_plus_ig_l_disagree_NA_maj)1", "1+ Aligned IG Disagreeing with Leaders"),
                              c("num_ig_l_agree_NA_maj", "Number Aligned IG Agreeing with Leaders"),
                              c("as.factor(one_plus_ig_l_disagree_NA_min)1", "1+ Aligned IG Disagreeing with Leaders"),
                              c("num_ig_l_agree_NA_min", "Number Aligned IG Agreeing with Leaders"),
                              c("vote_other", "Other Vote"),
                              c("vote_passage", "Passage Vote"),
                              c("vote_passage_constrained", "Passage Vote (Constrained)"),
                              c("divided_cong", "Divided Congress"),
                              c("ext_nominate", "Extremity NOMINATE Sponsor"),
                              c("ImpBill", "Important Bills (not minor)"),
                              c("ImpBill", "Important Bills (not minor)"),
                              c("as.factor(majortopic_ig)1", "Macroeconomics"),
                              c("as.factor(majortopic_ig)2", "Civil Rights"),
                              c("as.factor(majortopic_ig)5", "Labor"),
                              c("as.factor(majortopic_ig)6", "Education"),
                              c("as.factor(majortopic_ig)7", "Environment"),
                              c("as.factor(majortopic_ig)9", "Immigration"),
                              c("as.factor(majortopic_ig)13", "Social Welfare"),
                              c("as.factor(majortopic_ig)15", "Domestic Commerce and Small Bus"),
                              c("maj_intra_agree_mean", "Committee Vote Intra-Party Agreement (Mean)"),
                              c("min_intra_agree_mean", "Committee Vote Intra-Party Agreement (Mean)")),
             p.levels =c(0.10,0.05,0.01,0.001),
             scientific = 5,
             digits = 3,
             p.levels.labels=c("^", "*","**","***"),
             "TableE5.rtf")

##DV2: Models with indicator for each group agreeing, by party (Table E8)
outtable.rtf(list("Democrats, Scored Votes"=party_unity_ols_dem_5,
                  "Republicans, Scored Votes"=party_unity_ols_rep_5,
                  "Democrats, Scored Votes"=party_unity_ols_dem_5_group,
                  "Republicans, Scored Votes"=party_unity_ols_rep_5_group), 
             replacelist=list(c("(Intercept)", "Constant"),
                              c("as.factor(one_plus_ig_l_disagree_NA_dem)1", "1+ Aligned IG Disagreeing with Leaders"),
                              c("num_ig_l_agree_NA_dem", "Number Aligned IG Agreeing with Leaders"),
                              c("as.factor(one_plus_ig_l_disagree_NA_rep)1", "1+ Aligned IG Disagreeing with Leaders"),
                              c("num_ig_l_agree_NA_rep", "Number Aligned IG Agreeing with Leaders"),
                              c("acu_pref_l_agree_disagree", "ACU Disagree with Leaders"), ##Republican
                              c("nfib_pref_l_agree_disagree", "NFIB Disagree with Leaders"),
                              c("frc_pref_l_agree_disagree", "FRC Disagree with Leaders"),
                              c("fw_pref_l_agree_disagree", "Freedom Works Disagree with Leaders"),
                              c("cg_pref_l_agree_disagree", "Club for Growth Disagree with Leaders"),
                              c("cc_pref_l_agree_disagree", "Chamber of Commerce Disagree with Leaders"),
                              c("fair_pref_l_agree_disagree", "FAIR Disagree with Leaders"),
                              c("aauw_pref_l_agree_disagree", "AAUW Disagree with Leaders"), ##Democratic
                              c("ada_pref_l_agree_disagree", "ADA Disagree with Leaders"),
                              c("afl_pref_l_agree_disagree", "AFL Disagree with Leaders"),
                              c("nea_pref_l_agree_disagree", "NEA Disagree with Leaders"),
                              c("lcv_pref_l_agree_disagree", "LCV Disagree with Leaders"),
                              c("naacp_pref_l_agree_disagree", "NAACP Disagree with Leaders"),
                              c("ara_pref_l_agree_disagree", "ARA Disagree with Leaders"),
                              c("num_ig_l_agree_dem", "Number Dem. Aligned IG Agreeing with Leaders"),
                              c("num_ig_l_agree_rep", "Number Rep. Aligned IG Agreeing with Leaders"),
                              c("vote_other", "Other Vote"),
                              c("vote_passage", "Passage Vote"),
                              c("vote_passage_constrained", "Passage Vote (Constrained)"),
                              c("divided_cong", "Divided Congress"),
                              c("ext_nominate", "Extremity NOMINATE Sponsor"),
                              c("ImpBill", "Important Bills (not minor)"),
                              c("as.factor(majortopic_ig)1", "Macroeconomics"),
                              c("as.factor(majortopic_ig)2", "Civil Rights"),
                              c("as.factor(majortopic_ig)5", "Labor"),
                              c("as.factor(majortopic_ig)6", "Education"),
                              c("as.factor(majortopic_ig)7", "Environment"),
                              c("as.factor(majortopic_ig)9", "Immigration"),
                              c("as.factor(majortopic_ig)13", "Social Welfare"),
                              c("as.factor(majortopic_ig)15", "Domestic Commerce and Small Bus"),
                              c("majorityGOP", "Majority = Republican")),
             p.levels =c(0.10,0.05,0.01,0.001),
             scientific = 5,
             digits = 3,
             p.levels.labels=c("^", "*","**","***"),
             "TableE8.rtf")

##DV2: Models with indicator for each group agreeing, by majority-minority (Table E4)
dv2_groups<- list(
  "Majority, Scored Votes"=party_unity_ols_maj_5_group,
  "Minority, Scored Votes"=party_unity_ols_min_5_group
)
modelsummary(dv2_groups, 
             estimate = "{estimate}{stars}",
             coef_rename = c("(Intercept)" = "Constant",
                             "num_ig_l_agree_NA_dem" = "Number Aligned IG Agreeing with Leaders",
                             "num_ig_l_agree_NA_rep" = "Number Aligned IG Agreeing with Leaders",
                             "acu_pref_l_agree_disagree_maj" = "ACU Disagree with Leaders", ##Republican
                             "nfib_pref_l_agree_disagree_maj" = "NFIB Disagree with Leaders",
                             "frc_pref_l_agree_disagree_maj" = "FRC Disagree with Leaders",
                             "fw_pref_l_agree_disagree_maj" = "Freedom Works Disagree with Leaders",
                             "cg_pref_l_agree_disagree_maj" = "Club for Growth Disagree with Leaders",
                             "cc_pref_l_agree_disagree_maj" = "Chamber of Commerce Disagree with Leaders",
                             "fair_pref_l_agree_disagree_maj" = "FAIR Disagree with Leaders",
                             "aauw_pref_l_agree_disagree_maj" = "AAUW Disagree with Leaders", ##Democratic
                             "ada_pref_l_agree_disagree_maj" = "ADA Disagree with Leaders",
                             "afl_pref_l_agree_disagree_maj" = "AFL Disagree with Leaders",
                             "nea_pref_l_agree_disagree_maj" = "NEA Disagree with Leaders",
                             "lcv_pref_l_agree_disagree_maj" = "LCV Disagree with Leaders",
                             "naacp_pref_l_agree_disagree_maj" = "NAACP Disagree with Leaders",
                             "ara_pref_l_agree_disagree_maj" = "ARA Disagree with Leaders",
                             "acu_pref_l_agree_disagree_min" = "ACU Disagree with Leaders", ##Republican
                             "nfib_pref_l_agree_disagree_min" = "NFIB Disagree with Leaders",
                             "frc_pref_l_agree_disagree_min" = "FRC Disagree with Leaders",
                             "fw_pref_l_agree_disagree_min" = "Freedom Works Disagree with Leaders",
                             "cg_pref_l_agree_disagree_min" = "Club for Growth Disagree with Leaders",
                             "cc_pref_l_agree_disagree_min" = "Chamber of Commerce Disagree with Leaders",
                             "fair_pref_l_agree_disagree_min" = "FAIR Disagree with Leaders",
                             "aauw_pref_l_agree_disagree_min" = "AAUW Disagree with Leaders", ##Democratic
                             "ada_pref_l_agree_disagree_min" = "ADA Disagree with Leaders",
                             "afl_pref_l_agree_disagree_min" = "AFL Disagree with Leaders",
                             "nea_pref_l_agree_disagree_min" = "NEA Disagree with Leaders",
                             "lcv_pref_l_agree_disagree_min" = "LCV Disagree with Leaders",
                             "naacp_pref_l_agree_disagree_min" = "NAACP Disagree with Leaders",
                             "ara_pref_l_agree_disagree_min" = "ARA Disagree with Leaders",
                             "num_ig_l_agree_NA_maj" = "Number Aligned IG Agreeing with Leaders",
                             "num_ig_l_agree_NA_min" = "Number Aligned IG Agreeing with Leaders",
                             "vote_other" = "Other Vote",
                             "vote_passage" = "Passage Vote",
                             "vote_passage_constrained" = "Passage Vote (Constrained)",
                             "chamber_catSenate" = "Senate",
                             "divided_cong" = "Divided Congress",
                             "ext_nominate" = "Extremity NOMINATE Sponsor",
                             "ImpBill" = "Important Bills (not minor)"),
             output = "TableE4.docx")


#############################################
##  Stack data to test maj/min interaction ##
#############################################
##change committee vars to have maj/min at end (also dropping "mean" because text extraction removes mean)
data$intra_agree_maj<- data$maj_intra_agree_mean
data$intra_agree_min<- data$min_intra_agree_mean
summary(data$intra_agree_maj)
summary(data$intra_agree_min)

##create variable for interest groups from both parties scored vote
data$both_party_ig_scored<- ifelse(data$num_ig_aligned_maj>0 & data$num_ig_aligned_min>0, 1, 0)

data1<- data %>%
  select(cong_chamber_rollv1, vote_leader_pct_maj, vote_leader_pct_min,
         num_ig_aligned_maj, num_ig_aligned_min,
         num_ig_l_agree_maj, num_ig_l_agree_min,
         num_ig_l_disagree_maj, num_ig_l_disagree_min,
         one_plus_ig_l_disagree_NA_maj, one_plus_ig_l_disagree_NA_min,
         one_plus_ig_l_agree_NA_maj, one_plus_ig_l_agree_NA_min,
         intra_agree_maj, intra_agree_min,
         leadership_vote_cat_maj, leadership_vote_cat_min,
         num_ig_lib, num_ig_cons,
         vote_passage, vote_passage_constrained, vote_other,
         cong, chamber_cat, session, year, v1, majority,
         divided_cong, ext_nominate, ImpBill, majortopic_ig, cq_story,
         both_party_ig_scored) %>%
  gather(key = variables, value = values, 
         vote_leader_pct_maj, vote_leader_pct_min,
         num_ig_aligned_maj, num_ig_aligned_min,
         num_ig_l_agree_maj, num_ig_l_agree_min,
         leadership_vote_cat_maj, leadership_vote_cat_min,
         num_ig_l_disagree_maj, num_ig_l_disagree_min,
         one_plus_ig_l_disagree_NA_maj, one_plus_ig_l_disagree_NA_min,
         one_plus_ig_l_agree_NA_maj, one_plus_ig_l_agree_NA_min,
         intra_agree_maj, intra_agree_min) %>% 
  #here you will now have a super long format data where all of the variables are grouped together under the column 'variables' and all of the corresponding values are in a column called 'values'
  mutate(party_control = if_else(!is.na(str_extract(variables, "maj")), "majority", "minority"), #indicator column for if variable is associated with minority or majority party
         variables = str_remove(variables, "_m..")) %>%  #removes the min and maj portion so the columns will house min and maj values together (so 'num_agree_maj' and 'num_agree_min' are just 'num_agree') 
  spread(key = variables, value = values) %>% #this will now spread out the data so each so-called variable will be its own column 
  arrange(cong_chamber_rollv1, party_control) #just to see if it did what you were hoping for

##Define as numeric or factor
data1$one_plus_ig_l_disagree_NA<- as.numeric(data1$one_plus_ig_l_disagree_NA)
data1$one_plus_ig_l_agree_NA<- as.numeric(data1$one_plus_ig_l_agree_NA)
data1$party_control<- as.factor(data1$party_control)
data1$chamber_cat<- as.factor(data1$chamber_cat)
data1$num_ig_aligned<- as.numeric(data1$num_ig_aligned)
data1$intra_agree<- as.numeric(data1$intra_agree)
data1$majority<- as.factor(data1$majority)
##create variable for minority party
data1$minority<- ifelse(data1$majority=="Dem", "Rep",
                        ifelse(data1$majority=="GOP", "Dem", NA))
##create variable for relevant party of group scoring in one_plus_ig_l_disagree_NA
data1$ig_party<- ifelse((data1$party_control=="majority" & data1$majority=="Dem")|   ##CHECK THIS - not sure this is right yet
                          (data1$party_control=="minority" & data1$majority=="GOP"), "Dem",
                        ifelse((data1$party_control=="majority" & data1$majority=="GOP")|
                                 (data1$party_control=="minority" & data1$majority=="Dem"), "GOP", NA))
summary(as.factor(data1$ig_party))

####################
##Create dataset with subsets
data1.scored<- subset(data1, data1$num_ig_aligned>0)
data1.scored.both<- subset(data, data1$both_party_ig_scored==1)

####################
##Regression for 1+ IG disagreeing with Leadership, cluster SE by bill (Table E2 and Table E7)

##subset on votes scored by 1+ group in that party
##Logistic with clustered SE (Table E2.1)
ig_disagree_logit_stacked_2<- glm(one_plus_ig_l_disagree_NA ~ party_control + vote_passage + vote_passage_constrained + vote_other + chamber_cat + divided_cong +
                                          ext_nominate + ImpBill + as.factor(majortopic_ig), 
                                        data=data1,
                                        subset=c(data1$num_ig_aligned>0),
                                        family="binomial")
summary(ig_disagree_logit_stacked_2)

ig_disagree_logit_stacked_2cluster<- coeftest(ig_disagree_logit_stacked_2, vcov = vcovCL, type="HC1", cluster= ~cong_chamber_rollv1)
ig_disagree_logit_stacked_2cluster

##Logistic with clustered SE, Subset by party (Table E7.1)
##Democrats
ig_disagree_logit_stacked_2_dem<- glm(one_plus_ig_l_disagree_NA ~ party_control + vote_passage + vote_passage_constrained + vote_other + chamber_cat + divided_cong +
                                            ext_nominate + ImpBill + as.factor(majortopic_ig), 
                                          data=data1,
                                          subset=c(data1$num_ig_aligned>0 & data1$ig_party=="Dem"), 
                                          family="binomial")
summary(ig_disagree_logit_stacked_2_dem)

ig_disagree_logit_stacked_2_dem_cluster<- coeftest(ig_disagree_logit_stacked_2_dem, vcov = vcovCL, type="HC1", cluster= ~cong_chamber_rollv1)
ig_disagree_logit_stacked_2_dem_cluster

##Republicans (Table E7.3)
ig_disagree_logit_stacked_2_rep<- glm(one_plus_ig_l_disagree_NA ~ party_control + vote_passage + vote_passage_constrained + vote_other + chamber_cat + divided_cong +
                                        ext_nominate + ImpBill + as.factor(majortopic_ig), 
                                      data=data1,
                                      subset=c(data1$num_ig_aligned>0 & data1$ig_party=="GOP"),  
                                      family="binomial")
summary(ig_disagree_logit_stacked_2_rep)

ig_disagree_logit_stacked_2_rep_cluster<- coeftest(ig_disagree_logit_stacked_2_rep, vcov = vcovCL, type="HC1", cluster= ~cong_chamber_rollv1)
ig_disagree_logit_stacked_2_rep_cluster


#######################
##subset on votes scored by MAJORITY AND MINORITY aligned
##Logistic, clustered standard errors (Table E2.2)
ig_disagree_logit_stacked_3<- glm(one_plus_ig_l_disagree_NA ~ party_control + vote_passage + vote_passage_constrained + vote_other + chamber_cat + divided_cong +
                                    ext_nominate + ImpBill + as.factor(majortopic_ig), 
                                  data=data1,
                                  subset=c(data1$both_party_ig_scored==1),
                                  family="binomial")
summary(ig_disagree_logit_stacked_3)

ig_disagree_logit_stacked_3cluster<- coeftest(ig_disagree_logit_stacked_3, vcov = vcovCL, type="HC1", cluster= ~cong_chamber_rollv1)
ig_disagree_logit_stacked_3cluster

##Logistic with clustered SE, Subset by party
##Democrats (Table E7.2)
ig_disagree_logit_stacked_3_dem<- glm(one_plus_ig_l_disagree_NA ~ party_control + vote_passage + vote_passage_constrained + vote_other + chamber_cat + divided_cong +
                                        ext_nominate + ImpBill + as.factor(majortopic_ig), 
                                      data=data1,
                                      subset=c(data1$both_party_ig_scored==1 & data1$ig_party=="Dem"), 
                                      family="binomial")
summary(ig_disagree_logit_stacked_3_dem)

ig_disagree_logit_stacked_3_dem_cluster<- coeftest(ig_disagree_logit_stacked_3_dem, vcov = vcovCL, type="HC1", cluster= ~cong_chamber_rollv1)
ig_disagree_logit_stacked_3_dem_cluster

##Republicans (Table E7.4)
ig_disagree_logit_stacked_3_rep<- glm(one_plus_ig_l_disagree_NA ~ party_control + vote_passage + vote_passage_constrained + vote_other + chamber_cat + divided_cong +
                                        ext_nominate + ImpBill + as.factor(majortopic_ig), 
                                      data=data1,
                                      subset=c(data1$both_party_ig_scored==1 & data1$ig_party=="GOP"),  
                                      family="binomial")
summary(ig_disagree_logit_stacked_3_rep)

ig_disagree_logit_stacked_3_rep_cluster<- coeftest(ig_disagree_logit_stacked_3_rep, vcov = vcovCL, type="HC1", cluster= ~cong_chamber_rollv1)
ig_disagree_logit_stacked_3_rep_cluster


###########################
##output models
##DV1: Did 1+ group disagree (Table E2)
stacked_models<- list(
  "Own Side Scored Votes"=ig_disagree_logit_stacked_2cluster,
  "Both Scored Votes"=ig_disagree_logit_stacked_3cluster
)
modelsummary(stacked_models, 
             estimate = "{estimate}{stars}",
             coef_rename = c("(Intercept)" = "Constant",
                             "party_controlminority"= "Minority Party",
                             "vote_other"= "Other Vote",
                             "vote_passage"= "Passage Vote",
                             "vote_passage_constrained"= "Passage Vote (Constrained)",
                             "chamber_catSenate"= "Senate",
                             "divided_cong"= "Divided Congress",
                             "ext_nominate"= "Extremity of Bill Sponsor (NOMINATE)",
                             "ImpBill"= "Important Bills (not minor)",
                             "as.factor(majortopic_ig)1"= "Macroeconomics",
                             "as.factor(majortopic_ig)2"= "Civil Rights",
                             "as.factor(majortopic_ig)5"= "Labor",
                             "as.factor(majortopic_ig)6"= "Education",
                             "as.factor(majortopic_ig)7"= "Environment",
                             "as.factor(majortopic_ig)9"= "Immigration",
                             "as.factor(majortopic_ig)13"= "Social Welfare",
                             "as.factor(majortopic_ig)15"= "Domestic Commerce and Small Bus"),
             output = "TableE2.docx")

stacked_models_party<- list(
  "(Dem) Own Side Scored Votes"=ig_disagree_logit_stacked_2_dem_cluster,
  "(Dem) Both Scored Votes"=ig_disagree_logit_stacked_3_dem_cluster,
  "(GOP) Own Side Scored Votes"=ig_disagree_logit_stacked_2_rep_cluster,
  "(GOP) Both Scored Votes"=ig_disagree_logit_stacked_3_rep_cluster
)
modelsummary(stacked_models_party, 
             estimate = "{estimate}{stars}",
             coef_rename = c("(Intercept)" = "Constant",
                             "party_controlminority"= "Minority Party",
                             "vote_other"= "Other Vote",
                             "vote_passage"= "Passage Vote",
                             "vote_passage_constrained"= "Passage Vote (Constrained)",
                             "chamber_catSenate"= "Senate",
                             "divided_cong"= "Divided Congress",
                             "ext_nominate"= "Extremity of Bill Sponsor (NOMINATE)",
                             "ImpBill"= "Important Bills (not minor)",
                             "as.factor(majortopic_ig)1"= "Macroeconomics",
                             "as.factor(majortopic_ig)2"= "Civil Rights",
                             "as.factor(majortopic_ig)5"= "Labor",
                             "as.factor(majortopic_ig)6"= "Education",
                             "as.factor(majortopic_ig)7"= "Environment",
                             "as.factor(majortopic_ig)9"= "Immigration",
                             "as.factor(majortopic_ig)13"= "Social Welfare",
                             "as.factor(majortopic_ig)15"= "Domestic Commerce and Small Bus"),
             output = "TableE7.docx")


###################################################################################
##    Descriptive statistics for scored votes with disagreement vs. agreement    ##
###################################################################################

##Leadership position (Table C6)
##Scored votes with disagreement
##majority 
summary(as.factor(data1$leadership_vote_cat[data1$one_plus_ig_l_disagree_NA>0 & data1$party_control=="majority"]))
summary(as.factor(data1$leadership_vote_cat[data1$one_plus_ig_l_disagree_NA>0 & data1$party_control=="majority" & data1$vote_passage==1]))
##minority
summary(as.factor(data1$leadership_vote_cat[data1$one_plus_ig_l_disagree_NA>0 & data1$party_control=="minority"]))
summary(as.factor(data1$leadership_vote_cat[data1$one_plus_ig_l_disagree_NA>0 & data1$party_control=="minority" & data1$vote_passage==1]))

##Scored votes with agreement
##majority
summary(as.factor(data1$leadership_vote_cat[data1$one_plus_ig_l_agree_NA>0 & data1$party_control=="majority"]))
summary(as.factor(data1$leadership_vote_cat[data1$one_plus_ig_l_agree_NA>0 & data1$party_control=="majority" & data1$vote_passage==1]))
##minority
summary(as.factor(data1$leadership_vote_cat[data1$one_plus_ig_l_agree_NA>0 & data1$party_control=="minority"]))
summary(as.factor(data1$leadership_vote_cat[data1$one_plus_ig_l_agree_NA>0 & data1$party_control=="minority" & data1$vote_passage==1]))

#########################
##Table C7

##percent passage votes
##disagreement
summary(data1$vote_passage[data1$one_plus_ig_l_disagree_NA>0]) 
##agreement
summary(data1$vote_passage[data1$one_plus_ig_l_agree_NA>0]) 

##percent passage_constrained
##disagreement
summary(data1$vote_passage_constrained[data1$one_plus_ig_l_disagree_NA>0]) 
##agreement
summary(data1$vote_passage_constrained[data1$one_plus_ig_l_agree_NA>0]) 

########################
##chamber
##disagreement
summary(as.factor(data1$chamber_cat[data1$one_plus_ig_l_disagree_NA>0]))
##agreement
summary(as.factor(data1$chamber_cat[data1$one_plus_ig_l_agree_NA>0]))

#########################
##divided congress
##disagreement
summary(data$divided_cong[data1$one_plus_ig_l_disagree_NA>0])
##agreement
summary(data$divided_cong[data1$one_plus_ig_l_agree_NA>0])

#######################
##Bill salience
##important bill indicator
##disagreement
summary(data1$ImpBill[data1$one_plus_ig_l_disagree_NA>0])
##agreement
summary(data1$ImpBill[data1$one_plus_ig_l_agree_NA>0])

##CQ story
##disagreement
summary(data1$cq_story[data1$one_plus_ig_l_disagree_NA>0])
##agreement
summary(data1$cq_story[data1$one_plus_ig_l_agree_NA>0])



