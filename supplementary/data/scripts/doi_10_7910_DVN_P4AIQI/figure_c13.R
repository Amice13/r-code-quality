# ----------------------------------------------------------------------------
# INFO  
# ----------------------------------------------------------------------------

# Article: The Electoral Politics of Immigration and Crime
# Author(s): Jeyhun Alizade
# Code to replicate Figure C.13

# ----------------------------------------------------------------------------
# SETUP
# ----------------------------------------------------------------------------

# clear environment
rm(list=ls())

# install and load necessary packages
# install.packages("ggplot2", "multiwayvcov", "lmtest")
library(ggplot2)
library(multiwayvcov)
library(lmtest)

# set working directory (change working directory to where the replication folder is saved on your computer)
setwd("/Users/jeyhunalizade/Dropbox/immigration crime project/ajps_replication/replication_files/")

# load data set
dat = read.csv("data/conjoint.csv", sep=",")


# ----------------------------------------------------------------------------
# PREPARE ANALYSIS
# ----------------------------------------------------------------------------

# CDU more restrictive than the Greens

dat$CDU_more_restrict_greens = ifelse(dat$deport.Gruene=="No deportations" & 
                                        (dat$deport.CDU=="Severe crimes" | dat$deport.CDU=="Any crime"), 1, 0)
dat$CDU_more_restrict_greens = ifelse(dat$deport.Gruene=="Severe crimes" & dat$deport.CDU=="Any crime", 1, dat$CDU_more_restrict_greens)

# CDU more restrictive than the SPD

dat$CDU_more_restrict_spd = ifelse(dat$deport.SPD=="No deportations" & 
                                     (dat$deport.CDU=="Severe crimes" | dat$deport.CDU=="Any crime"), 1, 0)
dat$CDU_more_restrict_spd = ifelse(dat$deport.SPD=="Severe crimes" & dat$deport.CDU=="Any crime", 1, dat$CDU_more_restrict_spd)

# recode variable measuring views on cultural predisiposition toward crime into character variable
dat$cult_predis[dat$cult_predis==1] = "Disagree strongly"
dat$cult_predis[dat$cult_predis==2] = "Disagree"
dat$cult_predis[dat$cult_predis==3] = "Agree"
dat$cult_predis[dat$cult_predis==4] = "Strongly agree"


# ----------------------------------------------------------------------------
# MODELS
# ----------------------------------------------------------------------------

mod_cdu_morerestrict_lo_cult_green = lm(elected.CDU~CDU_more_restrict_greens, data=dat[dat$voted_last=="Gruene" & (dat$cult_predis=="Disagree strongly" | dat$cult_predis=="Disagree"),])
mod_cdu_morerestrict_lo_cult_green_cluster = coeftest(mod_cdu_morerestrict_lo_cult_green, cluster.vcov(mod_cdu_morerestrict_lo_cult_green, ~ID)) # clustered standard errors (by respondent)

mod_cdu_morerestrict_lo_cult_spd = lm(elected.CDU~CDU_more_restrict_spd, data=dat[dat$voted_last=="SPD" & (dat$cult_predis=="Disagree strongly" | dat$cult_predis=="Disagree"),])
mod_cdu_morerestrict_lo_cult_spd_cluster = coeftest(mod_cdu_morerestrict_lo_cult_spd, cluster.vcov(mod_cdu_morerestrict_lo_cult_spd, ~ID))

mod_cdu_morerestrict_hi_cult_green = lm(elected.CDU~CDU_more_restrict_greens, data=dat[dat$voted_last=="Gruene" & (dat$cult_predis=="Agree" | dat$cult_predis=="Strongly agree"),])
mod_cdu_morerestrict_hi_cult_green_cluster = coeftest(mod_cdu_morerestrict_hi_cult_green, cluster.vcov(mod_cdu_morerestrict_hi_cult_green, ~ID))

mod_cdu_morerestrict_hi_cult_spd = lm(elected.CDU~CDU_more_restrict_spd, data=dat[dat$voted_last=="SPD" & (dat$cult_predis=="Agree" | dat$cult_predis=="Strongly agree"),])
mod_cdu_morerestrict_hi_cult_spd_cluster = coeftest(mod_cdu_morerestrict_hi_cult_spd, cluster.vcov(mod_cdu_morerestrict_hi_cult_spd, ~ID))


# ----------------------------------------------------------------------------
# PLOTTING
# ----------------------------------------------------------------------------

# take out estimates and standard errors and save them in a data frame

cult_ests = rbind.data.frame(mod_cdu_morerestrict_lo_cult_green_cluster[2,1:2], 
                             mod_cdu_morerestrict_lo_cult_spd_cluster[2, 1:2], 
                             mod_cdu_morerestrict_hi_cult_green_cluster[2, 1:2], 
                             mod_cdu_morerestrict_hi_cult_spd_cluster[2, 1:2])
names(cult_ests)[1:2] = c("coef", "se")

# create variable for the sample of voters analyzed

cult_ests$party=c("Green voter", "SPD voter", "Green voter", "SPD voter")
cult_ests$party=factor(cult_ests$party, levels=c("SPD voter", "Green voter"))

# create variable indicating attitude on cultural predisposition of immigrants toward crime

cult_ests$cultural_concern = c("(Strongly)\ndisagrees", "(Strongly)\ndisagrees", "(Strongly)\nagrees", "(Strongly)\nagrees")
cult_ests$cultural_concern = factor(cult_ests$cultural_concern, levels=c("(Strongly)\ndisagrees", "(Strongly)\nagrees"))

# create Figure C.13

ggplot(data=cult_ests, aes(x=party, y=coef, group=cultural_concern, shape=cultural_concern)) + 
  geom_point(size=3, position=position_dodge(width = .4)) +
  geom_errorbar(aes(ymin=coef-se*1.645, ymax=coef+se*1.645), width=0, linewidth=1, position=position_dodge(width = .4)) +
  geom_errorbar(aes(ymin=coef-se*1.96, ymax=coef+se*1.96), width=0, linewidth=.5, position=position_dodge(width = .4)) +
  coord_flip() +
  geom_hline(yintercept=0, linetype="dashed") +
  ylab("Effect on\nPr(Choosing CDU/CSU Candidate)") + xlab("Voter subsample") +
  theme_bw() +
  theme(panel.grid = element_blank(), text=element_text(size=18), legend.position = "bottom") +
  scale_shape_manual(values=c(16, 17), name="Immigrants culturally\npredisposed to crime") 

# save Figure C.13

ggsave("figures/figure_c13.pdf", width = 7, height = 5)
