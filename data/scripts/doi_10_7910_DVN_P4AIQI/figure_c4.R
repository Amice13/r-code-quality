# ----------------------------------------------------------------------------
# INFO  
# ----------------------------------------------------------------------------

# Article: The Electoral Politics of Immigration and Crime
# Author(s): Jeyhun Alizade
# Code to replicate Figure C.4

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
# DATA PREPARATION
# ----------------------------------------------------------------------------

# change reference level for cause of immigrant crime

dat$cause.CDU = factor(dat$cause.CDU, levels=c("Poverty", "Culture"))

# CDU more restrictive than the Greens

dat$CDU_more_restrict_greens = ifelse(dat$deport.Gruene=="No deportations" & 
                                        (dat$deport.CDU=="Severe crimes" | dat$deport.CDU=="Any crime"), 1, 0)
dat$CDU_more_restrict_greens = ifelse(dat$deport.Gruene=="Severe crimes" & dat$deport.CDU=="Any crime", 1, dat$CDU_more_restrict_greens)

# CDU more restrictive than the SPD

dat$CDU_more_restrict_spd = ifelse(dat$deport.SPD=="No deportations" & 
                                     (dat$deport.CDU=="Severe crimes" | dat$deport.CDU=="Any crime"), 1, 0)
dat$CDU_more_restrict_spd = ifelse(dat$deport.SPD=="Severe crimes" & dat$deport.CDU=="Any crime", 1, dat$CDU_more_restrict_spd)


# ----------------------------------------------------------------------------
# MODELS
# ----------------------------------------------------------------------------

# Interaction between CDU/CSU being more restrictive and CDU/CSU position on cause of immigrant crime

mod_cdu_morerestrict_green_cause = lm(elected.CDU~CDU_more_restrict_greens*cause.CDU, data=dat[dat$voted_last=="Gruene",])
mod_cdu_morerestrict_green_cause_cluster = coeftest(mod_cdu_morerestrict_green_cause, cluster.vcov(mod_cdu_morerestrict_green_cause, ~ID)) # clustered standard errors (by respondent)

mod_cdu_morerestrict_spd_cause = lm(elected.CDU~CDU_more_restrict_spd*cause.CDU, data=dat[dat$voted_last=="SPD",])
mod_cdu_morerestrict_spd_cause_cluster = coeftest(mod_cdu_morerestrict_spd_cause, cluster.vcov(mod_cdu_morerestrict_spd_cause, ~ID))

# Interaction between CDU/CSU being more restrictive and CDU/CSU position on same-sex adoption rights

mod_cdu_morerestrict_green_adopt = lm(elected.CDU~CDU_more_restrict_greens*adopt.CDU, data=dat[dat$voted_last=="Gruene",])
mod_cdu_morerestrict_green_adopt_cluster = coeftest(mod_cdu_morerestrict_green_adopt, cluster.vcov(mod_cdu_morerestrict_green_adopt, ~ID))

mod_cdu_morerestrict_spd_adopt = lm(elected.CDU~CDU_more_restrict_spd*adopt.CDU, data=dat[dat$voted_last=="SPD",])
mod_cdu_morerestrict_spd_adopt_cluster = coeftest(mod_cdu_morerestrict_spd_adopt, cluster.vcov(mod_cdu_morerestrict_spd_adopt, ~ID))


# ----------------------------------------------------------------------------
# PLOTTING
# ----------------------------------------------------------------------------

# take out estimates and standard errors and save them in a data frame

mod_causeadopt_ests = rbind.data.frame(mod_cdu_morerestrict_green_cause_cluster[4,1:2],
                                       mod_cdu_morerestrict_spd_cause_cluster[4,1:2],
                                       mod_cdu_morerestrict_green_adopt_cluster[4,1:2],
                                       mod_cdu_morerestrict_spd_adopt_cluster[4,1:2])
names(mod_causeadopt_ests) = c("coef", "se")

# create variable for candidate attribute

mod_causeadopt_ests$var = rep(c("Cause of immigrant crime:\nCulture", "Adoption rights:\nRejects"), each=2)
mod_causeadopt_ests$var = factor(mod_causeadopt_ests$var, levels=c("Cause of immigrant crime:\nCulture", "Adoption rights:\nRejects"))

# create variable for sample of voters analyzed

mod_causeadopt_ests$party = rep(c("Green voter", "SPD voter"), 2)
mod_causeadopt_ests$party = factor(mod_causeadopt_ests$party, levels=c("SPD voter", "Green voter"))

# create Figure C.4

ggplot(data=mod_causeadopt_ests, aes(x=party, y=coef)) + 
  facet_wrap(~var) + 
  geom_point(size=2) +
  geom_errorbar(aes(ymin=coef-se*1.645, ymax=coef+se*1.645), width=0, size=1) +
  geom_errorbar(aes(ymin=coef-se*1.96, ymax=coef+se*1.96), width=0, size=.5) +
  coord_flip() +
  geom_hline(yintercept=0, linetype="dashed") +
  ylab("Interaction Effect on\nPr(Choosing CDU/CSU Candidate)") + xlab("Voter subsample") +
  theme_bw() +
  theme(panel.grid = element_blank(), text=element_text(size=20)) +
  scale_y_continuous(limits=c(-0.1, .18))

# save Figure C.4

ggsave("figures/figure_c4.pdf", width = 7, height = 5)