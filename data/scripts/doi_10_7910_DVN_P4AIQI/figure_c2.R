# ----------------------------------------------------------------------------
# INFO  
# ----------------------------------------------------------------------------

# Article: The Electoral Politics of Immigration and Crime
# Author(s): Jeyhun Alizade
# Code to replicate Figure C.2

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

# AfD more restrictive than the Greens

dat$AfD_more_restrict_greens = ifelse(dat$deport.Gruene=="No deportations" & 
                                                 (dat$deport.AfD=="Severe crimes" | dat$deport.AfD=="Any crime"), 1, 0)
dat$AfD_more_restrict_greens = ifelse(dat$deport.Gruene=="Severe crimes" & dat$deport.AfD=="Any crime", 1, dat$AfD_more_restrict_greens)

# AfD more restrictive than the SPD

dat$AfD_more_restrict_spd = ifelse(dat$deport.SPD=="No deportations" & 
                                              (dat$deport.AfD=="Severe crimes" | dat$deport.AfD=="Any crime"), 1, 0)
dat$AfD_more_restrict_spd = ifelse(dat$deport.SPD=="Severe crimes" & dat$deport.AfD=="Any crime", 1, dat$AfD_more_restrict_spd)

# AfD more restrictive than the CDU

dat$AfD_more_restrict_cdu = ifelse(dat$deport.CDU=="Severe crimes" & dat$deport.AfD=="Any crime", 1, 0)


# ----------------------------------------------------------------------------
# MODELS
# ----------------------------------------------------------------------------

mod_afd_morerestrict_green = lm(elected.AfD~AfD_more_restrict_greens, data=dat[dat$voted_last=="Gruene",]) # OLS regression
mod_afd_morerestrict_green_cluster = coeftest(mod_afd_morerestrict_green, cluster.vcov(mod_afd_morerestrict_green, ~ID)) # clustered standard errors (by respondent)

mod_afd_morerestrict_spd = lm(elected.AfD~AfD_more_restrict_spd, data=dat[dat$voted_last=="SPD",])
mod_afd_morerestrict_spd_cluster = coeftest(mod_afd_morerestrict_spd, cluster.vcov(mod_afd_morerestrict_spd, ~ID))

mod_afd_morerestrict_cdu = lm(elected.AfD~AfD_more_restrict_cdu, data=dat[dat$voted_last=="CDU",])
mod_afd_morerestrict_cdu_cluster = coeftest(mod_afd_morerestrict_cdu, cluster.vcov(mod_afd_morerestrict_cdu, ~ID))


# ----------------------------------------------------------------------------
# PLOTTING
# ----------------------------------------------------------------------------

# take out estimates and standard errors and save them in a data frame

mod_afd_morerestrict_ests = rbind.data.frame(mod_afd_morerestrict_green_cluster[-1,1:2], 
                                             mod_afd_morerestrict_spd_cluster[-1,1:2],
                                             mod_afd_morerestrict_cdu_cluster[-1,1:2])
names(mod_afd_morerestrict_ests) = c("coef", "se")

# create variable for sample of voters analyzed

mod_afd_morerestrict_ests$voted_for = c("Green voter", "SPD voter", "CDU/CSU voter")
mod_afd_morerestrict_ests$voted_for = factor(mod_afd_morerestrict_ests$voted_for, levels=rev(c("Green voter", "SPD voter", "CDU/CSU voter")))

# create Figure C.2 

ggplot(data=mod_afd_morerestrict_ests, aes(x=voted_for, y=coef)) + 
  geom_point(size=2) +
  geom_errorbar(aes(ymin=coef-se*1.645, ymax=coef+se*1.645), width=0, linewidth=1) +
  geom_errorbar(aes(ymin=coef-se*1.96, ymax=coef+se*1.96), width=0, linewidth=.5) +
  coord_flip() +
  geom_hline(yintercept=0, linetype="dashed") +
  ylab("Effect on Pr(Choosing AfD Candidate)") + xlab("Voter subsample") +
  theme_bw() +
  theme(panel.grid = element_blank(), text=element_text(size=20))

# save Figure C.2

ggsave("figures/figure_c2.pdf", width = 7, height = 5)
