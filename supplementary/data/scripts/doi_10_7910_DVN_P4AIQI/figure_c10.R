# ----------------------------------------------------------------------------
# INFO  
# ----------------------------------------------------------------------------

# Article: The Electoral Politics of Immigration and Crime
# Author(s): Jeyhun Alizade
# Code to replicate Figure C.10

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

mod_cdu_morerestrict_green = lm(eval.CDU~CDU_more_restrict_greens, data=dat[dat$voted_last=="Gruene",]) # OLS regression
mod_cdu_morerestrict_green_cluster = coeftest(mod_cdu_morerestrict_green, cluster.vcov(mod_cdu_morerestrict_green, ~ID)) # clustered standard errors (by respondent)

mod_cdu_morerestrict_spd = lm(eval.CDU~CDU_more_restrict_spd, data=dat[dat$voted_last=="SPD",])
mod_cdu_morerestrict_spd_cluster = coeftest(mod_cdu_morerestrict_spd, cluster.vcov(mod_cdu_morerestrict_spd, ~ID))


# ----------------------------------------------------------------------------
# PLOTTING
# ----------------------------------------------------------------------------

# take out estimates and standard errors and save them in a data frame

mod_cdu_morerestrict_ests = rbind.data.frame(mod_cdu_morerestrict_green_cluster[-1,1:2], 
                                             mod_cdu_morerestrict_spd_cluster[-1,1:2])
names(mod_cdu_morerestrict_ests) = c("coef", "se")

# create variable for the sample of voters analyzed

mod_cdu_morerestrict_ests$voted_for = c("Green voter", "SPD voter")
mod_cdu_morerestrict_ests$voted_for = factor(mod_cdu_morerestrict_ests$voted_for, levels=rev(c("Green voter", "SPD voter")))

# create Figure C.10

ggplot(data=mod_cdu_morerestrict_ests, aes(x=voted_for, y=coef)) + 
  geom_point(size=2) +
  geom_errorbar(aes(ymin=coef-se*1.645, ymax=coef+se*1.645), width=0, linewidth=1) +
  geom_errorbar(aes(ymin=coef-se*1.96, ymax=coef+se*1.96), width=0, linewidth=.5) +
  coord_flip() +
  geom_hline(yintercept=0, linetype="dashed") +
  ylab("Effect on CDU/CSU Candidate Evaluation") + xlab("Voter subsample") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(color="black"),
        axis.title.x = element_text(margin = margin(t = 10), size=17),
        axis.title.y = element_text(margin = margin(r = 10))) +
  theme(text=element_text(size=18)) +
  scale_y_continuous(breaks = seq(0, .1, 0.1))

# save Figure C.10

ggsave("figures/figure_c10.pdf", width = 7, height = 5)