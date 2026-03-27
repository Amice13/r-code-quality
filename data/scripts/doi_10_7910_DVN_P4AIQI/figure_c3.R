# ----------------------------------------------------------------------------
# INFO  
# ----------------------------------------------------------------------------

# Article: The Electoral Politics of Immigration and Crime
# Author(s): Jeyhun Alizade
# Code to replicate Figure C.3

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

# SPD more restrictive than the Greens

dat$SPD_more_restrict_greens = ifelse(dat$deport.Gruene=="No deportations" & 
                                                 (dat$deport.SPD=="Severe crimes" | dat$deport.SPD=="Any crime"), 1, 0)
dat$SPD_more_restrict_greens = ifelse(dat$deport.Gruene=="Severe crimes" & dat$deport.SPD=="Any crime", 1, dat$SPD_more_restrict_greens)


# ----------------------------------------------------------------------------
# MODELS
# ----------------------------------------------------------------------------

mod_cdu_morerestrict_green_spd_not = lm(elected.CDU~CDU_more_restrict_greens, data=dat[dat$voted_last=="Gruene" & dat$SPD_more_restrict_greens==0,]) # OLS regressions
mod_cdu_morerestrict_green_spd_not_cluster = coeftest(mod_cdu_morerestrict_green_spd_not, cluster.vcov(mod_cdu_morerestrict_green_spd_not, ~ID)) # Clustered standard errors (by respondent)

mod_cdu_morerestrict_green_spd_too = lm(elected.CDU~CDU_more_restrict_greens, data=dat[dat$voted_last=="Gruene" & dat$SPD_more_restrict_greens==1,])
mod_cdu_morerestrict_green_spd_too_cluster = coeftest(mod_cdu_morerestrict_green_spd_too, cluster.vcov(mod_cdu_morerestrict_green_spd_too, ~ID))


# ----------------------------------------------------------------------------
# PLOTTING
# ----------------------------------------------------------------------------

# take out estimates and standard errors and save them in a data frame

mod_cdu_morerestrict_green_spd_too_ests = rbind.data.frame(mod_cdu_morerestrict_green_spd_not_cluster[2,1:2],
                                                           mod_cdu_morerestrict_green_spd_too_cluster[2,1:2])
names(mod_cdu_morerestrict_green_spd_too_ests) = c("coef", "se")


# create variable indicating whether the SPD is also more restrictive than the Greens

mod_cdu_morerestrict_green_spd_too_ests$model = c("CDU/CSU more restrictive,\nSPD not", "CDU/CSU more restrictive,\nSPD too") 
mod_cdu_morerestrict_green_spd_too_ests$model = factor(mod_cdu_morerestrict_green_spd_too_ests$model,
                                                       levels=c("CDU/CSU more restrictive,\nSPD too", "CDU/CSU more restrictive,\nSPD not"))

# create Figure C.3

ggplot(data=mod_cdu_morerestrict_green_spd_too_ests, aes(x=model, y=coef)) + 
  geom_point(size=2) +
  geom_errorbar(aes(ymin=coef-se*1.645, ymax=coef+se*1.645), width=0, linewidth=1) +
  geom_errorbar(aes(ymin=coef-se*1.96, ymax=coef+se*1.96), width=0, linewidth=.5) +
  coord_flip() +
  geom_hline(yintercept=0, linetype="dashed") +
  ylab("Effect on\nPr(Choosing CDU/CSU Candidate)") + xlab("") +
  theme_bw() +
  theme(panel.grid = element_blank(), text=element_text(size=18))

# save Figure C.3

ggsave("figures/figure_c3.pdf", width = 7, height = 5)