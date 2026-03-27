# ----------------------------------------------------------------------------
# INFO  
# ----------------------------------------------------------------------------

# Article: The Electoral Politics of Immigration and Crime
# Author(s): Jeyhun Alizade
# Code to replicate Figure 7

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

# Treatment effects by CDU position on cause of immigrant crime

mod_cdu_morerestrict_green_pov = lm(elected.CDU~CDU_more_restrict_greens, data=dat[dat$voted_last=="Gruene" & dat$cause.CDU=="Poverty",]) # OLS regression 
mod_cdu_morerestrict_green_pov_cluster = coeftest(mod_cdu_morerestrict_green_pov, cluster.vcov(mod_cdu_morerestrict_green_pov, ~ID)) # Clustered standard errors (by respondent)

mod_cdu_morerestrict_green_cult = lm(elected.CDU~CDU_more_restrict_greens, data=dat[dat$voted_last=="Gruene" & dat$cause.CDU=="Culture",])
mod_cdu_morerestrict_green_cult_cluster = coeftest(mod_cdu_morerestrict_green_cult, cluster.vcov(mod_cdu_morerestrict_green_cult, ~ID))

mod_cdu_morerestrict_spd_pov = lm(elected.CDU~CDU_more_restrict_spd, data=dat[dat$voted_last=="SPD" & dat$cause.CDU=="Poverty",])
mod_cdu_morerestrict_spd_pov_cluster = coeftest(mod_cdu_morerestrict_spd_pov, cluster.vcov(mod_cdu_morerestrict_spd_pov, ~ID))

mod_cdu_morerestrict_spd_cult = lm(elected.CDU~CDU_more_restrict_spd, data=dat[dat$voted_last=="SPD" & dat$cause.CDU=="Culture",])
mod_cdu_morerestrict_spd_cult_cluster = coeftest(mod_cdu_morerestrict_spd_cult, cluster.vcov(mod_cdu_morerestrict_spd_cult, ~ID))

# Treatment effects by CDU position on same-sex adoption rights

mod_cdu_morerestrict_green_approve = lm(elected.CDU~CDU_more_restrict_greens, data=dat[dat$voted_last=="Gruene" & dat$adopt.CDU=="Approves",])
mod_cdu_morerestrict_green_approve_cluster = coeftest(mod_cdu_morerestrict_green_approve, cluster.vcov(mod_cdu_morerestrict_green_approve, ~ID))

mod_cdu_morerestrict_green_reject = lm(elected.CDU~CDU_more_restrict_greens, data=dat[dat$voted_last=="Gruene" & dat$adopt.CDU=="Rejects",])
mod_cdu_morerestrict_green_reject_cluster = coeftest(mod_cdu_morerestrict_green_cult, cluster.vcov(mod_cdu_morerestrict_green_reject, ~ID))

mod_cdu_morerestrict_spd_approve = lm(elected.CDU~CDU_more_restrict_spd, data=dat[dat$voted_last=="SPD" & dat$adopt.CDU=="Approves",])
mod_cdu_morerestrict_spd_approve_cluster = coeftest(mod_cdu_morerestrict_spd_approve, cluster.vcov(mod_cdu_morerestrict_spd_approve, ~ID))

mod_cdu_morerestrict_spd_reject = lm(elected.CDU~CDU_more_restrict_spd, data=dat[dat$voted_last=="SPD" & dat$adopt.CDU=="Rejects",])
mod_cdu_morerestrict_spd_reject_cluster = coeftest(mod_cdu_morerestrict_spd_cult, cluster.vcov(mod_cdu_morerestrict_spd_reject, ~ID))


# ----------------------------------------------------------------------------
# PLOTTING
# ----------------------------------------------------------------------------

## PANEL A: CAUSE OF IMMIGRANT CRIME ###

# take out estimates and standard errors and save them in a data frame

mod_cause_ests = rbind.data.frame(mod_cdu_morerestrict_green_pov_cluster[2,1:2],
                                  mod_cdu_morerestrict_green_cult_cluster[2,1:2],
                                  mod_cdu_morerestrict_spd_pov_cluster[2,1:2],
                                  mod_cdu_morerestrict_spd_cult_cluster[2,1:2])
names(mod_cause_ests) = c("coef", "se")

# create variable for sample of voters analyzed

mod_cause_ests$party=rep(c("Green voter", "SPD voter"), each=2)
mod_cause_ests$party = factor(mod_cause_ests$party, levels=c("SPD voter", "Green voter"))

# create variable for the attribute level

mod_cause_ests$condition = rep(c("Poverty", "Culture"), 2)
mod_cause_ests$condition = factor(mod_cause_ests$condition, levels=c("Poverty", "Culture"))

# create Figure 7(a)

ggplot(data = mod_cause_ests, 
       aes(x = party, y = coef, group = factor(condition), shape = factor(condition))) + 
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = coef - se * 1.645, ymax = coef + se * 1.645), 
                position = position_dodge(width = 0.5), width = 0, linewidth = 1) +
  geom_errorbar(aes(ymin = coef - se * 1.96, ymax = coef + se * 1.96), 
                position = position_dodge(width = 0.5), width = 0, linewidth = 0.5) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(data=mod_cause_ests[mod_cause_ests$party=="Green voter",],
            aes(label = condition), 
            position = position_dodge(width = 0.5), vjust = -0.8, size = 5) +
  ylab("Effect on Pr(choosing CDU/CSU candidate)") + 
  xlab("") +
  theme_bw() +
  theme(panel.grid = element_blank(), text = element_text(size = 20), legend.position = "none",
        axis.text = element_text(color = "black"),
        axis.title.x = element_text(margin = margin(t = 10), size = 17),
        axis.title.y = element_text(margin = margin(r = 10))) +
  scale_shape_manual(values = c(16, 17), name = "") +
  scale_y_continuous(limits = c(0, 0.15))

# save Figure 7(a)

ggsave("figures/figure_7a.pdf", width=7, height=5)


## PANEL B: SAME-SEX ADOPTION RIGHTS ###

# take out estimates and standard errors and save them in a data frame

mod_adopt_ests = rbind.data.frame(mod_cdu_morerestrict_green_approve_cluster[2,1:2],
                                  mod_cdu_morerestrict_green_reject_cluster[2,1:2],
                                  mod_cdu_morerestrict_spd_approve_cluster[2,1:2],
                                  mod_cdu_morerestrict_spd_reject_cluster[2,1:2])
names(mod_adopt_ests) = c("coef", "se")

# create variable for sample of voters analyzed

mod_adopt_ests$party=rep(c("Green voter", "SPD voter"), each=2)
mod_adopt_ests$party = factor(mod_adopt_ests$party, levels=c("SPD voter", "Green voter"))

# create variable for the attribute level

mod_adopt_ests$condition = rep(c("Approves", "Rejects"), 2)

# create Figure 7(b)

ggplot(data = mod_adopt_ests[mod_adopt_ests$party != "AfD voter", ], 
       aes(x = party, y = coef, group = factor(condition), shape = factor(condition))) + 
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = coef - se * 1.645, ymax = coef + se * 1.645), 
                position = position_dodge(width = 0.5), width = 0, linewidth = 1) +
  geom_errorbar(aes(ymin = coef - se * 1.96, ymax = coef + se * 1.96), 
                position = position_dodge(width = 0.5), width = 0, linewidth = 0.5) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(data=mod_adopt_ests[mod_adopt_ests$party=="Green voter",],
            aes(label = condition), 
            position = position_dodge(width = 0.5), vjust = -0.8, size = 5) +
  ylab("Effect on Pr(choosing CDU/CSU candidate)") + 
  xlab("") +
  theme_bw() +
  theme(panel.grid = element_blank(), text = element_text(size = 20), legend.position = "none",
        axis.text = element_text(color = "black"),
        axis.title.x = element_text(margin = margin(t = 10), size = 16),
        axis.title.y = element_text(margin = margin(r = 10))) +
  scale_shape_manual(values = c(16, 17), name = "") +
  scale_y_continuous(limits = c(0, 0.15))

# save Figure 7(b)

ggsave("figures/figure_7b.pdf", width=7, height=5)
