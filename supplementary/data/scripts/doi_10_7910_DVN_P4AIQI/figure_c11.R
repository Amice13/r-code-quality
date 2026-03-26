# ----------------------------------------------------------------------------
# INFO  
# ----------------------------------------------------------------------------

# Article: The Electoral Politics of Immigration and Crime
# Author(s): Jeyhun Alizade
# Code to replicate Figure C.11

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

# three-level categorical variable of relative restrictiveness of CDU/CSU vs SPD/the Greens

dat$CDU_more_restrict_greens_cat = NA
dat$CDU_more_restrict_greens_cat[dat$deport.CDU=="Severe crimes" & dat$deport.Gruene=="No deportations"]="More restrictive"
dat$CDU_more_restrict_greens_cat[dat$deport.CDU=="Any crime" & dat$deport.Gruene%in%c("Severe crimes", "No deportations")]="More restrictive"
dat$CDU_more_restrict_greens_cat[dat$deport.CDU==dat$deport.Gruene]="Equally restrictive"
dat$CDU_more_restrict_greens_cat[dat$deport.CDU=="Severe crimes" & dat$deport.Gruene=="Any crime"]="Less restrictive"
dat$CDU_more_restrict_greens_cat = factor(dat$CDU_more_restrict_greens_cat, levels=c("Equally restrictive", "Less restrictive", "More restrictive"))

dat$CDU_more_restrict_spd_cat = NA
dat$CDU_more_restrict_spd_cat[dat$deport.CDU=="Severe crimes" & dat$deport.SPD=="No deportations"]="More restrictive"
dat$CDU_more_restrict_spd_cat[dat$deport.CDU=="Any crime" & dat$deport.SPD%in%c("Severe crimes", "No deportations")]="More restrictive"
dat$CDU_more_restrict_spd_cat[dat$deport.CDU==dat$deport.SPD]="Equally restrictive"
dat$CDU_more_restrict_spd_cat[dat$deport.CDU=="Severe crimes" & dat$deport.SPD=="Any crime"]="Less restrictive"
dat$CDU_more_restrict_spd_cat = factor(dat$CDU_more_restrict_spd_cat, levels=c("Equally restrictive", "Less restrictive", "More restrictive"))


# ----------------------------------------------------------------------------
# MODELS
# ----------------------------------------------------------------------------

mod_cdu_morerestrict_green_cat = lm(elected.CDU~CDU_more_restrict_greens_cat, dat[dat$voted_last=="Gruene",])
mod_cdu_morerestrict_green_cat_cluster = coeftest(mod_cdu_morerestrict_green_cat, cluster.vcov(mod_cdu_morerestrict_green_cat, ~ID)) # clustered standard erros (by respondent)

mod_cdu_morerestrict_spd_cat = lm(elected.CDU~CDU_more_restrict_spd_cat, dat[dat$voted_last=="SPD",])
mod_cdu_morerestrict_spd_cat_cluster = coeftest(mod_cdu_morerestrict_spd_cat, cluster.vcov(mod_cdu_morerestrict_spd_cat, ~ID))


# ----------------------------------------------------------------------------
# PLOTTING
# ----------------------------------------------------------------------------

# take out estimates and standard errors and save them in a data frame

mod_cdu_morerestrict_cat_ests = rbind.data.frame(
  mod_cdu_morerestrict_green_cat_cluster[2:3,1:2],
  mod_cdu_morerestrict_spd_cat_cluster[2:3,1:2]
)
names(mod_cdu_morerestrict_cat_ests) = c("coef", "se")

# create variable for party compared to CDU/CSU (and used to subsample voters)

mod_cdu_morerestrict_cat_ests$party=rep(c("Greens", "SPD"), each=2)
mod_cdu_morerestrict_cat_ests$party=factor(mod_cdu_morerestrict_cat_ests$party, levels=c("Greens", "SPD"))

# create variable indicating category of relative restrictiveness

mod_cdu_morerestrict_cat_ests$category=rep(c("Less restrictive", "More restrictive"), 2)
mod_cdu_morerestrict_cat_ests = rbind.data.frame(mod_cdu_morerestrict_cat_ests,
                                                 data.frame(coef=c(0, 0), se=c(0, 0), party=c("Greens", "SPD"), category=rep("Equally restrictive", 2)))
mod_cdu_morerestrict_cat_ests$category=factor(mod_cdu_morerestrict_cat_ests$category, levels=c("Less restrictive", "Equally restrictive", "More restrictive"))

# create Figure C.11

ggplot(data=mod_cdu_morerestrict_cat_ests[mod_cdu_morerestrict_cat_ests$party!="AfD",], aes(x=category, y=coef)) + 
  geom_point(size=2) +
  geom_errorbar(aes(ymin=coef-se*1.645, ymax=coef+se*1.645), width=0, linewidth=1) +
  geom_errorbar(aes(ymin=coef-se*1.96, ymax=coef+se*1.96), width=0, linewidth=.5) +
  coord_flip() +
  geom_hline(yintercept=0, linetype="dashed") +
  ylab("Effect on\nPr(Choosing CDU/CSU Candidate)") + xlab("CDU/CSU position") +
  theme_bw() +
  theme(panel.grid = element_blank(), text=element_text(size=18)) +
  facet_wrap(~party)

# save Figure C.11

ggsave("figures/figure_c11.pdf", width = 7, height = 5)
