# ----------------------------------------------------------------------------
# INFO  
# ----------------------------------------------------------------------------

# Article: The Electoral Politics of Immigration and Crime
# Author(s): Jeyhun Alizade
# Code to replicate Figure 6

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

mod_cdu_morerestrict_green = lm(elected.CDU~CDU_more_restrict_greens, data=dat[dat$voted_last=="Gruene",]) # OLS regression
mod_cdu_morerestrict_green_cluster = coeftest(mod_cdu_morerestrict_green, cluster.vcov(mod_cdu_morerestrict_green, ~ID)) # clustered standard errors (by respondent)

mod_cdu_morerestrict_spd = lm(elected.CDU~CDU_more_restrict_spd, data=dat[dat$voted_last=="SPD",])
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

# create Figure 6

ggplot(data=mod_cdu_morerestrict_ests, aes(x=voted_for, y=coef)) + 
  geom_point(size=2) +
  geom_errorbar(aes(ymin=coef-se*1.645, ymax=coef+se*1.645), width=0, linewidth=1) +
  geom_errorbar(aes(ymin=coef-se*1.96, ymax=coef+se*1.96), width=0, linewidth=.5) +
  coord_flip() +
  geom_hline(yintercept=0, linetype="dashed") +
  ylab("Effect on Pr(choosing CDU/CSU candidate)") + xlab("") +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.text = element_text(color="black"),
        axis.title.x = element_text(margin = margin(t = 10), size=17),
        axis.title.y = element_text(margin = margin(r = 10))) +
  theme(text=element_text(size=18)) +
  scale_y_continuous(limits = c(0, .15))

# save Figure 6

ggsave("figures/figure_6.pdf", width=7, height=5)


# ----------------------------------------------------------------------------
# NUMBERS REPORTED ON PAGE 23, LAST PARAGRAPH
# ----------------------------------------------------------------------------

# effect of CDU/CSU candidate being more restrictive than Green candidate on vote choice for CDU/CSU candidate among Green voters
print(mod_cdu_morerestrict_ests[mod_cdu_morerestrict_ests$voted_for=="Green voter", "coef"]*100)

# effect of CDU/CSU candidate being more restrictive than Green candidate on vote choice for CDU/CSU candidate among SPD voters
print(mod_cdu_morerestrict_ests[mod_cdu_morerestrict_ests$voted_for=="SPD voter", "coef"]*100)


# ----------------------------------------------------------------------------
# NUMBERS REPORTED ON PAGE 23, FOOTNOTE 24
# ----------------------------------------------------------------------------

# Create variable for SPD being more restrictive than the Greens

dat$SPD_more_restrict_greens = ifelse(dat$deport.Gruene=="No deportations" & 
                                        (dat$deport.SPD=="Severe crimes" | dat$deport.SPD=="Any crime"), 1, 0)
dat$SPD_more_restrict_greens = ifelse(dat$deport.Gruene=="Severe crimes" & dat$deport.SPD=="Any crime", 1, dat$SPD_more_restrict_greens)

# Estimate effect of SPD candidate being more restrictive than Green candidate on SPD candidate election probability among Green voters

mod_spd_morerestrict_green = lm(elected.SPD~SPD_more_restrict_greens, data=dat[dat$voted_last=="Gruene",]) # OLS regression
mod_spd_morerestrict_green_cluster = coeftest(mod_spd_morerestrict_green, cluster.vcov(mod_spd_morerestrict_green, ~ID)) # clustered standard errors (by respondent)

# Print the effect of SPD candidate being more restrictive than Green candidate, with p-value

print(mod_spd_morerestrict_green_cluster["SPD_more_restrict_greens",c("Estimate", "Pr(>|t|)")])
