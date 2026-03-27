# ----------------------------------------------------------------------------
# INFO  
# ----------------------------------------------------------------------------

# Article: The Electoral Politics of Immigration and Crime
# Author(s): Jeyhun Alizade
# Code to replicate Figure C.1

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
dat = read.csv("data/conjoint_long.csv", sep=",")

# change reference level for cause of immigrant crime
dat$cause = factor(dat$cause, levels=c("Poverty", "Culture"))


# ----------------------------------------------------------------------------
# MODEL
# ----------------------------------------------------------------------------

# The overall effect of candidate attributes on candidate choice

mod_overall = lm(elected~party+deport+cause+adopt, data=dat)
mod_overall_cluster <- coeftest(mod_overall, cluster.vcov(mod_overall, ~ID)) # clustered standard errors (by respondent)

# ----------------------------------------------------------------------------
# DATA PREPARATION
# ----------------------------------------------------------------------------

# take out estimates and standard errors and save them in a data frame

mod_overall_ests = data.frame(mod_overall_cluster[5:8,1:2])
names(mod_overall_ests) = c("coef", "se")

# create attribute level variable

mod_overall_ests$level = c("No deportations", "For severe crimes", "Culture", "Rejects")
mod_overall_ests = rbind.data.frame(mod_overall_ests,
                                    data.frame(coef=c(0, 0, 0, NA, NA, NA), se=c(0, 0, 0, NA, NA, NA), 
                                               level=c("For any crime", "Poverty and social inequality", "Approves", "Deportation", "Cause of immigrant crime", "Adoption rights")))
mod_overall_ests$level=factor(mod_overall_ests$level, levels=c("Rejects", "Approves", "Adoption rights", "Culture", "Poverty and social inequality", "Cause of immigrant crime", "No deportations", "For severe crimes", "For any crime", "Deportation"))


# ----------------------------------------------------------------------------
# PLOTTING
# ----------------------------------------------------------------------------

# create Figure C.1

ggplot(data=mod_overall_ests, aes(x=level, y=coef)) + 
  geom_point(size=2) +
  geom_errorbar(aes(ymin=coef-se*1.645, ymax=coef+se*1.645), width=0, linewidth=1) +
  geom_errorbar(aes(ymin=coef-se*1.96, ymax=coef+se*1.96), width=0, linewidth=.5) +
  coord_flip() +
  geom_hline(yintercept=0, linetype="dashed") +
  ylab("Effect on Pr(Choosing Candidate)") + xlab("") +
  theme_bw() +
  theme(axis.text.y = element_text(face=c("plain", "plain", "bold", "plain", "plain", "bold", "plain", "plain", "plain", "bold", "plain", "plain", "plain", "plain", "bold")),
        text=element_text(size=16))

# save Figure C.1
ggsave("figures/figure_c1.pdf", width=7, height=5)


# ----------------------------------------------------------------------------
# NUMBERS REPORTED ON PAGE 24, FOOTNOTE 26
# ----------------------------------------------------------------------------

# Treatment effect for adoption rights attribute
print(abs(mod_overall_ests$coef[mod_overall_ests$level=="Rejects"])*100)

# Treatment effect for cause of immigrant crime attribute
print(abs(mod_overall_ests$coef[mod_overall_ests$level=="Culture"])*100)
