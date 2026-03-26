# ----------------------------------------------------------------------------
# INFO  
# ----------------------------------------------------------------------------

# Article: The Electoral Politics of Immigration and Crime
# Author(s): Jeyhun Alizade
# Code to replicate Figure 5

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
# MODELS
# ----------------------------------------------------------------------------

# The effect of the deportations attribute on candidate choice, by party of candidate

green_deport_mod = lm(elected.Gruene~deport.Gruene, data=dat) # OLS regression
green_deport_mod_cluster <- coeftest(green_deport_mod, cluster.vcov(green_deport_mod, ~ID)) # clustered standard errors (by respondent)

spd_deport_mod = lm(elected.SPD~deport.SPD, data=dat)
spd_deport_mod_cluster <- coeftest(spd_deport_mod, cluster.vcov(spd_deport_mod, ~ID))

cdu_deport_mod = lm(elected.CDU~deport.CDU, data=dat)
cdu_deport_mod_cluster <- coeftest(cdu_deport_mod, cluster.vcov(cdu_deport_mod, ~ID))

afd_deport_mod = lm(elected.AfD~deport.AfD, data=dat)
afd_deport_mod_cluster <- coeftest(afd_deport_mod, cluster.vcov(afd_deport_mod, ~ID))

# The effect of the deportations attribute on candidate choice, by party of candidate and within the group of voters of the candidate's party

green_deport_own_mod = lm(elected.Gruene~deport.Gruene, data=dat[dat$voted_last=="Gruene",])
green_deport_own_mod_cluster <- coeftest(green_deport_own_mod, cluster.vcov(green_deport_own_mod, ~ID))

spd_deport_own_mod = lm(elected.SPD~deport.SPD, data=dat[dat$voted_last=="SPD",])
spd_deport_own_mod_cluster <- coeftest(spd_deport_own_mod, cluster.vcov(spd_deport_own_mod, ~ID))

cdu_deport_own_mod = lm(elected.CDU~deport.CDU, data=dat[dat$voted_last=="CDU",])
cdu_deport_own_mod_cluster <- coeftest(cdu_deport_own_mod, cluster.vcov(cdu_deport_own_mod, ~ID))

afd_deport_own_mod = lm(elected.AfD~deport.AfD, data=dat[dat$voted_last=="AfD",])
afd_deport_own_mod_cluster <- coeftest(afd_deport_own_mod, cluster.vcov(afd_deport_own_mod, ~ID))


# ----------------------------------------------------------------------------
# PLOTTING
# ----------------------------------------------------------------------------

# take out estimates and standard errors and save them in a data frame

mods_party_ests = rbind.data.frame(green_deport_mod_cluster[2:3,1:2], spd_deport_mod_cluster[2:3,1:2], 
                                   cdu_deport_mod_cluster[2,1:2], afd_deport_mod_cluster[2,1:2],
                                   green_deport_own_mod_cluster[2:3,1:2], spd_deport_own_mod_cluster[2:3,1:2], 
                                   cdu_deport_own_mod_cluster[2,1:2], afd_deport_own_mod_cluster[2,1:2])
names(mods_party_ests) = c("coef", "se")

# create party variable

mods_party_ests$party = rep(rev(c("AfD", "CDU/\nCSU", "SPD", "SPD", "Greens", "Greens")),2)
mods_party_ests$party = factor(mods_party_ests$party, levels=c("Greens", "SPD", "CDU/\nCSU", "AfD"))

# create variable with attribute levels

mods_party_ests$level = rep(c("No deportations", "Severe crimes", "No deportations", "Severe crimes", "Severe crimes", "Severe crimes"), 2)

# append with another data frame that contains zeros for the reference level ("any crime")

mods_party_ests = rbind.data.frame(mods_party_ests,
                                   data.frame(coef=rep(c(0, 0, 0, 0), 2), 
                                              se=rep(c(0, 0, 0, 0), 2),
                                              party=rep(c("AfD", "CDU/\nCSU", "SPD", "Greens"),2),
                                              level=rep("Any crime", 8)))
mods_party_ests$level=factor(mods_party_ests$level, levels=c("No deportations", "Severe crimes", "Any crime"))

# create variable for the underlying sample of voters used in a model

mods_party_ests$sample=c(rep("All voters", 6), rep("Own supporters", 6), rep("All voters", 4), rep("Own supporters", 4))

# create Figure 5

ggplot(data=mods_party_ests, aes(x=level, y=coef)) + 
  geom_point(size=2) +
  geom_errorbar(aes(ymin=coef-se*1.645, ymax=coef+se*1.645), width=0, linewidth=1) +
  geom_errorbar(aes(ymin=coef-se*1.96, ymax=coef+se*1.96), width=0, linewidth=.5) +
  coord_flip() +
  facet_grid(party~sample) + 
  geom_hline(yintercept=0, linetype="dashed") +
  ylab("Effect on Pr(choosing candidate)") + xlab("") +
  theme_bw() +
  theme(text=element_text(size=18),
        panel.grid = element_blank(),
        axis.text = element_text(color="black"),
        axis.title.x = element_text(vjust= -.4, size=15),
        strip.text.y = element_text(angle = 0)) 

# save Figure 5

ggsave("figures/figure_5.pdf", width=7, height=5)


# ----------------------------------------------------------------------------
# NUMBERS REPORTED ON PAGE 23, FIRST PARAGRAPH
# ----------------------------------------------------------------------------

# mean probability of electing a Green candidate who proposes deportations for any crime among Green voters
mean_green_any = mean(dat$elected.Gruene[dat$voted_last=="Gruene" & dat$deport.Gruene=="Any crime"], na.rm = T)*100
print(mean_green_any)

# mean probability of electing a Green candidate who proposes no deportations among Green voters
mean_green_no = mean(dat$elected.Gruene[dat$voted_last=="Gruene" & dat$deport.Gruene=="No deportations"], na.rm = T)*100
print(mean_green_no)

# difference between the two means
print(mean_green_any-mean_green_no)
