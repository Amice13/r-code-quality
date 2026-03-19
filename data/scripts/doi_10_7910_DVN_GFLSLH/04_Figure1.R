#### 1. Read in Clean Gibson's data ####
load("data/analysis/gibson_replication/gibson_dat.rda")

## creating svydesign object
des <- svydesign(ids=~1, weight=~WEIGHT, data=gibson_dat)

#### 2. Calculate proportions for each wave ####
m1 <- svymean(~agree1, design=subset(des, WAVE == "July 2020"), na.rm=TRUE)
m2 <- svymean(~agree1, design=subset(des, WAVE == "December 2020"), na.rm=TRUE)
m3 <- svymean(~agree1, design=subset(des, WAVE == "March 2021"), na.rm=TRUE)
m4 <- svymean(~agree1, design=subset(des, WAVE == "July 2022"), na.rm=TRUE)

#### 3. Data for plot ####
## make a data frame that contains the wave, mean, standard error and 95% confidence intervals
mdat <- tibble(
  wave = factor(1:4, labels=c("July 2020", "December 2020", "March 2021", "July 2022")), 
  mean = c(m1[1], m2[1], m3[1], m4[1]), 
  se = sqrt(c(attr(m1, "var"), attr(m2, "var"), attr(m3, "var"), attr(m4, "var"))), 
  lwr = c(confint(m1)[1], confint(m2)[1], confint(m3)[1], confint(m4)[1]),
  upr = c(confint(m1)[2], confint(m2)[2], confint(m3)[2], confint(m4)[2]))

#### 4. Make plot of the confidence intervals by wave ####
ggplot(mdat, aes(x=wave, y=mean, ymin=lwr, ymax=upr)) + 
  geom_pointrange() + 
  theme_classic() + 
  labs(x = "", y="Proportion Agreeing")
ggsave("results/Figure1.pdf", height=6, width=6, units="in", dpi=300)

