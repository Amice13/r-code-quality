dat_gly <- read.csv("validation_proteins_merged.csv", header = T, sep = ",")

#plot


dat_gly <- dat_gly[!is.na(dat_gly$time) & !is.na(dat_gly$group),]

x <- ggplot(dat_gly, aes(x=time, y= log(hyaluronan))) + geom_point() + facet_grid(.~group.num)+
  geom_line(aes(group = subjid, color = as.factor(group.num))) +
  geom_boxplot(width = 0.2, fatten = 1.5, colour = "grey")
x + ylab ("log Hyaluronan (ng/mL)") + theme_classic() + theme(legend.position = "none")

y <- ggplot(dat_gly, aes(x=time, y= log(syndecan))) + geom_point() + facet_grid(.~group.num)+
  geom_line(aes(group = subjid, color = as.factor(group.num))) +
  geom_boxplot(width = 0.2, fatten = 1.5, colour = "grey")
y + ylab ("log Syndecan 1 (ng/mL)") + theme_classic() + theme(legend.position = "none")

y <- ggplot(dat_gly, aes(x=time, y= log(LUMICAN)/albumin)) + geom_point() + facet_grid(.~group.num)+
  geom_line(aes(group = subjid, color = as.factor(group.num))) +
  geom_boxplot(width = 0.2, fatten = 1.5, colour = "grey")
y + ylab ("log Lumican (ng/mL)") + theme_classic() + theme(legend.position = "none")



dat_gly$LA = (dat_gly$LUMICAN*1e-6)/(dat_gly$albumin*10)
dat_gly$SA = (dat_gly$syndecan*1e-6)/(dat_gly$albumin*10)
dat_gly$HA = (dat_gly$hyaluronan*1e-6)/(dat_gly$albumin*10)


y <- ggplot(dat_gly, aes(x=time, y= log(LA))) + geom_point() + facet_grid(.~group.num) +
  geom_line(aes(group = subjid, color = as.factor(group.num))) +
  geom_boxplot(width = 0.2, fatten = 1.5, colour = "grey")
y + ylab ("log Lumican:Albumin (g/g)") + theme_classic() + theme(legend.position = "none")
#400 x 250

dat_gly$MMP2 = (dat_gly$MMP2*1e-6)/(dat_gly$albumin*10)
dat_gly$TIMP1 = (dat_gly$TIMP1*1e-6)/(dat_gly$albumin*10)
dat_gly$TIMP2 = (dat_gly$TIMP2*1e-6)/(dat_gly$albumin*10)
dat_gly$podoplanin = (dat_gly$podoplanin*1e-6)/(dat_gly$albumin*10)
dat_gly$LYVE1 = (dat_gly$LYVE1*1e-6)/(dat_gly$albumin*10)
dat_gly$TNF = (dat_gly$TNF.ALPHA*1e-6)/(dat_gly$albumin*10)
dat_gly$IL6 = (dat_gly$IL.6*1e-6)/(dat_gly$albumin*10)
dat_gly$IL8 = (dat_gly$IL.8*1e-6)/(dat_gly$albumin*10)
dat_gly$IL10 = (dat_gly$IL.10*1e-6)/(dat_gly$albumin*10)
dat_gly$Fas = (dat_gly$Fas.Ligand*1e-6)/(dat_gly$albumin*10)
dat_gly$VEGFR3 = (dat_gly$VEGFR3*1e-6)/(dat_gly$albumin*10)
dat_gly$IL4 = (dat_gly$IL.4*1e-6)/(dat_gly$albumin*10)
dat_gly$GRO = (dat_gly$GRO.alpha.KC*1e-6)/(dat_gly$albumin*10)
dat_gly$sCD14 = (dat_gly$sCD14*1e-6)/(dat_gly$albumin*10)

#plot

y <- ggplot(dat_gly, aes(x=time, y= log(sCD14))) + geom_point() + facet_grid(.~group.num) +
  geom_line(aes(group = subjid, color = as.factor(group.num))) +
  geom_boxplot(width = 0.2, fatten = 1.5, colour = "grey")
y + ylab ("log sCD14:Albumin (g/g)") + theme_classic() + theme(legend.position = "none",  strip.text.x = element_blank())
#350 x 250


#analsye

library(dplyr)

d0 <- dat_gly %>% filter(time == "D0")
library(survival)

#cohort was matched based on age_cat, sex and site

p <- clogit(group.num ~ log(sCD14) + strata(strata), data = d0)
summary(p)
confint(p)

#time series test
library(lme4)

kwash <- dat_gly %>% filter(group == "kwash")
maras <- dat_gly %>% filter(group == "maras")

q = lmer(log(sCD14) ~ (1|subjid) + time, data = maras)
summary(q)

p = lmer(log(sCD14) ~ (1|subjid) + time, data = kwash)
summary(p)


