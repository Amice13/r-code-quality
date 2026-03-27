# ----------------------------------------------------------------------------
# INFO  
# ----------------------------------------------------------------------------

# Article: The Electoral Politics of Immigration and Crime
# Author(s): Jeyhun Alizade
# Code to replicate Figure 1

# ----------------------------------------------------------------------------
# SETUP
# ----------------------------------------------------------------------------

# clear environment
rm(list=ls())

# install and load necessary packages
# install.packages("ggplot2", "boot")
library(ggplot2)
library(boot)

# set working directory (change working directory to where the replication folder is saved on your computer)
setwd("/Users/jeyhunalizade/Dropbox/immigration crime project/ajps_replication/replication_files/")

# load data set
dat = readRDS("data/ess.rds")



# ----------------------------------------------------------------------------
# Figure 1 (a)
# ----------------------------------------------------------------------------

# create an aggregated data frame with the mean level of concern by party family voted for
party_means <- aggregate(cbind(immi_crime, immi_cult, immi_econ)~party_fam, data=dat, mean)

# transform data frame from wide to long
party_means <- reshape2::melt(party_means)

# create variable indicating the threat dimension examined
party_means$variable <- as.character(party_means$variable)
party_means$variable[party_means$variable=="immi_crime"] <- "Crime" 
party_means$variable[party_means$variable=="immi_cult"] <- "Culture" 
party_means$variable[party_means$variable=="immi_econ"] <- "Economy" 

# create Figure 1(a)
ggplot(data=party_means[party_means$party_fam%in%c("Radical-left", "Green", "Center-left", "Liberal", "Center-right", "Radical-right"),],
  aes(x=party_fam, y=value, fill=party_fam)) + 
  geom_bar(stat="identity") + 
  facet_wrap(~variable) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        text=element_text(size=20),
        legend.position = "bottom",
        axis.text = element_text(color="black"),
        panel.grid = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10))) +
  ylab("Mean concern") + xlab("") +
  scale_fill_manual(values=c("purple", "green", "red", "gold", "black", "blue"), name="")

# save Figure 1(a)
ggsave("figures/figure_1a.pdf", width=7, height=5.5)


# ----------------------------------------------------------------------------
# Figure 1 (b)
# ----------------------------------------------------------------------------

# create a variable for the ideological bloc of the party family a respondent voted for
dat$ideo_bloc <- ifelse(dat$party_fam%in%c("Radical-left", "Green", "Center-left"), "Left", ifelse(dat$party_fam%in%c("Liberal", "Center-right", "Radical-right"), "Right", NA))

# create an aggregated data frame with the mean level of concern by ideological bloc
bloc_means <- aggregate(cbind(immi_crime, immi_cult, immi_econ)~ideo_bloc, dat, mean, na.rm=T)

# get the ratio of means by threat dimension and turn it into a data frame
bloc_ratios <- data.frame(t(bloc_means[2,2:4]/bloc_means[1,2:4]))
names(bloc_ratios) <- "ratio"
bloc_ratios$dim <- c("Crime", "Culture", "Economy")

# set the seed so that the bootstrapping is reproducible
set.seed(1904)


## bootstrap for immigration & crime variable ##

# define bootstrap function for the ratio in means between ideological blocs
ratio_crime <- function(dat, i) {
  dat <- dat[i,]
  mean(dat$immi_crime[dat$ideo_bloc=="Right"], na.rm = T)/mean(dat$immi_crime[dat$ideo_bloc=="Left"], na.rm = T)
}

# run the function 1,000 times
boot_ratio_crime <- boot(data=dat, statistic=ratio_crime, R=1000)

# get the lower and upper limits of the confidence interval of the bootstrapped ratio
ratio_ci_crime <- boot.ci(boot_ratio_crime, conf = 0.95, type="perc")$percent[4:5]


## bootstrap for immigration & crime variable ##

# define bootstrap function for the ratio in means between ideological blocs
ratio_cult <- function(dat, i) {
  dat <- dat[i,]
  mean(dat$immi_cult[dat$ideo_bloc=="Right"], na.rm = T)/mean(dat$immi_cult[dat$ideo_bloc=="Left"], na.rm = T)
}

# run the function 1,000 times
boot_ratio_cult <- boot(data=dat, statistic=ratio_cult, R=1000)

# get the lower and upper limits of the confidence interval of the bootstrapped ratio
ratio_ci_cult <- boot.ci(boot_ratio_cult, conf = 0.95, type="perc")$percent[4:5]


## bootstrap for immigration & economy variable ##

# define bootstrap function for the ratio in means between ideological blocs
ratio_econ <- function(dat, i) {
  dat <- dat[i,]
  mean(dat$immi_econ[dat$ideo_bloc=="Right"], na.rm = T)/mean(dat$immi_econ[dat$ideo_bloc=="Left"], na.rm = T)
}

# run the function 1,000 times
boot_ratio_econ <- boot(data=dat, statistic=ratio_econ, R=1000)

# get the lower and upper limits of the confidence interval of the bootstrapped ratio
ratio_ci_econ <- boot.ci(boot_ratio_econ, conf = 0.95, type="perc")$percent[4:5]


# save the upper and lower limits in one variable each
bloc_ratios$ci_low <- c(ratio_ci_crime[1], ratio_ci_cult[1], ratio_ci_econ[1])
bloc_ratios$ci_high <- c(ratio_ci_crime[2], ratio_ci_cult[2], ratio_ci_econ[2])

# calculate the difference between the empirical ratios and the upper/lower limits (for plotting)
bloc_ratios$diff <- bloc_ratios$ratio - bloc_ratios$ci_low
bloc_ratios$diff2 <- bloc_ratios$ci_high - bloc_ratios$ratio

# create Figure 1(b)
ggplot(data=bloc_ratios, aes(x=dim, y=ratio)) +
  geom_point(size=4) +
  theme_bw() +
  theme(text=element_text(size=20), panel.grid = element_blank(), axis.text = element_text(color = "black"),
        axis.title.x = element_text(margin = margin(t = 10)),  # Adjust top margin of x-axis title
        axis.title.y = element_text(margin = margin(r = 10))) +
  xlab("") + ylab("Right-left ratio") +
  geom_errorbar(aes(ymin=ratio-diff, ymax=ratio+diff2), width=0, linewidth=1)

# save Figure 1(b)
ggsave("figures/figure_1b.pdf", width=7, height=5)


# ----------------------------------------------------------------------------
# NUMBERS REPORTED ON PAGE 13
# ----------------------------------------------------------------------------

# Percent difference between center-right and Green voters in immigration threat on culture
print(abs(1-party_means$value[party_means$variable=="Culture" & party_means$party_fam=="Center-right"] / 
                  party_means$value[party_means$variable=="Culture" & party_means$party_fam=="Green"])*100)


# Percent difference between center-right and Green voters in immigration threat on economy
print(abs(1-party_means$value[party_means$variable=="Economy" & party_means$party_fam=="Center-right"] / 
                  party_means$value[party_means$variable=="Economy" & party_means$party_fam=="Green"])*100)

# Percent difference between center-right and Green voters in immigration threat on economy
print(abs(1-party_means$value[party_means$variable=="Crime" & party_means$party_fam=="Center-right"] / 
                  party_means$value[party_means$variable=="Crime" & party_means$party_fam=="Green"])*100)
