# ----------------------------------------------------------------------------
# INFO  
# ----------------------------------------------------------------------------

# Article: The Electoral Politics of Immigration and Crime
# Author(s): Jeyhun Alizade
# Code to replicate Figure 2

# ----------------------------------------------------------------------------
# SETUP
# ----------------------------------------------------------------------------

# clear environment
rm(list=ls())

# install and load necessary packages
# install.packages("ggplot2")
library(ggplot2)
library(boot)

# set working directory (change working directory to where the replication folder is saved on your computer)
setwd("/Users/jeyhunalizade/Dropbox/immigration crime project/ajps_replication/replication_files/")

# load data set
dat = readRDS("data/ess_ches_merged.rds")


# ----------------------------------------------------------------------------
# FIGURE 2(a)
# ----------------------------------------------------------------------------

# aggregate party positions on the party level using the mean
dat_agg <- aggregate(cbind(civlib_laworder, multiculturalism)~cntry+family+party_vote+party_id, data=dat, FUN=mean)

# keep relevant party families and adjust their names for plotting
dat_agg <- dat_agg[dat_agg$family%in%c("rad right", "cons", "liberal", "christdem", "socialist", "green", "rad left"),]
dat_agg$party_fam[dat_agg$family=="rad left"] <- "Radical-left"
dat_agg$party_fam[dat_agg$family=="green"] <- "Greens"
dat_agg$party_fam[dat_agg$family=="socialist"] <- "Center-left"
dat_agg$party_fam[dat_agg$family=="liberal"] <- "Liberal"
dat_agg$party_fam[dat_agg$family=="cons" | dat_agg$family=="christdem"] <- "Center-right"
dat_agg$party_fam[dat_agg$family=="rad right"] <- "Radical-right"
dat_agg$party_fam <- factor(dat_agg$party_fam, levels=c("Radical-left", "Greens", "Center-left", "Liberal", "Center-right", "Radical-right"))

# further aggregate the data frame on the party family level
dat_agg_fam <- reshape2::melt(aggregate(cbind(civlib_laworder, multiculturalism)~party_fam, dat_agg, mean))
dat_agg_fam$variable <- rep(c("Civil liberties-\nlaw and order", "Multiculturalism-\nassimilation"), each=6)
dat_agg_fam$variable <- factor(dat_agg_fam$variable, levels=c("Civil liberties-\nlaw and order", "Multiculturalism-\nassimilation"))

# create Figure 2(a)
ggplot(data=dat_agg_fam, aes(x=party_fam, y=value, fill=party_fam)) + geom_bar(stat="identity") + facet_grid(~variable) +
  theme_bw() + ylab("Mean position") + xlab("") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text=element_text(size=20),
        legend.position = "bottom",
        axis.text = element_text(color="black"),
        panel.grid = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10), angle=90, hjust=0.65)) +
  scale_fill_manual(values=c("purple", "green", "red", "gold", "black", "blue"), name="")

# save Figure 2(a)
ggsave("figures/figure_2a.pdf", width=7, height=5)


# ----------------------------------------------------------------------------
# FIGURE 2(b)
# ----------------------------------------------------------------------------

# define ideological blocs
dat_agg$ideo_bloc <- ifelse(dat_agg$party_fam%in%c("Radical-left", "Greens", "Center-left"), "Left", ifelse(dat_agg$party_fam%in%c("Liberal", "Center-right", "Radical-right"), "Right", NA))

# aggregate party positions on the ideological bloc level
dat_agg_bloc <- aggregate(cbind(civlib_laworder, multiculturalism)~ideo_bloc, dat_agg, mean, na.rm=T)

# calculate the ratio of means between ideological blocs
bloc_ratios <- data.frame(t(dat_agg_bloc[2,2:3]/dat_agg_bloc[1,2:3]))
names(bloc_ratios) <- "ratio"
bloc_ratios$dimension <- c("Civil liberties-\nlaw and order", "Multiculturalism-\nassimilation")

# set the seed to ensure reproducibility of the bootstrapping
set.seed(1904)


## bootstrap for law & order variable ##

# define bootstrap function for the ratio in means between ideological blocs
ratio_crime <- function(dat_agg, i) {
  dat_agg <- dat_agg[i,]
  mean(dat_agg$civlib_laworder[dat_agg$ideo_bloc=="Right"], na.rm = T)/mean(dat_agg$civlib_laworder[dat_agg$ideo_bloc=="Left"], na.rm = T)
}

# run the function 1,000 times
boot_ratio_crime <- boot(dat_agg, statistic=ratio_crime, R=1000)

# get the lower and upper limits of the confidence interval of the bootstrapped ratio
ratio_ci_crime <- boot.ci(boot_ratio_crime, conf = 0.95, type="perc")$percent[4:5]


## bootstrap for multiculturalism variable ##

# define bootstrap function for the ratio in means between ideological blocs
ratio_cult <- function(dat_agg, i) {
  dat_agg <- dat_agg[i,]
  mean(dat_agg$multiculturalism[dat_agg$ideo_bloc=="Right"], na.rm = T)/mean(dat_agg$multiculturalism[dat_agg$ideo_bloc=="Left"], na.rm = T)
}

# run the function 1,000 times
boot_ratio_cult <- boot(dat_agg, statistic=ratio_cult, R=1000)

# get the lower and upper limits of the confidence interval of the bootstrapped ratio
ratio_ci_cult <- boot.ci(boot_ratio_cult, conf = 0.95, type="perc")$percent[4:5]


# save the upper and lower limits in one variable each
bloc_ratios$ci_low <- c(ratio_ci_crime[1], ratio_ci_cult[1])
bloc_ratios$ci_high <- c(ratio_ci_crime[2], ratio_ci_cult[2])

# calculate the difference between the empirical ratios and the upper/lower limits (for plotting)
bloc_ratios$diff <- bloc_ratios$ratio - bloc_ratios$ci_low
bloc_ratios$diff2 <- bloc_ratios$ci_high - bloc_ratios$ratio

# create Figure 2(b)
ggplot(data=bloc_ratios, aes(x=dimension, y=ratio)) +
  geom_point(size=4) +
  theme_bw() +
  theme(text=element_text(size=20), panel.grid = element_blank(), axis.text = element_text(color="black"),
        axis.title.y = element_text(margin = margin(r = 10))) +
  xlab("") + ylab("Right-left ratio") +
  geom_errorbar(aes(ymin=ratio-diff, ymax=ratio+diff2), width=0, linewidth=1) +
  scale_y_continuous(limits=c(1, 2.8))

# save Figure 2(b)
ggsave("figures/figure_2b.pdf", width=7, height=5)

