# ----------------------------------------------------------------------------
# INFO  
# ----------------------------------------------------------------------------

# Article: The Electoral Politics of Immigration and Crime
# Author(s): Jeyhun Alizade
# Code to replicate Figure 3

# ----------------------------------------------------------------------------
# SETUP
# ----------------------------------------------------------------------------

# clear environment
rm(list=ls())

# install and load necessary packages
# install.packages("ggplot2")
library(ggplot2)

# set working directory (change working directory to where the replication folder is saved on your computer)
setwd("/Users/jeyhunalizade/Dropbox/immigration crime project/ajps_replication/replication_files/")

# load data set
dat = readRDS("data/ess_ches_merged.rds")


# ----------------------------------------------------------------------------
# MODELS
# ----------------------------------------------------------------------------

### regressions of party positions from CHES on voter attitudes based on ESS, standardizing both the dependent and independent variable

## Crime

# without country fixed effects
crime_mod <- lm(scale(civlib_laworder)~scale(immi_crime), dat)
# with country fixed effects
crime_fe_mod <- lm(scale(civlib_laworder)~scale(immi_crime)+cntry, dat)

## Multiculturalism

# without country fixed effects
cult_mod <- lm(scale(multiculturalism)~scale(immi_cult), dat)
# with country fixed effects
cult_fe_mod <- lm(scale(multiculturalism)~scale(immi_cult)+cntry, dat)


# ----------------------------------------------------------------------------
# PLOTTING
# ----------------------------------------------------------------------------

# extract the ceofficients and standard errors for the attitudes from the models and combine them into one data frame
ests_df <- rbind.data.frame(summary(crime_mod)$coefficients[2,1:2], summary(crime_fe_mod)$coefficients[2,1:2], 
                            summary(cult_mod)$coefficients[2,1:2], summary(cult_fe_mod)$coefficients[2,1:2])
names(ests_df) <- c("coef", "se")

# create a variable indicating the threat/policy dimension
ests_df$dim <- c("Crime", "Crime", "Culture", "Culture")

# create a variable indicating the model
ests_df$mod <- c("no country fixed effects", "country fixed effects", "no country fixed effects", "country fixed effects")
ests_df$mod <- factor(ests_df$mod, levels=c("no country fixed effects", "country fixed effects"))

# create Figure 3
ggplot(data=ests_df, aes(x=dim, y=coef, group=mod, shape=mod)) + 
  geom_point(position=position_dodge(width = 0.5), size=2) + 
  geom_errorbar(aes(ymin=coef-1.96*se, ymax=coef+1.96*se), width=0, position=position_dodge(width = 0.5)) +
  theme_bw() +
  theme(text=element_text(size=18), legend.position = "bottom", panel.grid = element_blank(), axis.text = element_text(color="black"), axis.title.y = element_text(margin = margin(r = 10))) +
  xlab("") + ylab("Coefficient")  +
  scale_shape_manual(values=c(16, 17), name="")

# save Figure 3
ggsave("figures/figure_3.pdf", width=7, height=5)
