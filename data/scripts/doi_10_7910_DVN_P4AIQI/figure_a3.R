# ----------------------------------------------------------------------------
# INFO  
# ----------------------------------------------------------------------------

# Article: The Electoral Politics of Immigration and Crime
# Author(s): Jeyhun Alizade
# Code to replicate Figure A.3

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

## crime

# regress party position on voter attitudes in each country separately and extract the relevant coefficient and standard error
crime_cntry = sapply(levels(factor(dat$cntry)), function(x){
  summary(lm(scale(civlib_laworder)~scale(immi_crime), dat[dat$cntry==x,]))$coefficients[2,1:2]
})

# save the coefficients and standard errors in a data frame
crime_cntry = data.frame(t(crime_cntry))

# create a variable indicating the country
crime_cntry$cntry = row.names(crime_cntry)


## multiculturalism

# regress party position on voter attitudes in each country separately and extract the relevant coefficient and standard error
# exclude Ireland because all Irish parties had the same value on the multiculturalism scale 
multicult_cntry = sapply(levels(factor(dat$cntry[dat$cntry!="IE"])), function(x){
  summary(lm(scale(multiculturalism)~scale(immi_cult), dat[dat$cntry==x,]))$coefficients[2,1:2]
})

# save the coefficients and standard errors in a data frame
multicult_cntry = data.frame(t(multicult_cntry))

# create a variable indicating the country
multicult_cntry$cntry = row.names(multicult_cntry)


# ----------------------------------------------------------------------------
# PLOTTING
# ----------------------------------------------------------------------------

# prepare estimates for plotting
cor_cntry = rbind.data.frame(crime_cntry, multicult_cntry)
cor_cntry$dimension = c(rep("Crime", 12), rep("Culture", 11))
names(cor_cntry)[1:2] = c("coef", "se")

# create Figure A.3
ggplot(data=cor_cntry, aes(x=dimension, y=coef)) + 
  facet_wrap(~cntry, scales="free_y") + 
  geom_point() +
  geom_errorbar(aes(ymin=coef-se*1.96, ymax=coef+se*1.96), width=0) +
  theme_bw() +
  theme(panel.grid = element_blank(), text=element_text(size=18), axis.text.x = element_text(angle =50, vjust=1, hjust=1)) +
  xlab("") + ylab("Coefficient")

# save Figure A.3
ggsave("figures/figure_a3.pdf", width=7, height=5)
