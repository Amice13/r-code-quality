#Please restart R
options(scipen=999)

rm(list = ls())
library(data.table)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(rdrobust)
library(here)

load(here("data","ver_data.RData"))

# Create/modify variables
  ver_b[,leftwing:=ifelse(party %in% c('PT','PDT','PSB','PSTU','PCO','PCdoB','PV'),1,0)]
  ver_b[,log_donations := log(1+Donations)]

tt <- ver_b[year %in% c(2004,2008,2012) & abs(marginal)!=0 & law_enforcement == 1]
bw_rd = 0.01
tt <- tt[abs(marginal) < bw_rd]

vars <- c("dv_nonwhite_men_lagged","dv_finbra_lagged",'female','leftwing','age',"police_force_occ","not_police","gini","sc_before","lag_pop","gdp_pc")

tests.robust <- data.frame(robust_coef = as.numeric(),
                           robust_upper = as.numeric(),
                           robust_lower = as.numeric(),
                           n=as.numeric(),
                           estimation=as.character(),
                           stringsAsFactors=FALSE)

tt <- data.frame(tt)
for(v in vars){
  tt[paste0("dv")] <- tt[paste0(v)]
  tt$zdv <- I(tt$dv - mean(tt$dv,na.rm=TRUE)) / sd(tt$dv,na.rm=TRUE) #standardize so it can be all in the same plot
  temp = with(tt,rdrobust(zdv, marginal, all=TRUE, bwselect = 'msetwo',
                          covs = cbind(tt$y2008, tt$y2012)))
  temp.df <- data.frame(robust_coef = temp$coef[[3]],
                        robust_upper = temp$ci[[6]],
                        robust_lower = temp$ci[[3]],
                        n = temp$N_h[[1]] + temp$N_h[[2]],
                        estimation = v)
  tests.robust = rbind(tests.robust,temp.df)
}

vars_b <- c("Donors","log_donations","Brokers")

for(v in vars_b){
  tt[paste0("dv")] <- tt[paste0(v)]
  tt$zdv <- I(tt$dv - mean(tt$dv,na.rm=TRUE)) / sd(tt$dv,na.rm=TRUE) #standardize so it can be all in the same plot
  temp = with(tt,rdrobust(zdv, marginal, all=TRUE, bwselect = 'msetwo'))
  temp.df <- data.frame(robust_coef = temp$coef[[3]],
                        robust_upper = temp$ci[[6]],
                        robust_lower = temp$ci[[3]],
                        n = temp$N_h[[1]] + temp$N_h[[2]],
                        estimation = v)
  tests.robust = rbind(tests.robust,temp.df)
}

tests.robust$covs <- c("Lagged difference in homicides, non-white men",
                       'Lagged difference in spending',
                       'Female',
                       'Left-wing candidate',
                       'Age',
                       'Police officer candidate',
                       'Not police officer candidate',
                       'Gini',
                       'Preexisting sec. committee',
                       #'Preexisting municipal guard',
                       'Population',
                       'GDP per capita',
                       'Donors',
                       'Donations (in log)',
                       "Brokers")

plot.robust.individual <- ggplot(tests.robust, aes(x = covs, y = robust_coef)) +
  geom_errorbar(aes(ymax = (robust_upper), ymin = (robust_lower)), width=0,size=.75,position=position_dodge(width=0.9)) +
  geom_point(alpha=.9,size=2.5) +
  geom_hline(yintercept=0, linetype="dotted",colour='gray20') +
  theme_tufte(base_size = 12) +
  scale_colour_grey() +
  theme(legend.position = "none") +
  ylab("Standard deviations") +
  xlab('') +
  coord_flip() +
  NULL

#ggsave(here("writing","img","fig_A3.pdf"), plot = plot.robust.individual, device = 'pdf')
