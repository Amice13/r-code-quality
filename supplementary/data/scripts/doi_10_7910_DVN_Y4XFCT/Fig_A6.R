##########################################################################
# Figure A6: Balance tests
##########################################################################

rm(list = ls())
library(data.table)
library(rdrobust)
library(here)
library(ggplot2)
library(ggthemes)

balance_data <- readRDS(here('data','balance_data.rds'))

balance_data[, employed := as.numeric(employed)]
vars <- names(balance_data)
vars <- vars[!vars %in% c('muni_code','ibge','gdp','total_pbf_families','inverted_margin')]

tests.robust <- data.frame(robust_coef = as.numeric(),
                           robust_upper = as.numeric(),
                           robust_lower = as.numeric(),
                           n=as.numeric(),
                           estimation=as.character(),
                           stringsAsFactors=FALSE)

tt <- data.frame(balance_data)

for(v in vars){
  tt[paste0("dv")] <- tt[paste0(v)]
  tt$zdv <- I(tt$dv - mean(tt$dv,na.rm=TRUE)) / sd(tt$dv,na.rm=TRUE) #standardize so it can be all in the same plot
  temp = with(tt,rdrobust(zdv, inverted_margin, all=TRUE))
  temp.df <- data.frame(robust_coef = temp$coef[[3]],
                        robust_upper = temp$ci[[6]],
                        robust_lower = temp$ci[[3]],
                        n = temp$N_h[[1]] + temp$N_h[[2]],
                        estimation = v)
  tests.robust = rbind(tests.robust,temp.df)
}

tests.robust$covs <- c("Leftwing candidates",
                       "Candidates",
                       "Women candidates",
                       "Employment rate",
                       "Proportion evangelicals",
                       "Population",
                       "Literacy",
                       "Inequality (Gini)",
                       "Nonwhite population",
                       "Gdp per capita",
                       "Bolsa-Familia",
                       "UCKG branches",
                       "AG branches",
                       "Republicans are incumbents",
                       "Republicanos have fielded cand.",
                       "Republicans in coalition for mayor")

plot.robust.individual <- ggplot(tests.robust, aes(x = covs, y = robust_coef)) +
  geom_errorbar(aes(ymax = (robust_upper), ymin = (robust_lower)), width=0,size=.75,position=position_dodge(width=0.9)) +
  geom_point(alpha=.9,size=2.5) +
  geom_hline(yintercept=0, linetype="dotted",colour='gray20') +
  theme_tufte(base_size = 12) +
  scale_colour_grey() +
  theme(legend.position = "none") +
  #theme(legend.title=element_blank()) +
  ylab("Standard deviations") +
  xlab('') +
  coord_flip() +
  NULL

# ggsave(here('img','Fig_A6.pdf'), plot = plot.robust.individual, device = 'pdf',height = 13, width = 13, units = 'cm') 
