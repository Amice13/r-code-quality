##########################################################################
# Figure A7: Campaign spending after introduction of caps, big money parties and
# other parties
##########################################################################

rm(list=ls())
library(data.table)
library(ggplot2)
library(rdrobust)
library(ggthemes)
library(here)
library(ggplot2)


electoral <- readRDS(here("data","mayors_municipal_level.rds"))

# ANALYSIS

party_test <- data.frame(conventional_coef=as.numeric(),
                           robust_coef = as.numeric(),
                           conventional_upper = as.numeric(),
                           conventional_lower = as.numeric(),
                           robust_upper = as.numeric(),
                           robust_lower = as.numeric(),
                           n=as.numeric(),
                           group=as.character(),
                           dv=as.character(),
                           stringsAsFactors=FALSE)


prb_candidate <- with(electoral, rdrobust(median_spending_muni_big, inverted_margin, all=TRUE))
summary(prb_candidate)

temp <- data.frame(conventional_coef = prb_candidate$coef[[1]],
                   robust_coef = prb_candidate$coef[[3]],
                   conventional_upper = prb_candidate$ci[[4]],
                   conventional_lower = prb_candidate$ci[[1]],
                   robust_upper = prb_candidate$ci[[6]],
                   robust_lower = prb_candidate$ci[[3]],
                   n = prb_candidate$N_h[[1]] + prb_candidate$N_h[[2]],
                   group = 'Campaign expenditures',
                   dv = 'Big money parties')

party_test = rbind(party_test,temp)

prb_elected <- with(electoral, rdrobust(median_spending_muni_small, inverted_margin, all=TRUE))
summary(prb_elected)

temp <- data.frame(conventional_coef = prb_elected$coef[[1]],
                   robust_coef = prb_elected$coef[[3]],
                   conventional_upper = prb_elected$ci[[4]],
                   conventional_lower = prb_elected$ci[[1]],
                   robust_upper = prb_elected$ci[[6]],
                   robust_lower = prb_elected$ci[[3]],
                   n = prb_elected$N_h[[1]] + prb_elected$N_h[[2]],
                   group = 'Campaign expenditures',
                   dv = 'Other parties')
party_test = rbind(party_test,temp)

prb_elected <- with(electoral, rdrobust(median_spending_muni, inverted_margin, all=TRUE))
summary(prb_elected)

temp <- data.frame(conventional_coef = prb_elected$coef[[1]],
                   robust_coef = prb_elected$coef[[3]],
                   conventional_upper = prb_elected$ci[[4]],
                   conventional_lower = prb_elected$ci[[1]],
                   robust_upper = prb_elected$ci[[6]],
                   robust_lower = prb_elected$ci[[3]],
                   n = prb_elected$N_h[[1]] + prb_elected$N_h[[2]],
                   group = 'Campaign expenditures',
                   dv = 'All parties')
party_test = rbind(party_test,temp)

#party_test$group <- factor(party_test$group,levels = c("Before Spending Caps","After Spending Caps"))
#party_test$dv <- factor(party_test$dv,levels = c('Fielded Candidate','Elected Mayor'))

# Plot ----
plot.candidates <- ggplot(party_test, aes(x = dv, y = robust_coef)) +
  geom_errorbar(aes(ymax = (robust_upper), ymin = (robust_lower)), width=0, linewidth=.75,position=position_dodge(width=0.5)) +
  geom_point(alpha=.9,size=2.5 ,position=position_dodge(width=0.5)) +
  geom_hline(yintercept=0, linetype="dotted",colour='gray20') +
  theme_minimal(base_size = 12) +
  #scale_color_manual(values=c("grey", "black")) +
  theme(legend.position = "top") +
  theme(legend.title=element_blank()) +
  xlab("") +
  ylab('Effect of Stricter Spending Caps,\n Median Spending in 2016') +
  NULL

# ggsave(here('img','Fig_A7.pdf'), plot = plot.candidates, device = 'pdf',height = 13, width = 13, units = 'cm') 
