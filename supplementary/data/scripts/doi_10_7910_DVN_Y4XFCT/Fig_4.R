##########################################################################
# Figure 4: Effects of (stricter) campaign spending limits on 
# fielding candidates and electoral victories for selected parties
##########################################################################

rm(list = ls())
library(data.table)
library(rdrobust)
library(here)

electoral <- readRDS(here("data","mayors_municipal_level.rds"))

# Note that for each of these tests, the units of analysis are party candidacies in 2012
# This means that for the Republicanos, the study group are municipalities with a Republicanos
# mayoral candidate in 2012. For 2012 that do not appear in 2016, they are recorded as not
# having switched parties.

prb_switching <- with(electoral, rdrobust(change_prb_cand_16, inverted_margin, all=TRUE))
#summary(prb_switching)
psc_switching <- with(electoral, rdrobust(change_psc_cand_16, inverted_margin, all=TRUE))
#summary(psc_switching)

switching_test <- data.frame(conventional_coef=as.numeric(),
                         robust_coef = as.numeric(),
                         conventional_upper = as.numeric(),
                         conventional_lower = as.numeric(),
                         robust_upper = as.numeric(),
                         robust_lower = as.numeric(),
                         n=as.numeric(),
                         group=as.character(),
                         dv=as.character(),
                         stringsAsFactors=FALSE)

temp <- data.frame(conventional_coef = prb_switching$coef[[1]],
                   robust_coef = prb_switching$coef[[3]],
                   conventional_upper = prb_switching$ci[[4]],
                   conventional_lower = prb_switching$ci[[1]],
                   robust_upper = prb_switching$ci[[6]],
                   robust_lower = prb_switching$ci[[3]],
                   n = prb_switching$N_h[[1]] + prb_switching$N_h[[2]],
                   group = 'Republicanos Candidates',
                   dv = 'Party Switching')
switching_test = rbind(switching_test,temp)

temp <- data.frame(conventional_coef = psc_switching$coef[[1]],
                   robust_coef = psc_switching$coef[[3]],
                   conventional_upper = psc_switching$ci[[4]],
                   conventional_lower = psc_switching$ci[[1]],
                   robust_upper = psc_switching$ci[[6]],
                   robust_lower = psc_switching$ci[[3]],
                   n = psc_switching$N_h[[1]] + psc_switching$N_h[[2]],
                   group = 'PSC candidates',
                   dv = 'Party Switching')

switching_test = rbind(switching_test,temp)

plot.candidates <- ggplot(switching_test, aes(x = group, y = robust_coef, group = group, shape = group, color = group)) +
  geom_errorbar(aes(ymax = (robust_upper), ymin = (robust_lower)), width=0,size=.75,position=position_dodge(width=0.5)) +
  geom_point(alpha=.9,size=2.5 ,position=position_dodge(width=0.5)) +
  geom_hline(yintercept=0, linetype="dotted",colour='gray20') +
  theme_minimal(base_size = 12) +
  scale_color_manual(values=c("grey", "black")) +
  theme(legend.position = "None") +
  theme(legend.title=element_blank()) +
  xlab("") +
  ylab('Effect of Stricter Spending Caps\nProb. of Party Switching') +
  NULL

#ggsave(here('img','Fig_4.pdf'), plot = plot.candidates, device = 'pdf',height = 13, width = 13, units = 'cm') 
