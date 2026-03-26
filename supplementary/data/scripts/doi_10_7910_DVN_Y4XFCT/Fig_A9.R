##########################################################################
# Figure A9: Campaign spending effects on religious candidates.
##########################################################################

rm(list=ls())

library(ggplot2)
library(rdrobust)
library(ggthemes)
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


clergy_candidate <- with(electoral, rdrobust(religious_urna_candidate_2016, inverted_margin, all=TRUE))
summary(clergy_candidate)

temp <- data.frame(conventional_coef = clergy_candidate$coef[[1]],
                   robust_coef = clergy_candidate$coef[[3]],
                   conventional_upper = clergy_candidate$ci[[4]],
                   conventional_lower = clergy_candidate$ci[[1]],
                   robust_upper = clergy_candidate$ci[[6]],
                   robust_lower = clergy_candidate$ci[[3]],
                   n = clergy_candidate$N_h[[1]] + clergy_candidate$N_h[[2]],
                   group = 'After Spending Caps',
                   dv = 'Presence\nReligious Candidate')

party_test = rbind(party_test,temp)

clergy_elected <- with(electoral, rdrobust(religious_urna_candidate_won_2016, inverted_margin, all=TRUE))
summary(clergy_elected)

temp <- data.frame(conventional_coef = clergy_elected$coef[[1]],
                   robust_coef = clergy_elected$coef[[3]],
                   conventional_upper = clergy_elected$ci[[4]],
                   conventional_lower = clergy_elected$ci[[1]],
                   robust_upper = clergy_elected$ci[[6]],
                   robust_lower = clergy_elected$ci[[3]],
                   n = clergy_elected$N_h[[1]] + clergy_elected$N_h[[2]],
                   group = 'After Spending Caps',
                   dv = 'Elected\nReligious Candidate')
party_test = rbind(party_test,temp)


party_test$dv <- factor(party_test$dv,levels = c('Presence\nReligious Candidate','Elected\nReligious Candidate'))

# Plot ----
plot.candidates <- ggplot(party_test, aes(x = dv, y = robust_coef)) +
  geom_errorbar(aes(ymax = (robust_upper), ymin = (robust_lower)), width=0,size=.75,position=position_dodge(width=0.5)) +
  geom_point(alpha=.9,size=2.5 ,position=position_dodge(width=0.5)) +
  geom_hline(yintercept=0, linetype="dotted",colour='gray20') +
  theme_minimal(base_size = 12) +
  scale_color_manual(values=c("grey", "black")) +
  theme(legend.position = "top") +
  theme(legend.title=element_blank()) +
  xlab("") +
  ylab('Effect of Stricter Spending Caps,\n Religious Candidates') +
  NULL

# ggsave(here('img','Fig_A9.pdf'), plot = plot.candidates, device = 'pdf',height = 13, width = 13, units = 'cm') 
