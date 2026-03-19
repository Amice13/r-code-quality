##########################################################################
# Figure A10: Campaign spending effects on the PSC, conditional on local 
#  centralization and competition among Assembly of God Churches
##########################################################################

rm(list = ls())

library(data.table)
library(rdrobust)
library(ggplot2)
library(here)

electoral <- readRDS(here("data","mayors_municipal_level.rds"))

electoral[, treat := ifelse(inverted_margin > 0, 1, 0)]

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

psc_above <- with(electoral[HHI_ag > median(electoral$HHI_ag, na.rm = TRUE)], rdrobust(psc_candidate_2016, inverted_margin, all=TRUE))
summary(psc_above)


temp <- data.frame(conventional_coef = psc_above$coef[[1]],
                   robust_coef = psc_above$coef[[3]],
                   conventional_upper = psc_above$ci[[4]],
                   conventional_lower = psc_above$ci[[1]],
                   robust_upper = psc_above$ci[[6]],
                   robust_lower = psc_above$ci[[3]],
                   n = psc_above$N_h[[1]] + psc_above$N_h[[2]],
                   group = 'Centralization',
                   dv = 'Centralized\nAG Churches')
party_test = rbind(party_test,temp)

psc_below <- with(electoral[HHI_ag < median(electoral$HHI_ag, na.rm = TRUE)], rdrobust(psc_candidate_2016, inverted_margin, all=TRUE))
summary(psc_below)

temp <- data.frame(conventional_coef = psc_below$coef[[1]],
                   robust_coef = psc_below$coef[[3]],
                   conventional_upper = psc_below$ci[[4]],
                   conventional_lower = psc_below$ci[[1]],
                   robust_upper = psc_below$ci[[6]],
                   robust_lower = psc_below$ci[[3]],
                   n = psc_below$N_h[[1]] + psc_below$N_h[[2]],
                   group = 'Centralization',
                   dv = 'Decentralized\nAG Churches')
party_test = rbind(party_test,temp)


# Competition

psc_monopolistic <- with(electoral[assembleia_2014  == 1], rdrobust(psc_candidate_2016, inverted_margin, all=TRUE))
summary(psc_monopolistic)


temp <- data.frame(conventional_coef = psc_monopolistic$coef[[1]],
                   robust_coef = psc_monopolistic$coef[[3]],
                   conventional_upper = psc_monopolistic$ci[[4]],
                   conventional_lower = psc_monopolistic$ci[[1]],
                   robust_upper = psc_monopolistic$ci[[6]],
                   robust_lower = psc_monopolistic$ci[[3]],
                   n = psc_monopolistic$N_h[[1]] + psc_monopolistic$N_h[[2]],
                   group = 'Competition',
                   dv = 'Single\nAG Church')
party_test = rbind(party_test,temp)

psc_competition <- with(electoral[assembleia_2014 > 1], rdrobust(psc_candidate_2016, inverted_margin, all=TRUE))
summary(psc_competition)

temp <- data.frame(conventional_coef = psc_competition$coef[[1]],
                   robust_coef = psc_competition$coef[[3]],
                   conventional_upper = psc_competition$ci[[4]],
                   conventional_lower = psc_competition$ci[[1]],
                   robust_upper = psc_competition$ci[[6]],
                   robust_lower = psc_competition$ci[[3]],
                   n = psc_competition$N_h[[1]] + psc_competition$N_h[[2]],
                   group = 'Competition',
                   dv = 'Two or more\nAG Churches')
party_test = rbind(party_test,temp)

#party_test$group <- factor(party_test$group,levels = c("Centralization","Coordination"))
party_test$dv <- factor(party_test$dv,levels = c("Decentralized\nAG Churches","Centralized\nAG Churches",'Two or more\nAG Churches','Single\nAG Church'))

# Plot ----
plot.candidates <- ggplot(party_test, aes(x = dv, y = robust_coef)) +
  geom_errorbar(aes(ymax = (robust_upper), ymin = (robust_lower)), width=0,size=.75,position=position_dodge(width=0.5)) +
  geom_point(alpha=.9,size=2.5 ,position=position_dodge(width=0.5)) +
  geom_hline(yintercept=0, linetype="dotted",colour='gray20') +
  theme_minimal(base_size = 12) +
  scale_color_manual(values=c("grey", "black")) +
  theme(legend.position = "top") +
  facet_wrap(~group, scales = "free", ncol = 1) +
  theme(legend.title=element_blank()) +
  xlab("") +
  ylab('Effect of Stricter Spending Caps,\n PSC candidates') +
  NULL

# ggsave(here('img','Fig_A10_left'), plot = plot.candidates, device = 'pdf',height = 13, width = 13, units = 'cm') 


# Election of candidates (right panel) ----

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

psc_above <- with(electoral[HHI_ag > median(electoral$HHI_ag, na.rm = TRUE)], rdrobust(psc_elected_2016, inverted_margin, all=TRUE))
summary(psc_above)


temp <- data.frame(conventional_coef = psc_above$coef[[1]],
                   robust_coef = psc_above$coef[[3]],
                   conventional_upper = psc_above$ci[[4]],
                   conventional_lower = psc_above$ci[[1]],
                   robust_upper = psc_above$ci[[6]],
                   robust_lower = psc_above$ci[[3]],
                   n = psc_above$N_h[[1]] + psc_above$N_h[[2]],
                   group = 'Centralization',
                   dv = 'Centralized\nAG Churches')
party_test = rbind(party_test,temp)

psc_below <- with(electoral[HHI_ag < median(electoral$HHI_ag, na.rm = TRUE)], rdrobust(psc_elected_2016, inverted_margin, all=TRUE))
summary(psc_below)

temp <- data.frame(conventional_coef = psc_below$coef[[1]],
                   robust_coef = psc_below$coef[[3]],
                   conventional_upper = psc_below$ci[[4]],
                   conventional_lower = psc_below$ci[[1]],
                   robust_upper = psc_below$ci[[6]],
                   robust_lower = psc_below$ci[[3]],
                   n = psc_below$N_h[[1]] + psc_below$N_h[[2]],
                   group = 'Centralization',
                   dv = 'Decentralized\nAG Churches')
party_test = rbind(party_test,temp)


# Competition

psc_monopolistic <- with(electoral[assembleia_2014  == 1], rdrobust(psc_elected_2016, inverted_margin, all=TRUE))
summary(psc_monopolistic)


temp <- data.frame(conventional_coef = psc_monopolistic$coef[[1]],
                   robust_coef = psc_monopolistic$coef[[3]],
                   conventional_upper = psc_monopolistic$ci[[4]],
                   conventional_lower = psc_monopolistic$ci[[1]],
                   robust_upper = psc_monopolistic$ci[[6]],
                   robust_lower = psc_monopolistic$ci[[3]],
                   n = psc_monopolistic$N_h[[1]] + psc_monopolistic$N_h[[2]],
                   group = 'Competition',
                   dv = 'Single\nAG Church')
party_test = rbind(party_test,temp)

psc_competition <- with(electoral[assembleia_2014 > 1], rdrobust(psc_elected_2016, inverted_margin, all=TRUE))
summary(psc_competition)

temp <- data.frame(conventional_coef = psc_competition$coef[[1]],
                   robust_coef = psc_competition$coef[[3]],
                   conventional_upper = psc_competition$ci[[4]],
                   conventional_lower = psc_competition$ci[[1]],
                   robust_upper = psc_competition$ci[[6]],
                   robust_lower = psc_competition$ci[[3]],
                   n = psc_competition$N_h[[1]] + psc_competition$N_h[[2]],
                   group = 'Competition',
                   dv = 'Two or more\nAG Churches')
party_test = rbind(party_test,temp)

#party_test$group <- factor(party_test$group,levels = c("Centralization","Coordination"))
party_test$dv <- factor(party_test$dv,levels = c("Decentralized\nAG Churches","Centralized\nAG Churches",'Two or more\nAG Churches','Single\nAG Church'))

# Plot ----
plot.candidates_a <- ggplot(party_test, aes(x = dv, y = robust_coef)) +
  geom_errorbar(aes(ymax = (robust_upper), ymin = (robust_lower)), width=0,size=.75,position=position_dodge(width=0.5)) +
  geom_point(alpha=.9,size=2.5 ,position=position_dodge(width=0.5)) +
  geom_hline(yintercept=0, linetype="dotted",colour='gray20') +
  theme_minimal(base_size = 12) +
  scale_color_manual(values=c("grey", "black")) +
  theme(legend.position = "top") +
  facet_wrap(~group, scales = "free", ncol = 1) +
  theme(legend.title=element_blank()) +
  xlab("") +
  ylab('Effect of Stricter Spending Caps,\n PSC candidates') +
  NULL

# ggsave(here('img','Fig_A10_right.pdf'), plot = plot.candidates_a, device = 'pdf',height = 13, width = 13, units = 'cm') 