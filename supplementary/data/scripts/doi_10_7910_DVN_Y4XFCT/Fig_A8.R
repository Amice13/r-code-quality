##########################################################################
# Figure A8: The effect of campaign spending limits on the local presence of party
# candidates conditional on running in 2012
##########################################################################

rm(list=ls())

library(ggplot2)
library(rdrobust)
library(ggthemes)
library(ggplot2)
library(here)
library(data.table)

electoral <- readRDS(file = here('data','mayors_municipal_level.rds'))


party_test <- data.frame(conventional_coef=as.numeric(),
                           robust_coef = as.numeric(),
                           conventional_upper = as.numeric(),
                           conventional_lower = as.numeric(),
                           robust_upper = as.numeric(),
                           robust_lower = as.numeric(),
                           n=as.numeric(),
                           group=as.character(),
                           dv=as.character(),
                           party = as.character(),
                           stringsAsFactors=FALSE)


# PT

pt_candidate <- with(electoral[pt_candidate_2012==1], rdrobust(pt_candidate_2016, inverted_margin, all=TRUE))
summary(pt_candidate)

temp <- data.frame(conventional_coef = pt_candidate$coef[[1]],
                   robust_coef = pt_candidate$coef[[3]],
                   conventional_upper = pt_candidate$ci[[4]],
                   conventional_lower = pt_candidate$ci[[1]],
                   robust_upper = pt_candidate$ci[[6]],
                   robust_lower = pt_candidate$ci[[3]],
                   n = pt_candidate$N_h[[1]] + pt_candidate$N_h[[2]],
                   group = 'After Spending Caps',
                   party = 'PT',
                   dv = 'Fielded Candidate')

party_test = rbind(party_test,temp)


# psd

psd_candidate <- with(electoral[psd_candidate_2012==1], rdrobust(psd_candidate_2016, inverted_margin, all=TRUE))
summary(psd_candidate)

temp <- data.frame(conventional_coef = psd_candidate$coef[[1]],
                   robust_coef = psd_candidate$coef[[3]],
                   conventional_upper = psd_candidate$ci[[4]],
                   conventional_lower = psd_candidate$ci[[1]],
                   robust_upper = psd_candidate$ci[[6]],
                   robust_lower = psd_candidate$ci[[3]],
                   n = psd_candidate$N_h[[1]] + psd_candidate$N_h[[2]],
                   group = 'After Spending Caps',
                   party = 'PSD',
                   dv = 'Fielded Candidate')

party_test = rbind(party_test,temp)


# psdb

psdb_candidate <- with(electoral[psdb_candidate_2012==1], rdrobust(psdb_candidate_2016, inverted_margin, all=TRUE))
summary(psdb_candidate)

temp <- data.frame(conventional_coef = psdb_candidate$coef[[1]],
                   robust_coef = psdb_candidate$coef[[3]],
                   conventional_upper = psdb_candidate$ci[[4]],
                   conventional_lower = psdb_candidate$ci[[1]],
                   robust_upper = psdb_candidate$ci[[6]],
                   robust_lower = psdb_candidate$ci[[3]],
                   n = psdb_candidate$N_h[[1]] + psdb_candidate$N_h[[2]],
                   group = 'After Spending Caps',
                   party = 'PSDB',
                   dv = 'Fielded Candidate')

party_test = rbind(party_test,temp)

# pp

pp_candidate <- with(electoral[pp_candidate_2012==1], rdrobust(pp_candidate_2016, inverted_margin, all=TRUE))
summary(pp_candidate)

temp <- data.frame(conventional_coef = pp_candidate$coef[[1]],
                   robust_coef = pp_candidate$coef[[3]],
                   conventional_upper = pp_candidate$ci[[4]],
                   conventional_lower = pp_candidate$ci[[1]],
                   robust_upper = pp_candidate$ci[[6]],
                   robust_lower = pp_candidate$ci[[3]],
                   n = pp_candidate$N_h[[1]] + pp_candidate$N_h[[2]],
                   group = 'After Spending Caps',
                   party = 'PP',
                   dv = 'Fielded Candidate')

party_test = rbind(party_test,temp)

# mdb

mdb_candidate <- with(electoral[mdb_candidate_2012==1], rdrobust(mdb_candidate_2016, inverted_margin, all=TRUE))
summary(mdb_candidate)

temp <- data.frame(conventional_coef = mdb_candidate$coef[[1]],
                   robust_coef = mdb_candidate$coef[[3]],
                   conventional_upper = mdb_candidate$ci[[4]],
                   conventional_lower = mdb_candidate$ci[[1]],
                   robust_upper = mdb_candidate$ci[[6]],
                   robust_lower = mdb_candidate$ci[[3]],
                   n = mdb_candidate$N_h[[1]] + mdb_candidate$N_h[[2]],
                   group = 'After Spending Caps',
                   party = 'MDB',
                   dv = 'Fielded Candidate')

party_test = rbind(party_test,temp)

# psc

psc_candidate <- with(electoral[psc_candidate_2012==1], rdrobust(psc_candidate_2016, inverted_margin, all=TRUE))
summary(psc_candidate)

temp <- data.frame(conventional_coef = psc_candidate$coef[[1]],
                   robust_coef = psc_candidate$coef[[3]],
                   conventional_upper = psc_candidate$ci[[4]],
                   conventional_lower = psc_candidate$ci[[1]],
                   robust_upper = psc_candidate$ci[[6]],
                   robust_lower = psc_candidate$ci[[3]],
                   n = psc_candidate$N_h[[1]] + psc_candidate$N_h[[2]],
                   group = 'After Spending Caps',
                   party = 'PSC',
                   dv = 'Fielded Candidate')

party_test = rbind(party_test,temp)


# prb

prb_candidate <- with(electoral[prb_candidate_2012==1], rdrobust(prb_candidate_2016, inverted_margin, all=TRUE))
summary(prb_candidate)

temp <- data.frame(conventional_coef = prb_candidate$coef[[1]],
                   robust_coef = prb_candidate$coef[[3]],
                   conventional_upper = prb_candidate$ci[[4]],
                   conventional_lower = prb_candidate$ci[[1]],
                   robust_upper = prb_candidate$ci[[6]],
                   robust_lower = prb_candidate$ci[[3]],
                   n = prb_candidate$N_h[[1]] + prb_candidate$N_h[[2]],
                   group = 'After Spending Caps',
                   party = 'Republicanos',
                   dv = 'Fielded Candidate')

party_test = rbind(party_test,temp)


# Plot ----
plot.candidates <- ggplot(party_test[party_test$dv == 'Fielded Candidate',], aes(x = party, y = robust_coef)) +
  geom_errorbar(aes(ymax = (robust_upper), ymin = (robust_lower)), width=0,size=.75,position=position_dodge(width=0.5)) +
  geom_point(alpha=.9,size=2.5 ,position=position_dodge(width=0.5)) +
  geom_hline(yintercept=0, linetype="dotted",colour='gray20') +
  theme_minimal(base_size = 12) +
  #scale_color_manual(values=c("grey", "black")) +
  theme(legend.position = "top") +
  theme(legend.title=element_blank()) +
  xlab("") +
  ylab('Effect of Stricter Spending Caps on \n Candidacies') +
  NULL

# ggsave(here('img','Fig_A8.pdf'), plot = plot.candidates, device = 'pdf',height = 13, width = 13, units = 'cm') 
