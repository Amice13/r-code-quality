##########################################################################
# Figure 3: Effects of (stricter) campaign spending limits on 
# fielding candidates and electoral victories for selected parties
##########################################################################

rm(list=ls())

library(ggplot2)
library(rdrobust)
library(ggthemes)
library(ggplot2)
library(here)

electoral <- readRDS(here("data","mayors_municipal_level.rds"))


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

pt_candidate <- with(electoral, rdrobust(pt_candidate_2016, inverted_margin, all=TRUE))

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

pt_elected <- with(electoral, rdrobust(pt_elected_2016, inverted_margin, all=TRUE))

temp <- data.frame(conventional_coef = pt_elected$coef[[1]],
                   robust_coef = pt_elected$coef[[3]],
                   conventional_upper = pt_elected$ci[[4]],
                   conventional_lower = pt_elected$ci[[1]],
                   robust_upper = pt_elected$ci[[6]],
                   robust_lower = pt_elected$ci[[3]],
                   n = pt_elected$N_h[[1]] + pt_elected$N_h[[2]],
                   group = 'After Spending Caps',
                   party = 'PT',
                   dv = 'Elected Mayor')
party_test = rbind(party_test,temp)

# psd

psd_candidate <- with(electoral, rdrobust(psd_candidate_2016, inverted_margin, all=TRUE))

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

psd_elected <- with(electoral, rdrobust(psd_elected_2016, inverted_margin, all=TRUE))

temp <- data.frame(conventional_coef = psd_elected$coef[[1]],
                   robust_coef = psd_elected$coef[[3]],
                   conventional_upper = psd_elected$ci[[4]],
                   conventional_lower = psd_elected$ci[[1]],
                   robust_upper = psd_elected$ci[[6]],
                   robust_lower = psd_elected$ci[[3]],
                   n = psd_elected$N_h[[1]] + psd_elected$N_h[[2]],
                   group = 'After Spending Caps',
                   party = 'PSD',
                   dv = 'Elected Mayor')
party_test = rbind(party_test,temp)

# psdb

psdb_candidate <- with(electoral, rdrobust(psdb_candidate_2016, inverted_margin, all=TRUE))

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

psdb_elected <- with(electoral, rdrobust(psdb_elected_2016, inverted_margin, all=TRUE))

temp <- data.frame(conventional_coef = psdb_elected$coef[[1]],
                   robust_coef = psdb_elected$coef[[3]],
                   conventional_upper = psdb_elected$ci[[4]],
                   conventional_lower = psdb_elected$ci[[1]],
                   robust_upper = psdb_elected$ci[[6]],
                   robust_lower = psdb_elected$ci[[3]],
                   n = psdb_elected$N_h[[1]] + psdb_elected$N_h[[2]],
                   group = 'After Spending Caps',
                   party = 'PSDB',
                   dv = 'Elected Mayor')
party_test = rbind(party_test,temp)

# pp

pp_candidate <- with(electoral, rdrobust(pp_candidate_2016, inverted_margin, all=TRUE))

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

pp_elected <- with(electoral, rdrobust(pp_elected_2016, inverted_margin, all=TRUE))

temp <- data.frame(conventional_coef = pp_elected$coef[[1]],
                   robust_coef = pp_elected$coef[[3]],
                   conventional_upper = pp_elected$ci[[4]],
                   conventional_lower = pp_elected$ci[[1]],
                   robust_upper = pp_elected$ci[[6]],
                   robust_lower = pp_elected$ci[[3]],
                   n = pp_elected$N_h[[1]] + pp_elected$N_h[[2]],
                   group = 'After Spending Caps',
                   party = 'PP',
                   dv = 'Elected Mayor')
party_test = rbind(party_test,temp)

# mdb

mdb_candidate <- with(electoral, rdrobust(mdb_candidate_2016, inverted_margin, all=TRUE))

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

mdb_elected <- with(electoral, rdrobust(mdb_elected_2016, inverted_margin, all=TRUE))

temp <- data.frame(conventional_coef = mdb_elected$coef[[1]],
                   robust_coef = mdb_elected$coef[[3]],
                   conventional_upper = mdb_elected$ci[[4]],
                   conventional_lower = mdb_elected$ci[[1]],
                   robust_upper = mdb_elected$ci[[6]],
                   robust_lower = mdb_elected$ci[[3]],
                   n = mdb_elected$N_h[[1]] + mdb_elected$N_h[[2]],
                   group = 'After Spending Caps',
                   party = 'MDB',
                   dv = 'Elected Mayor')
party_test = rbind(party_test,temp)

# psc

psc_candidate <- with(electoral, rdrobust(psc_candidate_2016, inverted_margin, all=TRUE))

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

psc_elected <- with(electoral, rdrobust(psc_elected_2016, inverted_margin, all=TRUE))

temp <- data.frame(conventional_coef = psc_elected$coef[[1]],
                   robust_coef = psc_elected$coef[[3]],
                   conventional_upper = psc_elected$ci[[4]],
                   conventional_lower = psc_elected$ci[[1]],
                   robust_upper = psc_elected$ci[[6]],
                   robust_lower = psc_elected$ci[[3]],
                   n = psc_elected$N_h[[1]] + psc_elected$N_h[[2]],
                   group = 'After Spending Caps',
                   party = 'PSC',
                   dv = 'Elected Mayor')
party_test = rbind(party_test,temp)


# Small

small_candidate <- with(electoral, rdrobust(small_cand_2016, inverted_margin, all=TRUE))

temp <- data.frame(conventional_coef = small_candidate$coef[[1]],
                   robust_coef = small_candidate$coef[[3]],
                   conventional_upper = small_candidate$ci[[4]],
                   conventional_lower = small_candidate$ci[[1]],
                   robust_upper = small_candidate$ci[[6]],
                   robust_lower = small_candidate$ci[[3]],
                   n = small_candidate$N_h[[1]] + small_candidate$N_h[[2]],
                   group = 'After Spending Caps',
                   party = 'Small Parties',
                   dv = 'Fielded Candidate')

party_test = rbind(party_test,temp)

small_elected <- with(electoral, rdrobust(small_elected_2016, inverted_margin, all=TRUE))

temp <- data.frame(conventional_coef = small_elected$coef[[1]],
                   robust_coef = small_elected$coef[[3]],
                   conventional_upper = small_elected$ci[[4]],
                   conventional_lower = small_elected$ci[[1]],
                   robust_upper = small_elected$ci[[6]],
                   robust_lower = small_elected$ci[[3]],
                   n = small_elected$N_h[[1]] + small_elected$N_h[[2]],
                   group = 'After Spending Caps',
                   party = 'Small Parties',
                   dv = 'Elected Mayor')
party_test = rbind(party_test,temp)


# Plot ----
plot.candidates <- ggplot(party_test[party_test$dv == 'Fielded Candidate',], aes(x = party, y = robust_coef)) +
  geom_errorbar(aes(ymax = (robust_upper), ymin = (robust_lower)), width=0,size=.75,position=position_dodge(width=0.5)) +
  geom_point(alpha=.9,size=2.5 ,position=position_dodge(width=0.5)) +
  geom_hline(yintercept=0, linetype="dotted",colour='gray20') +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top") +
  theme(legend.title=element_blank()) +
  xlab("") +
  ylab('Effect of Stricter Spending Caps on \n Candidacies') +
  NULL

#ggsave(here("img","Fig_3_left.pdf"), plot = plot.candidates, device = 'pdf',height = 13, width = 13, units = 'cm') 

plot.elected <- ggplot(party_test[party_test$dv == 'Elected Mayor',], aes(x = party, y = robust_coef)) +
  geom_errorbar(aes(ymax = (robust_upper), ymin = (robust_lower)), width=0,size=.75,position=position_dodge(width=0.5)) +
  geom_point(alpha=.9,size=2.5 ,position=position_dodge(width=0.5)) +
  geom_hline(yintercept=0, linetype="dotted",colour='gray20') +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top") +
  theme(legend.title=element_blank()) +
  xlab("") +
  ylab('Effect of Stricter Spending Caps on\n Winning Mayoral Election') +
  NULL

#ggsave(here('img','Fig_3_right.pdf'), plot = plot.elected, device = 'pdf',height = 13, width = 13, units = 'cm') 
