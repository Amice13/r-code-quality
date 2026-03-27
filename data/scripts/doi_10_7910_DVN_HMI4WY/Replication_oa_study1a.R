# Libraries
install.packages(c("lmtest","plm","texreg"))
library(lmtest)
library(plm)
library(texreg)

# Load the data
load("Study1a.RData")

#######################
### Table OA3.1
#######################

lm_binary_inter <- lm(pervote_change ~ neg_share_all_other_only*lr_ches  + total_counts, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"))

lm_cont_inter <- lm(pervote_change ~ neg_share_all_other_only*lrgen_inverted + total_counts, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"))

texreg(list(lm_binary_inter, lm_cont_inter), stars=c(0.01,0.05,0.1))

#######################
### Table OA3.2
#######################

votet1_binary <- lm(pervote_change ~ neg_share_all_other_only*lr  + votet1, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"))

votet1_cont <- lm(pervote_change ~ neg_share_all_other_only*originalCMP_rile_inverted + votet1, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"))

texreg(list(votet1_binary, votet1_cont), stars=c(0.01,0.05,0.1))

#######################
### Table OA3.3
#######################

all_tot_binary <- lm(pervote_change ~ neg_share_all_other_only*lr  + total_counts, data=study1a)

all_tot_cont <- lm(pervote_change ~ neg_share_all_other_only*originalCMP_rile_inverted + total_counts, data=study1a)

texreg(list(all_tot_binary, all_tot_cont), stars=c(0.01,0.05,0.1))

#######################
### Table OA3.4
#######################

all_tot_binary_ches <- lm(pervote_change ~ neg_share_all_other_only*lr_ches  + total_counts, data=study1a)

all_tot_cont_ches <- lm(pervote_change ~ neg_share_all_other_only*lrgen_inverted  + total_counts, data=study1a)

texreg(list(all_tot_binary_ches, all_tot_cont_ches), stars=c(0.01,0.05,0.1))

#######################
### Table OA3.5
#######################

country_fe_bin <- lm(pervote_change ~ neg_share_all_other_only*lr  + total_counts + country, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"))

country_fe_cont <- lm(pervote_change ~ neg_share_all_other_only*originalCMP_rile_inverted + total_counts + country, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"))

year_fe_bin <- lm(pervote_change ~ neg_share_all_other_only*lr  + total_counts + country_year, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"))

year_fe_cont <- lm(pervote_change ~ neg_share_all_other_only*originalCMP_rile_inverted + total_counts + country_year, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"))

texreg(list(country_fe_bin, country_fe_cont, year_fe_bin, year_fe_cont), stars=c(0.01,0.05,0.1))

#######################
### Table OA3.6
#######################

temp <- subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional")
temp <- temp[,c("pervote_change","neg_share_all_other_only","lr","total_counts","CMPcode")]
temp <- temp[complete.cases(temp),]
party_cse_bin <- lm(pervote_change ~ neg_share_all_other_only*lr  + total_counts, data=temp)
G <- length(unique(temp$CMPcode))
N <- length(temp$CMPcode)
dfa <- (G/(G - 1)) * (N - 1)/party_cse_bin$df.residual
party_c_vcov_bin <- dfa * vcovHC(party_cse_bin, type = "HC0", cluster = "group", adjust = T)

temp <- subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional")
temp <- temp[,c("pervote_change","neg_share_all_other_only","originalCMP_rile_inverted","total_counts","CMPcode")]
temp <- temp[complete.cases(temp),]
party_cse_cont <- lm(pervote_change ~ neg_share_all_other_only*originalCMP_rile_inverted  + total_counts, data=temp)
G <- length(unique(temp$CMPcode))
N <- length(temp$CMPcode)
dfa <- (G/(G - 1)) * (N - 1)/party_cse_cont$df.residual
party_c_vcov_cont <- dfa * vcovHC(party_cse_cont, type = "HC0", cluster = "group", adjust = T)

texreg(list(coeftest(party_cse_bin, vcov = party_c_vcov_bin),coeftest(party_cse_cont, vcov = party_c_vcov_cont)),stars=c(0.01,0.05,0.1))

#######################
### Table OA3.7
#######################

lm_binary_inter_lastgovt <- lm(pervote_change ~ neg_share_all_other_only*lr  + total_counts + lastgovt, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"))

lm_cont_inter_lastgovt <- lm(pervote_change ~ neg_share_all_other_only*originalCMP_rile_inverted + total_counts + lastgovt, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"))

lm_binary_inter_pmparty <- lm(pervote_change ~ neg_share_all_other_only*lr  + total_counts + pm_party, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"))

lm_cont_inter_pmparty <- lm(pervote_change ~ neg_share_all_other_only*originalCMP_rile_inverted + total_counts + pm_party, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"))

texreg(list(lm_binary_inter_lastgovt, lm_cont_inter_lastgovt, lm_binary_inter_pmparty, lm_cont_inter_pmparty), stars=c(0.01,0.05,0.1))

summary(lm(pervote_change ~ neg_share_all_other_only*lr  + total_counts + lastgovt, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"&country_year!="NL_2010")))

##########################
### Table OA3.8
###########################

bin_mod_w_controls <- lm(pervote_change ~ neg_share_all_other_only*lr  + total_counts + pm_party + gdp + moderation, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"))

cont_mod_w_controls <- lm(pervote_change ~ neg_share_all_other_only*originalCMP_rile_inverted + total_counts + pm_party + gdp + moderation, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"))

bin_mod_w_controls_country_fe <- lm(pervote_change ~ neg_share_all_other_only*lr  + total_counts + pm_party + gdp + moderation + country, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"))

cont_mod_w_controls_country_fe <- lm(pervote_change ~ neg_share_all_other_only*originalCMP_rile_inverted + total_counts + pm_party + gdp + moderation + country, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"))

bin_mod_w_controls_year_fe <- lm(pervote_change ~ neg_share_all_other_only*lr  + total_counts + pm_party + gdp + moderation + country_year, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"))

cont_mod_w_controls_year_fe <- lm(pervote_change ~ neg_share_all_other_only*originalCMP_rile_inverted + total_counts + pm_party + gdp + moderation + country_year, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"))

texreg(list(bin_mod_w_controls,cont_mod_w_controls,bin_mod_w_controls_country_fe,cont_mod_w_controls_country_fe,bin_mod_w_controls_year_fe,cont_mod_w_controls_year_fe), stars=c(0.01,0.05,0.1))

##########################
### Table OA3.9
###########################

mod1_gov_econ_inter <- lm(pervote_change ~ neg_share_all_other_only*lr  + total_counts + lastgovt * gdp, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"))
mod2_gov_econ_inter <- lm(pervote_change ~ neg_share_all_other_only*originalCMP_rile_inverted + total_counts + lastgovt * gdp, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"))

mod3_gov_econ_inter <- lm(pervote_change ~ neg_share_all_other_only*lr  + total_counts + pm_party * gdp, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"))
mod4_gov_econ_inter <- lm(pervote_change ~ neg_share_all_other_only*originalCMP_rile_inverted + total_counts + pm_party * gdp, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"))

texreg(list(mod1_gov_econ_inter, mod2_gov_econ_inter, mod3_gov_econ_inter, mod4_gov_econ_inter),stars=c(0.01,0.05,0.1))

summary(lm(pervote_change ~ neg_share_all_other_only*lr  + total_counts + lastgovt * gdp, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"&country_year!="NL_2010")))

##########################
### Table OA3.10
###########################

## Niche

mod1_niche_only <- lm(pervote_change ~ neg_share_all_other_only*lr  + total_counts, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"&niche==1))
mod2_niche_only <- lm(pervote_change ~ neg_share_all_other_only*originalCMP_rile_inverted + total_counts, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"&niche==1))

## Mainstream

mod1_nonniche_only <- lm(pervote_change ~ neg_share_all_other_only*lr  + total_counts, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"&niche==0))
mod2_nonniche_only <- lm(pervote_change ~ neg_share_all_other_only*originalCMP_rile_inverted + total_counts, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"&niche==0))

# Control

mod1_niche_control <- lm(pervote_change ~ neg_share_all_other_only*lr  + total_counts + niche, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"))
mod2_niche_control <- lm(pervote_change ~ neg_share_all_other_only*originalCMP_rile_inverted  + total_counts+ niche, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"))

texreg(list(mod1_niche_only,mod2_niche_only,mod1_nonniche_only,mod2_nonniche_only,mod1_niche_control,mod2_niche_control), stars=c(0.01,0.05,0.1))

##########################
### Table OA3.11
###########################

## Governing

# Using lastgovt
mod1_lastgov_only <- lm(pervote_change ~ neg_share_all_other_only*lr  + total_counts, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional" & lastgovt==1))
mod2_lastgov_only <- lm(pervote_change ~ neg_share_all_other_only*originalCMP_rile_inverted  + total_counts, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional" & lastgovt==1))

# Using pm_party
mod1_pmparty_only <- lm(pervote_change ~ neg_share_all_other_only*lr  + total_counts, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional" & pm_party==1))
mod2_pmparty_only <- lm(pervote_change ~ neg_share_all_other_only*originalCMP_rile_inverted  + total_counts, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional" & pm_party==1))

## Opposition

# Using lastgovt
mod1_nonlastgovt_only <- lm(pervote_change ~ neg_share_all_other_only*lr  + total_counts, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional" & lastgovt==0))
mod2_nonlastgovt_only <- lm(pervote_change ~ neg_share_all_other_only*originalCMP_rile_inverted  + total_counts, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional" & lastgovt==0))

# Using pm_party
mod1_nonpmparty_only <- lm(pervote_change ~ neg_share_all_other_only*lr  + total_counts, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional" & pm_party==0))
mod2_nonpmparty_only <- lm(pervote_change ~ neg_share_all_other_only*originalCMP_rile_inverted  + total_counts, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional" & pm_party==0))

texreg(list(mod1_lastgov_only,mod2_lastgov_only,mod1_pmparty_only, mod2_pmparty_only, mod1_nonlastgovt_only,mod2_nonlastgovt_only,mod1_nonpmparty_only,mod2_nonpmparty_only), stars=c(0.01,0.05,0.1))

##########################
### Table OA3.12
###########################

# Control for party system
mod1_party_system_control <- lm(pervote_change ~ neg_share_all_other_only*lr  + total_counts + enp_votes, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"))
mod2_party_system_control <- lm(pervote_change ~ neg_share_all_other_only*originalCMP_rile_inverted  + total_counts + enp_votes, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"))

# Two-party system
mod1_twoparty_only <- lm(pervote_change ~ neg_share_all_other_only*lr  + total_counts, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"&enp_votes<3))
mod2_twoparty_only <- lm(pervote_change ~ neg_share_all_other_only*originalCMP_rile_inverted  + total_counts, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"&enp_votes<3))

# Multiparty system
mod1_multiparty_only <- lm(pervote_change ~ neg_share_all_other_only*lr  + total_counts, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"&enp_votes>=3))
mod2_multiparty_only <- lm(pervote_change ~ neg_share_all_other_only*originalCMP_rile_inverted  + total_counts, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"&enp_votes>=3))

texreg(list(mod1_party_system_control, mod2_party_system_control, mod1_twoparty_only, mod2_twoparty_only, mod1_multiparty_only, mod2_multiparty_only),stars=c(0.01,0.05,0.1))

##########################
### Table OA3.13
###########################

mod1_leftnum_control <- lm(pervote_change ~ neg_share_all_other_only*lr  + total_counts + left_num, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"))
mod2_leftnum_control <- lm(pervote_change ~ neg_share_all_other_only*originalCMP_rile_inverted  + total_counts + left_num, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"))

texreg(list(mod1_leftnum_control, mod2_leftnum_control),stars=c(0.01,0.05,0.1))


