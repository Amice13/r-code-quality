###################
###################
## replication original paper
###################
###################

## clear environment
rm(list = ls())

## load libraries
library(tidyverse)
library(plm)
library(lmtest)
library(texreg)
library(stargazer)

## load data
dat <- haven::read_dta("original-replication-material/morning_after_dataset_countrylevel.dta")

## quick look at a few tables
table(dat$replacement_rateadj_minister, dat$democracy1)
table(dat$coupattempt_whogov, dat$democracy1)

table(dat$democracy1)
table(dat$independent)

## filter according to xtreg commands
## democracy: 3407 1s, 4329 0s
## independent: 7643 1s, 97 0s
# dat <- dat %>%
#   filter(democracy1 == 0 & independent == 1)


###################
## Tables for Figure 2
###################


## define formula components for easier model building
dv <- "replacement_rateadj_minister"
treatment <- "coupattempt_whogov"
base <- c("gdp_cap_pwt_ln", "pop_pwt_ln")
main <- c("monarchy_c", "military_c", "party_c")
elec <- c("elec_all", "t1_e_a")
econ <- c("growth_pwt", "oil_valuepop_2014_ln")
instab <- c("onset1", "nonvio_camp_2", "mid_onset", "war_onset", "domestic3_10")
purge <- c("replacement_rateadj_minister_300", "replacement_rateadj_minister_200", "replacement_rateadj_minister_100")
year <- "as.factor(YEAR_DATA)"
country.factor <- "as.factor(COUNTRY_ID)"
## random effects models (for country and leader)
country.re <- "(1 | COUNTRY_ID)"
leader.re <- "(1 | leader_cab_id)"

## alternative treatment models
civilwar <- "onset1"
nonviol <- "nonvio_camp_2"
mildisp <- "mid_onset"
intstwar <- "war_onset"
strikes <- "domestic3_10"


## all vars that are to be lagged:
lagged.vars <- c(base, main, elec, econ, instab)


## create separate data frames for different models to match no of observations


## country random effects

# plm data frame:
dat_cfe.p <- pdata.frame(dat, index = c("COUNTRY_ID", "YEAR_DATA"))

# adding lagged the vars:
dat_cfe.p <- data.frame(dat_cfe.p, 
                        setNames(data.frame(lapply(as.list(dat_cfe.p[,lagged.vars], keep.attributes = TRUE), plm::lag, k = 1)), 
                                 paste0(lagged.vars, ".l1")))
dat_cfe.p <- pdata.frame(dat_cfe.p, index = c("COUNTRY_ID", "YEAR_DATA"))

## filter according to xtreg commands
dat_cfe.p <- dat_cfe.p %>%
  filter(democracy1 == 0 & independent == 1)

base <- paste0(base, ".l1")
main <- paste0(main, ".l1")
elec <- paste0(elec, ".l1")
econ <- paste0(econ, ".l1")
instab <- paste0(instab, ".l1")

## create separate data frames for different models to match no of observations

base_data_cfe.p <- na.omit(dat_cfe.p[,c(dv, treatment, base, "YEAR_DATA", "COUNTRY_ID")])
base_data_cfe <- as.data.frame(base_data_cfe.p)

main_data_cfe.p <- na.omit(dat_cfe.p[,c(dv, treatment, base, main, "YEAR_DATA", "COUNTRY_ID")])
main_data_cfe <- as.data.frame(main_data_cfe.p)

elec_data_cfe.p <- na.omit(dat_cfe.p[,c(dv, treatment, base, main, elec, "YEAR_DATA", "COUNTRY_ID")])
elec_data_cfe <- as.data.frame(elec_data_cfe.p)

econ_data_cfe.p <- na.omit(dat_cfe.p[,c(dv, treatment, base, main, econ, "YEAR_DATA", "COUNTRY_ID")])
econ_data_cfe <- as.data.frame(econ_data_cfe.p)

instab_data_cfe.p <- na.omit(dat_cfe.p[,c(dv, treatment, base, main, instab, "YEAR_DATA", "COUNTRY_ID")])
instab_data_cfe <- as.data.frame(instab_data_cfe.p)

purge_data_cfe.p <- na.omit(dat_cfe.p[,c(dv, treatment, base, main, purge, "YEAR_DATA", "COUNTRY_ID")])
purge_data_cfe <- as.data.frame(purge_data_cfe.p)

all_data_cfe.p <- na.omit(dat_cfe.p[,c(dv, treatment, base, main, elec, econ, instab, purge, "YEAR_DATA", "COUNTRY_ID")])
all_data_cfe <- as.data.frame(all_data_cfe.p)



## leader random effects

# plm data frame:
dat_lfe.p <- pdata.frame(dat, index = c("leader_cab_id", "YEAR_DATA"))

# adding lagged the vars:
dat_lfe.p <- data.frame(dat_lfe.p, 
                        setNames(data.frame(lapply(as.list(dat_lfe.p[,lagged.vars], keep.attributes = TRUE), plm::lag, k = 1)), 
                                 paste0(lagged.vars, ".l1")))
dat_lfe.p <- pdata.frame(dat_lfe.p, index = c("leader_cab_id", "YEAR_DATA"))

## filter according to xtreg commands
dat_lfe.p <- dat_lfe.p %>%
  filter(democracy1 == 0 & independent == 1)


## create separate data frames for different models to match no of observations

base_data_lfe.p <- na.omit(dat_lfe.p[,c(dv, treatment, base, "YEAR_DATA", "leader_cab_id", "COUNTRY_ID")])
base_data_lfe <- as.data.frame(base_data_lfe.p)

main_data_lfe.p <- na.omit(dat_lfe.p[,c(dv, treatment, base, main, "YEAR_DATA", "leader_cab_id", "COUNTRY_ID")])
main_data_lfe <- as.data.frame(main_data_lfe.p)

elec_data_lfe.p <- na.omit(dat_lfe.p[,c(dv, treatment, base, main, elec, "YEAR_DATA", "leader_cab_id", "COUNTRY_ID")])
elec_data_lfe <- as.data.frame(elec_data_lfe.p)

econ_data_lfe.p <- na.omit(dat_lfe.p[,c(dv, treatment, base, main, econ, "YEAR_DATA", "leader_cab_id", "COUNTRY_ID")])
econ_data_lfe <- as.data.frame(econ_data_lfe.p)

instab_data_lfe.p <- na.omit(dat_lfe.p[,c(dv, treatment, base, main, instab, "YEAR_DATA", "leader_cab_id", "COUNTRY_ID")])
instab_data_lfe <- as.data.frame(instab_data_lfe.p)

purge_data_lfe.p <- na.omit(dat_lfe.p[,c(dv, treatment, base, main, purge, "YEAR_DATA", "leader_cab_id", "COUNTRY_ID")])
purge_data_lfe <- as.data.frame(purge_data_lfe.p)

all_data_lfe.p <- na.omit(dat_lfe.p[,c(dv, treatment, base, main, elec, econ, instab, purge, "YEAR_DATA", "leader_cab_id", "COUNTRY_ID")])
all_data_lfe <- as.data.frame(all_data_lfe.p)




## Table A1 (country fixed effects)


## xtreg replacement_rateadj_minister coupattempt_whogov L.gdp_cap_pwt_ln L.pop_pwt_ln i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# base_data_cfe.p <- pdata.frame(base_data_cfe, index = c("COUNTRY_ID", "YEAR_DATA"))
length(unique(base_data_cfe.p$COUNTRY_ID)) ## 115 (correct)
## how balanced are the data
punbalancedness(base_data_cfe.p)

## parameters for ordered beta regression
delta <- 0.95
ncores <- 1
nchains <- 4
niter <- 1000
nthreads <- 5

m1_cf_model.re <- as.formula(paste(dv, " ~ ", paste0(c(treatment, base, country.re, year), collapse = " + ")))
m1_cf_obg_ob <- ordbetareg::ordbetareg(m1_cf_model.re,
                                       data = base_data_cfe,
                                       control = list(adapt_delta = delta),
                                       cores = ncores, chains = nchains, iter = niter,
                                       ## the following two lines for parallel processing within chains
                                       threads = nthreads,
                                       backend = "cmdstanr",
                                       refresh = 0)




# xtreg replacement_rateadj_minister coupattempt_whogov L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# main_data_cfe.p <- pdata.frame(main_data_cfe, index = c("COUNTRY_ID", "YEAR_DATA"))
length(unique(main_data_cfe.p$COUNTRY_ID)) 
## how balanced are the data
punbalancedness(main_data_cfe.p)
m2_cf_model.re <- as.formula(paste(dv, " ~ ", paste0(c(treatment, base, country.re, year, main), collapse = " + ")))
m2_cf_obg_ob <- ordbetareg::ordbetareg(m2_cf_model.re,
                                       data = main_data_cfe,
                                       control = list(adapt_delta = delta),
                                       cores = ncores, chains = nchains, iter = niter,
                                       ## the following two lines for parallel processing within chains
                                       threads = nthreads,
                                       backend = "cmdstanr",
                                       refresh = 0)

## Electoral controls
# xtreg replacement_rateadj_minister coupattempt_whogov L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c L.elec_all L.t1_e_a i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# elec_data_cfe.p <- pdata.frame(elec_data_cfe, index = c("COUNTRY_ID", "YEAR_DATA"))
length(unique(elec_data_cfe.p$COUNTRY_ID)) ## 107 (original: 105)
## how balanced are the data
punbalancedness(elec_data_cfe.p)
m3_cf_model.re <- as.formula(paste(dv, " ~ ", paste0(c(treatment, base, country.re, year, main, elec), collapse = " + ")))
m3_cf_obg_ob <- ordbetareg::ordbetareg(m3_cf_model.re,
                                       data = elec_data_cfe,
                                       control = list(adapt_delta = delta),
                                       cores = ncores, chains = nchains, iter = niter,
                                       ## the following two lines for parallel processing within chains
                                       threads = nthreads,
                                       backend = "cmdstanr",
                                       refresh = 0)

## Economic controls
# xtreg replacement_rateadj_minister coupattempt_whogov L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c L.oil_valuepop_2014_ln L.growth_pwt i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# econ_data_cfe.p <- pdata.frame(econ_data_cfe, index = c("COUNTRY_ID", "YEAR_DATA"))
length(unique(econ_data_cfe.p$COUNTRY_ID)) ## 110 (original: 109)
## how balanced are the data
punbalancedness(econ_data_cfe.p)
m4_cf_model.re <- as.formula(paste(dv, " ~ ", paste0(c(treatment, base, country.re, year, main, econ), collapse = " + ")))
m4_cf_obg_ob <- ordbetareg::ordbetareg(m4_cf_model.re,
                                       data = econ_data_cfe,
                                       control = list(adapt_delta = delta),
                                       cores = ncores, chains = nchains, iter = niter,
                                       ## the following two lines for parallel processing within chains
                                       threads = nthreads,
                                       backend = "cmdstanr",
                                       refresh = 0)

## Instability controls
# xtreg replacement_rateadj_minister coupattempt_whogov L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c L.onset1 L.nonvio_camp_2 L.mid_onset L.war_onset L.domestic3_10 i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# instab_data_cfe.p <- pdata.frame(instab_data_cfe, index = c("COUNTRY_ID", "YEAR_DATA"))
length(unique(instab_data_cfe.p$COUNTRY_ID)) ## 115 (original: 114)
## how balanced are the data
punbalancedness(instab_data_cfe.p)
m5_cf_model.re <- as.formula(paste(dv, " ~ ", paste0(c(treatment, base, country.re, year, main, instab), collapse = " + ")))
m5_cf_obg_ob <- ordbetareg::ordbetareg(m5_cf_model.re,
                                       data = instab_data_cfe,
                                       control = list(adapt_delta = delta),
                                       cores = ncores, chains = nchains, iter = niter,
                                       ## the following two lines for parallel processing within chains
                                       threads = nthreads,
                                       backend = "cmdstanr",
                                       refresh = 0)

## Purges MAs
# xtreg replacement_rateadj_minister coupattempt_whogov L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c replacement_rateadj_minister_300 replacement_rateadj_minister_200 replacement_rateadj_minister_100 i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# purge_data_cfe.p <- pdata.frame(purge_data_cfe, index = c("COUNTRY_ID", "YEAR_DATA"))
length(unique(purge_data_cfe.p$COUNTRY_ID)) ## 114 (correct)
## how balanced are the data
punbalancedness(purge_data_cfe.p)
m6_cf_model.re <- as.formula(paste(dv, " ~ ", paste0(c(treatment, base, country.re, year, main, purge), collapse = " + ")))
m6_cf_obg_ob <- ordbetareg::ordbetareg(m6_cf_model.re,
                                       data = purge_data_cfe,
                                       control = list(adapt_delta = delta),
                                       cores = ncores, chains = nchains, iter = niter,
                                       ## the following two lines for parallel processing within chains
                                       threads = nthreads,
                                       backend = "cmdstanr",
                                       refresh = 0)

## All controls
# xtreg replacement_rateadj_minister coupattempt_whogov L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c L.elec_all L.t1_e_a L.oil_valuepop_2014_ln L.growth_pwt L.onset1 L.nonvio_camp_2 L.mid_onset L.war_onset L.domestic3_10 replacement_rateadj_minister_300 replacement_rateadj_minister_200 replacement_rateadj_minister_100 i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# all_data_cfe.p <- pdata.frame(all_data_cfe, index = c("COUNTRY_ID", "YEAR_DATA"))
length(unique(all_data_cfe.p$COUNTRY_ID)) ## 104 (original: 103)
## how balanced are the data
punbalancedness(all_data_cfe.p)
m7_cf_model.re <- as.formula(paste(dv, " ~ ", paste0(c(treatment, base, country.re, year, main, elec, econ, instab, purge), collapse = " + ")))
m7_cf_obg_ob <- ordbetareg::ordbetareg(m7_cf_model.re,
                                       data = all_data_cfe,
                                       control = list(adapt_delta = delta),
                                       cores = ncores, chains = nchains, iter = niter,
                                       ## the following two lines for parallel processing within chains
                                       threads = nthreads,
                                       backend = "cmdstanr",
                                       refresh = 0)




## Table A2 (leader fixed effects)





## xtreg replacement_rateadj_minister coupattempt_whogov L.gdp_cap_pwt_ln L.pop_pwt_ln i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# base_data_lfe.p <- pdata.frame(base_data_lf, index = c("leader_cab_id", "YEAR_DATA"))
length(unique(base_data_lfe.p$COUNTRY_ID)) ## 115 (correct)
## how balanced are the data
punbalancedness(base_data_lfe.p)
m1_lf_model.re <- as.formula(paste(dv, " ~ ", paste0(c(treatment, base, leader.re, year), collapse = " + ")))
m1_lf_obg_ob <- ordbetareg::ordbetareg(m1_lf_model.re,
                                       data = base_data_lfe,
                                       control = list(adapt_delta = delta),
                                       cores = ncores, chains = nchains, iter = niter,
                                       ## the following two lines for parallel processing within chains
                                       threads = nthreads,
                                       backend = "cmdstanr",
                                       refresh = 0)


# xtreg replacement_rateadj_minister coupattempt_whogov L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# main_data_lfe.p <- pdata.frame(main_data_lf, index = c("leader_cab_id", "YEAR_DATA"))
length(unique(main_data_lfe.p$COUNTRY_ID)) ## 115 (original: 114)
## how balanced are the data
punbalancedness(main_data_lfe.p)
m2_lf_model.re <- as.formula(paste(dv, " ~ ", paste0(c(treatment, base, leader.re, year, main), collapse = " + ")))
m2_lf_obg_ob <- ordbetareg::ordbetareg(m2_lf_model.re,
                                       data = main_data_lfe,
                                       control = list(adapt_delta = delta),
                                       cores = ncores, chains = nchains, iter = niter,
                                       ## the following two lines for parallel processing within chains
                                       threads = nthreads,
                                       backend = "cmdstanr",
                                       refresh = 0)

## Electoral controls
# xtreg replacement_rateadj_minister coupattempt_whogov L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c L.elec_all L.t1_e_a i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# elec_data_lfe.p <- pdata.frame(elec_data_lf, index = c("leader_cab_id", "YEAR_DATA"))
length(unique(elec_data_lfe.p$COUNTRY_ID)) ## 107 (original: 105)
## how balanced are the data
punbalancedness(elec_data_lfe.p)
m3_lf_model.re <- as.formula(paste(dv, " ~ ", paste0(c(treatment, base, leader.re, year, main, elec), collapse = " + ")))
m3_lf_obg_ob <- ordbetareg::ordbetareg(m3_lf_model.re,
                                       data = elec_data_lfe,
                                       control = list(adapt_delta = delta),
                                       cores = ncores, chains = nchains, iter = niter,
                                       ## the following two lines for parallel processing within chains
                                       threads = nthreads,
                                       backend = "cmdstanr",
                                       refresh = 0)

## Economic controls
# xtreg replacement_rateadj_minister coupattempt_whogov L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c L.oil_valuepop_2014_ln L.growth_pwt i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# econ_data_lfe.p <- pdata.frame(econ_data_lf, index = c("leader_cab_id", "YEAR_DATA"))
length(unique(econ_data_lfe.p$COUNTRY_ID)) ## 110 (original: 109)
## how balanced are the data
punbalancedness(econ_data_lfe.p)
m4_lf_model.re <- as.formula(paste(dv, " ~ ", paste0(c(treatment, base, leader.re, year, main, econ), collapse = " + ")))
m4_lf_obg_ob <- ordbetareg::ordbetareg(m4_lf_model.re,
                                       data = econ_data_lfe,
                                       control = list(adapt_delta = delta),
                                       cores = ncores, chains = nchains, iter = niter,
                                       ## the following two lines for parallel processing within chains
                                       threads = nthreads,
                                       backend = "cmdstanr",
                                       refresh = 0)

## Instability controls
# xtreg replacement_rateadj_minister coupattempt_whogov L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c L.onset1 L.nonvio_camp_2 L.mid_onset L.war_onset L.domestic3_10 i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# instab_data_lfe.p <- pdata.frame(instab_data_lf, index = c("leader_cab_id", "YEAR_DATA"))
length(unique(instab_data_lfe.p$COUNTRY_ID)) ## 115 (original: 114)
## how balanced are the data
punbalancedness(instab_data_lfe.p)
m5_lf_model.re <- as.formula(paste(dv, " ~ ", paste0(c(treatment, base, leader.re, year, main, instab), collapse = " + ")))
m5_lf_obg_ob <- ordbetareg::ordbetareg(m5_lf_model.re,
                                       data = instab_data_lfe,
                                       control = list(adapt_delta = delta),
                                       cores = ncores, chains = nchains, iter = niter,
                                       ## the following two lines for parallel processing within chains
                                       threads = nthreads,
                                       backend = "cmdstanr",
                                       refresh = 0)

## Purges MAs
# xtreg replacement_rateadj_minister coupattempt_whogov L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c replacement_rateadj_minister_300 replacement_rateadj_minister_200 replacement_rateadj_minister_100 i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# purge_data_lfe.p <- pdata.frame(purge_data_lf, index = c("leader_cab_id", "YEAR_DATA"))
length(unique(purge_data_lfe.p$COUNTRY_ID)) ## 114 (correct)
## how balanced are the data
punbalancedness(purge_data_lfe.p)
m6_lf_model.re <- as.formula(paste(dv, " ~ ", paste0(c(treatment, base, leader.re, year, main, purge), collapse = " + ")))
m6_lf_obg_ob <- ordbetareg::ordbetareg(m6_lf_model.re,
                                       data = purge_data_lfe,
                                       control = list(adapt_delta = delta),
                                       cores = ncores, chains = nchains, iter = niter,
                                       ## the following two lines for parallel processing within chains
                                       threads = nthreads,
                                       backend = "cmdstanr",
                                       refresh = 0)

## All controls
# xtreg replacement_rateadj_minister coupattempt_whogov L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c L.elec_all L.t1_e_a L.oil_valuepop_2014_ln L.growth_pwt L.onset1 L.nonvio_camp_2 L.mid_onset L.war_onset L.domestic3_10 replacement_rateadj_minister_300 replacement_rateadj_minister_200 replacement_rateadj_minister_100 i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# all_data_lfe.p <- pdata.frame(all_data_lf, index = c("leader_cab_id", "YEAR_DATA"))
length(unique(all_data_lfe.p$COUNTRY_ID)) ## 104 (original: 103)
## how balanced are the data
punbalancedness(all_data_lfe.p)
m7_lf_model.re <- as.formula(paste(dv, " ~ ", paste0(c(treatment, base, leader.re, year, main, elec, econ, instab, purge), collapse = " + ")))
m7_lf_obg_ob <- ordbetareg::ordbetareg(m7_lf_model.re,
                                       data = all_data_lfe,
                                       control = list(adapt_delta = delta),
                                       cores = ncores, chains = nchains, iter = niter,
                                       ## the following two lines for parallel processing within chains
                                       threads = nthreads,
                                       backend = "cmdstanr",
                                       refresh = 0)


## final table

screenreg(list(m1_lf_obg_ob, m2_lf_obg_ob, m3_lf_obg_ob, m4_lf_obg_ob, m5_lf_obg_ob, m6_lf_obg_ob, m7_lf_obg_ob),
          omit.coef = "YEAR_DATA",
          digits = 5)



## Table A3 (country fixed effects)

## create separate data frames for different models to match no of observations

civilwar_data_cfe.p <- na.omit(dat_cfe.p[,c(dv, civilwar, base, main, "YEAR_DATA", "COUNTRY_ID")])
civilwar_data_cfe <- as.data.frame(civilwar_data_cfe.p)

nonviol_data_cfe.p <- na.omit(dat_cfe.p[,c(dv, nonviol, base, main, "YEAR_DATA", "COUNTRY_ID")])
nonviol_data_cfe <- as.data.frame(nonviol_data_cfe.p)

mildisp_data_cfe.p <- na.omit(dat_cfe.p[,c(dv, mildisp, base, main, "YEAR_DATA", "COUNTRY_ID")])
mildisp_data_cfe <- as.data.frame(mildisp_data_cfe.p)

intstwar_data_cfe.p <- na.omit(dat_cfe.p[,c(dv, intstwar, base, main, "YEAR_DATA", "COUNTRY_ID")])
intstwar_data_cfe <- as.data.frame(intstwar_data_cfe.p)

strikes_data_cfe.p <- na.omit(dat_cfe.p[,c(dv, strikes, base, main, "YEAR_DATA", "COUNTRY_ID")])
strikes_data_cfe <- as.data.frame(strikes_data_cfe.p)



## xtreg replacement_rateadj_minister onset1 L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# civilwar_data_cfe.p <- pdata.frame(civilwar_data_cfe, index = c("COUNTRY_ID", "YEAR_DATA"))
length(unique(civilwar_data_cfe.p$COUNTRY_ID)) ## 115 (original: 114)
## how balanced are the data
punbalancedness(civilwar_data_cfe.p)
m8_cf_model.re <- as.formula(paste(dv, " ~ ", paste0(c(civilwar, base, country.re, year, main), collapse = " + ")))
m8_cf_obg_ob <- ordbetareg::ordbetareg(m8_cf_model.re,
                                       data = civilwar_data_cfe,
                                       control = list(adapt_delta = delta),
                                       cores = ncores, chains = nchains, iter = niter,
                                       ## the following two lines for parallel processing within chains
                                       threads = nthreads,
                                       backend = "cmdstanr",
                                       refresh = 0)


## xtreg replacement_rateadj_minister nonvio_camp_2 L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# nonviol_data_cfe.p <- pdata.frame(nonviol_data_cfe, index = c("COUNTRY_ID", "YEAR_DATA"))
length(unique(nonviol_data_cfe.p$COUNTRY_ID)) ## 115 (original: 114)
## how balanced are the data
punbalancedness(nonviol_data_cfe.p)
m9_cf_model.re <- as.formula(paste(dv, " ~ ", paste0(c(nonviol, base, country.re, year, main), collapse = " + ")))
m9_cf_obg_ob <- ordbetareg::ordbetareg(m9_cf_model.re,
                                       data = nonviol_data_cfe,
                                       control = list(adapt_delta = delta),
                                       cores = ncores, chains = nchains, iter = niter,
                                       ## the following two lines for parallel processing within chains
                                       threads = nthreads,
                                       backend = "cmdstanr",
                                       refresh = 0)


## xtreg replacement_rateadj_minister mid_onset L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# mildisp_data_cfe.p <- pdata.frame(mildisp_data_cfe, index = c("COUNTRY_ID", "YEAR_DATA"))
length(unique(mildisp_data_cfe.p$COUNTRY_ID)) ## 115 (original: 114)
## how balanced are the data
punbalancedness(mildisp_data_cfe.p)
m10_cf_model.re <- as.formula(paste(dv, " ~ ", paste0(c(mildisp, base, country.re, year, main), collapse = " + ")))
m10_cf_obg_ob <- ordbetareg::ordbetareg(m10_cf_model.re,
                                        data = mildisp_data_cfe,
                                        control = list(adapt_delta = delta),
                                        cores = ncores, chains = nchains, iter = niter,
                                        ## the following two lines for parallel processing within chains
                                        threads = nthreads,
                                        backend = "cmdstanr",
                                        refresh = 0)


## xtreg replacement_rateadj_minister war_onset L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# intstwar_data_cfe.p <- pdata.frame(intstwar_data_cfe, index = c("COUNTRY_ID", "YEAR_DATA"))
length(unique(intstwar_data_cfe.p$COUNTRY_ID)) ## 115 (original: 114)
## how balanced are the data
punbalancedness(intstwar_data_cfe.p)
m11_cf_model.re <- as.formula(paste(dv, " ~ ", paste0(c(intstwar, base, country.re, year, main), collapse = " + ")))
m11_cf_obg_ob <- ordbetareg::ordbetareg(m11_cf_model.re,
                                        data = intstwar_data_cfe,
                                        control = list(adapt_delta = delta),
                                        cores = ncores, chains = nchains, iter = niter,
                                        ## the following two lines for parallel processing within chains
                                        threads = nthreads,
                                        backend = "cmdstanr",
                                        refresh = 0)


## xtreg replacement_rateadj_minister domestic3_10 L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# strikes_data_cfe.p <- pdata.frame(strikes_data_cfe, index = c("COUNTRY_ID", "YEAR_DATA"))
length(unique(strikes_data_cfe.p$COUNTRY_ID)) ## 115 (original: 114)
## how balanced are the data
punbalancedness(strikes_data_cfe.p)
m12_cf_model.re <- as.formula(paste(dv, " ~ ", paste0(c(strikes, base, country.re, year, main), collapse = " + ")))
m12_cf_obg_ob <- ordbetareg::ordbetareg(m12_cf_model.re,
                                        data = strikes_data_cfe,
                                        control = list(adapt_delta = delta),
                                        cores = ncores, chains = nchains, iter = niter,
                                        ## the following two lines for parallel processing within chains
                                        threads = nthreads,
                                        backend = "cmdstanr",
                                        refresh = 0)



## final table
screenreg(list(m8_cf_obg_ob, m9_cf_obg_ob, m10_cf_obg_ob, m11_cf_obg_ob, m12_cf_obg_ob),
          omit.coef = "YEAR_DATA",
          digits = 5)


## Table A3 (leader fixed effects)

## create separate data frames for different models to match no of observations

civilwar_data_lfe.p <- na.omit(dat_lfe.p[,c(dv, civilwar, base, main, "YEAR_DATA", "COUNTRY_ID", "leader_cab_id")])
civilwar_data_lfe <- as.data.frame(civilwar_data_lfe.p)

nonviol_data_lfe.p <- na.omit(dat_lfe.p[,c(dv, nonviol, base, main, "YEAR_DATA", "COUNTRY_ID", "leader_cab_id")])
nonviol_data_lfe <- as.data.frame(nonviol_data_lfe.p)

mildisp_data_lfe.p <- na.omit(dat_lfe.p[,c(dv, mildisp, base, main, "YEAR_DATA", "COUNTRY_ID", "leader_cab_id")])
mildisp_data_lfe <- as.data.frame(mildisp_data_lfe.p)

intstwar_data_lfe.p <- na.omit(dat_lfe.p[,c(dv, intstwar, base, main, "YEAR_DATA", "COUNTRY_ID", "leader_cab_id")])
intstwar_data_lfe <- as.data.frame(intstwar_data_lfe.p)

strikes_data_lfe.p <- na.omit(dat_lfe.p[,c(dv, strikes, base, main, "YEAR_DATA", "COUNTRY_ID", "leader_cab_id")])
strikes_data_lfe <- as.data.frame(strikes_data_lfe.p)



## xtreg replacement_rateadj_minister onset1 L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# civilwar_data_lfe.p <- pdata.frame(civilwar_data_lf, index = c("leader_cab_id", "YEAR_DATA"))
length(unique(civilwar_data_lfe.p$COUNTRY_ID)) ## 115 (original: 114)
## how balanced are the data
punbalancedness(civilwar_data_lfe.p)
m8_lf_model.re <- as.formula(paste(dv, " ~ ", paste0(c(civilwar, base, leader.re, year, main), collapse = " + ")))
m8_lf_obg_ob <- ordbetareg::ordbetareg(m8_lf_model.re,
                                       data = civilwar_data_lfe,
                                       control = list(adapt_delta = delta),
                                       cores = ncores, chains = nchains, iter = niter,
                                       ## the following two lines for parallel processing within chains
                                       threads = nthreads,
                                       backend = "cmdstanr",
                                       refresh = 0)


## xtreg replacement_rateadj_minister nonvio_camp_2 L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# nonviol_data_lfe.p <- pdata.frame(nonviol_data_lf, index = c("leader_cab_id", "YEAR_DATA"))
length(unique(nonviol_data_lfe.p$COUNTRY_ID)) ## 115 (original: 114)
## how balanced are the data
punbalancedness(nonviol_data_lfe.p)
m9_lf_model.re <- as.formula(paste(dv, " ~ ", paste0(c(nonviol, base, leader.re, year, main), collapse = " + ")))
m9_lf_obg_ob <- ordbetareg::ordbetareg(m9_lf_model.re,
                                       data = nonviol_data_lfe,
                                       control = list(adapt_delta = delta),
                                       cores = ncores, chains = nchains, iter = niter,
                                       ## the following two lines for parallel processing within chains
                                       threads = nthreads,
                                       backend = "cmdstanr",
                                       refresh = 0)


## xtreg replacement_rateadj_minister mid_onset L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# mildisp_data_lfe.p <- pdata.frame(mildisp_data_lf, index = c("leader_cab_id", "YEAR_DATA"))
length(unique(mildisp_data_lfe.p$COUNTRY_ID)) ## 115 (original: 114)
## how balanced are the data
punbalancedness(mildisp_data_lfe.p)
m10_lf_model.re <- as.formula(paste(dv, " ~ ", paste0(c(mildisp, base, leader.re, year, main), collapse = " + ")))
m10_lf_obg_ob <- ordbetareg::ordbetareg(m10_lf_model.re,
                                        data = mildisp_data_lfe,
                                        control = list(adapt_delta = delta),
                                        cores = ncores, chains = nchains, iter = niter,
                                        ## the following two lines for parallel processing within chains
                                        threads = nthreads,
                                        backend = "cmdstanr",
                                        refresh = 0)


## xtreg replacement_rateadj_minister war_onset L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# intstwar_data_lfe.p <- pdata.frame(intstwar_data_lf, index = c("leader_cab_id", "YEAR_DATA"))
length(unique(intstwar_data_lfe.p$COUNTRY_ID)) ## 115 (original: 114)
## how balanced are the data
punbalancedness(intstwar_data_lfe.p)
m11_lf_model.re <- as.formula(paste(dv, " ~ ", paste0(c(intstwar, base, leader.re, year, main), collapse = " + ")))
m11_lf_obg_ob <- ordbetareg::ordbetareg(m11_lf_model.re,
                                        data = intstwar_data_lfe,
                                        control = list(adapt_delta = delta),
                                        cores = ncores, chains = nchains, iter = niter,
                                        ## the following two lines for parallel processing within chains
                                        threads = nthreads,
                                        backend = "cmdstanr",
                                        refresh = 0)


## xtreg replacement_rateadj_minister domestic3_10 L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# strikes_data_lfe.p <- pdata.frame(strikes_data_lf, index = c("leader_cab_id", "YEAR_DATA"))
length(unique(strikes_data_lfe.p$COUNTRY_ID)) ## 115 (original: 114)
## how balanced are the data
punbalancedness(strikes_data_lfe.p)
m12_lf_model.re <- as.formula(paste(dv, " ~ ", paste0(c(strikes, base, leader.re, year, main), collapse = " + ")))
m12_lf_obg_ob <- ordbetareg::ordbetareg(m12_lf_model.re,
                                        data = strikes_data_lfe,
                                        control = list(adapt_delta = delta),
                                        cores = ncores, chains = nchains, iter = niter,
                                        ## the following two lines for parallel processing within chains
                                        threads = nthreads,
                                        backend = "cmdstanr",
                                        refresh = 0)


## final table

screenreg(list(m8_lf_obg_ob, m9_lf_obg_ob, m10_lf_obg_ob, m11_lf_obg_ob, m12_lf_obg_ob),
          omit.coef = "YEAR_DATA",
          digits = 5)

covlabels <- c("Intercept", 
               "Failed coup attempt",
               "Log of GDP per capita",
               "Log of Population",
               "Monarchy",
               "Military regime",
               "Party regime",
               "Election",
               "Years since last election",
               "GDP growth",
               "Log of oil value/population",
               "Civil war onset",
               "Nonviolent campaign",
               "Military dispute",
               "Interstate war",
               "Strike",
               "Rate100",
               "Rate200",
               "Rate300")
collabels <- c("Base Model",
               "Main Model",
               "Electoral Controls",
               "Economic Controls",
               "Instability Controls",
               "Purges MAs",
               "All Controls")


texreg(list(m1_cf_obg_ob, m2_cf_obg_ob, m3_cf_obg_ob, m4_cf_obg_ob, m5_cf_obg_ob, m6_cf_obg_ob, m7_cf_obg_ob), 
       digits = 6,
       omit.coef = c("YEAR_DATA", "replacement_rateadj_minister_300", "replacement_rateadj_minister_200", "replacement_rateadj_minister_100"),
       sideways = T,
       custom.header = list("Dependent variable: Cabinet replacement rate" = 1:7), 
       custom.model.names = collabels, 
       custom.coef.names = covlabels,
       caption = "Ordered Beta Regression Models: Country-random and year-fixed effects",
       fontsize = "scriptsize",
       use.packages = F,
       file = "tables/regressiontable_obg_cf.tex"
       )
texreg(list(m1_lf_obg_ob, m2_lf_obg_ob, m3_lf_obg_ob, m4_lf_obg_ob, m5_lf_obg_ob, m6_lf_obg_ob, m7_lf_obg_ob), 
       digits = 6,
       omit.coef = c("YEAR_DATA", "replacement_rateadj_minister_300", "replacement_rateadj_minister_200", "replacement_rateadj_minister_100"),
       sideways = T,
       custom.header = list("Dependent variable: Cabinet replacement rate" = 1:7), 
       custom.model.names = collabels, 
       custom.coef.names = covlabels,
       caption = "Ordered Beta Regression Models: Leader-random and year-fixed effects",
       fontsize = "scriptsize",
       use.packages = F,
       file = "tables/regressiontable_obg_lf.tex"
)

covlabels_aiv <- c("Intercept", 
                   "Alternative event: Civil war",
                   "Log of GDP per capita",
                   "Log of Population",
                   "Monarchy",
                   "Military regime",
                   "Party regime",
                   "Alternative event: Nonviolent campaigns",
                   "Alternative event: Military disputes",
                   "Alternative event: Interstate war",
                   "Alternative event: Strikes")

texreg(list(m8_cf_obg_ob, m9_cf_obg_ob, m10_cf_obg_ob, m11_cf_obg_ob, m12_cf_obg_ob), 
       digits = 6,
       omit.coef = "YEAR_DATA",
       sideways = T,
       custom.header = list("Dependent variable: Cabinet replacement rate" = 1:5), 
       custom.coef.names = covlabels_aiv,
       caption = "Ordered Beta Regression Models: Country-random and year-fixed effects",
       fontsize = "scriptsize",
       use.packages = F,
       file = "tables/regressiontable_obg_aiv_cf.tex"
)

texreg(list(m8_lf_obg_ob, m9_lf_obg_ob, m10_lf_obg_ob, m11_lf_obg_ob, m12_lf_obg_ob), 
       digits = 6,
       omit.coef = c("YEAR_DATA", "replacement_rateadj_minister_300", "replacement_rateadj_minister_200", "replacement_rateadj_minister_100"),
       sideways = T,
       custom.header = list("Dependent variable: Cabinet replacement rate" = 1:5), 
       custom.coef.names = covlabels_aiv,
       caption = "Ordered Beta Regression Models: Leader-random and year-fixed effects",
       fontsize = "scriptsize",
       use.packages = F,
       file = "tables/regressiontable_obg_aiv_lf.tex"
)


