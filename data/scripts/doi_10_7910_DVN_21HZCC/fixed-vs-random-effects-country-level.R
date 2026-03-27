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
# year <- "as.factor(YEAR_DATA)"

## alternative treatment models
civilwar <- "onset1"
nonviol <- "nonvio_camp_2"
mildisp <- "mid_onset"
intstwar <- "war_onset"
strikes <- "domestic3_10"


## all vars that are to be lagged:
lagged.vars <- c(base, main, elec, econ, instab)


## create separate data frames for different models to match no of observations

## country ficxed effects

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

main_data_cfe.p <- na.omit(dat_cfe.p[,c(dv, treatment, base, main, "YEAR_DATA", "COUNTRY_ID")])

elec_data_cfe.p <- na.omit(dat_cfe.p[,c(dv, treatment, base, main, elec, "YEAR_DATA", "COUNTRY_ID")])

econ_data_cfe.p <- na.omit(dat_cfe.p[,c(dv, treatment, base, main, econ, "YEAR_DATA", "COUNTRY_ID")])

instab_data_cfe.p <- na.omit(dat_cfe.p[,c(dv, treatment, base, main, instab, "YEAR_DATA", "COUNTRY_ID")])

purge_data_cfe.p <- na.omit(dat_cfe.p[,c(dv, treatment, base, main, purge, "YEAR_DATA", "COUNTRY_ID")])

all_data_cfe.p <- na.omit(dat_cfe.p[,c(dv, treatment, base, main, elec, econ, instab, purge, "YEAR_DATA", "COUNTRY_ID")])




## leader ficxed effects

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

main_data_lfe.p <- na.omit(dat_lfe.p[,c(dv, treatment, base, main, "YEAR_DATA", "leader_cab_id", "COUNTRY_ID")])

elec_data_lfe.p <- na.omit(dat_lfe.p[,c(dv, treatment, base, main, elec, "YEAR_DATA", "leader_cab_id", "COUNTRY_ID")])

econ_data_lfe.p <- na.omit(dat_lfe.p[,c(dv, treatment, base, main, econ, "YEAR_DATA", "leader_cab_id", "COUNTRY_ID")])

instab_data_lfe.p <- na.omit(dat_lfe.p[,c(dv, treatment, base, main, instab, "YEAR_DATA", "leader_cab_id", "COUNTRY_ID")])

purge_data_lfe.p <- na.omit(dat_lfe.p[,c(dv, treatment, base, main, purge, "YEAR_DATA", "leader_cab_id", "COUNTRY_ID")])

all_data_lfe.p <- na.omit(dat_lfe.p[,c(dv, treatment, base, main, elec, econ, instab, purge, "YEAR_DATA", "leader_cab_id", "COUNTRY_ID")])




## Table A1 (country fixed effects)





## xtreg replacement_rateadj_minister coupattempt_whogov L.gdp_cap_pwt_ln L.pop_pwt_ln i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# base_data_cfe.p <- pdata.frame(base_data_cfe, index = c("COUNTRY_ID", "YEAR_DATA"))
length(unique(base_data_cfe.p$COUNTRY_ID)) ## 115 (correct)
## how balanced are the data
punbalancedness(base_data_cfe.p)
m1_cf_model <- as.formula(paste(dv, " ~ ", paste0(c(treatment, base), collapse = " + ")))
m1_cf <- plm(m1_cf_model,
             data = base_data_cfe.p, 
             model = "within", 
             effect = "twoways")
m1_cr <- plm(m1_cf_model,
             data = base_data_cfe.p, 
             model = "random", 
             random.method = "walhus",
             random.dfcor = 3)

phtest(m1_cf, m1_cr)

# coef(summary(m1_cf))
clustse1_cf <- coeftest(m1_cf, vcovHC(m1_cf, type = "HC0", cluster = "group"))
clustse1_cf
clustse1_cr <- coeftest(m1_cr, vcovHC(m1_cr, type = "HC0", cluster = "group"))
clustse1_cr
# pvals <- clustse2[, 4]
# ses <- clustse2[, 2]
stargazer(m1_cf, m1_cr, 
          omit = "YEAR_DATA",
          # p = pvals,
          # se = ses,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text")


# xtreg replacement_rateadj_minister coupattempt_whogov L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# main_data_cfe.p <- pdata.frame(main_data_cfe, index = c("COUNTRY_ID", "YEAR_DATA"))
length(unique(main_data_cfe.p$COUNTRY_ID)) ## 115 (original: 114)
## how balanced are the data
punbalancedness(main_data_cfe.p)
m2_cf_model <- as.formula(paste(dv, " ~ ", paste0(c(treatment, base, main), collapse = " + ")))
m2_cf <- plm(m2_cf_model,
             data = main_data_cfe.p, 
             model = "within", effect = "twoways")
m2_cr <- plm(m2_cf_model,
             data = main_data_cfe.p, 
             model = "random", 
             random.method = "walhus",
             random.dfcor = 3)

phtest(m2_cf, m2_cr)

# coef(summary(m2_cf))
clustse2_cf <- coeftest(m2_cf, vcovHC(m2_cf, type = "HC0", cluster = "group"))
clustse2_cf
clustse2_cr <- coeftest(m2_cr, vcovHC(m2_cr, type = "HC0", cluster = "group"))
clustse2_cr
# pvals <- clustse2[, 4]
# ses <- clustse2[, 2]
stargazer(m2_cf, m2_cr,
          omit = "YEAR_DATA",
          # p = pvals,
          # se = ses,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text")

## Electoral controls
# xtreg replacement_rateadj_minister coupattempt_whogov L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c L.elec_all L.t1_e_a i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# elec_data_cfe.p <- pdata.frame(elec_data_cfe, index = c("COUNTRY_ID", "YEAR_DATA"))
length(unique(elec_data_cfe.p$COUNTRY_ID)) ## 107 (original: 105)
## how balanced are the data
punbalancedness(elec_data_cfe.p)
m3_cf_model <- as.formula(paste(dv, " ~ ", paste0(c(treatment, base, main, elec), collapse = " + ")))
m3_cf <- plm(m3_cf_model,
             data = elec_data_cfe.p, 
             model = "within", effect = "twoways")
m3_cr <- plm(m3_cf_model,
             data = elec_data_cfe.p, 
             model = "random", 
             random.method = "walhus",
             random.dfcor = 3)

phtest(m3_cf, m3_cr)

# coef(summary(m3_cf))
clustse3_cf <- coeftest(m3_cf, vcovHC(m3_cf, type = "HC0", cluster = "group"))
clustse3_cf
clustse3_cr <- coeftest(m3_cr, vcovHC(m3_cr, type = "HC0", cluster = "group"))
clustse3_cr
# pvals <- clustse2[, 4]
# ses <- clustse2[, 2]
stargazer(m3_cf, m3_cr,
          omit = "YEAR_DATA",
          # p = pvals,
          # se = ses,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text")

## Economic controls
# xtreg replacement_rateadj_minister coupattempt_whogov L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c L.oil_valuepop_2014_ln L.growth_pwt i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# econ_data_cfe.p <- pdata.frame(econ_data_cfe, index = c("COUNTRY_ID", "YEAR_DATA"))
length(unique(econ_data_cfe.p$COUNTRY_ID)) ## 110 (original: 109)
## how balanced are the data
punbalancedness(econ_data_cfe.p)
m4_cf_model <- as.formula(paste(dv, " ~ ", paste0(c(treatment, base, main, econ), collapse = " + ")))
m4_cf <- plm(m4_cf_model,
             data = econ_data_cfe.p, 
             model = "within", effect = "twoways")
m4_cr <- plm(m4_cf_model,
             data = econ_data_cfe.p, 
             model = "random", 
             random.method = "walhus",
             random.dfcor = 3)

phtest(m4_cf, m4_cr)

# coef(summary(m4_cf))
clustse4_cf <- coeftest(m4_cf, vcovHC(m4_cf, type = "HC0", cluster = "group"))
clustse4_cf
clustse4_cr <- coeftest(m4_cr, vcovHC(m4_cr, type = "HC0", cluster = "group"))
clustse4_cr
# pvals <- clustse2[, 4]
# ses <- clustse2[, 2]
stargazer(m4_cf, m4_cr,
          omit = "YEAR_DATA",
          # p = pvals,
          # se = ses,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text")

## Instability controls
# xtreg replacement_rateadj_minister coupattempt_whogov L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c L.onset1 L.nonvio_camp_2 L.mid_onset L.war_onset L.domestic3_10 i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# instab_data_cfe.p <- pdata.frame(instab_data_cfe, index = c("COUNTRY_ID", "YEAR_DATA"))
length(unique(instab_data_cfe.p$COUNTRY_ID)) ## 115 (original: 114)
## how balanced are the data
punbalancedness(instab_data_cfe.p)
m5_cf_model <- as.formula(paste(dv, " ~ ", paste0(c(treatment, base, main, instab), collapse = " + ")))
m5_cf <- plm(m5_cf_model,
             data = instab_data_cfe.p, 
             model = "within", effect = "twoways")
m5_cr <- plm(m5_cf_model,
             data = instab_data_cfe.p, 
             model = "random", 
             random.method = "walhus",
             random.dfcor = 3)

phtest(m5_cf, m5_cr)

# coef(summary(m5_cf))
clustse5_cf <- coeftest(m5_cf, vcovHC(m5_cf, type = "HC0", cluster = "group"))
clustse5_cf
clustse5_cr <- coeftest(m5_cr, vcovHC(m5_cr, type = "HC0", cluster = "group"))
clustse5_cr
# pvals <- clustse2[, 4]
# ses <- clustse2[, 2]
stargazer(m5_cf, m5_cr,
          omit = "YEAR_DATA",
          # p = pvals,
          # se = ses,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text")

## Purges MAs
# xtreg replacement_rateadj_minister coupattempt_whogov L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c replacement_rateadj_minister_300 replacement_rateadj_minister_200 replacement_rateadj_minister_100 i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# purge_data_cfe.p <- pdata.frame(purge_data_cfe, index = c("COUNTRY_ID", "YEAR_DATA"))
length(unique(purge_data_cfe.p$COUNTRY_ID)) ## 114 (correct)
## how balanced are the data
punbalancedness(purge_data_cfe.p)
m6_cf_model <- as.formula(paste(dv, " ~ ", paste0(c(treatment, base, main, purge), collapse = " + ")))
m6_cf <- plm(m6_cf_model,
             data = purge_data_cfe.p, 
             model = "within", effect = "twoways")
m6_cr <- plm(m6_cf_model,
             data = purge_data_cfe.p, 
             model = "random", 
             random.method = "walhus",
             random.dfcor = 3)

phtest(m6_cf, m6_cr)

# coef(summary(m6_cf))
clustse6_cf <- coeftest(m6_cf, vcovHC(m6_cf, type = "HC0", cluster = "group"))
clustse6_cf
clustse6_cr <- coeftest(m6_cr, vcovHC(m6_cr, type = "HC0", cluster = "group"))
clustse6_cr
# pvals <- clustse2[, 4]
# ses <- clustse2[, 2]
stargazer(m6_cf, m6_cr,
          omit = "YEAR_DATA",
          # p = pvals,
          # se = ses,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text")

## All controls
# xtreg replacement_rateadj_minister coupattempt_whogov L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c L.elec_all L.t1_e_a L.oil_valuepop_2014_ln L.growth_pwt L.onset1 L.nonvio_camp_2 L.mid_onset L.war_onset L.domestic3_10 replacement_rateadj_minister_300 replacement_rateadj_minister_200 replacement_rateadj_minister_100 i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# all_data_cfe.p <- pdata.frame(all_data_cfe, index = c("COUNTRY_ID", "YEAR_DATA"))
length(unique(all_data_cfe.p$COUNTRY_ID)) ## 104 (original: 103)
## how balanced are the data
punbalancedness(all_data_cfe.p)
m7_cf_model <- as.formula(paste(dv, " ~ ", paste0(c(treatment, base, main, elec, econ, instab, purge), collapse = " + ")))
m7_cf <- plm(m7_cf_model,
             data = all_data_cfe.p, 
             model = "within", effect = "twoways")
m7_cr <- plm(m7_cf_model,
             data = all_data_cfe.p, 
             model = "random", 
             random.method = "walhus",
             random.dfcor = 3)

phtest(m7_cf, m7_cr)

# coef(summary(m7_cf))
clustse7_cf <- coeftest(m7_cf, vcovHC(m7_cf, type = "HC0", cluster = "group"))
clustse7_cf
clustse7_cr <- coeftest(m7_cr, vcovHC(m7_cr, type = "HC0", cluster = "group"))
clustse7_cr
# pvals <- clustse2[, 4]
# ses <- clustse2[, 2]
stargazer(m7_cf, m7_cr,
          omit = "YEAR_DATA",
          # p = pvals,
          # se = ses,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text")


## final table
screenreg(list(m1_cf, m2_cf, m3_cf, m4_cf, m5_cf, m6_cf, m7_cf),
          omit.coef = "YEAR_DATA",
          digits = 5)
## get pvalues
m1cf <- extract(m1_cf)
m2cf <- extract(m2_cf)
m3cf <- extract(m3_cf)
m4cf <- extract(m4_cf)
m5cf <- extract(m5_cf)
m6cf <- extract(m6_cf)
m7cf <- extract(m7_cf)

m1cf
clustse1_cf
m2cf
clustse2_cf
m3cf
clustse3_cf
m4cf
clustse4_cf
m5cf
clustse5_cf
m6cf
clustse6_cf
m7cf
clustse7_cf

screenreg(list(m1_cr, m2_cr, m3_cr, m4_cr, m5_cr, m6_cr, m7_cr),
          omit.coef = "YEAR_DATA",
          digits = 5)
## get pvalues
m1cr <- extract(m1_cr)
m2cr <- extract(m2_cr)
m3cr <- extract(m3_cr)
m4cr <- extract(m4_cr)
m5cr <- extract(m5_cr)
m6cr <- extract(m6_cr)
m7cr <- extract(m7_cr)

m1cr
clustse1_cr
m2cr
clustse2_cr
m3cr
clustse3_cr
m4cr
clustse4_cr
m5cr
clustse5_cr
m6cr
clustse6_cr
m7cr
clustse7_cr


## phtest(fixedeff,randomeff)

## random effects models




## Table A2 (leader fixed effects)


## xtreg replacement_rateadj_minister coupattempt_whogov L.gdp_cap_pwt_ln L.pop_pwt_ln i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# base_data_lfe.p <- pdata.frame(base_data_lf, index = c("leader_cab_id", "YEAR_DATA"))
length(unique(base_data_lfe.p$COUNTRY_ID)) ## 115 (correct)
## how balanced are the data
punbalancedness(base_data_lfe.p)
m1_lf_model <- as.formula(paste(dv, " ~ ", paste0(c(treatment, base), collapse = " + ")))
m1_lf <- plm(m1_lf_model,
             data = base_data_lfe.p, 
             model = "within", effect = "twoways")
m1_lr <- plm(m1_lf_model,
             data = base_data_lfe.p, 
             model = "random", 
             random.method = "walhus",
             random.dfcor = 3)

phtest(m1_lf, m1_lr)

# coef(summary(m1_lf))
clustse1_lf <- coeftest(m1_lf, vcovHC(m1_lf, type = "HC0", cluster = "group"))
clustse1_lf
clustse1_lr <- coeftest(m1_lr, vcovHC(m1_lr, type = "HC0", cluster = "group"))
clustse1_lr
# pvals <- clustse2[, 4]
# ses <- clustse2[, 2]
stargazer(m1_lf, m1_lr,
          omit = "YEAR_DATA",
          # p = pvals,
          # se = ses,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text")


# xtreg replacement_rateadj_minister coupattempt_whogov L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# main_data_lfe.p <- pdata.frame(main_data_lf, index = c("leader_cab_id", "YEAR_DATA"))
length(unique(main_data_lfe.p$COUNTRY_ID)) ## 115 (original: 114)
## how balanced are the data
punbalancedness(main_data_lfe.p)
m2_lf_model <- as.formula(paste(dv, " ~ ", paste0(c(treatment, base, main), collapse = " + ")))
m2_lf <- plm(m2_lf_model,
             data = main_data_lfe.p, 
             model = "within", effect = "twoways")
m2_lr <- plm(m2_lf_model,
             data = main_data_lfe.p, 
             model = "random", 
             random.method = "walhus",
             random.dfcor = 3)

phtest(m2_lf, m2_lr)

# coef(summary(m2_lf))
clustse2_lf <- coeftest(m2_lf, vcovHC(m2_lf, type = "HC0", cluster = "group"))
clustse2_lf
clustse2_lr <- coeftest(m2_lr, vcovHC(m2_lr, type = "HC0", cluster = "group"))
clustse2_lr
# pvals <- clustse2[, 4]
# ses <- clustse2[, 2]
stargazer(m2_lf, m2_lr,
          omit = "YEAR_DATA",
          # p = pvals,
          # se = ses,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text")

## Electoral controls
# xtreg replacement_rateadj_minister coupattempt_whogov L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c L.elec_all L.t1_e_a i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# elec_data_lfe.p <- pdata.frame(elec_data_lf, index = c("leader_cab_id", "YEAR_DATA"))
length(unique(elec_data_lfe.p$COUNTRY_ID)) ## 107 (original: 105)
## how balanced are the data
punbalancedness(elec_data_lfe.p)
m3_lf_model <- as.formula(paste(dv, " ~ ", paste0(c(treatment, base, main, elec), collapse = " + ")))
m3_lf <- plm(m3_lf_model,
             data = elec_data_lfe.p, 
             model = "within", effect = "twoways")
m3_lr <- plm(m3_lf_model,
             data = elec_data_lfe.p, 
             model = "random", 
             random.method = "walhus",
             random.dfcor = 3)

phtest(m3_lf, m3_lr)

# coef(summary(m3_lf))
clustse3_lf <- coeftest(m3_lf, vcovHC(m3_lf, type = "HC0", cluster = "group"))
clustse3_lf
clustse3_lr <- coeftest(m3_lr, vcovHC(m3_lr, type = "HC0", cluster = "group"))
clustse3_lr
# pvals <- clustse2[, 4]
# ses <- clustse2[, 2]
stargazer(m3_lf, m3_lr,
          omit = "YEAR_DATA",
          # p = pvals,
          # se = ses,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text")

## Economic controls
# xtreg replacement_rateadj_minister coupattempt_whogov L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c L.oil_valuepop_2014_ln L.growth_pwt i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# econ_data_lfe.p <- pdata.frame(econ_data_lf, index = c("leader_cab_id", "YEAR_DATA"))
length(unique(econ_data_lfe.p$COUNTRY_ID)) ## 110 (original: 109)
## how balanced are the data
punbalancedness(econ_data_lfe.p)
m4_lf_model <- as.formula(paste(dv, " ~ ", paste0(c(treatment, base, main, econ), collapse = " + ")))
m4_lf <- plm(m4_lf_model,
             data = econ_data_lfe.p, 
             model = "within", effect = "twoways")
m4_lr <- plm(m4_lf_model,
             data = econ_data_lfe.p, 
             model = "random", 
             random.method = "walhus",
             random.dfcor = 3)

phtest(m4_lf, m4_lr)

# coef(summary(m4_lf))
clustse4_lf <- coeftest(m4_lf, vcovHC(m4_lf, type = "HC0", cluster = "group"))
clustse4_lf
clustse4_lr <- coeftest(m4_lr, vcovHC(m4_lr, type = "HC0", cluster = "group"))
clustse4_lr
# pvals <- clustse2[, 4]
# ses <- clustse2[, 2]
stargazer(m4_lf, m4_lr,
          omit = "YEAR_DATA",
          # p = pvals,
          # se = ses,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text")

## Instability controls
# xtreg replacement_rateadj_minister coupattempt_whogov L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c L.onset1 L.nonvio_camp_2 L.mid_onset L.war_onset L.domestic3_10 i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# instab_data_lfe.p <- pdata.frame(instab_data_lf, index = c("leader_cab_id", "YEAR_DATA"))
length(unique(instab_data_lfe.p$COUNTRY_ID)) ## 115 (original: 114)
## how balanced are the data
punbalancedness(instab_data_lfe.p)
m5_lf_model <- as.formula(paste(dv, " ~ ", paste0(c(treatment, base, main, instab), collapse = " + ")))
m5_lf <- plm(m5_lf_model,
             data = instab_data_lfe.p, 
             model = "within", effect = "twoways")
m5_lr <- plm(m5_lf_model,
             data = instab_data_lfe.p, 
             model = "random", 
             random.method = "walhus",
             random.dfcor = 3)

phtest(m5_lf, m5_lr)

# coef(summary(m5_lf))
clustse5_lf <- coeftest(m5_lf, vcovHC(m5_lf, type = "HC0", cluster = "group"))
clustse5_lf
clustse5_lr <- coeftest(m5_lr, vcovHC(m5_lr, type = "HC0", cluster = "group"))
clustse5_lr
# pvals <- clustse2[, 4]
# ses <- clustse2[, 2]
stargazer(m5_lf, m5_lr,
          omit = "YEAR_DATA",
          # p = pvals,
          # se = ses,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text")

## Purges MAs
# xtreg replacement_rateadj_minister coupattempt_whogov L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c replacement_rateadj_minister_300 replacement_rateadj_minister_200 replacement_rateadj_minister_100 i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# purge_data_lfe.p <- pdata.frame(purge_data_lf, index = c("leader_cab_id", "YEAR_DATA"))
length(unique(purge_data_lfe.p$COUNTRY_ID)) ## 114 (correct)
## how balanced are the data
punbalancedness(purge_data_lfe.p)
m6_lf_model <- as.formula(paste(dv, " ~ ", paste0(c(treatment, base, main, purge), collapse = " + ")))
m6_lf <- plm(m6_lf_model,
             data = purge_data_lfe.p, 
             model = "within", effect = "twoways")
m6_lr <- plm(m6_lf_model,
             data = purge_data_lfe.p, 
             model = "random", 
             random.method = "walhus",
             random.dfcor = 3)

phtest(m6_lf, m6_lr)

# coef(summary(m6_lf))
clustse6_lf <- coeftest(m6_lf, vcovHC(m6_lf, type = "HC0", cluster = "group"))
clustse6_lf
clustse6_lr <- coeftest(m6_lr, vcovHC(m6_lr, type = "HC0", cluster = "group"))
clustse6_lr
# pvals <- clustse2[, 4]
# ses <- clustse2[, 2]
stargazer(m6_lf, m6_lr,
          omit = "YEAR_DATA",
          # p = pvals,
          # se = ses,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text")

## All controls
# xtreg replacement_rateadj_minister coupattempt_whogov L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c L.elec_all L.t1_e_a L.oil_valuepop_2014_ln L.growth_pwt L.onset1 L.nonvio_camp_2 L.mid_onset L.war_onset L.domestic3_10 replacement_rateadj_minister_300 replacement_rateadj_minister_200 replacement_rateadj_minister_100 i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# all_data_lfe.p <- pdata.frame(all_data_lf, index = c("leader_cab_id", "YEAR_DATA"))
length(unique(all_data_lfe.p$COUNTRY_ID)) ## 104 (original: 103)
## how balanced are the data
punbalancedness(all_data_lfe.p)
m7_lf_model <- as.formula(paste(dv, " ~ ", paste0(c(treatment, base, main, elec, econ, instab, purge), collapse = " + ")))
m7_lf <- plm(m7_lf_model,
             data = all_data_lfe.p, 
             model = "within", effect = "twoways")
m7_lr <- plm(m7_lf_model,
             data = all_data_lfe.p, 
             model = "random", 
             random.method = "walhus",
             random.dfcor = 3)

phtest(m7_lf, m7_lr)

# coef(summary(m7_lf))
clustse7_lf <- coeftest(m7_lf, vcovHC(m7_lf, type = "HC0", cluster = "group"))
clustse7_lf
clustse7_lr <- coeftest(m7_lr, vcovHC(m7_lr, type = "HC0", cluster = "group"))
clustse7_lr
# pvals <- clustse2[, 4]
# ses <- clustse2[, 2]
stargazer(m7_lf, m7_lr,
          omit = "YEAR_DATA",
          # p = pvals,
          # se = ses,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text")


## final table
screenreg(list(m1_lf, m2_lf, m3_lf, m4_lf, m5_lf, m6_lf, m7_lf),
          omit.coef = "YEAR_DATA",
          digits = 5)
## get pvalues
m1lf <- extract(m1_lf)
m2lf <- extract(m2_lf)
m3lf <- extract(m3_lf)
m4lf <- extract(m4_lf)
m5lf <- extract(m5_lf)
m6lf <- extract(m6_lf)
m7lf <- extract(m7_lf)

m1lf
clustse1_lf
m2lf
clustse2_lf
m3lf
clustse3_lf
m4lf
clustse4_lf
m5lf
clustse5_lf
m6lf
clustse6_lf
m7lf
clustse7_lf


screenreg(list(m1_lr, m2_lr, m3_lr, m4_lr, m5_lr, m6_lr, m7_lr),
          omit.coef = "YEAR_DATA",
          digits = 5)
## get pvalues
m1lr <- extract(m1_lr)
m2lr <- extract(m2_lr)
m3lr <- extract(m3_lr)
m4lr <- extract(m4_lr)
m5lr <- extract(m5_lr)
m6lr <- extract(m6_lr)
m7lr <- extract(m7_lr)

m1lr
clustse1_lr
m2lr
clustse2_lr
m3lr
clustse3_lr
m4lr
clustse4_lr
m5lr
clustse5_lr
m6lr
clustse6_lr
m7lr
clustse7_lr


## Table A3 (country fixed effects)

## create separate data frames for different models to match no of observations

civilwar_data_cfe.p <- na.omit(dat_cfe.p[,c(dv, civilwar, base, main, "YEAR_DATA", "COUNTRY_ID")])

nonviol_data_cfe.p <- na.omit(dat_cfe.p[,c(dv, nonviol, base, main, "YEAR_DATA", "COUNTRY_ID")])

mildisp_data_cfe.p <- na.omit(dat_cfe.p[,c(dv, mildisp, base, main, "YEAR_DATA", "COUNTRY_ID")])

intstwar_data_cfe.p <- na.omit(dat_cfe.p[,c(dv, intstwar, base, main, "YEAR_DATA", "COUNTRY_ID")])

strikes_data_cfe.p <- na.omit(dat_cfe.p[,c(dv, strikes, base, main, "YEAR_DATA", "COUNTRY_ID")])



## xtreg replacement_rateadj_minister onset1 L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# civilwar_data_cfe.p <- pdata.frame(civilwar_data_cfe, index = c("COUNTRY_ID", "YEAR_DATA"))
length(unique(civilwar_data_cfe.p$COUNTRY_ID)) ## 115 (original: 114)
## how balanced are the data
punbalancedness(civilwar_data_cfe.p)
m8_cf_model <- as.formula(paste(dv, " ~ ", paste0(c(civilwar, base, main), collapse = " + ")))
m8_cf <- plm(m8_cf_model,
             data = civilwar_data_cfe.p, 
             model = "within", effect = "twoways")
m8_cr <- plm(m8_cf_model,
             data = civilwar_data_cfe.p, 
             model = "random", 
             random.method = "walhus",
             random.dfcor = 3)

phtest(m8_cf, m8_cr)

# coef(summary(m1_cf))
clustse8_cf <- coeftest(m8_cf, vcovHC(m8_cf, type = "HC0", cluster = "group"))
clustse8_cf
clustse8_cr <- coeftest(m8_cr, vcovHC(m8_cr, type = "HC0", cluster = "group"))
clustse8_cr
# pvals <- clustse2[, 4]
# ses <- clustse2[, 2]
stargazer(m8_cf, m8_cr,
          omit = "YEAR_DATA",
          # p = pvals,
          # se = ses,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text")


## xtreg replacement_rateadj_minister nonvio_camp_2 L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# nonviol_data_cfe.p <- pdata.frame(nonviol_data_cfe, index = c("COUNTRY_ID", "YEAR_DATA"))
length(unique(nonviol_data_cfe.p$COUNTRY_ID)) ## 115 (original: 114)
## how balanced are the data
punbalancedness(nonviol_data_cfe.p)
m9_cf_model <- as.formula(paste(dv, " ~ ", paste0(c(nonviol, base, main), collapse = " + ")))
m9_cf <- plm(m9_cf_model,
             data = nonviol_data_cfe.p, 
             model = "within", effect = "twoways")
m9_cr <- plm(m9_cf_model,
             data = nonviol_data_cfe.p, 
             model = "random", 
             random.method = "walhus",
             random.dfcor = 3)

phtest(m9_cf, m9_cr)

# coef(summary(m1_cf))
clustse9_cf <- coeftest(m9_cf, vcovHC(m9_cf, type = "HC0", cluster = "group"))
clustse9_cf
clustse9_cr <- coeftest(m9_cr, vcovHC(m9_cr, type = "HC0", cluster = "group"))
clustse9_cr
# pvals <- clustse2[, 4]
# ses <- clustse2[, 2]
stargazer(m9_cf, m9_cr,
          omit = "YEAR_DATA",
          # p = pvals,
          # se = ses,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text")


## xtreg replacement_rateadj_minister mid_onset L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# mildisp_data_cfe.p <- pdata.frame(mildisp_data_cfe, index = c("COUNTRY_ID", "YEAR_DATA"))
length(unique(mildisp_data_cfe.p$COUNTRY_ID)) ## 115 (original: 114)
## how balanced are the data
punbalancedness(mildisp_data_cfe.p)
m10_cf_model <- as.formula(paste(dv, " ~ ", paste0(c(mildisp, base, main), collapse = " + ")))
m10_cf <- plm(m10_cf_model,
              data = mildisp_data_cfe.p, 
              model = "within", effect = "twoways")
m10_cr <- plm(m10_cf_model,
              data = mildisp_data_cfe.p, 
              model = "random", 
              random.method = "walhus",
              random.dfcor = 3)

phtest(m10_cf, m10_cr)

# coef(summary(m1_cf))
clustse10_cf <- coeftest(m10_cf, vcovHC(m10_cf, type = "HC0", cluster = "group"))
clustse10_cf
clustse10_cr <- coeftest(m10_cr, vcovHC(m10_cr, type = "HC0", cluster = "group"))
clustse10_cr
# pvals <- clustse2[, 4]
# ses <- clustse2[, 2]
stargazer(m10_cf, m10_cr,
          omit = "YEAR_DATA",
          # p = pvals,
          # se = ses,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text")


## xtreg replacement_rateadj_minister war_onset L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# intstwar_data_cfe.p <- pdata.frame(intstwar_data_cfe, index = c("COUNTRY_ID", "YEAR_DATA"))
length(unique(intstwar_data_cfe.p$COUNTRY_ID)) ## 115 (original: 114)
## how balanced are the data
punbalancedness(intstwar_data_cfe.p)
m11_cf_model <- as.formula(paste(dv, " ~ ", paste0(c(intstwar, base, main), collapse = " + ")))
m11_cf <- plm(m11_cf_model,
              data = intstwar_data_cfe.p, 
              model = "within", effect = "twoways")
m11_cr <- plm(m11_cf_model,
              data = intstwar_data_cfe.p, 
              model = "random", 
              random.method = "walhus",
              random.dfcor = 3)

phtest(m11_cf, m11_cr)

# coef(summary(m1_cf))
clustse11_cf <- coeftest(m11_cf, vcovHC(m11_cf, type = "HC0", cluster = "group"))
clustse11_cf
clustse11_cr <- coeftest(m11_cr, vcovHC(m11_cr, type = "HC0", cluster = "group"))
clustse11_cr
# pvals <- clustse2[, 4]
# ses <- clustse2[, 2]
stargazer(m11_cf, m11_cr,
          omit = "YEAR_DATA",
          # p = pvals,
          # se = ses,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text")


## xtreg replacement_rateadj_minister domestic3_10 L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# strikes_data_cfe.p <- pdata.frame(strikes_data_cfe, index = c("COUNTRY_ID", "YEAR_DATA"))
length(unique(strikes_data_cfe.p$COUNTRY_ID)) ## 115 (original: 114)
## how balanced are the data
punbalancedness(strikes_data_cfe.p)
m12_cf_model <- as.formula(paste(dv, " ~ ", paste0(c(strikes, base, main), collapse = " + ")))
m12_cf <- plm(m12_cf_model,
              data = strikes_data_cfe.p, 
              model = "within", effect = "twoways")
m12_cr <- plm(m12_cf_model,
              data = strikes_data_cfe.p, 
              model = "random", 
              random.method = "walhus",
              random.dfcor = 3)

phtest(m12_cf, m12_cr)

# coef(summary(m1_cf))
clustse12_cf <- coeftest(m12_cf, vcovHC(m12_cf, type = "HC0", cluster = "group"))
clustse12_cf
clustse12_cr <- coeftest(m12_cr, vcovHC(m12_cr, type = "HC0", cluster = "group"))
clustse12_cr
# pvals <- clustse2[, 4]
# ses <- clustse2[, 2]
stargazer(m12_cf, m12_cr,
          omit = "YEAR_DATA",
          # p = pvals,
          # se = ses,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text")



## final table
screenreg(list(m8_cf, m9_cf, m10_cf, m11_cf, m12_cf),
          omit.coef = "YEAR_DATA",
          digits = 5)
## get pvalues
m8cf <- extract(m8_cf)
m9cf <- extract(m9_cf)
m10cf <- extract(m10_cf)
m11cf <- extract(m11_cf)
m12cf <- extract(m12_cf)

m8cf
clustse8_cf
m9cf
clustse9_cf
m10cf
clustse10_cf
m11cf
clustse11_cf
m12cf
clustse12_cf

screenreg(list(m8_cr, m9_cr, m10_cr, m11_cr, m12_cr),
          omit.coef = "YEAR_DATA",
          digits = 5)
## get pvalues
m8cr <- extract(m8_cr)
m9cr <- extract(m9_cr)
m10cr <- extract(m10_cr)
m11cr <- extract(m11_cr)
m12cr <- extract(m12_cr)

m8cr
clustse8_cr
m9cr
clustse9_cr
m10cr
clustse10_cr
m11cr
clustse11_cr
m12cr
clustse12_cr


## Table A3 (leader fixed effects)

## create separate data frames for different models to match no of observations

civilwar_data_lfe.p <- na.omit(dat_lfe.p[,c(dv, civilwar, base, main, "YEAR_DATA", "COUNTRY_ID", "leader_cab_id")])

nonviol_data_lfe.p <- na.omit(dat_lfe.p[,c(dv, nonviol, base, main, "YEAR_DATA", "COUNTRY_ID", "leader_cab_id")])

mildisp_data_lfe.p <- na.omit(dat_lfe.p[,c(dv, mildisp, base, main, "YEAR_DATA", "COUNTRY_ID", "leader_cab_id")])

intstwar_data_lfe.p <- na.omit(dat_lfe.p[,c(dv, intstwar, base, main, "YEAR_DATA", "COUNTRY_ID", "leader_cab_id")])

strikes_data_lfe.p <- na.omit(dat_lfe.p[,c(dv, strikes, base, main, "YEAR_DATA", "COUNTRY_ID", "leader_cab_id")])



## xtreg replacement_rateadj_minister onset1 L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# civilwar_data_lfe.p <- pdata.frame(civilwar_data_lf, index = c("leader_cab_id", "YEAR_DATA"))
length(unique(civilwar_data_lfe.p$COUNTRY_ID)) ## 115 (original: 114)
## how balanced are the data
punbalancedness(civilwar_data_lfe.p)
m8_lf_model <- as.formula(paste(dv, " ~ ", paste0(c(civilwar, base, main), collapse = " + ")))
m8_lf <- plm(m8_lf_model,
             data = civilwar_data_lfe.p, 
             model = "within", effect = "twoways")
m8_lr <- plm(m8_lf_model,
             data = civilwar_data_lfe.p, 
             model = "random", 
             random.method = "walhus",
             random.dfcor = 3)

phtest(m8_lf, m8_lr)

# coef(summary(m1_lf))
clustse8_lf <- coeftest(m8_lf, vcovHC(m8_lf, type = "HC0", cluster = "group"))
clustse8_lf
clustse8_lr <- coeftest(m8_lr, vcovHC(m8_lr, type = "HC0", cluster = "group"))
clustse8_lr
# pvals <- clustse2[, 4]
# ses <- clustse2[, 2]
stargazer(m8_lf, m8_lr,
          omit = "YEAR_DATA",
          # p = pvals,
          # se = ses,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text")


## xtreg replacement_rateadj_minister nonvio_camp_2 L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# nonviol_data_lfe.p <- pdata.frame(nonviol_data_lf, index = c("leader_cab_id", "YEAR_DATA"))
length(unique(nonviol_data_lfe.p$COUNTRY_ID)) ## 115 (original: 114)
## how balanced are the data
punbalancedness(nonviol_data_lfe.p)
m9_lf_model <- as.formula(paste(dv, " ~ ", paste0(c(nonviol, base, main), collapse = " + ")))
m9_lf <- plm(m9_lf_model,
             data = nonviol_data_lfe.p, 
             model = "within", effect = "twoways")
m9_lr <- plm(m9_lf_model,
             data = nonviol_data_lfe.p, 
             model = "random", 
             random.method = "walhus",
             random.dfcor = 3)

phtest(m9_lf, m9_lr)

# coef(summary(m1_lf))
clustse9_lf <- coeftest(m9_lf, vcovHC(m9_lf, type = "HC0", cluster = "group"))
clustse9_lf
clustse9_lr <- coeftest(m9_lr, vcovHC(m9_lr, type = "HC0", cluster = "group"))
clustse9_lr
# pvals <- clustse2[, 4]
# ses <- clustse2[, 2]
stargazer(m9_lf, m9_lr,
          omit = "YEAR_DATA",
          # p = pvals,
          # se = ses,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text")


## xtreg replacement_rateadj_minister mid_onset L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# mildisp_data_lfe.p <- pdata.frame(mildisp_data_lf, index = c("leader_cab_id", "YEAR_DATA"))
length(unique(mildisp_data_lfe.p$COUNTRY_ID)) ## 115 (original: 114)
## how balanced are the data
punbalancedness(mildisp_data_lfe.p)
m10_lf_model <- as.formula(paste(dv, " ~ ", paste0(c(mildisp, base, main), collapse = " + ")))
m10_lf <- plm(m10_lf_model,
              data = mildisp_data_lfe.p, 
              model = "within", effect = "twoways")
m10_lr <- plm(m10_lf_model,
              data = mildisp_data_lfe.p, 
              model = "random", 
              random.method = "walhus",
              random.dfcor = 3)

phtest(m10_lf, m10_lr)

# coef(summary(m1_lf))
clustse10_lf <- coeftest(m10_lf, vcovHC(m10_lf, type = "HC0", cluster = "group"))
clustse10_lf
clustse10_lr <- coeftest(m10_lr, vcovHC(m10_lr, type = "HC0", cluster = "group"))
clustse10_lr
# pvals <- clustse2[, 4]
# ses <- clustse2[, 2]
stargazer(m10_lf, m10_lr,
          omit = "YEAR_DATA",
          # p = pvals,
          # se = ses,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text")


## xtreg replacement_rateadj_minister war_onset L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# intstwar_data_lfe.p <- pdata.frame(intstwar_data_lf, index = c("leader_cab_id", "YEAR_DATA"))
length(unique(intstwar_data_lfe.p$COUNTRY_ID)) ## 115 (original: 114)
## how balanced are the data
punbalancedness(intstwar_data_lfe.p)
m11_lf_model <- as.formula(paste(dv, " ~ ", paste0(c(intstwar, base, main), collapse = " + ")))
m11_lf <- plm(m11_lf_model,
              data = intstwar_data_lfe.p, 
              model = "within", effect = "twoways")
m11_lr <- plm(m11_lf_model,
              data = intstwar_data_lfe.p, 
              model = "random", 
              random.method = "walhus",
              random.dfcor = 3)

phtest(m11_lf, m11_lr)

# coef(summary(m1_lf))
clustse11_lf <- coeftest(m11_lf, vcovHC(m11_lf, type = "HC0", cluster = "group"))
clustse11_lf
clustse11_lr <- coeftest(m11_lr, vcovHC(m11_lr, type = "HC0", cluster = "group"))
clustse11_lr
# pvals <- clustse2[, 4]
# ses <- clustse2[, 2]
stargazer(m11_lf, m11_lr,
          omit = "YEAR_DATA",
          # p = pvals,
          # se = ses,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text")


## xtreg replacement_rateadj_minister domestic3_10 L.gdp_cap_pwt_ln L.pop_pwt_ln L.military_c L.monarchy_c L.party_c i.YEAR_DATA if democracy1==0 & independent==1, fe cluster(COUNTRY_ID)
# strikes_data_lfe.p <- pdata.frame(strikes_data_lf, index = c("leader_cab_id", "YEAR_DATA"))
length(unique(strikes_data_lfe.p$COUNTRY_ID)) ## 115 (original: 114)
## how balanced are the data
punbalancedness(strikes_data_lfe.p)
m12_lf_model <- as.formula(paste(dv, " ~ ", paste0(c(strikes, base, main), collapse = " + ")))
m12_lf <- plm(m12_lf_model,
              data = strikes_data_lfe.p, 
              model = "within", effect = "twoways")
m12_lr <- plm(m12_lf_model,
              data = strikes_data_lfe.p, 
              model = "random", 
              random.method = "walhus",
              random.dfcor = 3)

phtest(m12_lf, m12_lr)

# coef(summary(m1_lf))
clustse12_lf <- coeftest(m12_lf, vcovHC(m12_lf, type = "HC0", cluster = "group"))
clustse12_lf
clustse12_lr <- coeftest(m12_lr, vcovHC(m12_lr, type = "HC0", cluster = "group"))
clustse12_lr
# pvals <- clustse2[, 4]
# ses <- clustse2[, 2]
stargazer(m12_lf, m12_lr,
          omit = "YEAR_DATA",
          # p = pvals,
          # se = ses,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text")


## final table
screenreg(list(m8_lf, m9_lf, m10_lf, m11_lf, m12_lf),
          omit.coef = "YEAR_DATA",
          digits = 5)
## get pvalues
m8lf <- extract(m8_lf)
m9lf <- extract(m9_lf)
m10lf <- extract(m10_lf)
m11lf <- extract(m11_lf)
m12lf <- extract(m12_lf)

m8lf
clustse8_lf
m9lf
clustse9_lf
m10lf
clustse10_lf
m11lf
clustse11_lf
m12lf
clustse12_lf

screenreg(list(m8_lr, m9_lr, m10_lr, m11_lr, m12_lr),
          omit.coef = "YEAR_DATA",
          digits = 5)
## get pvalues
m8lr <- extract(m8_lr)
m9lr <- extract(m9_lr)
m10lr <- extract(m10_lr)
m11lr <- extract(m11_lr)
m12lr <- extract(m12_lr)

m8lr
clustse8_lr
m9lr
clustse9_lr
m10lr
clustse10_lr
m11lr
clustse11_lr
m12lr
clustse12_lr


covlabels <- c("Failed coup attempt",
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
               "Strike")
collabels <- c("Base Model",
               "Main Model",
               "Electoral Controls",
               "Economic Controls",
               "Instability Controls",
               "Purges MAs",
               "All Controls")

## main iv: failed coup attempt
## country fixed effects
stargazer(m1_cf, m2_cf, m3_cf, m4_cf, m5_cf, m6_cf, m7_cf,
          se = list(clustse1_cf[,2], clustse2_cf[,2], clustse3_cf[,2], clustse4_cf[,2], clustse5_cf[,2], clustse6_cf[,2], clustse7_cf[,2]),
          p = list(clustse1_cf[,4], clustse2_cf[,4], clustse3_cf[,4], clustse4_cf[,4], clustse5_cf[,4], clustse6_cf[,4], clustse7_cf[,4]),
          omit = c("YEAR_DATA", "replacement_rateadj_minister_300", "replacement_rateadj_minister_200", "replacement_rateadj_minister_100"),
          covariate.labels = covlabels,
          dep.var.labels = "Dependent variable: Cabinet replacement rate",
          column.labels = collabels,
          title = "Country- and year-fixed effects",
          model.numbers = F,
          df = F,
          # # p = pvals,
          # # se = ses,
          star.cutoffs = c(0.05, 0.01, 0.001),
          # keep.stat = c("n", "rsq"),
          type = "latex",
          # omit.table.layout = "s",
          # colnames = F,
          style = "ajps",
          font.size = "scriptsize",
          float.env = "sidewaystable",
          out = "tables/regressiontable_cf.tex")
## country random effects
stargazer(m1_cr, m2_cr, m3_cr, m4_cr, m5_cr, m6_cr, m7_cr,
          se = list(clustse1_cr[,2], clustse2_cr[,2], clustse3_cr[,2], clustse4_cr[,2], clustse5_cr[,2], clustse6_cr[,2], clustse7_cr[,2]),
          p = list(clustse1_cr[,4], clustse2_cr[,4], clustse3_cr[,4], clustse4_cr[,4], clustse5_cr[,4], clustse6_cr[,4], clustse7_cr[,4]),
          omit = c("YEAR_DATA", "replacement_rateadj_minister_300", "replacement_rateadj_minister_200", "replacement_rateadj_minister_100"),
          covariate.labels = covlabels,
          dep.var.labels = "Dependent variable: Cabinet replacement rate",
          column.labels = collabels,
          title = "Country-random and year-fixed effects",
          model.numbers = F,
          df = F,
          # p = pvals,
          # se = ses,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "latex",
          # omit.table.layout = "s",
          colnames = F,
          style = "ajps",
          font.size = "scriptsize",
          float.env = "sidewaystable",
          out = "tables/regressiontable_cr.tex")
## leader fixed effects
stargazer(m1_lf, m2_lf, m3_lf, m4_lf, m5_lf, m6_lf, m7_lf,
          se = list(clustse1_lf[,2], clustse2_lf[,2], clustse3_lf[,2], clustse4_lf[,2], clustse5_lf[,2], clustse6_lf[,2], clustse7_lf[,2]),
          p = list(clustse1_lf[,4], clustse2_lf[,4], clustse3_lf[,4], clustse4_lf[,4], clustse5_lf[,4], clustse6_lf[,4], clustse7_lf[,4]),
          omit = c("YEAR_DATA", "replacement_rateadj_minister_300", "replacement_rateadj_minister_200", "replacement_rateadj_minister_100"),
          covariate.labels = covlabels,
          dep.var.labels = "Dependent variable: Cabinet replacement rate",
          column.labels = collabels,
          title = "Leader- and year-fixed effects",
          model.numbers = F,
          df = F,
          # p = pvals,
          # se = ses,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "latex",
          # omit.table.layout = "s",
          colnames = F,
          style = "ajps",
          font.size = "scriptsize",
          float.env = "sidewaystable",
          out = "tables/regressiontable_lf.tex")
## leader random effects
stargazer(m1_lr, m2_lr, m3_lr, m4_lr, m5_lr, m6_lr, m7_lr,
          se = list(clustse1_lr[,2], clustse2_lr[,2], clustse3_lr[,2], clustse4_lr[,2], clustse5_lr[,2], clustse6_lr[,2], clustse7_lr[,2]),
          p = list(clustse1_lr[,4], clustse2_lr[,4], clustse3_lr[,4], clustse4_lr[,4], clustse5_lr[,4], clustse6_lr[,4], clustse7_lr[,4]),
          omit = c("YEAR_DATA", "replacement_rateadj_minister_300", "replacement_rateadj_minister_200", "replacement_rateadj_minister_100"),
          covariate.labels = covlabels,
          dep.var.labels = "Dependent variable: Cabinet replacement rate",
          column.labels = collabels,
          title = "Leader-random and year-fixed effects",
          model.numbers = F,
          df = F,
          # p = pvals,
          # se = ses,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "latex",
          # omit.table.layout = "s",
          colnames = F,
          style = "ajps",
          font.size = "scriptsize",
          float.env = "sidewaystable",
          out = "tables/regressiontable_lr.tex")


covlabels_aiv <- c("Alternative event: Civil war",
                   "Alternative event: Nonviolent campaigns",
                   "Alternative event: Military disputes",
                   "Alternative event: Interstate war",
                   "Alternative event: Strikes",
                   "Log of GDP per capita",
                   "Log of Population",
                   "Monarchy",
                   "Military regime",
                   "Party regime")


## main iv: alternative iv's
## country fixed effects
stargazer(m8_cf, m9_cf, m10_cf, m11_cf, m12_cf,
          se = list(clustse8_cf[,2], clustse9_cf[,2], clustse10_cf[,2], clustse11_cf[,2], clustse12_cf[,2]),
          p = list(clustse8_cf[,4], clustse9_cf[,4], clustse10_cf[,4], clustse11_cf[,4], clustse12_cf[,4]),
          omit = "YEAR_DATA",
          covariate.labels = covlabels_aiv,
          dep.var.labels = "Dependent variable: Cabinet replacement rate",
          title = "Country- and year-fixed effects",
          model.numbers = F,
          df = F,
          # p = pvals,
          # se = ses,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "latex",
          # omit.table.layout = "s",
          colnames = F,
          style = "ajps",
          font.size = "scriptsize",
          float.env = "sidewaystable",
          out = "tables/regressiontable_aiv_cf.tex")
## country random effects
stargazer(m8_cr, m9_cr, m10_cr, m11_cr, m12_cr,
          se = list(clustse8_cr[,2], clustse9_cr[,2], clustse10_cr[,2], clustse11_cr[,2], clustse12_cr[,2]),
          p = list(clustse8_cr[,4], clustse9_cr[,4], clustse10_cr[,4], clustse11_cr[,4], clustse12_cr[,4]),
          omit = "YEAR_DATA",
          covariate.labels = covlabels_aiv,
          dep.var.labels = "Dependent variable: Cabinet replacement rate",
          title = "Country-random and year-fixed effects",
          model.numbers = F,
          df = F,
          # p = pvals,
          # se = ses,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "latex",
          # omit.table.layout = "s",
          colnames = F,
          style = "ajps",
          font.size = "scriptsize",
          float.env = "sidewaystable",
          out = "tables/regressiontable_aiv_cr.tex")
## leader fixed effects
stargazer(m8_lf, m9_lf, m10_lf, m11_lf, m12_lf,
          se = list(clustse8_lf[,2], clustse9_lf[,2], clustse10_lf[,2], clustse11_lf[,2], clustse12_lf[,2]),
          p = list(clustse8_lf[,4], clustse9_lf[,4], clustse10_lf[,4], clustse11_lf[,4], clustse12_lf[,4]),
          omit = "YEAR_DATA",
          covariate.labels = covlabels_aiv,
          dep.var.labels = "Dependent variable: Cabinet replacement rate",
          title = "Leader- and year-fixed effects",
          model.numbers = F,
          df = F,
          # p = pvals,
          # se = ses,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "latex",
          # omit.table.layout = "s",
          colnames = F,
          style = "ajps",
          font.size = "scriptsize",
          float.env = "sidewaystable",
          out = "tables/regressiontable_aiv_lf.tex")
## leader random effects
stargazer(m8_lr, m9_lr, m10_lr, m11_lr, m12_lr,
          se = list(clustse8_lr[,2], clustse9_lr[,2], clustse10_lr[,2], clustse11_lr[,2], clustse12_lr[,2]),
          p = list(clustse8_lr[,4], clustse9_lr[,4], clustse10_lr[,4], clustse11_lr[,4], clustse12_lr[,4]),
          omit = "YEAR_DATA",
          covariate.labels = covlabels_aiv,
          dep.var.labels = "Dependent variable: Cabinet replacement rate",
          title = "Leader-random and year-fixed effects",
          model.numbers = F,
          df = F,
          # p = pvals,
          # se = ses,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "latex",
          # omit.table.layout = "s",
          colnames = F,
          style = "ajps",
          font.size = "scriptsize",
          float.env = "sidewaystable",
          out = "tables/regressiontable_aiv_lr.tex")



## replicate figure 2
## inspiration: https://felixhaass.github.io/dataviz_ggplot2/session4.html


## clustered standard errors
## part a
## fixed effects
m1_cf_tidy <- broom::tidy(clustse1_cf) %>%
  mutate(model = "Country FE")
m1_lf_tidy <- broom::tidy(clustse1_lf) %>%
  mutate(model = "Leader FE")
m2_cf_tidy <- broom::tidy(clustse2_cf) %>%
  mutate(model = "Country FE")
m2_lf_tidy <- broom::tidy(clustse2_lf) %>%
  mutate(model = "Leader FE")
m3_cf_tidy <- broom::tidy(clustse3_cf) %>%
  mutate(model = "Country FE")
m3_lf_tidy <- broom::tidy(clustse3_lf) %>%
  mutate(model = "Leader FE")
m4_cf_tidy <- broom::tidy(clustse4_cf) %>%
  mutate(model = "Country FE")
m4_lf_tidy <- broom::tidy(clustse4_lf) %>%
  mutate(model = "Leader FE")
m5_cf_tidy <- broom::tidy(clustse5_cf) %>%
  mutate(model = "Country FE")
m5_lf_tidy <- broom::tidy(clustse5_lf) %>%
  mutate(model = "Leader FE")
m6_cf_tidy <- broom::tidy(clustse6_cf) %>%
  mutate(model = "Country FE")
m6_lf_tidy <- broom::tidy(clustse6_lf) %>%
  mutate(model = "Leader FE")
m7_cf_tidy <- broom::tidy(clustse7_cf) %>%
  mutate(model = "Country FE")
m7_lf_tidy <- broom::tidy(clustse7_lf) %>%
  mutate(model = "Leader FE")

## random effects
m1_cr_tidy <- broom::tidy(clustse1_cr) %>%
  mutate(model = "Country RE")
m1_lr_tidy <- broom::tidy(clustse1_lr) %>%
  mutate(model = "Leader RE")
m2_cr_tidy <- broom::tidy(clustse2_cr) %>%
  mutate(model = "Country RE")
m2_lr_tidy <- broom::tidy(clustse2_lr) %>%
  mutate(model = "Leader RE")
m3_cr_tidy <- broom::tidy(clustse3_cr) %>%
  mutate(model = "Country RE")
m3_lr_tidy <- broom::tidy(clustse3_lr) %>%
  mutate(model = "Leader RE")
m4_cr_tidy <- broom::tidy(clustse4_cr) %>%
  mutate(model = "Country RE")
m4_lr_tidy <- broom::tidy(clustse4_lr) %>%
  mutate(model = "Leader RE")
m5_cr_tidy <- broom::tidy(clustse5_cr) %>%
  mutate(model = "Country RE")
m5_lr_tidy <- broom::tidy(clustse5_lr) %>%
  mutate(model = "Leader RE")
m6_cr_tidy <- broom::tidy(clustse6_cr) %>%
  mutate(model = "Country RE")
m6_lr_tidy <- broom::tidy(clustse6_lr) %>%
  mutate(model = "Leader RE")
m7_cr_tidy <- broom::tidy(clustse7_cr) %>%
  mutate(model = "Country RE")
m7_lr_tidy <- broom::tidy(clustse7_lr) %>%
  mutate(model = "Leader RE")

all_models_a <- bind_rows(m1_cf_tidy, m1_lf_tidy,
                          m2_cf_tidy, m2_lf_tidy,
                          m3_cf_tidy, m3_lf_tidy,
                          m4_cf_tidy, m4_lf_tidy,
                          m5_cf_tidy, m5_lf_tidy,
                          m6_cf_tidy, m6_lf_tidy,
                          m7_cf_tidy, m7_lf_tidy,
                          m1_cr_tidy, m1_lr_tidy,
                          m2_cr_tidy, m2_lr_tidy,
                          m3_cr_tidy, m3_lr_tidy,
                          m4_cr_tidy, m4_lr_tidy,
                          m5_cr_tidy, m5_lr_tidy,
                          m6_cr_tidy, m6_lr_tidy,
                          m7_cr_tidy, m7_lr_tidy
)

all_models_a <- all_models_a %>% 
  filter(grepl("coupattempt_whogov", term))
all_models_a
## rename the terms according to models to match original plot
## fixed effects
all_models_a$term[1] <- "Base models"
all_models_a$term[2] <- "Base models"
all_models_a$term[3] <- "Main models"
all_models_a$term[4] <- "Main models"
all_models_a$term[5] <- "Electoral controls"
all_models_a$term[6] <- "Electoral controls"
all_models_a$term[7] <- "Economic controls"
all_models_a$term[8] <- "Economic controls"
all_models_a$term[9] <- "Instability controls"
all_models_a$term[10] <- "Instability controls"
all_models_a$term[11] <- "Purges MA"
all_models_a$term[12] <- "Purges MA"
all_models_a$term[13] <- "All controls"
all_models_a$term[14] <- "All controls"
## random effects
all_models_a$term[15] <- "Base models"
all_models_a$term[16] <- "Base models"
all_models_a$term[17] <- "Main models"
all_models_a$term[18] <- "Main models"
all_models_a$term[19] <- "Electoral controls"
all_models_a$term[20] <- "Electoral controls"
all_models_a$term[21] <- "Economic controls"
all_models_a$term[22] <- "Economic controls"
all_models_a$term[23] <- "Instability controls"
all_models_a$term[24] <- "Instability controls"
all_models_a$term[25] <- "Purges MA"
all_models_a$term[26] <- "Purges MA"
all_models_a$term[27] <- "All controls"
all_models_a$term[28] <- "All controls"

all_models_a

coefplot_a <- dotwhisker::dwplot(all_models_a,
                                 dot_args = list(aes(colour = model)), 
                                 size = 3) +
  theme_light() +
  labs(title = "A", 
       x = "Coefficient Estimate with 95% CIs", 
       y = "") +
  scale_x_continuous(limits = c(-0.07, 0.2), 
                     breaks = c(-0.05, 0, 0.05, 0.1, 0.15, 0.2),
                     labels = c(-0.05, 0, 0.05, 0.1, 0.15, 0.2)) + 
  geom_vline(xintercept = 0, linetype = "dotted") + 
  theme(plot.title = element_text(face="bold"),
        legend.position = "bottom",
        legend.justification = "left",
        legend.background = element_rect(colour="grey80"),
        legend.title.align = .5) +
  scale_shape_discrete(name  ="Models", breaks = c(0, 1)) + # breaks assign shapes
  scale_colour_grey(start = .7, end = .3, name = "Models") # start/end for light/dark greys


## part b
## fixed effects
## clustered standard errors
m8_cf_tidy <- broom::tidy(clustse8_cf) %>%
  mutate(model = "Country FE")
m8_lf_tidy <- broom::tidy(clustse8_lf) %>%
  mutate(model = "Leader FE")
m9_cf_tidy <- broom::tidy(clustse9_cf) %>%
  mutate(model = "Country FE")
m9_lf_tidy <- broom::tidy(clustse9_lf) %>%
  mutate(model = "Leader FE")
m10_cf_tidy <- broom::tidy(clustse10_cf) %>%
  mutate(model = "Country FE")
m10_lf_tidy <- broom::tidy(clustse10_lf) %>%
  mutate(model = "Leader FE")
m11_cf_tidy <- broom::tidy(clustse11_cf) %>%
  mutate(model = "Country FE")
m11_lf_tidy <- broom::tidy(clustse11_lf) %>%
  mutate(model = "Leader FE")
m12_cf_tidy <- broom::tidy(clustse12_cf) %>%
  mutate(model = "Country FE")
m12_lf_tidy <- broom::tidy(clustse12_lf) %>%
  mutate(model = "Leader FE")

## random effects
m8_cr_tidy <- broom::tidy(clustse8_cr) %>%
  mutate(model = "Country RE")
m8_lr_tidy <- broom::tidy(clustse8_lr) %>%
  mutate(model = "Leader RE")
m9_cr_tidy <- broom::tidy(clustse9_cr) %>%
  mutate(model = "Country RE")
m9_lr_tidy <- broom::tidy(clustse9_lr) %>%
  mutate(model = "Leader RE")
m10_cr_tidy <- broom::tidy(clustse10_cr) %>%
  mutate(model = "Country RE")
m10_lr_tidy <- broom::tidy(clustse10_lr) %>%
  mutate(model = "Leader RE")
m11_cr_tidy <- broom::tidy(clustse11_cr) %>%
  mutate(model = "Country RE")
m11_lr_tidy <- broom::tidy(clustse11_lr) %>%
  mutate(model = "Leader RE")
m12_cr_tidy <- broom::tidy(clustse12_cr) %>%
  mutate(model = "Country RE")
m12_lr_tidy <- broom::tidy(clustse12_lr) %>%
  mutate(model = "Leader RE")

all_models_b <- bind_rows(m8_cf_tidy, m8_lf_tidy,
                          m9_cf_tidy, m9_lf_tidy,
                          m10_cf_tidy, m10_lf_tidy,
                          m11_cf_tidy, m11_lf_tidy,
                          m12_cf_tidy, m12_lf_tidy,
                          m8_cr_tidy, m8_lr_tidy,
                          m9_cr_tidy, m9_lr_tidy,
                          m10_cr_tidy, m10_lr_tidy,
                          m11_cr_tidy, m11_lr_tidy,
                          m12_cr_tidy, m12_lr_tidy
)

all_models_b <- all_models_b %>% 
  filter(term %in% c("onset1", "nonvio_camp_2", "mid_onset", "war_onset", "domestic3_10"))
all_models_b
## rename the terms according to models to match original plot
## fixed effects
all_models_b$term[1] <- "Civil war"
all_models_b$term[2] <- "Civil war"
all_models_b$term[3] <- "Nonviolent campaigns"
all_models_b$term[4] <- "Nonviolent campaigns"
all_models_b$term[5] <- "Military disputes"
all_models_b$term[6] <- "Military disputes"
all_models_b$term[7] <- "Interstate war"
all_models_b$term[8] <- "Interstate war"
all_models_b$term[9] <- "Strikes"
all_models_b$term[10] <- "Strikes"
## random effects
all_models_b$term[11] <- "Civil war"
all_models_b$term[12] <- "Civil war"
all_models_b$term[13] <- "Nonviolent campaigns"
all_models_b$term[14] <- "Nonviolent campaigns"
all_models_b$term[15] <- "Military disputes"
all_models_b$term[16] <- "Military disputes"
all_models_b$term[17] <- "Interstate war"
all_models_b$term[18] <- "Interstate war"
all_models_b$term[19] <- "Strikes"
all_models_b$term[20] <- "Strikes"

all_models_b

coefplot_b <- dotwhisker::dwplot(all_models_b,
                                 dot_args = list(aes(colour = model)), 
                                 size = 3) +
  theme_light() +
  labs(title = "B", 
       x = "Coefficient Estimate with 95% CIs", 
       y = "") +
  scale_x_continuous(limits = c(-0.07, 0.2), 
                     breaks = c(-0.05, 0, 0.05, 0.1, 0.15, 0.2),
                     labels = c(-0.05, 0, 0.05, 0.1, 0.15, 0.2)) + 
  geom_vline(xintercept = 0, linetype = "dotted") + 
  theme(plot.title = element_text(face="bold"),
        legend.position = "bottom",
        legend.justification = "left",
        legend.background = element_rect(colour="grey80"),
        legend.title.align = .5) +
  scale_shape_discrete(name  ="Models", breaks = c(0, 1)) + # breaks assign shapes
  scale_colour_grey(start = .7, end = .3, name = "Models") # start/end for light/dark greys

coefplot_original <- gridExtra::grid.arrange(coefplot_a, coefplot_b,
                                             nrow = 1)


ggsave("plots/coefplot_fixed_vs_random_effects.png", width = 13, height = 5, dpi = 500, coefplot_original)
