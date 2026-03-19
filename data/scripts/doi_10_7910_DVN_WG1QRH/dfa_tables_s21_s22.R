#### purpose: producing tables s16, s17 #### 

#### installing key packages #### 

list.of.packages = 
  c('readstata13', 'haven', 'tidyverse', 'dplyr', 'estimatr', 'texreg',
    'gridExtra', 'ggthemes', 'wCorr', 'questionr', 'xtable', 'sf', 
    'TAM', 'purrr', 'kable', 'kableExtra', 'wCorr', 'psych',
    'psychTools')
new.packages =  list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#### libraries #### 

suppressPackageStartupMessages(
  
  {
    
    library(readstata13)    
    library(haven)
    library(tidyverse)
    library(dplyr)
    library(estimatr)
    library(texreg)
    library(gridExtra)
    library(ggthemes)
    library(wCorr)
    library(questionr)
    library(xtable)
    library(sf)
    library(TAM)
    library(purrr)
    library(kable)
    library(kableExtra)
    library(wCorr)
    library(psych)
    library(psychTools)
    
  }
  
)

#### cmps '16 --- loading datasets #### 

load(file = "cmps_lat.RData")
load(file = "cmps_wht.RData")
load(file = "cmps_blk.RData")

#### cmps '20 --- loading datasets #### 

load(file = "cmps20w_clean.RData")
load(file = "cmps20b_clean.RData")
load(file = "cmps20l_clean.RData")
# mean(cmps20w$blm_ft)
# mean(cmps20b$blm_ft)
# mean(cmps20l$blm_ft)

# quick fix for missingness 

cmps20l$cath = ifelse(is.na(cmps20l$cath), 0, cmps20l$cath)

# another quick fix on dtp

cmps20l$dtps2 = (cmps20l$dtp + cmps20l$dtp2 + cmps20l$dtp3) / 3
cmps20w$dtps2 = (cmps20w$dtp + cmps20w$dtp2 + cmps20w$dtp3) / 3
cmps20b$dtps2 = (cmps20b$dtp + cmps20b$dtp2 + cmps20b$dtp3) / 3

#### more fixes #### 

# 2016 fixes --- recode so that outcomes = 1) oppose BLM and 2) belief BLM ineffective

cmps16$supp_blm  = 1 - cmps16$supp_blm 
cmps16$blm_eff = 1 - cmps16$blm_eff 

cmps16b$supp_blm  = 1 - cmps16b$supp_blm 
cmps16b$blm_eff = 1 - cmps16b$blm_eff

cmps16w$supp_blm  = 1 - cmps16w$supp_blm 

# 2020 fixes --- recode so 1 = anti-black 

# cmps20l$blm_supports = 1 - cmps20l$blm_supports
# cmps20l$blm_eff = 1 - cmps20l$blm_eff
# cmps20l$white_hood_rank_diff = cmps20l$white_hood_rank - cmps20l$black_hood_rank
# cmps20l$dtp = 1 - cmps20l$dtp
# cmps20l$dtp3 = 1 - cmps20l$dtp3
# cmps20l$dtps = (cmps20l$dtp + cmps20l$dtp3) / 2
# cmps20l$polint = abs(cmps20l$Q29 - 4)
# cmps20l$lmhhi = log(cmps20l$mhhi + 1)
# cmps20l$lmhhi_cty = log(cmps20l$mhhi_cty + 1)
# cmps20l$dep_rate = (cmps20l$total / ((cmps20l$pfb_cty / 100) * exp(cmps20l$ltpop_cty))) * 1000
cmps16$threat = cmps16$worry
cmps16$ltpop = log(cmps16$tpop + 1)
cmps16$lmhhi = log(cmps16$mhhi + 1)

cmps20l$threat2 = cmps20l$threat2 / max(cmps20l$threat2, na.rm = TRUE)
cmps20l$threat = cmps20l$threat2

#### re-analysis, factorize acculturation ####

outs = c("supp_blm", "blm_eff")
forms = 
  c(
    'factor(acc2) * threat',
    'factor(acc2) * threat + wom + acc2 + skin + age + mar + cath + 
      no_mx + no_pr + no_dr + no_cb + no_es + 
      inc + inc_ref + edu + unemp + ownhome + econ_worse + 
      ed + pd_latinx + pd_black + pi_dem2 + pi_gop2 + ide + ide_miss + polcomp +
      id_cent + amer_cent + know_undoc + polint + lf_lat + neigh_blk + neigh_blk_miss +
      ltpop + plt + pbl + pfb + pue + lmhhi + ltpop_cty + plt_cty + pbl_cty + pfb_cty + lmhhi_cty + 
      pue_cty + dep_rate + ltotal + miss_fips + pcol_lat_adv * pbl + ppov_lat_adv * pbl + 
      ppov_lat_adv * pbl + pcol_lat_adv * pbl + pbl_church + miss_church + mar_black_spouse + 
      cenfe_ws + cenfe_nc + cenfe_st + miss_adv'
  )

outs_list = as.list(rep(NA, length(outs)))

for (i in 1:length(outs)) {
  
  forms_list = as.list(rep(NA, length(forms)))
  
  for (k in 1:length(forms)) {
    
    forms_list[[k]] = 
      lm_robust(as.formula(paste0(outs[i], "~", forms[k])),
                data = cmps16, weight = weight, subset = black_lat == 0)
    
  }
  
  outs_list[[i]] = forms_list
  
}

outcomes2l = 
  c("rr", "black_stereotype_diff",
    "black_threat", "white_hood_rank_diff", "blm_ft",
    "blm_supports", "blm_eff", "blm_protest", "blm_nprotest", "dtps2")

# formulas 

form_list_20 = list(
  '~factor(acc2) * threat',
  '~factor(acc2) * threat + wom + skin + age + mar + cath + no_mx + 
    no_pr + no_dr + no_cb + no_es + inc + inc_ref + edu + 
    unemp + ownhome + econ_afraid_pers + econ_afraid_socio + econ_grp_worse + 
    exp_disc + pd_lat + pd_lat_ref + pd_blk + pd_blk_ref + 
    pid7 + ide + ide_ref + polcomp + id_cent + amer_id + 
    know_undoc + know_deport + polint + lf_lat + pct_blk_perc + 
    ltpop + plat + pblk + lmhhi + pune + pcol + ltpop_cty + plat_cty + pblk_cty + pfb_cty + lmhhi_cty + 
    pune_cty + dep_rate + ltotal + dep_rate + ppov_lat_adv * pblk + 
    cenfe_ws + cenfe_nc + cenfe_st + miss_cty'  
)

# looping results

form_list_out = as.list(rep(NA, length(form_list_20)))

for (i in 1:length(form_list_20)) {
  
  print(paste0("iteration ", i))
  outcomes_2l_list = as.list(rep(NA, length(outcomes2l)))
  
  for (k in 1:length(outcomes2l)) {
    
    print(paste0("small iteration ", k))
    outcomes_2l_list[[k]] = 
      lm_robust(formula = as.formula(paste0(outcomes2l[k], form_list_20[[i]])),
                data = cmps20l, 
                weight = weight,
                se_type = "stata",
                subset = black_lat == 0)
    
    
  }
  
  form_list_out[[i]] = outcomes_2l_list
  
}

#### producing tables s21-22 ####

# anti-black attitudes --- yes controls

texreg(l = list(form_list_out[[2]][[1]],
                form_list_out[[2]][[2]],
                form_list_out[[2]][[3]],
                form_list_out[[2]][[4]]),
       include.ci = FALSE,
       float.pos = "!htbp",
       caption.above = TRUE,
       caption = "The threat of deportation undercuts the adoption of anti-Black attitudes via acculturation among non-Black Latinxs",
       include.rmse = FALSE,
       include.adjrs = FALSE,
       custom.coef.map = list("factor(acc2)0.25:threat" = "Acculturation (1) x Threat",
                              "factor(acc2)0.5:threat" = "Acculturation (2) x Threat",
                              "factor(acc2)0.75:threat" = "Acculturation (3) x Threat",
                              "factor(acc2)1:threat" = "Acculturation (4) x Threat",
                              "factor(acc2)0.25" = "Acculturation (1)",
                              "factor(acc2)0.5" = "Acculturation (2)",
                              "factor(acc2)0.75" = "Acculturation (3)",
                              "factor(acc2)1" = "Acculturation (4)",
                              "threat" = "Threat"),
       custom.model.names = c("Racial Resentment", "Anti-Black Stereotype", "Black = Threat", "Prefer White Residence"),
       stars = c(.1, .05, .01, .001),
       symbol = "\\dagger")

# opposition to black political interests --- yes controls

texreg(l = list(outs_list[[1]][[2]],
                outs_list[[2]][[2]],
                form_list_out[[2]][[5]],
                form_list_out[[2]][[6]],
                form_list_out[[2]][[7]],
                form_list_out[[2]][[8]],
                form_list_out[[2]][[9]],
                form_list_out[[2]][[10]]),
       include.ci = FALSE,
       float.pos = "!htbp",
       caption.above = TRUE,
       caption = "The threat of deportation undercuts the adoption of anti-Black attitudes via acculturation among non-Black Latinxs",
       include.rmse = FALSE,
       include.adjrs = FALSE,
       custom.coef.map = list("factor(acc2)0.25:threat" = "Acculturation (1) x Threat",
                              "factor(acc2)0.5:threat" = "Acculturation (2) x Threat",
                              "factor(acc2)0.75:threat" = "Acculturation (3) x Threat",
                              "factor(acc2)1:threat" = "Acculturation (4) x Threat",
                              "factor(acc2)0.25" = "Acculturation (1)",
                              "factor(acc2)0.5" = "Acculturation (2)",
                              "factor(acc2)0.75" = "Acculturation (3)",
                              "factor(acc2)1" = "Acculturation (4)",
                              "threat" = "Threat"),
       custom.model.names = c("Oppose BLM", "BLM Ineffective", "Anti-BLM FT",
                              "Oppose BLM", "BLM Ineffective", "BLM No Protest",
                              "BLM No Support", "Don't Abolish"),
       stars = c(.1, .05, .01, .001),
       symbol = "\\dagger")
