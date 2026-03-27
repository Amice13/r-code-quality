#### purpose: producing table r11 #### 

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
    library(knitr)
    
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

#### cmps 16 --- ruling out alternative explanations (full) #### 

outs = c("supp_blm", "blm_eff")
forms = 
  c(
    'acc2 * threat',
    'acc2 * threat + acc2 * pd_latinx + acc2 * ed + acc2 * lf_lat + acc2 * skin + 
    acc2 * pue + acc2 * pue_cty + 
      acc2 * neigh_blk + acc2 * neigh_blk_miss + acc2 * pbl + acc2 * pbl_cty + acc2 * miss_fips +  
      acc2 * polint + acc2 * edu + 
      acc2 * know_undoc + acc2 * ltotal + acc2 * dep_rate + acc2 * pfb + acc2 * pnc + acc2 * amer_cent + 
      acc2 * pi_gop2 + acc2 * pi_dem2 + pcol_lat_adv * pbl * acc2 + ppov_lat_adv * pbl * acc2 + 
      wom + acc2 + skin + age + mar + cath + 
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

texreg(l = list(outs_list[[1]][[2]], outs_list[[2]][[2]]),
       include.ci = FALSE,
       custom.coef.map = list("acc2:threat" = "Acculturation x Threat",
                              "acc2" = "Acculturation",
                              "threat" = "Threat"),
       custom.model.names = c("BLM Support", "BLM Effective"),
       stars = c(.1, .05, .01, .001),
       symbol = "\\dagger",
       float.pos = "!htbp")

#### cmps 20 --- ruling out alternative explanations #### 

outcomes2l = 
  c("rr", "black_stereotype_diff",
    "black_threat", "white_hood_rank_diff", "blm_ft",
    "blm_supports", "blm_eff", "blm_protest", "blm_nprotest")

# fix for consistency 

cmps20l$pd_latinx = cmps20l$pd_lat
cmps20l$pd_latinx = 
  cmps20l$pd_latinx / max(cmps20l$pd_latinx, na.rm = TRUE)
cmps20l$pd_latinx_ref = cmps20l$pd_lat_ref
cmps20l$ed = cmps20l$exp_disc
cmps20l$pbl = cmps20l$pblk
cmps20l$pbl = cmps20l$pbl / max(cmps20l$pbl, na.rm = TRUE)
cmps20l$pbl_cty = cmps20l$pblk_cty
cmps20l$pbl_cty = cmps20l$pbl_cty / max(cmps20l$pbl_cty, na.rm = TRUE)
cmps20l$polint = cmps20l$polint / max(cmps20l$polint, na.rm = TRUE)
cmps20l$edu = cmps20l$edu / max(cmps20l$edu, na.rm = TRUE)
cmps20l$ltotal = cmps20l$ltotal / max(cmps20l$ltotal, na.rm = TRUE)
cmps20l$pfb = cmps20l$pfb / max(cmps20l$pfb, na.rm = TRUE)
cmps20l$pnc = cmps20l$pnc / max(cmps20l$pnc, na.rm = TRUE)
cmps20l$amer_cent = cmps20l$amer_id / max(cmps20l$amer_id, na.rm = TRUE)
cmps20l$pfb_cty = cmps20l$pfb_cty / max(cmps20l$pfb_cty, na.rm = TRUE)
cmps20l$skin = cmps20l$skin / max(cmps20l$skin, na.rm = TRUE)
cmps20l$econ_afraid_pers = cmps20l$econ_afraid_pers / max(cmps20l$econ_afraid_pers, na.rm = TRUE)
cmps20l$econ_afraid_socio = cmps20l$econ_afraid_socio / max(cmps20l$econ_afraid_socio, na.rm = TRUE)
cmps20l$econ_grp_worse = cmps20l$econ_grp_worse / max(cmps20l$econ_grp_worse, na.rm = TRUE)
cmps20l$pue = cmps20l$pune
cmps20l$pue_cty = cmps20l$pune_cty
cmps20l$pue = cmps20l$pue / max(cmps20l$pue, na.rm = TRUE)
cmps20l$pue_cty = cmps20l$pue_cty / max(cmps20l$pue_cty, na.rm = TRUE)

# formulas 

form_list_20 = list(
  '~acc2 * threat',
  '~acc2 * threat + acc2 * skin + acc2 * ed + acc2 * pd_latinx + acc2 * pd_latinx_ref + acc2 * pbl + acc2 * pbl_cty + 
  acc2 * miss_zip + acc2 * miss_cty + acc2 * edu + acc2 * know_undoc + acc2 * ltotal + acc2 * pfb + acc2 * pnc + acc2 * pfb_cty + acc2 * pnc_cty + acc2 * amer_cent + 
  acc2 * polint + acc2 * pi_gop + acc2 * pi_dem + acc2 * lf_lat + acc2 * skin + acc2 * pue + acc2 * pue_cty + acc2 * imm_work_ethic +
    wom + skin + age + mar + cath + no_mx + 
    no_pr + no_dr + no_cb + no_es + inc + inc_ref + edu + 
    unemp + ownhome + econ_afraid_pers + econ_afraid_socio + econ_grp_worse + 
    ed + pd_latinx + pd_latinx_ref + pd_blk + pd_blk_ref + 
    pid7 + ide + ide_ref + polcomp + id_cent + amer_cent + 
    know_undoc + know_deport + polint + lf_lat + pct_blk_perc + 
    ltpop + plat + pbl + lmhhi + pue + pcol + ltpop_cty + plat_cty + pbl_cty + pfb_cty + lmhhi_cty + 
    pue_cty + dep_rate + ltotal + dep_rate + ppov_lat_adv * pbl + 
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
                subset = black_lat == 0,
                se_type = "stata")
    
    
  }
  
  form_list_out[[i]] = outcomes_2l_list
  
}

texreg(l = list(form_list_out[[2]][[1]], form_list_out[[2]][[2]],
                form_list_out[[2]][[3]], form_list_out[[2]][[4]]),
       include.ci = FALSE,
       custom.coef.map = list("acc2:threat" = "Acculturation x Threat",
                              "acc2" = "Acculturation",
                              "threat" = "Threat"),
       custom.model.names = c("Racial Resentment", "Anti-Black Stereotype",
                              "Black = Threat", "Prefer White Residence"),
       stars = c(.1, .05, .01, .001),
       symbol = "\\dagger",
       float.pos = "!htbp",
       include.adjrs = FALSE,
       include.rmse = FALSE,
       label = "table:rulealtexp1")

texreg(l = list(outs_list[[1]][[2]], outs_list[[2]][[2]], form_list_out[[2]][[5]],
                form_list_out[[2]][[6]], form_list_out[[2]][[7]], form_list_out[[2]][[8]],
                form_list_out[[2]][[9]]),
       include.ci = FALSE,
       custom.coef.map = list("acc2:threat" = "Acculturation x Threat",
                              "acc2" = "Acculturation",
                              "threat" = "Threat"),
       custom.model.names = c("Oppose BLM", "BLM Ineffective",
                              "Anti-BLM FT", "Oppose BLM", "BLM Ineffective", 
                              "BLM No Protest", "BLM No Support"),
       stars = c(.1, .05, .01, .001),
       symbol = "\\dagger",
       float.pos = "!htbp",
       include.adjrs = FALSE,
       include.rmse = FALSE,
       label = "table:rulealtexp2")
