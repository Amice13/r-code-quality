#### purpose: producing tables s23 #### 

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

#### alternative acculturation specifications (table 23) ####

cmps16$eng = cmps16$english
cmps20l$second_gen = ifelse(cmps20l$genstat == 1, 1, 0)
cmps20l$acc = 
  ifelse(cmps20l$second_gen == 1, 1, 
         ifelse(cmps20l$third_gen == 1, 2, 0)) + cmps20l$eng

cmps16$acc = cmps16$acc / max(cmps16$acc, na.rm = TRUE)
cmps20l$acc = cmps20l$acc / max(cmps20l$acc, na.rm = TRUE)
outs = c("supp_blm", "blm_eff")
forms = 
  c(
    'usborn * threat + wom + acc2 + skin + age + mar + cath + 
      no_mx + no_pr + no_dr + no_cb + no_es + 
      inc + inc_ref + edu + unemp + ownhome + econ_worse + 
      ed + pd_latinx + pd_black + pi_dem2 + pi_gop2 + ide + ide_miss + polcomp +
      id_cent + amer_cent + know_undoc + polint + lf_lat + neigh_blk + neigh_blk_miss +
      ltpop + plt + pbl + pfb + pue + lmhhi + ltpop_cty + plt_cty + pbl_cty + pfb_cty + lmhhi_cty + 
      pue_cty + dep_rate + ltotal + miss_fips + pcol_lat_adv * pbl + ppov_lat_adv * pbl + 
      ppov_lat_adv * pbl + pcol_lat_adv * pbl + pbl_church + miss_church + mar_black_spouse + 
      cenfe_ws + cenfe_nc + cenfe_st + miss_adv',
    'cit * threat + wom + acc2 + skin + age + mar + cath + 
      no_mx + no_pr + no_dr + no_cb + no_es + 
      inc + inc_ref + edu + unemp + ownhome + econ_worse + 
      ed + pd_latinx + pd_black + pi_dem2 + pi_gop2 + ide + ide_miss + polcomp +
      id_cent + amer_cent + know_undoc + polint + lf_lat + neigh_blk + neigh_blk_miss +
      ltpop + plt + pbl + pfb + pue + lmhhi + ltpop_cty + plt_cty + pbl_cty + pfb_cty + lmhhi_cty + 
      pue_cty + dep_rate + ltotal + miss_fips + pcol_lat_adv * pbl + ppov_lat_adv * pbl + 
      ppov_lat_adv * pbl + pcol_lat_adv * pbl + pbl_church + miss_church + mar_black_spouse + 
      cenfe_ws + cenfe_nc + cenfe_st + miss_adv',
    'english * threat + wom + acc2 + skin + age + mar + cath + 
      no_mx + no_pr + no_dr + no_cb + no_es + 
      inc + inc_ref + edu + unemp + ownhome + econ_worse + 
      ed + pd_latinx + pd_black + pi_dem2 + pi_gop2 + ide + ide_miss + polcomp +
      id_cent + amer_cent + know_undoc + polint + lf_lat + neigh_blk + neigh_blk_miss +
      ltpop + plt + pbl + pfb + pue + lmhhi + ltpop_cty + plt_cty + pbl_cty + pfb_cty + lmhhi_cty + 
      pue_cty + dep_rate + ltotal + miss_fips + pcol_lat_adv * pbl + ppov_lat_adv * pbl + 
      ppov_lat_adv * pbl + pcol_lat_adv * pbl + pbl_church + miss_church + mar_black_spouse + 
      cenfe_ws + cenfe_nc + cenfe_st + miss_adv',
    'second_gen * threat + third_gen * threat + wom + acc2 + skin + age + mar + cath + 
      no_mx + no_pr + no_dr + no_cb + no_es + 
      inc + inc_ref + edu + unemp + ownhome + econ_worse + 
      ed + pd_latinx + pd_black + pi_dem2 + pi_gop2 + ide + ide_miss + polcomp +
      id_cent + amer_cent + know_undoc + polint + lf_lat + neigh_blk + neigh_blk_miss +
      ltpop + plt + pbl + pfb + pue + lmhhi + ltpop_cty + plt_cty + pbl_cty + pfb_cty + lmhhi_cty + 
      pue_cty + dep_rate + ltotal + miss_fips + pcol_lat_adv * pbl + ppov_lat_adv * pbl + 
      ppov_lat_adv * pbl + pcol_lat_adv * pbl + pbl_church + miss_church + mar_black_spouse + 
      cenfe_ws + cenfe_nc + cenfe_st + miss_adv',
    'acc * threat + wom + acc2 + skin + age + mar + cath + 
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
  '~usborn * threat + wom + skin + age + mar + cath + no_mx + 
    no_pr + no_dr + no_cb + no_es + inc + inc_ref + edu + 
    unemp + ownhome + econ_afraid_pers + econ_afraid_socio + econ_grp_worse + 
    exp_disc + pd_lat + pd_lat_ref + pd_blk + pd_blk_ref + 
    pid7 + ide + ide_ref + polcomp + id_cent + amer_id + 
    know_undoc + know_deport + polint + lf_lat + pct_blk_perc + 
    ltpop + plat + pblk + lmhhi + pune + pcol + ltpop_cty + plat_cty + pblk_cty + pfb_cty + lmhhi_cty + 
    pune_cty + dep_rate + ltotal + dep_rate + ppov_lat_adv * pblk + 
    cenfe_ws + cenfe_nc + cenfe_st + miss_cty'  ,
  '~cit * threat + wom + skin + age + mar + cath + no_mx + 
    no_pr + no_dr + no_cb + no_es + inc + inc_ref + edu + 
    unemp + ownhome + econ_afraid_pers + econ_afraid_socio + econ_grp_worse + 
    exp_disc + pd_lat + pd_lat_ref + pd_blk + pd_blk_ref + 
    pid7 + ide + ide_ref + polcomp + id_cent + amer_id + 
    know_undoc + know_deport + polint + lf_lat + pct_blk_perc + 
    ltpop + plat + pblk + lmhhi + pune + pcol + ltpop_cty + plat_cty + pblk_cty + pfb_cty + lmhhi_cty + 
    pune_cty + dep_rate + ltotal + dep_rate + ppov_lat_adv * pblk + 
    cenfe_ws + cenfe_nc + cenfe_st + miss_cty',
  '~eng * threat + wom + skin + age + mar + cath + no_mx + 
    no_pr + no_dr + no_cb + no_es + inc + inc_ref + edu + 
    unemp + ownhome + econ_afraid_pers + econ_afraid_socio + econ_grp_worse + 
    exp_disc + pd_lat + pd_lat_ref + pd_blk + pd_blk_ref + 
    pid7 + ide + ide_ref + polcomp + id_cent + amer_id + 
    know_undoc + know_deport + polint + lf_lat + pct_blk_perc + 
    ltpop + plat + pblk + lmhhi + pune + pcol + ltpop_cty + plat_cty + pblk_cty + pfb_cty + lmhhi_cty + 
    pune_cty + dep_rate + ltotal + dep_rate + ppov_lat_adv * pblk + 
    cenfe_ws + cenfe_nc + cenfe_st + miss_cty',
  '~second_gen * threat + third_gen * threat + wom + skin + age + mar + cath + no_mx + 
    no_pr + no_dr + no_cb + no_es + inc + inc_ref + edu + 
    unemp + ownhome + econ_afraid_pers + econ_afraid_socio + econ_grp_worse + 
    exp_disc + pd_lat + pd_lat_ref + pd_blk + pd_blk_ref + 
    pid7 + ide + ide_ref + polcomp + id_cent + amer_id + 
    know_undoc + know_deport + polint + lf_lat + pct_blk_perc + 
    ltpop + plat + pblk + lmhhi + pune + pcol + ltpop_cty + plat_cty + pblk_cty + pfb_cty + lmhhi_cty + 
    pune_cty + dep_rate + ltotal + dep_rate + ppov_lat_adv * pblk + 
    cenfe_ws + cenfe_nc + cenfe_st + miss_cty',
  '~acc * threat + wom + skin + age + mar + cath + no_mx + 
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


data.frame(
  quantity = rep(c("US-Born x Threat",
                   "Citizen x Threat",
                   "English x Threat",
                   "Second Gen x Threat",
                   "Third Gen x Threat",
                   "New Acculturation x Threat"), 12),
  
  est = 
    c(outs_list[[1]][[1]]$coefficients[grepl(x = names(outs_list[[1]][[1]]$coefficients), ":threat|threat:")],
      outs_list[[1]][[2]]$coefficients[grepl(x = names(outs_list[[1]][[2]]$coefficients), ":threat|threat:")],
      outs_list[[1]][[3]]$coefficients[grepl(x = names(outs_list[[1]][[3]]$coefficients), ":threat|threat:")],
      outs_list[[1]][[4]]$coefficients[grepl(x = names(outs_list[[1]][[4]]$coefficients), ":threat|threat:")],
      outs_list[[1]][[5]]$coefficients[grepl(x = names(outs_list[[1]][[5]]$coefficients), ":threat|threat:")],
      outs_list[[2]][[1]]$coefficients[grepl(x = names(outs_list[[2]][[1]]$coefficients), ":threat|threat:")],
      outs_list[[2]][[2]]$coefficients[grepl(x = names(outs_list[[2]][[2]]$coefficients), ":threat|threat:")],
      outs_list[[2]][[3]]$coefficients[grepl(x = names(outs_list[[2]][[3]]$coefficients), ":threat|threat:")],
      outs_list[[2]][[4]]$coefficients[grepl(x = names(outs_list[[2]][[4]]$coefficients), ":threat|threat:")],
      outs_list[[2]][[5]]$coefficients[grepl(x = names(outs_list[[2]][[5]]$coefficients), ":threat|threat:")],
      form_list_out[[1]][[1]]$coefficients[grepl(x = names(form_list_out[[1]][[1]]$coefficients), ":threat|threat:")],
      form_list_out[[2]][[1]]$coefficients[grepl(x = names(form_list_out[[2]][[1]]$coefficients), ":threat|threat:")],
      form_list_out[[3]][[1]]$coefficients[grepl(x = names(form_list_out[[3]][[1]]$coefficients), ":threat|threat:")],
      form_list_out[[4]][[1]]$coefficients[grepl(x = names(form_list_out[[4]][[1]]$coefficients), ":threat|threat:")],
      form_list_out[[5]][[1]]$coefficients[grepl(x = names(form_list_out[[5]][[1]]$coefficients), ":threat|threat:")],
      form_list_out[[1]][[2]]$coefficients[grepl(x = names(form_list_out[[1]][[2]]$coefficients), ":threat|threat:")],
      form_list_out[[2]][[2]]$coefficients[grepl(x = names(form_list_out[[2]][[2]]$coefficients), ":threat|threat:")],
      form_list_out[[3]][[2]]$coefficients[grepl(x = names(form_list_out[[3]][[2]]$coefficients), ":threat|threat:")],
      form_list_out[[4]][[2]]$coefficients[grepl(x = names(form_list_out[[4]][[2]]$coefficients), ":threat|threat:")],
      form_list_out[[5]][[2]]$coefficients[grepl(x = names(form_list_out[[5]][[2]]$coefficients), ":threat|threat:")],
      form_list_out[[1]][[3]]$coefficients[grepl(x = names(form_list_out[[1]][[3]]$coefficients), ":threat|threat:")],
      form_list_out[[2]][[3]]$coefficients[grepl(x = names(form_list_out[[2]][[3]]$coefficients), ":threat|threat:")],
      form_list_out[[3]][[3]]$coefficients[grepl(x = names(form_list_out[[3]][[3]]$coefficients), ":threat|threat:")],
      form_list_out[[4]][[3]]$coefficients[grepl(x = names(form_list_out[[4]][[3]]$coefficients), ":threat|threat:")],
      form_list_out[[5]][[3]]$coefficients[grepl(x = names(form_list_out[[5]][[3]]$coefficients), ":threat|threat:")],
      form_list_out[[1]][[4]]$coefficients[grepl(x = names(form_list_out[[1]][[4]]$coefficients), ":threat|threat:")],
      form_list_out[[2]][[4]]$coefficients[grepl(x = names(form_list_out[[2]][[4]]$coefficients), ":threat|threat:")],
      form_list_out[[3]][[4]]$coefficients[grepl(x = names(form_list_out[[3]][[4]]$coefficients), ":threat|threat:")],
      form_list_out[[4]][[4]]$coefficients[grepl(x = names(form_list_out[[4]][[4]]$coefficients), ":threat|threat:")],
      form_list_out[[5]][[4]]$coefficients[grepl(x = names(form_list_out[[5]][[4]]$coefficients), ":threat|threat:")],
      form_list_out[[1]][[5]]$coefficients[grepl(x = names(form_list_out[[1]][[5]]$coefficients), ":threat|threat:")],
      form_list_out[[2]][[5]]$coefficients[grepl(x = names(form_list_out[[2]][[5]]$coefficients), ":threat|threat:")],
      form_list_out[[3]][[5]]$coefficients[grepl(x = names(form_list_out[[3]][[5]]$coefficients), ":threat|threat:")],
      form_list_out[[4]][[5]]$coefficients[grepl(x = names(form_list_out[[4]][[5]]$coefficients), ":threat|threat:")],
      form_list_out[[5]][[5]]$coefficients[grepl(x = names(form_list_out[[5]][[5]]$coefficients), ":threat|threat:")],
      form_list_out[[1]][[6]]$coefficients[grepl(x = names(form_list_out[[1]][[6]]$coefficients), ":threat|threat:")],
      form_list_out[[2]][[6]]$coefficients[grepl(x = names(form_list_out[[2]][[6]]$coefficients), ":threat|threat:")],
      form_list_out[[3]][[6]]$coefficients[grepl(x = names(form_list_out[[3]][[6]]$coefficients), ":threat|threat:")],
      form_list_out[[4]][[6]]$coefficients[grepl(x = names(form_list_out[[4]][[6]]$coefficients), ":threat|threat:")],
      form_list_out[[5]][[6]]$coefficients[grepl(x = names(form_list_out[[5]][[6]]$coefficients), ":threat|threat:")],
      form_list_out[[1]][[7]]$coefficients[grepl(x = names(form_list_out[[1]][[7]]$coefficients), ":threat|threat:")],
      form_list_out[[2]][[7]]$coefficients[grepl(x = names(form_list_out[[2]][[7]]$coefficients), ":threat|threat:")],
      form_list_out[[3]][[7]]$coefficients[grepl(x = names(form_list_out[[3]][[7]]$coefficients), ":threat|threat:")],
      form_list_out[[4]][[7]]$coefficients[grepl(x = names(form_list_out[[4]][[7]]$coefficients), ":threat|threat:")],
      form_list_out[[5]][[7]]$coefficients[grepl(x = names(form_list_out[[5]][[7]]$coefficients), ":threat|threat:")],
      form_list_out[[1]][[8]]$coefficients[grepl(x = names(form_list_out[[1]][[8]]$coefficients), ":threat|threat:")],
      form_list_out[[2]][[8]]$coefficients[grepl(x = names(form_list_out[[2]][[8]]$coefficients), ":threat|threat:")],
      form_list_out[[3]][[8]]$coefficients[grepl(x = names(form_list_out[[3]][[8]]$coefficients), ":threat|threat:")],
      form_list_out[[4]][[8]]$coefficients[grepl(x = names(form_list_out[[4]][[8]]$coefficients), ":threat|threat:")],
      form_list_out[[5]][[8]]$coefficients[grepl(x = names(form_list_out[[5]][[8]]$coefficients), ":threat|threat:")],
      form_list_out[[1]][[9]]$coefficients[grepl(x = names(form_list_out[[1]][[9]]$coefficients), ":threat|threat:")],
      form_list_out[[2]][[9]]$coefficients[grepl(x = names(form_list_out[[2]][[9]]$coefficients), ":threat|threat:")],
      form_list_out[[3]][[9]]$coefficients[grepl(x = names(form_list_out[[3]][[9]]$coefficients), ":threat|threat:")],
      form_list_out[[4]][[9]]$coefficients[grepl(x = names(form_list_out[[4]][[9]]$coefficients), ":threat|threat:")],
      form_list_out[[5]][[9]]$coefficients[grepl(x = names(form_list_out[[5]][[9]]$coefficients), ":threat|threat:")],
      form_list_out[[1]][[10]]$coefficients[grepl(x = names(form_list_out[[1]][[10]]$coefficients), ":threat|threat:")],
      form_list_out[[2]][[10]]$coefficients[grepl(x = names(form_list_out[[2]][[10]]$coefficients), ":threat|threat:")],
      form_list_out[[3]][[10]]$coefficients[grepl(x = names(form_list_out[[3]][[10]]$coefficients), ":threat|threat:")],
      form_list_out[[4]][[10]]$coefficients[grepl(x = names(form_list_out[[4]][[10]]$coefficients), ":threat|threat:")],
      form_list_out[[5]][[10]]$coefficients[grepl(x = names(form_list_out[[5]][[10]]$coefficients), ":threat|threat:")]
    ),
  
  se = 
    c(outs_list[[1]][[1]]$std.error[grepl(x = names(outs_list[[1]][[1]]$std.error), ":threat|threat:")],
      outs_list[[1]][[2]]$std.error[grepl(x = names(outs_list[[1]][[2]]$std.error), ":threat|threat:")],
      outs_list[[1]][[3]]$std.error[grepl(x = names(outs_list[[1]][[3]]$std.error), ":threat|threat:")],
      outs_list[[1]][[4]]$std.error[grepl(x = names(outs_list[[1]][[4]]$std.error), ":threat|threat:")],
      outs_list[[1]][[5]]$std.error[grepl(x = names(outs_list[[1]][[5]]$std.error), ":threat|threat:")],
      outs_list[[2]][[1]]$std.error[grepl(x = names(outs_list[[2]][[1]]$std.error), ":threat|threat:")],
      outs_list[[2]][[2]]$std.error[grepl(x = names(outs_list[[2]][[2]]$std.error), ":threat|threat:")],
      outs_list[[2]][[3]]$std.error[grepl(x = names(outs_list[[2]][[3]]$std.error), ":threat|threat:")],
      outs_list[[2]][[4]]$std.error[grepl(x = names(outs_list[[2]][[4]]$std.error), ":threat|threat:")],
      outs_list[[2]][[5]]$std.error[grepl(x = names(outs_list[[2]][[5]]$std.error), ":threat|threat:")],
      form_list_out[[1]][[1]]$std.error[grepl(x = names(form_list_out[[1]][[1]]$std.error), ":threat|threat:")],
      form_list_out[[2]][[1]]$std.error[grepl(x = names(form_list_out[[2]][[1]]$std.error), ":threat|threat:")],
      form_list_out[[3]][[1]]$std.error[grepl(x = names(form_list_out[[3]][[1]]$std.error), ":threat|threat:")],
      form_list_out[[4]][[1]]$std.error[grepl(x = names(form_list_out[[4]][[1]]$std.error), ":threat|threat:")],
      form_list_out[[5]][[1]]$std.error[grepl(x = names(form_list_out[[5]][[1]]$std.error), ":threat|threat:")],
      form_list_out[[1]][[2]]$std.error[grepl(x = names(form_list_out[[1]][[2]]$std.error), ":threat|threat:")],
      form_list_out[[2]][[2]]$std.error[grepl(x = names(form_list_out[[2]][[2]]$std.error), ":threat|threat:")],
      form_list_out[[3]][[2]]$std.error[grepl(x = names(form_list_out[[3]][[2]]$std.error), ":threat|threat:")],
      form_list_out[[4]][[2]]$std.error[grepl(x = names(form_list_out[[4]][[2]]$std.error), ":threat|threat:")],
      form_list_out[[5]][[2]]$std.error[grepl(x = names(form_list_out[[5]][[2]]$std.error), ":threat|threat:")],
      form_list_out[[1]][[3]]$std.error[grepl(x = names(form_list_out[[1]][[3]]$std.error), ":threat|threat:")],
      form_list_out[[2]][[3]]$std.error[grepl(x = names(form_list_out[[2]][[3]]$std.error), ":threat|threat:")],
      form_list_out[[3]][[3]]$std.error[grepl(x = names(form_list_out[[3]][[3]]$std.error), ":threat|threat:")],
      form_list_out[[4]][[3]]$std.error[grepl(x = names(form_list_out[[4]][[3]]$std.error), ":threat|threat:")],
      form_list_out[[5]][[3]]$std.error[grepl(x = names(form_list_out[[5]][[3]]$std.error), ":threat|threat:")],
      form_list_out[[1]][[4]]$std.error[grepl(x = names(form_list_out[[1]][[4]]$std.error), ":threat|threat:")],
      form_list_out[[2]][[4]]$std.error[grepl(x = names(form_list_out[[2]][[4]]$std.error), ":threat|threat:")],
      form_list_out[[3]][[4]]$std.error[grepl(x = names(form_list_out[[3]][[4]]$std.error), ":threat|threat:")],
      form_list_out[[4]][[4]]$std.error[grepl(x = names(form_list_out[[4]][[4]]$std.error), ":threat|threat:")],
      form_list_out[[5]][[4]]$std.error[grepl(x = names(form_list_out[[5]][[4]]$std.error), ":threat|threat:")],
      form_list_out[[1]][[5]]$std.error[grepl(x = names(form_list_out[[1]][[5]]$std.error), ":threat|threat:")],
      form_list_out[[2]][[5]]$std.error[grepl(x = names(form_list_out[[2]][[5]]$std.error), ":threat|threat:")],
      form_list_out[[3]][[5]]$std.error[grepl(x = names(form_list_out[[3]][[5]]$std.error), ":threat|threat:")],
      form_list_out[[4]][[5]]$std.error[grepl(x = names(form_list_out[[4]][[5]]$std.error), ":threat|threat:")],
      form_list_out[[5]][[5]]$std.error[grepl(x = names(form_list_out[[5]][[5]]$std.error), ":threat|threat:")],
      form_list_out[[1]][[6]]$std.error[grepl(x = names(form_list_out[[1]][[6]]$std.error), ":threat|threat:")],
      form_list_out[[2]][[6]]$std.error[grepl(x = names(form_list_out[[2]][[6]]$std.error), ":threat|threat:")],
      form_list_out[[3]][[6]]$std.error[grepl(x = names(form_list_out[[3]][[6]]$std.error), ":threat|threat:")],
      form_list_out[[4]][[6]]$std.error[grepl(x = names(form_list_out[[4]][[6]]$std.error), ":threat|threat:")],
      form_list_out[[5]][[6]]$std.error[grepl(x = names(form_list_out[[5]][[6]]$std.error), ":threat|threat:")],
      form_list_out[[1]][[7]]$std.error[grepl(x = names(form_list_out[[1]][[7]]$std.error), ":threat|threat:")],
      form_list_out[[2]][[7]]$std.error[grepl(x = names(form_list_out[[2]][[7]]$std.error), ":threat|threat:")],
      form_list_out[[3]][[7]]$std.error[grepl(x = names(form_list_out[[3]][[7]]$std.error), ":threat|threat:")],
      form_list_out[[4]][[7]]$std.error[grepl(x = names(form_list_out[[4]][[7]]$std.error), ":threat|threat:")],
      form_list_out[[5]][[7]]$std.error[grepl(x = names(form_list_out[[5]][[7]]$std.error), ":threat|threat:")],
      form_list_out[[1]][[8]]$std.error[grepl(x = names(form_list_out[[1]][[8]]$std.error), ":threat|threat:")],
      form_list_out[[2]][[8]]$std.error[grepl(x = names(form_list_out[[2]][[8]]$std.error), ":threat|threat:")],
      form_list_out[[3]][[8]]$std.error[grepl(x = names(form_list_out[[3]][[8]]$std.error), ":threat|threat:")],
      form_list_out[[4]][[8]]$std.error[grepl(x = names(form_list_out[[4]][[8]]$std.error), ":threat|threat:")],
      form_list_out[[5]][[8]]$std.error[grepl(x = names(form_list_out[[5]][[8]]$std.error), ":threat|threat:")],
      form_list_out[[1]][[9]]$std.error[grepl(x = names(form_list_out[[1]][[9]]$std.error), ":threat|threat:")],
      form_list_out[[2]][[9]]$std.error[grepl(x = names(form_list_out[[2]][[9]]$std.error), ":threat|threat:")],
      form_list_out[[3]][[9]]$std.error[grepl(x = names(form_list_out[[3]][[9]]$std.error), ":threat|threat:")],
      form_list_out[[4]][[9]]$std.error[grepl(x = names(form_list_out[[4]][[9]]$std.error), ":threat|threat:")],
      form_list_out[[5]][[9]]$std.error[grepl(x = names(form_list_out[[5]][[9]]$std.error), ":threat|threat:")],
      form_list_out[[1]][[10]]$std.error[grepl(x = names(form_list_out[[1]][[10]]$std.error), ":threat|threat:")],
      form_list_out[[2]][[10]]$std.error[grepl(x = names(form_list_out[[2]][[10]]$std.error), ":threat|threat:")],
      form_list_out[[3]][[10]]$std.error[grepl(x = names(form_list_out[[3]][[10]]$std.error), ":threat|threat:")],
      form_list_out[[4]][[10]]$std.error[grepl(x = names(form_list_out[[4]][[10]]$std.error), ":threat|threat:")],
      form_list_out[[5]][[10]]$std.error[grepl(x = names(form_list_out[[5]][[10]]$std.error), ":threat|threat:")]
    ),
  
  pv = 
    c(outs_list[[1]][[1]]$p.value[grepl(x = names(outs_list[[1]][[1]]$p.value), ":threat|threat:")],
      outs_list[[1]][[2]]$p.value[grepl(x = names(outs_list[[1]][[2]]$p.value), ":threat|threat:")],
      outs_list[[1]][[3]]$p.value[grepl(x = names(outs_list[[1]][[3]]$p.value), ":threat|threat:")],
      outs_list[[1]][[4]]$p.value[grepl(x = names(outs_list[[1]][[4]]$p.value), ":threat|threat:")],
      outs_list[[1]][[5]]$p.value[grepl(x = names(outs_list[[1]][[5]]$p.value), ":threat|threat:")],
      outs_list[[2]][[1]]$p.value[grepl(x = names(outs_list[[2]][[1]]$p.value), ":threat|threat:")],
      outs_list[[2]][[2]]$p.value[grepl(x = names(outs_list[[2]][[2]]$p.value), ":threat|threat:")],
      outs_list[[2]][[3]]$p.value[grepl(x = names(outs_list[[2]][[3]]$p.value), ":threat|threat:")],
      outs_list[[2]][[4]]$p.value[grepl(x = names(outs_list[[2]][[4]]$p.value), ":threat|threat:")],
      outs_list[[2]][[5]]$p.value[grepl(x = names(outs_list[[2]][[5]]$p.value), ":threat|threat:")],
      form_list_out[[1]][[1]]$p.value[grepl(x = names(form_list_out[[1]][[1]]$p.value), ":threat|threat:")],
      form_list_out[[2]][[1]]$p.value[grepl(x = names(form_list_out[[2]][[1]]$p.value), ":threat|threat:")],
      form_list_out[[3]][[1]]$p.value[grepl(x = names(form_list_out[[3]][[1]]$p.value), ":threat|threat:")],
      form_list_out[[4]][[1]]$p.value[grepl(x = names(form_list_out[[4]][[1]]$p.value), ":threat|threat:")],
      form_list_out[[5]][[1]]$p.value[grepl(x = names(form_list_out[[5]][[1]]$p.value), ":threat|threat:")],
      form_list_out[[1]][[2]]$p.value[grepl(x = names(form_list_out[[1]][[2]]$p.value), ":threat|threat:")],
      form_list_out[[2]][[2]]$p.value[grepl(x = names(form_list_out[[2]][[2]]$p.value), ":threat|threat:")],
      form_list_out[[3]][[2]]$p.value[grepl(x = names(form_list_out[[3]][[2]]$p.value), ":threat|threat:")],
      form_list_out[[4]][[2]]$p.value[grepl(x = names(form_list_out[[4]][[2]]$p.value), ":threat|threat:")],
      form_list_out[[5]][[2]]$p.value[grepl(x = names(form_list_out[[5]][[2]]$p.value), ":threat|threat:")],
      form_list_out[[1]][[3]]$p.value[grepl(x = names(form_list_out[[1]][[3]]$p.value), ":threat|threat:")],
      form_list_out[[2]][[3]]$p.value[grepl(x = names(form_list_out[[2]][[3]]$p.value), ":threat|threat:")],
      form_list_out[[3]][[3]]$p.value[grepl(x = names(form_list_out[[3]][[3]]$p.value), ":threat|threat:")],
      form_list_out[[4]][[3]]$p.value[grepl(x = names(form_list_out[[4]][[3]]$p.value), ":threat|threat:")],
      form_list_out[[5]][[3]]$p.value[grepl(x = names(form_list_out[[5]][[3]]$p.value), ":threat|threat:")],
      form_list_out[[1]][[4]]$p.value[grepl(x = names(form_list_out[[1]][[4]]$p.value), ":threat|threat:")],
      form_list_out[[2]][[4]]$p.value[grepl(x = names(form_list_out[[2]][[4]]$p.value), ":threat|threat:")],
      form_list_out[[3]][[4]]$p.value[grepl(x = names(form_list_out[[3]][[4]]$p.value), ":threat|threat:")],
      form_list_out[[4]][[4]]$p.value[grepl(x = names(form_list_out[[4]][[4]]$p.value), ":threat|threat:")],
      form_list_out[[5]][[4]]$p.value[grepl(x = names(form_list_out[[5]][[4]]$p.value), ":threat|threat:")],
      form_list_out[[1]][[5]]$p.value[grepl(x = names(form_list_out[[1]][[5]]$p.value), ":threat|threat:")],
      form_list_out[[2]][[5]]$p.value[grepl(x = names(form_list_out[[2]][[5]]$p.value), ":threat|threat:")],
      form_list_out[[3]][[5]]$p.value[grepl(x = names(form_list_out[[3]][[5]]$p.value), ":threat|threat:")],
      form_list_out[[4]][[5]]$p.value[grepl(x = names(form_list_out[[4]][[5]]$p.value), ":threat|threat:")],
      form_list_out[[5]][[5]]$p.value[grepl(x = names(form_list_out[[5]][[5]]$p.value), ":threat|threat:")],
      form_list_out[[1]][[6]]$p.value[grepl(x = names(form_list_out[[1]][[6]]$p.value), ":threat|threat:")],
      form_list_out[[2]][[6]]$p.value[grepl(x = names(form_list_out[[2]][[6]]$p.value), ":threat|threat:")],
      form_list_out[[3]][[6]]$p.value[grepl(x = names(form_list_out[[3]][[6]]$p.value), ":threat|threat:")],
      form_list_out[[4]][[6]]$p.value[grepl(x = names(form_list_out[[4]][[6]]$p.value), ":threat|threat:")],
      form_list_out[[5]][[6]]$p.value[grepl(x = names(form_list_out[[5]][[6]]$p.value), ":threat|threat:")],
      form_list_out[[1]][[7]]$p.value[grepl(x = names(form_list_out[[1]][[7]]$p.value), ":threat|threat:")],
      form_list_out[[2]][[7]]$p.value[grepl(x = names(form_list_out[[2]][[7]]$p.value), ":threat|threat:")],
      form_list_out[[3]][[7]]$p.value[grepl(x = names(form_list_out[[3]][[7]]$p.value), ":threat|threat:")],
      form_list_out[[4]][[7]]$p.value[grepl(x = names(form_list_out[[4]][[7]]$p.value), ":threat|threat:")],
      form_list_out[[5]][[7]]$p.value[grepl(x = names(form_list_out[[5]][[7]]$p.value), ":threat|threat:")],
      form_list_out[[1]][[8]]$p.value[grepl(x = names(form_list_out[[1]][[8]]$p.value), ":threat|threat:")],
      form_list_out[[2]][[8]]$p.value[grepl(x = names(form_list_out[[2]][[8]]$p.value), ":threat|threat:")],
      form_list_out[[3]][[8]]$p.value[grepl(x = names(form_list_out[[3]][[8]]$p.value), ":threat|threat:")],
      form_list_out[[4]][[8]]$p.value[grepl(x = names(form_list_out[[4]][[8]]$p.value), ":threat|threat:")],
      form_list_out[[5]][[8]]$p.value[grepl(x = names(form_list_out[[5]][[8]]$p.value), ":threat|threat:")],
      form_list_out[[1]][[9]]$p.value[grepl(x = names(form_list_out[[1]][[9]]$p.value), ":threat|threat:")],
      form_list_out[[2]][[9]]$p.value[grepl(x = names(form_list_out[[2]][[9]]$p.value), ":threat|threat:")],
      form_list_out[[3]][[9]]$p.value[grepl(x = names(form_list_out[[3]][[9]]$p.value), ":threat|threat:")],
      form_list_out[[4]][[9]]$p.value[grepl(x = names(form_list_out[[4]][[9]]$p.value), ":threat|threat:")],
      form_list_out[[5]][[9]]$p.value[grepl(x = names(form_list_out[[5]][[9]]$p.value), ":threat|threat:")],
      form_list_out[[1]][[10]]$p.value[grepl(x = names(form_list_out[[1]][[10]]$p.value), ":threat|threat:")],
      form_list_out[[2]][[10]]$p.value[grepl(x = names(form_list_out[[2]][[10]]$p.value), ":threat|threat:")],
      form_list_out[[3]][[10]]$p.value[grepl(x = names(form_list_out[[3]][[10]]$p.value), ":threat|threat:")],
      form_list_out[[4]][[10]]$p.value[grepl(x = names(form_list_out[[4]][[10]]$p.value), ":threat|threat:")],
      form_list_out[[5]][[10]]$p.value[grepl(x = names(form_list_out[[5]][[10]]$p.value), ":threat|threat:")]
    ),
  
  outcome = c(rep("Oppose BLM", 6),
              rep("BLM Ineffective", 6),
              rep("Racial Resentment", 6),
              rep("Stereotype", 6),
              rep("Black Threat", 6),
              rep("White Residential Preference", 6),
              rep("Anti-BLM FT", 6),
              rep("Oppose BLM", 6),
              rep("BLM Ineffective", 6),
              rep("BLM No Protest", 6),
              rep("BLM No Support", 6),
              rep("Don't Defund", 6)),
  
  survey = c(rep("CMPS '16", 12), rep("CMPS '20", 60))
  
) %>% 
  `colnames<-` (c("Specification", "Coefficient", "SE", "p-value", "Outcome", "Survey")) %>% 
  xtable(digits = 2, caption = "Alternative Acculturation Specifications",
         align = "llccccc") %>% 
  print(include.rownames = FALSE,
        table.placement = "!htbp",
        caption.placement = 'top',
        comment = FALSE)
