#### purpose: producing table r7-r10 #### 

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

#### ruling out main alternative explanations (producing tables r7-r10) #### 

# Rescale covariates 


cmps16[, c("edu", "pd_latinx", "ed", "lf_lat", "skin", "inc", "unemp", "econ_worse",
           "pue_cty", "pcol_lat_adv", "pbl", "ppov_lat_adv", "polcomp")] = 
  cmps16[, c("edu", "pd_latinx", "ed", "lf_lat", "skin", "inc", "unemp", "econ_worse",
             "pue_cty", "pcol_lat_adv", "pbl", "ppov_lat_adv", "polcomp")] %>% 
  apply(X = ., MARGIN = 2, FUN = function(x) x / max(x, na.rm = TRUE))

cmps20l[, c("edu", "pd_lat", "lf_lat", "skin", "inc", "econ_afraid_pers",
            "econ_afraid_socio", "pune", "pune_cty", "ppov_lat_adv", "pblk", "polcomp")] = 
  cmps20l[, c("edu", "pd_lat", "lf_lat", "skin", "inc", "econ_afraid_pers",
              "econ_afraid_socio", "pune", "pune_cty", "ppov_lat_adv", "pblk", "polcomp")] %>% 
  apply(X = ., MARGIN = 2, FUN = function(x) x / max(x, na.rm = TRUE))


# competition

c(
  'acc2 * threat',
  'acc2 * threat + acc2 * inc + acc2 * inc_ref + acc2 * unemp + acc2 * econ_afraid_pers + acc2 * econ_afraid_socio + acc2 * econ_grp_worse + 
    acc2 * pune + acc2 * pune_cty + acc2 * ppov_lat_adv * pblk + acc2 * polcomp +
    wom + skin + age + mar + cath + no_mx + 
    no_pr + no_dr + no_cb + no_es + inc + inc_ref + edu + 
    unemp + ownhome + econ_afraid_pers + econ_afraid_socio + econ_grp_worse + 
    exp_disc + pd_lat + pd_lat_ref + pd_blk + pd_blk_ref + 
    pid7 + ide + ide_ref + polcomp + id_cent + amer_id + imm_work_ethic + 
    know_undoc + know_deport + polint + lf_lat + pct_blk_perc + 
    ltpop + plat + pblk + lmhhi + pune + pcol + ltpop_cty + plat_cty + pblk_cty + pfb_cty + lmhhi_cty + 
    pune_cty + dep_rate + ltotal + dep_rate + ppov_lat_adv * pblk + 
    cenfe_ws + cenfe_nc + cenfe_st + miss_cty'
  
)



# FORMULAS 

the_forms_alt_exps1 = list(
  
  # social desirability 
  
  c( 
    'acc2 * threat',
    'acc2 * threat +  acc2 * edu + 
        wom + acc2 + skin + age + mar + cath + 
        no_mx + no_pr + no_dr + no_cb + no_es + 
        inc + inc_ref + edu + unemp + ownhome + econ_worse + 
        ed + pd_latinx + pd_black + pi_dem2 + pi_gop2 + ide + ide_miss + polcomp +
        id_cent + amer_cent + know_undoc + polint + lf_lat + neigh_blk + neigh_blk_miss +
        ltpop + plt + pbl + pfb + pue + lmhhi + ltpop_cty + plt_cty + pbl_cty + pfb_cty + lmhhi_cty + 
        pue_cty + dep_rate + ltotal + miss_fips + pcol_lat_adv * pbl + ppov_lat_adv * pbl + 
        ppov_lat_adv * pbl + pcol_lat_adv * pbl + pbl_church + miss_church + mar_black_spouse + 
        cenfe_ws + cenfe_nc + cenfe_st + miss_adv'
  ),
  
  # discrimination
  
  c(
    'acc2 * threat',
    'acc2 * threat +  acc2 * pd_latinx + acc2 * ed +
        wom + acc2 + skin + age + mar + cath + 
        no_mx + no_pr + no_dr + no_cb + no_es + 
        inc + inc_ref + edu + unemp + ownhome + econ_worse + 
        ed + pd_latinx + pd_black + pi_dem2 + pi_gop2 + ide + ide_miss + polcomp +
        id_cent + amer_cent + know_undoc + polint + lf_lat + neigh_blk + neigh_blk_miss +
        ltpop + plt + pbl + pfb + pue + lmhhi + ltpop_cty + plt_cty + pbl_cty + pfb_cty + lmhhi_cty + 
        pue_cty + dep_rate + ltotal + miss_fips + pcol_lat_adv * pbl + ppov_lat_adv * pbl + 
        ppov_lat_adv * pbl + pcol_lat_adv * pbl + pbl_church + miss_church + mar_black_spouse + 
        cenfe_ws + cenfe_nc + cenfe_st + miss_adv'
  ),
  
  # linked fate 
  
  c(
    'acc2 * threat',
    'acc2 * threat +  acc2 * lf_lat +
        wom + acc2 + skin + age + mar + cath + 
        no_mx + no_pr + no_dr + no_cb + no_es + 
        inc + inc_ref + edu + unemp + ownhome + econ_worse + 
        ed + pd_latinx + pd_black + pi_dem2 + pi_gop2 + ide + ide_miss + polcomp +
        id_cent + amer_cent + know_undoc + polint + lf_lat + neigh_blk + neigh_blk_miss +
        ltpop + plt + pbl + pfb + pue + lmhhi + ltpop_cty + plt_cty + pbl_cty + pfb_cty + lmhhi_cty + 
        pue_cty + dep_rate + ltotal + miss_fips + pcol_lat_adv * pbl + ppov_lat_adv * pbl + 
        ppov_lat_adv * pbl + pcol_lat_adv * pbl + pbl_church + miss_church + mar_black_spouse + 
        cenfe_ws + cenfe_nc + cenfe_st + miss_adv'
  ),
  
  # racialization
  
  c(
    'acc2 * threat',
    'acc2 * threat + acc2 * skin + 
        wom + acc2 + skin + age + mar + cath + 
        no_mx + no_pr + no_dr + no_cb + no_es + 
        inc + inc_ref + edu + unemp + ownhome + econ_worse + 
        ed + pd_latinx + pd_black + pi_dem2 + pi_gop2 + ide + ide_miss + polcomp +
        id_cent + amer_cent + know_undoc + polint + lf_lat + neigh_blk + neigh_blk_miss +
        ltpop + plt + pbl + pfb + pue + lmhhi + ltpop_cty + plt_cty + pbl_cty + pfb_cty + lmhhi_cty + 
        pue_cty + dep_rate + ltotal + miss_fips + pcol_lat_adv * pbl + ppov_lat_adv * pbl + 
        ppov_lat_adv * pbl + pcol_lat_adv * pbl + pbl_church + miss_church + mar_black_spouse + 
        cenfe_ws + cenfe_nc + cenfe_st + miss_adv'
  ),
  
  # competition
  
  c(
    'acc2 * threat',
    'acc2 * threat + acc2 * inc + acc2 * inc_ref + acc2 * unemp + acc2 * econ_worse + acc2 * pue + acc2 * pue_cty + acc2 * pcol_lat_adv * pbl + acc2 * ppov_lat_adv * pbl + acc2 * polcomp +
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
  
)


the_forms_alt_exps2 = list(
  
  # social desirability 
  
  c(
    'acc2 * threat',
    'acc2 * threat +  acc2 * edu + wom + skin + age + mar + cath + no_mx + 
    no_pr + no_dr + no_cb + no_es + inc + inc_ref + edu + 
    unemp + ownhome + econ_afraid_pers + econ_afraid_socio + econ_grp_worse + 
    exp_disc + pd_lat + pd_lat_ref + pd_blk + pd_blk_ref + 
    pid7 + ide + ide_ref + polcomp + id_cent + amer_id + imm_work_ethic + 
    know_undoc + know_deport + polint + lf_lat + pct_blk_perc + 
    ltpop + plat + pblk + lmhhi + pune + pcol + ltpop_cty + plat_cty + pblk_cty + pfb_cty + lmhhi_cty + 
    pune_cty + dep_rate + ltotal + dep_rate + ppov_lat_adv * pblk + 
    cenfe_ws + cenfe_nc + cenfe_st + miss_cty'
    
  ),
  
  # discrimination
  
  c(
    'acc2 * threat',
    'acc2 * threat +  acc2 * pd_lat + acc2 * ed + wom + skin + age + mar + cath + no_mx + 
    no_pr + no_dr + no_cb + no_es + inc + inc_ref + edu + 
    unemp + ownhome + econ_afraid_pers + econ_afraid_socio + econ_grp_worse + 
    exp_disc + pd_lat + pd_lat_ref + pd_blk + pd_blk_ref + 
    pid7 + ide + ide_ref + polcomp + id_cent + amer_id + imm_work_ethic + 
    know_undoc + know_deport + polint + lf_lat + pct_blk_perc + 
    ltpop + plat + pblk + lmhhi + pune + pcol + ltpop_cty + plat_cty + pblk_cty + pfb_cty + lmhhi_cty + 
    pune_cty + dep_rate + ltotal + dep_rate + ppov_lat_adv * pblk + 
    cenfe_ws + cenfe_nc + cenfe_st + miss_cty'
  ),
  
  # linked fate 
  
  c(
    'acc2 * threat',
    'acc2 * threat +  acc2 * lf_lat + wom + skin + age + mar + cath + no_mx + 
    no_pr + no_dr + no_cb + no_es + inc + inc_ref + edu + 
    unemp + ownhome + econ_afraid_pers + econ_afraid_socio + econ_grp_worse + 
    exp_disc + pd_lat + pd_lat_ref + pd_blk + pd_blk_ref + 
    pid7 + ide + ide_ref + polcomp + id_cent + amer_id + imm_work_ethic + 
    know_undoc + know_deport + polint + lf_lat + pct_blk_perc + 
    ltpop + plat + pblk + lmhhi + pune + pcol + ltpop_cty + plat_cty + pblk_cty + pfb_cty + lmhhi_cty + 
    pune_cty + dep_rate + ltotal + dep_rate + ppov_lat_adv * pblk + 
    cenfe_ws + cenfe_nc + cenfe_st + miss_cty'
  ),
  
  # racialization
  
  c(
    'acc2 * threat',
    'acc2 * threat +  acc2 * skin + wom + skin + age + mar + cath + no_mx + 
    no_pr + no_dr + no_cb + no_es + inc + inc_ref + edu + 
    unemp + ownhome + econ_afraid_pers + econ_afraid_socio + econ_grp_worse + 
    exp_disc + pd_lat + pd_lat_ref + pd_blk + pd_blk_ref + 
    pid7 + ide + ide_ref + polcomp + id_cent + amer_id + imm_work_ethic + 
    know_undoc + know_deport + polint + lf_lat + pct_blk_perc + 
    ltpop + plat + pblk + lmhhi + pune + pcol + ltpop_cty + plat_cty + pblk_cty + pfb_cty + lmhhi_cty + 
    pune_cty + dep_rate + ltotal + dep_rate + ppov_lat_adv * pblk + 
    cenfe_ws + cenfe_nc + cenfe_st + miss_cty'
  ),
  
  # competition
  
  c(
    'acc2 * threat',
    'acc2 * threat + acc2 * inc + acc2 * inc_ref + acc2 * unemp + acc2 * econ_afraid_pers + acc2 * econ_afraid_socio + acc2 * econ_grp_worse + 
    acc2 * pune + acc2 * pune_cty + acc2 * ppov_lat_adv * pblk + acc2 * polcomp +
    wom + skin + age + mar + cath + no_mx + 
    no_pr + no_dr + no_cb + no_es + inc + inc_ref + edu + 
    unemp + ownhome + econ_afraid_pers + econ_afraid_socio + econ_grp_worse + 
    exp_disc + pd_lat + pd_lat_ref + pd_blk + pd_blk_ref + 
    pid7 + ide + ide_ref + polcomp + id_cent + amer_id + imm_work_ethic + 
    know_undoc + know_deport + polint + lf_lat + pct_blk_perc + 
    ltpop + plat + pblk + lmhhi + pune + pcol + ltpop_cty + plat_cty + pblk_cty + pfb_cty + lmhhi_cty + 
    pune_cty + dep_rate + ltotal + dep_rate + ppov_lat_adv * pblk + 
    cenfe_ws + cenfe_nc + cenfe_st + miss_cty'
    
  )
  
)

the_forms_alt_exps_list = 
  list(the_forms_alt_exps1, the_forms_alt_exps2)
outs_list = list(c("supp_blm", "blm_eff"),
                 c("rr", "black_stereotype_diff",
                   "black_threat", "white_hood_rank_diff", "blm_ft",
                   "blm_supports", "blm_eff", "blm_protest", "blm_nprotest"))
data_list_tol = list(cmps16, cmps20l)


the_forms_alt_exps_list_out = as.list(rep(NA, length(the_forms_alt_exps_list)))

for (i in 1:length(the_forms_alt_exps_list)) {
  
  print(paste0("Iteration ", i))
  
  out_alt_mechs = as.list(rep(NA, length(the_forms_alt_exps_list[[i]])))
  
  for (j in 1:length(the_forms_alt_exps_list[[i]])) {
    
    print(paste0("Iteration ", j))
    out_the_outs = as.list(rep(NA, length(length(outs_list[[i]]))))
    
    for (k in 1:length(outs_list[[i]])) {
      
      print(paste0("Iteration ", k))
      modout = lm_robust(as.formula(paste0(outs_list[[i]][[k]], "~", the_forms_alt_exps_list[[i]][[j]][[2]])),
                         data = data_list_tol[[i]], subset = black_lat == 0, weight = weight)
      
      out_the_outs[[k]] = modout
      
    }
    
    out_alt_mechs[[j]] = out_the_outs
    
  }
  
  the_forms_alt_exps_list_out[[i]] = out_alt_mechs
  
}

altformlist = the_forms_alt_exps_list_out
mech_label = c('Social Desirability', 'Discrimination', 'Linked Fate',
               'Skin Color', "Intergroup Competition")
dataset = c("CMPS '16", "CMPS '20")
outlabs = list(
  c("Oppose BLM", "BLM Ineffective"),
  c("Racial Resentment", "Stereotype", "Black Threat", "White Res. Pref.",
    "Anti-BLM FT", "Oppose BLM", "BLM Ineffective", "BLM No Protest", "BLM No Support")
)
data_out_list = as.list(rep(NA, length(the_forms_alt_exps_list_out)))

for (i in 1:length(the_forms_alt_exps_list_out)) {
  
  print(paste0("Iteration ", i))
  mech_list = as.list(rep(NA, length(the_forms_alt_exps_list_out[[i]])))
  
  for (k in 1:length(the_forms_alt_exps_list_out[[i]])) {
    
    print(paste0("Iteration ", k))
    
    out_list = as.list(rep(NA, length(the_forms_alt_exps_list_out[[i]][[k]])))
    
    for (j in 1:length(the_forms_alt_exps_list_out[[i]][[k]])) {
      
      print(paste0("Iteration ", j))
      
      coefs = 
        the_forms_alt_exps_list_out[[i]][[k]][[j]]$coefficients[
          grepl(x = names(the_forms_alt_exps_list_out[[i]][[k]][[j]]$coefficients), "acc2:|:acc2")
          ]
      
      ses = 
        the_forms_alt_exps_list_out[[i]][[k]][[j]]$std.error[
          grepl(x = names(the_forms_alt_exps_list_out[[i]][[k]][[j]]$coefficients), "acc2:|:acc2")
          ]
      
      pvs = 
        the_forms_alt_exps_list_out[[i]][[k]][[j]]$p.value[
          grepl(x = names(the_forms_alt_exps_list_out[[i]][[k]][[j]]$coefficients), "acc2:|:acc2")
          ]
      
      df_out = data.frame(
        est = coefs,
        se = ses,
        pv = pvs,
        name = names(the_forms_alt_exps_list_out[[i]][[k]][[j]]$coefficients)[
          grepl(x = names(the_forms_alt_exps_list_out[[i]][[k]][[j]]$coefficients), "acc2:|:acc2")
          ],
        outcome = outlabs[[i]][[j]],
        mech = mech_label[k], 
        datalabel = dataset[i]
        
      )
      
      out_list[[j]] = df_out
      
    }
    
    mech_list[[k]] = out_list
    
  }
  
  data_out_list[[i]] = mech_list
  
}

df_alt_mechs = 
  data_out_list %>% 
  unlist(., recursive = FALSE) %>% 
  unlist(., recursive = FALSE) %>% 
  do.call(rbind.data.frame, .) %>% 
  as.data.frame() %>% 
  dplyr::select(datalabel, outcome, mech, name, est, se, pv) %>% 
  `colnames<-` (c("Dataset", "Outcome", "Mechanism", "Name", "Coef.", "SE", "pval")) %>% 
  mutate(Dataset = as.character(Dataset),
         Outcome = as.character(Outcome),
         Mechanism = as.character(Mechanism),
         Name = as.character(Name))


df_alt_mechs$Name = 
  ifelse(df_alt_mechs$Name == "acc2:threat", "Acculturation x Threat",
         ifelse(df_alt_mechs$Name == "acc2:edu", "Acculturation x Education",
                ifelse(df_alt_mechs$Name == "acc2:pd_latinx", "Acculturation x Perceived Discrim.",
                       ifelse(df_alt_mechs$Name == "acc2:pd_latinx", "Acculturation x Perceived Discrim.",
                              ifelse(df_alt_mechs$Name == "acc2:ed", "Acculturation x Experienced Discrim.",
                                     ifelse(df_alt_mechs$Name == "acc2:lf_lat", "Acculturation x Linked Fate",
                                            ifelse(df_alt_mechs$Name == "acc2:skin", "Acculturation x Skin Color",
                                                   ifelse(df_alt_mechs$Name == "acc2:inc", "Acculturation x Income",
                                                          ifelse(df_alt_mechs$Name == "acc2:inc_ref", "Acculturation x Income (Refused)",
                                                                 ifelse(df_alt_mechs$Name == "acc2:unemp", "Acculturation x Unemployed",
                                                                        ifelse(df_alt_mechs$Name == "acc2:inc", "Acculturation x Income (Refused)",
                                                                               ifelse(df_alt_mechs$Name == "acc2:inc_ref", "Acculturation x Income (Refused)",
                                                                                      ifelse(df_alt_mechs$Name == "acc2:econ_worse", "Acculturation x Retro. Econ Worse",
                                                                                             ifelse(df_alt_mechs$Name == "acc2:unemp", "Acculturation x Unemployed",
                                                                                                    ifelse(df_alt_mechs$Name == "acc2:pue", "Acculturation x % Unemployed (Zip)",
                                                                                                           ifelse(df_alt_mechs$Name == "acc2:pue_cty", "Acculturation x % Unemployed (County)",
                                                                                                                  ifelse(df_alt_mechs$Name == "acc2:polcomp", "Acculturation x Political Competition",
                                                                                                                         ifelse(df_alt_mechs$Name == "acc2:pcol_lat_adv:pbl", "Acculturation x Latinx College Advantage",
                                                                                                                                ifelse(df_alt_mechs$Name == "acc2:ppov_lat_adv:pbl", "Acculturation x Latinx Poverty Advantage",
                                                                                                                                       ifelse(df_alt_mechs$Name == "acc2:pd_lat", "Acculturation x Perceived Discrim.",
                                                                                                                                              ifelse(df_alt_mechs$Name == "acc2:econ_afraid_pers", "Acculturation x Prospective Pers. Econ Worse",
                                                                                                                                                     ifelse(df_alt_mechs$Name == "acc2:econ_afraid_socio", "Acculturation x Prospective Socio. Econ Worse",
                                                                                                                                                            ifelse(df_alt_mechs$Name == "acc2:econ_grp_worse", "Acculturation x Prospective Group Econ Worse",
                                                                                                                                                                   ifelse(df_alt_mechs$Name == "acc2:pune", "Acculturation x % Unemployed",
                                                                                                                                                                          ifelse(df_alt_mechs$Name == "acc2:pbl:pcol_lat_adv", "Acculturation x Latinx-Black College Advantage",
                                                                                                                                                                                 ifelse(df_alt_mechs$Name == "acc2:pbl:ppov_lat_adv", "Acculturation x Latinx-Black Poverty Advantage",
                                                                                                                                                                                        ifelse(df_alt_mechs$Name == "acc2:ppov_lat_adv:pblk", "Latinx-Black Poverty Advantage", df_alt_mechs$Name)))))))))))))))))))))))))))

df_alt_mechs = df_alt_mechs %>% filter(!grepl(x = df_alt_mechs$Name, "acc2:"))

df_alt_mechs %>% 
  filter(Dataset == "CMPS '16") %>% 
  xtable(digits = 3,
         align = "lccccccc",
         label = "table:altmechs1",
         caption = "Threat Undercuts The Adoption of or Maintenance of Anti-Black Beliefs As Non-Black Latinxs Acculturate Net of Established Alternative Mechanisms (Part 1)") %>% 
  print(include.rownames = FALSE,
        comment = FALSE,
        caption.placement = "top",
        scalebox = .65,
        table.placement = "!htbp")

df_alt_mechs %>% 
  filter(Dataset == "CMPS '20") %>%
  dplyr::slice(1:60) %>% 
  xtable(digits = 3,
         align = "lccccccc",
         label = "table:altmechs2",
         caption = "Threat Undercuts The Adoption of or Maintenance of Anti-Black Beliefs As Non-Black Latinxs Acculturate Net of Established Alternative Mechanisms  (Part 2)") %>% 
  print(include.rownames = FALSE,
        comment = FALSE,
        caption.placement = "top",
        scalebox = .65,
        table.placement = "!htbp") 

df_alt_mechs %>% 
  filter(Dataset == "CMPS '20") %>%
  dplyr::slice(61:120) %>% 
  xtable(digits = 3,
         align = "lccccccc",
         label = "table:altmechs2",
         caption = "Threat Undercuts The Adoption of or Maintenance of Anti-Black Beliefs As Non-Black Latinxs Acculturate Net of Established Alternative Mechanisms  (Part 3)") %>% 
  print(include.rownames = FALSE,
        comment = FALSE,
        caption.placement = "top",
        scalebox = .65,
        table.placement = "!htbp") 

df_alt_mechs %>% 
  filter(Dataset == "CMPS '20") %>%
  dplyr::slice(121:171) %>% 
  xtable(digits = 3,
         align = "lccccccc",
         label = "table:altmechs2",
         caption = "Threat Undercuts The Adoption of or Maintenance of Anti-Black Beliefs As Non-Black Latinxs Acculturate Net of Established Alternative Mechanisms  (Part 4)") %>% 
  print(include.rownames = FALSE,
        comment = FALSE,
        caption.placement = "top",
        scalebox = .65,
        table.placement = "!htbp") 

