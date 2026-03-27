#### purpose: producing tables s24 #### 

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

cmps16$vote_dem = ifelse(cmps16$c14 == 1, 1, 0)

#### replicating table s24 ####

mod16 = lm_robust(vote_dem ~ acc2 * threat + wom + acc2 + skin + age + mar + cath + 
  no_mx + no_pr + no_dr + no_cb + no_es + 
  inc + inc_ref + edu + unemp + ownhome + econ_worse + 
  ed + pd_latinx + pd_black + pi_dem2 + pi_gop2 + ide + ide_miss + polcomp +
  id_cent + amer_cent + know_undoc + polint + lf_lat + neigh_blk + neigh_blk_miss +
  ltpop + plt + pbl + pfb + pue + lmhhi + ltpop_cty + plt_cty + pbl_cty + pfb_cty + lmhhi_cty + 
  pue_cty + dep_rate + ltotal + miss_fips + pcol_lat_adv * pbl + ppov_lat_adv * pbl + 
  ppov_lat_adv * pbl + pcol_lat_adv * pbl + pbl_church + miss_church + mar_black_spouse + 
  cenfe_ws + cenfe_nc + cenfe_st + miss_adv, data = cmps16, weight = weight, subset = black_lat == 0 & c12 == 1)

mod20 = lm_robust(vote_dem~acc2 * threat + wom + skin + age + mar + cath + no_mx + 
  no_pr + no_dr + no_cb + no_es + inc + inc_ref + edu + 
  unemp + ownhome + econ_afraid_pers + econ_afraid_socio + econ_grp_worse + 
  exp_disc + pd_lat + pd_lat_ref + pd_blk + pd_blk_ref + 
  pid7 + ide + ide_ref + polcomp + id_cent + amer_id + imm_work_ethic + 
  know_undoc + know_deport + polint + lf_lat + pct_blk_perc + 
  ltpop + plat + pblk + lmhhi + pune + pcol + ltpop_cty + plat_cty + pblk_cty + pfb_cty + lmhhi_cty + 
  pune_cty + dep_rate + ltotal + dep_rate + ppov_lat_adv * pblk + 
  cenfe_ws + cenfe_nc + cenfe_st + miss_cty, data = cmps20l, weight = weight, subset = black_lat == 0 & voted == 1)

texreg(l = list(mod16, mod20),
       booktabs = TRUE,
       include.ci = FALSE,
       include.rmse = FALSE,
       include.adjrs = FALSE,
       custom.gof.rows = list("Survey" = c("CMPS '16", "CMPS '20"),
                              "Controls?" = c("Y", "Y")),
       label = "table:votechoiceout",
       custom.coef.map = list("acc2:threat" = "Acculturation x Threat",
                              "acc2" = "Acculturation",
                              "threat" = "Threat"),
       custom.model.names = paste0("(", seq(from = 1, to = 2) ,")"),
       float.pos = "!htbp",
       caption.above = TRUE,
       caption = "Association Between Threat, Acculturation, and Presidential Vote Choice in 2016 and 2020")
