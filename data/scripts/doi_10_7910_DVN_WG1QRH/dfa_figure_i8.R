#### purpose: producing table i8 #### 

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



#### validating acculturation scale (producing figure i8) ####

# more fixes 

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


cmps20l$plt = cmps20l$plat
outs_val = 
  c("id_cent", "amer_cent", "ed", "pd_latinx", "mar_white", "inc", "edu",
    "ownhome", "plt", "pbl", "pfb", "pnc")
outs_val_lab = c("Latino ID", "American ID", 
                 "Experience Discrim.", "Perceived Discrim.",
                 "White Intermarriage", "Income",
                 "Education", "Homeowner", "% Latinx", "% Black", "% Foreign",
                 "% Non-citizen")
data_val_lab = c("CMPS '16", "CMPS '20")
cmps16 = cmps16 %>% as.data.frame

cmps16[, outs_val] = cmps16[, outs_val] %>% 
  apply(X = ., MARGIN = 2, FUN = function(x) x / max(x, na.rm = TRUE))
cmps20l[, outs_val] = cmps20l[, outs_val] %>% 
  apply(X = ., MARGIN = 2, FUN = function(x) x / max(x, na.rm = TRUE))

datasetsacc = list(cmps16, cmps20l)
datasetsacc_out = as.list(rep(NA, length(datasetsacc)))
datasetsacc_out2 = as.list(rep(NA, length(datasetsacc)))

for (i in 1:length(datasetsacc)) {
  
  print(paste0("Big Iteration ", i))
  outs_val_list = as.list(rep(NA, length(outs_val)))
  outs_val_list2 = as.list(rep(NA, length(outs_val)))
  
  for (k in 1:length(outs_val)) {
    
    print(paste0("Smaller Iteration ", i))
    outs_val_list[[k]] = 
      lm_robust(as.formula(paste0(outs_val[k], "~acc2")),
                data = datasetsacc[[i]],
                weight = weight,
                subset = black_lat == 0)
    
    pred_out = predict(object = outs_val_list[[k]],
                       newdata = data.frame(acc2 = seq(from = 0, to = 1, by = .25)),
                       se.fit = TRUE)
    
    outs_val_list2[[k]] = data.frame(
      acc2 = seq(from = 0, to = 1, by = .25),
      est = pred_out$fit,
      se = pred_out$se.fit,
      outcome = outs_val_lab[k],
      dataset = data_val_lab[i]
    )
    
    
  }
  
  datasetsacc_out[[i]] = outs_val_list
  datasetsacc_out2[[i]] = outs_val_list2
  
}


dataoutpdf = datasetsacc_out2 %>% 
  unlist(recursive = FALSE) %>% 
  do.call(rbind.data.frame, .)

dataoutpdf$outcome_dataset = 
  paste0(dataoutpdf$outcome, " (", dataoutpdf$dataset, ")")

dataoutpdf$outcome_dataset = 
  factor(dataoutpdf$outcome_dataset,
         levels = c("Latino ID (CMPS '16)", "Latino ID (CMPS '20)",
                    "American ID (CMPS '16)", "American ID (CMPS '20)",
                    "Experience Discrim. (CMPS '16)",
                    "Experience Discrim. (CMPS '20)",
                    "Perceived Discrim. (CMPS '16)",
                    "Perceived Discrim. (CMPS '20)",
                    "White Intermarriage (CMPS '16)", 
                    "White Intermarriage (CMPS '20)",
                    "Income (CMPS '16)", "Income (CMPS '20)",
                    "Education (CMPS '16)", "Education (CMPS '20)",
                    "Homeowner (CMPS '16)", "Homeowner (CMPS '20)",
                    "% Latinx (CMPS '16)", "% Latinx (CMPS '20)",
                    "% Black (CMPS '16)", "% Black (CMPS '20)",
                    "% Foreign (CMPS '16)", "% Foreign (CMPS '20)",
                    "% Non-citizen (CMPS '16)", "% Non-citizen (CMPS '20)"),
         labels = c("A. Latino ID (CMPS '16)", "B. Latino ID (CMPS '20)",
                    "C. American ID (CMPS '16)", "D. American ID (CMPS '20)",
                    "E. Experience Discrim. (CMPS '16)",
                    "F. Experience Discrim. (CMPS '20)",
                    "G. Perceived Discrim. (CMPS '16)",
                    "H. Perceived Discrim. (CMPS '20)",
                    "I. White Intermarriage (CMPS '16)", 
                    "J. White Intermarriage (CMPS '20)",
                    "K. Income (CMPS '16)", "L. Income (CMPS '20)",
                    "M. Education (CMPS '16)", "N. Education (CMPS '20)",
                    "O. Homeowner (CMPS '16)", "P. Homeowner (CMPS '20)",
                    "Q. % Latinx (CMPS '16)", "R. % Latinx (CMPS '20)",
                    "S. % Black (CMPS '16)", "T. % Black (CMPS '20)",
                    "U. % Foreign (CMPS '16)", "V. % Foreign (CMPS '20)",
                    "W. % Non-citizen (CMPS '16)", "X. % Non-citizen (CMPS '20)"))

accplot = dataoutpdf %>% 
  ggplot() + 
  geom_point(aes(x = acc2, y = est)) + 
  geom_errorbar(aes(x = acc2, 
                    ymin = est - 1.645 * se,
                    ymax = est + 1.645 * se),
                width = 0,
                size = .4) + 
  geom_errorbar(aes(x = acc2, 
                    ymin = est - 1.96 * se,
                    ymax = est + 1.96 * se),
                width = 0,
                size = .2) + 
  facet_wrap(~outcome_dataset, ncol = 4,
             scales = 'free') + 
  labs(x = "Acculturation", y = "Predicted Value (Panel Outcome)") + 
  theme_tufte(base_size = 9)

ggsave(plot = accplot, filename = "accplotval.png", width = 8, height = 6)

