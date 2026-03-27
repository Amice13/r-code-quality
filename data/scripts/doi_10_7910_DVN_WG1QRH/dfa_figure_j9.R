#### purpose: producing figure j9 #### 

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

cmps16$pnc = cmps16$pnc / max(cmps16$pnc, na.rm = TRUE)
cmps16$pfb = cmps16$pfb / max(cmps16$pfb, na.rm = TRUE)
cmps20l$pnc = cmps20l$pnc / max(cmps20l$pnc, na.rm = TRUE)
cmps20l$pfb = cmps20l$pfb / max(cmps20l$pfb, na.rm = TRUE)

#### validating deportation threat scale (reproducing figure j9) ####

cmps16$ltotal = cmps16$ltotal / max(cmps16$ltotal, na.rm = TRUE)
cmps20l$ltotal = cmps20l$ltotal / max(cmps20l$ltotal, na.rm = TRUE)

df_threat_val = list(cmps16, cmps20l)
df_threat_val_lab = c("CMPS '16", "CMPS '20")
df_outcome_val_list = list(
  c("ltotal", "know_undoc", "pfb", "pnc"),
  c("ltotal", "know_undoc", "know_deport", "pfb", "pnc")
)

df_outcome_val_list_lab = list(
  c("Log(Deportations + 1)", "Know Undocumented", "% Foreign", "% Non-citizen"),
  c("Log(Deportations + 1)", "Know Undocumented", "Know Deportee", "% Foreign", "% Non-citizen")
)


df_val_out_list = as.list(rep(NA, length(df_threat_val)))
df_val_out_list2 = as.list(rep(NA, length(df_threat_val)))

for (i in 1:length(df_threat_val)) {
  
  print(paste0("Big Iteration ", i))
  df_outcome_list_out = as.list(rep(NA, length(df_outcome_val_list[[i]])))
  df_outcome_list_out2 = as.list(rep(NA, length(df_outcome_val_list[[i]])))
  
  for (k in 1:length(df_outcome_val_list[[i]])) {
    
    print(paste0("Small Iteration ", k))  
    df_outcome_list_out[[k]] = 
      lm_robust(as.formula(paste0('threat~', df_outcome_val_list[[i]][[k]])),
                data = df_threat_val[[i]], weight = weight, subset = black_lat == 0)
    
    predout = 
      predict(object = df_outcome_list_out[[k]],
              newdata = data.frame(var = seq(from = 0, to = 1, by = .1)) %>% 
                `colnames<-` (c(df_outcome_val_list[[i]][[k]])),
              se.fit = TRUE)
    
    df_outcome_list_out2[[k]] = 
      data.frame(
        est = predout$fit,
        se = predout$se.fit,
        outcome = df_outcome_val_list_lab[[i]][[k]],
        dataset = df_threat_val_lab[[i]],
        val = seq(from = 0, to = 1, by = .1)
      )
    
  }
  
  df_val_out_list[[i]] = df_outcome_list_out
  df_val_out_list2[[i]] = df_outcome_list_out2
  
}

df_val_out_list2 = 
  df_val_out_list2 %>% 
  unlist(recursive = FALSE) %>% 
  do.call(rbind.data.frame, .) %>%
  mutate(outcome_data = paste0(outcome, " (", dataset, ")"))

df_val_out_list2 = bind_rows(
  df_val_out_list2[!grepl(x = df_val_out_list2$outcome, "Undocumented|Deportee"), ],
  (df_val_out_list2[grepl(x = df_val_out_list2$outcome, "Undocumented|Deportee"), ] %>% 
     filter(val == 0 | val == 1))
)

df_val_out_list2$outcome_data = 
  factor(df_val_out_list2$outcome_data, 
         levels = c("Log(Deportations + 1) (CMPS '16)",
                    "Log(Deportations + 1) (CMPS '20)",
                    "Know Undocumented (CMPS '16)",
                    "Know Undocumented (CMPS '20)",
                    "Know Deportee (CMPS '20)",
                    "% Foreign (CMPS '16)",
                    "% Foreign (CMPS '20)",
                    "% Non-citizen (CMPS '16)",
                    "% Non-citizen (CMPS '20)"),
         labels = c("A. Log(Deportations + 1) (CMPS '16)",
                    "B. Log(Deportations + 1) (CMPS '20)",
                    "C. Know Undocumented (CMPS '16)",
                    "D. Know Undocumented (CMPS '20)",
                    "E. Know Deportee (CMPS '20)",
                    "F. % Foreign (CMPS '16)",
                    "G. % Foreign (CMPS '20)",
                    "H. % Non-citizen (CMPS '16)",
                    "I. % Non-citizen (CMPS '20)"))

df_val_out_list2 %>% 
  ggplot() + 
  geom_point(aes(x = val, y = est)) + 
  geom_errorbar(aes(x = val, 
                    ymin = est - 1.96 * se,
                    ymax = est + 1.96 * se),
                width = 0,
                size = .2) + 
  geom_errorbar(aes(x = val, 
                    ymin = est - 1.645 * se,
                    ymax = est + 1.645 * se),
                width = 0,
                size = .4) + 
  facet_wrap(~ outcome_data) + 
  labs(x = "Covariate Value (Panel Header Variable)",
       y = "Prediced Value (Deportation Threat)") + 
  theme_tufte()

ggsave(width = 8, height = 7.5, filename = "threatvalidation.png")
