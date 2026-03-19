#### purpose: producing figure h7 #### 

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

#### acculturation association ####

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
# dataset list 

df_list = list(cmps16, cmps20l)
df_list_lab = c("CMPS '16", "CMPS '20")
outcome_lists = 
  list(
    c("supp_blm", "blm_eff"),
    c("rr", "black_stereotype_diff",
      "black_threat", "white_hood_rank_diff", "blm_ft",
      "blm_supports", "blm_eff", "blm_protest", "blm_nprotest", "dtps2")
  )

outcome_lists_lab = 
  list(
    c("E. Oppose BLM", "F. BLM Ineffective"),
    c("A. Racial Resentment", "B. Anti-Black Stereotype", "C. Black = Threat", "D. Prefer White Residence",
      "G. Anti-BLM FT", "H. BLM Opposition", "I. BLM Ineffective", "J. No BLM Protest", "K. No BLM Protest Support", "L. Don't Defund")
  )

df_list_out = as.list(rep(NA, length(df_list)))

for (i in 1:length(df_list)) {
  
  print(paste0("Iteration ", i))
  
  outcome_list_out = as.list(rep(NA, length(outcome_lists[[i]])))
  
  for (k in 1:length(outcome_lists[[i]])) {
    
    print(paste0("Iteration ", k))
    out = lm_robust(as.formula(paste0(paste0(outcome_lists[[i]][[k]]), "~", 'acc2')),
                    data = df_list[[i]],
                    weight = weight,
                    subset = black_lat == 0)
    
    outcome_list_out[[k]] = data.frame(
      
      acc2 = c(0, .25, .5, .75, 1),
      est = predict(object = out,
                    newdata = data.frame(acc2 = c(0, .25, .5, .75, 1)),
                    se.fit = TRUE),
      outcome = outcome_lists_lab[[i]][[k]],
      survey = df_list_lab[[i]]
      
    )
    
    
  } 
  
  df_list_out[[i]] = outcome_list_out
  
}

df_list_out = df_list_out %>% 
  unlist(recursive = FALSE) %>% 
  do.call(rbind.data.frame, .) %>% 
  mutate(outsurv = paste0(outcome, ", ", survey))

df_list_out %>%
  ggplot() + 
  geom_point(aes(x = acc2, y = est.fit)) +
  geom_errorbar(aes(x = acc2, 
                    ymin = est.fit - 1.96 * est.se.fit,
                    ymax = est.fit + 1.96 * est.se.fit),
                width = 0) + 
  facet_wrap(~ outsurv, scales = "free") + 
  labs(x = "Acculturation",
       y = "Anti-Black Beliefs OR Opposition to Black Political Interests") + 
  theme_tufte(base_size = 9)

ggsave(plot = last_plot(), width = 8, height = 6, filename = "accmot.png")
