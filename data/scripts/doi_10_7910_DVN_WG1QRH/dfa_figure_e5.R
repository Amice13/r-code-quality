#### purpose: producing figure E5 #### 

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

#### demonstrating racially polarized attitudes #### 

df_race_polarize = data.frame(
  
  est = c(weighted.mean(cmps20b$rr, cmps20b$weight),
          weighted.mean(cmps20b$black_stereotype_diff, cmps20b$weight),
          weighted.mean(cmps20b$black_threat, cmps20b$weight),
          weighted.mean(cmps20b$white_hood_rank_diff, cmps20b$weight),
          weighted.mean(cmps16b$supp_blm, cmps16b$weight),
          weighted.mean(cmps20b$blm_supports, cmps20b$weight),
          weighted.mean(cmps16b$blm_eff, cmps16b$weight, na.rm = TRUE),
          weighted.mean(cmps20b$blm_eff, cmps20b$weight),
          weighted.mean(cmps20b$blm_ft, cmps20b$weight),
          weighted.mean(cmps20b$blm_protest, cmps20b$weight),
          weighted.mean(cmps20b$blm_nprotest, cmps20b$weight),
          c(weighted.mean(cmps20l$rr[cmps20l$black_lat == 1],
                          cmps20l$weight[cmps20l$black_lat == 1]),
            weighted.mean(cmps20l$black_stereotype_diff[cmps20l$black_lat == 1],
                          cmps20l$weight[cmps20l$black_lat == 1]),
            weighted.mean(cmps20l$black_threat[cmps20l$black_lat == 1],
                          cmps20l$weight[cmps20l$black_lat == 1]),
            weighted.mean(cmps20l$white_hood_rank_diff[cmps20l$black_lat == 1],
                          cmps20l$weight[cmps20l$black_lat == 1]),
            weighted.mean(cmps16$supp_blm[cmps16$black_lat == 1],
                          cmps16$weight[cmps16$black_lat == 1]),
            weighted.mean(cmps20l$blm_supports[cmps20l$black_lat == 1],
                          cmps20l$weight[cmps20l$black_lat == 1]),
            weighted.mean(cmps16$blm_eff[cmps16$black_lat == 1],
                          cmps16$weight[cmps16$black_lat == 1], na.rm = TRUE),
            weighted.mean(cmps20l$blm_eff[cmps20l$black_lat == 1],
                          cmps20l$weight[cmps20l$black_lat == 1]),
            weighted.mean(cmps20l$blm_ft[cmps20l$black_lat == 1],
                          cmps20l$weight[cmps20l$black_lat == 1]),
            weighted.mean(cmps20l$blm_protest[cmps20l$black_lat == 1],
                          cmps20l$weight[cmps20l$black_lat == 1]),
            weighted.mean(cmps20l$blm_nprotest[cmps20l$black_lat == 1],
                          cmps20l$weight[cmps20l$black_lat == 1])),
          c(weighted.mean(cmps20l$rr[cmps20l$black_lat == 0],
                          cmps20l$weight[cmps20l$black_lat == 0]),
            weighted.mean(cmps20l$black_stereotype_diff[cmps20l$black_lat == 0],
                          cmps20l$weight[cmps20l$black_lat == 0]),
            weighted.mean(cmps20l$black_threat[cmps20l$black_lat == 0],
                          cmps20l$weight[cmps20l$black_lat == 0]),
            weighted.mean(cmps20l$white_hood_rank_diff[cmps20l$black_lat == 0],
                          cmps20l$weight[cmps20l$black_lat == 0]),
            weighted.mean(cmps16$supp_blm[cmps16$black_lat == 0],
                          cmps16$weight[cmps16$black_lat == 0]),
            weighted.mean(cmps20l$blm_supports[cmps20l$black_lat == 0],
                          cmps20l$weight[cmps20l$black_lat == 0]),
            weighted.mean(cmps16$blm_eff[cmps16$black_lat == 0],
                          cmps16$weight[cmps16$black_lat == 0], na.rm = TRUE),
            weighted.mean(cmps20l$blm_eff[cmps20l$black_lat == 0],
                          cmps20l$weight[cmps20l$black_lat == 0]),
            weighted.mean(cmps20l$blm_ft[cmps20l$black_lat == 0],
                          cmps20l$weight[cmps20l$black_lat == 0]),
            weighted.mean(cmps20l$blm_protest[cmps20l$black_lat == 0],
                          cmps20l$weight[cmps20l$black_lat == 0]),
            weighted.mean(cmps20l$blm_nprotest[cmps20l$black_lat == 0],
                          cmps20l$weight[cmps20l$black_lat == 0])),
          c(weighted.mean(cmps20w$rr,
                          cmps20w$weight),
            weighted.mean(cmps20w$black_stereotype_diff,
                          cmps20w$weight),
            weighted.mean(cmps20w$black_threat,
                          cmps20w$weight),
            weighted.mean(cmps20w$white_hood_rank_diff,
                          cmps20w$weight),
            weighted.mean(cmps16w$supp_blm,
                          cmps16w$weight),
            weighted.mean(cmps20w$blm_supports,
                          cmps20w$weight),
            NA,
            weighted.mean(cmps20w$blm_eff,
                          cmps20w$weight),
            weighted.mean(cmps20w$blm_ft,
                          cmps20w$weight),
            weighted.mean(cmps20w$blm_protest,
                          cmps20w$weight),
            weighted.mean(cmps20w$blm_nprotest,
                          cmps20w$weight))),
  
  race = factor(c(rep("Black\nnon-Latinx", 11), rep("Black\nLatinx", 11), 
                  rep("non-Black\nLatinx", 11), rep("White", 11)),
                levels = c("Black\nnon-Latinx", "Black\nLatinx",
                           "non-Black\nLatinx", "White")), 
  
  outcome = factor(rep(c("Racial Resentment", "Anti-Black Stereotype",
                         "Black Threat", "White Residential Preference",
                         "BLM Opposition ('16)", "BLM Opposition ('20)",
                         "BLM Ineffective ('16)", "BLM Ineffective ('20)",
                         "Anti-BLM FT", "BLM No Protest", "BLM No Protest Support"), 4),
                   levels = c("Racial Resentment", "Anti-Black Stereotype",
                              "Black Threat", "White Residential Preference",
                              "BLM Opposition ('16)", "BLM Opposition ('20)",
                              "BLM Ineffective ('16)", "BLM Ineffective ('20)",
                              "Anti-BLM FT",
                              "BLM No Protest", "BLM No Protest Support"),
                   labels = c("A. Racial Resentment", "B. Anti-Black Stereotype",
                              "C. Black Threat", "D. White Residential Preference",
                              "E. BLM Opposition ('16)", "F. BLM Opposition ('20)",
                              "G. BLM Ineffective ('16)", "H. BLM Ineffective ('20)",
                              "I. Anti-BLM FT",
                              "J. BLM No Protest", "K. BLM No Protest Support"))
  
)


annotate_df = data.frame(
  x_to_ann = rep(c(1, 2, 3, 4), 11),
  the_y_to_ann = 
    c(df_race_polarize$est[df_race_polarize$outcome == "A. Racial Resentment"],
      df_race_polarize$est[df_race_polarize$outcome == "B. Anti-Black Stereotype"],
      df_race_polarize$est[df_race_polarize$outcome == "C. Black Threat"],
      df_race_polarize$est[df_race_polarize$outcome == "D. White Residential Preference"],
      df_race_polarize$est[df_race_polarize$outcome == "E. BLM Opposition ('16)"],
      df_race_polarize$est[df_race_polarize$outcome == "F. BLM Opposition ('20)"],
      df_race_polarize$est[df_race_polarize$outcome == "G. BLM Ineffective ('16)"],
      df_race_polarize$est[df_race_polarize$outcome == "H. BLM Ineffective ('20)"],
      df_race_polarize$est[df_race_polarize$outcome == "I. Anti-BLM FT"],
      df_race_polarize$est[df_race_polarize$outcome == "J. BLM No Protest"],
      df_race_polarize$est[df_race_polarize$outcome == "K. BLM No Protest Support"]),
  outcome = c(rep("A. Racial Resentment", 4),
              rep("B. Anti-Black Stereotype", 4),
              rep("C. Black Threat", 4),
              rep("D. White Residential Preference",  4),
              rep("E. BLM Opposition ('16)", 4),
              rep("F. BLM Opposition ('20)", 4),
              rep("G. BLM Ineffective ('16)", 4),
              rep("H. BLM Ineffective ('20)", 4),
              rep("I. Anti-BLM FT", 4),
              rep("J. BLM No Protest", 4),
              rep("K. BLM No Protest Support", 4))
) 

annotate_df$the_y_to_ann = round(annotate_df$the_y_to_ann, 2)

polarize_plot = df_race_polarize %>% 
  ggplot() + 
  geom_col(aes(x = race, y = est)) + 
  facet_wrap(~outcome, ncol = 3, scales = "free") + 
  geom_text(data = annotate_df,
            aes(x = x_to_ann,  y = ifelse(the_y_to_ann >= 0, the_y_to_ann + .06,
                                          the_y_to_ann-.04),
                label = the_y_to_ann),
            family = "serif",
            size = 2.25) +
  labs(x = "Ethno-Racial Group", y = "Outcome Weighted Mean") + 
  theme_tufte()

ggsave(plot = polarize_plot, width = 8, height = 7, filename = "polarize_race.png")
