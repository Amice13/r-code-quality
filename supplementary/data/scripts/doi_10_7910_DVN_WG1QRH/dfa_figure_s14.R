#### purpose: producing figure s14 #### 

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

#### validating residential preference measure (figure s14) #### 

cmps20l[, c('neigh_service_good', 'neigh_rate', 'rr', 'black_stereotype_diff', 
            'id_cent', 'ownhome', 
            'ltpop', 'mhhi', 'pune', 'pcol', 'plat', 'pblk', 'pnc', 'pfb')] = 
  cmps20l[, c('neigh_service_good', 'neigh_rate', 'rr', 'black_stereotype_diff', 
              'id_cent', 'ownhome', 
              'ltpop', 'mhhi', 'pune', 'pcol', 'plat', 'pblk', 'pnc', 'pfb')] %>% 
  apply(X = ., MARGIN = 2, FUN = function(x) x / max(x, na.rm = TRUE))


resp1 = lm_robust(white_hood_rank_diff ~ neigh_service_good + neigh_rate + rr + black_stereotype_diff + 
                    id_cent + ownhome + 
                    ltpop + mhhi + pune + pcol + plat + pblk + pnc + pfb,
                  data = cmps20l, weight = weight, subset = black_lat == 0)

df_coefplot = 
  data.frame(
    
    est = resp1$coefficients[2:length(resp1$coefficients)],
    se = resp1$std.error[2:length(resp1$coefficients)],
    covars = factor(c("Neighborhood Service Rating", "Neighborhood Rating",
                      "Racial Resentment", "Anti-Black Stereotype",
                      "Latinx ID", "Homeowner",
                      "Log(Population) (Zip)",
                      "Median Household Income (Zip)",
                      "% Unemployed (Zip)",
                      "% College (Zip)",
                      "% Latino (Zip)",
                      "% Black (Zip)",
                      "% Non-Citizen (Zip)",
                      "% Foreign (Zip)"),
                    levels = 
                      rev(c("Neighborhood Service Rating", "Neighborhood Rating",
                            "Racial Resentment", "Anti-Black Stereotype",
                            "Latinx ID", "Homeowner",
                            "Log(Population) (Zip)",
                            "Median Household Income (Zip)",
                            "% Unemployed (Zip)",
                            "% College (Zip)",
                            "% Latino (Zip)",
                            "% Black (Zip)",
                            "% Non-Citizen (Zip)",
                            "% Foreign (Zip)")))
  )

df_coefplot %>% 
  ggplot() + 
  geom_point(aes(x = covars, y = est),
             size = 2) + 
  geom_errorbar(aes(x = covars, 
                    ymin = est - 1.96 * se,
                    ymax = est + 1.96 * se),
                width = 0,
                size = .4) +
  geom_errorbar(aes(x = covars, 
                    ymin = est - 1.645 * se,
                    ymax = est + 1.645 * se),
                width = 0,
                size = .6) +
  geom_hline(yintercept = 0) + 
  labs(x = "Coefficient", y = "Covariate") + 
  coord_flip() + 
  theme_tufte()

ggsave(width = 4, height = 3, filename = "nhoodplot.png")

