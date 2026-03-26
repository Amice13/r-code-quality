#### purpose: producing figure S15 #### 

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

#### validating blm ft measure (producing figure s15) #### 

# loading in data 

anes20 = 
  read_stata("anes2020/anes_timeseries_2020_stata_20220210/anes_timeseries_2020_stata_20220210.dta")

# cleaning data 

anes20 = 
  anes20 %>% 
  mutate(blm_ft = ifelse(V202174 < 0 | V202174 > 100, NA, V202174)) %>% 
  mutate(aidblk = abs(ifelse(V201258 == -9 | V201258 == -8 | 
                               V201258 == 99, NA, V201258) - 7)) %>% 
  mutate(ide = 
           ifelse(V201200 == -9 | V201200 == -8, NA, V201200),
         ide = 
           abs(ifelse(ide == 99, 4, ide) - 7)) %>% 
  mutate(affact = 
           ifelse(V202249 == -9 | V202249 == -8 | 
                    V202249 == -7 | V202249 == -6 | 
                    V202249 == -5, NA, V202249)) %>% 
  mutate(affact = ifelse(affact == 1, 1, 0)) %>% 
  mutate(weight = V200010a) %>% 
  mutate(blm_ft = blm_ft / max(blm_ft, na.rm = TRUE),
         aidblk = aidblk / max(aidblk, na.rm = TRUE),
         affact = affact / max(affact, na.rm = TRUE)) %>% 
  mutate(inc_ref = ifelse(V202468x == -9 | V202468x == -5, 1, 0),
         inc = ifelse(V202468x == -9 | V202468x == -5, 1, V202468x) - 1) %>%
  mutate(latino = ifelse(V201546 == 1, 1, 0),
         black = ifelse(V201549x == 3, 1, 0),
         woman = ifelse(V202637 == 2, 1, 0), 
         pidem = ifelse(V201228 == 1, 1, 0),
         pigop = ifelse(V201228 == 2, 1, 0),
         college = ifelse(V201510 == 6 | V201510 == 7 | V201510 == 8, 1, 0)) %>% 
  mutate(age_ref = ifelse(V201507x == -9, 1, 0),
         age = ifelse(V201507x == -9, 18, V201507x),
         age = age / max(age, na.rm = TRUE)) %>% 
  dplyr::select(blm_ft, aidblk, affact,
                age, age_ref, woman, black, latino, 
                inc, inc_ref, college,
                pidem, pigop, ide, 
                weight)  


blmftval1 =
  lm_robust(affact ~ blm_ft + age + age_ref + woman + 
              inc + inc_ref + college + pidem + pigop + ide,
            weight = weight, data = anes20,
            subset = black == 0)
blmftval2 = 
  lm_robust(aidblk ~ blm_ft + age + age_ref + woman + 
              inc + inc_ref + college + pidem + pigop + ide,
            weight = weight, data = anes20,
            subset = black == 0)


valblmp1 = data.frame(
  
  est = blmftval1$coefficients[2:length(blmftval1$coefficients)],
  se = blmftval1$std.error[2:length(blmftval1$std.error)],
  covs = factor(c("BLM FT", "Age", "Age (Missing)", "Woman",
                  "Income", "Income (Missing)", "College", "Democrat", "Republican",
                  "Ideology (7pt)"),
                levels = 
                  rev(c("BLM FT", "Age", "Age (Missing)", "Woman",
                        "Income", "Income (Missing)", "College", "Democrat", "Republican",
                        "Ideology (7pt)")))
) %>% 
  ggplot() + 
  geom_point(aes(x = covs, y = est),
             size = 2) +
  geom_errorbar(aes(x = covs, 
                    ymin = est - 1.96 * se,
                    ymax = est + 1.96 * se),
                width = 0,
                size = .4) + 
  geom_errorbar(aes(x = covs, 
                    ymin = est - 1.645 * se,
                    ymax = est + 1.645 * se),
                width = 0,
                size = .6) + 
  coord_flip() + 
  geom_hline(yintercept = 0) + 
  labs(x = "Coefficient", y = "Covariate",
       title = "A. Black Affirmative Action") + 
  theme_tufte()

valblmp2 = data.frame(
  
  est = blmftval2$coefficients[2:length(blmftval2$coefficients)],
  se = blmftval2$std.error[2:length(blmftval2$std.error)],
  covs = factor(c("BLM FT", "Age", "Age (Missing)", "Woman",
                  "Income", "Income (Missing)", "College", "Democrat", "Republican",
                  "Ideology (7pt)"),
                levels = 
                  rev(c("BLM FT", "Age", "Age (Missing)", "Woman",
                        "Income", "Income (Missing)", "College", "Democrat", "Republican",
                        "Ideology (7pt)")))
) %>% 
  ggplot() + 
  geom_point(aes(x = covs, y = est),
             size = 2) +
  geom_errorbar(aes(x = covs, 
                    ymin = est - 1.96 * se,
                    ymax = est + 1.96 * se),
                width = 0,
                size = .4) + 
  geom_errorbar(aes(x = covs, 
                    ymin = est - 1.645 * se,
                    ymax = est + 1.645 * se),
                width = 0,
                size = .6) + 
  coord_flip() + 
  geom_hline(yintercept = 0) + 
  labs(x = "Coefficient", y = "Covariate",
       title = "B. Aid to Blacks") + 
  theme_tufte()

valblm_grob = 
  arrangeGrob(valblmp1, valblmp2, ncol = 2)

ggsave(plot = valblm_grob, width = 8, height = 2.5, filename = "valblmplot.png")
