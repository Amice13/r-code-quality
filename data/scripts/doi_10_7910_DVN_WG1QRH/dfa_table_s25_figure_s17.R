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


#### table s25 ####

mod_dtp = 
  lm_robust(dtps ~ acc2 * threat + wom + skin + age + mar + cath + no_mx + 
              no_pr + no_dr + no_cb + no_es + inc + inc_ref + edu + 
              unemp + ownhome + econ_afraid_pers + econ_afraid_socio + econ_grp_worse + 
              exp_disc + pd_lat + pd_lat_ref + pd_blk + pd_blk_ref + 
              pid7 + ide + ide_ref + polcomp + id_cent + amer_id + imm_work_ethic + 
              know_undoc + know_deport + polint + lf_lat + pct_blk_perc + 
              ltpop + plat + pblk + lmhhi + pune + pcol + ltpop_cty + plat_cty + pblk_cty + pfb_cty + lmhhi_cty + 
              pune_cty + dep_rate + ltotal + dep_rate + ppov_lat_adv * pblk + 
              cenfe_ws + cenfe_nc + cenfe_st + miss_cty, data = cmps20l, weight = weight, subset = black_lat == 0)


texreg(l = list(mod_dtp),
       booktabs = TRUE,
       include.ci = FALSE,
       include.rmse = FALSE,
       include.adjrs = FALSE,
       custom.gof.rows = list("Survey" = c("CMPS '20"),
                              "Controls?" = c("Y")),
       label = "table:dtptable",
       custom.coef.map = list("acc2:threat" = "Acculturation x Threat",
                              "acc2" = "Acculturation",
                              "threat" = "Threat"),
       custom.model.names = paste0("(", seq(from = 1, to = 1) ,")"),
       float.pos = "!htbp",
       caption.above = TRUE,
       stars = c(.1, .05, .01, .001), 
       symbol = "\\dagger",
       caption = "\\textbf{Association Between Threat, Acculturation, and Opposition To Defunding the Police}")

#### figure S17 #### 

fkdta = 
  cmps20l[cmps20l$black_lat == 0, names(mod_dtp$coefficients)[2:57]] %>% 
  apply(X = ., MARGIN = 2, FUN = function(x) mean(x, na.rm = TRUE)) %>% 
  as.data.frame %>% t %>% data.frame %>% 
  slice(rep(1:n(), each = 10)) %>% 
  mutate(miss_cty = 0) %>% 
  mutate(cenfe_ws = 1) %>% 
  mutate(acc2 = rep(seq(from = 0, to = 1, by = .25), 2 )) %>% 
  mutate(threat = c(rep(0, 5), rep(1, 5)))

data.frame(
  est = predict(newdata = fkdta, object = mod_dtp, se.fit = TRUE)$fit,
  se = predict(newdata = fkdta, object = mod_dtp, se.fit = TRUE)$se.fit  
) %>% 
  mutate(acc2 = rep(seq(from = 0, to = 1, by = .25), 2 )) %>% 
  mutate(threat = c(rep(0, 5), rep(1, 5))) %>% 
  mutate(threat = factor(threat)) %>% 
  ggplot() + 
  geom_point(aes(x = acc2, y = est, col = threat),
             position = position_dodge(.15)) + 
  geom_line(aes(x = acc2, y = est, col = threat),
            position = position_dodge(.15)) + 
  geom_errorbar(aes(x = acc2, 
                    ymin = est - 1.645 * se,
                    ymax = est + 1.645 * se,
                    col = threat),
                width = 0,
                position = position_dodge(.15),
                size = .6) + 
  geom_errorbar(aes(x = acc2, 
                    ymin = est - 1.96 * se,
                    ymax = est + 1.96 * se,
                    col = threat),
                width = 0,
                position = position_dodge(.15),
                size = .4) + 
  labs(x = "Acculturation", y = "Oppose Defund The Police",
       col = "Threat",
       title = "Oppose Defunding the Police (CMPS '20)") + 
  scale_color_grey(start = .6, end = 0) + 
  geom_hline(yintercept = weighted.mean(cmps20l$dtps[cmps20l$black_lat == 1],
                                        w = cmps20l$weight[cmps20l$black_lat == 1],
                                        na.rm = TRUE),
             linetype = 2) +
  annotate("text", label = "Black Latinx",
           label = "Black Latinx", x = .5, y = .535,
           family = "serif",
           size = 2.5) + 
  geom_hline(yintercept = weighted.mean(cmps20b$dtps, w = cmps20b$weight, na.rm = TRUE),
             linetype = 2) + 
  annotate("text", label = "Black Non-Latinx", x = .5, y = .585,
           family = "serif",
           size = 2.5) +
  geom_hline(yintercept = weighted.mean(cmps20w$dtps,
                                        w = cmps20w$weight,
                                        na.rm = TRUE),
             linetype = 2) +
  annotate("text", label = "White", x = .5, y = .8,
           family = "serif",
           size = 2.5) +
  theme_tufte(base_size = 9)

ggsave(plot = last_plot(), width = 4, height = 3, filename = "dtpout.png")


