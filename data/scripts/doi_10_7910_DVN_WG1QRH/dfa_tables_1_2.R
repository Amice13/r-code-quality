#### purpose: producing tables 1 and 2 #### 

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

#### cmps '16 --- running analyses #### 

# fixing threat variable 

weighted.mean(cmps16$supp_blm[cmps16$acc == 0 & cmps16$threat == 0],
              w = cmps16$weight[cmps16$acc == 0 & cmps16$threat == 0],
              na.rm = TRUE)
weighted.mean(cmps16$supp_blm[cmps16$acc == 1 & cmps16$threat == 0],
              w = cmps16$weight[cmps16$acc == 1 & cmps16$threat == 0],
              na.rm = TRUE)
weighted.mean(cmps16$supp_blm[cmps16$acc == 2 & cmps16$threat == 0],
              w = cmps16$weight[cmps16$acc == 2 & cmps16$threat == 0],
              na.rm = TRUE)
weighted.mean(cmps16$supp_blm[cmps16$acc == 3 & cmps16$threat == 0],
              w = cmps16$weight[cmps16$acc == 3 & cmps16$threat == 0],
              na.rm = TRUE)

outs = c("supp_blm", "blm_eff")
forms = 
  c(
    'acc2 * threat',
    'acc2 * threat + wom + acc2 + skin + age + mar + cath + 
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


#### cmps 16' --- predicted values ####

each_times = 
  length(unique(cmps16$acc2)[order(unique(cmps16$acc2))]) * 
  length(unique(cmps16$worry)[order(unique(cmps16$worry))])

fakedf = 
  cmps16[, names(outs_list[[1]][[2]]$coefficients[2:(length(outs_list[[1]][[2]]$coefficients)-3)])] %>% 
  apply(X = ., MARGIN = 2, function(x) mean(x, na.rm = TRUE)) %>% 
  t %>% 
  as.data.frame() %>% 
  slice(rep(1:n(), each = each_times)) %>% 
  mutate(cenfe_nc = 0,
         cenfe_st = 0,
         cenfe_ws = 1,
         miss_adv = 0,
         miss_fips = 0) %>% 
  mutate(acc2 = c(rep(unique(cmps16$acc2)[order(unique(cmps16$acc2))][1], 
                      length(unique(cmps16$worry)[order(unique(cmps16$worry))])),
                  rep(unique(cmps16$acc2)[order(unique(cmps16$acc2))][2], 
                      length(unique(cmps16$worry)[order(unique(cmps16$worry))])),
                  rep(unique(cmps16$acc2)[order(unique(cmps16$acc2))][3], 
                      length(unique(cmps16$worry)[order(unique(cmps16$worry))])),
                  rep(unique(cmps16$acc2)[order(unique(cmps16$acc2))][4], 
                      length(unique(cmps16$worry)[order(unique(cmps16$worry))])),
                  rep(unique(cmps16$acc2)[order(unique(cmps16$acc2))][5], 
                      length(unique(cmps16$worry)[order(unique(cmps16$worry))]))),
         threat = rep(unique(cmps16$acc2)[order(unique(cmps16$acc2))], 5))

df_plot16_1 = data.frame(
  acc2 = fakedf$acc2,
  threat = fakedf$threat,
  est = predict(newdata = fakedf, object = outs_list[[1]][[2]], se.fit = TRUE)$fit,
  se = predict(newdata = fakedf, object = outs_list[[1]][[2]], se.fit = TRUE)$se.fit
) %>% filter(threat == 0 | threat == 1) 

df_plot16_2 = data.frame(
  acc2 = fakedf$acc2,
  threat = fakedf$threat,
  est = predict(newdata = fakedf, object = outs_list[[2]][[2]], se.fit = TRUE)$fit,
  se = predict(newdata = fakedf, object = outs_list[[2]][[2]], se.fit = TRUE)$se.fit
) %>% filter(threat == 0 | threat == 1) 

plot16_1 = df_plot16_1 %>% 
  ggplot() + 
  geom_point(aes(x = acc2, y = est, col = factor(threat)),
             position = position_dodge(.075),
             size = 1.5) +
  geom_errorbar(aes(x = acc2,
                    ymin = est - 1.645 * se,
                    ymax = est + 1.645 * se,
                    col = factor(threat)),
                width = 0,
                position = position_dodge(.075),
                size = .4) + 
  geom_errorbar(aes(x = acc2,
                    ymin = est - 1.96 * se,
                    ymax = est + 1.96 * se,
                    col = factor(threat)),
                width = 0,
                position = position_dodge(.075),
                size = .2) + 
  geom_line(aes(x = acc2, y = est, col = factor(threat)),
            position = position_dodge(.075)) + 
  scale_color_grey(start = .6, end = .2) + 
  labs(x = "Acculturation", y = "Oppose BLM", 
       col = "Threat",
       title = "A. Oppose BLM (CMPS '16)") + 
  annotate("text",
           label = paste0("White (", round(weighted.mean(cmps16w$supp_blm, w = cmps16w$weight), 2), ")"),
           x = .75,
           y = round(weighted.mean(cmps16w$supp_blm, w = cmps16w$weight), 2) + .02,
           family = "serif",
           size = 2.15) + 
  annotate("text",
           label = paste0("Black Latinx (", round(weighted.mean(cmps16$supp_blm[cmps16$black_lat == 1],
                                                                w = cmps16$weight[cmps16$black_lat == 1]), 2), ")"),
           x = .25,
           y = round(weighted.mean(cmps16$supp_blm[cmps16$black_lat == 1],
                                   w = cmps16$weight[cmps16$black_lat == 1]), 2) + .02,
           family = "serif",
           size = 2.15) + 
  annotate("text",
           label = paste0("Black Non-Latinx (", round(weighted.mean(cmps16b$supp_blm, w = cmps16b$weight), 2), ")"),
           x = .35,
           y = round(weighted.mean(cmps16b$supp_blm, w = cmps16b$weight), 2) + .02,
           family = "serif",
           size = 2.15) + 
  geom_hline(yintercept = round(weighted.mean(cmps16$supp_blm[cmps16$black_lat == 1],
                                              w = cmps16$weight[cmps16$black_lat == 1]), 2),
             col = "grey44",
             linetype = 2) + 
  geom_hline(yintercept = round(weighted.mean(cmps16b$supp_blm, w = cmps16b$weight), 2),
             linetype = 2) + 
  geom_hline(yintercept = round(weighted.mean(cmps16w$supp_blm, w = cmps16w$weight), 2), col = "grey72",
             linetype = 2) + 
  theme_tufte(base_size = 6) + 
  theme(legend.key.size = unit(.2, 'cm'))

plot16_2 = df_plot16_2 %>% 
  ggplot() + 
  geom_point(aes(x = acc2, y = est, col = factor(threat)),
             position = position_dodge(.075),
             size = 1.5) +
  geom_errorbar(aes(x = acc2,
                    ymin = est - 1.645 * se,
                    ymax = est + 1.645 * se,
                    col = factor(threat)),
                width = 0,
                position = position_dodge(.075),
                size = .4) + 
  geom_errorbar(aes(x = acc2,
                    ymin = est - 1.96 * se,
                    ymax = est + 1.96 * se,
                    col = factor(threat)),
                width = 0,
                position = position_dodge(.075),
                size = .2) + 
  geom_line(aes(x = acc2, y = est, col = factor(threat)),
            position = position_dodge(.075)) + 
  scale_color_grey(start = .6, end = .2) + 
  labs(x = "Acculturation", y = "Believe BLM = Ineffective", 
       col = "Threat",
       title = "B. BLM Ineffective (CMPS '16)") + 
  annotate("text",
           label = paste0("Black Latinx (",
                          round(weighted.mean(cmps16$blm_eff[cmps16$black_lat == 1],
                                              w = cmps16$weight[cmps16$black_lat == 1], na.rm = TRUE), 2), ")"),
           x = .785,
           y = .53,
           family = "serif",
           size = 2.15) + 
  annotate("text",
           label = paste0("Black Non-Latinx (",
                          round(weighted.mean(cmps16b$blm_eff,
                                              w = cmps16b$weight, na.rm = TRUE), 2), ")"),
           x = .35,
           y = round(weighted.mean(cmps16b$blm_eff,
                                   w = cmps16b$weight, na.rm = TRUE), 2) - .02,
           family = "serif",
           size = 2.15) + 
  geom_hline(yintercept = round(weighted.mean(cmps16$blm_eff[cmps16$black_lat == 1],
                                              w = cmps16$weight[cmps16$black_lat == 1], na.rm = TRUE), 2),
             col = "grey44",
             linetype = 2) + 
  geom_hline(yintercept = round(weighted.mean(cmps16b$blm_eff, w = cmps16b$weight, na.rm = TRUE), 2),
             linetype = 2) + 
  theme_tufte(base_size = 6) + 
  theme(legend.key.size = unit(.2, 'cm'))

plot16_grob = arrangeGrob(plot16_1, plot16_2, ncol = 2)

# ggsave(plot = plot16_grob, width = 8, height = 2.5, filename = "cmpsres16.png")

#### cmps '20 --- running heterogeneity analyses #### 


cmps20l$threat2 = cmps20l$threat2 / max(cmps20l$threat2, na.rm = TRUE)
cmps20l$threat = cmps20l$threat2
# now, outcomes 

outcomes2l = 
  c("rr", "black_stereotype_diff",
    "black_threat", "white_hood_rank_diff", "blm_ft",
    "blm_supports", "blm_eff", "blm_protest", "blm_nprotest")

# formulas 

form_list_20 = list(
  '~acc2 * threat',
  '~acc2 * threat + wom + skin + age + mar + cath + no_mx + 
    no_pr + no_dr + no_cb + no_es + inc + inc_ref + edu + 
    unemp + ownhome + econ_afraid_pers + econ_afraid_socio + econ_grp_worse + 
    exp_disc + pd_lat + pd_lat_ref + pd_blk + pd_blk_ref + 
    pid7 + ide + ide_ref + polcomp + id_cent + amer_id + imm_work_ethic + 
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
                subset = black_lat == 0,
                se_type = "stata")
    
    
  }
  
  form_list_out[[i]] = outcomes_2l_list
  
}


#### tables 1 and 2 #### 

# anti-black attitudes --- no controls

texreg(l = list(form_list_out[[1]][[1]],
                form_list_out[[1]][[2]],
                form_list_out[[1]][[3]],
                form_list_out[[1]][[4]]),
       include.ci = FALSE,
       float.pos = "!htbp",
       caption.above = TRUE,
       caption = "The threat of deportation undercuts the adoption of anti-Black attitudes via acculturation among non-Black Latinxs",
       include.rmse = FALSE,
       include.adjrs = FALSE,
       custom.coef.map = list("acc2:threat" = "Acculturation x Threat",
                              "acc2" = "Acculturation",
                              "threat" = "Threat"),
       custom.model.names = c("Racial Resentment", "Anti-Black Stereotype", "Black = Threat", "Prefer White Residence"),
       stars = c(.1, .05, .01, .001),
       symbol = "\\dagger")

# anti-black attitudes --- yes controls (table 1)

texreg(l = list(form_list_out[[2]][[1]],
                form_list_out[[2]][[2]],
                form_list_out[[2]][[3]],
                form_list_out[[2]][[4]]),
       include.ci = FALSE,
       float.pos = "!htbp",
       caption.above = TRUE,
       caption = "The threat of deportation undercuts the adoption of anti-Black attitudes via acculturation among non-Black Latinxs",
       include.rmse = FALSE,
       include.adjrs = FALSE,
       custom.coef.map = list("acc2:threat" = "Acculturation x Threat",
                              "acc2" = "Acculturation",
                              "threat" = "Threat"),
       custom.model.names = c("Racial Resentment", "Anti-Black Stereotype", "Black = Threat", "Prefer White Residence"),
       stars = c(.1, .05, .01, .001),
       symbol = "\\dagger")


# opposition to black political interests --- no controls

texreg(l = list(outs_list[[1]][[1]],
                outs_list[[2]][[1]],
                form_list_out[[1]][[5]],
                form_list_out[[1]][[6]],
                form_list_out[[1]][[7]],
                form_list_out[[1]][[8]],
                form_list_out[[1]][[9]]),
       include.ci = FALSE,
       float.pos = "!htbp",
       caption.above = TRUE,
       caption = "The threat of deportation undercuts the adoption of anti-Black attitudes via acculturation among non-Black Latinxs",
       include.rmse = FALSE,
       include.adjrs = FALSE,
       custom.coef.map = list("acc2:threat" = "Acculturation x Threat",
                              "acc2" = "Acculturation",
                              "threat" = "Threat"),
       custom.model.names = c("Oppose BLM", "BLM Ineffective", "Anti-BLM FT",
                              "Oppose BLM", "BLM Ineffective", "BLM No Protest",
                              "BLM No Support"),
       stars = c(.1, .05, .01, .001),
       symbol = "\\dagger")

# opposition to black political interests --- yes controls (table 2)

texreg(l = list(outs_list[[1]][[2]],
                outs_list[[2]][[2]],
                form_list_out[[2]][[5]],
                form_list_out[[2]][[6]],
                form_list_out[[2]][[7]],
                form_list_out[[2]][[8]],
                form_list_out[[2]][[9]]),
       include.ci = FALSE,
       float.pos = "!htbp",
       caption.above = TRUE,
       caption = "The threat of deportation undercuts the adoption of anti-Black attitudes via acculturation among non-Black Latinxs",
       include.rmse = FALSE,
       include.adjrs = FALSE,
       custom.coef.map = list("acc2:threat" = "Acculturation x Threat",
                              "acc2" = "Acculturation",
                              "threat" = "Threat"),
       custom.model.names = c("Oppose BLM", "BLM Ineffective", "Anti-BLM FT",
                              "Oppose BLM", "BLM Ineffective", "BLM No Protest",
                              "BLM No Support"),
       stars = c(.1, .05, .01, .001),
       symbol = "\\dagger")
