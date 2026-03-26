#### purpose: producing figures 1 and 2 #### 

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

ggsave(plot = plot16_grob, width = 8, height = 2.5, filename = "cmpsres16.png")

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

# anti-black attitudes --- yes controls

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

# opposition to black political interests --- yes controls

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


#### cmps '20 --- anti-black beliefs --- predicted values (Reproducing Figure 1) ####

newdf2 = 
  cmps20l[, names(form_list_out[[2]][[1]]$coefficients[2:(length(form_list_out[[2]][[1]]$coefficients)-2)])] %>% 
  apply(X = ., MARGIN = 2, function(x) mean(x, na.rm = TRUE)) %>% 
  as.matrix() %>% 
  t %>% 
  as.data.frame %>% 
  slice(rep(1:n(), each = length(unique(cmps20l$threat)) * length(unique(cmps20l$acc2)))) %>% 
  mutate(cenfe_ws = 1, cenfe_nc = 0, cenfe_st = 0, miss_cty = 0) %>% 
  mutate(acc2 = rep(unique(cmps20l$acc2)[order(unique(cmps20l$acc2))], 4),
         threat = c(rep(unique(cmps20l$threat)[order(unique(cmps20l$threat))][1], 5),
                    rep(unique(cmps20l$threat)[order(unique(cmps20l$threat))][2], 5),
                    rep(unique(cmps20l$threat)[order(unique(cmps20l$threat))][3], 5),
                    rep(unique(cmps20l$threat)[order(unique(cmps20l$threat))][4], 5))) %>%
  filter(threat == 0 | threat == 1)

sddf1 = data.frame(
  est = predict(newdata = newdf2, object = form_list_out[[2]][[1]], se.fit = TRUE)$fit,
  se = predict(newdata = newdf2, object = form_list_out[[2]][[1]], se.fit = TRUE)$se.fit,
  acc2 = newdf2$acc2,
  threat = newdf2$threat
)

abs((sddf1$est[sddf1$acc2 == 1 & sddf1$threat == 1] - 
       sddf1$est[sddf1$acc2 == 0 & sddf1$threat == 1]) - 
      (sddf1$est[sddf1$acc2 == 1 & sddf1$threat == 0] - 
         sddf1$est[sddf1$acc2 == 0 & sddf1$threat == 0])) / 
  sqrt(wtd.var(cmps20l$rr[cmps20l$black_lat == 1],
               w = cmps20l$weight[cmps20l$black_lat == 1],
               na.rm = TRUE))


patts1 = data.frame(
  est = predict(newdata = newdf2, object = form_list_out[[2]][[1]], se.fit = TRUE)$fit,
  se = predict(newdata = newdf2, object = form_list_out[[2]][[1]], se.fit = TRUE)$se.fit,
  acc2 = newdf2$acc2,
  threat = newdf2$threat
) %>% ggplot() + 
  geom_hline(yintercept = weighted.mean(cmps20w$rr, w = cmps20w$weight),
             linetype = 2,
             size = .35) + 
  geom_hline(yintercept = weighted.mean(cmps20l$rr[cmps20l$black_lat == 1],
                                        w = cmps20l$weight[cmps20l$black_lat == 1]),
             linetype = 2,
             col = "grey44",
             size = .35) + 
  geom_hline(yintercept = weighted.mean(cmps20b$rr,
                                        w = cmps20b$weight),
             linetype = 2,
             col = "grey72",
             size = .35) + 
  annotate('text', label = paste0("White (", round(weighted.mean(cmps20w$rr, w = cmps20w$weight), 2), ")"), x = .75, y = .62,
           family = "serif",
           size = 2.15) + 
  annotate('text', label = paste0("Black Latinx (", round(weighted.mean(cmps20l$rr[cmps20l$black_lat == 1],
                                                                        w = cmps20l$weight[cmps20l$black_lat == 1]), 2), ")"),
           x = .22, y = .42,
           family = "serif",
           size = 2.15) +
  annotate('text', label = paste0("Black Non-Latinx (", round(weighted.mean(cmps20b$rr,
                                                                            w = cmps20b$weight), 2), ")"),
           x = .29, y = .36,
           family = "serif",
           size = 2.15) +
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
  labs(x = "Acculturation", y = "Racial Resentment", 
       col = "Threat",
       title = "A. Racial Resentment (CMPS '20)") + 
  theme_tufte(base_size = 6) + 
  theme(legend.key.size = unit(.2, 'cm'))

sddf2 = data.frame(
  est = predict(newdata = newdf2, object = form_list_out[[2]][[2]], se.fit = TRUE)$fit,
  se = predict(newdata = newdf2, object = form_list_out[[2]][[2]], se.fit = TRUE)$se.fit,
  acc2 = newdf2$acc2,
  threat = newdf2$threat
)

abs((sddf2$est[sddf2$acc2 == 1 & sddf2$threat == 1] - 
       sddf2$est[sddf2$acc2 == 0 & sddf2$threat == 1]) - 
      (sddf2$est[sddf2$acc2 == 1 & sddf2$threat == 0] - 
         sddf2$est[sddf2$acc2 == 0 & sddf2$threat == 0])) / 
  sqrt(wtd.var(cmps20l$black_stereotype_diff[cmps20l$black_lat == 1],
               w = cmps20l$weight[cmps20l$black_lat == 1],
               na.rm = TRUE))


patts2 = data.frame(
  est = predict(newdata = newdf2, object = form_list_out[[2]][[2]], se.fit = TRUE)$fit,
  se = predict(newdata = newdf2, object = form_list_out[[2]][[2]], se.fit = TRUE)$se.fit,
  acc2 = newdf2$acc2,
  threat = newdf2$threat
) %>% ggplot() + 
  geom_hline(yintercept = weighted.mean(cmps20w$black_stereotype_diff, w = cmps20w$weight),
             linetype = 2,
             size = .35) + 
  geom_hline(yintercept = weighted.mean(cmps20l$black_stereotype_diff[cmps20l$black_lat == 1],
                                        w = cmps20l$weight[cmps20l$black_lat == 1]),
             linetype = 2,
             col = "grey44",
             size = .35) + 
  geom_hline(yintercept = weighted.mean(cmps20b$black_stereotype_diff,
                                        w = cmps20b$weight),
             linetype = 2,
             col = "grey72",
             size = .35) + 
  annotate('text', label = paste0("White (", round(weighted.mean(cmps20w$black_stereotype_diff, w = cmps20w$weight), 2), ")"), x = .75, y = .1,
           family = "serif",
           size = 2.15) + 
  annotate('text', label = paste0("Black Latinx (", round(weighted.mean(cmps20l$black_stereotype_diff[cmps20l$black_lat == 1],
                                                                        w = cmps20l$weight[cmps20l$black_lat == 1]), 2), ")"), x = .23, y = -.08,
           family = "serif",
           size = 2.15) +
  annotate('text', label = paste0("Black Non-Latinx (", round(weighted.mean(cmps20b$black_stereotype_diff,
                                                                            w = cmps20b$weight), 2), ")"), x = .325, y = -.1,
           family = "serif",
           size = 2.15) +
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
  labs(x = "Acculturation", y = "Anti-Black Stereotype", 
       col = "Threat",
       title = "B. Anti-Black Stereotype (CMPS '20)") + 
  theme_tufte(base_size = 6) + 
  theme(legend.key.size = unit(.2, 'cm'))

patts3 = data.frame(
  est = predict(newdata = newdf2, object = form_list_out[[2]][[3]], se.fit = TRUE)$fit,
  se = predict(newdata = newdf2, object = form_list_out[[2]][[3]], se.fit = TRUE)$se.fit,
  acc2 = newdf2$acc2,
  threat = newdf2$threat
) %>% ggplot() + 
  geom_hline(yintercept = mean(cmps20w$black_threat),
             linetype = 2,
             size = .35) + 
  geom_hline(yintercept = mean(cmps20l$black_threat[cmps20l$black_lat == 1]),
             linetype = 2,
             col = "grey44",
             size = .35) + 
  geom_hline(yintercept = mean(cmps20b$black_threat),
             linetype = 2,
             col = "grey72",
             size = .35) + 
  annotate('text', label = paste0("White (", round(weighted.mean(cmps20w$black_threat, w = cmps20w$weight), 2), ")"), x = .75, y = .075,
           family = "serif",
           size = 2.15) + 
  annotate('text', label = paste0("Black Latinx (", round(weighted.mean(cmps20l$black_threat[cmps20l$black_lat == 1],
                                                                        w = cmps20l$weight[cmps20l$black_lat == 1]), 2), ")"), x = .25, y = -.16,
           family = "serif",
           size = 2.15) +
  annotate('text', label = paste0("Black Non-Latinx (", round(weighted.mean(cmps20b$black_threat,
                                                                            w = cmps20b$weight), 2), ")"), x = .33, y = -.25,
           family = "serif",
           size = 2.15) +
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
  labs(x = "Acculturation", y = "Black = Threat", 
       col = "Threat",
       title = "C. Black = Threat (CMPS '20)") + 
  theme_tufte(base_size = 6) + 
  theme(legend.key.size = unit(.2, 'cm'))

patts4 = data.frame(
  est = predict(newdata = newdf2, object = form_list_out[[2]][[4]], se.fit = TRUE)$fit,
  se = predict(newdata = newdf2, object = form_list_out[[2]][[4]], se.fit = TRUE)$se.fit,
  acc2 = newdf2$acc2,
  threat = newdf2$threat
) %>% ggplot() + 
  geom_hline(yintercept = mean(cmps20w$white_hood_rank_diff),
             linetype = 2,
             size = .35) + 
  geom_hline(yintercept = mean(cmps20l$white_hood_rank_diff[cmps20l$black_lat == 1]),
             linetype = 2,
             col = "grey44",
             size = .35) + 
  geom_hline(yintercept = mean(cmps20b$white_hood_rank_diff),
             linetype = 2,
             col = "grey72",
             size = .35) + 
  annotate('text', label = paste0("White (", round(weighted.mean(cmps20w$white_hood_rank_diff, w = cmps20w$weight), 2), ")"), x = .75, y = .52,
           family = "serif",
           size = 2.15) + 
  annotate('text', label = paste0("Black Latinx (",
                                  round(weighted.mean(cmps20l$white_hood_rank_diff[cmps20l$black_lat == 1],
                                                      w = cmps20l$weight[cmps20l$black_lat == 1]), 2), ")"), x = .25, y = -.01,
           family = "serif",
           size = 2.15) +
  annotate('text', label = paste0("Black Non-Latinx (",
                                  round(weighted.mean(cmps20b$black_threat,
                                                      w = cmps20b$weight), 2), ")"), x = .33, y = -.3,
           family = "serif",
           size = 2.15) +
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
  labs(x = "Acculturation", y = "Prefer White Residence", 
       col = "Threat",
       title = "D. Prefer White Residence (CMPS '20)") + 
  theme_tufte(base_size = 6) + 
  theme(legend.key.size = unit(.2, 'cm'))

patts_grob = arrangeGrob(patts1, patts2, patts3, patts4, ncol = 4)

ggsave(plot = patts_grob, width = 8, height = 2, filename = "antiblk.png")

#### pred values: presentation format #### 


patts1_p1 = data.frame(
  est = predict(newdata = newdf2, object = form_list_out[[2]][[1]], se.fit = TRUE)$fit,
  se = predict(newdata = newdf2, object = form_list_out[[2]][[1]], se.fit = TRUE)$se.fit,
  acc2 = newdf2$acc2,
  threat = newdf2$threat
) %>% ggplot() + 
  geom_hline(yintercept = weighted.mean(cmps20w$rr, w = cmps20w$weight),
             linetype = 2,
             size = .35) + 
  geom_hline(yintercept = weighted.mean(cmps20l$rr[cmps20l$black_lat == 1],
                                        w = cmps20l$weight[cmps20l$black_lat == 1]),
             linetype = 2,
             col = "grey44",
             size = .35) + 
  geom_hline(yintercept = weighted.mean(cmps20b$rr,
                                        w = cmps20b$weight),
             linetype = 2,
             col = "grey72",
             size = .35) + 
  annotate('text', label = paste0("White (", round(weighted.mean(cmps20w$rr, w = cmps20w$weight), 2), ")"), x = .75, y = .62,
           family = "serif",
           size = 2.15) + 
  annotate('text', label = paste0("Black Latinx (", round(weighted.mean(cmps20l$rr[cmps20l$black_lat == 1],
                                                                        w = cmps20l$weight[cmps20l$black_lat == 1]), 2), ")"),
           x = .22, y = .42,
           family = "serif",
           size = 2.15) +
  annotate('text', label = paste0("Black Non-Latinx (", round(weighted.mean(cmps20b$rr,
                                                                            w = cmps20b$weight), 2), ")"),
           x = .29, y = .36,
           family = "serif",
           size = 2.15) +
  geom_point(aes(x = acc2, y = est, col = factor(threat)),
             position = position_dodge(.075),
             size = 0) +
  geom_errorbar(aes(x = acc2,
                    ymin = est - 1.645 * se,
                    ymax = est + 1.645 * se,
                    col = factor(threat)),
                width = 0,
                position = position_dodge(.075),
                size = 0) + 
  geom_errorbar(aes(x = acc2,
                    ymin = est - 1.96 * se,
                    ymax = est + 1.96 * se,
                    col = factor(threat)),
                width = 0,
                position = position_dodge(.075),
                size = 0) + 
  geom_line(aes(x = acc2, y = est, col = factor(threat)),
            position = position_dodge(.075),
            size = 0) + 
  scale_color_grey(start = .6, end = .2) + 
  labs(x = "Acculturation", y = "Racial Resentment", 
       col = "Threat",
       title = "A. Racial Resentment (CMPS '20)") + 
  theme_tufte(base_size = 6) + 
  theme(legend.key.size = unit(.2, 'cm'))

patts1_p2 = data.frame(
  est = predict(newdata = newdf2, object = form_list_out[[2]][[1]], se.fit = TRUE)$fit,
  se = predict(newdata = newdf2, object = form_list_out[[2]][[1]], se.fit = TRUE)$se.fit,
  acc2 = newdf2$acc2,
  threat = newdf2$threat
) %>% ggplot() + 
  geom_hline(yintercept = weighted.mean(cmps20w$rr, w = cmps20w$weight),
             linetype = 2,
             size = .35) + 
  geom_hline(yintercept = weighted.mean(cmps20l$rr[cmps20l$black_lat == 1],
                                        w = cmps20l$weight[cmps20l$black_lat == 1]),
             linetype = 2,
             col = "grey44",
             size = .35) + 
  geom_hline(yintercept = weighted.mean(cmps20b$rr,
                                        w = cmps20b$weight),
             linetype = 2,
             col = "grey72",
             size = .35) + 
  annotate('text', label = paste0("White (", round(weighted.mean(cmps20w$rr, w = cmps20w$weight), 2), ")"), x = .75, y = .62,
           family = "serif",
           size = 2.15) + 
  annotate('text', label = paste0("Black Latinx (", round(weighted.mean(cmps20l$rr[cmps20l$black_lat == 1],
                                                                        w = cmps20l$weight[cmps20l$black_lat == 1]), 2), ")"),
           x = .22, y = .42,
           family = "serif",
           size = 2.15) +
  annotate('text', label = paste0("Black Non-Latinx (", round(weighted.mean(cmps20b$rr,
                                                                            w = cmps20b$weight), 2), ")"),
           x = .29, y = .36,
           family = "serif",
           size = 2.15) +
  geom_point(aes(x = acc2, y = est, col = factor(threat)),
             position = position_dodge(.075),
             size = 0) +
  geom_errorbar(aes(x = acc2,
                    ymin = est - 1.645 * se,
                    ymax = est + 1.645 * se,
                    col = factor(threat)),
                width = 0,
                position = position_dodge(.075),
                size = 0) + 
  geom_errorbar(aes(x = acc2,
                    ymin = est - 1.96 * se,
                    ymax = est + 1.96 * se,
                    col = factor(threat)),
                width = 0,
                position = position_dodge(.075),
                size = 0) + 
  geom_line(aes(x = acc2, y = est, col = factor(threat)),
            position = position_dodge(.075),
            size = 0) + 
  scale_color_grey(start = .6, end = .2) + 
  labs(x = "Acculturation", y = "Racial Resentment", 
       col = "Threat",
       title = "A. Racial Resentment (CMPS '20)") + 
  theme_tufte(base_size = 6) + 
  theme(legend.key.size = unit(.2, 'cm'))


patts1_p2 = data.frame(
  est = predict(newdata = newdf2, object = form_list_out[[2]][[1]], se.fit = TRUE)$fit,
  se = predict(newdata = newdf2, object = form_list_out[[2]][[1]], se.fit = TRUE)$se.fit,
  acc2 = newdf2$acc2,
  threat = newdf2$threat
) %>% 
  mutate(threat = factor(threat, levels = c(0, 1))) %>% 
  filter(threat == 0) %>% 
  ggplot() + 
  geom_hline(yintercept = weighted.mean(cmps20w$rr, w = cmps20w$weight),
             linetype = 2,
             size = .35) + 
  geom_hline(yintercept = weighted.mean(cmps20l$rr[cmps20l$black_lat == 1],
                                        w = cmps20l$weight[cmps20l$black_lat == 1]),
             linetype = 2,
             col = "grey44",
             size = .35) + 
  geom_hline(yintercept = weighted.mean(cmps20b$rr,
                                        w = cmps20b$weight),
             linetype = 2,
             col = "grey72",
             size = .35) + 
  annotate('text', label = paste0("White (", round(weighted.mean(cmps20w$rr, w = cmps20w$weight), 2), ")"), x = .75, y = .62,
           family = "serif",
           size = 2.15) + 
  annotate('text', label = paste0("Black Latinx (", round(weighted.mean(cmps20l$rr[cmps20l$black_lat == 1],
                                                                        w = cmps20l$weight[cmps20l$black_lat == 1]), 2), ")"),
           x = .22, y = .42,
           family = "serif",
           size = 2.15) +
  annotate('text', label = paste0("Black Non-Latinx (", round(weighted.mean(cmps20b$rr,
                                                                            w = cmps20b$weight), 2), ")"),
           x = .29, y = .36,
           family = "serif",
           size = 2.15) +
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
  labs(x = "Acculturation", y = "Racial Resentment", 
       col = "Threat",
       title = "A. Racial Resentment (CMPS '20)") + 
  theme_tufte(base_size = 6) + 
  theme(legend.key.size = unit(.2, 'cm'))


patts1 = data.frame(
  est = predict(newdata = newdf2, object = form_list_out[[2]][[1]], se.fit = TRUE)$fit,
  se = predict(newdata = newdf2, object = form_list_out[[2]][[1]], se.fit = TRUE)$se.fit,
  acc2 = newdf2$acc2,
  threat = newdf2$threat
) %>% ggplot() + 
  geom_hline(yintercept = weighted.mean(cmps20w$rr, w = cmps20w$weight),
             linetype = 2,
             size = .35) + 
  geom_hline(yintercept = weighted.mean(cmps20l$rr[cmps20l$black_lat == 1],
                                        w = cmps20l$weight[cmps20l$black_lat == 1]),
             linetype = 2,
             col = "grey44",
             size = .35) + 
  geom_hline(yintercept = weighted.mean(cmps20b$rr,
                                        w = cmps20b$weight),
             linetype = 2,
             col = "grey72",
             size = .35) + 
  annotate('text', label = paste0("White (", round(weighted.mean(cmps20w$rr, w = cmps20w$weight), 2), ")"), x = .75, y = .62,
           family = "serif",
           size = 2.15) + 
  annotate('text', label = paste0("Black Latinx (", round(weighted.mean(cmps20l$rr[cmps20l$black_lat == 1],
                                                                        w = cmps20l$weight[cmps20l$black_lat == 1]), 2), ")"),
           x = .22, y = .42,
           family = "serif",
           size = 2.15) +
  annotate('text', label = paste0("Black Non-Latinx (", round(weighted.mean(cmps20b$rr,
                                                                            w = cmps20b$weight), 2), ")"),
           x = .29, y = .36,
           family = "serif",
           size = 2.15) +
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
  labs(x = "Acculturation", y = "Racial Resentment", 
       col = "Threat",
       title = "A. Racial Resentment (CMPS '20)") + 
  theme_tufte(base_size = 6) + 
  theme(legend.key.size = unit(.2, 'cm'))

sddf2 = data.frame(
  est = predict(newdata = newdf2, object = form_list_out[[2]][[2]], se.fit = TRUE)$fit,
  se = predict(newdata = newdf2, object = form_list_out[[2]][[2]], se.fit = TRUE)$se.fit,
  acc2 = newdf2$acc2,
  threat = newdf2$threat
)

abs((sddf2$est[sddf2$acc2 == 1 & sddf2$threat == 1] - 
       sddf2$est[sddf2$acc2 == 0 & sddf2$threat == 1]) - 
      (sddf2$est[sddf2$acc2 == 1 & sddf2$threat == 0] - 
         sddf2$est[sddf2$acc2 == 0 & sddf2$threat == 0])) / 
  sqrt(wtd.var(cmps20l$black_stereotype_diff[cmps20l$black_lat == 1],
               w = cmps20l$weight[cmps20l$black_lat == 1],
               na.rm = TRUE))


patts2 = data.frame(
  est = predict(newdata = newdf2, object = form_list_out[[2]][[2]], se.fit = TRUE)$fit,
  se = predict(newdata = newdf2, object = form_list_out[[2]][[2]], se.fit = TRUE)$se.fit,
  acc2 = newdf2$acc2,
  threat = newdf2$threat
) %>% ggplot() + 
  geom_hline(yintercept = weighted.mean(cmps20w$black_stereotype_diff, w = cmps20w$weight),
             linetype = 2,
             size = .35) + 
  geom_hline(yintercept = weighted.mean(cmps20l$black_stereotype_diff[cmps20l$black_lat == 1],
                                        w = cmps20l$weight[cmps20l$black_lat == 1]),
             linetype = 2,
             col = "grey44",
             size = .35) + 
  geom_hline(yintercept = weighted.mean(cmps20b$black_stereotype_diff,
                                        w = cmps20b$weight),
             linetype = 2,
             col = "grey72",
             size = .35) + 
  annotate('text', label = paste0("White (", round(weighted.mean(cmps20w$black_stereotype_diff, w = cmps20w$weight), 2), ")"), x = .75, y = .1,
           family = "serif",
           size = 2.15) + 
  annotate('text', label = paste0("Black Latinx (", round(weighted.mean(cmps20l$black_stereotype_diff[cmps20l$black_lat == 1],
                                                                        w = cmps20l$weight[cmps20l$black_lat == 1]), 2), ")"), x = .23, y = -.08,
           family = "serif",
           size = 2.15) +
  annotate('text', label = paste0("Black Non-Latinx (", round(weighted.mean(cmps20b$black_stereotype_diff,
                                                                            w = cmps20b$weight), 2), ")"), x = .325, y = -.1,
           family = "serif",
           size = 2.15) +
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
  labs(x = "Acculturation", y = "Anti-Black Stereotype", 
       col = "Threat",
       title = "B. Anti-Black Stereotype (CMPS '20)") + 
  theme_tufte(base_size = 6) + 
  theme(legend.key.size = unit(.2, 'cm'))

patts3 = data.frame(
  est = predict(newdata = newdf2, object = form_list_out[[2]][[3]], se.fit = TRUE)$fit,
  se = predict(newdata = newdf2, object = form_list_out[[2]][[3]], se.fit = TRUE)$se.fit,
  acc2 = newdf2$acc2,
  threat = newdf2$threat
) %>% ggplot() + 
  geom_hline(yintercept = mean(cmps20w$black_threat),
             linetype = 2,
             size = .35) + 
  geom_hline(yintercept = mean(cmps20l$black_threat[cmps20l$black_lat == 1]),
             linetype = 2,
             col = "grey44",
             size = .35) + 
  geom_hline(yintercept = mean(cmps20b$black_threat),
             linetype = 2,
             col = "grey72",
             size = .35) + 
  annotate('text', label = paste0("White (", round(weighted.mean(cmps20w$black_threat, w = cmps20w$weight), 2), ")"), x = .75, y = .075,
           family = "serif",
           size = 2.15) + 
  annotate('text', label = paste0("Black Latinx (", round(weighted.mean(cmps20l$black_threat[cmps20l$black_lat == 1],
                                                                        w = cmps20l$weight[cmps20l$black_lat == 1]), 2), ")"), x = .25, y = -.16,
           family = "serif",
           size = 2.15) +
  annotate('text', label = paste0("Black Non-Latinx (", round(weighted.mean(cmps20b$black_threat,
                                                                            w = cmps20b$weight), 2), ")"), x = .33, y = -.25,
           family = "serif",
           size = 2.15) +
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
  labs(x = "Acculturation", y = "Black = Threat", 
       col = "Threat",
       title = "C. Black = Threat (CMPS '20)") + 
  theme_tufte(base_size = 6) + 
  theme(legend.key.size = unit(.2, 'cm'))

patts4 = data.frame(
  est = predict(newdata = newdf2, object = form_list_out[[2]][[4]], se.fit = TRUE)$fit,
  se = predict(newdata = newdf2, object = form_list_out[[2]][[4]], se.fit = TRUE)$se.fit,
  acc2 = newdf2$acc2,
  threat = newdf2$threat
) %>% ggplot() + 
  geom_hline(yintercept = mean(cmps20w$white_hood_rank_diff),
             linetype = 2,
             size = .35) + 
  geom_hline(yintercept = mean(cmps20l$white_hood_rank_diff[cmps20l$black_lat == 1]),
             linetype = 2,
             col = "grey44",
             size = .35) + 
  geom_hline(yintercept = mean(cmps20b$white_hood_rank_diff),
             linetype = 2,
             col = "grey72",
             size = .35) + 
  annotate('text', label = paste0("White (", round(weighted.mean(cmps20w$white_hood_rank_diff, w = cmps20w$weight), 2), ")"), x = .75, y = .52,
           family = "serif",
           size = 2.15) + 
  annotate('text', label = paste0("Black Latinx (",
                                  round(weighted.mean(cmps20l$white_hood_rank_diff[cmps20l$black_lat == 1],
                                                      w = cmps20l$weight[cmps20l$black_lat == 1]), 2), ")"), x = .25, y = -.01,
           family = "serif",
           size = 2.15) +
  annotate('text', label = paste0("Black Non-Latinx (",
                                  round(weighted.mean(cmps20b$black_threat,
                                                      w = cmps20b$weight), 2), ")"), x = .33, y = -.3,
           family = "serif",
           size = 2.15) +
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
  labs(x = "Acculturation", y = "Prefer White Residence", 
       col = "Threat",
       title = "D. Prefer White Residence (CMPS '20)") + 
  theme_tufte(base_size = 6) + 
  theme(legend.key.size = unit(.2, 'cm'))

patts_grob = arrangeGrob(patts1, patts2, patts3, patts4, ncol = 4)

ggsave(plot = patts_grob, width = 8, height = 2, filename = "antiblk.png")


#### cmps '20 --- black political interests (Reproducing Figure 2) #### 

patts5 = data.frame(
  est = predict(newdata = newdf2, object = form_list_out[[2]][[5]], se.fit = TRUE)$fit,
  se = predict(newdata = newdf2, object = form_list_out[[2]][[5]], se.fit = TRUE)$se.fit,
  acc2 = newdf2$acc2,
  threat = newdf2$threat
) %>% ggplot() + 
  geom_hline(yintercept = weighted.mean(cmps20w$blm_ft, w = cmps20w$weight),
             linetype = 2,
             size = .35) + 
  geom_hline(yintercept = weighted.mean(cmps20l$blm_ft[cmps20l$black_lat == 1],
                                        w = cmps20l$weight[cmps20l$black_lat == 1]),
             linetype = 2,
             col = "grey44",
             size = .35) + 
  geom_hline(yintercept = weighted.mean(cmps20b$blm_ft,
                                        w = cmps20b$weight),
             linetype = 2,
             col = "grey72",
             size = .35) + 
  annotate('text', label = paste0("White (", round(weighted.mean(cmps20w$blm_ft, w = cmps20w$weight), 2), ")"), x = .75, y = .63,
           family = "serif",
           size = 2.15) + 
  annotate('text', label = paste0("Black Latinx (", round(weighted.mean(cmps20l$blm_ft[cmps20l$black_lat == 1],
                                                                        w = cmps20l$weight[cmps20l$black_lat == 1]), 2), ")"),
           x = .22, y = .47,
           family = "serif",
           size = 2.15) +
  annotate('text', label = paste0("Black Non-Latinx (", round(weighted.mean(cmps20b$blm_ft,
                                                                            w = cmps20b$weight), 2), ")"),
           x = .29, y = .39,
           family = "serif",
           size = 2.15) +
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
  labs(x = "Acculturation", y = "Anti-BLM FT", 
       col = "Threat",
       title = "E. Anti-BLM FT (CMPS '20)") + 
  theme_tufte(base_size = 6) + 
  theme(legend.key.size = unit(.2, 'cm'))

patts6 = data.frame(
  est = predict(newdata = newdf2, object = form_list_out[[2]][[6]], se.fit = TRUE)$fit,
  se = predict(newdata = newdf2, object = form_list_out[[2]][[6]], se.fit = TRUE)$se.fit,
  acc2 = newdf2$acc2,
  threat = newdf2$threat
) %>% ggplot() + 
  geom_hline(yintercept = weighted.mean(cmps20w$blm_supports, w = cmps20w$weight),
             linetype = 2,
             size = .35) + 
  geom_hline(yintercept = weighted.mean(cmps20l$blm_supports[cmps20l$black_lat == 1],
                                        w = cmps20l$weight[cmps20l$black_lat == 1]),
             linetype = 2,
             col = "grey44",
             size = .35) + 
  geom_hline(yintercept = weighted.mean(cmps20b$blm_supports,
                                        w = cmps20b$weight),
             linetype = 2,
             col = "grey72",
             size = .35) + 
  annotate('text', label = paste0("White (", round(weighted.mean(cmps20w$blm_supports, w = cmps20w$weight), 2), ")"), x = .75, y = .62,
           family = "serif",
           size = 2.15) + 
  annotate('text', label = paste0("Black Latinx (", round(weighted.mean(cmps20l$blm_supports[cmps20l$black_lat == 1],
                                                                        w = cmps20l$weight[cmps20l$black_lat == 1]), 2), ")"), x = .8, y = .425,
           family = "serif",
           size = 2.15) +
  annotate('text', label = paste0("Black Non-Latinx (", round(weighted.mean(cmps20b$blm_supports,
                                                                            w = cmps20b$weight), 2), ")"), x = .3, y = .28,
           family = "serif",
           size = 2.15) +
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
  labs(x = "Acculturation", y = "BLM Opposition", 
       col = "Threat",
       title = "C. Oppose BLM (CMPS '20)") + 
  theme_tufte(base_size = 6) + 
  theme(legend.key.size = unit(.2, 'cm'))

patts7 = data.frame(
  est = predict(newdata = newdf2, object = form_list_out[[2]][[7]], se.fit = TRUE)$fit,
  se = predict(newdata = newdf2, object = form_list_out[[2]][[7]], se.fit = TRUE)$se.fit,
  acc2 = newdf2$acc2,
  threat = newdf2$threat
) %>% ggplot() + 
  geom_hline(yintercept = weighted.mean(cmps20w$blm_eff, w = cmps20w$weight),
             linetype = 2,
             size = .35) + 
  geom_hline(yintercept = weighted.mean(cmps20l$blm_eff[cmps20l$black_lat == 1],
                                        w = cmps20l$weight[cmps20l$black_lat == 1]),
             linetype = 2,
             col = "grey44",
             size = .35) + 
  geom_hline(yintercept = weighted.mean(cmps20b$blm_eff,
                                        w = cmps20b$weight),
             linetype = 2,
             col = "grey72",
             size = .35) + 
  annotate('text', label = paste0("White (", round(weighted.mean(cmps20w$blm_eff, w = cmps20w$weight), 2), ")"), x = .75, y = .55,
           family = "serif",
           size = 2.15) + 
  annotate('text', label = paste0("Black Latinx (", round(weighted.mean(cmps20l$blm_eff[cmps20l$black_lat == 1],
                                                                        w = cmps20l$weight[cmps20l$black_lat == 1]), 2), ")"), x = .3, y = .425,
           family = "serif",
           size = 2.15) +
  annotate('text', label = paste0("Black Non-Latinx (", round(weighted.mean(cmps20b$blm_eff,
                                                                            w = cmps20b$weight), 2), ")"), x = .3, y = .25,
           family = "serif",
           size = 2.15) +
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
  labs(x = "Acculturation", y = "BLM Ineffective", 
       col = "Threat",
       title = "D. BLM Ineffective (CMPS '20)") + 
  theme_tufte(base_size = 6) + 
  theme(legend.key.size = unit(.2, 'cm'))

patts8 = data.frame(
  est = predict(newdata = newdf2, object = form_list_out[[2]][[8]], se.fit = TRUE)$fit,
  se = predict(newdata = newdf2, object = form_list_out[[2]][[8]], se.fit = TRUE)$se.fit,
  acc2 = newdf2$acc2,
  threat = newdf2$threat
) %>% ggplot() + 
  geom_hline(yintercept = weighted.mean(cmps20w$blm_protest, w = cmps20w$weight),
             linetype = 2,
             size = .35) + 
  geom_hline(yintercept = weighted.mean(cmps20l$blm_protest[cmps20l$black_lat == 1],
                                        w = cmps20l$weight[cmps20l$black_lat == 1]),
             linetype = 2,
             col = "grey44",
             size = .35) + 
  geom_hline(yintercept = weighted.mean(cmps20b$blm_protest,
                                        w = cmps20b$weight),
             linetype = 2,
             col = "grey72",
             size = .35) + 
  annotate('text', label = paste0("White (", round(weighted.mean(cmps20w$blm_protest, w = cmps20w$weight), 2), ")"), x = .75, y = .95,
           family = "serif",
           size = 2.15) + 
  annotate('text', label = paste0("Black Latinx (", round(weighted.mean(cmps20l$blm_protest[cmps20l$black_lat == 1],
                                                                        w = cmps20l$weight[cmps20l$black_lat == 1]), 2), ")"), x = .25, y = .67,
           family = "serif",
           size = 2.15) +
  annotate('text', label = paste0("Black Non-Latinx (", round(weighted.mean(cmps20b$blm_protest,
                                                                            w = cmps20b$weight), 2), ")"), x = .75, y = .8,
           family = "serif",
           size = 2.15) +
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
  labs(x = "Acculturation", y = "BLM Protest Non-Participation", 
       col = "Threat",
       title = "F. No BLM Protest (CMPS '20)") + 
  theme_tufte(base_size = 6) + 
  theme(legend.key.size = unit(.2, 'cm'))

patts9 = data.frame(
  est = predict(newdata = newdf2, object = form_list_out[[2]][[9]], se.fit = TRUE)$fit,
  se = predict(newdata = newdf2, object = form_list_out[[2]][[9]], se.fit = TRUE)$se.fit,
  acc2 = newdf2$acc2,
  threat = newdf2$threat
) %>% ggplot() + 
  geom_hline(yintercept = weighted.mean(cmps20w$blm_nprotest, w = cmps20w$weight),
             linetype = 2,
             size = .35) + 
  geom_hline(yintercept = weighted.mean(cmps20l$blm_nprotest[cmps20l$black_lat == 1],
                                        w = cmps20l$weight[cmps20l$black_lat == 1]),
             linetype = 2,
             col = "grey44",
             size = .35) + 
  geom_hline(yintercept = weighted.mean(cmps20b$blm_nprotest,
                                        w = cmps20b$weight),
             linetype = 2,
             col = "grey72",
             size = .35) + 
  annotate('text', label = paste0("White (", round(weighted.mean(cmps20w$blm_nprotest, w = cmps20w$weight), 2), ")"), x = .75, y = .91,
           family = "serif",
           size = 2.15) + 
  annotate('text', label = paste0("Black Latinx (", round(weighted.mean(cmps20l$blm_nprotest[cmps20l$black_lat == 1],
                                                                        w = cmps20l$weight[cmps20l$black_lat == 1]), 2), ")"), x = .25, y = .58,
           family = "serif",
           size = 2.15) +
  annotate('text', label = paste0("Black Non-Latinx (", round(weighted.mean(cmps20b$blm_nprotest,
                                                                            w = cmps20b$weight), 2), ")"), x = .3, y = .63,
           family = "serif",
           size = 2.15) +
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
  labs(x = "Acculturation", y = "BLM Protest No Support", 
       col = "Threat",
       title = "G. No BLM Protest Support (CMPS '20)") + 
  theme_tufte(base_size = 6) + 
  theme(legend.key.size = unit(.2, 'cm'))

plot_polatts = 
  arrangeGrob(plot16_1, plot16_2, patts6, patts7,
              grid::nullGrob(), patts5, patts8, patts9, grid::nullGrob(),
              layout_matrix = matrix(c(1, 1, 2, 2, 3, 3, 4, 4,
                                       5, 6, 6, 7, 7, 8, 8, 9),
                                     byrow = TRUE, ncol = 8))

ggsave(filename = "polatts.png", plot = plot_polatts, width = 8, height = 4)