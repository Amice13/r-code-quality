#### purpose: producing figure n10 #### 

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

#### demonstrating threat = dispositional ot #### 

# reading in pew surveys 

pew07 = read_stata("pew_surveys/pew07/pew2007original.dta") %>% 
  mutate(threat = abs(ifelse(qn33 == 8 | qn33 == 9, NA, qn33) - 4)) %>% 
  dplyr::select(weight, threat) %>% 
  mutate(year = 2007) %>% 
  mutate(threat = threat / max(threat, na.rm = TRUE))

pew08 = read_spss("pew_surveys/pew2008/PHCNSL2008aPublicRelease.sav")
pew08$threat = ifelse(is.na(pew08$qn37), pew08$qn38, pew08$qn37)
pew08 = pew08 %>% 
  mutate(threat = abs(ifelse(threat == 8 | threat == 9, NA, threat) - 4)) %>% 
  dplyr::select(weight, threat) %>% 
  mutate(year = 2008) %>% 
  mutate(threat = threat / max(threat, na.rm = TRUE))

pew10 = read_spss("pew_surveys/pew2010/PHCNSL2010PublicRelease.sav") %>% 
  mutate(threat = abs(ifelse(qn32 == 8 | qn32 == 9, NA, qn32) - 4)) %>% 
  mutate(year = 2010) %>% 
  dplyr::select(threat, weight, year) %>% 
  mutate(threat = threat / max(threat, na.rm = TRUE))

pew13 = read_spss("pew_surveys/pew2013/HISPANIC2013PUBLICRELEASE.sav") %>% 
  mutate(threat = abs(ifelse(im34 == 9, NA, im34) - 4)) %>% 
  mutate(year = 2013) %>% 
  dplyr::select(threat, weight, year) %>% 
  mutate(threat = threat / max(threat, na.rm = TRUE))

pew18 = read_spss("pew_surveys/pew2018/NSL 2018_FOR RELEASE.sav") %>% 
  mutate(threat = abs(ifelse(qn24 == 8 | qn24 == 9, NA, qn24) - 4)) %>% 
  mutate(year = 2018) %>% 
  dplyr::select(threat, weight, year) %>% 
  mutate(threat = threat / max(threat, na.rm = TRUE))

pew19 = read_spss("pew_surveys/pew2019/W58_Dec19/ATP W58.sav") %>% 
  mutate(threat = abs(ifelse(WORRYDPORT_W58 == 99, NA, WORRYDPORT_W58) - 4)) %>% 
  mutate(year = 2019) %>% 
  mutate(weight = WEIGHT_W58) %>% 
  dplyr::select(threat, weight, year) %>% 
  mutate(threat = threat / max(threat, na.rm = TRUE))

threat_df_a = bind_rows(
  pew07,
  pew08,
  pew10,
  pew13,
  pew18
)

threatotdf = bind_rows(
  pew07,
  pew08,
  pew10,
  pew13,
  pew18
) %>% 
  group_by(year) %>% 
  summarize(threat = weighted.mean(threat, weight, na.rm = TRUE))

threatot_plot = threatotdf %>% 
  ggplot + 
  geom_point(aes(x = year, y = threat)) +
  geom_line(aes(x = year, y = threat)) + 
  ylim(0, 1) + 
  labs(x = "Survey Year", y = "Deportation Threat",
       title = "A. Deportation Threat Over Time (Pew)") + 
  theme_tufte()

# testing if post-means = higher 

model_threat_ot = 
  estimatr::lm_robust(threat ~ factor(year), data = threat_df_a, weight = weight)

threatot_plot2 = data.frame(
  est = model_threat_ot$coefficients[2:length(model_threat_ot$coefficients)],
  se = model_threat_ot$std.error[2:length(model_threat_ot$coefficients)],
  year = c("2008", "2010", "2013", "2018")
) %>% 
  ggplot() + 
  geom_point(aes(x = year, y = est),
             size = 2) + 
  geom_errorbar(aes(x = year, 
                    ymin = est - 1.96 * se,
                    ymax = est + 1.96 * se),
                width = 0,
                size = .4) +
  geom_errorbar(aes(x = year, 
                    ymin = est - 1.645 * se,
                    ymax = est + 1.645 * se),
                width = 0,
                size = .6) + 
  geom_hline(yintercept = 0) + 
  labs(x = "Year", y = "Coefficient",
       title = "B. Year Effects (Pew)") + 
  theme_tufte()

threat_ot_plot_grob = arrangeGrob(threatot_plot, threatot_plot2, ncol = 2)
ggsave(plot = threat_ot_plot_grob, filename = "threatot.png", width = 8, height = 2.5)

#### demonstrating threat = dispositional in panel #### 

# gen data 

lines = read_dta("lines_data/LINES1617.Public.dta") %>% 
  mutate(deport_worry_post = abs(ifelse(deport_worry_post == 8 | deport_worry_post == 9, NA, 
                                        deport_worry_post) - 5),
         deport_worry_W3 = abs(ifelse(deport_worry_W3 == 8 | deport_worry_W3 == 9, NA, 
                                      deport_worry_W3) - 5)) %>% 
  mutate(
    libcon_post = abs(ifelse(libcon_post == 0 | libcon_post == 8 | libcon_post == 9, NA, libcon_post) - 7),
    libcon_W3 = abs(ifelse(libcon_W3 == 0 | libcon_W3 == 8 | libcon_W3 == 9, NA, libcon_W3) - 7)  
  ) %>% 
  mutate(
    pid3_post = 
      ifelse(ptyid_rptyid_post == 1, 2, ifelse(ptyid_rptyid_post == 2, 0, 1)),
    lean_gop_post = ifelse(pid3_post == 1 & ptyid_leanpty_post == 1, 1, 0),
    lean_dem_post = ifelse(pid3_post == 1 & ptyid_leanpty_post == 3, 1, 0),
    pid3_W3 = 
      ifelse(ptyid_rptyid_W3 == 1, 2, ifelse(ptyid_rptyid_W3 == 2, 0, 1)),
    lean_gop_W3 = ifelse(pid3_W3 == 1 & ptyid_leanpty_W3 == 1, 1, 0),
    lean_dem_W3 = ifelse(pid3_W3 == 1 & ptyid_leanpty_W3 == 3, 1, 0)
  ) %>% 
  mutate(pid5_post = ifelse(pid3_post == 0, 0,
                            ifelse(lean_gop_post == 1, 1,
                                   ifelse(lean_gop_post == 0 & lean_dem_post == 0, 2,
                                          ifelse(lean_dem_post == 1, 3, 4)))),
         pid5_W3 = ifelse(pid3_W3 == 0, 0,
                          ifelse(lean_gop_W3 == 1, 1,
                                 ifelse(lean_gop_W3 == 0 & lean_dem_W3 == 0, 2,
                                        ifelse(lean_dem_W3 == 1, 3, 4))))
  ) %>% 
  #dplyr::select(deport_worry_post, deport_worry_W3, libcon_post, libcon_W3,
  #              pid5_post, pid5_W3, weight) %>% 
  filter(!is.na(deport_worry_post) & !is.na(deport_worry_W3)) %>% 
  mutate(deport_worry_post2 = deport_worry_post,
         deport_worry_W32 = deport_worry_W3) %>% 
  mutate(deport_worry_post = deport_worry_post / max(deport_worry_post, na.rm = TRUE),
         deport_worry_W3 = deport_worry_W3 / max(deport_worry_W3, na.rm = TRUE)) %>% 
  mutate(deport_worry_bin_post = ifelse(deport_worry_post2 == 4 | deport_worry_post2 == 3, 1, 0),
         deport_worry_bin_W3 = ifelse(deport_worry_W32 == 4 | deport_worry_W32 == 3, 1, 0))

lines$pref_amer_w2 = ifelse(lines$dem_ethnic_group_pref_post == 2, 1, 0)
lines$pref_amer_w3 = ifelse(lines$dem_ethnic_group_pref_W3 == 2, 1, 0)
lines$pref_amer_w2 = ifelse(is.na(lines$pref_amer_w2), 0, lines$pref_amer_w2)
lines$pref_amer_w3 = ifelse(is.na(lines$pref_amer_w3), 0, lines$pref_amer_w3)

lines$amer_id_w2 = ifelse(lines$dem_ethnic_group_identity01_post == 2 | lines$pref_amer_w2 == 1, 1, 0)
lines$amer_id_w3 = ifelse(lines$dem_ethnic_group_identity01_W3 == 2, 1, 0)

lm_robust(I(deport_worry_W3 - deport_worry_post) ~ amer_id_w2, data = lines, weight = weight)
lm_robust(I(amer_id_w3 - amer_id_w2) ~ deport_worry_post, data = lines, weight = weight)

# gen panel 

lines_panel = 
  bind_rows(
    lines %>% dplyr::select(weight, deport_worry_post) %>% 
      rename(deport_worry = deport_worry_post) %>% 
      mutate(post = 0),
    lines %>% dplyr::select(weight, deport_worry_W3) %>% 
      rename(deport_worry = deport_worry_W3) %>% 
      mutate(post = 1)
  )

# period effect regression 

post_mod_lines = lm_robust(deport_worry ~ post, data = lines_panel, weight = weight)

# plot 

threat_ot_lines = data.frame(
  
  date = factor(c('November 2016-\nJanuary 2017', 'July 2017-\nSeptember 2017'),
                levels = c('November 2016-\nJanuary 2017', 'July 2017-\nSeptember 2017')),
  threat = c(weighted.mean(lines$deport_worry_post, w = lines$weight, na.rm = TRUE),
             weighted.mean(lines$deport_worry_W3, w = lines$weight, na.rm = TRUE))
  
) %>% 
  ggplot() + 
  geom_point(aes(x = date, y = threat)) +
  stat_summary(aes(x = date, y = threat, group = 1),
               fun.y=sum, geom="line") + 
  ylim(0, 1) + 
  labs(x = "Survey Period", y = "Deportation Threat",
       title = "C. Threat Over Time (LINES)") + 
  annotate('text', x = c(1, 2),
           y = c(weighted.mean(lines$deport_worry_post,
                               w = lines$weight, na.rm = TRUE),
                 weighted.mean(lines$deport_worry_W3,
                               w = lines$weight, na.rm = TRUE)) + .07,
           label = round(c(weighted.mean(lines$deport_worry_post,
                                         w = lines$weight, na.rm = TRUE),
                           weighted.mean(lines$deport_worry_W3,
                                         w = lines$weight, na.rm = TRUE)), 2),
           family = "serif") + 
  annotate("text",
           x = 1.75,
           y = .85,
           label = paste0("July-Sep '17 Effect\n",
                          "Est: ", round(post_mod_lines$coefficients[2], 3), "\n",
                          "SE: ", round(post_mod_lines$std.error[2], 3), "\n",
                          "p: ", round(post_mod_lines$p.value[2], 3)),
           family = "serif",
           size = 2.5) + 
  theme_tufte()

# now assessing stability 

stabilitydf = data.frame(
  
  c(weightedCorr(lines$deport_worry_post, lines$deport_worry_W3,
                 weights = lines$weight),
    weightedCorr(lines$libcon_post[!is.na(lines$libcon_post) & !is.na(lines$libcon_W3)],
                 lines$libcon_W3[!is.na(lines$libcon_post) & !is.na(lines$libcon_W3)],
                 weights = lines$weight[!is.na(lines$libcon_post) & !is.na(lines$libcon_W3)]),
    weightedCorr(lines$pid5_post[!is.na(lines$pid5_post) & !is.na(lines$pid5_W3)],
                 lines$pid5_W3[!is.na(lines$pid5_post) & !is.na(lines$pid5_W3)],
                 weights = lines$weight[!is.na(lines$pid5_post) & !is.na(lines$pid5_W3)])),
  
  factor(c("Threat", "Ideology (5pt)", "Party ID (5pt)"),
         levels = c("Threat", "Ideology (5pt)", "Party ID (5pt)"))
  
) %>% 
  `colnames<-` (c("Rho", "Covariate"))

threat_ot_lines2 = stabilitydf %>% 
  ggplot() + 
  geom_bar(aes(x = Covariate, y = Rho), stat = "identity") + 
  annotate("text", x = c(1, 2 ,3), y = stabilitydf$Rho + .05,
           label = round(stabilitydf$Rho, 2),
           family = "serif") + 
  ylim(0, 1) + 
  labs(x = "Covariate", title = "D. Test-Retest Reliability (LINES)",
       y = "Pearson's Rho") + 
  theme_tufte() 

grob_threat_ot_lines = arrangeGrob(threatot_plot, threatot_plot2, 
                                   threat_ot_lines, threat_ot_lines2, ncol = 2)
ggsave(plot = grob_threat_ot_lines, filename = "threat_ot_panel.png", width = 8, height = 4.5)
