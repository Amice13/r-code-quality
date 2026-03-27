windowsFonts(Times=windowsFont("Times New Roman"))

pacman::p_load(tidyverse,texreg,haven,lfe, alpaca)

###
## Load data ---
###

df_aut <- read_rds("morning_after_dataset_individuallevel.rds") %>% filter(democracy_lag == 0 & leader == 0 & !is.na(purged_y1) & !is.na(gdp_cap_pwt_ln) & !is.na(pop_pwt_ln))

##
# Get Number of coup attempts ---
##

df_aut_n <- df_aut %>% filter(!is.na(coupattempt_presentyear))

df_aut_names <- length(unique(df_aut_n$name))

n_countries <-  length(unique(df_aut_n$country_isocode))

table(df_aut_n$coupattempt_presentyear)

####
## Set up template for graphs ---
####

pd <- position_dodge(0.75)

plot_canvas <- function(df=df,x=name,y=estimate,se=se){
  ggplot(data = df, aes(y = estimate, x = name)) + 
    geom_point(position = pd) + 
    geom_errorbar(aes(ymin = estimate-1.96*se, ymax = estimate+1.96*se),
                  width = 0,
                  position = pd,
                  size = 1) +
    coord_flip() +
    ylim(-0.1,0.5) +
    labs(x = "", 
         y = "Marginal Effects",
         color = "black") +
    geom_hline(yintercept = 0,
               linetype = "dashed") +
    theme_bw() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
          legend.position = "",plot.title = element_text(size=14),
          axis.text.y = element_text(colour = "black",size=14),
          axis.text.x = element_text(colour = "black",size=14),
          text=element_text(family="Times",size=14,color="black"))
}

###
# Figure 3, 4, and 5 ---
###

# Responsibility ---

responsibility_felm <- felm(purged_y1 ~ minister_type/coupattempt_presentyear +
                              gdp_cap_pwt_ln + pop_pwt_ln + experience + I(experience^2) +
                              I(experience^3) | country_isocode+year | 0 | country_isocode , data=df_aut) %>% summary()

coef_responsibility <- tibble(name = c("Minister of Defense","Minister of Finance","Minister of Foreign Affairs",
                                       "Minister of Natural Resources","Other type of minister"),
                              estimate = c(responsibility_felm$coefficients[10],
                                           responsibility_felm$coefficients[11],
                                           responsibility_felm$coefficients[12],
                                           responsibility_felm$coefficients[13],
                                           responsibility_felm$coefficients[14]),
                              se = c(responsibility_felm$coefficients[10,2],
                                     responsibility_felm$coefficients[11,2],
                                     responsibility_felm$coefficients[12,2],
                                     responsibility_felm$coefficients[13,2],
                                     responsibility_felm$coefficients[14,2]
                              )) %>% mutate(name = fct_relevel(name,
                                                               "Minister of Natural Resources",
                                                               "Other type of minister",
                                                               "Minister of Finance",
                                                               "Minister of Foreign Affairs",
                                                               "Minister of Defense"
                              ))


plot_resp <- plot_canvas(coef_responsibility) + ggtitle("H3a: Ministerial responsibility")
plot_resp

# Fixed effects GLM
fe_glm_res <- feglm(purged_y1 ~ minister_type/coupattempt_presentyear +
                      gdp_cap_pwt_ln + pop_pwt_ln + experience + I(experience^2) +
                      I(experience^3) | country_isocode+year | country_isocode , data=df_aut)

apes.fe_glm_res <- getAPEs(fe_glm_res)


effects <- as.vector(apes.fe_glm_res$delta)
effects_felm <- as.vector(responsibility_felm$coefficients)
apes_effects <- summary(apes.fe_glm_res, cluster="country_isocode")
apes_effects <- as.data.frame(apes_effects$cm)

coef_responsibility_feglm <- tibble(name = c("Minister of Defense","Minister of Finance","Minister of Foreign Affairs",
                                       "Minister of Natural Resources","Other type of minister"),
                              estimate = c(apes_effects$Estimate[10],
                                           apes_effects$Estimate[11],
                                           apes_effects$Estimate[12],
                                           apes_effects$Estimate[13],
                                           apes_effects$Estimate[14]),
                              se = c(apes_effects$'Std. error'[10],
                                     apes_effects$'Std. error'[11],
                                     apes_effects$'Std. error'[12],
                                     apes_effects$'Std. error'[13],
                                     apes_effects$'Std. error'[14]
                              )) %>% mutate(name = fct_relevel(name,
                                                               "Minister of Natural Resources",
                                                               "Other type of minister",
                                                               "Minister of Finance",
                                                               "Minister of Foreign Affairs",
                                                               "Minister of Defense"
                              ))


plot_resp_feglm <- plot_canvas(coef_responsibility_feglm) + ggtitle("H3a: Ministerial responsibility - FEGLM")
plot_resp_feglm


# Affiliation ---

df_party <- df_aut %>% filter(party_group != "unknown") %>% mutate(party_group = as.factor(recode(party_group,"independent"="No party affiliation","leader_party"="From the leader's party","other_party"="From another party")))

df_party <- df_party %>% mutate(party_group = fct_relevel(party_group,
                                                          "From another party",
                                                          "No party affiliation",
                                                          "From the leader's party"
))

affiliation_felm <- felm(purged_y1 ~ party_group/coupattempt_presentyear +
                           gdp_cap_pwt_ln + pop_pwt_ln + experience + I(experience^2) +
                           I(experience^3) | country_isocode+year | 0 | country_isocode , data=df_party) %>% summary()

coef_affiliation <- tibble(name = c("From another party","No party affiliation","From the leader's party"),
                           estimate = c(affiliation_felm$coefficients[8],
                                        affiliation_felm$coefficients[9],
                                        affiliation_felm$coefficients[10]),
                           se = c(affiliation_felm$coefficients[8,2],
                                  affiliation_felm$coefficients[9,2],
                                  affiliation_felm$coefficients[10,2]
                           )
) %>% mutate(name = fct_relevel(name,
                                "From the leader's party",
                                "No party affiliation",
                                "From another party"))

plot_affiliation <- plot_canvas(coef_affiliation) + ggtitle("H2b: Affiliation")
plot_affiliation

fe_glm_af <- feglm(purged_y1 ~ party_group/coupattempt_presentyear +
                     gdp_cap_pwt_ln + pop_pwt_ln + experience + I(experience^2) +
                     I(experience^3) | country_isocode+year | country_isocode , data=df_party)
apes.fe_glm_af <- getAPEs(fe_glm_af)

effects <- as.vector(apes.fe_glm_af$delta)
effects_felm <- as.vector(affiliation_felm$coefficients)
apes_effects <- summary(apes.fe_glm_af, cluster="country_isocode")
apes_effects <- as.data.frame(apes_effects$cm)


coef_affiliation_feglm <- tibble(name = c("From another party","No party affiliation","From the leader's party"),
                           estimate = c(apes_effects$Estimate[8],
                                        apes_effects$Estimate[9],
                                        apes_effects$Estimate[10]),
                           se = c(apes_effects$'Std. error'[8],
                                  apes_effects$'Std. error'[9],
                                  apes_effects$'Std. error'[10]
                           )
) %>% mutate(name = fct_relevel(name,
                                "From the leader's party",
                                "No party affiliation",
                                "From another party"))

plot_affiliation_feglm <- plot_canvas(coef_affiliation_feglm) + ggtitle("H2b: Affiliation - FEGLM")
plot_affiliation_feglm

# Importance ---

df_importance <- df_aut %>% filter(importance_factor != "0" & !is.na(importance_factor))
df_importance <- droplevels(df_importance[!df_importance$importance_numeric == '0',])
df_importance <- df_importance %>% mutate(importance_factor = fct_relevel(importance_factor,
                                                                          "Prime minister/President\n(not leader)",
                                                                          "Vice president,\ndeputy prime minister,\ntop minister",
                                                                          "Medium-ranking\nminister",
                                                                          "Low-ranking\nminister",
                                                                          "Junior minister\nor other low-\nranking post"
))

importance_felm <- felm(purged_y1 ~ importance_factor/coupattempt_presentyear +
                          gdp_cap_pwt_ln + pop_pwt_ln + experience + I(experience^2) +
                          I(experience^3) | country_isocode+year | 0 | country_isocode , data=df_importance) %>% summary()

coef_importance <- tibble(name = c("Prime minister/President\n(not leader)","Vice president,\ndeputy prime minister,\ntop minister",
                                   "Medium-ranking\nminister", "Junior minister\nor other low-\nranking post","Low-ranking\nminister"),
                          estimate = c(importance_felm$coefficients[10],
                                       importance_felm$coefficients[11],
                                       importance_felm$coefficients[12],
                                       importance_felm$coefficients[13],
                                       importance_felm$coefficients[14]),
                          se = c(importance_felm$coefficients[10,2],
                                 importance_felm$coefficients[11,2],
                                 importance_felm$coefficients[12,2],
                                 importance_felm$coefficients[13,2],
                                 importance_felm$coefficients[14,2]
                          )
) %>% mutate(name = fct_relevel(name,
                                "Junior minister\nor other low-\nranking post","Low-ranking\nminister","Medium-ranking\nminister",
                                "Vice president,\ndeputy prime minister,\ntop minister","Prime minister/President\n(not leader)"))


plot_importance <- plot_canvas(coef_importance) + ggtitle("H3b: Importance in cabinet") + ylim(-0.1,0.6)
plot_importance

fe_glm_imp <- feglm(purged_y1 ~ importance_factor/coupattempt_presentyear +
                      gdp_cap_pwt_ln + pop_pwt_ln + experience + I(experience^2) +
                      I(experience^3) | country_isocode+year| country_isocode , data=df_importance)


apes.fe_glm_imp <- getAPEs(fe_glm_imp)
effects <- as.vector(apes.fe_glm_imp$delta)
effects_felm <- as.vector(importance_felm$coefficients)
apes_effects <- summary(apes.fe_glm_imp, cluster="country_isocode")
apes_effects <- as.data.frame(apes_effects$cm)
apes_effects <- as.data.frame(apes_effects)

coef_importance_feglm <- tibble(name = c("Prime minister/President\n(not leader)","Vice president,\ndeputy prime minister,\ntop minister",
                                         "Medium-ranking\nminister", "Junior minister\nor other low-\nranking post","Low-ranking\nminister"),
                          estimate = c(apes_effects$Estimate[10],
                                       apes_effects$Estimate[11],
                                       apes_effects$Estimate[12],
                                       apes_effects$Estimate[13],
                                       apes_effects$Estimate[14]),
                          se = c(apes_effects$'Std. error'[10],
                                 apes_effects$'Std. error'[11],
                                 apes_effects$'Std. error'[12],
                                 apes_effects$'Std. error'[13],
                                 apes_effects$'Std. error'[14]
                          )
) %>% mutate(name = fct_relevel(name,
                                "Junior minister\nor other low-\nranking post","Low-ranking\nminister","Medium-ranking\nminister",
                                "Vice president,\ndeputy prime minister,\ntop minister","Prime minister/President\n(not leader)"))


plot_importance_feglm <- plot_canvas(coef_importance_feglm) + ggtitle("H3b: Importance in cabinet - FEGLM") + ylim(-0.1,0.6)
plot_importance_feglm

# Experience ---

df_experience <- df_aut %>% filter(importance_factor != "0.5")

quantiles_experience <- df_aut %>% summarise(x = quantile(experience, c(0.25, 0.5, 0.75)), experience = c(0.25, 0.5, 0.75))

experience_group <- felm(purged_y1 ~ as.factor(experience_group)/coupattempt_presentyear +
                           gdp_cap_pwt_ln + pop_pwt_ln | country_isocode+year | 0 | country_isocode, data=df_experience) %>% summary()

coef_experience_group <- tibble(name = c("Less than 2 years","2-3 years",
                                         "4-6 years","Over 6 years"),
                                estimate = c(experience_group$coefficients[6],
                                             experience_group$coefficients[7],
                                             experience_group$coefficients[8],
                                             experience_group$coefficients[9]),
                                se = c(experience_group$coefficients[6,2],
                                       experience_group$coefficients[7,2],
                                       experience_group$coefficients[8,2],
                                       experience_group$coefficients[9,2]
                                )) %>% mutate(name = fct_relevel(name,
                                                                 "Over 6 years","4-6 years",
                                                                 "2-3 years",
                                                                 "Less than 2 years"
                                                                 
                                ))


plot_expgroup <- plot_canvas(coef_experience_group) + ggtitle("H2a: Experience")
plot_expgroup

fe_glm_exp <- feglm(purged_y1 ~ as.factor(experience_group)/coupattempt_presentyear +
                      gdp_cap_pwt_ln + pop_pwt_ln | country_isocode+ year | country_isocode, data=df_experience)

apes.fe_glm_exp <- getAPEs(fe_glm_exp)
effects <- as.vector(apes.fe_glm_exp$delta)
effects_felm <- as.vector(experience_group$coefficients)
apes_effects <- summary(apes.fe_glm_exp, cluster="country_isocode")
apes_effects <- as.data.frame(apes_effects$cm)

coef_experience_group_feglm <- tibble(name = c("Less than 2 years","2-3 years",
                                         "4-6 years","Over 6 years"),
                                estimate = c(apes_effects$Estimate[6],
                                             apes_effects$Estimate[7],
                                             apes_effects$Estimate[8],
                                             apes_effects$Estimate[9]),
                                se = c(apes_effects$'Std. error'[6],
                                       apes_effects$'Std. error'[7],
                                       apes_effects$'Std. error'[8],
                                       apes_effects$'Std. error'[9]
                                )) %>% mutate(name = fct_relevel(name,
                                                                 "Over 6 years","4-6 years",
                                                                 "2-3 years",
                                                                 "Less than 2 years"
                                                                 
                                ))


plot_expgroup_feglm <- plot_canvas(coef_experience_group_feglm) + ggtitle("H2a: Experience - FEGLM")
plot_expgroup_feglm

# Combination (typology) ---

medianbyyear <- df_experience %>% group_by(country_isocode,year) %>% summarize(experience_median = median(experience,na.rm=TRUE))

df_combination <- df_experience %>% left_join(.,medianbyyear,by=c("country_isocode","year")) %>% filter(party_group != "unknown") %>%
  mutate(experience_bin = ifelse(experience >= experience_median, 1,0),
         important_bin = if_else(importance_numeric %in% c(3,4),1,0),
         loyal_bin = if_else(experience_bin == 1 & party_group == "leader_party",1,0),
         typology = case_when(important_bin == 0 & loyal_bin == 0 ~ 1,
                              important_bin == 1 & loyal_bin == 0 ~ 2,
                              important_bin == 0 & loyal_bin == 1 ~ 3,
                              important_bin == 1 & loyal_bin == 1 ~ 4
         )
  )

combination_felm <- felm(purged_y1 ~ as.factor(typology)/coupattempt_presentyear +
                           gdp_cap_pwt_ln + pop_pwt_ln | country_isocode+year | 0 | country_isocode , data=df_combination) %>% summary()

coef_combination <- tibble(name = c("Low responsibility and weak signs of loyalty","High responsibility and weak signs of loyalty",
                                    "Low responsibility and strong signs of loyalty","High responsibility and strong signs of loyalty"),
                           estimate = c(combination_felm$coefficients[6],
                                        combination_felm$coefficients[7],
                                        combination_felm$coefficients[8],
                                        combination_felm$coefficients[9]),
                           se = c(combination_felm$coefficients[6,2],
                                  combination_felm$coefficients[7,2],
                                  combination_felm$coefficients[8,2],
                                  combination_felm$coefficients[9,2]
                           )) %>% mutate(name = fct_relevel(name,
                                                            "High responsibility and strong signs of loyalty","Low responsibility and strong signs of loyalty",
                                                            "High responsibility and weak signs of loyalty","Low responsibility and weak signs of loyalty"
                                                            
                           ))


plot_combination <- plot_canvas(coef_combination) + ggtitle("H4: Combinations of traits")
plot_combination

fe_glm_com <- feglm(purged_y1 ~ as.factor(typology)/coupattempt_presentyear +
                      gdp_cap_pwt_ln + pop_pwt_ln | country_isocode+year | country_isocode , data=df_combination)
apes.fe_glm_com <- getAPEs(fe_glm_com)
effects <- as.vector(apes.fe_glm_com$delta)
effects_felm <- as.vector(combination_felm$coefficients)
apes_effects <- summary(apes.fe_glm_com, cluster="country_isocode")
apes_effects <- as.data.frame(apes_effects$cm)

coef_combination_feglm <- tibble(name = c("Low responsibility and weak signs of loyalty","High responsibility and weak signs of loyalty",
                                    "Low responsibility and strong signs of loyalty","High responsibility and strong signs of loyalty"),
                           estimate = c(apes_effects$Estimate[6],
                                        apes_effects$Estimate[7],
                                        apes_effects$Estimate[8],
                                        apes_effects$Estimate[9]),
                           se = c(apes_effects$'Std. error'[6],
                                  apes_effects$'Std. error'[7],
                                  apes_effects$'Std. error'[8],
                                  apes_effects$'Std. error'[9]
                           )) %>% mutate(name = fct_relevel(name,
                                                            "High responsibility and strong signs of loyalty","Low responsibility and strong signs of loyalty",
                                                            "High responsibility and weak signs of loyalty","Low responsibility and weak signs of loyalty"
                                                            
                           ))


plot_combination_feglm <- plot_canvas(coef_combination_feglm) + ggtitle("H4: Combinations of traits - FEGLM")
plot_combination_feglm

# Get N's

n_exp <-  df_experience %>% filter(!is.na(coupattempt_presentyear)) %>% group_by(coupattempt_presentyear) %>% summarize(n = as.numeric(n())) %>% ungroup() %>%
  mutate(sum = sum(n))

n_affliation <-  df_party %>% filter(!is.na(coupattempt_presentyear)) %>% group_by(coupattempt_presentyear) %>% summarize(n = as.numeric(n())) %>% ungroup() %>%
  mutate(sum = sum(n))

n_resp <-  df_aut %>% filter(!is.na(coupattempt_presentyear) & !is.na(minister_type)) %>% group_by(coupattempt_presentyear) %>% summarize(n = as.numeric(n())) %>% ungroup() %>%
  mutate(sum = sum(n))

n_importance <-  df_importance %>% filter(!is.na(coupattempt_presentyear)) %>% group_by(coupattempt_presentyear) %>% summarize(n = as.numeric(n())) %>% ungroup() %>%
  mutate(sum = sum(n))

n_comb <-  df_combination %>% filter(!is.na(coupattempt_presentyear)) %>% group_by(coupattempt_presentyear) %>% summarize(n = as.numeric(n())) %>% ungroup() %>%
  mutate(sum = sum(n))

# Print ---


ggsave(
  'output/fig_responsibility.jpg',
  gridExtra::grid.arrange(plot_resp, plot_resp_feglm,ncol=2),
  width = 13,
  height = 6,
  dpi = 120
)

ggsave(
  'output/fig_affiliation.jpg',
  gridExtra::grid.arrange(plot_affiliation, plot_affiliation_feglm,ncol=2),
  width = 13,
  height = 6,
  dpi = 120
)

ggsave(
  'output/fig_experience.jpg',
  gridExtra::grid.arrange(plot_expgroup, plot_expgroup_feglm,ncol=2),
  width = 13,
  height = 6,
  dpi = 120
)

ggsave(
  'output/fig_importance.jpg',
  gridExtra::grid.arrange(plot_importance, plot_importance_feglm,ncol=2),
  width = 13,
  height = 6,
  dpi = 120
)

ggsave(
  'output/fig_combination.jpg',
  gridExtra::grid.arrange(plot_combination, plot_combination_feglm,ncol=2),
  width = 13,
  height = 6,
  dpi = 120
)

ggsave(
  'output/figure3.jpg',
  gridExtra::grid.arrange(plot_expgroup, plot_affiliation,ncol=2),
  width = 13,
  height = 6,
  dpi = 120
)


ggsave(
  'output/figure3_feglm.jpg',
  gridExtra::grid.arrange(plot_expgroup_feglm, plot_affiliation_feglm,ncol=2),
  width = 13,
  height = 6,
  dpi = 120
)

ggsave(
  'output/figure4.jpg',
  gridExtra::grid.arrange(plot_resp, plot_importance,ncol=2),
  width = 13,
  height = 6,
  dpi = 120
)

ggsave(
  'output/figure4_feglm.jpg',
  gridExtra::grid.arrange(plot_resp_feglm, plot_importance_feglm,ncol=2),
  width = 13,
  height = 6,
  dpi = 120
)

ggsave(
  'output/figure5.jpg',
  plot_combination,
  width = 10,
  height = 6,
  dpi = 120
)

ggsave(
  'output/figure5_feglm.jpg',
  plot_combination_feglm,
  width = 10,
  height = 6,
  dpi = 120
)

################
## Appendix K ##
################

# Responsibility ---

df_aut <- df_aut %>% mutate(minister_type = fct_relevel(minister_type,
                                                        "Minister of Defense",
                                                        "Minister of Foreign Affairs",
                                                        "Minister of Finance",
                                                        "Other type of minister",                                                      
                                                        "Minister of Natural Resources"
))


responsibility_felm <- felm(purged_y1 ~ minister_type*coupattempt_presentyear +
                              gdp_cap_pwt_ln + pop_pwt_ln + experience + I(experience^2) +
                              I(experience^3) | country_isocode+year | 0 | country_isocode, data=df_aut)

# Fixed effects GLM
fe_glm_res <- feglm(purged_y1 ~ minister_type*coupattempt_presentyear +
                      gdp_cap_pwt_ln + pop_pwt_ln + experience + I(experience^2) +
                      I(experience^3) | country_isocode+year | country_isocode , data=df_aut)
apes.fe_glm_res <- getAPEs(fe_glm_res)


effects_res <- as.vector(apes.fe_glm_res$delta)
effects_felm_res <- as.vector(responsibility_felm$coefficients)
apes_effects_res <- summary(apes.fe_glm_res, cluster="country_isocode")
apes_effects_res <- as.data.frame(apes_effects_res$cm)

# Table for responsibility

texreg(list(responsibility_felm, fe_glm_res), override.coef = list(effects_felm_res, apes_effects_res$Estimate),
       override.se = list(responsibility_felm$cse, apes_effects_res$'Std. error'),
       override.pvalues = list(responsibility_felm$pval, apes_effects_res$`Pr(> |z|)`), digits = 3,stars = c(0.05),
       file="output/table_responsibility.tex",
       custom.model.names=c("H3a: Responsibility - FELM","H3a: Responsibility - FEGLM"),
       custom.coef.names=c("Minister of Foreign Affairs (Ref: D)","Minister of Finance (Ref: D)", "Other type of minister (Ref: D)",
                           "Minister of Natural Resources (Ref: D)","Failed coup attempt","Log of GDP per capita","Log of Population",
                           "Experience","Experience^2","Experience^3","FCA: Minister of Foreign Affairs (Ref: D)",
                           "FCA*Minister of Finance (Ref: D)","FCA*Other type of minister (Ref: D)","FCA*Minister of Natural Resources (Ref: D)"
       ),
       reorder.coef=c(1,2,3,4,5,11,12,13,14,6,7,8,9,10),
       label = "indtable2",
       single.row = TRUE,
       caption ="Table for Figure 4",
       caption.above=TRUE,
       include.ci = FALSE,
       include.adjrs = FALSE,
       include.rmse = FALSE,
       custom.note = ("\\parbox{\\linewidth}{\\vspace{2pt}%stars. Dependent variable: Purged next year. 
                      All models include country and year fixed effects. Country-clustered standard errors in parentheses.
                      FCA = Failed coup attempt, D = Defense.}"))
# Party affiliation ---

df_party <- df_party %>% mutate(party_group = fct_relevel(party_group,
                                                          "From another party",
                                                          "No party affiliation",
                                                          "From the leader's party"
))

affiliation_felm <- felm(purged_y1 ~ party_group*coupattempt_presentyear +
                           gdp_cap_pwt_ln + pop_pwt_ln + experience + I(experience^2) +
                           I(experience^3) | country_isocode+year | 0 | country_isocode, data=df_party)

fe_glm_af <- feglm(purged_y1 ~ party_group*coupattempt_presentyear +
                     gdp_cap_pwt_ln + pop_pwt_ln + experience + I(experience^2) +
                     I(experience^3) | country_isocode+year | country_isocode , data=df_party)
apes.fe_glm_af <- getAPEs(fe_glm_af)

effects_af <- as.vector(apes.fe_glm_af$delta)
effects_felm_af <- as.vector(affiliation_felm$coefficients)
apes_effects_af <- summary(apes.fe_glm_af, cluster="country_isocode")
apes_effects_af <- as.data.frame(apes_effects_af$cm)

# Table for Affiliation

texreg(list(affiliation_felm, fe_glm_af), override.coef = list(effects_felm_af, apes_effects_af$Estimate),
       override.se = list(affiliation_felm$cse, apes_effects_af$'Std. error'),
       override.pvalues = list(affiliation_felm$pval, apes_effects_af$`Pr(> |z|)`), digits = 3,
       file="output/table_affiliation.tex",
       custom.model.names=c("H2b: Affiliation - FELM","H2b: Affiliation - FEGLM"),
       custom.coef.names=c("No party affiliation (Ref: Other party)","From the leader's party (Ref: Other party)",
                           "Failed coup attempt", "Log of GDP per capita","Log of Population",
                           "Experience","Experience^2","Experience^3",
                           "FCA*No party affiliation (Ref: Other party)",
                           "FCA*From the leader's party (Ref: Other party)"),
       reorder.coef=c(1,2,3,9,10,4,5,6,7,8),
       label = "indtable",
       single.row = TRUE,
       caption ="Table for Figure 3",
       caption.above=TRUE,
       include.ci = FALSE,
       include.adjrs = FALSE,
       include.rmse = FALSE,
       custom.note = ("\\parbox{\\linewidth}{\\vspace{2pt}%stars. Dependent variable: Purged next year. 
                      All models include country and year fixed effects. Country-clustered standard errors in parentheses. 
                      FCA = Failed coup attempt}"))


# Importance ---

df_importance <- df_importance %>% mutate(importance_factor = fct_relevel(importance_factor,
                                                                          "Prime minister/President\n(not leader)",
                                                                          "Vice president,\ndeputy prime minister,\ntop minister",
                                                                          "Medium-ranking\nminister",
                                                                          "Low-ranking\nminister",
                                                                          "Junior minister\nor other low-\nranking post"
))
df_importance <- droplevels(df_importance[!df_importance$importance_numeric == '0',])
importance_felm <- felm(purged_y1 ~ importance_factor*coupattempt_presentyear +
                          gdp_cap_pwt_ln + pop_pwt_ln + experience + I(experience^2) +
                          I(experience^3) | country_isocode+year | 0 | country_isocode, data=df_importance)

fe_glm_imp <- feglm(purged_y1 ~ importance_factor*coupattempt_presentyear +
                      gdp_cap_pwt_ln + pop_pwt_ln + experience + I(experience^2) +
                      I(experience^3) | country_isocode+year| country_isocode , data=df_importance)


apes.fe_glm_imp <- getAPEs(fe_glm_imp)
effects_imp <- as.vector(apes.fe_glm_imp$delta)
effects_felm_imp <- as.vector(importance_felm$coefficients)
apes_effects_imp <- summary(apes.fe_glm_imp, cluster="country_isocode")
apes_effects_imp <- as.data.frame(apes_effects_imp$cm)
apes_effects_imp <- as.data.frame(apes_effects_imp)

# Table for Importance

texreg(list(importance_felm, fe_glm_imp), override.coef = list(effects_felm_imp, apes_effects_imp$Estimate),
       override.se = list(importance_felm$cse, apes_effects_imp$'Std. error'),
       override.pvalues = list(importance_felm$pval, apes_effects_imp$`Pr(> |z|)`), digits = 3,stars = c(0.05),
       file="output/table_importance.tex",
       custom.model.names=c("H3b: Importance - FELM","H3b: Importance - FEGLM"),
       custom.coef.names=c("VP, DP, top minister (Ref: PMP)","Medium-ranking minister (Ref: PMP)",
                           "Low-ranking minister (Ref: PMP)", "Junior minister (Ref: PMP)",
                           "Failed coup attempt","Log of GDP per capita", "Log of Population", 
                           "Experience","Experience^2","Experience^3",
                           "FCA*VP, DP, top minister (Ref: PMP)","FCA*Medium-ranking minister (Ref: PMP)",
                           "FCA*Low-ranking minister (Ref: PMP)","FCA*Junior minister (Ref: PMP)"),
       reorder.coef=c(1,2,3,4,5,11,12,13,14,6,7,8,9,10),
       label = "indtable2",
       single.row = TRUE,
       caption ="Table for Figure 4",
       caption.above=TRUE,
       include.ci = FALSE,
       include.adjrs = FALSE,
       include.rmse = FALSE,
       custom.note = ("\\parbox{\\linewidth}{\\vspace{2pt}%stars. Dependent variable: Purged next year. 
                      All models include country and year fixed effects. Country-clustered standard errors in parentheses.
                      FCA = Failed coup attempt, PMP = Prime minister or President.}"))



# Experience ---

experience_felm <- felm(purged_y1 ~ experience_group*coupattempt_presentyear +
                          gdp_cap_pwt_ln + pop_pwt_ln | country_isocode+year | 0 | country_isocode, data=df_experience)

fe_glm_exp <- feglm(purged_y1 ~ experience_group*coupattempt_presentyear +
                      gdp_cap_pwt_ln + pop_pwt_ln | country_isocode+ year | country_isocode, data=df_experience)

apes.fe_glm_exp <- getAPEs(fe_glm_exp)
effects_exp <- as.vector(apes.fe_glm_exp$delta)
effects_felm_exp <- as.vector(experience_felm$coefficients)
apes_effects_exp <- summary(apes.fe_glm_exp, cluster="country_isocode")
apes_effects_exp <- as.data.frame(apes_effects_exp$cm)

# Table for Experience

texreg(list(experience_felm, fe_glm_exp), override.coef = list(effects_felm_exp, apes_effects_exp$Estimate),
       override.se = list(experience_felm$cse, apes_effects_exp$'Std. error'),
       override.pvalues = list(experience_felm$pval, apes_effects_exp$`Pr(> |z|)`), digits = 3,
       file="output/table_experience.tex",
       custom.model.names=c("H2a: Experience - FELM","H2a: Experience - FEGLM"),
       custom.coef.names=c("2-3 years of experience (Ref: <2 y)","4-6 years of experience (Ref: <2 y)","Over 6 years of experience (Ref: <2 y)",
                           "Failed coup attempt", "Log of GDP per capita","Log of Population","FCA*2-3 years of experience (Ref: <2 y)",
                           "FCA*4-6 years of experience (Ref: <2 y)","FCA*Over 6 years of experience (Ref: <2 y)"),
       reorder.coef=c(1,2,3,4,7,8,9,5,6),
       label = "indtable",
       single.row = TRUE,
       caption ="Table for Figure 3",
       caption.above=TRUE,
       include.ci = FALSE,
       include.adjrs = FALSE,
       include.rmse = FALSE,
       custom.note = ("\\parbox{\\linewidth}{\\vspace{2pt}%stars. Dependent variable: Purged next year. 
                      All models include country and year fixed effects. Country-clustered standard errors in parentheses. 
                      FCA = Failed coup attempt}"))


# Combination ---

combination_felm <- felm(purged_y1 ~ relevel(as.factor(typology),2)*coupattempt_presentyear +
                           gdp_cap_pwt_ln + pop_pwt_ln | country_isocode+year | 0 | country_isocode , data=df_combination)

fe_glm_com <- feglm(purged_y1 ~ relevel(as.factor(typology),2)*coupattempt_presentyear +
                      gdp_cap_pwt_ln + pop_pwt_ln | country_isocode+year | country_isocode , data=df_combination)
apes.fe_glm_com <- getAPEs(fe_glm_com)
effects_com <- as.vector(apes.fe_glm_com$delta)
effects_felm_com <- as.vector(combination_felm$coefficients)
apes_effects_com <- summary(apes.fe_glm_com, cluster="country_isocode")
apes_effects_com <- as.data.frame(apes_effects_com$cm)


# Table Combination FEGLM

texreg(list(combination_felm, fe_glm_com), override.coef = list(effects_felm_com, apes_effects_com$Estimate),
       override.se = list(combination_felm$cse, apes_effects_com$'Std. error'),
       override.pvalues = list(combination_felm$pval, apes_effects_com$`Pr(> |z|)`), digits = 3,stars = c(0.05),
       file="output/table_combination.tex",
       custom.model.names=c("Combination - FELM", "Combination - FEGLM"),
       custom.coef.names=c("Low responsibility and weak signs of loyalty (Ref: HW)",
                           "Low responsibility and strong signs of loyalty (Ref: HW)",
                           "High responsibility and strong signs of loyalty (Ref: HW)",
                           "Failed coup attempt","Log of GDP per capita","Log of Population",
                           "FCA*High responsibility and weak signs of loyalt (Ref: HW)",
                           "FCA*Low responsibility and strong signs of loyalty (Ref: HW)",
                           "FCA*High responsibility and strong signs of loyalty (Ref: HW)"),
       reorder.coef=c(1,2,3,4,
                      7,8,9,
                      6,5),
       label = "indtable3",
       single.row = TRUE,
       caption ="Table for Figure 5",
       caption.above=TRUE,
       include.ci = FALSE,
       include.adjrs = FALSE,
       include.rmse = FALSE,
       custom.note = ("\\parbox{\\linewidth}{\\vspace{2pt}%stars. Dependent variable: Purged next year. 
                      All models include country and year fixed effects. Country-clustered standard errors in parentheses.
                      FCA = Failed coup attempt, LW = High responsibility and weak signs of loyalty.}"))


# Tables ---

# Table for Figure 3

texreg(list(experience_felm,affiliation_felm),stars = c(0.05),
       file="output/appendix_k1.tex",
       custom.model.names=c("H2a: Experience","H2b: Affiliation"),
       custom.coef.names=c("2-3 years of experience (Ref: <2 y)","4-6 years of experience (Ref: <2 y)","Over 6 years of experience (Ref: <2 y)",
                           "Failed coup attempt", "Log of GDP per capita","Log of Population",
                           "FCA*2-3 years of experience (Ref: <2 y)","FCA*4-6 years of experience (Ref: <2 y)","FCA*Over 6 years of experience (Ref: <2 y)",
                           "No party affiliation (Ref: Other party)","From the leader's party (Ref: Other party)","Experience","Experience^2","Experience^3",
                           "FCA*No party affiliation (Ref: Other party)",
                           "FCA*From the leader's party (Ref: Other party)"),
       reorder.coef=c(1,2,3,10,11,4,7,8,9,15,16,5,6,12,13,14),
       label = "indtable",
       single.row = TRUE,
       caption ="Table for Figure 3",
       caption.above=TRUE,
       include.ci = FALSE,
       include.adjrs = FALSE,
       include.rmse = FALSE,
       custom.note = ("\\parbox{\\linewidth}{\\vspace{2pt}%stars. Dependent variable: Purged next year. 
                      All models include country and year fixed effects. Country-clustered standard errors in parentheses. 
                      FCA = Failed coup attempt}"))



# Table for Figure 4

texreg(list(responsibility_felm,importance_felm),stars = c(0.05),
       file="output/appendix_k2.tex",
       custom.model.names=c("H3a: Responsibility","H3b: Importance"),
       custom.coef.names=c("Minister of Foreign Affairs (Ref: D)","Minister of Finance (Ref: D)", "Other type of minister (Ref: D)",
                           "Minister of Natural Resources (Ref: D)","Failed coup attempt","Log of GDP per capita","Log of Population",
                           "Experience","Experience^2","Experience^3","FCA: Minister of Foreign Affairs (Ref: D)",
                           "FCA*Minister of Finance (Ref: D)","FCA*Other type of minister (Ref: D)","FCA*Minister of Natural Resources (Ref: D)",
                           "VP, DP, top minister (Ref: PMP)","Medium-ranking minister (Ref: PMP)","Low-ranking minister (Ref: PMP)",
                           "Junior minister (Ref: PMP)","FCA*VP, DP, top minister (Ref: PMP)",
                           "FCA*Medium-ranking minister (Ref: PMP)","FCA*Low-ranking minister (Ref: PMP)",
                           "FCA*Junior minister (Ref: PMP)"),
       reorder.coef=c(1,2,3,4,
                      15,16,17,18,
                      5,
                      11,12,13,14,
                      19,20,21,22,
                      6,7,8,9,10),
       label = "indtable2",
       single.row = TRUE,
       caption ="Table for Figure 4",
       caption.above=TRUE,
       include.ci = FALSE,
       include.adjrs = FALSE,
       include.rmse = FALSE,
       custom.note = ("\\parbox{\\linewidth}{\\vspace{2pt}%stars. Dependent variable: Purged next year. 
                      All models include country and year fixed effects. Country-clustered standard errors in parentheses.
                      FCA = Failed coup attempt, D = Defense, PMP = Prime minister or President.}"))


# Table for Figure 5

texreg(list(combination_felm),stars = c(0.05),
       file="output/appendix_k3.tex",
       custom.model.names=c("Combination of traits"),
       custom.coef.names=c("Low responsibility and weak signs of loyalty (Ref: HW)",
                           "Low responsibility and strong signs of loyalty (Ref: HW)",
                           "High responsibility and strong signs of loyalty (Ref: HW)",
                           "Failed coup attempt","Log of GDP per capita","Log of Population",
                           "FCA*High responsibility and weak signs of loyalt (Ref: HW)",
                           "FCA*Low responsibility and strong signs of loyalty (Ref: HW)",
                           "FCA*High responsibility and strong signs of loyalty (Ref: HW)"),
       reorder.coef=c(1,2,3,4,
                      7,8,9,
                      6,5),
       label = "indtable3",
       single.row = TRUE,
       caption ="Table for Figure 5",
       caption.above=TRUE,
       include.ci = FALSE,
       include.adjrs = FALSE,
       include.rmse = FALSE,
       custom.note = ("\\parbox{\\linewidth}{\\vspace{2pt}%stars. Dependent variable: Purged next year. 
                      All models include country and year fixed effects. Country-clustered standard errors in parentheses.
                      FCA = Failed coup attempt, LW = High responsibility and weak signs of loyalty.}"))


