library(tidyverse)
library(estimatr)
library(coefplot)
library(xtable)
library(MBESS)
library(ggthemes)
library(ggplot2)
library(VGAM)

#devtools::install_github("acoppock/commarobust")
library(commarobust)

rm(list = ls())
gc()

add_parens <- function(x, digits = 3) {
  x <- as.numeric(x)
  return(paste0("(", sprintf(paste0("%.", digits, "f"), x), ")"))
}

format_num <- function(x, digits = 3) {
  x <- as.numeric(x)
  return(paste0(sprintf(paste0("%.", digits, "f"), x)))
}

make_entry <- function(est, se, p, digits = 3) {
  entry <- paste0(format_num(est, digits = digits), " ", 
                  add_parens(se, digits = digits))
  entry[p < 0.05] <- paste0(entry[p < 0.05], "*")
  return(entry)
}

table_entry <- function(est, se, digits = 3) {
  entry <- paste0(format_num(est, digits = digits), " ", 
                  add_parens(se, digits = digits))
  return(entry)
}

glass_delta <- function(Y, Z, name = "Placebo"){
  Y/sd(Y[Z == name], na.rm = TRUE)
}

## Helper function to estimate treatment fx and plot output. 
ate_plot <- function(covariates = NULL, loopvar = NULL, dv = NULL, Z = "Z",
                     df = NULL) {
  
  if(is.null(covariates)) print("Error. Input covariates!")
  covariates <- covariates
  
  gg_dim <- list()
  gg_ols <- list()
  
  for(i in 1:length(loopvar)) {
    
    loop_df <- df[df$experiment == loopvar[i],]
    
    
    fit <- lm_robust(as.formula(paste0(dv, " ~ ", Z)), weights = wts, 
                     data = loop_df)
    fit_r <- data.frame(tidy(fit))[, c(2,3,5)]
    colnames(fit_r) <- c("est", "se", "p")
    fit_r$exp <- loopvar[i]
    fit_r$treatment <- c("Intercept", "Corrupt", "Honest")
    fit_r$estimator <- "Difference-in-Means"
    gg_dim[[i]] <- fit_r
    
    
    fit <- lm_lin(as.formula(paste0(dv, " ~ ", Z)), weights = wts,
                  covariates = as.formula(paste("~", covariates)), 
                  data = loop_df)
    fit_r <- data.frame(tidy(fit))[1:3, c(2,3,5)]
    colnames(fit_r) <- c("est", "se", "p")
    fit_r$exp <- loopvar[i]
    fit_r$treatment <- c("Intercept", "Corrupt", "Honest")
    fit_r$estimator <- "Covariate-Adjusted"
    gg_ols[[i]] <- fit_r
    
  }
  
  gg_df <- 
    bind_rows(
      (gg_dim %>% bind_rows()),
      (gg_ols %>% bind_rows())) %>% 
    mutate(ui = est + 1.96*se,
           li = est - 1.96*se) %>%
    filter(treatment != "Intercept") %>%
    mutate(experiment = factor(exp, levels = loopvar))
  
  
  g <- 
    ggplot(gg_df, aes(x = est, y = experiment, group = treatment, 
                      color = treatment, shape = treatment)) +
    geom_vline(xintercept = 0, linetype = "dashed") + 
    geom_point(size = 3, position = position_dodgev(height = .5)) +
    geom_errorbarh(aes(xmin = li, xmax = ui), size = 1.25,
                   position = position_dodgev(height = .5), height = 0) 
  return(list(plot = g, gg_df = gg_df))
}

# Theme for pretty plots
devtools::source_url("https://kylepeyton.github.io/assets/theme_kyle.R")

# Read dataset
trust_df <- 
  read_csv("mopf_plos.csv") %>% 
  mutate(Z = factor(Z, levels = c("Placebo", "Corrupt", "Honest")),
         experiment = factor(experiment, levels = c("MTurk_Pols14", 
                                                    "GenPop_Pols14",
                                                    "MTurk_NFL14"))) %>% 
  group_by(experiment) %>% 
  mutate(fmptrust_scale = glass_delta(Y = fmptrust_index, Z = Z),
         probity_scale = glass_delta(Y = probity_index, Z = Z),
         trustgov_scale = glass_delta(Y = trustgov_trustgstd, Z = Z),
         socialtrust_scale = glass_delta(Y = socialtrust_index, Z = Z)) %>% 
  ungroup()

## Set directories for exporting tables/figures 
plotpath <- "XXX/Figures/"
tabpath <- "XXX/Tables/"


####-------------Figure 1, Table 1 in Manuscript------------####
study <- c("MTurk_Pols14", "GenPop_Pols14", "MTurk_NFL14")
covariates <- c("dem_female + dem_age + dem_white + dem_college + dem_work +
                dem_pid3 + dem_ideo5 + dem_income")

trustgov_ate <- ate_plot(df = trust_df, covariates = covariates, 
                         loopvar = study, dv = "trustgov_scale")

g1 <- 
  trustgov_ate$plot + 
  scale_colour_grey(start = 0.2, end = 0.6) +
  scale_y_discrete(labels = c("Study 1 (N = 643)", 
                              "Study 2 (N = 1,324)",
                              "Study 3 (N = 584)")) + 
  xlab("Average Treatment Effect Estimates in Standard Units") +
  facet_wrap(~ estimator) +
  theme_kyle() +
  coord_cartesian(xlim = c(-1, 1)) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        strip.background = element_blank(),
        panel.spacing = unit(2, "lines"))

ggsave(paste0(plotpath, "trustgov_ate_scale_ates.tiff"), g1, device = "tiff",
       width = 7, height = 5)
setwd(plotpath)
system("open trustgov_ate_scale_ates.tiff")


## Export table
trustgov_regtable <-
  trustgov_ate$gg_df %>%
  mutate(entry = make_entry(est = est, se = se, p = p, digits = 2)) %>% 
  select(estimator, entry, experiment, treatment) %>%
  spread(estimator, entry) 

xtable(trustgov_regtable) %>%
  print(include.rownames = FALSE, 
        type = "html",
        file = paste0(tabpath,"trustgov_ates.html"))

####-------------Figure 2, Table 2 in Manuscript------------####
study <- c("MTurk_Pols14", "GenPop_Pols14", "MTurk_NFL14")
covariates <- c("dem_female + dem_age + dem_white + dem_college + dem_work +
                dem_pid3 + dem_ideo5 + dem_income")

fmptrust_ate <- ate_plot(df = trust_df, covariates = covariates, 
                         loopvar = study, dv = "fmptrust_scale")

g1 <- 
  fmptrust_ate$plot + 
  scale_colour_grey(start = 0.2, end = 0.6) +
  scale_y_discrete(labels = c("Study 1 (N = 643)", 
                              "Study 2 (N = 1,324)",
                              "Study 3 (N = 584)")) + 
  xlab("Average Treatment Effect Estimates in Standard Units") +
  facet_wrap(~ estimator) +
  theme_kyle() +
  coord_cartesian(xlim = c(-1, 1)) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        strip.background = element_blank(),
        panel.spacing = unit(2, "lines"))

ggsave(paste0(plotpath, "fmptrust_scale_ates.tiff"), g1, device = "tiff",
       width = 7, height = 5)
setwd(plotpath)
system("open fmptrust_scale_ates.tiff")

## Export Table
fmp_regtable <-
  fmptrust_ate$gg_df %>%
  mutate(entry = make_entry(est = est, se = se, p = p, digits = 2)) %>% 
  select(estimator, entry, experiment, treatment) %>%
  spread(estimator, entry)

xtable(fmp_regtable) %>%
  print(include.rownames = FALSE, 
        type = "html",
        file = paste0(tabpath, "fmptrust_ates.html"))

####-------------Figure 3, Table 3 in Manuscript-------------#####
covariates <- c("dem_female", "dem_age", "dem_white", "dem_college", "dem_work", 
                "dem_pid3", "dem_ideo5", "dem_income")

cate_dems_fmp <- 
  lm_robust(as.formula(paste("fmptrust_scale ~", paste(c("Z", covariates[-6], 
                                                         "experiment"), 
                                                       collapse = "+"))),
            data = trust_df %>% 
              filter(experiment %in% study[1:2], dem_pid3 == "Democrat")) %>% 
  tidy(hetfit)%>%
  mutate_if(is.numeric, funs(round(., 2))) %>% 
  mutate(group = "Democrats") 


cate_indep_fmp <- 
  lm_robust(as.formula(paste("fmptrust_scale ~", paste(c("Z", covariates[-6], 
                                                         "experiment"), 
                                                       collapse = "+"))),
            data = trust_df %>% 
              filter(experiment %in% study[1:2], dem_pid3 == "Independent")) %>% 
  tidy(hetfit) %>%
  mutate_if(is.numeric, funs(round(., 2)))%>% 
  mutate(group = "Independents") 

cate_rep_fmp <- 
  lm_robust(as.formula(paste("fmptrust_scale ~", paste(c("Z", covariates[-6], 
                                                         "experiment"), 
                                                       collapse = "+"))),
            data = trust_df %>% 
              filter(experiment %in% study[1:2], dem_pid3 == "Republican")) %>% 
  tidy(hetfit) %>%
  mutate_if(is.numeric, funs(round(., 2))) %>% 
  mutate(group = "Republicans") 


# Repeat for ANES measure
cate_dems_anes <- 
  lm_robust(as.formula(paste("trustgov_scale ~", paste(c("Z", covariates[-6], 
                                                         "experiment"), 
                                                       collapse = "+"))),
            data = trust_df %>% 
              filter(experiment %in% study[1:2], dem_pid3 == "Democrat")) %>% 
  tidy(hetfit)%>%
  mutate_if(is.numeric, funs(round(., 2))) %>% 
  mutate(group = "Democrats") 


cate_indep_anes <- 
  lm_robust(as.formula(paste("trustgov_scale ~", paste(c("Z", covariates[-6], 
                                                         "experiment"), 
                                                       collapse = "+"))),
            data = trust_df %>% 
              filter(experiment %in% study[1:2], dem_pid3 == "Independent")) %>% 
  tidy(hetfit) %>%
  mutate_if(is.numeric, funs(round(., 2)))  %>% 
  mutate(group = "Independents") 

cate_rep_anes <- 
  lm_robust(as.formula(paste("trustgov_scale ~", paste(c("Z", covariates[-6], 
                                                         "experiment"), 
                                                       collapse = "+"))),
            data = trust_df %>% 
              filter(experiment %in% study[1:2], dem_pid3 == "Republican")) %>% 
  tidy(hetfit) %>%
  mutate_if(is.numeric, funs(round(., 2))) %>% 
  mutate(group = "Republicans") 

fmp_cates <- 
  bind_rows(cate_dems_fmp, cate_indep_fmp, cate_rep_fmp) %>% 
  filter(term %in% c("(Intercept)", "ZCorrupt", "ZHonest"))

anes_cates <- 
  bind_rows(cate_dems_anes, cate_indep_anes, cate_rep_anes) %>% 
  filter(term %in% c("(Intercept)", "ZCorrupt", "ZHonest"))

cates_df <- 
  bind_rows(fmp_cates, anes_cates)

cate_plot <- 
  cates_df %>% 
  filter(term != "(Intercept)") %>% 
  mutate(treatment = ifelse(term == "ZCorrupt", "Corrupt", "Honest"),
         outcome = ifelse(outcome == "fmptrust_scale", "Likert Scale", "ANES Item")) %>% 
  ggplot(., aes(x = estimate, y = group, group = treatment, 
                color = treatment, shape = treatment)) +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  geom_point(size = 3, position = position_dodgev(height = .5)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), size = 1.25,
                 position = position_dodgev(height = .5), height = 0) +
  facet_wrap( ~ outcome) +
  scale_colour_grey(start = 0.2, end = 0.6) +
  xlab("Conditional Average Treatment Effect Estimates in Standard Units") +
  theme_kyle() +
  coord_cartesian(xlim = c(-1, 1)) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        strip.background = element_blank(),
        panel.spacing = unit(2, "lines"))

ggsave(paste0(plotpath, "cate_plot.tiff"), cate_plot, device = "tiff",
       width = 7, height = 5)
setwd(plotpath)
system("open cate_plot.tiff")


cate_regtable <-
  cates_df %>% 
  filter(term != "(Intercept)") %>% 
  mutate(treatment = ifelse(term == "ZCorrupt", "Corrupt", "Honest"),
         outcome = ifelse(outcome == "fmptrust_scale", "Likert Scale", 
                          "ANES Item")) %>% 
  mutate(entry = make_entry(est = estimate, se = std.error, p = p.value, 
                            digits = 2)) %>% 
  select(outcome, entry, group, treatment) %>%
  spread(outcome, entry) %>% 
  arrange(desc(group)) 


xtable(cate_regtable ) %>%
  print(include.rownames = FALSE, 
        type = "html",
        file = paste0(tabpath,"cate_regtable.html"))

## Partisan diffs in means across control in each experiment
trust_df %>%
  filter(Z == "Placebo") %>%
  group_by(dem_pid3) %>%
  summarise(mean_fmptrust = mean(fmptrust_scale, na.rm = T),
            mean_anes = mean(trustgov_scale, na.rm = T))


####-------------S3 Fig. ---------####

library(ri2)
balance_fun <- function(data){
  fit <- lm(as.integer(Z) ~ factor(dem_partyid) + dem_age + dem_female + 
              factor(dem_edu) + scale(dem_income) + dem_ideo5 + 
              factor(dem_race5), data = data)
  summary(fit)$f[1]
}


set.seed(1)

## Study 1
study1_df <- trust_df %>% filter(experiment == "MTurk_Pols14")
design_study1 <- declare_ra(N = nrow(study1_df), num_arms = 3, simple = TRUE,
                            conditions = c("Placebo", "Corrupt", "Honest"))

ri_study1 <- 
  conduct_ri(
    test_function = balance_fun,
    declaration = design_study1, 
    assignment = "Z", 
    sharp_hypothesis = 0,
    data = study1_df, 
    sims = 5000
  )

summary(ri_study1)

## Study 2
study2_df <- trust_df %>% filter(experiment == "GenPop_Pols14")
block_mat <- as.matrix(table(study2_df$dem_pid3, study2_df$Z))
design_study2 <- declare_ra(N = nrow(study2_df), num_arms = 3, 
                            blocks = study2_df$dem_pid3,
                            block_m_each = block_mat,
                            conditions = c("Placebo", "Corrupt", "Honest"))

ri_study2 <- 
  conduct_ri(
    test_function = balance_fun,
    declaration = design_study2, 
    assignment = "Z", 
    sharp_hypothesis = 0,
    data = study2_df, 
    IPW_weights = "weights",
    sims = 5000
  )

summary(ri_study2)

## Study 3
study3_df <- trust_df %>% filter(experiment == "MTurk_NFL14")
design_study3 <- declare_ra(N = nrow(study3_df), num_arms = 3, simple = TRUE,
                            conditions = c("Placebo", "Corrupt", "Honest"))


ri_study3 <- 
  conduct_ri(
    test_function = balance_fun,
    declaration = design_study3, 
    assignment = "Z", 
    sharp_hypothesis = 0,
    data = study3_df,
    sims = 5000
  )

summary(ri_study3)

ri_study1$sims_df$study <- "Experiment 1"
ri_study2$sims_df$study <- "Experiment 2"
ri_study3$sims_df$study <- "Experiment 3"

ri_df <- bind_rows(ri_study1$sims_df, ri_study2$sims_df, ri_study3$sims_df)

ggplot(data = ri_df, aes(x = est_sim)) + 
  geom_histogram(binwidth = 0.05) + 
  geom_vline(aes(xintercept = est_obs), col = 'red', alpha = 0.5) + 
  facet_wrap(~ study) + 
  xlab("Distribution of Simulated F-Statistics") + 
  ylab("") + 
  theme_kyle()

####-------------S7-S9 Table------------------####
study <- c("MTurk_Pols14", "GenPop_Pols14", "MTurk_NFL14")

## Print Reliability Estimates
## NB: uncomment for Omega statitics, make sure MBESS package is loaded.
trust_df %>% 
  filter(experiment == study[1], Z == "Placebo") %>% 
  select(starts_with("fmptrust_"), -fmptrust_scale, - fmptrust_index) %>% 
  na.omit() %>%
  #ci.reliability(type = "omega")
  psych::alpha()

trust_df %>% 
  filter(experiment == study[2], Z == "Placebo") %>% 
  select(starts_with("fmptrust_"), -fmptrust_scale, - fmptrust_index) %>% 
  na.omit() %>%
  #ci.reliability(type = "omega")
  psych::alpha()

trust_df %>% 
  filter(experiment == study[3], Z == "Placebo") %>%  
  select(starts_with("fmptrust_"), -fmptrust_scale, - fmptrust_index) %>% 
  na.omit() %>%
  #ci.reliability(type = "omega")
  psych::alpha()

# Probity measure
trust_df %>% 
  filter(experiment == study[1], Z == "Placebo") %>% 
  select(starts_with("probity_"), -probity_index) %>% 
  na.omit() %>%
  #ci.reliability(type = "omega")
  psych::alpha()

trust_df %>% 
  filter(experiment == study[2], Z == "Placebo") %>% 
  select(starts_with("probity_"), -probity_index) %>% 
  na.omit() %>%
  #ci.reliability(type = "omega")
  psych::alpha()

trust_df %>% 
  filter(experiment == study[3], Z == "Placebo") %>%  
  select(starts_with("probity_"), -probity_index) %>% 
  na.omit() %>%
  #ci.reliability(type = "omega")
  psych::alpha()

## S8 Table
means <- 
  trust_df %>%
  filter(experiment %in% study) %>%
  group_by(Z, experiment) %>%
  summarise(mean_est = mean(post_persuade, na.rm = TRUE)) %>%
  mutate(measure = "Persuasive") %>% 
  rbind(trust_df %>%
          filter(experiment %in% study) %>%
          group_by(Z, experiment) %>%
          summarise(mean_est = mean(post_changeview, na.rm = TRUE)) %>% 
          mutate(measure = "Changed"))

ses <- 
  trust_df %>%
  filter(experiment %in% study) %>%
  group_by(Z, experiment) %>%
  summarise(se = se_mean(post_persuade, na.rm = TRUE)) %>%
  mutate(measure = "Persuasive") %>% 
  rbind(trust_df %>%
          filter(experiment %in% study) %>%
          group_by(Z, experiment) %>%
          summarise(se = se_mean(post_changeview, na.rm = TRUE)) %>% 
          mutate(measure = "Changed"))

n <- 
  trust_df %>%
  filter(experiment %in% study) %>%
  group_by(Z, experiment) %>%
  tally()

gg_df <- 
  left_join(n, left_join(means, ses)) %>%
  ungroup() %>%
  mutate(li = mean_est - 1.96*se,
         ui = mean_est + 1.96*se,
         Z = factor(Z, levels = c("Corrupt", "Placebo", "Honest"))) %>%
  data.frame

group_means <-
  gg_df %>%
  mutate(entry = table_entry(est = mean_est, se = se, digits = 2),
         experiment = factor(experiment, labels = paste("Study", 1:3))) %>%
  select(measure, Z, experiment, entry) %>%
  spread(Z, entry) 
  

xtable(group_means) %>%
  print(include.rownames = FALSE, 
        include.colnames = FALSE,
        hline.after = c(),
        only.contents = TRUE,
        type = "latex",
        file = paste0(tabpath, "post_article_means.tex"))



####-------------S4 Fig. S10 Table---------
study <- c("MTurk_Pols14", "GenPop_Pols14", "MTurk_NFL14")
covariates <- c("dem_female + dem_age + dem_white + dem_college + dem_work +
                dem_pid3 + dem_ideo5 + dem_income")

probity_ate <- ate_plot(df = trust_df, covariates = covariates, 
                        loopvar = study, dv = "probity_scale")

g1 <- 
  probity_ate$plot + 
  scale_colour_grey(start = 0.2, end = 0.6) +
  scale_y_discrete(labels = c("Study 1 (N = 643)", 
                              "Study 2 (N = 1,324)",
                              "Study 3 (N = 584)")) + 
  xlab("Average Treatment Effect Estimates in Standard Units") +
  facet_wrap(~ estimator) +
  theme_kyle() +
  coord_cartesian(xlim = c(-1, 1)) +
  theme(legend.position = "bottom",
          legend.title = element_blank(),
        axis.title.y = element_blank(),
        strip.background = element_blank(),
        panel.spacing = unit(2, "lines"))

ggsave(paste0(plotpath, "probity_ate_scale_ates.tiff"), g1, device = "tiff",
       width = 7, height = 5)
setwd(plotpath)
system("open probity_ate_scale_ates.tiff")


probity_regtable <-
  probity_ate$gg_df %>%
  mutate(entry = make_entry(est = est, se = se, p = p, digits = 2)) %>% 
  select(estimator, entry, experiment, treatment) %>%
  spread(estimator, entry) 

xtable(probity_regtable) %>%
  print(include.rownames = FALSE, 
        type = "html",
        file = paste0(tabpath, "probity_ates.html"))

####-------------S11 Table------####
trust_df <- 
  trust_df %>%
  mutate(D = ifelse(Z == "Placebo", 0, NA),
         D = ifelse(Z == "Corrupt", 1, D))

nboot <- 2000
est_boot_mturk <- est_boot_qualtrics <- numeric()
for(j in 1:nboot){
  
  boot_df <- 
    trust_df %>% 
    filter(experiment == study[1]) %>%
    sample_frac(., replace = TRUE)
  
  est_boot_mturk[j] <- coef(vglm(fmptrust_index ~ D, tobit(Lower = 4), 
                                 data = boot_df))[3]
  
  boot_df <- 
    trust_df %>% 
    filter(experiment == study[2]) %>%
    sample_frac(., replace = TRUE)
  
  est_boot_qualtrics[j] <- coef(vglm(fmptrust_index ~ D, tobit(Lower = 4), 
                                     data = boot_df))[3]
  
}

round(quantile(est_boot_mturk, c(0.025, 0.975)), 2)
round(quantile(est_boot_qualtrics, c(0.025, 0.975)), 2)

coef(vglm(fmptrust_index ~ D, tobit(Lower = 4), data = trust_df %>% 
            filter(experiment == study[1])))[3]

coef(vglm(fmptrust_index ~ D, tobit(Lower = 4), data = trust_df %>% 
            filter(experiment == study[2])))[3]

# Compare to OLS
tidy(lm_robust(fmptrust_index ~ D, data = trust_df %>% 
                 filter(experiment == study[1]))) %>%
  mutate_if(is.numeric, funs(round(., 2)))

tidy(lm_robust(fmptrust_index ~ D, data = trust_df %>% 
                 filter(experiment == study[2]))) %>%
  mutate_if(is.numeric, funs(round(., 2)))


