# campaign experiment
library(data.table)
library(tidyverse)
library(dtplyr)
library(colorblindr)
library(readxl)
library(grf)

theme_jg <- function(){
  theme_classic()+
    theme(text = element_text(family = "serif", size = 16),
          strip.text = element_text(face = "bold"),
          plot.title = element_text(face = "bold", size = 20))
}

here::here()

# read in data
datsub <- readr::read_csv("data/clean_correspondence_data.csv")

# simple lm
library(fixest)
ate <- fixest::feglm(outcome ~ ideo_treat, data = datsub, family = "binomial")
ate_int <- fixest::feglm(outcome ~ ideo_treat*abs(tweetscore_noise), data = datsub, family = "binomial")
ate_gen <- fixest::feglm(outcome ~ email_alias, data = datsub, family = "binomial")
ate_gen_int <- fixest::feglm(outcome ~ email_alias*abs(tweetscore_noise), data = datsub, family = "binomial")
ate_gen_intp <- fixest::feglm(outcome ~ email_alias*PartyD, data = datsub, family = "binomial")

ate_int_alt <- fixest::feglm(outcome ~ ideo_treat*abs(X1), data = datsub, family = "binomial")
ate_int_al2 <- fixest::feglm(outcome ~ ideo_treat*X2, data = datsub, family = "binomial")

ate.lm <- fixest::feols(outcome ~ ideo_treat, data = datsub)
ate_int.lm <- fixest::feols(outcome ~ ideo_treat*abs(tweetscore_noise), data = datsub)
ate_gen.lm <- fixest::feols(outcome ~ email_alias, data = datsub)
ate_gen_int.lm <- fixest::feols(outcome ~ email_alias*abs(tweetscore_noise), data = datsub)
ate_gen_intp.lm <- fixest::feols(outcome ~ email_alias*PartyD, data = datsub)

ate_int_alt.lm <- fixest::feols(outcome ~ ideo_treat*abs(X1), data = datsub)
ate_int_al2.lm <- fixest::feols(outcome ~ ideo_treat*X2, data = datsub)

etable(ate, ate_int, ate_int_alt, ate_int_al2)
etable(ate_gen, ate_gen_int, ate_gen_intp)

etable(ate.lm, ate_int.lm, ate_int_alt.lm)
etable(ate_gen.lm, ate_gen_int.lm, ate_gen_intp.lm)


# for main
ate.main <- lm(outcome ~ treat.gen, data = datsub)
ate.int <- lm(outcome ~ treat.gen*abs(tweetscore_noise), data= datsub)
ate.int.d <- lm(outcome ~ treat.gen*abs(tweetscore_noise), data= datsub %>% filter(PartyD == 1))
ate.int.r <- lm(outcome ~ treat.gen*abs(tweetscore_noise), data= datsub %>% filter(PartyR == 1))

# prep for forest
datprep <- datsub %>%
  
  # subset (remove variables that were manipulated but not overwritten)
  dplyr::select(Office, Incumbent, PartyD, PartyR, 
                gender_treat, ideo_treat, 
                tweetscore_noise, X1, X2, 
                outcome) %>%
  
  # add extremity in addition to base tweetscore_noise
  mutate(extremity1 = abs(tweetscore_noise),
         extremity2 = abs(X1),
         extremity3 = -1*X2) %>%
  na.omit()

# split to test and training set
# skip for now
#set.seed(1839)
#cases <- sample(seq_len(nrow(datsub)), round(nrow(datprep) * .6))
#train <- datprep[cases, ]
#test <- datprep[-cases, ]

input_main <- data.frame(model.matrix(~ ., data = datprep %>% 
                                   dplyr::select(-outcome, -ideo_treat, 
                                                 -tweetscore_noise, -X1, -X2)))

# run causal forests (main +2 for SI)
cfs.all <- lapply(1:3, function(x){
  
  which.out <- c(1:3)[-x]
  
  extrem.out <- paste0("extremity", which.out)
  
  message(paste0("running forest with extremity version ", x))
  
  mod <- causal_forest(
    
    # include all vars
    X = input_main[,-which(names(input_main) %in% c("X.Intercept.", extrem.out))],
    
    # specify outcome
    Y = datprep %>% pull(outcome),
    
    # specify treatment
    W = datprep %>% pull(ideo_treat),
    num.trees = 5000,
    seed = 1111
  )
  
  return(mod)
})

names(cfs.all) <- c("main", "alt1", "alt2")

get_varimp <- function(forest){
  varimp <- variable_importance(forest)
  varimp_df <- data.frame(var = names(forest$X.orig),
                          imp = varimp)
  return(varimp_df %>% arrange(desc(imp)))
}

# check variable importance

# identify important variables
imp_main <- get_varimp(forest = cfs.all$main)
imp_alt1 <- get_varimp(forest = cfs.all$alt1)
imp_alt2 <- get_varimp(forest = cfs.all$alt2)

# get predictions (out of bag, so not quite 'in sample')
id_preds_main <- predict(cfs.all$main, estimate.variance = TRUE)
id_preds_alt1 <- predict(cfs.all$alt1, estimate.variance = TRUE)
id_preds_alt2 <- predict(cfs.all$alt2, estimate.variance = TRUE)

# check distribution
hist(id_preds_main$predictions)
hist(id_preds_alt1$predictions)
hist(id_preds_alt2$predictions)

# append predictions
input_main$obs_preds <- id_preds_main$predictions
input_main$obs_vars <- id_preds_main$variance.estimates

input_main$obs_preds_alt1 <- id_preds_alt1$predictions
input_main$obs_vars_alt1 <- id_preds_alt1$variance.estimates

input_main$obs_preds_alt2 <- id_preds_alt2$predictions
input_main$obs_vars_alt2 <- id_preds_alt2$variance.estimates

input_main$preds_rank <- rank(input_main$obs_preds)
input_main$preds_quantile <- ntile(input_main$preds_rank , 5)
input_main$preds_decile <- ntile(input_main$preds_rank , 10)

input_main$preds_rank_alt1 <- rank(input_main$obs_preds_alt1)
input_main$preds_quantile_alt1 <- ntile(input_main$preds_rank_alt1 , 5)
input_main$preds_decile_alt1 <- ntile(input_main$preds_rank_alt1 , 10)

input_main$preds_rank_alt2 <- rank(input_main$obs_preds_alt2)
input_main$preds_quantile_alt2 <- ntile(input_main$preds_rank_alt2 , 5)
input_main$preds_decile_alt2 <- ntile(input_main$preds_rank_alt2 , 10)

input_main$extrem_rank <- rank(input_main$extremity1)
input_main$extrem_quantile <- ntile(input_main$extrem_rank , 5)

input_main$extrem_rank_alt1 <- rank(input_main$extremity2)
input_main$extrem_quantile_alt1 <- ntile(input_main$extrem_rank_alt1 , 5)

input_main$extrem_rank_alt2 <- rank(input_main$extremity3)
input_main$extrem_quantile_alt2 <- ntile(input_main$extrem_rank_alt2 , 5)


ate_quantiles <- 
  bind_rows(lapply(1:5, function(x){
    ate <- average_treatment_effect(cfs.all$main, 
                                    target.sample = "all",
                                    subset = which(input_main$preds_quantile == x))
    df <- data.frame(est = ate[1],
                     sd = ate[2],
                     quantile = x)
    return(df)
  }))

ate_deciles <- 
  bind_rows(lapply(1:10, function(x){
    ate <- average_treatment_effect(cfs.all$main, 
                                    target.sample = "all",
                                    subset = which(input_main$preds_decile == x))
    df <- data.frame(est = ate[1],
                     sd = ate[2],
                     decile = x)
    return(df)
  }))
ate_overall <- average_treatment_effect(cfs.all$main, target.sample = "all")

ate_quantiles %>%
  ggplot()+
  geom_ribbon(aes(x = quantile,
                  ymin = ate_overall[1] - 1.96*ate_overall[2],
                  ymax = ate_overall[1] + 1.96*ate_overall[2]),
              col = "grey",alpha = .5)+
  geom_pointrange(aes(x = quantile, y = est, ymin = est - 1.96*sd, ymax = est + 1.96*sd))+
  geom_hline(yintercept = 0)+
  labs(x = "Rank Quantile",
       y = "Sorted Group Average Treatment Effect",
       title = "A null overall effect masks significant heterogeneous effects",
       subtitle = "95% interval for overall ATE shown in ribbon")+
  theme_jg()

ate_deciles %>%
  ggplot()+
  geom_ribbon(aes(x = decile,
                  ymin = ate_overall[1] - 1.96*ate_overall[2],
                  ymax = ate_overall[1] + 1.96*ate_overall[2]),
              col = "grey",alpha = .5)+
  geom_pointrange(aes(x = decile, y = est, ymin = est - 1.96*sd, ymax = est + 1.96*sd))+
  geom_hline(yintercept = 0)+
  labs(x = "Rank Decile",
       y = "Sorted Group Average Treatment Effect",
       title = "A null overall effect masks significant heterogeneous effects",
       subtitle = "95% interval for overall ATE shown in ribbon")+
  theme_bw()

ate_extrem_quantiles <- 
  bind_rows(lapply(1:5, function(x){
    ate <- average_treatment_effect(cfs.all$main, 
                                    target.sample = "all",
                                    subset = which(input_main$extrem_quantile == x))
    df <- data.frame(est = ate[1],
                     sd = ate[2],
                     quantile = x)
    return(df)
  }))

ate_extrem_plot <- 
  ate_extrem_quantiles %>%
  ggplot(aes(x = quantile, y = est, ymin = est - 1.96*sd, ymax = est + 1.96*sd))+
  geom_pointrange()+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = ate_overall[1], lty = "dashed")+
  labs(x = "Candidate Extremity Quintile",
       y = "Group Average Treatment Effect\nVolunteer Extreme",
       title = "Grouped Effects of Volunteer Extremity by Candidate Extremity",
       subtitle = "Average treatment effect of extreme volunteer on response rate shown with dashed line")+
  theme_jg()
ggsave(ate_extrem_plot, file = "output/figures/cates_extrem_quintiles_tsnoise.png", width = 10, height = 5)

plot_extrem_fx <- function(forest, name_extrem){
  
  ate_overall <- grf::average_treatment_effect(forest, target.sample = "all")
  preds <- predict(forest, estimate.variance = TRUE)
  
  preds <- preds %>%
    mutate(se = sqrt(variance.estimates)) %>%
    mutate(lwr = predictions - 1.96*se,
           upr = predictions + 1.96*se)
  
  preds$extrem <- forest$X.orig %>% pull(name_extrem)
  
  if(name_extrem == "extremity3"){
    preds$extrem = -1*preds$extrem
  }
  
  fx_plot <- 
    preds %>%
    ggplot(aes(x = extrem, y = predictions))+
    geom_pointrange(aes(ymin = lwr,
                        ymax = upr), col = "grey",
                    alpha = .3)+
    geom_point(size = .2)+
    geom_hline(yintercept = 0)+
    geom_hline(yintercept = ate_overall[1], lty = "dashed")+
    theme_jg()
}

plot_ind_fx <- function(forest, name_extrem){
  
  ate_overall <- grf::average_treatment_effect(forest, target.sample = "all")
  preds <- predict(forest, estimate.variance = TRUE)
  
  preds <- preds %>%
    mutate(se = sqrt(variance.estimates)) %>%
    mutate(lwr = predictions - 1.96*se,
           upr = predictions + 1.96*se)
  
  preds$extrem <- forest$X.orig %>% pull(name_extrem)
  
  if(name_extrem == "extremity3"){
    preds$extrem = -1*preds$extrem
  }
    
  fx_plot <- 
    preds %>%
    ggplot(aes(x = rank(predictions), y = predictions))+
    geom_pointrange(aes(ymin = lwr,
                        ymax = upr), col = "grey",
                    alpha = .1)+
    geom_point(aes(col = extrem),
               size = .2,
               alpha = .3)+
    geom_hline(yintercept = 0)+
    geom_hline(yintercept = ate_overall[1], lty = "dashed")+
    coord_flip()+
    scale_color_gradient(name = "Candidate Extremity",
                         low = "white", high = "black")+
    theme_jg()
}

extrem_fx_plot <- plot_extrem_fx(forest = cfs.all$main, name_extrem = "extremity1")+
  labs(x = "Candidate Extremity",
       y = "Predicted Effect",
       title = "Individual Effects of Volunteer Extremity by Candidate Extremity",
       subtitle = "Average treatment effect of extreme volunteer on response rate shown with dashed line")

extrem_fx_plot_alt1 <- plot_extrem_fx(forest = cfs.all$alt1, name_extrem = "extremity2")+
  labs(x = "Candidate Extremity",
       y = "Predicted Effect",
       title = "Individual Effects of Volunteer Extremity by Candidate Extremity\n(First Alternate Specification)",
       subtitle = "Average treatment effect of extreme volunteer on response rate shown with dashed line")

extrem_fx_plot_alt2 <- plot_extrem_fx(forest = cfs.all$alt2, name_extrem = "extremity3")+
  labs(x = "Candidate Extremity",
       y = "Predicted Effect",
       title = "Individual Effects of Volunteer Extremity by Candidate Extremity\n(Second Alternate Specification)",
       subtitle = "Average treatment effect of extreme volunteer on response rate shown with dashed line")

ind_fx_plot <- plot_ind_fx(forest = cfs.all$main, name_extrem = "extremity1")+
  labs(x = "Predicted Effect Rank",
       y = "Predicted Effect",
       title = "Sorted Individual Effects",
       subtitle = "Average treatment effect of extreme volunteer on response rate shown with dashed line")

ind_fx_plot_alt1 <- plot_ind_fx(forest = cfs.all$alt1, name_extrem = "extremity2")+
  labs(x = "Predicted Effect Rank",
       y = "Predicted Effect",
       title = "Sorted Individual Effects (First Alternate Specification)",
       subtitle = "Average treatment effect of extreme volunteer on response rate shown with dashed line")

ind_fx_plot_alt2 <- plot_ind_fx(forest = cfs.all$alt2, name_extrem = "extremity3")+
  labs(x = "Predicted Effect Rank",
       y = "Predicted Effect",
       title = "Sorted Individual Effects (Second Alternate Specification)",
       subtitle = "Average treatment effect of extreme volunteer on response rate shown with dashed line")

ggsave(extrem_fx_plot, file = "output/figures/exfx_extrem_tsnoise.png", width = 10, height = 5)
ggsave(extrem_fx_plot_alt1, file = "output/figures/exfx_extrem_alt1_tsnoise.png", width = 10, height = 6)
ggsave(extrem_fx_plot_alt2, file = "output/figures/exfx_extrem_alt2_tsnoise.png", width = 10, height = 6)

ggsave(ind_fx_plot, file = "output/figures/indfx_extrem_tsnoise.png", width = 10, height = 5)
ggsave(ind_fx_plot_alt1, file = "output/figures/indfx_extrem_alt1_tsnoise.png", width = 10, height = 5)
ggsave(ind_fx_plot_alt2, file = "output/figures/indfx_extrem_alt2_tsnoise.png", width = 10, height = 5)

plot_ind_byExtrem <- function(forest, name_extrem){
  ate_overall <- grf::average_treatment_effect(forest, target.sample = "all")
  
  preds <- predict(forest, estimate.variance = TRUE)
  
  preds <- preds %>%
    mutate(se = sqrt(variance.estimates)) %>%
    mutate(lwr = predictions - 1.96*se,
           upr = predictions + 1.96*se)
  
  preds$extrem <- forest$X.orig %>% pull(name_extrem)
  
  fx_plot <- 
    preds %>%
    ggplot(aes(x = extrem, y = predictions))+
    geom_pointrange(aes(ymin = lwr,
                        ymax = upr),
                    col = "grey", alpha = .3)+
    geom_point(size = .3)+
    geom_hline(yintercept = 0)+
    geom_hline(yintercept = ate_overall[1], lty = "dashed")+
    stat_smooth(method = "loess")
}


extrem_plot <- plot_ind_byExtrem(forest = cfs.all$main, name_extrem = "extremity1")+
  labs(x = "Candidate Extremity",
       y = "Predicted Effect",
       title = "Effects by Extremity",
       subtitle = "Overall average treatment effect shown with dashed line")+
  theme_jg()
extrem_plot_alt1 <- plot_ind_byExtrem(forest = cfs.all$alt1, name_extrem = "extremity2")+
  labs(x = "Candidate Extremity",
       y = "Predicted Effect",
       title = "Effects by Extremity (First Alternate Specification)",
       subtitle = "Overall average treatment effect shown with dashed line")+
  theme_jg()
extrem_plot_alt2 <- plot_ind_byExtrem(forest = cfs.all$alt2, name_extrem = "extremity3")+
  labs(x = "Candidate Extremity",
       y = "Predicted Effect",
       title = "Effects by Extremity (Second Alternate Specification)",
       subtitle = "Overall average treatment effect shown with dashed line")+
  theme_jg()

ggsave(extrem_plot, file = "output/figures/indfx_extrem_tsnoise.png", width = 8, height = 8)
ggsave(extrem_plot_alt1, file = "output/figures/indfx_extrem_alt1_tsnoise.png", width = 8, height = 8)
ggsave(extrem_plot_alt2, file = "output/figures/indfx_extrem_alt2_tsnoise.png", width = 8, height = 8)

#save(input_main, 
#     id_preds_alt1, id_preds_alt2, id_preds_main, 
#     datsub,
#     file = "output/hfx_SI_output.RData")

# for the SI
hfx_A1 <- datsub %>%
  filter(!is.na(X1) & !is.na(tweetscore_noise)) %>%
  ggplot(aes(x = tweetscore_noise, y = X1))+
  geom_point() +
  labs(x = "tweetscore_noise", y = "Full Follow Profile First Dimension",
       title = "Correspondence Between Alternate Ideal Points")+
  theme_jg()
ggsave(hfx_A1, file = "output/figures/tweetscore_noises_vs_X1_tsnoise.png", width = 10, height = 5)

hfx_A2 <- datsub %>%
  filter(!is.na(X1)) %>%
  ggplot(aes(x = X1, y = X2))+
  geom_point() +
  labs(x = "Full Follow Profile First Dimension", y = "Full Follow Profile Second Dimension",
       title = "Alternate Scaling First Two Dimensions")+
  theme_jg()
ggsave(hfx_A2, file = "output/figures/X1_vs_X2.png", width = 10, height = 5)

