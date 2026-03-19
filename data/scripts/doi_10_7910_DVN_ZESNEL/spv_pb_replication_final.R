###############################################################################   
#### Taegyoon Kim. The Effects of Partisan Elites' Violent Rhetoric on     ####
#### Support for Political Violence. Political Behavior. 2025.             ####
###############################################################################  


###############################################################################
################################### set up ####################################
###############################################################################

### packages

pacman::p_load("readr", "dplyr", "ggplot2", "ggthemes", "texreg", "tibble",
               "mediation", "tidyr", "MKinfer", "interplot", "nnet", 
               "marginaleffects", "interflex")

### set path (set your own path for data and figure directories)

path_data <- "/Users/taegyoon/Dropbox/Materials/research/support_violence/" 
path_output <- "/Users/taegyoon/Dropbox/Materials/research/support_violence/figs/"

### read data sets

df <- read_csv( # four treatment groups, finished survey, submitted data
  paste0(path_data, "df_spv.csv") # n = 1,857
  )
df_full <- read_csv( # all groups (+ recipe group), finished survey, submitted data
  paste0(path_data, "df_full_spv.csv") # n = 2,312
  )

df_full_submit <- read_csv( # all groups (+ recipe group), finished survey
  paste0(path_data, "df_full_submit_spv.csv") # n = 2,503 (191 withdrew data)
  )

df$race <- relevel(as.factor(df$race), ref = "White")
df$latin <- relevel(as.factor(df$latin), ref = "Non-Latin/Hispanic")

df_full$race <- relevel(as.factor(df_full$race), ref = "White")
df_full$latin <- relevel(as.factor(df_full$latin), ref = "Non-Latin/Hispanic")


###############################################################################
############################# t-tests (figure 4a) #############################
###############################################################################

### violent vs. non-violent

set.seed(8480)
perm.t.test(
  df$support ~ df$treatment_violent, 
  R = 10000)

### violent vs. non-political

set.seed(8480)
perm.t.test(
  df_full[which(df_full$treatment_violent != 0), ]$support ~ 
    df_full[which(df_full$treatment_violent != 0), ]$treatment_violent, 
  R = 10000)

### non-violent vs. non-political

set.seed(8480)
perm.t.test(
  df_full[which(df_full$treatment_violent != 1), ]$support ~ 
    df_full[which(df_full$treatment_violent != 1), ]$treatment_violent, 
  R = 10000)

### figure 4 (a)

df_full$treatment_all_factor <- ifelse(
  df_full$treatment_violent == 1, "Violent",
  ifelse(df_full$treatment_violent == 0, "Non-violent", "Non-political"))
df_full$treatment_all_factor <- factor(
  df_full$treatment_all_factor,
  levels = c("Violent", "Non-violent", "Non-political"))

ggplot(df_full, aes(
  x = treatment_all_factor, 
  y = support,
  color = treatment_all_factor)) + 
  geom_boxplot(width = 0.75, show.legend = FALSE, outlier.shape = 1) +
  scale_color_manual(values = c("black", "black", "black")) + 
  geom_jitter(size = 5, shape = 1, position = position_jitter(0.2), show.legend = FALSE) + 
  labs(x = "\nGroup", 
       y = "Support for Political Violence\n", 
       colour = "Treatment") +
  annotate("text", 
           x = 1, 
           y = 110, 
           col = 'black',
           size = 8,
           label = paste0('Mean: ', round(mean(df_full[which(df_full$treatment_violent==1),]$support), 1))) +
  annotate("text", 
           x = 2, 
           y = 110, 
           col = 'black',
           size = 8,
           label = paste0('Mean: ', round(mean(df_full[which(df_full$treatment_violent==0),]$support), 1))) +
  annotate("text", 
           x = 3, 
           y = 110, 
           col = 'black',
           size = 8,
           label = paste0('Mean: ', round(mean(df_full[which(df_full$treatment_violent==9),]$support), 1))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(angle= 0, vjust = 0.5, size = 30),
        axis.text.y = element_text(size = 30),
        legend.text = element_text(size = 30),
        legend.position = "none") 
ggsave(paste0(path_output, 'mean_diff_all.pdf'),
       dpi = 100,
       width = 12,
       height = 10,
       units = 'in')


###############################################################################
##################### general effect (figure 4b; table a6) ####################
###############################################################################

### without covariates 

ols_marg <- lm(support ~ 
                 treatment_violent + treatment_coparty,
               data = df)

### with covariates

ols_marg_cov_all <- lm(support ~ 
                         treatment_violent +
                         treatment_coparty +
                         party_gen + ideology +  
                         gender + age_ord + race + latin + 
                         education + income,
                       data = df)

### figure 4 (b)

df_marg_coef <- data.frame(summary(ols_marg_cov_all)$coefficients)[2:16, ]
df_marg_coef <- rownames_to_column(df_marg_coef, var = "predictor")
df_marg_coef$predictor <- c("Violent", "Co-party", "Republican", 
                            "Conservatism", "Gender: Non-binary", 
                            "Gender: Women", "Age", "Race: Asian", 
                            "Race: Black", "Race: Multi", "Race: Native", 
                            "Race: Pacific", "Latin/Hispanic", "Education", 
                            "Income")
df_marg_coef$ci_low_95 <- df_marg_coef$Estimate - (1.96 *  df_marg_coef$Std..Error)
df_marg_coef$ci_high_95 <- df_marg_coef$Estimate + (1.96 *  df_marg_coef$Std..Error)
df_marg_coef$ci_low_90 <- df_marg_coef$Estimate - (1.64 *  df_marg_coef$Std..Error)
df_marg_coef$ci_high_90 <- df_marg_coef$Estimate + (1.64 *  df_marg_coef$Std..Error)

ggplot(data = df_marg_coef, 
       aes(x = Estimate, 
           y = predictor)) +
  geom_linerange(aes(xmin = ci_low_95, xmax = ci_high_95),  lwd = 0.75) +
  geom_linerange(aes(xmin = ci_low_90, xmax = ci_high_90),  lwd = 1.5) +
  geom_point(size = 5, shape = 21, , color = "black", fill = "white") +
  geom_vline(xintercept = 0, color = "black", linetype = 2) +
  labs(title = "",
       x = "\nCoefficients",
       y = "Predictors\n") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(angle= 0, vjust = 0.5, size = 30),
        axis.text.y = element_text(size = 30),
        strip.text = element_text(size = 30),
        legend.text = element_text(size = 30)) 
ggsave(paste0(path_output, 'coef_marg.pdf'),
       dpi = 100,
       width = 12,
       height = 10,
       units = 'in')

### table a6

texreg(list(ols_marg, ols_marg_cov_all), 
       stars = c(0.01, 0.05, 0.1), 
       digits = 2)
texreg(list(ols_marg, ols_marg_cov_all), 
       stars = c(0.01, 0.05, 0.1), 
       digits = 3)


###############################################################################
############ source partisanship (figure 5a-b; table a7; table a8) ############
###############################################################################

### without covariates

ols_inter <- lm(support ~ 
                  treatment_violent * treatment_coparty,
                data = df)

### with covariates

ols_inter_cov_all <- lm(support ~ 
                          treatment_violent * treatment_coparty +
                          party_gen + ideology +  
                          gender + age_ord + race + latin + 
                          education + income,
                        data = df)

### figure 5 (a)

beta.hat <- coef(ols_inter_cov_all) 
cov <- vcov(ols_inter_cov_all)
z0 <- c(1, 0)
dy.dx <- beta.hat["treatment_violent"] +
  beta.hat["treatment_violent:treatment_coparty"] * z0
se.dy.dx <- sqrt(
  cov["treatment_violent", 
      "treatment_violent"] + 
    z0^2*cov["treatment_violent:treatment_coparty", 
             "treatment_violent:treatment_coparty"] + 
    2*z0*cov["treatment_violent", 
             "treatment_violent:treatment_coparty"])
upr_95 <- dy.dx + 1.96 * se.dy.dx
lwr_95 <- dy.dx - 1.96 * se.dy.dx
upr_90 <- dy.dx + 1.64 * se.dy.dx
lwr_90 <- dy.dx - 1.64 * se.dy.dx
coparty <- c("Co-party", "Opposing party")
coef_int <- data.frame(coparty, dy.dx, se.dy.dx, upr_95, lwr_95, upr_90, lwr_90)

ggplot(coef_int, aes(x = coparty, y = dy.dx, group = coparty)) +
  geom_linerange(data = coef_int, 
                 aes(ymin = lwr_95, ymax = upr_95), lwd = 1.25) +
  geom_linerange(data = coef_int, 
                 aes(ymin = lwr_90, ymax = upr_90), lwd = 2.5) +
  geom_point(size = 10, shape = 21, , color = "black", fill = "white") +
  labs(x = "\nSource Partisanship", 
       y = "Coefficient on Violent Treatment\n", 
       colour = "Source Partisanship") + 
  geom_hline(yintercept = 0, color = "grey60", linetype = 2) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title = element_text(size = 30),
        axis.text = element_text(size = 30), 
        axis.text.x = element_text(angle = 0, vjust = 0.5), 
        strip.text = element_text(size = 30),
        legend.text = element_text(size = 30))
ggsave(paste0(path_output, 'coef_cond_1.pdf'),
       dpi = 100,
       width = 12,
       height = 10,
       units = 'in')

### figure 5 (b)

df_int_coef <- data.frame(summary(ols_inter_cov_all)$coefficients)[2:17, ]
df_int_coef <- rownames_to_column(df_int_coef, var = "predictor")
df_int_coef$predictor <- c("Violent", "Co-party", "Republican", 
                           "Conservatism", "Gender: Non-binary", 
                           "Gender: Women", "Age", "Race: Asian", 
                           "Race: Black", "Race: Multi", "Race: Native", 
                           "Race: Pacific", "Latin/Hispanic", "Education", 
                           "Income", "Violent x Co-party")
df_int_coef$predictor <- factor(
  df_int_coef$predictor,
  levels = rev(c("Violent", "Co-party", "Violent x Co-party", 
                 "Republican", "Conservatism", "Gender: Non-binary", 
                 "Gender: Women", "Age", "Race: Asian", 
                 "Race: Black", "Race: Multi", "Race: Native", 
                 "Race: Pacific", "Latin/Hispanic", "Education", 
                 "Income")))
df_int_coef$ci_low_95 <- df_int_coef$Estimate - (1.96 *  df_int_coef$Std..Error)
df_int_coef$ci_high_95 <- df_int_coef$Estimate + (1.96 *  df_int_coef$Std..Error)
df_int_coef$ci_low_90 <- df_int_coef$Estimate - (1.64 *  df_int_coef$Std..Error)
df_int_coef$ci_high_90 <- df_int_coef$Estimate + (1.64 *  df_int_coef$Std..Error)

ggplot(data = df_int_coef, 
       aes(x = Estimate, 
           y = predictor)) +
  geom_linerange(aes(xmin = ci_low_95, xmax = ci_high_95),  lwd = 0.75) +
  geom_linerange(aes(xmin = ci_low_90, xmax = ci_high_90),  lwd = 1.5) +
  geom_point(size = 5, shape = 21, , color = "black", fill = "white") +
  geom_vline(xintercept = 0, color = "black", linetype = 2) +
  labs(title = "",
       x = "\nCoefficients",
       y = "Predictors\n") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title = element_text(size = 30),
        axis.text = element_text(size = 30), 
        axis.text.x = element_text(angle = 0, vjust = 0.5), 
        strip.text = element_text(size = 30),
        legend.text = element_text(size = 30))
ggsave(paste0(path_output, 'coef_cond_2.pdf'),
       dpi = 100,
       width = 12,
       height = 10,
       units = 'in')

### table a7

texreg(list(ols_inter, ols_inter_cov_all), 
       stars = c(0.01, 0.05, 0.1), 
       digits = 2)
texreg(list(ols_inter, ols_inter_cov_all), 
       stars = c(0.01, 0.05, 0.1), 
       digits = 3)

### split sample regression analysis

ols_split_coparty <- lm(support ~ 
                          treatment_violent,
                        data = df[which(df$treatment_coparty == 1), ])

ols_split_coparty_cov_all <- lm(support ~ 
                                  treatment_violent +
                                  party_gen + ideology +  
                                  gender + age_ord + race + latin + 
                                  education + income,
                                data = df[which(df$treatment_coparty == 1), ])

ols_split_oppoparty <- lm(support ~ 
                            treatment_violent, 
                          data = df[which(df$treatment_coparty == 0), ])

ols_split_oppoparty_cov_all <- lm(support ~ 
                                    treatment_violent +
                                    party_gen + ideology +  
                                    gender + age_ord + race + latin + 
                                    education + income,
                                  data = df[which(df$treatment_coparty == 0), ])

### table a8

texreg(list(ols_split_coparty, ols_split_coparty_cov_all, 
            ols_split_oppoparty, ols_split_oppoparty_cov_all), 
       stars = c(0.01, 0.05, 0.1),
       digits = 2)

texreg(list(ols_split_coparty, ols_split_coparty_cov_all, 
            ols_split_oppoparty, ols_split_oppoparty_cov_all), 
       stars = c(0.01, 0.05, 0.1),
       digits = 3)


###############################################################################
########## differences in treatment effects (figure 6; table a9-11) ###########
###############################################################################

### partisanship (huddy et al. measure)

ols_cov_all_p1 <- lm(support ~ 
                       treatment_violent * party_social_stan + 
                       treatment_coparty + party_gen + ideology + gender + 
                       age_ord + race + latin + education + income,
                     data = df)

df$party_strength <- as.factor(df$party_strength)

ols_cov_all_p2 <- lm(support ~ 
                       treatment_violent * party_strength + 
                       treatment_coparty + party_gen + ideology + gender + 
                       age_ord + race + latin + education + income,
                     data = df)

### figure 6 (a)

interplot(m = ols_cov_all_p1, 
          var1 = "treatment_violent", 
          var2 = "party_social_stan", hist = TRUE) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  labs(x = "\nPartisan Identity Strength", 
       y = "Coefficient on Violence Treatment\n") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(angle= 0, vjust = 0.5, size = 30),
        axis.text.y = element_text(size = 30),
        strip.text = element_blank(),
        legend.text = element_text(size = 30)) 
ggsave(paste0(path_output, 'coef_hte_p1.pdf'),
       dpi = 100,
       width = 12,
       height = 10,
       units = 'in')

### partisanship (anes measure)

beta.hat <- coef(ols_cov_all_p2) 
cov <- vcov(ols_cov_all_p2)
z0 <- c(1, 0)
dy.dx <- beta.hat["treatment_violent"] + beta.hat["treatment_violent:party_strength1"]*z0
se.dy.dx <- sqrt(cov["treatment_violent", "treatment_violent"] + z0^2*cov["treatment_violent:party_strength1", "treatment_violent:party_strength1"] + 2*z0*cov["treatment_violent", "treatment_violent:party_strength1"])
upr_95 <- dy.dx + 1.96*se.dy.dx
lwr_95 <- dy.dx - 1.96*se.dy.dx
upr_90 <- dy.dx + 1.64*se.dy.dx
lwr_90 <- dy.dx - 1.64*se.dy.dx
coparty <- c("Strong", "Not so strong")
coef_int <- data.frame(coparty, dy.dx, se.dy.dx, upr_95, lwr_95, upr_90, lwr_90)

### figure 6 (b)

ggplot(coef_int, aes(x = coparty, y = dy.dx, group = coparty)) +
  geom_linerange(data = coef_int, 
                 aes(ymin = lwr_95, ymax = upr_95), lwd = 1.25) +
  geom_linerange(data = coef_int, 
                 aes(ymin = lwr_90, ymax = upr_90), lwd = 2.5) +
  geom_point(size = 5, shape = 21, , color = "black", fill = "white") +
  labs(x = "\nPartisan Identity Strength", 
       y = "Coefficient on Violent Treatment\n") + 
  geom_hline(yintercept = 0, color = "grey60", linetype = 2) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(angle= 0, vjust = 0.5, size = 30),
        axis.text.y = element_text(size = 30),
        strip.text = element_blank(),
        legend.text = element_text(size = 30)) 
ggsave(paste0(path_output, 'coef_hte_p2.pdf'),
       dpi = 100,
       width = 12,
       height = 10,
       units = 'in')

### trait aggression

ols_cov_all_ta <- lm(support ~ 
                       treatment_violent * ta +
                       treatment_coparty + party_gen + ideology + gender + 
                       age_ord + race + latin + education + income,
                     data = df)

### figure 6 (c)

interplot(m = ols_cov_all_ta, 
          var1 = "treatment_violent", 
          var2 = "ta", hist = TRUE) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  labs(x = "\nTrait Aggression", 
       y = "Coefficient on Violence Treatment\n") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(angle= 0, vjust = 0.5, size = 30),
        axis.text.y = element_text(size = 30),
        strip.text = element_blank(),
        legend.text = element_text(size = 30)) 
ggsave(paste0(path_output, 'coef_hte_ta.pdf'),
       dpi = 100,
       width = 12,
       height = 10,
       units = 'in')

### norm of non-violence

ols_cov_all_concern <- lm(support ~
                            treatment_violent * concern +
                            treatment_coparty + party_gen + ideology + gender + 
                            age_ord + race + latin + education + income,
                          data = df)

### figure 6 (d)

interplot(m = ols_cov_all_concern, 
          var1 = "treatment_violent", 
          var2 = "concern", hist = TRUE) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  labs(x = "\nNorm of Non-Violence", 
       y = "Coefficient on Violence Treatment\n") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(angle= 0, vjust = 0.5, size = 30),
        axis.text.y = element_text(size = 30),
        strip.text = element_blank(),
        legend.text = element_text(size = 30)) 
ggsave(paste0(path_output, 'coef_hte_concern.pdf'),
       dpi = 100,
       width = 12,
       height = 10,
       units = 'in')

### table a9

texreg(list(ols_cov_all_p1, ols_cov_all_p2, 
            ols_cov_all_ta, ols_cov_all_concern), 
       stars = c(0.01, 0.05, 0.1),
       digits = 2)

texreg(list(ols_cov_all_p1, ols_cov_all_p2, 
            ols_cov_all_ta, ols_cov_all_concern), 
       stars = c(0.01, 0.05, 0.1),
       digits = 3)

### models based on co-party observations

ols_cov_co_p1 <- lm(support ~ 
                      treatment_violent * party_social_stan + 
                      party_gen + ideology + gender + 
                      age_ord + race + latin + education + income,
                    data = df[which(df$treatment_coparty == 1), ])

ols_cov_co_p2 <- lm(support ~ 
                      treatment_violent * party_strength + 
                      party_gen + ideology + gender + 
                      age_ord + race + latin + education + income,
                    data = df[which(df$treatment_coparty == 1), ])

ols_cov_co_ta <- lm(support ~ 
                      treatment_violent * ta +
                      party_gen + ideology + gender + 
                      age_ord + race + latin + education + income,
                    data = df[which(df$treatment_coparty == 1), ])

ols_cov_co_concern <- lm(support ~ 
                           treatment_violent * concern +
                           party_gen + ideology + gender + 
                           age_ord + race + latin + education + income,
                         data = df[which(df$treatment_coparty == 1), ])

### table a10

texreg(list(ols_cov_co_p1, ols_cov_co_p2, 
            ols_cov_co_ta, ols_cov_co_concern), 
       stars = c(0.01, 0.05, 0.1),
       digits = 2)

texreg(list(ols_cov_co_p1, ols_cov_co_p2, 
            ols_cov_co_ta, ols_cov_co_concern), 
       stars = c(0.01, 0.05, 0.1),
       digits = 3)

### model for partisan differences

ols_cov_all_partisanship <- lm(support ~ 
                                 treatment_violent * party_gen +
                                 treatment_coparty + ideology + gender + 
                                 age_ord + race + latin + education + income,
                               data = df)

### table a11

texreg(ols_cov_all_partisanship, 
       stars = c(0.01, 0.05, 0.1),
       digits = 2)

texreg(ols_cov_all_partisanship, 
       stars = c(0.01, 0.05, 0.1),
       digits = 3)


###############################################################################
################### mediation analysis (table 3; table a13) ###################
###############################################################################

### disgust

disgust_med <- lm(disgust ~ treatment_violent + treatment_coparty + ideology + 
                    gender + age_ord + race + latin + 
                    education + income +
                    party_gen, 
                  data = df)
disgust_out <- lm(support ~ treatment_violent + treatment_coparty + disgust + 
                    ideology + gender + age_ord + race + latin + 
                    education + income +
                    party_gen, 
                  data = df)

set.seed(8480)
disgust_final <- mediate(disgust_med, 
                         disgust_out, 
                         treat = 'treatment_violent', 
                         mediator = 'disgust', 
                         robustSE = TRUE,
                         sims = 2000)
summary(disgust_final) # table 3
disgust_final_boot <- mediate(disgust_med, 
                              disgust_out, 
                              treat = 'treatment_violent', 
                              mediator = 'disgust', 
                              boot = TRUE,
                              sims = 2000)
summary(disgust_final_boot) # table a13

### anger

anger_med <- lm(anger ~ treatment_violent + treatment_coparty + ideology + 
                  gender + age_ord + race + latin + 
                  education + income +
                  party_gen, 
                data = df)
anger_out <- lm(support ~ treatment_violent + treatment_coparty + anger + 
                  ideology + gender + age_ord + race + latin + 
                  education + income +
                  party_gen, 
                data = df)

set.seed(8480)
anger_final <- mediate(anger_med, 
                       anger_out, 
                       treat = 'treatment_violent', 
                       mediator = 'anger', 
                       robustSE = TRUE,
                       sims = 2000)
summary(anger_final) # table 3
anger_final_boot <- mediate(anger_med, 
                            anger_out, 
                            treat = 'treatment_violent', 
                            mediator = 'anger', 
                            boot = TRUE,
                            sims = 2000)
summary(anger_final_boot) # table a13

### shame

shame_med <- lm(shame ~ treatment_violent + treatment_coparty + ideology + 
                  gender + age_ord + race + latin + 
                  education + income +
                  party_gen, 
                data = df)
shame_out <- lm(support ~ treatment_violent + treatment_coparty + shame + 
                  ideology + gender + age_ord + race + latin + 
                  education + income +
                  party_gen, 
                data = df)

set.seed(8480)
shame_final <- mediate(shame_med, 
                       shame_out, 
                       treat = 'treatment_violent', 
                       mediator = 'shame', 
                       robustSE = TRUE,
                       sims = 2000)
summary(shame_final) # table 3
shame_final_boot <- mediate(shame_med, 
                            shame_out, 
                            treat = 'treatment_violent', 
                            mediator = 'shame', 
                            boot = TRUE,
                            sims = 2000)
summary(shame_final_boot) # table a13

### guilt

guilt_med <- lm(guilt ~ treatment_violent + treatment_coparty + ideology + 
                  gender + age_ord + race + latin + 
                  education + income +
                  party_gen, 
                data = df)
guilt_out <- lm(support ~ treatment_violent + treatment_coparty + guilt + 
                  ideology + gender + age_ord + race + latin + 
                  education + income +
                  party_gen, 
                data = df)

set.seed(8480)
guilt_final <- mediate(guilt_med, 
                       guilt_out, 
                       treat = 'treatment_violent', 
                       mediator = 'guilt', 
                       robustSE = TRUE,
                       sims = 2000)
summary(guilt_final) # table 3
guilt_final_boot <- mediate(guilt_med, 
                            guilt_out, 
                            treat = 'treatment_violent', 
                            mediator = 'guilt', 
                            boot = TRUE,
                            sims = 2000)
summary(guilt_final_boot) # table a13

### fear

fear_med <- lm(fear ~ treatment_violent + treatment_coparty + ideology + 
                 gender + age_ord + race + latin + 
                 education + income +
                 party_gen, 
               data = df)
fear_out <- lm(support ~ treatment_violent + treatment_coparty + fear + 
                 ideology + gender + age_ord + race + latin + 
                 education + income +
                 party_gen, 
               data = df)

set.seed(8480)
fear_final <- mediate(fear_med, 
                      fear_out, 
                      treat = 'treatment_violent', 
                      mediator = 'fear', 
                      robustSE = TRUE,
                      sims = 2000)
summary(fear_final) # table 3
fear_final_boot <- mediate(fear_med, 
                           fear_out, 
                           treat = 'treatment_violent', 
                           mediator = 'fear', 
                      boot = TRUE,
                      sims = 2000)
summary(fear_final_boot) # table a13


### anxiety

anxiety_med <- lm(anxiety ~ treatment_violent + treatment_coparty + ideology +
                    gender + age_ord + race + latin + 
                    education + income +
                    party_gen,
                  data = df)
anxiety_out <- lm(support ~ treatment_violent + treatment_coparty + anxiety + 
                    ideology + gender + age_ord + race + latin + 
                    education + income +
                    party_gen, 
                  data = df)

set.seed(8480)
anxiety_final <- mediate(anxiety_med,
                         anxiety_out, 
                         treat = 'treatment_violent', 
                         mediator = 'anxiety', 
                         robustSE = TRUE,
                         sims = 2000)
summary(anxiety_final) # table 3
anxiety_final_boot <- mediate(anxiety_med, 
                              anxiety_out, 
                              treat = 'treatment_violent', 
                              mediator = 'anxiety', 
                              boot = TRUE,
                              sims = 2000)
summary(anxiety_final_boot) # table a13

### sadness

sadness_med <- lm(sadness ~ treatment_violent + treatment_coparty + ideology +
                    gender + age_ord + race + latin + 
                    education + income +
                    party_gen, 
                  data = df)
sadness_out <- lm(support ~ treatment_violent + treatment_coparty + sadness + 
                    ideology + gender + age_ord + race + latin + 
                    education + income +
                    party_gen, 
                  data = df)

set.seed(8480)
sadness_final <- mediate(sadness_med, 
                         sadness_out, 
                         treat = 'treatment_violent', 
                         mediator = 'sadness', 
                         robustSE = TRUE,
                         sims = 2000)
summary(sadness_final) # table 3
sadness_final_boot <- mediate(sadness_med, 
                              sadness_out, 
                              treat = 'treatment_violent', 
                              mediator = 'sadness', 
                              boot = TRUE,
                              sims = 2000)
summary(sadness_final_boot) # table a13

### happiness

happiness_med <- lm(happiness ~ treatment_violent + treatment_coparty + ideology + 
                      gender + age_ord + race + latin + 
                      education + income +
                      party_gen, 
                    data = df)
happiness_out <- lm(support ~ treatment_violent + treatment_coparty + happiness +
                      ideology + gender + age_ord + race + latin + 
                      education + income +
                      party_gen, 
                    data = df)

set.seed(8480)
happiness_final <- mediate(happiness_med, 
                           happiness_out, 
                           treat = 'treatment_violent', 
                           mediator = 'happiness', 
                           robustSE = TRUE,
                           sims = 2000)
summary(happiness_final) # table 3
happiness_final_boot <- mediate(happiness_med, 
                                happiness_out, 
                                treat = 'treatment_violent', 
                                mediator = 'happiness', 
                                boot = TRUE,
                                sims = 2000)
summary(happiness_final_boot) # table a13


###############################################################################
####################### treatment assignment (table a1) #######################
###############################################################################

participants_pre <- table(df_full_submit$treatment_coparty, 
      df_full_submit$treatment_violent) # pre

participants_post <- table(df_full$treatment_coparty, 
      df_full$treatment_violent) # post

round(participants_post / participants_pre, 4) * 100


###############################################################################
####################### manipulation check (figure a9) ########################
###############################################################################

### dem & non-violent

df_man_dem_nv <- read_csv(
  paste0(path_data,
         "manipulation_check_dem_nonviolent_May 26, 2023_15.26.csv"))
df_man_dem_nv <- dplyr::slice(df_man_dem_nv, 3:nrow(df_man_dem_nv))
df_man_dem_nv <- df_man_dem_nv[c("nv_d_1_1", "nv_d_2_1", "nv_d_3_1", "nv_d_4_1")]
df_man_dem_nv <- df_man_dem_nv[complete.cases(df_man_dem_nv) == T, ]
names(df_man_dem_nv) <- c("Election integrity I", "Election integrity II", 
                          "Vaccine mandate I", "Vaccine mandate II")
df_man_dem_nv <- df_man_dem_nv %>%
  mutate_all(as.numeric) %>%
  gather(key = "Post", value = "score")

ggplot(data = df_man_dem_nv, 
       aes(x =  score, group = Post, color = Post, fill = Post)) +
  geom_density(adjust = 1.5, alpha = 0.05, lwd = 1) +
  geom_vline(data = df_man_dem_nv %>% dplyr::group_by(Post) %>% 
               dplyr::summarize(score = mean(score)), 
             aes(xintercept = score, color = Post),
             lwd = 1,
             linetype = "22") +
  scale_fill_manual(values = c("darkgreen", "skyblue", "darkred", "orange")) +
  scale_color_manual(values = c("darkgreen", "skyblue", "darkred", "orange")) +
  labs(title = "",
       x = "\nPerceived Violence", 
       y = "Density\n") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.position = "right",
        legend.direction = "vertical",
        plot.margin = grid::unit(c(3, 3, 3, 3), "mm")) 

### dem & violent

df_man_dem_v <- read_csv(
  paste0(path_data,
         "manipulation_check_dem_violent_May 26, 2023_15.28.csv"))
df_man_dem_v <- dplyr::slice(df_man_dem_v, 3:nrow(df_man_dem_v))
df_man_dem_v <- df_man_dem_v[c("v_d_1_1", "v_d_2_1", "v_d_3_1", "v_d_4_1")]
df_man_dem_v <- df_man_dem_v[complete.cases(df_man_dem_v) == T, ]
names(df_man_dem_v) <- c("Election integrity I", "Election integrity II", 
                         "Vaccine mandate I", "Vaccine mandate II")
df_man_dem_v <- df_man_dem_v %>%
  mutate_all(as.numeric) %>%
  gather(key = "Post", value = "score")

ggplot(data = df_man_dem_v, 
       aes(x =  score, group = Post, color = Post, fill = Post)) +
  geom_density(adjust = 1.5, alpha = 0.05, lwd = 1) +
  geom_vline(data = df_man_dem_v %>% dplyr::group_by(Post) %>% 
               dplyr::summarize(score = mean(score)), 
             aes(xintercept = score, color = Post),
             lwd = 1,
             linetype = "22") +
  scale_fill_manual(values = c("darkgreen", "skyblue", "darkred", "orange")) +
  scale_color_manual(values = c("darkgreen", "skyblue", "darkred", "orange")) +
  labs(title = "",
       x = "\nPerceived Violence", 
       y = "Density\n") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.position = "right",
        legend.direction = "vertical",
        plot.margin = grid::unit(c(3, 3, 3, 3), "mm")) 

### rep & non-violent

df_man_rep_nv <- read_csv(
  paste0(path_data,
         "manipulation_check_rep_nonviolent_May 26, 2023_15.29.csv"))
df_man_rep_nv <- dplyr::slice(df_man_rep_nv, 3:nrow(df_man_rep_nv))
df_man_rep_nv <- df_man_rep_nv[c("nv_r_1 _1", "nv_r_2_1", "nv_r_3_1", "nv_r_4_1")]
df_man_rep_nv <- df_man_rep_nv[complete.cases(df_man_rep_nv) == T, ]
names(df_man_rep_nv) <- c("Election integrity I", "Election integrity II", 
                          "Vaccine mandate I", "Vaccine mandate II")
df_man_rep_nv <- df_man_rep_nv %>%
  mutate_all(as.numeric) %>%
  gather(key = "Post", value = "score")

ggplot(data = df_man_rep_nv, 
       aes(x =  score, group = Post, color = Post, fill = Post)) +
  geom_density(adjust = 1.5, alpha = 0.05, lwd = 1) +
  geom_vline(data = df_man_rep_nv %>% dplyr::group_by(Post) %>% 
               dplyr::summarize(score = mean(score)), 
             aes(xintercept = score, color = Post),
             lwd = 1,
             linetype = "22") +
  scale_fill_manual(values = c("darkgreen", "skyblue", "darkred", "orange")) +
  scale_color_manual(values = c("darkgreen", "skyblue", "darkred", "orange")) +
  labs(title = "",
       x = "\nPerceived Violence", 
       y = "Density\n") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.position = "right",
        legend.direction = "vertical",
        plot.margin = grid::unit(c(3, 3, 3, 3), "mm")) 

### rep & violent

df_man_rep_v <- read_csv(
  paste0(path_data,
         "manipulation_check_rep_violent_May 26, 2023_15.30.csv"))
df_man_rep_v <- dplyr::slice(df_man_rep_v, 3:nrow(df_man_rep_v))
df_man_rep_v <- df_man_rep_v[c("v_r_1_1", "v_r_2_1", "v_r_3_1", "v_r_4_1")]
df_man_rep_v <- df_man_rep_v[complete.cases(df_man_rep_v) == T, ]
names(df_man_rep_v) <- c("Election integrity I", "Election integrity II", 
                         "Vaccine mandate I", "Vaccine mandate II")
df_man_rep_v <- df_man_rep_v %>%
  mutate_all(as.numeric) %>%
  gather(key = "Post", value = "score")

ggplot(data = df_man_rep_v, 
       aes(x =  score, group = Post, color = Post, fill = Post)) +
  geom_density(adjust = 1.5, alpha = 0.05, lwd = 1) +
  geom_vline(data = df_man_rep_v %>% dplyr::group_by(Post) %>% 
               dplyr::summarize(score = mean(score)), 
             aes(xintercept = score, color = Post),
             lwd = 1,
             linetype = "22") +
  scale_fill_manual(values = c("darkgreen", "skyblue", "darkred", "orange")) +
  scale_color_manual(values = c("darkgreen", "skyblue", "darkred", "orange")) +
  labs(title = "",
       x = "\nPerceived Violence", 
       y = "Density\n") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.position = "right",
        legend.direction = "vertical",
        plot.margin = grid::unit(c(3, 3, 3, 3), "mm")) 

### non-political

df_man_non_pol <- read_csv(
  paste0(path_data,
         "manipulation_check_control_May 26, 2023_15.31.csv"))
df_man_non_pol <- dplyr::slice(df_man_non_pol, 3:nrow(df_man_non_pol))
df_man_non_pol <- df_man_non_pol[c("c_1_1", "c_2_1", "c_3_1", "c_4_1")]
df_man_non_pol <- df_man_non_pol[complete.cases(df_man_non_pol) == T, ]
names(df_man_non_pol) <- c("Healthy recipe I", "Healthy recipe II", 
                           "Healthy recipe III", "Healthy recipe IV")
df_man_non_pol <- df_man_non_pol %>%
  mutate_all(as.numeric) %>%
  gather(key = "Post", value = "score")

ggplot(data = df_man_non_pol, 
       aes(x =  score, group = Post, color = Post, fill = Post)) +
  geom_density(adjust = 1.5, alpha = 0.05, lwd = 1) +
  geom_vline(data = df_man_non_pol %>% dplyr::group_by(Post) %>% 
               dplyr::summarize(score = mean(score)), 
             aes(xintercept = score, color = Post),
             lwd = 1,
             linetype = "22") +
  scale_fill_manual(values = c("darkgreen", "skyblue", "darkred", "orange")) +
  scale_color_manual(values = c("darkgreen", "skyblue", "darkred", "orange")) +
  labs(title = "",
       x = "\nPerceived Violence", 
       y = "Density\n") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.position = "right",
        legend.direction = "vertical",
        plot.margin = grid::unit(c(3, 3, 3, 3), "mm")) 


###############################################################################
########################### balance table (table a2) ##########################
###############################################################################

round(prop.table(table(df_full$party_gen, 
                       df_full$group), margin = 2), 3) * 100

round(prop.table(table(df_full$general_ideo, 
                       df_full$group), margin = 2), 3) * 100

round(prop.table(table(df_full$gender, 
                       df_full$group), margin = 2), 3) * 100

round(prop.table(table(df_full$age_ord, 
                       df_full$group), margin = 2), 3) * 100

round(prop.table(table(df_full$race, 
                       df_full$group), margin = 2), 3) * 100

round(prop.table(table(df_full$latin, 
                       df_full$group), margin = 2), 3) * 100

round(prop.table(table(df_full$general_educ, 
                       df_full$group), margin = 2), 3) * 100

round(prop.table(table(df_full$general_income, 
                       df_full$group), margin = 2), 3) * 100

round(prop.table(table(df_full$party_social_1, 
                       df_full$group), margin = 2), 3) * 100

round(prop.table(table(df_full$party_social_2, 
                       df_full$group), margin = 2), 3) * 100

round(prop.table(table(df_full$party_social_3, 
                       df_full$group), margin = 2), 3) * 100

round(prop.table(table(df_full$party_social_4, 
                       df_full$group), margin = 2), 3) * 100

round(prop.table(table(df_full$party_strength, 
                       df_full$group), margin = 2), 3) * 100

round(prop.table(table(df_full$ta_1, 
                       df_full$group), margin = 2), 3) * 100

round(prop.table(table(df_full$ta_2, 
                       df_full$group), margin = 2), 3) * 100

round(prop.table(table(df_full$ta_3, 
                       df_full$group), margin = 2), 3) * 100

round(prop.table(table(df_full$ta_4, 
                       df_full$group), margin = 2), 3) * 100


###############################################################################
######################## randomization check (table a3) #######################
###############################################################################

ran_check <- multinom(group ~ party_gen + ideology +  
                 gender + age_ord + race + latin + 
                 education + income, data = df_full)
texreg(ran_check, stars = c(0.01, 0.05, 0.1), digits = 2, longtable = TRUE)


###############################################################################
########### descriptive tables and figures (table a4; figure a10-12) ##########
###############################################################################

### table a4

table(df_full$party_gen, 
      df_full$party_strength)
round(prop.table(table(df_full$party_gen, 
                       df_full$party_strength)) * 100, 1)

table(df_full$gender)
round(prop.table(table(df_full$gender)) * 100, 1)

table(df_full$race)
round(prop.table(table(df_full$race)) * 100, 1)

table(df_full$latin)
round(prop.table(table(df_full$latin)) * 100, 1)

### figure a10

ggplot(df_full, aes(x = support_1)) + 
  geom_histogram(color = "gray20", fill = "gray90", binwidth = 10) + 
  geom_vline(aes(xintercept = mean(support_4)), col = 'brown3', size= 0.2) +
  labs(
    x = "\nSupport for Political Violence 1", 
    y = "Frequency\n") + 
  annotate("text", 
           x = 40, 
           y = 600, 
           size = 7, 
           col = 'brown3',
           label = paste0('Mean: ', round(mean(df_full$support_1), 1))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
      text = element_text(size = 30),
      axis.text.x = element_text(size = 30),
      axis.text.y = element_text(size = 30),
      legend.position = "right",
      legend.direction = "vertical",
      plot.margin = grid::unit(c(3, 3, 3, 3), "mm")) 
ggsave(paste0(path_output, 'support_1.pdf'),
       dpi = 100,
       width = 12,
       height = 10,
       units = 'in')

ggplot(df_full, aes(x = support_2)) + 
  geom_histogram(color = "gray20", fill = "gray90", binwidth = 10) + 
  geom_vline(aes(xintercept = mean(support_4)), col = 'brown3', size= 0.2) +
  labs(
    x = "\nSupport for Political Violence 2", 
    y = "Frequency\n") + 
  annotate("text", 
           x = 40, 
           y = 600, 
           size = 7, 
           col = 'brown3',
           label = paste0('Mean: ', round(mean(df_full$support_2), 1))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        legend.position = "right",
        legend.direction = "vertical",
        plot.margin = grid::unit(c(3, 3, 3, 3), "mm")) 
ggsave(paste0(path_output, 'support_2.pdf'),
       dpi = 100,
       width = 12,
       height = 10,
       units = 'in')

ggplot(df_full, aes(x = support_3)) + 
  geom_histogram(color = "gray20", fill = "gray90", binwidth = 10) + 
  geom_vline(aes(xintercept = mean(support_3)), col = 'brown3', size= 0.2) +
  labs(
    x = "\nSupport for Political Violence 3", 
    y = "Frequency\n") + 
  annotate("text", 
           x = 40, 
           y = 600, 
           size = 7, 
           col = 'brown3',
           label = paste0('Mean: ', round(mean(df_full$support_3), 1))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        legend.position = "right",
        legend.direction = "vertical",
        plot.margin = grid::unit(c(3, 3, 3, 3), "mm")) 
ggsave(paste0(path_output, 'support_3.pdf'),
       dpi = 100,
       width = 12,
       height = 10,
       units = 'in')

ggplot(df_full, aes(x = support_4)) + 
  geom_histogram(color = "gray20", fill = "gray90", binwidth = 10) + 
  geom_vline(aes(xintercept = mean(support_4)), col = 'brown3', size= 0.2) +
  labs(
    x = "\nSupport for Political Violence 4", 
    y = "Frequency\n") + 
  annotate("text", 
           x = 40, 
           y = 600, 
           size = 7, 
           col = 'brown3',
           label = paste0('Mean: ', round(mean(df_full$support_4), 1))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        legend.position = "right",
        legend.direction = "vertical",
        plot.margin = grid::unit(c(3, 3, 3, 3), "mm")) 
ggsave(paste0(path_output, 'support_4.pdf'),
       dpi = 100,
       width = 12,
       height = 10,
       units = 'in')

### figure a11

ggplot(df_full, aes(x = education)) + # education
  geom_histogram(color = "gray20", fill = "gray90", binwidth = 1) + 
  labs(
    x = "\nEducation", 
    y = "Frequency\n") + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        legend.position = "right",
        legend.direction = "vertical",
        plot.margin = grid::unit(c(3, 3, 3, 3), "mm")) 
ggsave(paste0(path_output, 'education.pdf'),
       dpi = 100,
       width = 12,
       height = 10,
       units = 'in')

ggplot(df_full, aes(x = income)) + # income
  geom_histogram(color = "gray20", fill = "gray90", binwidth = 1) + 
  labs(
    x = "\nIncome", 
    y = "Frequency\n") + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        legend.position = "right",
        legend.direction = "vertical",
        plot.margin = grid::unit(c(3, 3, 3, 3), "mm")) 
ggsave(paste0(path_output, 'income.pdf'),
       dpi = 100,
       width = 12,
       height = 10,
       units = 'in')

ggplot(df_full, aes(x = ideology)) + # ideology
  geom_histogram(color = "gray20", fill = "gray90", binwidth = 1) + 
  labs(
    x = "\nIdeology", 
    y = "Frequency\n") + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        legend.position = "right",
        legend.direction = "vertical",
        plot.margin = grid::unit(c(3, 3, 3, 3), "mm")) 
ggsave(paste0(path_output, 'ideology.pdf'),
       dpi = 100,
       width = 12,
       height = 10,
       units = 'in')

ggplot(df_full, aes(x = party_social_stan)) + # partisan social identity
  geom_histogram(color = "gray20", fill = "gray90", binwidth = 0.2) + 
  labs(
    x = "\nPartisan Social Identity (standardized / composite)", 
    y = "Frequency\n") + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        legend.position = "right",
        legend.direction = "vertical",
        plot.margin = grid::unit(c(3, 3, 3, 3), "mm")) 
ggsave(paste0(path_output, 'party_social.pdf'),
       dpi = 100,
       width = 12,
       height = 10,
       units = 'in')

ggplot(df_full, aes(x = ta)) + # trait aggression
  geom_histogram(color = "gray20", fill = "gray90", binwidth = 1) + 
  labs(
    x = "\nTrait Aggression (composite)", 
    y = "Frequency\n") + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        legend.position = "right",
        legend.direction = "vertical",
        plot.margin = grid::unit(c(3, 3, 3, 3), "mm")) 
ggsave(paste0(path_output, 'ta.pdf'),
       dpi = 100,
       width = 12,
       height = 10,
       units = 'in')

ggplot(df_full, aes(x = concern)) + # norm of non-violence
  geom_histogram(color = "gray20", fill = "gray90", binwidth = 10) + 
  labs(
    x = "\nNorm of Non-violence", 
    y = "Frequency\n") + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        legend.position = "right",
        legend.direction = "vertical",
        plot.margin = grid::unit(c(3, 3, 3, 3), "mm")) 
ggsave(paste0(path_output, 'concern.pdf'),
       dpi = 100,
       width = 12,
       height = 10,
       units = 'in')

### figure a12

emotions <- c("anger", "shame", "guilt", "disgust", "fear", "anxiety", "sadness", "happiness")

for (i in emotions){
  print(i)
  ggplot(df_full, aes_string(x = i)) + 
    geom_histogram(color = "gray20", fill = "gray90", bins = 20, binwidth = 1) +
    labs(
      x = "\nExtent of emotional experience", 
      y = "Frequency\n") +
    scale_x_continuous(breaks=c(0, 1, 2, 3, 4, 5, 6)) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(size = 30),
          axis.text.x = element_text(size = 30),
          axis.text.y = element_text(size = 30),
          legend.position = "right",
          legend.direction = "vertical",
          plot.margin = grid::unit(c(3, 3, 3, 3), "mm")) 
  ggsave(paste0(path_output, 'hist_', i, '.pdf'),
         dpi = 100,
         width = 8,
         height = 6,
         units = 'in')
  }


###############################################################################
################### two-way analysis of variance (table a5) ###################
###############################################################################

summary(aov(support ~ 
        treatment_violent * treatment_coparty, 
      data = df))


###############################################################################
############## partisanship and ideology (figure a13; table a12) ##############
###############################################################################

### correlation between partisanship and ideology

df_full <- df_full %>% dplyr::mutate(
  party_comb_num = case_when(
    party_comb == "Strong Democrat" ~ 1,
    party_comb == "Democrat" ~ 2,
    party_comb == "Strong Republican" ~ 3,
    party_comb == "Republican" ~ 4
    )
  )

cor(df_full$party_comb_num, # spearman's rho: 0.574
    df_full$ideology, 
    method = "spearman")

### cross-tabulation between partisanship and ideology

df_full <- df_full %>% dplyr::mutate( # combined partisanship variable
  party_comb = case_when(
    party_dem == "Strong" ~ "Strong Democrat",
    party_dem == "Not very strong" ~ "Democrat",
    party_rep == "Strong" ~ "Strong Republican",
    party_rep == "Not very strong" ~ "Republican"
    )
  )

df_party_ideo <- data.frame(prop.table(table(df_full$party_comb, 
                                             df_full$ideology)))
names(df_party_ideo) <- c("party", "ideo", "prop")

df_party_ideo$party <- factor(df_party_ideo$party, 
                              levels = c("Strong Democrat", 
                                         "Democrat", 
                                         "Republican", 
                                         "Strong Republican"))

df_dem <- filter(df_full, party_comb == "Strong Democrat" |
                            party_comb == "Democrat")
prop.table(table(df_dem$ideology)) # liberal: 0.6014551, moderate: 0.31689572, conservative: 0.08164915

df_rep <- filter(df_full, party_comb == "Strong Republican" |
                            party_comb == "Republican")
prop.table(table(df_rep$ideology)) # conservative: 0.68, moderate: 0.19720930, liberal: 0.1227907

### figure a13 (a)

ggplot(df_party_ideo, aes(x = party, y = ideo)) +
  geom_tile(aes(fill = prop)) +
  scale_fill_gradient(low = "white", 
                      high = "black",
                      name = "Proportion\n") +
  labs(x = "\nPartisanship",
       y = "Conservatism\n") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(angle= 0, vjust = 0.5, size = 30),
        axis.text.y = element_text(size = 30),
        strip.text = element_text(size = 30),
        legend.text = element_text(size = 30)) 
ggsave(paste0(path_output, 'heatmap_party_ideo.pdf'),
       dpi = 100,
       width = 16,
       height = 12,
       units = 'in')

### support for political violence by partisanship and ideology

df_full$conservatism <- df_full$ideology 
df_full$rep_chr <- ifelse(df_full$rep == 1, "Republican", "Democrat")

df_summary <- df_full %>%
  dplyr::group_by(rep_chr, conservatism) %>%
  dplyr::summarize(support_median = median(support, na.rm = TRUE),
                   support_mean = mean(support, na.rm = TRUE))

ggplot(df_summary, # figure a13 (b)
       aes(x = rep_chr, y = conservatism, fill = support_mean)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red", limits = c(0, 100)) +  # Choose a color gradient
  labs(title = "",
       x = "\nPartisanship",
       y = "Conservatism\n",
       fill = "Support for\nPolitical Violence\n") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(angle= 0, vjust = 0.5, size = 30),
        axis.text.y = element_text(size = 30),
        strip.text = element_text(size = 30),
        legend.text = element_text(size = 30)) 
ggsave(paste0(path_output, 'heatmap_party_ideo_violence_mean.pdf'),
       dpi = 100,
       width = 14,
       height = 12,
       units = 'in')

ggplot(df_summary, # figure a13 (c)
       aes(x = rep_chr, y = conservatism, fill = support_median)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red", limits = c(0, 100)) +  # Choose a color gradient
  labs(title = "",
       x = "\nPartisanship",
       y = "Conservatism\n",
       fill = "Support for\nPolitical Violence\n") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(angle= 0, vjust = 0.5, size = 30),
        axis.text.y = element_text(size = 30),
        strip.text = element_text(size = 30),
        legend.text = element_text(size = 30)) 
ggsave(paste0(path_output, 'heatmap_party_ideo_violence_median.pdf'),
       dpi = 100,
       width = 14,
       height = 12,
       units = 'in')

### model for interaction between partisanship and ideology

df$treatment_violent <- droplevels(as.factor(df$treatment_violent))
df$treatment_coparty <- droplevels(as.factor(df$treatment_coparty))

df$conservatism <- df$ideology
m_sort <- lm(support ~ 
               conservatism * rep + 
               gender + age_ord + race + latin + education + income +
               treatment_violent * treatment_coparty,
             data = df)

### figure a13 (d)

pred_m_sort <- predictions(
  m_sort, 
  newdata = datagrid(conservatism = seq(1:7),
                     rep = c(0, 1),
                     gender = "Men",
                     age_ord = mean(df$age_ord),
                     race = "White",
                     latin = "Non-Latin/Hispanic",
                     education = mean(df$education),
                     income = mean(df$income),
                     treatment_violent = 0,
                     treatment_violent = 0))

pred_m_sort$rep <- ifelse(pred_m_sort$rep == 1, "Rep", "Dem")
ggplot(pred_m_sort,
       aes(x = conservatism, 
           y = estimate, 
           color = rep)) + 
  geom_line(size = 1.5) +
  scale_color_manual(values = c('#156B90', '#9A3E25')) +
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high), 
              linetype = 0, 
              alpha = 0.1)  +
  labs(
    title = "",
    x = "\nConservatism",
    y = "Predicted Support for\nPolitical Violence\n",
    color = "",  
    shape = ""   
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(angle= 0, vjust = 0.5, size = 30),
        axis.text.y = element_text(size = 30),
        strip.text = element_text(size = 30),
        legend.text = element_text(size = 30)) 
ggsave(paste0(path_output, 'pred_party_ideo_violence.pdf'),
       dpi = 100,
       width = 14,
       height = 12,
       units = 'in')

### table a12

texreg(m_sort, 
       stars = c(0.01, 0.05, 0.1), 
       digits = 2)

texreg(m_sort, 
       stars = c(0.01, 0.05, 0.1), 
       digits = 3)


###############################################################################
################## linear interaction diagnostic (figure a14) #################
###############################################################################

### partisan strength 

df <- data.frame(df)
df$treatment_violent <- as.character(df$treatment_violent)

ols_cov_all_p1 <- lm(support ~ treatment_violent * party_social_stan + 
                       treatment_coparty + party_gen + ideology + gender + 
                       age_ord + race + latin + education + income,
                     data = df)
summary(ols_cov_all_p1) 

interflex(estimator = "raw",
          Y = "support", 
          D = "treatment_violent", 
          X = "party_social_stan", 
          data = df, 
          weights = NULL, 
          Ylabel = "Support for Political Violence\n", 
          Dlabel = "Violence Treatment", 
          Xlabel="\nParty Identity Strength", 
          main = "", 
          cex.main = 1, 
          ncols = 1,
          na.rm = TRUE)
ggsave(paste0(path_output, 'coef_hte_p1_lia_diag.pdf'),
       dpi = 100,
       width = 4,
       height = 5,
       units = 'in')

ols_cov_all_ta <- lm(support ~ treatment_violent * ta +
                       treatment_coparty + party_gen + ideology + gender + 
                       age_ord + race + latin + education + income,
                     data = df)
summary(ols_cov_all_ta)

interplot(m = ols_cov_all_ta, 
          var1 = "treatment_violent1", 
          var2 = "ta", hist = TRUE) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  labs(x = "\nTrait Aggression", 
       y = "Coefficient on Violence Treatment\n") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(angle= 0, vjust = 0.5, size = 15),
        axis.text.y = element_text(size = 15),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15)) 

interflex(estimator = "raw",
          Y = "support", 
          D = "treatment_violent", 
          X = "ta", 
          data = df, 
          weights = NULL, 
          Ylabel = "Support for Political Violence\n", 
          Dlabel = "Violence Treatment", 
          Xlabel="\nTrait Aggression", 
          main = "", 
          cex.main = 1, 
          ncols = 1,
          na.rm = TRUE)
ggsave(paste0(path_output, 'coef_hte_ta_lia_diag.pdf'),
       dpi = 100,
       width = 4,
       height = 5,
       units = 'in')

### concern

ols_cov_all_concern <- lm(support ~ treatment_violent * concern +
                            treatment_coparty + party_gen + ideology + gender + 
                            age_ord + race + latin + education + income,
                          data = df)
summary(ols_cov_all_concern)

interplot(m = ols_cov_all_concern, 
          var1 = "treatment_violent1", 
          var2 = "concern", hist = TRUE) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  labs(x = "\nNorm of non-violence", 
       y = "Coefficient on Violence Treatment\n") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(angle= 0, vjust = 0.5, size = 15),
        axis.text.y = element_text(size = 15),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15)) 

interflex(estimator = "raw",
          Y = "support", 
          D = "treatment_violent", 
          X = "concern", 
          data = df, 
          weights = NULL, 
          Ylabel = "Support for Political Violence\n", 
          Dlabel = "Violence Treatment", 
          Xlabel="\nNorm of Non-violence", 
          main = "", 
          cex.main = 1, 
          ncols = 1,
          na.rm = TRUE)
ggsave(paste0(path_output, 'coef_hte_concern_lia_diag.pdf'),
       dpi = 100,
       width = 4,
       height = 5,
       units = 'in')