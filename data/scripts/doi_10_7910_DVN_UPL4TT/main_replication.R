######################################################################
                                                                  
##      PROJECT TITLE:    The MIDAS Touch: Accurate and Scalable Missing-Data Imputation with Deep Learning                                        
##      PROJECT AUTHOR:   RANJIT LALL AND THOMAS S. ROBINSON                        
##      EMAIL:            R.LALL@LSE.AC.UK         
                                                                  
##      DESCRIPTION:      MAIN REPLICATION FILE FOR PA PAPER                                          
                                                                  
######################################################################

#### Package Dependencies ####
library(tidyverse)
library(xtable)

#### Figure 1 ####
## MIDAS Neural Network Architecture
## Generated manually using illustration software

#### Figure 2 ####
## Schematic of MIDAS Training Steps
## Generated manually using illustration software

#### Figure 3 ####
## Estimated Posterior Densities in MAR-1 Simulation Experiment

rbind(read_csv("mar1/mar1_r_results.csv"),
      read_csv("mar1/mar1_midas_results.csv")) %>% 
  gather("coefficient","value", b0,b1,b2) %>%
  mutate(type = ifelse(type == "amelia", "Amelia", type),
         type = ifelse(type == "full", "Complete Data", type),
         type = ifelse(type == "Listwise", "LWD", type),
         type = ifelse(type == "midas", "MIDAS", type)) %>% 
  
  mutate(type = factor(type, levels = c("MIDAS","Amelia","LWD","Complete Data")),
         
         coefficient  = ifelse(coefficient == "b0","beta[0]",coefficient),
         coefficient  = ifelse(coefficient == "b1","beta[1]",coefficient),
         coefficient  = ifelse(coefficient == "b2","beta[2]",coefficient)) %>% 
  
  ggplot(aes(x = value, linetype = type, color = type)) +
  facet_wrap(~ coefficient, labeller=label_parsed) +
  geom_density(outline.type = "full") +
  scale_linetype_manual(values = c("dotted","dashed","dotdash","solid")) +
  scale_color_manual(values = c("darkmagenta","dodgerblue2","firebrick2","black")) +
  labs(x = "", y="Density", linetype = "", color = "") +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "white")) +
  ggsave("figures/figure_3.pdf", width = 8, height = 3, dpi=300)

#### Figure 4 #### 
## Inverse Imputation and Linear Model Accuracy in Kropko et al. Simulation Test

kropko_lowcorr <- read_csv("kropko/lowcorr_results.csv") %>% 
  mutate(correlation = "Moderate")
kropko_highcorr <- read_csv("kropko/highcorr_results.csv") %>% 
  mutate(correlation = "Strong")

kropko_df <- rbind(kropko_lowcorr, kropko_highcorr) %>% 
  dplyr::select(method, mv.match, coef2.mns, fit2.all, fit2.av, correlation) %>% 
  mutate(coef2.mns = log(coef2.mns)) %>% 
  group_by(method, correlation) %>% 
  summarise_at(c("mv.match", "coef2.mns", "fit2.all", "fit2.av"),mean, na.rm=TRUE) %>% 
  gather("statistic","value", -method, -correlation) %>% 
  ungroup() %>% 
  mutate(method = gsub("Complete Cases","LWD", method),
         method = gsub("MCAR","Marginals", method),
         method = gsub("Midas","MIDAS", method),
         method = gsub("MI: MNL","mi", method),
         method = gsub("NORM","norm", method),
         method = factor(method, levels = c("Amelia","LWD","Marginals","mi","MIDAS","norm")),
         
         statistic = gsub("mv.match","Imputation RMSE", statistic),
         statistic = gsub("coef2.mns","Coefficient Mahalanobis Dist.",statistic),
         statistic = gsub("fit2.all","Fitted values RMSE",statistic),
         statistic = gsub("fit2.av","Fitted values RMSE, complete rows",statistic),
         
         statistic = factor(statistic, levels = c("Imputation RMSE", 
                                                  "Coefficient Mahalanobis Dist.", 
                                                  "Fitted values RMSE", 
                                                  "Fitted values RMSE, complete rows")),
         
         row = ifelse(grepl("Fitted",statistic),"bottom","top")) %>% 
  filter(!is.nan(value))

ggplot(kropko_df, 
       aes(x = method, y =  value, fill = method, alpha = correlation)) +
  facet_wrap(~statistic, ncol=2, scales = "free_y") +
  geom_col(position = "dodge", color = "white") +
  # geom_point(data=df_tmp, aes(x = method, y = value), alpha = 0) +
  labs(x = "", y = "", alpha = "Intercorrelations: ") +
  # scale_fill_manual(values = c("lightmagenta","darkmagenta")) +
  scale_fill_manual(values = c("dodgerblue2","firebrick2","darkseagreen3","goldenrod","darkmagenta","sienna2")) +
  scale_alpha_manual(values = c(0.5,1)) +
  guides(fill = FALSE,
         alpha = guide_legend(override.aes=list(fill='darkmagenta'))) +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 18),
        axis.text=element_text(size=18,
                               color = "black"),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18)) +
  ggsave(filename = "figures/figure_4.pdf", dpi = 300, width = 13, height = 9)

#### Figure 5 ####
## Results of Applied Imputation Accuracy Test

adult_df <- read_csv("adult/adult_test_results.csv") %>% 
  dplyr::select(-iteration, -coef2.mns, -coefficients) %>% 
  mutate(method = gsub("amelia","Amelia", method),
         method = gsub("lwd","LWD", method),
         method = gsub("mcar","Marginals", method),
         method = gsub("midas","MIDAS", method),
         method = gsub("mnl","mi", method),
         method = factor(method, levels = c("Amelia","LWD","Marginals","mi","MIDAS","norm")),
         type = str_to_upper(type),
         time_elapsed =  log(time_elapsed)) %>% 
  group_by(miss_prop, type, method) %>% 
  summarise_all(mean, na.rm = T) %>% 
  gather(key = "test",  value = "result", -miss_prop, - type, -method) %>%
  mutate(test = gsub("cat.acc","Cat. Classification Error", test),
         test = gsub("cont.acc","Cont. RMSE",test),
         test = gsub("fit.all","Fit (RMSE), all rows",test),
         test = gsub("fit.av","Fit (RMSE), complete rows",test),
         test = gsub("time_elapsed","Imputation Time (log)",test))

adult_df %>% 
  filter(test %in% c("Cat. Classification Error","Cont. RMSE"),
         method != "LWD") %>% 
  mutate(test = factor(test, levels = c("Cont. RMSE", "Cat. Classification Error")),
         method = factor(method, levels = c("Amelia","Marginals","MIDAS","mi","norm"))) %>% 
  
  ggplot(aes(x = miss_prop, y = result, shape = method, linetype = method, color = method)) +
  facet_grid(test ~ type, scales = "free") +
  scale_shape_manual(values = c(4,2,8,6,3)) +
  scale_linetype_manual(values = c("dashed","longdash","dotted","dotdash","twodash")) +
  scale_colour_manual(values = c("dodgerblue2","darkseagreen3","darkmagenta","goldenrod","sienna2")) +
  geom_point(size = 3) +
  geom_line() +
  labs(x = "Proportion of missing columns",
       y = "",
       shape = "",
       linetype = "",
       color = "") +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        strip.text.x = element_text(size = 18),
        strip.text.y = element_text(size = 18),
        axis.text=element_text(size=18),
        axis.title=element_text(size = 18),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        legend.key = element_rect(fill = "white")) +
  ggsave("figures/figure_5.pdf", dpi = 300, width = 13, height = 9)

#### Figure 6 ####
## Results of Column-Wise Scalability Test

set.seed(89)
col_results <- read.csv("speed/speed_col_results.csv", stringsAsFactors = FALSE)

reg_data <- as.data.frame(col_results, stringsAsFactors = FALSE) %>%
  mutate(time = as.numeric(time),
         columns = as.numeric(columns))

# mnl_reg <- lm("time ~ poly(columns,1)", data = reg_data[reg_data$method == "mnl",])
mcar_reg <- lm("time ~ poly(columns,1)", data = reg_data[reg_data$method == "mcar",])
midas_reg <- lm("time ~ poly(columns,1)", data = reg_data[reg_data$method == "midas",])
norm_reg <- lm("time ~ poly(columns,2)", data = reg_data[reg_data$method == "norm",])
amelia_reg <- lm("time ~ poly(columns,2)", data = reg_data[reg_data$method == "amelia",])

pred_cols <- data.frame(columns = c(1:400),
                        method = "new",
                        iteration = 11)

# Extrapolate from predictions

# 2000 columns

predict(amelia_reg, newdata = data.frame(columns = 2000,
                                         method = "test",
                                         iteration = 12))/60

predict(midas_reg, newdata = data.frame(columns = 2000,
                                        method = "test",
                                        iteration = 12))/60

# mnl_pred <- predict(mnl_reg, newdata = pred_cols, se.fit = TRUE)
mcar_pred <- predict(mcar_reg, newdata = pred_cols, se.fit = TRUE)
midas_pred <- predict(midas_reg, newdata = pred_cols, se.fit = TRUE)
norm_pred <- predict(norm_reg, newdata = pred_cols, se.fit = TRUE)
amelia_pred <- predict(amelia_reg, newdata = pred_cols, se.fit = TRUE)

speed_regs <- data.frame(columns = rep(1:400,4),
                         method = rep(c("mcar","midas","norm","amelia"), each = 400),
                         time = c(mcar_pred$fit,midas_pred$fit,norm_pred$fit,amelia_pred$fit),
                         se = c(mcar_pred$se.fit, midas_pred$se.fit, norm_pred$se.fit, amelia_pred$se.fit)) %>%
  mutate(time = time/60,
         se = se/60,
         method = gsub("amelia","Amelia", method),
         method = gsub("mcar","Marginals", method),
         method = gsub("midas","MIDAS", method),
         method = gsub("mnl","mi", method),
         # method = gsub("norm","norm", method),
         method = factor(method, levels = c("Amelia","Marginals","MIDAS","mi","norm"))) %>% 
  filter(method != "Marginals")

point_data <- as.data.frame(col_results, stringsAsFactors = FALSE) %>%
  mutate(time = as.numeric(time)/60,
         columns = as.numeric(columns),
         method = gsub("amelia","Amelia", method),
         method = gsub("mcar","Marginals", method),
         method = gsub("midas","MIDAS", method),
         method = gsub("mnl","mi", method),
         # method = gsub("norm","norm", method),
         method = factor(method, levels = c("Amelia","Marginals","MIDAS","mi","norm")))

ggplot(speed_regs, aes(x = columns, y = time, linetype = method, shape = method, color = method)) +
  geom_line() +
  geom_point(data = point_data, aes(x = columns, y = time), size = 2) +
  xlim(30,400) +
  ylim(-5,300) +
  theme_minimal() +
  labs(y = "Time (minutes)", x = "Effective no. of columns", shape = "", linetype = "", color = "") +
  scale_shape_manual(values = c(4,2,6,8,3)) +
  scale_linetype_manual(values = c("dashed","longdash","dotdash","dotted","twodash")) +
  scale_colour_manual(values = c("dodgerblue2","darkseagreen3","goldenrod","darkmagenta","sienna2")) +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        strip.text.x = element_text(size = 18),
        strip.text.y = element_text(size = 18),
        axis.text=element_text(size=18),
        axis.title=element_text(size = 18),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18))

ggsave(filename = "figures/figure_6.pdf", dpi = 300, width = 13, height = 9)

#### Figure 7 ####
## Results of Row-Wise Scalability Test

row_results <- read.csv("speed/speed_row_results.csv", stringsAsFactors = FALSE)

row_data <- as.data.frame(row_results, stringsAsFactors = FALSE) %>%
  mutate(time = as.numeric(time)/60,
         rows = as.numeric(rows),
         method = gsub("amelia","Amelia", method),
         method = gsub("mcar","Marginals", method),
         method = gsub("midas","MIDAS", method),
         method = gsub("mnl","mi", method),
         
         method = factor(method, levels = c("Amelia","Marginals","MIDAS","mi","norm")))

midas_row_reg <- lm("time ~ poly(rows,1)", data = row_data[row_data$method == "MIDAS",])
norm_row_reg <- lm("time ~ poly(rows,1)", data = row_data[row_data$method == "norm",])
amelia_row_reg <- lm("time ~ poly(rows,1)", data = row_data[row_data$method == "Amelia",])

pred_vals <- seq(5000,500000, 5000)

pred_rows <- data.frame(rows = pred_vals,
                        method = "new",
                        iteration = 11)

midas_row_pred <- predict(midas_row_reg, newdata = pred_rows, se.fit = TRUE)
norm_row_pred <- predict(norm_row_reg, newdata = pred_rows, se.fit = TRUE)
amelia_row_pred <- predict(amelia_row_reg, newdata = pred_rows, se.fit = TRUE)

row_regs <- data.frame(rows = rep(pred_vals,3),
                       method = rep(c("MIDAS","norm","Amelia"), each = length(pred_vals)),
                       time = c(midas_row_pred$fit,norm_row_pred$fit,amelia_row_pred$fit),
                       se = c(midas_row_pred$se.fit, norm_row_pred$se.fit, amelia_row_pred$se.fit)) %>% 
  mutate(method = factor(method, levels = c("Amelia","Marginals","MIDAS","mi","norm")))

ggplot(row_data, aes(x = rows, y  = time, shape = method, linetype = method, color = method)) +
  geom_point(size = 3) +
  
  geom_line(data = row_regs) +
  labs(y = "Time (minutes)", x = "Number of Rows", shape = "", linetype = "", color = "") +
  scale_colour_manual(values = c("dodgerblue2","darkseagreen3","darkmagenta","goldenrod","sienna2")) +
  scale_shape_manual(values = c(4,2,8,6,3)) +
  scale_linetype_manual(values = c("dashed","longdash","dotted","dotdash","twodash")) +
  scale_x_continuous(
    breaks = c(5000, 50000, 100000, 250000,500000),
    label = c("5k", "50k","100k","250k","500k")
  ) +
  
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        strip.text.x = element_text(size = 18),
        strip.text.y = element_text(size = 18),
        axis.text=element_text(size=14),
        axis.title=element_text(size = 18),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18))

ggsave(filename = "figures/figure_7.pdf", dpi = 300, width = 13, height = 9)

#### Figure 8 ####
## Regression-Based Estimates of CCES Respondents' Latent Ideology

cces <- read.csv("data/cces_format.csv")
M = 15 

# Ideology regression equation
reg_f <- formula(CC18_334A ~ CC18_414A + CC18_414B + CC18_414C + CC18_414D + CC18_414E +
                   CC18_324a + CC18_324b + CC18_324c + CC18_324d +
                   CC18_415a + CC18_415b + CC18_415c + CC18_415d + 
                   CC18_416 +
                   CC18_426_1 + CC18_426_2 + CC18_426_3 + CC18_426_4 + CC18_426_5)

# Fitted values for non-impted data
non_imp <- lm(reg_f, data = cces)$fitted.values

# Read in MIDAS imputations
mid_imps <- list()
for (m in 1:M) {
  mid_imps[[m]] <- read.csv(paste0("application/data_tmp/cces_mid_",m,".csv"))
}

# Get fitted values across 15 imputations and average
mid_regs <- list()
for (m in 1:M) {
  data_tmp <- mid_imps[[m]]
  mid_regs[[m]] <- lm(reg_f, data = data_tmp)$fitted.values
  
  if (m == 1) {
    mid_mean <- mid_regs[[m]]
    mid_app <- data_tmp$CC18_app_dtrmp_post
  } else {
    mid_mean <- mid_mean + mid_regs[[m]]
    mid_app <- mid_app + data_tmp$CC18_app_dtrmp_post
  }
}

imp <- mid_mean/M
mid_app_mean <- mid_app/M

ks.test(imp, non_imp)

ideo_df <- data.frame(value = c(non_imp, imp),
                      type = c(rep("LWD", length(non_imp)), 
                               rep("MIDAS", length(imp)))) %>% 
  mutate(type = factor(type, levels = c("LWD",
                                        "MIDAS")))

ggplot(ideo_df, aes(x = value, linetype = type, color = type)) +
  geom_density(alpha = 0.5, outline.type = "full") +
  xlim(0,8) +
  scale_color_manual(values = c("black","darkmagenta")) +
  scale_linetype_manual(values = c("solid","dashed")) +
  labs(x = "Estimated Ideology", y = "Density", color = "", linetype = "") +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),  
        panel.background = element_blank())

ggsave("figures/figure_8.pdf", device = "pdf", width = 7, height = 5)

#### Table 1 ####
## Regression of Presidential Job Approval on Different Measures of Ideology

cces_lm <- cces

mod_non_imp <- summary(lm(CC18_app_dtrmp_post ~ CC18_334A, data=cces_lm))

mod_imp <- summary(lm(mid_app_mean ~ imp))

mod_table <- data.frame(Model = c("Self-reported","Regression-based (MIDAS)"),
                        Beta = c(mod_non_imp$coefficients[2,1],
                                 mod_imp$coefficients[2,1]),
                        `Standard Error` = c(mod_non_imp$coefficients[2,2],
                                             mod_imp$coefficients[2,2]),
                        `Adjusted R-squared` = c(mod_non_imp$adj.r.squared,
                                                 mod_imp$adj.r.squared),
                        N = c(as.character(mod_non_imp$df[2]+2), 
                              as.character(mod_imp$df[2]+2)))

colnames(mod_table) <- c("\\textbf{Measure of Ideology}","$\\boldsymbol\\beta$",
                         "\\textbf{Std. Error}","\\textbf{Adj. }$\\boldsymbol{R^2}$",
                         "\\textbf{N}")

print(xtable(mod_table,
             align = "llcccc",
             caption = "Regression of Presidential Job Approval on Different Measures of Ideology",
             label = "tab:table_1",
             digits = 3),
      include.rownames = FALSE,
      sanitize.colnames.function = function (x) {x},
      table.placement = "t!",
      file = "tables/table_1.tex")

######################################################################
########### APPENDIX ###########

#### Figure A1 ####

library(MASS)
library(Amelia)
library(mice)
library(ggpubr)
library(reshape2)

set.seed(89)

B <- 100
Ms <- 10 # no. of imputed datasets

#### Import MIDAS imputed datasets and run analysis ####

## MIDAS analysis

tmpl_results <- data.frame(boot = (1:B),
                           type = NA,
                           b0 = NA,
                           b1 = NA,
                           b2 = NA,
                           b0_se = NA,
                           b1_se = NA,
                           b2_se = NA)

midas_results <- tmpl_results
am_results <- tmpl_results
lwd_results <- tmpl_results

for (b in 1:B) {
  
  mid_mods <- list()
  # Loop over imputed datasets and recover coefficient & RMSE estimates
  for (m in 1:Ms) {
    mid_data <- paste0("mar1/data_tmp/midas_",b,"_",m,".csv")
    mid_mods[[m]] <- lm(Y ~ X1 + X2, data = read_csv(mid_data))
  }
  
  pooled_mod <- summary(mice::pool(mid_mods))
  
  mod_b0 <- pooled_mod$estimate[1]
  mod_b1 <- pooled_mod$estimate[2]
  mod_b2 <- pooled_mod$estimate[3]
  
  mod_b0_se <- pooled_mod$std.error[1]
  mod_b1_se <- pooled_mod$std.error[2]
  mod_b2_se <- pooled_mod$std.error[3]
  
  midas_results[b,2] <- "MIDAS"
  midas_results[b,3:8] <- c(mod_b0,
                            mod_b1,
                            mod_b2,
                            mod_b0_se,
                            mod_b1_se,
                            mod_b2_se)
  
  miss <- read_csv(paste0("mar1/data_tmp/mar1_draw_",b,".csv"))
  
  ## Listwise analysis
  lw_data <- miss[,c("Y","X1","X2")] %>% 
    filter(complete.cases(.))
  
  lw_lm <- lm(Y ~ X1 + X2, data = lw_data)
  lw_coefs <- lw_lm$coefficients
  lw_ses <- summary(lw_lm)$coefficients[,2]
  
  lwd_mod_b0 <- lw_coefs[1]
  lwd_mod_b1 <- lw_coefs[2]
  lwd_mod_b2 <- lw_coefs[3]
  
  lwd_mod_b0_se <- lw_ses[1]
  lwd_mod_b1_se <- lw_ses[2]
  lwd_mod_b2_se <- lw_ses[3]
  
  lwd_results[b,2] <- "LWD"
  lwd_results[b,3:8] <- c(lwd_mod_b0,
                          lwd_mod_b1,
                          lwd_mod_b2,
                          lwd_mod_b0_se,
                          lwd_mod_b1_se,
                          lwd_mod_b2_se)
  
  ## Amelia
  # Generate imputed datasets
  am_imputes <- amelia(as.data.frame(miss), m=Ms)$imputations
  am_mods <- list()
  for (m in 1:Ms) {
    am_data <- am_imputes[[m]]
    am_mods[[m]] <- lm(Y ~ X1 + X2, data = am_data)
  }
  
  am_pooled_mod <- summary(mice::pool(am_mods))
  
  am_mod_b0 <- am_pooled_mod$estimate[1]
  am_mod_b1 <- am_pooled_mod$estimate[2]
  am_mod_b2 <- am_pooled_mod$estimate[3]
  
  am_mod_b0_se <- am_pooled_mod$std.error[1]
  am_mod_b1_se <- am_pooled_mod$std.error[2]
  am_mod_b2_se <- am_pooled_mod$std.error[3]
  
  am_results[b,2] <- "Amelia"
  am_results[b,3:8] <- c(am_mod_b0,
                         am_mod_b1,
                         am_mod_b2,
                         am_mod_b0_se,
                         am_mod_b1_se,
                         am_mod_b2_se)
  
  
}

#### Fixed covariance matrix
# sigma_mat <- matrix(c(1, -.12, -.1, .5, .1,
#                       -.12, 1, .1, -.6, .1,
#                       -.1, .1, 1, -.5, .1,
#                       .5, -.6, -.5, 1, .1,
#                       .1, .1, .1, .1, 1),
#                     ncol = 5)
# B <- 100000 # no. of simulations
# b0 <- 0
# b1 <- 0
# b2 <- 0
# b0_se <- 0
# b1_se <- 0
# b2_se <- 0
# 
# for (b in 1:B) {
#   D <- mvrnorm(n = 500, mu= c(0,0,0,0,0), Sigma = sigma_mat) %>%
#     as.data.frame(.)
#   colnames(D) <- c("Y","X1","X2","X3","X4")
#   D_mod <- lm(Y ~ X1 + X2, data = D)
#   mvn_draw_coefs <- D_mod$coefficients
#   b0 <- b0 + mvn_draw_coefs[1]
#   b1 <- b1 + mvn_draw_coefs[2]
#   b2 <- b2 + mvn_draw_coefs[3]
#   
#   b0_se <- b0_se + summary(D_mod)$coefficients[1,2]
#   b1_se <- b1_se + summary(D_mod)$coefficients[2,2]
#   b2_se <- b2_se + summary(D_mod)$coefficients[3,2]
# }
# 
# b0 <- b0/B
# b1 <- b1/B
# b2 <- b2/B
# 
# b0_se <- b0_se/B
# b1_se <- b1_se/B
# b2_se <- b2_se/B

b0 <- 0.0001679964
b1 <- -0.1111758
b2 <- -0.08893342

b0_se <- 0.0442847
b1_se <- 0.04457091
b2_se <- 0.04457069

results <- rbind(midas_results, am_results, lwd_results)

sig_level <- 1.96

results$b0_covered <- (b0 >= (results$b0 - sig_level*results$b0_se)) & 
  (b0 <= (results$b0 + sig_level*results$b0_se))
results$b1_covered <- (b1 >= (results$b1 - sig_level*results$b1_se)) & 
  (b1 <= (results$b1 + sig_level*results$b1_se))
results$b2_covered <- (b2 >= (results$b2 - sig_level*results$b2_se)) & 
  (b2 <= (results$b2 + sig_level*results$b2_se))

## Coverage

results %>% group_by(type) %>% 
  summarise(b0_cov = mean(b0_covered),
            b1_cov = mean(b1_covered),
            b2_cov = mean(b2_covered))

## Graph

b0_graph <- results %>% 
  mutate(type = factor(type, levels = c("MIDAS","Amelia","LWD"))) %>% 
  group_by(type) %>% 
  arrange(b0) %>% 
  mutate(draw = 1:100,
         fac_lab = "beta[0]") %>% 
  
  ggplot(., aes(x = draw, 
                ymin = b0 - 1.96*b0_se,
                ymax = b0 + 1.96*b0_se,
                color = type)) +
  # facet_wrap(~fac_lab, labeller = label_parsed) +
  geom_hline(yintercept = b0, linetype = "solid") +
  geom_ribbon(alpha =  0, aes(linetype = type), outline.type = "full") +
  scale_color_manual(values = c("darkmagenta","dodgerblue2","firebrick2")) +
  scale_linetype_manual(values = c("dotted","dashed","dotdash")) +
  labs(x = "", title = expression(beta[0]), linetype = "", color = "") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

b1_graph <- results %>% 
  mutate(type = factor(type, levels = c("MIDAS","Amelia","LWD"))) %>% 
  group_by(type) %>% 
  arrange(b1) %>% 
  mutate(draw = 1:100,
         fac_lab = "beta[1]") %>% 
  
  ggplot(., aes(x = draw, 
                ymin = b1 - 1.96*b1_se,
                ymax = b1 + 1.96*b1_se,
                color = type)) +
  # facet_wrap(~fac_lab, labeller = label_parsed) +
  geom_hline(yintercept = b1, linetype = "solid") +
  geom_ribbon(alpha =  0, aes(linetype = type), outline.type = "full") +
  scale_color_manual(values = c("darkmagenta","dodgerblue2","firebrick2")) +
  scale_linetype_manual(values = c("dotted","dashed","dotdash")) +
  labs(x = "", title = expression(beta[1]), linetype = "", color = "") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

b2_graph <- results %>% 
  mutate(type = factor(type, levels = c("MIDAS","Amelia","LWD"))) %>% 
  group_by(type) %>% 
  arrange(b2) %>% 
  mutate(draw = 1:100,
         fac_lab = "beta[2]") %>% 
  
  ggplot(., aes(x = draw, 
                ymin = b2 - 1.96*b2_se,
                ymax = b2 + 1.96*b2_se,
                color = type)) +
  geom_hline(yintercept = b2, linetype = "solid") +
  geom_ribbon(alpha =  0, aes(linetype = type), outline.type = "full") +
  scale_color_manual(values = c("darkmagenta","dodgerblue2","firebrick2")) +
  scale_linetype_manual(values = c("dotted","dashed","dotdash")) +
  labs(x = "Trial of MAR-1 Experiment", title = expression(beta[2]), linetype = "", color = "") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

coverage_figure <- ggarrange(b0_graph, b1_graph, b2_graph,
                             common.legend = TRUE,
                             legend = "bottom",
                             ncol = 1, nrow = 3)

ggsave(coverage_figure, filename = "figures/figure_a1.pdf", dpi = 300)

#### Table A1 ####
## Summary Statistics for Adult Dataset
## Long text table -- manually generated

#### Table A2 ####
## Missingness Treatments Applied to Adult Dataset
## Long text table -- manually generated

#### Figure A2 ####
## Inverse Imputation Accuracy in Kropko et al. Applied ANES Test

anes_res <- read_csv("kropko/kropko_anes_results.csv") %>% 
  group_by(method) %>% 
  summarise_all(mean) %>% 
  pivot_longer(-method) %>% 
  filter(name != "iteration") %>% 
  mutate(name = gsub("religion","religion: ", name),
         name = gsub("vote","vote: ", name),
         method = gsub("amelia","Amelia", method),
         method = gsub("mcar","Marginals", method),
         method = gsub("midas","MIDAS", method),
         
         method = factor(method, levels = c("Amelia","Marginals","mi","MIDAS","norm")))

ggplot(anes_res, aes(y = value, x = method, fill = method)) +
  facet_wrap(~name, scales = "free",
             ncol = 3) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(values = c("dodgerblue2","darkseagreen3","goldenrod","darkmagenta","sienna2")) +
  labs(x = "", y = "RMSE", fill = "") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text=element_text(size=13,
                               color = "black")) +
  ggsave("figures/figure_a2.pdf",
         width = 13, height = 9)

#### Table A3 ####
## List of CCES Policy Variables Included in Latent Ideology Estimation
## Long text table -- manually generated

policy_vars <- c("CC18_414A","CC18_414B","CC18_414C","CC18_414D","CC18_414E",
                 "CC18_324a","CC18_324b","CC18_324c","CC18_324d",
                 "CC18_415a","CC18_415b","CC18_415c","CC18_415d",
                 "CC18_416","CC18_426_1","CC18_426_2","CC18_426_3","CC18_426_4","CC18_426_5")

policy_missing <- cces %>% dplyr::select(policy_vars) %>% is.na(.) %>% colSums(.)

policy_type <- c("Minimum Wage", "Millionaire's tax", "Sales tax", "Income tax","Abortion spending",
                 "Government Spending","Government Spending","Government Spending","Government Spending",
                 "Carbon Dioxide regulation", "Fuel efficiency regulation", "Renewable energy policy","EPA powers",
                 "Financial regulation","State welfare spending","State healthcare spending","State education spending",
                 "State law enforcement spending", "State transportation/infrastructure spending")

policy_answer <- c("For/Against","For/Against","For/Against","For/Against","For/Against",
                   "Support/Oppose","Support/Oppose","Support/Oppose","Support/Oppose",
                   "Support/Oppose","Support/Oppose","Support/Oppose","Support/Oppose","Support/Oppose",
                   rep("Increase/Decrease (1-5)",5))

ideo_desc <- data.frame(Variable = policy_vars,
                        `Policy Area` = policy_type,
                        `Response Type` = policy_answer,
                        Missing = policy_missing)

print(xtable(ideo_desc,
             caption = "List of CCES Policy Variables Included in Latent Ideology Estimation",
             label = "tab:cces_ideo_desc",
             align = c("l","p{0.16\\textwidth}","p{0.4\\textwidth}","p{0.3\\textwidth}","p{0.08\\textwidth}"),
             digits = 0),
      include.rownames = FALSE,
      sanitize.colnames.function = function (x) paste0("\\textbf{",x,"}"),
      table.placament = "t!",
      file = "tables/table_a3.tex")

#### Figure A3 ####
## Comparison of Latent Ideology Estimates from Different MI Strategies

amelia_3 <- amelia(cces[,c(policy_vars, "CC18_334A","CC18_app_dtrmp_post","gender","sexuality","race","employ","pid3")],
                   noms = c("gender","sexuality","race","employ","pid3"),
                   m = 15)

am_regs <- list()
for (m in 1:M) {
  data_tmp <- amelia_3$imputations[[m]]
  am_regs[[m]] <- lm(reg_f, data = data_tmp)$fitted.values
  
  if (m == 1) {
    am_mean <- am_regs[[m]]
    am_app <- data_tmp$CC18_app_dtrmp_post
  } else {
    am_mean <- am_mean + am_regs[[m]]
    am_app <- am_app + data_tmp$CC18_app_dtrmp_post
  }
}

am_imp <- am_mean/M
am_app_mean <- am_app/M

ideo_df_am <- data.frame(value = c(non_imp, imp, am_imp),
                         type = c(rep("LWD", length(non_imp)), 
                                  rep("MIDAS", length(imp)),
                                  rep("Amelia", length(am_imp)))) %>% 
  mutate(type = factor(type, levels = c("LWD",
                                        "MIDAS",
                                        "Amelia")))

ggplot(ideo_df_am, aes(x = value,linetype = type, color = type)) +
  geom_density(alpha = 0.5) +
  scale_linetype_manual(values = c("solid","dotted","dashed")) +
  scale_color_manual(values = c("black","darkmagenta","dodgerblue2")) +
  geom_hline(yintercept = 0) +
  xlim(0,8) +
  labs(x = "Estimated Ideology", y = "Density", fill = "", linetype = "", color = "") +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),  
        panel.background = element_blank())

ggsave("figures/figure_a3.pdf", device = "pdf", width = 7, height = 5)


ks.test(ideo_df_am$value[ideo_df_am$type == "Amelia"], ideo_df_am$value[ideo_df_am$type == "MIDAS"])
ks.test(ideo_df_am$value[ideo_df_am$type == "Amelia"], ideo_df_am$value[ideo_df_am$type == "LWD"])
ks.test(ideo_df_am$value[ideo_df_am$type == "MIDAS"], ideo_df_am$value[ideo_df_am$type == "LWD"])

#### Figure A4 ####

original <-  read.csv("data/raw_numbers.csv", check.names=F)
ccodes <- c("CIV", "CMR", "COG", "GHA", "NER", "ZMB")

for(i in 1:length(ccodes)){
  assign(paste(ccodes[i],"imp",sep="_"), read.csv(paste0("wdi/",ccodes[i],"_output.csv"), check.names=F))
  assign(paste(ccodes[i],"dat",sep="_"), merge(melt(data.frame(Year=names(get(paste(ccodes[i],"imp",sep="_"))), t(get(paste(ccodes[i],"imp",sep="_")))),id.vars="Year"),original[ which(original["Country Code"]==ccodes[i]), ],by="Year"))
}

pdf("figures/figure_a4.pdf", width=10, height=8)
m <- matrix(c(1,2,3,4,5,6,7,7,7),nrow = 3,ncol = 3,byrow = TRUE)
layout(mat = m,
       heights = c(0.47,0.47,.06))
par(oma=c(.2,.5,.2,1), 
    mai=c(.6,.51,.5,0))

boxplot(CIV_dat$value ~ CIV_dat$Year,
     main="Cote D'Ivoire", 
     xlab="Year", 
     ylab="GDP (constant 2010 US$)", 
     pch=21, col="blue", border="lightslateblue", ylim=c(0, 26000000000))
points(factor(CIV_dat$Year), CIV_dat$NY.GDP.MKTP.KD,
       pch=17, cex=.75, col="red")

boxplot(CMR_dat$value ~ CMR_dat$Year, 
     main="Cameroon", 
     xlab="Year", 
     ylab=NA, 
     pch=21, col="blue", border="lightslateblue", yaxt="n", ylim=c(0, 26000000000))
points(factor(CMR_dat$Year), CMR_dat$NY.GDP.MKTP.KD,
       pch=17,cex=.75,col="red")

boxplot(COG_dat$value ~ COG_dat$Year, 
     main="Congo Republic", 
     xlab="Year", 
     ylab=NA, 
     pch=21, col="blue", border="lightslateblue", yaxt="n", ylim=c(0, 26000000000))
points(factor(COG_dat$Year), COG_dat$NY.GDP.MKTP.KD,
       pch=17,cex=.75,col="red")

boxplot(GHA_dat$value ~ GHA_dat$Year,
     main="Ghana", 
     xlab="Year", 
     ylab="GDP (constant 2010 US$)", 
     pch=21, col="blue", border="lightslateblue", ylim=c(0, 26000000000))
points(factor(GHA_dat$Year), GHA_dat$NY.GDP.MKTP.KD,
       pch=17,cex=.75,col="red")

boxplot(NER_dat$value ~ NER_dat$Year, 
     main="Niger", 
     xlab="Year", 
     ylab=NA, 
     pch=21, col="blue", border="lightslateblue", yaxt="n", ylim=c(0, 26000000000))
points(factor(NER_dat$Year), NER_dat$NY.GDP.MKTP.KD,
       pch=17,cex=.75,col="red")

boxplot(ZMB_dat$value ~ ZMB_dat$Year, 
     main="Zambia", 
     xlab="Year", 
     ylab=NA, 
     pch=21, col="blue", border="lightslateblue", yaxt="n", ylim=c(0, 26000000000))
points(factor(ZMB_dat$Year), ZMB_dat$NY.GDP.MKTP.KD,
       pch=17,cex=.75,col="red")

par(mai=c(0,1,0,0))

plot.new()
legend(x="center", ncol=2,
       legend=c("MIDAS imputations","Real value"), 
       col=c("lightslateblue","red"),
       pch=c(15,17),lty=c(1,NA),bty="n",cex=1.35)
dev.off()

