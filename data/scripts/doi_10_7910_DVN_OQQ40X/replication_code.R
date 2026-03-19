# Load R packages
pacman::p_load(tidyverse, janitor, scales, lubridate, fixest, mice, miceadds, patchwork, Amelia, purrr)

# Create custom function to pool results from each data permutation
# https://stackoverflow.com/questions/65627475/correcting-for-robust-clustered-standard-errors-within-the-lm-function-or-replac
pool_models <- function(model_list){

  library(purrr)
  quiet_summary <- quietly(summary)
  
  betas <- map(model_list, coef) # pull out coefs
  vcovs <- map(model_list, vcov) # pull out vcovs
  parms <- pool_mi(qhat = betas, u = vcovs) # the magic happens
  
  out <- quiet_summary(parms) |> 
    pluck("result") |> 
    rownames_to_column("term") |> 
    as_tibble() 
  
  broom_col_names <- names(broom::tidy(lm(mpg ~ 1, mtcars), conf.int = TRUE)) # (dummy data)
  names(out)[1:length(broom_col_names)] <- broom_col_names
  
  # add N obs
  outN <- out |> mutate(N = nobs(model_list[[1]]))
  return(outN)
  
}

# Load data
data_imputed <- read_rds("imputed_dataset.rds")

# Create empty list for results
results <- list()

##---------------------------------------------------------------------
##---------------------------------------------------------------------
##                                                                   
##       ~       RUN MODELS                             
##                                                                   
##---------------------------------------------------------------------
##---------------------------------------------------------------------

# H1
results$h1_ccawa <- data_imputed |> 
  map(~fixest::feols(ccawa ~ age + age^2 + gender + education + income + lrscale + party + mip + polaware + pre_industry + pre_gender + pre_immigration + municipality_id + d_recall, data = ., vcov = "iid")) |> 
  pool_models() |> 
  mutate(h = "h1", outcome = "ccawa")

results$h1_ccopi <- data_imputed |> 
  map(~fixest::feols(ccopi ~ age + age^2 + gender + education + income + lrscale + party + mip + polaware + pre_industry + pre_gender + pre_immigration + municipality_id + d_recall, data = ., vcov = "iid")) |> 
  pool_models() |> 
  mutate(h = "h1", outcome = "ccopi")

results$h1_ccwta <- data_imputed |> 
  map(~fixest::feols(ccwta ~ age + age^2 + gender + education + income + lrscale + party + mip + polaware + pre_industry + pre_gender + pre_immigration + municipality_id + d_recall, data = ., vcov = "iid")) |> 
  pool_models() |> 
  mutate(h = "h1", outcome = "ccwta")

results$h1_ccwtp <- data_imputed |> 
  map(~fixest::feols(ccwtp ~ age + age^2 + gender + education + income + lrscale + party + mip + polaware + pre_industry + pre_gender + pre_immigration + municipality_id + d_recall, data = ., vcov = "iid")) |> 
  pool_models() |> 
  mutate(h = "h1", outcome = "ccwtp")

# h2a
results$h2a_ccawa <- data_imputed |> 
  map(~fixest::feols(ccawa ~ age + age^2 + gender + education + income + lrscale + party + mip + polaware + pre_industry + pre_gender + pre_immigration + municipality_id + d_info_control, data = ., vcov = "iid")) |>   
  pool_models() |> 
  mutate(h = "h2a", outcome = "ccawa")

results$h2a_ccopi <- data_imputed |> 
  map(~fixest::feols(ccopi ~ age + age^2 + gender + education + income + lrscale + party + mip + polaware + pre_industry + pre_gender + pre_immigration + municipality_id + d_info_control, data = ., vcov = "iid")) |> 
  pool_models() |> 
  mutate(h = "h2a", outcome = "ccopi") 

results$h2a_ccwta <- data_imputed |> 
  map(~fixest::feols(ccwta ~ age + age^2 + gender + education + income + lrscale + party + mip + polaware + pre_industry + pre_gender + pre_immigration + municipality_id + d_info_control, data = ., vcov = "iid")) |> 
  pool_models() |> 
  mutate(h = "h2a", outcome = "ccwta")

results$h2a_ccwtp <- data_imputed |> 
  map(~fixest::feols(ccwtp ~ age + age^2 + gender + education + income + lrscale + party + mip + polaware + pre_industry + pre_gender + pre_immigration + municipality_id + d_info_control, data = ., vcov = "iid")) |> 
  pool_models() |> 
  mutate(h = "h2a", outcome = "ccwtp")

# h2b
results$h2b_ccawa <- data_imputed |> 
  map(~fixest::feols(ccawa ~ age + age^2 + gender + education + income + lrscale + party + mip + polaware + pre_industry + pre_gender + pre_immigration + municipality_id + i(d_info3, ref = 0), data = ., vcov = "iid")) |> 
  pool_models() |> 
  mutate(h = "h2b", outcome = "ccawa")

results$h2b_ccopi <- data_imputed |> 
  map(~fixest::feols(ccopi ~ age + age^2 + gender + education + income + lrscale + party + mip + polaware + pre_industry + pre_gender + pre_immigration + municipality_id + i(d_info3, ref = 0), data = ., vcov = "iid")) |> 
  pool_models() |> 
  mutate(h = "h2b", outcome = "ccopi")

results$h2b_ccwta <- data_imputed |> 
  map(~fixest::feols(ccwta ~ age + age^2 + gender + education + income + lrscale + party + mip + polaware + pre_industry + pre_gender + pre_immigration + municipality_id + i(d_info3, ref = 0), data = ., vcov = "iid")) |> 
  pool_models() |> 
  mutate(h = "h2b", outcome = "ccwta")

results$h2b_ccwtp <- data_imputed |> 
  map(~fixest::feols(ccwtp ~ age + age^2 + gender + education + income + lrscale + party + mip + polaware + pre_industry + pre_gender + pre_immigration + municipality_id + i(d_info3, ref = 0), data = ., vcov = "iid")) |> 
  pool_models() |> 
  mutate(h = "h2b", outcome = "ccwtp")

# h2c
results$h2c_ccawa <- data_imputed |> 
  map(~fixest::feols(ccawa ~ age + age^2 + gender + education + income + lrscale + party + mip + polaware + pre_industry + pre_gender + pre_immigration + municipality_id + i(d_info3, ref = 0), data = ., vcov = "iid")) |> 
  pool_models() |> 
  mutate(h = "h2c", outcome = "ccawa")

results$h2c_ccopi <- data_imputed |> 
  map(~fixest::feols(ccopi ~ age + age^2 + gender + education + income + lrscale + party + mip + polaware + pre_industry + pre_gender + pre_immigration + municipality_id + i(d_info3, ref = 0), data = ., vcov = "iid")) |> 
  pool_models() |> 
  mutate(h = "h2c", outcome = "ccopi")

results$h2c_ccwta <- data_imputed |> 
  map(~fixest::feols(ccwta ~ age + age^2 + gender + education + income + lrscale + party + mip + polaware + pre_industry + pre_gender + pre_immigration + municipality_id + i(d_info3, ref = 0), data = ., vcov = "iid")) |> 
  pool_models() |> 
  mutate(h = "h2c", outcome = "ccwta")

results$h2c_ccwtp <- data_imputed |> 
  map(~fixest::feols(ccwtp ~ age + age^2 + gender + education + income + lrscale + party + mip + polaware + pre_industry + pre_gender + pre_immigration + municipality_id + i(d_info3, ref = 0), data = ., vcov = "iid")) |> 
  pool_models() |> 
  mutate(h = "h2c", outcome = "ccwtp")

# h2d
results$h2d_ccawa <- data_imputed |> 
  map(~fixest::feols(ccawa ~ age + age^2 + gender + education + income + lrscale + party + mip + polaware + pre_industry + pre_gender + pre_immigration + municipality_id + i(d_info3, ref = 1), data = ., vcov = "iid")) |> 
  pool_models() |> 
  mutate(h = "h2d", outcome = "ccawa")

results$h2d_ccopi <- data_imputed |> 
  map(~fixest::feols(ccopi ~ age + age^2 + gender + education + income + lrscale + party + mip + polaware + pre_industry + pre_gender + pre_immigration + municipality_id + i(d_info3, ref = 1), data = ., vcov = "iid")) |> 
  pool_models() |> 
  mutate(h = "h2d", outcome = "ccopi")

results$h2d_ccwta <- data_imputed |> 
  map(~fixest::feols(ccwta ~ age + age^2 + gender + education + income + lrscale + party + mip + polaware + pre_industry + pre_gender + pre_immigration + municipality_id + i(d_info3, ref = 1), data = ., vcov = "iid")) |> 
  pool_models() |> 
  mutate(h = "h2d", outcome = "ccwta")

results$h2d_ccwtp <- data_imputed |> 
  map(~fixest::feols(ccwtp ~ age + age^2 + gender + education + income + lrscale + party + mip + polaware + pre_industry + pre_gender + pre_immigration + municipality_id + i(d_info3, ref = 1), data = ., vcov = "iid")) |> 
  pool_models() |> 
  mutate(h = "h2d", outcome = "ccwtp")

# H3 - interactions
results$h3_ccawa <- data_imputed |> 
  map(~fixest::feols(ccawa ~ age + age^2 + gender + education + income + lrscale + party + mip + polaware + pre_industry + pre_gender + pre_immigration + municipality_id + d_recall * d_info_control, data = ., vcov = "iid")) |> 
  pool_models() |> 
  mutate(h = "h3", outcome = "ccawa")

results$h3_ccopi <- data_imputed |> 
  map(~fixest::feols(ccopi ~ age + age^2 + gender + education + income + lrscale + party + mip + polaware + pre_industry + pre_gender + pre_immigration + municipality_id + d_recall * d_info_control, data = ., vcov = "iid")) |> 
  pool_models() |> 
  mutate(h = "h3", outcome = "ccopi")

results$h3_ccwta <- data_imputed |> 
  map(~fixest::feols(ccwta ~ age + age^2 + gender + education + income + lrscale + party + mip + polaware + pre_industry + pre_gender + pre_immigration + municipality_id + d_recall * d_info_control, data = ., vcov = "iid")) |> 
  pool_models() |> 
  mutate(h = "h3", outcome = "ccwta")

results$h3_ccwtp <- data_imputed |> 
  map(~fixest::feols(ccwtp ~ age + age^2 + gender + education + income + lrscale + party + mip + polaware + pre_industry + pre_gender + pre_immigration + municipality_id + d_recall * d_info_control, data = ., vcov = "iid")) |> 
  pool_models() |> 
  mutate(h = "h3", outcome = "ccwtp")

# h3 - marginal effects
results$h3_ccawa_me <- data_imputed |> 
  map(~fixest::feols(ccawa ~ age + age^2 + gender + education + income + lrscale + party + mip + polaware + pre_industry + pre_gender + pre_immigration + municipality_id + d_info_control / d_recall, data = ., vcov = "iid")) |> 
  pool_models() |> 
  mutate(h = "h3_me", outcome = "ccawa") |> 
  filter(str_detect(term, ":"))

results$h3_ccopi_me <- data_imputed |> 
  map(~fixest::feols(ccopi ~ age + age^2 + gender + education + income + lrscale + party + mip + polaware + pre_industry + pre_gender + pre_immigration + municipality_id + d_info_control / d_recall, data = ., vcov = "iid")) |> 
  pool_models() |> 
  mutate(h = "h3_me", outcome = "ccopi") |> 
  filter(str_detect(term, ":"))

results$h3_ccwta_me <- data_imputed |> 
  map(~fixest::feols(ccwta ~ age + age^2 + gender + education + income + lrscale + party + mip + polaware + pre_industry + pre_gender + pre_immigration + municipality_id + d_info_control / d_recall, data = ., vcov = "iid")) |> 
  pool_models() |> 
  mutate(h = "h3_me", outcome = "ccwta") |> 
  filter(str_detect(term, ":"))

results$h3_ccwtp_me <- data_imputed |> 
  map(~fixest::feols(ccwtp ~ age + age^2 + gender + education + income + lrscale + party + mip + polaware + pre_industry + pre_gender + pre_immigration + municipality_id + d_info_control / d_recall, data = ., vcov = "iid")) |> 
  pool_models() |> 
  mutate(h = "h3_me", outcome = "ccwtp") |> 
  filter(str_detect(term, ":"))

# H4 interactions
results$h4_ccawa <- data_imputed |> 
  map(~fixest::feols(ccawa ~ age + age^2 + gender + education + income + municipality_id + d_info_control * temp_dev7, data = ., vcov = vcov_cluster(cluster = ~zipcode))) |> 
  pool_models() |> 
  mutate(h = "h4", outcome = "ccawa")

results$h4_ccopi <- data_imputed |> 
  map(~fixest::feols(ccopi ~ age + age^2 + gender + education + income + municipality_id + d_info_control * temp_dev7, data = ., vcov = vcov_cluster(cluster = ~zipcode))) |> 
  pool_models() |> 
  mutate(h = "h4", outcome = "ccopi")

results$h4_ccwta <- data_imputed |> 
  map(~fixest::feols(ccwta ~ age + age^2 + gender + education + income + municipality_id + d_info_control * temp_dev7, data = ., vcov = vcov_cluster(cluster = ~zipcode))) |> 
  pool_models() |> 
  mutate(h = "h4", outcome = "ccwta")

results$h4_ccwtp <- data_imputed |> 
  map(~fixest::feols(ccwtp ~ age + age^2 + gender + education + income + municipality_id + d_info_control * temp_dev7, data = ., vcov = vcov_cluster(cluster = ~zipcode))) |> 
  pool_models() |> 
  mutate(h = "h4", outcome = "ccwtp")

# H4 marginal effects
results$h4_ccawa_me <- data_imputed |> 
  map(~fixest::feols(ccawa ~ age + age^2 + gender + education + income + municipality_id + d_info_control / temp_dev7, data = ., vcov = vcov_cluster(cluster = ~zipcode))) |> 
  pool_models() |> 
  mutate(h = "h4_me", outcome = "ccawa") |> 
  filter(str_detect(term, ":"))

results$h4_ccopi_me <- data_imputed |> 
  map(~fixest::feols(ccopi ~ age + age^2 + gender + education + income + municipality_id + d_info_control / temp_dev7, data = ., vcov = vcov_cluster(cluster = ~zipcode))) |> 
  pool_models() |> 
  mutate(h = "h4_me", outcome = "ccopi") |> 
  filter(str_detect(term, ":"))

results$h4_ccwta_me <- data_imputed |> 
  map(~fixest::feols(ccwta ~ age + age^2 + gender + education + income + municipality_id + d_info_control / temp_dev7, data = ., vcov = vcov_cluster(cluster = ~zipcode))) |> 
  pool_models() |> 
  mutate(h = "h4_me", outcome = "ccwta") |> 
  filter(str_detect(term, ":"))

results$h4_ccwtp_me <- data_imputed |> 
  map(~fixest::feols(ccwtp ~ age + age^2 + gender + education + income + municipality_id + d_info_control / temp_dev7, data = ., vcov = vcov_cluster(cluster = ~zipcode))) |> 
  pool_models() |> 
  mutate(h = "h4_me", outcome = "ccwtp") |> 
  filter(str_detect(term, ":"))

##---------------------------------------------------------------------
##---------------------------------------------------------------------
##                                                                   
##       ~       COLLECT RESULTS                             
##                                                                   
##---------------------------------------------------------------------
##---------------------------------------------------------------------

# Bind results
results_df <- results |> 
  bind_rows() |> 
  filter(str_detect(term, "d_|temp_dev|:")) |> 
  mutate(conf.low90 = estimate - (std.error * 1.645),
         conf.high90 = estimate + (std.error * 1.645),
         # TMP
         conf.low95 = estimate - (std.error * 1.96),  # OBS! Different for WTP
         conf.high95 = estimate + (std.error * 1.96)) |> # OBS! Different for WTP
  # TMP
  select(h, outcome, term, estimate, std.error, statistic, p.value, starts_with("conf"), N) |> 
  mutate(outcome_label = case_when(outcome == "ccawa" ~ "Climate awareness",
                                   outcome == "ccopi" ~ "Climate opinions",
                                   outcome == "ccwta" ~ "Willingness-to-act",
                                   outcome == "ccwtp" ~ "Willingness-to-pay",
                                   TRUE ~ outcome),
         h_label = case_when(h == "h1" ~ "H1: Recall",
                             h == "h2a" ~ "A Pooled info vs control",
                             h == "h2b" ~ "B Climate info vs control",
                             h == "h2c" ~ "C Health info vs control",
                             h == "h2d" ~ "D Health info vs climate info",
                             h == "h3" ~ "H3: Recall x Info",
                             h == "h3_me" ~ "H3: Recall x Info",
                             h == "h4" ~ "H4: Temperature x Info",
                             h == "h4_me" ~ "H4: Temperature x Info"))

##---------------------------------------------------------------------
##---------------------------------------------------------------------
##                                                                   
##       ~       REPORTED ESTIMATES AND P-VALUES                             
##                                                                   
##---------------------------------------------------------------------
##---------------------------------------------------------------------

# Get number of obs. per test
distinct(results_df, h_label, outcome_label, N) |> 
  print(n = 30)

# Heat recall even has a statistically significant negative effect on reported willingness-to-act (p = 0.026) for subjects treated with the contextualizing information (black).

results_df |> 
  filter(outcome == "ccwta", 
         h == "h3_me",
         term == "d_info_control1:d_recall1")

# We do, however, find tentative evidence in support of H3b; as panel C and D show, emphasizing personal heat-related health risks tends to increase pro-climate opinions and willingness-to-pay to combat climate change. However, these positive effects barely fall short of conventional statistical significance (climate opinions, health info vs. control: p = 0.058; willingness-to-pay, health info vs. control: p = 0.074; climate opinions, health info vs. climate info: p = 0.057). For example, panel C indicates that the health risk information treatment increases climate opinions by 1.5 percentage points relative to the control group, equivalent to a small increase in climate support for the average respondent from 59.3 to 60.8%.

results_df |> 
  filter(outcome == "ccopi", 
         h == "h2c",
         term == "d_info3::2")

results_df |> 
  filter(outcome == "ccwtp", 
         h == "h2c",
         term == "d_info3::2")

results_df |> 
  filter(outcome == "ccopi", 
         h == "h2d",
         term == "d_info3::2")

# If climate information amplified the effect of experiencing unusually warm temperatures, we would see positive interaction effects (red). Instead, they tend to be negative. For climate awareness, the interaction is even statistically significant (p = 0.016). Thus, the local warming effect is positive and statistically significant (p = 0.003) but only in the control group (gray).

results_df |> 
  filter(outcome == "ccawa", 
         h == "h4",
         term == "d_info_control1:temp_dev7")

results_df |> 
  filter(outcome == "ccawa", 
         h == "h4",
         term == "temp_dev7")

##---------------------------------------------------------------------
##---------------------------------------------------------------------
##                                                                   
##       ~       FIGURE 1-4                             
##                                                                   
##---------------------------------------------------------------------
##---------------------------------------------------------------------

# Same axis limits
(plot_limits <- c(min(results_df$conf.low95), max(results_df$conf.high95)))
(plot_limits <- c(-7,5))

#######################################################################
## figure 1
#######################################################################

results_df |> 
  filter(h %in% c("h1"),
         term != "d_info::0",
         !str_detect(outcome, "_")) |> 
  ggplot(aes(x = fct_rev(outcome_label), y = estimate)) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", linewidth = .4) +
  geom_pointrange(aes(ymin = conf.low95, ymax = conf.high95), linewidth = .4, size = .2, position = position_dodge(width = .5)) +
  scale_y_continuous(breaks = seq(-100,100,2), expand = c(0,0)) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(),         legend.position = c(.89,.85),
        text = element_text(size = 11, family = "Times New Roman"),
  ) +
  guides(color = "none") +
  labs(x = NULL, y = "Effect estimate, %-points") +
  coord_flip(ylim = plot_limits)

ggsave("Damsbo-Svendsen and Heide-Jørgensen corr 23-0346.R1 Figure 1.png", width = 5, height = 2.2, dpi = 1200, bg = "white")

#######################################################################
## figure 2
#######################################################################

p_dat1 <- results_df |> 
  filter(h %in% c("h3_me"), !str_detect(outcome, "_")) |> 
  mutate(term = case_when(term == "d_info_control0:d_recall1" ~ "Control group",
                          term == "d_info_control1:d_recall1" ~ "Treatment group"))

p_dat2 <- results_df |> 
  filter(h %in% c("h3"),
         str_detect(term, ":"),
         !str_detect(outcome, "_")) |> 
  mutate(term = case_when(term == "d_recall1:d_info_control1" ~ "Difference"))

bind_rows(p_dat1, p_dat2) |> 
  mutate(interaction = term == "Difference",
         term = factor(term, 
                       levels = c("Treatment group", "Control group", "Difference"),
                       labels = c("Pooled info", "Control", "Interaction\n(difference)"))) |>
  ggplot(aes(x = fct_rev(outcome_label), y = estimate, color = term, shape = term)) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", linewidth = .4) +
  geom_pointrange(aes(ymin = conf.low95, ymax = conf.high95), linewidth = .4, size = .2, position = position_dodge(width = .5)) +
  scale_y_continuous(breaks = seq(-100,100,2), expand = c(0,0)) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.x = element_blank(),         
        legend.position = "bottom",
        text = element_text(size = 11, family = "Times New Roman")
  ) +
  labs(x = NULL, y = "Effect estimate, %-points") +
  coord_flip(ylim = plot_limits) + guides(color = guide_legend(reverse = TRUE),          shape = guide_legend(reverse = TRUE)) +
  scale_color_manual(name = NULL, values = c("grey40", "grey70", "black")) +
  scale_shape_manual(name = NULL, values = c(16, 16, 17))

ggsave("Damsbo-Svendsen and Heide-Jørgensen corr 23-0346.R1 Figure 2.png", width = 5, height = 3, dpi = 1200, bg = "white")

#######################################################################
## figure 3
#######################################################################

results_df |> 
  filter(h == "h2a" | h == "h2b" & str_sub(term, -1) == 1 | h == "h2c" & str_sub(term, -1) == 2 | h == "h2d" & str_sub(term, -1) == 2,
         !str_detect(outcome, "_")) |> 
  ggplot(aes(x = fct_rev(outcome_label), y = estimate)) +
  facet_wrap(~ h_label, nrow = 2) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", linewidth = .4) +
  geom_pointrange(aes(ymin = conf.low95, ymax = conf.high95), linewidth = .4, size = .2, position = position_dodge(width = .5)) +
  scale_y_continuous(breaks = seq(-100,100,2), expand = c(0,0)) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.x = element_blank(),         
        legend.position = "bottom",
        panel.spacing = unit(.1, "cm"),
        text = element_text(size = 11, family = "Times New Roman"),
        strip.text = element_text(size = 11, family = "Times New Roman"),
  ) +
  labs(x = NULL, y = "Effect estimate, %-points") +
  coord_flip(ylim = plot_limits)

ggsave("Damsbo-Svendsen and Heide-Jørgensen corr 23-0346.R1 Figure 3.png", width = 5, height = 5, dpi = 1200, bg = "white")

#######################################################################
## figure 4
#######################################################################

p_dat1 <- results_df |> 
  filter(h %in% c("h4_me"), !str_detect(outcome, "_")) |> 
  mutate(term = case_when(term == "d_info_control0:temp_dev7" ~ "Control group",
                          term == "d_info_control1:temp_dev7" ~ "Treatment group"))

p_dat2 <- results_df |> 
  filter(h %in% c("h4"),
         str_detect(term, ":"), !str_detect(outcome, "_")) |> 
  mutate(term = case_when(term == "d_info_control1:temp_dev7" ~ "Difference"))

bind_rows(p_dat1, p_dat2) |> 
  mutate(interaction = term == "Difference",
         term = factor(term, 
                       levels = c("Treatment group", "Control group", "Difference"),
                       labels = c("Pooled info", "Control", "Interaction\n(difference)"))) |>
  ggplot(aes(x = fct_rev(outcome_label), y = estimate, color = term, shape = term)) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", linewidth = .4) +
  geom_pointrange(aes(ymin = conf.low95, ymax = conf.high95), linewidth = .4, size = .2, position = position_dodge(width = .5)) +
  scale_y_continuous(breaks = seq(-100,100,2), expand = c(0,0)) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.x = element_blank(),         
        legend.position = "bottom",
        text = element_text(size = 11, family = "Times New Roman"),
  ) +
  labs(x = NULL, y = "Effect estimate, %-points") +
  coord_flip(ylim = plot_limits) + guides(color = guide_legend(reverse = TRUE),          shape = guide_legend(reverse = TRUE)) +
  scale_color_manual(name = NULL, values = c("grey40", "grey70", "black")) +
  scale_shape_manual(name = NULL, values = c(16, 16, 17))

ggsave("Damsbo-Svendsen and Heide-Jørgensen corr 23-0346.R1 Figure 4.png", width = 5, height = 3, dpi = 1200, bg = "white")