
library(tidyverse)

# create results directory if it doesn't already exist
if(!dir.exists("../results")) {
  dir.create("../results")
}


# read in data from pre-computed simulation run
res <- bind_rows(
  read_csv("results/pscore_rf_rvote__06-Jan-2023-20-26-42.csv"),
  read_csv("results/pscore_4_lowreg_out4_lowreg__06-Jan-2023-15-36-47.csv")
) %>%
  mutate(weight_order = as.numeric(sapply(str_split(weight_name, "_"), `[`, 3)))
  



# APPENDIX FIGURE E.1

# compute bias and rmse across simulation runs
res %>%
  pivot_longer(contains("est") | contains("oracle"), names_to = "estimator",
               values_to = "estimate") %>%
  filter(!estimator %in% c("oracle")) %>%
  group_by(weight_order, out_name, estimator, pscore, y_model) %>%
  summarise(bias = mean(estimate - truth), mad = mean(abs(estimate - truth)),
            sd = sqrt(mean((estimate - mean(estimate)) ^ 2)),
            rmse = sqrt(bias ^ 2 + sd ^ 2)
            ) %>%
  ungroup() %>%
  filter(estimator %in% c("oracle_ht", "weight_est" ,"mr_est", "drp_est")) %>%
  mutate(y_model = case_when(y_model == "out4_lowreg" ~ "Outcome: 4th Order Logit",
                            y_model == "rvote" ~ "Outcome: True Vote"),
         pscore = case_when(pscore == "pscore_4_lowreg" ~ "Response: 4th Order Logit",
                            pscore == "pscore_rf" ~ "Response: Random Forest"),
         estimator = case_when(
            estimator == "mr_est" & out_name == "out_rf" ~ "Random Forest",
            estimator == "drp_est" & out_name == "out_rf" ~ "DRP: RF",
            estimator == "mr_est" & out_name == "out_ridge_order_3" ~ "Ridge",
            estimator == "drp_est" & out_name == "out_ridge_order_3" ~ "DRP: Ridge",
            estimator == "weight_est" ~ "Multilevel Calibration",
            estimator == "oracle_ht" ~ "Oracle Hajek",
            TRUE  ~ out_name),
          ) %>%
  pivot_longer(c(bias, mad, sd, rmse), names_to = "metric", values_to = "value") %>%
  filter(metric %in% c("bias", "rmse"),
         !(estimator == "Oracle Hajek" &
            y_model == "Outcome: 4th Order Logit" & metric == "rmse")
            ) %>%
  mutate(metric = case_when(metric == "bias" ~ "Absolute Bias",
                            metric == "rmse" ~ "RMSE"),
         estimator = fct_relevel(estimator, "Multilevel Calibration",
                           "Random Forest", "Ridge", "DRP: RF"))-> plotdf

plotdf %>%
  ggplot(aes(x = weight_order, y = abs(value), color = estimator,
             lty = (str_detect(estimator, "DRP") | estimator == "Multilevel Calibration"))) +
  geom_line() +
  guides(color = guide_legend(nrow = 2)) +
  xlab("Order of interactions balanced") +
  ylab("") +
  scale_y_continuous(labels = scales::percent_format(accuracy= .1)) +
  facet_grid(metric ~ y_model + pscore, scales = "free") +
  scale_color_brewer("", type = "qual", palette = "Dark2") +
  scale_linetype_manual("", values = c(2,1)) +
  guides(lty = F) +
  theme_bw(8) + 
  theme(legend.position = "bottom") -> p

ggsave("figure_e1.png", p, "png", "../results", width = 5, height = 3.5)




# APPENDIX FIGURE E.2

# compute coverage of standard errors
res %>%
  pivot_longer(contains("est") | contains("oracle") | contains("se")) %>%
  mutate(se = ifelse(str_detect(name, "se"), "se2", "estimate"), 
         name = str_replace(str_replace(name, "_est", ""), "_se2", "")) %>%
  pivot_wider(names_from = se, values_from = value) %>%
  rename(estimator = name) %>%
  filter(!str_detect(estimator, "oracle")) %>%
  group_by(weight_order, out_name, estimator, pscore, y_model) %>%
  summarise(bias = mean(estimate - truth), mad = mean(abs(estimate - truth)),
            sd = sqrt(mean((estimate - mean(estimate)) ^ 2)),
            rmse = sqrt(bias ^ 2 + sd ^ 2),
            coverage = mean((truth <= estimate + 2 * sqrt(se2)) * (truth >= estimate -  2 * sqrt(se2)))
            ) %>%
  ungroup() %>%
  filter(estimator %in% c("oracle_ht", "weight", "drp")) %>%
  mutate(y_model = case_when(y_model == "out4_lowreg" ~ "Outcome: 4th Order Logit",
                            y_model == "rvote" ~ "Outcome: True Vote"),
         pscore = case_when(pscore == "pscore_4_lowreg" ~ "Response: 4th Order Logit",
                            pscore == "pscore_rf" ~ "Response: Random Forest"),
         estimator = case_when(
            estimator == "mr" & out_name == "out_rf" ~ "Random Forest",
            estimator == "drp" & out_name == "out_rf" ~ "DRP: RF",
            estimator == "mr" & out_name == "out_ridge_order_3" ~ "Ridge",
            estimator == "drp" & out_name == "out_ridge_order_3" ~ "DRP: Ridge",
            estimator == "weight" ~ "Multilevel Calibration",
            estimator == "oracle_ht" ~ "Oracle Hajek",
                                TRUE  ~ out_name),
          ) %>%
  pivot_longer(c(bias, mad, sd, rmse, coverage), names_to = "metric", values_to = "value") %>%
  filter(metric == "coverage") %>%
  mutate(metric = case_when(metric == "bias" ~ "Absolute Bias",
                            metric == "rmse" ~ "RMSE",
                            metric == "coverage" ~ "Coverage"),
         estimator = fct_relevel(estimator, "Multilevel Calibration",
                           "Random Forest", "Ridge", "DRP: RF"))-> plotdf_cov

plotdf_cov %>%
  ggplot(aes(x = weight_order, y = abs(value), color = estimator)) +
  geom_line() +
  geom_hline(yintercept = 0.95, lty = 3) +
  guides(color = guide_legend(nrow = 2)) +
  xlab("Order of interactions balanced") +
  ylab("Coverage of Approximate 95% Confidence Interval") +
  scale_y_continuous(labels = scales::percent_format(accuracy= .1), lim = c(0.9, 1)) +
  facet_grid(. ~ y_model + pscore, scales = "free") +
  scale_color_brewer("", type = "qual", palette = "Dark2") +
  scale_linetype_manual("", values = c(2,1)) +
  guides(lty = F) +
  theme_bw(8) + 
  theme(legend.position = "bottom") -> p

ggsave("figure_e2.png", p, "png", "../results", width = 5, height = 3.5)

