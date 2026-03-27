## Data analyses ##

## Reproduce Fig. 2, Fig. 3, Fig. S3, Fig. S4 and estimates in the manuscript ##

# Load R package
library(tidyverse)

# Read in dataset
data <- read_csv("ebv_data.csv")

## Data wrangling ##

# Identify individuals with negative baseline EBV and CMV
ebv_neg_bsl <- data %>% 
  filter(serumn == 1, ebv_pos == 0) %>% 
  pull(id)

cmv_neg_bsl <- data %>% 
  filter(serumn == 1, cmv_pos == 0) %>% 
  pull(id)

# Identify individuals with negative baseline EBV or CMV but not follow-up samples
ebv_neg_bsl_nofu <- data %>% 
  group_by(id) %>% 
  filter(n() == 1, serumn == 1, ebv_pos == 0) %>% 
  pull(id)

cmv_neg_bsl_nofu <- data %>% 
  group_by(id) %>% 
  filter(n() == 1, serumn == 1, cmv_pos == 0) %>% 
  pull(id)

# Identify individuals who became CMV negative after being CMV positive
false_cmv <- data %>% 
  filter(!is.na(cmv_pos)) %>% 
  group_by(id) %>% 
  arrange(id, serumn) %>% 
  mutate(
    false_pos = case_when(nth(cmv_pos, 3) < nth(cmv_pos, 2) ~ 1,
                          nth(cmv_pos, 2) < nth(cmv_pos, 1) ~ 1,
                          nth(cmv_pos, 3) < nth(cmv_pos, 1) ~ 1)
  ) %>% 
  filter(false_pos == 1) %>%
  distinct(id) %>% 
  pull(id)

# Define datasets for analyses
ebv_neg <- data %>% 
  filter(id %in% ebv_neg_bsl) %>% 
  filter(!id %in% ebv_neg_bsl_nofu)

cmv_neg <- data %>% 
  filter(id %in% cmv_neg_bsl) %>% 
  filter(!id %in% cmv_neg_bsl_nofu) %>% 
  filter(!id %in% false_cmv)
  
# Define seroconversion during follow-up

# 1 = baseline positive
# 2 = seroconverted during follow-up
# 3 = remained negative during follow-up

ebv_seroconvert <- data %>% 
  filter(!is.na(ebv_pos)) %>% 
  filter(!id %in% ebv_neg_bsl_nofu) %>% 
  group_by(id) %>% 
  mutate(
    seroconvert = if_else(min(ebv_pos) == 1, 1,
                          if_else(min(ebv_pos) == 0 & max(ebv_pos) == 1, 2,
                                  if_else(max(ebv_pos) == 0, 3, NA_real_)))
  ) %>% 
  filter(serumn == 1)

cmv_seroconvert <- data %>% 
  filter(!is.na(cmv_pos)) %>% 
  filter(!id %in% cmv_neg_bsl_nofu) %>% 
  filter(!id %in% false_cmv) %>% 
  group_by(id) %>% 
  mutate(
    seroconvert = if_else(min(cmv_pos) == 1, 1,
                          if_else(min(cmv_pos) == 0 & max(cmv_pos) == 1, 2,
                                  if_else(max(cmv_pos) == 0, 3, NA_real_)))
  ) %>% 
  filter(serumn == 1)

## Figure 2A ##

# Load R packages
library(Hmisc)
library(survival)
library(ggpol)
library(ggpubr)

# Estimate proportion of EBV
# Restrict to those with three samples
proportion_ebv <- ebv_neg %>%
  group_by(id) %>% 
  filter(n() == 3) %>% 
  group_by(casestat, serumn) %>%
  summarise(
    prop_ebv = mean(ebv_pos),
    ebv_pos = sum(ebv_pos, na.rm = TRUE),
    n = n()
  ) %>% 
  ungroup() %>% 
  pivot_longer(starts_with("prop_"), names_to = "variable", values_to = "values") %>% 
  group_by(serumn, casestat) %>%
  nest() %>%  
  mutate(
    ci = map(data, ~binconf(.$ebv_pos, .$n, return.df = TRUE)),
    ci_perc = map(ci, ~(. * 100))
  ) %>% 
  unnest(c(ci_perc, data))

# Estimate p-values
fig2a_pvalue <- ebv_neg %>%
  group_by(id) %>% 
  filter(n() == 3) %>% 
  mutate(caco = if_else(casestat == 1, "MS", "No MS")) %>%
  filter(!serumn == 1) %>% 
  group_by(serumn) %>%
  select(serumn, caco, ebv_pos) %>% 
  nest() %>% 
  mutate(
    model = map(data, ~fisher.test(table(.$caco, .$ebv_pos))),
    tidy_model = map(model, broom::tidy)
  ) %>% 
  unnest(tidy_model) %>% 
  mutate(symb_pvalue = if_else(p.value < 0.0001, "****", "n.s.")) %>% 
  ungroup() %>% 
  select(serumn, p.value, symb_pvalue)

# Define color palette
mycols <- c("#009E73", "#0072B2", "#D55E00", "#999999", "#F0E442")

# Make figure
fig2a <- proportion_ebv %>% 
  mutate(casestat = factor(casestat, levels = c("1", "0"))) %>% 
  left_join(fig2a_pvalue, by = "serumn") %>% 
  ggplot(aes(x = serumn, y = PointEst, ymin = Lower, ymax = Upper, 
             color = factor(casestat), group = casestat)) +
  geom_pointrange(position = position_dodge(width = 0.25)) +
  geom_line(position = position_dodge(width = 0.25)) +
  geom_text(aes(x = serumn, y = 105, label = symb_pvalue), 
            color = "black", size = 5, check_overlap = TRUE) +
  scale_x_discrete(
    limits = c("1", "2","3"),
    labels = c("1st", "2nd", "3rd"),
    name = "Serum sample"
  ) +
  scale_y_continuous(breaks = seq(0, 100, 20)) +
  scale_color_manual(
    values = mycols,
    name = "MS",
    breaks = c("1", "0"),
    labels=c("Yes", "No")
  ) +
  expand_limits(y = 105) +
  ylab("Seropositive samples (%)") +
  ggtitle("EBV") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(0.5, 0.5, 0, 0.5, "cm"),
    legend.position="none",
    legend.margin=margin(0,0,0,0)
  )

## Figure 2B ##

# Estimate proportion of CMV
# Restrict to those who were EBV-negative
# Restrict to those with three samples
proportion_cmv <- cmv_neg %>%
  filter(id %in% ebv_neg$id) %>%
  group_by(id) %>% 
  filter(n() == 3) %>% 
  group_by(casestat, serumn) %>%
  summarise(
    prop_cmv = mean(cmv_pos),
    cmv_pos = sum(cmv_pos, na.rm = TRUE),
    n = n()
  ) %>%  
  ungroup() %>% 
  pivot_longer(starts_with("prop_"), names_to = "variable", values_to = "values") %>% 
  group_by(serumn, casestat) %>%
  nest() %>%  
  mutate(
    ci = map(data, ~binconf(.$cmv_pos, .$n, return.df = TRUE)),
    ci_perc = map(ci, ~(. * 100))
  ) %>% 
  unnest(c(ci_perc, data))

# Estimate p-values
fig2b_pvalue <- cmv_neg %>%
  filter(id %in% ebv_neg$id) %>%
  group_by(id) %>% 
  filter(n() == 3) %>% 
  mutate(caco = if_else(casestat == 1, "MS", "No MS")) %>%
  filter(!serumn == 1) %>% 
  group_by(serumn) %>%
  select(serumn, caco, cmv_pos) %>% 
  nest() %>% 
  mutate(
    model = map(data, ~fisher.test(table(.$caco, .$cmv_pos))),
    tidy_model = map(model, broom::tidy)
  ) %>% 
  unnest(tidy_model) %>% 
  mutate(symb_pvalue = if_else(p.value < 0.0001, "****", "n.s.")) %>% 
  ungroup() %>% 
  select(serumn, p.value, symb_pvalue)

# Make figure
fig2b <- proportion_cmv %>%
  mutate(casestat = factor(casestat, levels = c("1", "0"))) %>% 
  left_join(fig2b_pvalue, by = "serumn") %>% 
  ggplot(aes(x = serumn, y = PointEst, ymin = Lower, ymax = Upper, 
             color = casestat, group = casestat)) +
  geom_pointrange(position = position_dodge(width = 0.25)) +
  geom_line(position = position_dodge(width = 0.25)) +
  geom_text(aes(x = serumn, y = 42, label = symb_pvalue), 
            color = "black", size = 5, check_overlap = TRUE) +
  scale_x_discrete(
    limits = c("1", "2","3"),
    labels = c("1st", "2nd", "3rd"),
    name = "Serum sample"
  ) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  scale_color_manual(
    values = mycols,
    name = "MS",
    breaks = c("1", "0"),
    labels=c("Yes", "No")
  ) +
  expand_limits(y = 42) +
  ylab("Seropositive samples (%)") +
  ggtitle("CMV") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(0.5, 0.5, 0, 0.5, "cm"),
    legend.position="none",
    legend.margin=margin(0,0,0,0)
  )

## Figure 2C ##

model_all_ebv <- ebv_seroconvert %>% 
  mutate(
    ebv_status = case_when(seroconvert == 3 ~ "EBV-negative",
                           seroconvert == 2 ~ "Seroconverted",
                           seroconvert == 1 ~ "Baseline positive"),
    ebv_status = factor(ebv_status, levels = c("EBV-negative", "Seroconverted", "Baseline positive")),
    ebv_status = relevel(ebv_status, ref = "EBV-negative"),
    time = if_else(casestat == 0, 2, 1)
  )

matched_ebv <- coxph(Surv(time, casestat) ~ ebv_status + strata(group), data = model_all_ebv, method=c("exact"))
matched_ebv_tidy <- broom::tidy(matched_ebv, exponentiate = TRUE, conf.int = TRUE)

fig2c <- matched_ebv_tidy %>% 
  select(term, estimate, conf.low, conf.high, p.value) %>% 
  add_row(term = "EBV-negative", estimate = 1, conf.low = 1, conf.high = 1, p.value = NA_real_) %>% 
  mutate(
    term = str_replace(term, "ebv_status", ""),
    term = factor(term, levels = c("EBV-negative", "Seroconverted", "Baseline positive")),
    symb_pvalue = if_else(p.value < 0.001, "***", 
                          if_else(p.value < 0.01, "**", "n.s."))
  ) %>% 
  ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_pointrange(color = "#D55E00") +
  geom_text(aes(x = term, y = 350, label = symb_pvalue), size = 5) +
  expand_limits(y = 0.5) +
  scale_y_continuous(
    breaks = c(1, 5, 20, 50),
    trans = "log"
  ) +
  scale_x_discrete(labels = c("EBV-negative\n(ref. group)", "Seroconverted", "Baseline\nEBV-positive")) +
  ylab("Hazard ratio for MS (95% CI)") +
  theme_minimal() +
  theme(
    plot.margin = margin(1.5, 0.5, 0, 0.5, "cm"),
    axis.title.x = element_blank()
  )

## Figure 2D ##

model_all_cmv <- cmv_seroconvert %>% 
  mutate(
    cmv_status = case_when(seroconvert == 3 ~ "CMV-negative",
                           seroconvert == 2 ~ "Seroconverted",
                           seroconvert == 1 ~ "Baseline positive"),
    cmv_status = factor(cmv_status, levels = c("CMV-negative", "Seroconverted", "Baseline positive")),
    cmv_status = relevel(cmv_status, ref = "CMV-negative"),
    time = if_else(casestat == 0, 2, 1)
  )

matched_cmv <- coxph(Surv(time, casestat) ~ cmv_status + strata(group), data = model_all_cmv, method=c("exact"))
matched_cmv_tidy <- broom::tidy(matched_cmv, exponentiate = TRUE, conf.int = TRUE)

fig2d <- matched_cmv_tidy %>% 
  select(term, estimate, conf.low, conf.high, p.value) %>% 
  add_row(term = "CMV-negative", estimate = 1, conf.low = 1, conf.high = 1, p.value = NA_real_) %>% 
  mutate(
    term = str_replace(term, "cmv_status", ""),
    term = factor(term, levels = c("CMV-negative", "Seroconverted", "Baseline positive")),
    symb_pvalue = if_else(p.value < 0.001, "***", 
                          if_else(p.value < 0.01, "**", 
                                  if_else(p.value < 0.05, "*", "n.s.")))
  ) %>% 
  ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_pointrange(color = "#D55E00") +
  geom_text(aes(x = term, y = 350, label = symb_pvalue), size = 5) +
  expand_limits(y = 0.5) +
  scale_y_continuous(
    breaks = c(1, 5, 20, 50),
    trans = "log"
  ) +
  scale_x_discrete(labels = c("CMV-negative\n(ref. group)", "Seroconverted", "Baseline\nCMV-positive")) +
  ylab("Hazard ratio for MS (95% CI)") +
  theme_minimal() +
  theme(
    plot.margin = margin(1.5, 0.5, 0, 0.5, "cm"),
    axis.title.x = element_blank()
  )

## Combine into one figure ##
ggarrange(ggarrange(fig2a, fig2b, common.legend = TRUE, legend = "bottom", ncol = 2, labels = c("A", "B")), 
          ggarrange(fig2c, fig2d, ncol = 2, labels = c("C", "D"), vjust = 3), nrow = 2)

# Output figure
ggsave("figure2.pdf", width = 8, height = 6, device = cairo_pdf)

## Figure 3 ##

# Load R packages
library(lme4)
library(effects)
library(afex)

# Define dataset for analyses
# Restrict to those with 1) 3 samples, 2) seroconverted at second sample 3) ebv-neg at baseline
nfl_long <- ebv_neg %>% 
  filter(!is.na(nfl)) %>% 
  group_by(id) %>% 
  filter(n() == 3) %>%
  arrange(id, serumn) %>% 
  mutate(
    ebv_pos_middle = if_else(nth(ebv_pos, 2) == 1, 1, 0),
    caco = if_else(casestat == 1, "MS", "No MS"),
    nfl_log = log(nfl),
    nfl_log_bsl = first(nfl_log),
    nfl_log_scale = nfl_log-nfl_log_bsl,
    time_cat = serumn - 1,
    time_cat = factor(time_cat),
    caco = factor(caco),
    caco = relevel(caco, ref = "No MS"),
    time_onset = time_to_MS*-1
  ) %>% 
  filter(ebv_pos_middle == 1)

## Figure 3A ##

# Calculate p-values
# Use age and sex-adjusted log NfL values
fig3a_pvalues <- nfl_long %>% 
  group_by(time_cat) %>% 
  nest() %>% 
  mutate(
    model = map(data, ~lm(nfl_log_adj ~ casestat, data = .x)),
    tidy_model = map(model, broom::tidy)
  ) %>% 
  unnest(tidy_model) %>% 
  filter(term == "casestat") %>% 
  mutate(
    symb_pvalue = if_else(p.value < 0.001, "***", 
                          if_else(p.value < 0.01, "**", 
                                  if_else(p.value < 0.05, "*", "n.s.")))
  ) %>% 
  select(time_cat, p.value, symb_pvalue) 

# Make figure
labels <- c("0" = "NfL levels\nbefore EBV infection", 
            "1" = "NfL levels\nat time of 1st EBV positive sample",
            "2" = "NfL levels\nafter EBV infection")

fig3a <- nfl_long %>%
  mutate(caco = factor(caco, levels = c("MS", "No MS"))) %>% 
  ggplot() +
  geom_boxjitter(aes(x = caco, y = nfl, fill = caco),
                 jitter.shape = 21, jitter.color = NA, 
                 jitter.params = list(width = 0.08),
                 outlier.color = NA, errorbar.draw = TRUE, show.legend = FALSE) +
  annotate("segment", y = 80, yend=80, x=1, xend=2, size = 0.3) +
  annotate("segment", y = 80, yend=75, x=1, xend=1, size = 0.3) +
  annotate("segment", y = 80, yend=75, x=2, xend=2, size = 0.3) +
  geom_text(data = fig3a_pvalues,
            aes(x = 1.5, y = 95, label = symb_pvalue), size = 5) +
  expand_limits(y = 95) +
  scale_y_continuous(
    name = "Neurofilament light chain, pg/mL", 
    trans = "log", 
    breaks = c(5, 10, 20, 50)
  ) +
  scale_x_discrete(limits = c("MS", "No MS")) +
  facet_wrap(time_cat ~ ., labeller=labeller(time_cat = labels)) +
  scale_fill_manual(values = mycols) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  )

## Figure 3B and 3C ##

# Calculate estimates for figure
model <- lmer(nfl_log_scale ~ caco * time_cat + (1 | id), data = nfl_long)

summary(model) # p-value for cacoMS:time_cat2  0.00471 **

estimates <- allEffects(model)
est_extracted <- estimates[[1]]

est_df <- as.data.frame(est_extracted) %>%
  mutate(across(c(fit, lower, upper), exp)) %>% 
  mutate(across(c(fit, lower, upper), ~ (. - 1))) %>% 
  mutate(p.value = if_else(caco == "MS" & time_cat == 2, "**", "n.s."), # p-value for cacoMS:time_cat2  0.00471 **
         lower = if_else(time_cat == 0, 0, lower),
         upper = if_else(time_cat == 0, 0, upper))

## Figure 3B ##

fig3b <- est_df %>% 
  filter(caco == "MS") %>% 
  ggplot(aes(x = time_cat, y = fit, ymin = lower, ymax = upper, 
             group = caco, color = caco)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange() +
  geom_line() +
  geom_text(data = est_df %>% filter(!time_cat == 0, caco == "MS"),
            aes(x = time_cat, y = 1.05, label = p.value), 
            color = "black", size = 4) +
  ggtitle("MS") +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(-0.2, 1, 0.2)
  ) +
  scale_x_discrete(labels = c("Before\nEBV-infection\n(ref. group)", 
                              "Around time of\nEBV-infection",
                              "After\nEBV-infection")) +
  scale_color_manual(values = "#009E73") +
  expand_limits(y = c(-0.27, 1.05)) +
  ylab("Within-person change in NfL (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    plot.margin = margin(0.5, 0.5, 0, 0.5, "cm"),
    legend.position="none",
    legend.margin=margin(0,0,0,0)
  )

## Figure 3C ##

fig3c <- est_df %>% 
  filter(caco == "No MS") %>% 
  ggplot(aes(x = time_cat, y = fit, ymin = lower, ymax = upper, 
             group = caco, color = caco)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange() +
  geom_line() +
  geom_text(data = est_df %>% filter(!time_cat == 0),
            aes(x = time_cat, y = 1.05, label = p.value), 
            color = "black", size = 4, check_overlap = TRUE) +
  ggtitle("No MS") +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(-0.2, 1, 0.2)
  ) +
  scale_x_discrete(labels = c("Before\nEBV-infection\n(ref. group)", 
                              "Around time of\nEBV-infection",
                              "After\nEBV-infection")) +
  scale_color_manual(values = "#0072B2") +
  expand_limits(y = c(-0.27, 1.05)) +
  ylab("Within-person change in NfL (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    plot.margin = margin(0.5, 0.5, 0, 0.5, "cm"),
    legend.position="none",
    legend.margin=margin(0,0,0,0)
  )

# Combine into one figure
ggarrange(fig3a, ncol = 1, labels = c("A"), heights = c(1.2, 1),
          ggarrange(fig3b, fig3c, ncol = 2, labels = c("B", "C"), vjust = 2))

# Output figure
ggsave("figure3.pdf", 
       width = 8, height = 8, device = cairo_pdf)

## EBV - positive or negative at last sample, estimates provided in the manuscript ##

model_last_ebv <- ebv_seroconvert %>% 
  mutate(
    ebv_status_last = case_when(seroconvert == 3 ~ "EBV-negative",
                           seroconvert == 2 ~ "Positive",
                           seroconvert == 1 ~ "Positive"),
    ebv_status_last = factor(ebv_status_last, levels = c("EBV-negative", "Positive")),
    ebv_status_last = relevel(ebv_status_last, ref = "EBV-negative"),
    time = if_else(casestat == 0, 2, 1)
  )

matched_ebv_last <- coxph(Surv(time, casestat) ~ ebv_status_last + strata(group), data = model_last_ebv, method=c("exact"))
matched_ebv_last_tidy <- broom::tidy(matched_ebv_last, exponentiate = TRUE, conf.int = TRUE)

### Supplementary materials ###

## Figure S3 ##

# Define order for figure, sort by follow-up time and time of EBV conversion
order <- ebv_neg %>% 
  filter(casestat == 1) %>% 
  group_by(id) %>% 
  mutate(
    time_ebv = if_else(ebv_pos == 1, time_from_bsl, NA_real_),
    time_ms = max(time_to_MS)
  ) %>% 
  arrange(id, serumn) %>% 
  fill(time_ebv, .direction = "updown") %>%
  filter(serumn == 1) %>% 
  arrange(time_to_MS, time_ebv) %>% 
  ungroup() %>% 
  mutate(
    order = row_number(),
    order = factor(order),
    order = fct_reorder(order, desc(order))
  ) %>% 
  select(id, order, time_ebv, time_ms)

# Define data for figure
fig_S3_data <- ebv_neg %>% 
  filter(casestat == 1) %>%
  group_by(id) %>% 
  arrange(id, serumn) %>%
  mutate(
    time_sample_end = lead(time_from_bsl),
    time_sample_last = time_from_bsl,
    ms = 1
  ) %>% 
  group_by(id) %>% 
  left_join(order, by = "id")

# Make figure
figS3 <- fig_S3_data %>% 
  ggplot() +
  geom_segment(aes(x = time_from_bsl, xend = time_sample_end, y = order, yend = order, color = factor(ebv_pos))) +
  geom_segment(aes(x = time_sample_last, xend = time_ms, y = order, yend = order, color = factor(ebv_pos))) +
  geom_point(aes(x = time_from_bsl, y = order, color = factor(ebv_pos))) +
  geom_point(aes(x = time_ms, y = order, fill = factor(ms)), color = "#D55E00") +
  scale_color_manual(name = "EBV status",
                     values = mycols,
                     labels = c("EBV-negative", "EBV-positive")) +
  scale_fill_discrete(name = "MS status",
                      labels = c("MS onset")) +
  xlab("\nFollow-up time, years") +
  ylab("Participants") +
  theme_minimal() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y=element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(1, 1, 0, 1, "cm"),
    legend.position="bottom",
    legend.title = element_blank(),
    legend.margin=margin(0,0,0,0)
  )

# Arrange figure
ggarrange(figS3)

# Output figure
ggsave("figureS3.pdf", 
       width = 4, height = 6, device = cairo_pdf)

## Figure S4 ##

fig_S4 <- nfl_long %>% 
  select(id, caco, nfl, time_onset, time_cat) %>% 
  filter(caco == "MS") %>% 
  group_by(id) %>% 
  mutate(
    nfl_first = first(nfl),
    fold_change = nfl/nfl_first
  ) %>% 
  filter(time_cat %in% c("1", "2")) %>% 
  ggplot(aes(x = time_onset, y = fold_change)) +
  geom_line(aes(group = id), color = "black", linetype = "solid", alpha = 0.2) +
  geom_point(aes(color = time_cat)) +
  geom_smooth(method = "loess") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_continuous(name = "Fold change in neurofilament light chain, pg/mL", 
                     trans = "log", 
                     breaks = c(0.5, 1, 2, 5, 10)) +
  scale_x_continuous(breaks = seq(-10, 1, 2)) +
  xlab("Time to MS onset, years") +
  scale_color_manual(values = c("#0072B2", "#D55E00"), 
                     labels = c("1st EBV-positive", "2nd EBV-positive"),
                     name = "Sample") +
  theme_bw()

# Arrange figure
ggarrange(fig_S4)

# Output figure
ggsave("figureS4.pdf", 
       width = 8, height = 5, device = cairo_pdf)
