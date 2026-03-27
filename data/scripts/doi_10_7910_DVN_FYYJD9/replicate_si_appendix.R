### ---- Setup ------
## Plotting params
fixed <- 1
spacing <- .1285

## Read in survey data
norc_survey <- read_rds("norc_survey.rds") 

lucid_survey <- read_rds("lucid_survey.rds")

## Read in conjoint data
lucid_conjoint_a <- read_rds("lucid_conjoint_agreement.rds")
lucid_conjoint_p <- read_rds("lucid_conjoint_person.rds")

norc_conjoint <- read_rds("norc_conjoint.rds")

## Join conjoint data w/ survey data
norc_conjoint_long <-
  right_join(norc_conjoint, norc_survey, by = "CaseId")

lucid_conjoint_a_long <- 
  left_join(lucid_conjoint_a, lucid_survey, by = "admin_ResponseId")

lucid_conjoint_p_long <- 
  left_join(lucid_conjoint_p, lucid_survey, by = "admin_ResponseId")


### ---- Fig. S1-S2: correlations among survey items ---- 

source("si_corrplots.R")

### ---- Table S2: demographics in survey samples and U.S. pop ---- 

## Target proportions from census
target_pop <- read_rds("census_targets.rds")

## Check divergence between surveys and target pop.
get_current_miss(lucid_survey, target = target_pop)
get_current_miss(norc_survey, target = target_pop)

## variance inflation caused by weights
design_effect(lucid_survey$survey_weight)
design_effect(norc_survey$survey_weight)

effective_sample_size(lucid_survey$survey_weight)
effective_sample_size(norc_survey$survey_weight)

diagnose_lucid <-
  lucid_survey %>%
  diagnose_weights(weights = lucid_survey$survey_weight,
                   target = target_pop) %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate(level = factor(
    level,
    levels = c(
      "Female",
      "Male",
      "South",
      "West",
      "Midwest",
      "Northeast",
      "White",
      "Hispanic",
      "Black",
      "AAPI",
      "Other",
      "$15,000 or less",
      "$15,000-$24,999",
      "$25,000-$34,999",
      "$35,000-$49,999",
      "$50,000-$74,999",
      "$75,000-$99,999",
      "$100,000-$149,999",
      "$150,000-$199,999",
      "$200,000 and above",
      "No high school diploma",
      "High school diploma",
      "Some college",
      "Associate's degree",
      "Bachelor's degree",
      "Graduate degree",
      "18-24",
      "25-29",
      "30-34",
      "35-39",
      "40-44",
      "45-49",
      "50-54",
      "55-59",
      "60-64",
      "65-69",
      "70-74",
      "75+"
    )
  )) %>%
  arrange(level)

diagnose_norc <-
  norc_survey %>%
  diagnose_weights(weights = norc_survey$survey_weight,
                   target = target_pop) %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate(level = factor(
    level,
    levels = c(
      "Female",
      "Male",
      "South",
      "West",
      "Midwest",
      "Northeast",
      "White",
      "Hispanic",
      "Black",
      "AAPI",
      "Other",
      "$15,000 or less",
      "$15,000-$24,999",
      "$25,000-$34,999",
      "$35,000-$49,999",
      "$50,000-$74,999",
      "$75,000-$99,999",
      "$100,000-$149,999",
      "$150,000-$199,999",
      "$200,000 and above",
      "No high school diploma",
      "High school diploma",
      "Some college",
      "Associate's degree",
      "Bachelor's degree",
      "Graduate degree",
      "18-24",
      "25-29",
      "30-34",
      "35-39",
      "40-44",
      "45-49",
      "50-54",
      "55-59",
      "60-64",
      "65-69",
      "70-74",
      "75+"
    )
  )) %>%
  arrange(level) 

## Lucid error
mean(diagnose_lucid$error_original)
mean(diagnose_lucid$error_weighted)

## NORC error
mean(diagnose_norc$error_original)
mean(diagnose_norc$error_weighted)

norc_subset <-
  diagnose_norc %>%
  select(level, target, prop_weighted, prop_original) %>%
  rename(norc_weighted = prop_weighted,
         norc_original = prop_original)

lucid_subset <-
  diagnose_lucid %>%
  select(level, target, prop_weighted, prop_original) %>%
  rename(lucid_weighted = prop_weighted,
         lucid_original = prop_original)

diagnose_dat <-
  left_join(lucid_subset, norc_subset) %>%
  select(level,
         target,
         lucid_original,
         norc_original,
         lucid_weighted,
         norc_weighted) %>% 
  mutate()

diagnose_dat %>%
  xtable() %>%
  print(include.rownames = FALSE,
        include.colnames = FALSE,
        hline.after = c(),
        only.contents = TRUE,
        type = "latex",
        file = "compare_sample.tex")

### ---- Fig. S3-S9, Table S3: Marginal means, AMCEs, ATEs by survey weights ----

## Fig. S3-S6
source("si_wts_lucid.R")

## Fig. S7-S9, Table S3
source("si_wts_norc.R")

### ---- Fig. S10-S12: Supplementary analyses for vaccine recipient conjoint ----

source("si_causal_interact_lucid.R")

### ---- Fig. S13-S14, Table S4: conjoint choices and COVAX donations ----

source("si_conjoint_donate_norc.R")

### ---- Fig. S15-S20: Marginal means, AMCEs on ordinal outcomes ----

source("si_conjoint_ordinal.R")

### ---- Fig. S21-S68: Heterogeneity in MMs and AMCEs by covariates ----

## Note: if the graphics are not rendering properly then open these files in 
## a separate window and execute there rather than via source() 

## Lucid person conjoints: Fig. S21-S38
source("si_conjoint_hetfx_p_lucid.R")

## Lucid agreement conjoints: Fig. S39-S56
source("si_conjoint_hetfx_a_lucid.R")

## NORC agreement conjoints: Fig. S57-S68
source("si_conjoint_hetfx_a_norc.R")


### ---- Fig. S69-S75: Causal forest estimates for persuasion experiment -----

## Run code for estimation. Note that GRF estimation may be slow depending on 
## your hardware
source("si_causal_forest.R")

## Optional: uncomment to directly import dataset of estimates from GFR. 
#grf_df <- read_rds("grf_estimates.rds")

## Descriptive summaries
grf_df %>%
  group_by(W) %>%
  summarise(prop_pos = mean(tau_hat > 0),
            prop_sig = mean(ci_lower > 0))

## What proportion of point estimates are positive?
grf_df %>%
  group_by(W) %>%
  summarise(prop_pos = mean(tau_hat > 0))

# How many CIs include zero?
grf_df %>%
  group_by(W) %>%
  summarise(mean(ci_lower <= 0 & ci_upper >= 0))

# What proportion of the CIs that don't include zero are positive?
these <- with(grf_df, which(ci_lower <= 0 & ci_upper >= 0))
grf_df[-these, ] %>%
  group_by(W) %>%
  summarise(prop_pos = mean(tau_hat > 0))

###-------------- Fig. S69: Causal forest estimated treatment effects ---------
plot_df <-
  grf_df %>%
  group_by(W) %>%
  mutate(order = rank(tau_hat, ties.method = "first")) %>%
  ungroup()


grf_plot <-
  plot_df %>%
  ggplot(., aes(x = tau_hat, y = order)) +
  geom_errorbarh(
    aes(xmin = ci_lower, xmax = ci_upper),
    col = "dimgrey",
    alpha = 0.2,
    height = 0,
    size = 0.5
  ) +
  geom_point(
    size = 1,
    pch = 20,
    alpha = 0.8,
    fill = "black",
    col = "black"
  ) +
  geom_vline(xintercept = 0,
             size = 0.5,
             lty = 2) +
  labs(title = "",
       x = "Estimated treatment effects and 95% confidence intervals from causal forest",
       y = "") +
  scale_x_continuous(limits = c(-0.5, 0.5)) +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  facet_wrap( ~ W, ncol = 2) +
  theme_tufte() +
  theme(
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(size = 0.25),
    strip.text = element_text(size = 11),
    axis.line.x = element_line(size = 0.25),
    axis.title.x = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    plot.margin = unit(c(
      t = -0.5,
      r = 0.5,
      b = 0.1,
      l = 0.1
    ), "lines")
  )


ggsave(
  grf_plot,
  filename = "grf_plot.pdf",
  height = 8,
  width = 6.5,
  device = "pdf",
  dpi = 1000
)

###-------------- Fig. S70: Causal forest estimates by partisanship ---------
bpr_colors <- c("#1F3A93", "#7C2C55", "#D91E18")

pid_labs <- levels(norc_survey$x_pid7)

p_pid <-
  plot_df %>%
  select(W, tau_hat, ci_upper, ci_lower, x_pid_7n) %>%
  gather(key = "X", value = "value", x_pid_7n) %>%
  ggplot(., aes(
    y = tau_hat,
    x = factor(value),
    color = value,
    fill = value
  )) +
  geom_hline(
    yintercept = 0,
    color = "black",
    lty = 2,
    size = 0.5
  ) +
  geom_point(alpha = 0.5,
             pch = 21,
             #color = "black",
             position = position_dodge2(.5)) +
  scale_fill_gradient2(
    name = "",
    labels = pid_labs,
    low = bpr_colors[1],
    mid = bpr_colors[2],
    high = bpr_colors[3],
    midpoint = 4,
    guide = "none"
  ) +
  scale_color_gradient2(
    name = "",
    labels = pid_labs,
    low = bpr_colors[1],
    mid = bpr_colors[2],
    high = bpr_colors[3],
    midpoint = 4,
    guide = "legend"
  ) +
  facet_wrap( ~ W) +
  theme_tufte() +
  labs(x = "",
       y = expression(hat(tau)))  +
  theme(
    strip.text = element_text(size = 11),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = unit(c(
      t = 0.5,
      r = 0.5,
      b = 0.1,
      l = 0.1
    ), "lines"),
    legend.position = c(0.85, 0.3),
    legend.spacing = unit(0.1, 'lines'),
    legend.spacing.x = unit(0.1, 'lines'),
    legend.spacing.y = unit(0.3, 'lines'),
    legend.key = element_rect(color = NA, fill = NA),
    legend.key.size = unit(0.4, "cm")
  ) +
  guides(fill = guide_legend(override.aes = list(
    size = 2,
    alpha = 1,
    #shape = 15,
    key_glyph = "polygon3"
  )))


ggsave(filename = "grf_by_pid.pdf",
       p_pid,
       width = 7,
       height = 6)

###-------------- Fig. S71: Causal forest estimates by ideology ---------
ideo_labs <- levels(norc_survey$x_ideo_7)

p_ideo <-
  plot_df %>%
  select(W, tau_hat, ci_upper, ci_lower, x_ideo_7n) %>%
  gather(key = "X", value = "value", x_ideo_7n) %>%
  ggplot(., aes(
    y = tau_hat,
    x = factor(value),
    color = value,
    fill = value
  )) +
  geom_hline(
    yintercept = 0,
    color = "black",
    lty = 2,
    size = 0.5
  ) +
  geom_point(alpha = 0.5,
             pch = 21,
             #color = "black",
             position = position_dodge2(.5)) +
  scale_fill_gradient2(
    name = "",
    labels = ideo_labs,
    low = bpr_colors[1],
    mid = bpr_colors[2],
    high = bpr_colors[3],
    midpoint = 4,
    guide = "none"
  ) +
  scale_color_gradient2(
    name = "",
    labels = ideo_labs,
    low = bpr_colors[1],
    mid = bpr_colors[2],
    high = bpr_colors[3],
    midpoint = 4,
    guide = "legend"
  ) +
  facet_wrap( ~ W) +
  theme_tufte() +
  labs(x = "",
       y = expression(hat(tau))) +
  theme(
    strip.text = element_text(size = 11),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = unit(c(
      t = 0.5,
      r = 0.5,
      b = 0.1,
      l = 0.1
    ), "lines"),
    legend.position = c(0.85, 0.3),
    legend.spacing = unit(0.1, 'lines'),
    legend.spacing.x = unit(0.1, 'lines'),
    legend.spacing.y = unit(0.3, 'lines'),
    legend.key = element_rect(color = NA, fill = NA),
    legend.key.size = unit(0.4, "cm")
  ) +
  guides(fill = guide_legend(override.aes = list(
    size = 2,
    alpha = 1,
    #shape = 15,
    key_glyph = "polygon3"
  )))


ggsave(filename = "grf_by_ideo.pdf",
       p_ideo,
       width = 7,
       height = 6)

###-------------- Fig. S72: Causal forest estimates by altruism ---------
p_altruism <-
  plot_df %>%
  select(W, tau_hat, ci_upper, ci_lower, x_altruism_n) %>%
  gather(key = "X", value = "value", x_altruism_n) %>%
  ggplot(., aes(
    y = tau_hat,
    x = factor(value),
    color = value,
    fill = value
  )) +
  geom_hline(
    yintercept = 0,
    color = "black",
    lty = 2,
    size = 0.5
  ) +
  geom_point(alpha = 0.5,
             pch = 21,
             #color = "black",
             position = position_dodge2(.5)) +
  scale_fill_gradient2(
    name = "",
    labels = c("Low altruism", "", "Neutral", "", "High altruism"),
    midpoint = 3,
    mid = "grey",
    guide = "none"
  ) +
  scale_color_gradient2(
    name = "",
    labels = c("Low altruism", "", "Neutral", "", "High altruism"),
    midpoint = 3,
    mid = "grey",
    guide = "legend"
  ) +
  facet_wrap( ~ W) +
  theme_tufte() +
  labs(x = "",
       y = expression(hat(tau))) +
  theme(
    strip.text = element_text(size = 11),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = unit(c(
      t = 0.5,
      r = 0.5,
      b = 0.1,
      l = 0.1
    ), "lines"),
    legend.position = c(0.85, 0.3),
    legend.spacing = unit(0.1, 'lines'),
    legend.spacing.x = unit(0.1, 'lines'),
    legend.spacing.y = unit(0.3, 'lines'),
    legend.key = element_rect(color = NA, fill = NA),
    legend.key.size = unit(0.4, "cm")
  ) +
  guides(fill = guide_legend(override.aes = list(
    size = 2,
    alpha = 1,
    #shape = 15,
    key_glyph = "polygon3"
  )))

ggsave(filename = "grf_by_altruism.pdf",
       p_altruism,
       width = 7,
       height = 6)

###-------------- Fig. S73: Causal forest estimates by vax nationalism --------
## Vaccine nationalism: The US should cooperate with international efforts to
## provide COVID-19 vaccines to people in other countries]
p_vnat <-
  plot_df %>%
  select(W, tau_hat, ci_upper, ci_lower, x_vaxnat_n) %>%
  gather(key = "X", value = "value", x_vaxnat_n) %>%
  ggplot(., aes(
    y = tau_hat,
    x = factor(value),
    color = value,
    fill = value
  )) +
  geom_hline(
    yintercept = 0,
    color = "black",
    lty = 2,
    size = 0.5
  ) +
  geom_point(alpha = 0.5,
             pch = 21,
             # color = "black",
             position = position_dodge2(.5)) +
  scale_fill_gradient2(
    name = "",
    labels = c(
      "Low vaccine nationalism",
      "",
      "Neutral",
      "",
      "High vaccine nationalism"
    ),
    midpoint = 3,
    mid = "grey",
    guide = "none"
  ) +
  scale_color_gradient2(
    name = "",
    labels = c(
      "Low vaccine nationalism",
      "",
      "Neutral",
      "",
      "High vaccine nationalism"
    ),
    midpoint = 3,
    mid = "grey",
    guide = "legend"
  ) +
  facet_wrap( ~ W) +
  theme_tufte() +
  labs(x = "",
       y = expression(hat(tau)))  +
  theme(
    strip.text = element_text(size = 11),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = unit(c(
      t = 0.5,
      r = 0.5,
      b = 0.1,
      l = 0.1
    ), "lines"),
    legend.position = c(0.85, 0.3),
    legend.spacing = unit(0.1, 'lines'),
    legend.spacing.x = unit(0.1, 'lines'),
    legend.spacing.y = unit(0.3, 'lines'),
    legend.key = element_rect(color = NA, fill = NA),
    legend.key.size = unit(0.4, "cm")
  ) +
  guides(fill = guide_legend(override.aes = list(
    size = 2,
    alpha = 1,
    #shape = 15,
    key_glyph = "polygon3"
  )))

ggsave(filename = "grf_by_vnat.pdf",
       p_vnat,
       width = 7,
       height = 6)

###-------------- Fig. S74: Causal forest estimates by nationalism ---------
#Nationalism: I would rather be a citizen of the US than of any other
#country in the world.
p_pnat1 <-
  plot_df %>%
  select(W, tau_hat, ci_upper, ci_lower, x_patnat1_n) %>%
  gather(key = "X", value = "value", x_patnat1_n) %>%
  ggplot(., aes(
    y = tau_hat,
    x = factor(value),
    color = value,
    fill = value
  )) +
  geom_hline(
    yintercept = 0,
    color = "black",
    lty = 2,
    size = 0.5
  ) +
  geom_point(alpha = 0.5,
             pch = 21,
             #color = "black",
             position = position_dodge2(.5)) +
  scale_fill_gradient2(
    name = "",
    labels = c("Low nationalism", "", "Neutral", "", "High nationalism"),
    midpoint = 3,
    mid = "grey",
    guide = "none"
  ) +
  scale_color_gradient2(
    name = "",
    labels = c("Low nationalism", "", "Neutral", "", "High nationalism"),
    midpoint = 3,
    mid = "grey",
    guide = "legend"
  ) +
  facet_wrap( ~ W) +
  theme_tufte() +
  labs(x = "",
       y = expression(hat(tau))) +
  theme(
    strip.text = element_text(size = 11),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = unit(c(
      t = 0.5,
      r = 0.5,
      b = 0.1,
      l = 0.1
    ), "lines"),
    legend.position = c(0.85, 0.3),
    legend.spacing = unit(0.1, 'lines'),
    legend.spacing.x = unit(0.1, 'lines'),
    legend.spacing.y = unit(0.3, 'lines'),
    legend.key = element_rect(color = NA, fill = NA),
    legend.key.size = unit(0.4, "cm")
  ) +
  guides(fill = guide_legend(override.aes = list(
    size = 2,
    alpha = 1,
    #shape = 15,
    key_glyph = "polygon3"
  )))

ggsave(filename = "grf_by_pnat1.pdf",
       p_pnat1,
       width = 7,
       height = 6)

###-------------- Fig. S75: Causal forest estimates by patriotism ---------
#Patriotism: I am proud to be American.
p_pnat2 <-
  plot_df %>%
  select(W, tau_hat, ci_upper, ci_lower, x_patnat2_n) %>%
  gather(key = "X", value = "value", x_patnat2_n) %>%
  ggplot(., aes(
    y = tau_hat,
    x = factor(value),
    color = value,
    fill = value
  )) +
  geom_hline(
    yintercept = 0,
    color = "black",
    lty = 2,
    size = 0.5
  ) +
  geom_point(alpha = 0.5,
             pch = 21,
             #color = "black",
             position = position_dodge2(.5)) +
  scale_fill_gradient2(
    name = "",
    labels = c("Low patriotism", "", "Neutral", "", "High patriotism"),
    midpoint = 3,
    mid = "grey",
    guide = "none"
  ) +
  scale_color_gradient2(
    name = "",
    labels = c("Low patriotism", "", "Neutral", "", "High patriotism"),
    midpoint = 3,
    mid = "grey",
    guide = "legend"
  ) +
  facet_wrap( ~ W) +
  theme_tufte() +
  labs(x = "",
       y = expression(hat(tau))) +
  theme(
    strip.text = element_text(size = 11),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = unit(c(
      t = 0.5,
      r = 0.5,
      b = 0.1,
      l = 0.1
    ), "lines"),
    legend.position = c(0.85, 0.3),
    legend.spacing = unit(0.1, 'lines'),
    legend.spacing.x = unit(0.1, 'lines'),
    legend.spacing.y = unit(0.3, 'lines'),
    legend.key = element_rect(color = NA, fill = NA),
    legend.key.size = unit(0.4, "cm")
  ) +
  guides(fill = guide_legend(override.aes = list(
    size = 2,
    alpha = 1,
    #shape = 15,
    key_glyph = "polygon3"
  )))


ggsave(filename = "grf_by_pnat2.pdf",
       p_pnat2,
       width = 7,
       height = 6)
### ---- Table S1: balance table and tests for Reviewer 2 -----

## Covariate list:
X <-
  c(
    "x_female",
    "x_region",
    "x_race",
    "x_hhi_short",
    "x_edu",
    "x_age_cat",
    "x_employ_short",
    "x_ideo_7n",
    "x_pid_7n",
    "x_vaxnat_n",
    "x_patnat1_n",
    "x_patnat2_n",
    "x_altruism_n"
  )

## Use bal.tab() function from cobalt package and prep for latex
covs <-
  subset(norc_survey, select = X) %>%
  mutate(x_region = factor(x_region, levels = c("South",
                                                "West",
                                                "Midwest",
                                                "Northeast")),
         x_race = factor(x_race, levels = c(
           "White",
           "Hispanic",
           "Black",
           "AAPI",
           "Other"
         )))

treat <- norc_survey$Z_frame

b <-
  cobalt::bal.tab(
    treat ~ covs,
    binary = "std",
    continuous = "std",
    disp = c("means", "sds"),
    stats = c("mean.diffs"),
    s.d.denom = "Control",
    which.treat = .all
  )

b

## Extract levels for labels
row_levs <-
  rownames(b$Pair.Balance$`Inequality vs. Control`$Balance)

row_labs <-
  c(
    "Female",
    "South",
    "West",
    "Midwest",
    "Northeast",
    "White",
    "Hispanic",
    "Black",
    "AAPI",
    "Other",
    "$15,000 or less",
    "$15,000-$24,999",
    "$25,000-$34,999",
    "$35,000-$49,999",
    "$50,000-$74,999",
    "$75,000-$99,999",
    "$100,000-$149,999",
    "$150,000-$199,999",
    "$200,000 and above",
    "No high school diploma",
    "High school diploma",
    "Associate's degree",
    "Bachelor's degree",
    "Graduate degree",
    "18-24",
    "25-29",
    "30-34",
    "35-39",
    "40-44",
    "45-49",
    "50-54",
    "55-59",
    "60-64",
    "65-69",
    "70-74",
    "75+",
    "Employed",
    "Ideology",
    "Ideology <NA>",
    "Partisanship",
    "Vaccine nationalism",
    "Vaccine nationalism <NA>",
    "Nationalism",
    "Nationalism <NA>",
    "Patriotism",
    "Patriotism <NA>",
    "Altruism",
    "Altruism <NA>"
  )

## Control means / SDs
ctrl <-
  b$Pair.Balance$`Economic Benefits vs. Control`$Balance[, 1:3] %>%
  tibble() %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate(
    group = "Control",
    level = paste(row_levs),
    level = factor(level, levels = row_levs,
                   labels = row_labs)
  ) %>%
  rename(group_mean = M.0.Un,
         group_sd = SD.0.Un)

## Economic benefit means / SDs
econ <-
  b$Pair.Balance$`Economic Benefits vs. Control`$Balance[, c(1, 4:5)] %>%
  tibble() %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate(
    group = "Economic benefits",
    level = paste(row_levs),
    level = factor(level, levels = row_levs,
                   labels = row_labs)
  ) %>%
  rename(group_mean = M.1.Un,
         group_sd = SD.1.Un)

## Mutation risk
mutate <-
  b$Pair.Balance$`Mutation Risk vs. Control`$Balance[, c(1, 4:5)] %>%
  tibble() %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate(
    group = "Mutation risk",
    level = paste(row_levs),
    level = factor(level, levels = row_levs,
                   labels = row_labs)
  ) %>%
  rename(group_mean = M.1.Un,
         group_sd = SD.1.Un)

## Past success
past <-
  b$Pair.Balance$`Past Success vs. Control`$Balance[, c(1, 4:5)] %>%
  tibble() %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate(
    group = "Past success",
    level = paste(row_levs),
    level = factor(level, levels = row_levs,
                   labels = row_labs)
  ) %>%
  rename(group_mean = M.1.Un,
         group_sd = SD.1.Un)

## Vaccine Diplomacy
diplo <-
  b$Pair.Balance$`Vaccine Diplomacy vs. Control`$Balance[, c(1, 4:5)] %>%
  tibble() %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate(
    group = "Vaccine diplomacy",
    level = paste(row_levs),
    level = factor(level, levels = row_levs,
                   labels = row_labs)
  ) %>%
  rename(group_mean = M.1.Un,
         group_sd = SD.1.Un)

## Inequality
ineq <-
  b$Pair.Balance$`Inequality vs. Control`$Balance[, c(1, 4:5)] %>%
  tibble() %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate(
    group = "Global inequality",
    level = paste(row_levs),
    level = factor(level, levels = row_levs,
                   labels = row_labs)
  ) %>%
  rename(group_mean = M.1.Un,
         group_sd = SD.1.Un)

## Combine
balance_df <-
  bind_rows(ctrl, econ, mutate, past, diplo, ineq) %>%
  filter(!str_detect(level, "<NA>")) %>%
  mutate(entry = make_entry(est = group_mean, se = group_sd, p = 1)) %>%
  select(level, group, entry) %>%
  pivot_wider(names_from = group, values_from = entry)

xtable(balance_df) %>%
  print(
    include.rownames = FALSE,
    include.colnames = FALSE,
    hline.after = c(),
    only.contents = TRUE,
    type = "latex",
    file = "bal_tab_persuade_exp.tex"
  )


## Covariate list:
X <-
  c(
    "x_female",
    "x_region",
    "x_race",
    "x_hhi_short",
    "x_edu",
    "x_age_cat",
    "x_employ_short",
    "x_ideo_7n",
    "x_pid_7n",
    "x_vaxnat_n",
    "x_patnat1_n",
    "x_patnat2_n",
    "x_altruism_n"
  )

balance_fun <- function(data) {
  fm <-
    as.formula(paste("as.integer(Z)", "~", paste(X, collapse = "+")))
  summary(lm_robust(fm, data = data))$fstatistic[1]
}

declaration <-
  declare_ra(
    N = nrow(norc_survey),
    num_arms = 6,
    simple = TRUE,
    conditions = c(
      "Control",
      "Inequality",
      "Past Success",
      "Mutation Risk",
      "Economic Benefits",
      "Vaccine Diplomacy"
    )
  )

tmp <- norc_survey %>% mutate(Z = Z_frame) %>% data.frame()

set.seed(123)
ri_balance <-
  conduct_ri(
    test_function = balance_fun,
    declaration = declaration,
    sharp_hypothesis = 0,
    data =  tmp,
    sims = 5000
  )

summary(ri_balance)






