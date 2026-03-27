##################################################################################
# Agrawal and Chen (2026)
#
# Descriptions: Simulates income tax revenue losses from telework. 
# Produces Figure 9 and Figure A13.
# 
##################################################################################
# Clear environment
rm(list = ls())

library(tidyverse)
library(janitor)
library(purrr)
library(fixest)
library(rlang)
library(ipumsr)
library(haven)
library(patchwork)
library(scales)
library(ggthemes)
library(Hmisc)
library(readxl)

# Read in cleaned ACS + taxsim output data for all tax unit ---------------------------

# already non-institutionalized, non-farm, 50 states + DC
cleaned_all <- read_csv("data/derived_ipums/cleaned_all_wATR.csv")


# General cleaning --------------------------------------------------------
## add essential dummies ---------------------------------------------------

# add a dummy for identifying teleowrk tax units
# add dummys for interstate move & overall move
cleaned_all_4plot <- cleaned_all |> 
  mutate(
    # add dummy for if anyone in the couple telework
    any_telework = case_when(
      # married: true if you or spouse telework
      (marst == 1) & ((p1tranwork == 80 | p2tranwork == 80)) ~ TRUE,
      # not married: true if you telework
      (marst != 1) & (p1tranwork == 80) ~ TRUE,
      # if not married: missing if tranwork missing
      (marst != 1) & (p1tranwork == 0) ~ NA,
      
      # otherwise false
      .default = FALSE
    ), 
    
    # moved interstate dummy
    moved_interstate = case_when(
      migrate1 == 0 ~ NA,
      migrate1 == 3 ~ 1,
      .default = 0
    ),
    
    # moved both within and interstate dummy
    moved = case_when(
      migrate1 == 0 ~ NA,
      migrate1 %in% c(2, 3) ~ 1,
      .default = 0
    ),
    
    # spouse moved interstate dummy
    p2moved_interstate = case_when(
      is.na(p2migrate1) ~ NA,
      p2migrate1 == 0 ~ NA,
      p2migrate1 == 3 ~ 1,
      .default = 0
    ),
    
    # spouse moved both within and interstate dummy
    p2moved = case_when(
      is.na(p2migrate1) ~ NA,
      p2migrate1 == 0 ~ NA,
      p2migrate1 %in% c(2, 3) ~ 1,
      .default = 0
    ),
    
    # any one in the HH moved interstate dummy
    any_moved_interstate = case_when(
      # married: true if you or your spouse moved interstate
      (marst == 1) & (moved_interstate == 1 | p2moved_interstate == 1) ~ 1,
      
      # not married: true if you moved interstate
      (marst != 1) & (moved_interstate == 1) ~ 1,
      
      # married: missing if both you are your spouse's migration status are missing
      (marst == 1) & (is.na(moved_interstate) & is.na(p2moved_interstate)) ~ NA,
      
      # not married: missing if your migration status is missing
      (marst != 1) & is.na(moved_interstate) ~ NA,
      
      .default = 0
    ),
    
    # any one in the HH moved dummy
    any_moved = case_when(
      # married: true if you or your spouse moved 
      (marst == 1) & (moved == 1 | p2moved == 1) ~ 1,
      
      # not married: true if you moved interstate
      (marst != 1) & (moved == 1) ~ 1,
      
      # married: missing if both you are your spouse's migration status are missing
      (marst == 1) & (is.na(moved) & is.na(p2moved)) ~ NA,
      
      # not married: missing if your migration status is missing
      (marst != 1) & is.na(moved) ~ NA,
      
      .default = 0
    ),
    
    # construct federal AGI income bins
    real_fagi_bins = cut(
      real_fagi,
      breaks = c(25000, 50000, 75000, 100000, 250000, 500000, Inf),
      right  = FALSE,
      labels = c(
        "[$25k–$50k)",
        "[$50k–$75k)",
        "[$75k–$100k)",
        "[$100k-$250k)",
        "[$250k-$500k)",
        "[$500k+)"
      )
    ),
    
    # variables for revenume simulation
    group = case_when(
      any_telework == TRUE ~ "t", # teleworkers
      any_telework == FALSE ~ "nt", # non teleworkers
      .default = NA
    )
  )


# Define function and plot common layers ----------------------------------

# function that computes the weighted mean and standard errors
# same method as in Akan et al (2025)
compute_wtmean_se <- function(sample, income_bins, tax_var,
                              weight_var = hhwt, cluster_var = state_pair_no_direct) {
  # capture column names as quosures
  income_bins <- enquo(income_bins)
  tax_var     <- enquo(tax_var)
  weight_var  <- enquo(weight_var)
  cluster_var <- enquo(cluster_var)
  
  sample |>
    filter(!is.na(!!income_bins)) |> # drop missing bins
    # fit the regression for each income bin separately
    nest_by(!!income_bins) |>
    mutate(
      # one regression per income bin, with a full set of year dummies
      model = list(
        feols(
          as.formula(paste0(as_name(tax_var), " ~ 0 + factor(year)")),
          data    = data,
          weights = as.formula(paste0("~", as_name(weight_var))),
          cluster = as.formula(paste0("~", as_name(cluster_var)))
        )
      ),
      # turn model into a tibble of year-specific coefficients & SEs
      results = list({
        cf <- coef(model)
        se <- sqrt(diag(vcov(model)))
        tibble(
          term = names(cf),
          mean = as.numeric(cf),
          se   = as.numeric(se)
        )
      })
    ) |>
    select(-data, -model) |>
    unnest(results) |>
    # extract numeric year from the term "factor(year)YYYY"
    mutate(
      year   = as.integer(gsub("factor\\(year\\)", "", term)),
      ci_low = mean - 1.96 * se,
      ci_high = mean + 1.96 * se
    ) |>
    select(year, !!income_bins, mean, se, ci_low, ci_high)
}

# Revenue simulations ------------------------------------------------------

# Goal is to construct a cleaned dataset with year-incbin-group level variables:
# 1) number of people
# 2) average fagi
# 3) coeff from ASTR graph
# 4) coeff from probability of moving graph

# read in fagi adjustment factor
adjust_fagi_d <- read_csv("data/intermediate/adjust_agi_factor.csv")


adjust_fagi_d_min <- adjust_fagi_d |> 
  select(year, nom_fagi_bins, adjust_fagi_factor)


rev_sim_d <- cleaned_all_4plot |> 
  mutate(
    imputed_origin_state_fips = ifelse(migrate1==1, dest_state_fips, origin_state_fips)
  ) |> 
  mutate(
    nom_fagi_bins = cut(
      fagi,
      breaks = c(-Inf, 25000, 50000, 75000, 100000, 200000, 500000, Inf),
      right  = FALSE,
      labels = c(
        "[<$25k)",
        "[$25k–$50k)",
        "[$50k–$75k)",
        "[$75k–$100k)",
        "[$100k–$200k)",
        "[$200k–$500k)",
        "[$500k+)"
      )
    )
  ) |> 
  left_join(
    adjust_fagi_d_min, by = c("year", "nom_fagi_bins")
  ) |> 
  mutate(
    adjusted_fagi = fagi*adjust_fagi_factor
  )


# population, average agi, and pre-migration astr in each year by income bin
n_z_t <- rev_sim_d |>
  # restrict to households with at least one individual employed
  filter(
    (p1empstat == 1 | p2empstat == 1)
  ) |>
  filter(
    !is.na(any_telework), !is.na(real_fagi_bins)
  ) |> 
  group_by(year, real_fagi_bins, group) |> 
  summarise(
    N = sum(hhwt),
    z_bar = weighted.mean(adjusted_fagi, w = hhwt),
    z_bar_unadjusted = weighted.mean(fagi, w = hhwt),
    .groups = "drop"
  ) |> 
  pivot_wider(
    names_from = "group",
    values_from = c("N", "z_bar", "z_bar_unadjusted")
  )



# all teleworkers
# for estimating alpha
rev_sim_d_t <- rev_sim_d |> 
  # restrict to households with at least one individual employed
  filter(
    (p1empstat == 1 | p2empstat == 1)
  ) |>
  filter(
    !is.na(any_telework), !is.na(real_fagi_bins)
  ) |> 
  filter(group == "t") 

# all non teleworkers
# for estimating alpha
rev_sim_d_nt <- rev_sim_d |> 
  # restrict to households with at least one individual employed
  filter(
    (p1empstat == 1 | p2empstat == 1)
  ) |>
  filter(
    !is.na(any_telework), !is.na(real_fagi_bins)
  ) |> 
  filter(group == "nt") 


## alpha -------------------------------------------------------------------

# get alpha and se for teleworkers
alpha_t_d <- compute_wtmean_se(rev_sim_d_t, 
                               income_bins = real_fagi_bins,
                               tax_var     = any_moved_interstate) |> 
  select(-ci_low, -ci_high) |> 
  rename(
    alpha_t = mean,
    alpha_t_se = se
  )

# get alpha for non teleworkers
alpha_nt_d <- compute_wtmean_se(rev_sim_d_nt, 
                                income_bins = real_fagi_bins,
                                tax_var     = any_moved_interstate) |> 
  select(-ci_low, -ci_high) |> 
  rename(
    alpha_nt = mean,
    alpha_nt_se = se
  )


## beta on ASTR, interstate movers -------------------------------------------------

# all telework interstate movers
# for estimating beta
rev_sim_d_t_interstate_movers <- rev_sim_d_t |> 
  filter(any_moved_interstate == 1)

# all nontelework interstate movers
# for estimating beta
rev_sim_d_nt_interstate_movers <- rev_sim_d_nt |> 
  filter(any_moved_interstate == 1)

# get beta on ASTR for teleworkers
beta_astr_t_d <- compute_wtmean_se(rev_sim_d_t_interstate_movers, 
                                   income_bins = real_fagi_bins,
                                   tax_var     = astr_diff)  |> 
  select (-ci_low, -ci_high) |> 
  rename(
    beta_astr_t = mean,
    beta_astr_t_se = se
  )


# get beta on ASTR for non teleworkers
beta_astr_nt_d <- compute_wtmean_se(rev_sim_d_nt_interstate_movers, 
                                    income_bins = real_fagi_bins,
                                    tax_var     = astr_diff)  |> 
  select(-ci_low, -ci_high) |> 
  rename(
    beta_astr_nt = mean,
    beta_astr_nt_se = se
  )



# total state income tax revenue in each year 
agg_stax_d <- rev_sim_d |> 
  #drop people with fagi below 25k
  filter(fagi>25000) |> 
  group_by(year, nom_fagi_bins) |> 
  summarise(
    mean_astr = weighted.mean(origin_stax / fagi, w = hhwt, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  left_join(adjust_fagi_d) |> 
  mutate(
    adjusted_origin_stax = acs_agg_fagi*adjust_fagi_factor*mean_astr
    ) |> 
  group_by(year) |> 
  summarise(
    agg_stax = sum(adjusted_origin_stax),
    .groups = "drop"
  )

agg_stax_unadjusted_d <- rev_sim_d |> 
  #drop people with fagi below 25k
  filter(fagi>25000) |> 
  group_by(year) |> 
  summarise(
    agg_stax_unadjusted = sum(origin_stax*hhwt),
    .groups = "drop"
  ) 

# merge
m_rev_sim_d <- n_z_t |> 
  left_join(alpha_t_d, by = c("year", "real_fagi_bins")) |> 
  left_join(alpha_nt_d, by = c("year", "real_fagi_bins")) |> 
  left_join(beta_astr_t_d, by = c("year", "real_fagi_bins")) |> 
  left_join(beta_astr_nt_d, by = c("year", "real_fagi_bins")) |> 
  left_join(agg_stax_d, by = "year") |> 
  left_join(agg_stax_unadjusted_d, by = "year")


# effect for ASTR
delta_R_d <- m_rev_sim_d |> 
  mutate(
    # adjusted
    revenue_change = N_t*z_bar_t*((beta_astr_t/100) * alpha_t - (beta_astr_nt/100) * alpha_nt),
    revenue_change_nc = N_t*z_bar_t*((beta_astr_t/100) * alpha_t),
    
    # unadjusted
    revenue_change_unadjusted = N_t*z_bar_unadjusted_t*((beta_astr_t/100) * alpha_t - (beta_astr_nt/100) * alpha_nt),
    revenue_change_unadjusted_nc = N_t*z_bar_unadjusted_t*((beta_astr_t/100) * alpha_t)
  ) |> 
  group_by(year, agg_stax, agg_stax_unadjusted) |> 
  summarise(
    delta_R = sum(revenue_change),
    delta_R_nc = sum(revenue_change_nc),
    
    delta_R_unadjusted = sum(revenue_change_unadjusted),
    delta_R_unadjusted_nc = sum(revenue_change_unadjusted_nc),
    
    .groups = "drop"
  ) |> 
  mutate(
    frac_delta_R = delta_R / agg_stax * 100,
    frac_delta_R_nc = delta_R_nc / agg_stax * 100,
    
    frac_delta_R_unadjusted = delta_R_unadjusted / agg_stax_unadjusted * 100,
    frac_delta_R_unadjusted_nc = delta_R_unadjusted_nc / agg_stax_unadjusted * 100
  ) 


## Compute and plot --------------------------------------------------------

# Parametric bootstrap ----------------------------------------------------

set.seed(123)
B <- 100000  # number of draws

# 1) Keep only what we need
base <- m_rev_sim_d |> 
  select(
    year, real_fagi_bins, N_t, 
    z_bar_t, z_bar_unadjusted_t,
    beta_astr_t, beta_astr_t_se,
    beta_astr_nt, beta_astr_nt_se,
    alpha_t, alpha_t_se,
    alpha_nt, alpha_nt_se,
    agg_stax, agg_stax_unadjusted
  )

# 2) Draw coefficients and recompute delta_R for each draw
sims <- base  |> 
  crossing(draw = 1:B)  |> 
  mutate(
    # draw betas for astr
    beta_t_draw  = rnorm(n(), mean = beta_astr_t, sd = beta_astr_t_se),
    beta_nt_draw = rnorm(n(), mean = beta_astr_nt, sd = beta_astr_nt_se),
    
  
    # draw alphas (probabilities) and clip to [0,1]
    alpha_t_draw  = pmin(pmax(rnorm(n(), mean = alpha_t, sd = alpha_t_se),  0), 1),
    alpha_nt_draw = pmin(pmax(rnorm(n(), mean = alpha_nt, sd = alpha_nt_se), 0), 1),
    
    # bin-level contribution to delta_R under this draw
    # astr
    
    # adjusted
    contrib_delta_R = N_t*z_bar_t*((beta_t_draw/100)*alpha_t_draw - (beta_nt_draw/100) * alpha_nt_draw),
    contrib_delta_R_nc = N_t*z_bar_t*((beta_t_draw/100)*alpha_t_draw),
    
    contrib_frac_delta_R = contrib_delta_R / agg_stax * 100,
    contrib_frac_delta_R_nc = contrib_delta_R_nc / agg_stax * 100,
    
    # unadjusted
    contrib_delta_R_unadjusted = N_t*z_bar_unadjusted_t*((beta_t_draw/100)*alpha_t_draw - (beta_nt_draw/100) * alpha_nt_draw),
    contrib_delta_R_unadjusted_nc = N_t*z_bar_unadjusted_t*((beta_t_draw/100)*alpha_t_draw),
    
    contrib_frac_delta_R_unadjusted = contrib_delta_R_unadjusted / agg_stax_unadjusted * 100,
    contrib_frac_delta_R_unadjusted_nc = contrib_delta_R_unadjusted_nc / agg_stax_unadjusted * 100
    
  ) |> 
  group_by(year, draw) |> 
  # add up delta_R across income bins for each draw by year
  summarise(
    # adjusted
    delta_R = sum(contrib_delta_R), 
    frac_delta_R = sum(contrib_frac_delta_R), 
    
    delta_R_nc = sum(contrib_delta_R_nc), 
    frac_delta_R_nc = sum(contrib_frac_delta_R_nc),
    
    # unadjusted
    delta_R_unadjusted = sum(contrib_delta_R_unadjusted), 
    frac_delta_R_unadjusted = sum(contrib_frac_delta_R_unadjusted), 
    
    delta_R_unadjusted_nc = sum(contrib_delta_R_unadjusted_nc), 
    frac_delta_R_unadjusted_nc = sum(contrib_frac_delta_R_unadjusted_nc),
    
    .groups = "drop")

# 3) CI by year from the simulated distribution
deltaR_ci <- sims |> 
  group_by(year) |> 
  summarise(
    # ASTR
    
    # adjusted
    ci_low  = quantile(delta_R, 0.025),
    ci_high = quantile(delta_R, 0.975),
    frac_ci_low  = quantile(frac_delta_R, 0.025),
    frac_ci_high = quantile(frac_delta_R, 0.975),
    
    ci_low_nc  = quantile(delta_R_nc, 0.025),
    ci_high_nc = quantile(delta_R_nc, 0.975),
    frac_ci_low_nc  = quantile(frac_delta_R_nc, 0.025),
    frac_ci_high_nc = quantile(frac_delta_R_nc, 0.975),
    
    # unadjusred
    frac_ci_low_unadjusted  = quantile(frac_delta_R_unadjusted, 0.025),
    frac_ci_high_unadjusted = quantile(frac_delta_R_unadjusted, 0.975),
    
    frac_ci_low_unadjusted_nc  = quantile(frac_delta_R_unadjusted_nc, 0.025),
    frac_ci_high_unadjusted_nc = quantile(frac_delta_R_unadjusted_nc, 0.975),
    
    .groups = "drop"
  )

# merge in CI and total state income tax payment
delta_R_d_ci <- delta_R_d  |> 
  left_join(deltaR_ci, by = "year")

delta_R_d_ci |> filter(year==2024) |> glimpse()

## Figure 9: Graph on rev loss in shares, ASTR ---------------------------------------

p_rev_change_astr <-  delta_R_d |> 
  left_join(deltaR_ci, by = "year") |> 
  ggplot(aes(x = factor(year), y = frac_delta_R)) +
  geom_col(
    data = delta_R_d |> 
      filter(year <= 2019),
    fill = "grey70"
  ) +
  geom_col(
    data = delta_R_d |> 
      filter(year > 2019),
    fill = "grey30"
  ) +
  geom_errorbar(aes(ymin = frac_ci_low, ymax = frac_ci_high), linewidth = 0.8, width = 0.2) +
  geom_hline(yintercept = 0, linewidth = 0.7) +
  labs(
    x = NULL,
    y = "Change in state income tax revenue (%)",
  ) +
  scale_x_discrete(
    guide = guide_axis(angle = 45)
  ) +
  scale_y_continuous(
    limits = c(-0.65, 0.05),
    breaks = seq(-0.6, 0, 0.2)
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.ticks = element_line(color = "black", linewidth = 2),
    axis.title.y  = element_text(size = 18),
  ) +
  labs(
    title = "(a) With Counterfactual"
  )


## NC: Graph on rev loss in shares, ASTR ---------------------------------------

p_rev_change_astr_nc <- delta_R_d |> 
  left_join(deltaR_ci, by = "year") |> 
  ggplot(aes(x = factor(year), y = frac_delta_R_nc)) +
  geom_col(
    data = delta_R_d |> 
      filter(year <= 2019),
    fill = "grey70"
  ) +
  geom_col(
    data = delta_R_d |> 
      filter(year > 2019),
    fill = "grey30"
  ) +
  geom_errorbar(aes(ymin = frac_ci_low_nc, ymax = frac_ci_high_nc), linewidth = 0.8, width = 0.2) +
  geom_hline(yintercept = 0, linewidth = 0.7) +
  labs(
    x = NULL,
    y = "Change in state income tax revenue (%)",
  ) +
  scale_x_discrete(
    guide = guide_axis(angle = 45)
  ) +
  scale_y_continuous(
    limits = c(-0.65, 0.05),
    breaks = seq(-0.6, 0, 0.2)
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.ticks = element_line(color = "black", linewidth = 2),
    axis.title.y  = element_text(size = 18),
  ) +
  labs(
    title = "(b) Without Counterfactual"
  )


combined <- p_rev_change_astr + p_rev_change_astr_nc +
  plot_layout(ncol = 2, guides = "collect") &
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.position = "bottom"
  ) 

ggsave("output/figure_9.png", combined, width = 13.5, height = 6, dpi = 300)
ggsave("output/figure_9.tiff", combined, width = 13.5, height = 6, dpi = 300)


## Figure A13: Unadjusted --------------------------------------------------------------

### With Counterfactual --------------------------------------------------------------
p_rev_change_astr_na <- delta_R_d |> 
  left_join(deltaR_ci, by = "year") |> 
  ggplot(aes(x = factor(year), y = frac_delta_R_unadjusted)) +
  geom_col(
    data = delta_R_d |> 
      filter(year <= 2019),
    fill = "grey70"
  ) +
  geom_col(
    data = delta_R_d |> 
      filter(year > 2019),
    fill = "grey30"
  ) +
  geom_errorbar(aes(ymin = frac_ci_low_unadjusted, ymax = frac_ci_high_unadjusted), linewidth = 0.8, width = 0.2) +
  geom_hline(yintercept = 0, linewidth = 0.7) +
  labs(
    x = NULL,
    y = "Change in state income tax revenue (%)",
  ) +
  scale_x_discrete(
    guide = guide_axis(angle = 45)
  ) +
  scale_y_continuous(
    limits = c(-0.4, 0.01),
    breaks = seq(-0.4, 0, 0.1)
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.ticks = element_line(color = "black", linewidth = 2),
    axis.title.y  = element_text(size = 18),
  ) +
  labs(
    title = "(a) With Counterfactual"
  )



### No Counterfactual --------------------------------------------------------------
p_rev_change_astr_na_nc <- delta_R_d |> 
  left_join(deltaR_ci, by = "year") |> 
  ggplot(aes(x = factor(year), y = frac_delta_R_unadjusted_nc)) +
  geom_col(
    data = delta_R_d |> 
      filter(year <= 2019),
    fill = "grey70"
  ) +
  geom_col(
    data = delta_R_d |> 
      filter(year > 2019),
    fill = "grey30"
  ) +
  geom_errorbar(aes(ymin = frac_ci_low_unadjusted_nc, ymax = frac_ci_high_unadjusted_nc), linewidth = 0.8, width = 0.2) +
  geom_hline(yintercept = 0, linewidth = 0.7) +
  labs(
    x = NULL,
    y = "Change in state income tax revenue (%)",
  ) +
  scale_x_discrete(
    guide = guide_axis(angle = 45)
  ) +
  scale_y_continuous(
    limits = c(-0.4, 0.01),
    breaks = seq(-0.4, 0, 0.1)
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.ticks = element_line(color = "black", linewidth = 2),
    axis.title.y  = element_text(size = 18),
  ) +
  labs(
    title = "(b) Without Counterfactual"
  )

combined_na <- p_rev_change_astr_na + p_rev_change_astr_na_nc +
  plot_layout(ncol = 2, guides = "collect") &
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.position = "bottom"
  ) 

ggsave("output/figure_A13.png", combined_na, width = 13.5, height = 6, dpi = 300)
ggsave("output/figure_A13.tiff", combined_na, width = 13.5, height = 6, dpi = 300)


# Precise number ----------------------------------------------------------

# aggregate over 4 pre-covid years
delta_R_d_ci |> 
  filter(year < 2020) |> 
  summarise(
    sum_agg_stax = sum(agg_stax),
    sum_delta_R = sum(delta_R),
    share = sum_delta_R / sum_agg_stax * 100
  )

# aggregate over 4 post-covid years
delta_R_d_ci |> 
  filter(year > 2019 & year < 2024) |> 
  summarise(
    sum_agg_stax = sum(agg_stax),
    sum_delta_R = sum(delta_R),
    share = sum_delta_R / sum_agg_stax * 100
  )

# most pessimistic aggregate rev loss over 4 post-covid years
delta_R_d_ci |> 
  filter(year > 2019 & year < 2024) |> 
  summarise(
    sum_agg_stax = sum(agg_stax),
    sum_delta_R_low = sum(ci_low),
    share = sum_delta_R_low / sum_agg_stax * 100
  )

# aggregate over 5 post-covid years
delta_R_d_ci |> 
  filter(year > 2019) |> 
  summarise(
    sum_agg_stax = sum(agg_stax),
    sum_delta_R = sum(delta_R),
    share = sum_delta_R / sum_agg_stax * 100
  )

# most pessimistic aggregate rev loss over 5 post-covid years
delta_R_d_ci |> 
  filter(year > 2019) |> 
  summarise(
    sum_agg_stax = sum(agg_stax),
    sum_delta_R_low = sum(ci_low),
    share = sum_delta_R_low / sum_agg_stax * 100
  )

# precise numbers for simulated rev losses as a share
delta_R_d |> 
  select(year, frac_delta_R)
