##################################################################################
# Agrawal and Chen (2026)
#
# Descriptions: Main analysis. 
# Produces 14 figures and 1 table. 
# (Figure 2, 3, 4, 5, 6, 7, A5, A6, A7, A8, A9, A11, A12, B1, and Table B1)
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
library(xtable)

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
     
     # married: missing if both you and your spouse's migration status are missing
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
   
   is_retiree = case_when(
     # primary taxpayer is retiree
     # if has positive retirement income, is out of the labor force, and is over 64
     (p1incretir > 0) & (p1incretir != 999999) & p1empstat == 3 & p1age > 64 ~ 1,
     .default = 0
   ),
   
   p2_is_retiree = case_when(
     # missing if primary taxpayer not married
     marst != 1 ~ NA,
     # equals to 1 if married and spouse has positive retirement income, is out of the labor force, and is over 64
     marst == 1 & (p2incretir > 0) & (p2incretir != 999999) & p2empstat == 3 & p2age > 64 ~ 1,
     .default = 0
   ),
   
   is_retiree_hh = case_when(
     # equals 1 if primary taxpayer not married and is retiree
     marst != 1 & is_retiree == 1 ~ 1,
     # equals 1 if primary taxpayer is married and both are retirees
     marst == 1 & is_retiree == 1 & p2_is_retiree == 1 ~ 1,
     .default = 0
   ),
   
   # construct federal AGI income bins
   real_fagi_bins = cut(
     real_fagi,
     breaks = c(25000, 50000, 75000, 100000, 250000, 500000, Inf),
     right  = FALSE,
     labels = c(
       "[$25k-$50k)",
       "[$50k-$75k)",
       "[$75k-$100k)",
       "[$100k-$250k)",
       "[$250k-$500k)",
       "[$500k+)"
     )
   ),
   
   group = case_when(
     any_telework == TRUE ~ "t", # teleworkers
     any_telework == FALSE ~ "nt", # non teleworkers
     .default = NA
   ),
 
   # non-interstate movers have zero astr changes
   astr_diff = ifelse(any_moved_interstate == 0, 0, astr_diff)
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


# Common layers for combined plots ------------------------------------------
common_layers_combined <- list(
  geom_point(
    color = "black", 
    position = position_dodge(width = 0.8),
    stroke = 1.1
  ),
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    color = "black",
    position = position_dodge(width = 0.8),
    width = 0.3,
    linewidth = 0.9,
    show.legend = FALSE
  ),
  geom_hline(yintercept = 0, linetype = "dashed"),
  scale_x_continuous(
    name = NULL, 
    breaks = seq(2016, 2024, by = 1),
    guide = guide_axis(angle = 45)
  ),
  scale_size_manual(
    values = c(
      "[$25k-$50k)" = 3.5,
      "[$50k-$75k)" = 2,
      "[$75k-$100k)" = 3,
      "[$100k-$250k)" = 2,
      "[$250k-$500k)" = 3,
      "[$500k+)" = 2.5
    )
  ),
  scale_shape_manual(
    values = c(
      "[$25k-$50k)" = 18,   # solid diamond
      "[$50k-$75k)" = 5,  #  hollow diamond
      "[$75k-$100k)" = 17,   # solid triangle
      "[$100k-$250k)" = 2, # hollow triangle
      "[$250k-$500k)" = 16, # filled circle
      "[$500k+)" = 1 # hollow circle
    )
  ),
  scale_alpha_manual(
    values = c(
      "[$25k-$50k)" = 0.5,
      "[$50k-$75k)" = 0.5,
      "[$75k-$100k)" = 0.8,
      "[$100k-$250k)" = 0.8,
      "[$250k-$500k)" = 1,
      "[$500k+)" = 1
    )
  ),
  labs(
    alpha = "AGI bins",
    shape = "AGI bins",
    size = "AGI bins"
  ),
  guides(
    alpha = guide_legend(nrow = 1, byrow = TRUE),
    shape = guide_legend(nrow = 1, byrow = TRUE),
    size  = guide_legend(nrow = 1, byrow = TRUE)
  ),
  theme_classic(),
  theme(
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.ticks = element_line(color = "black", linewidth = 2),
    axis.title.y  = element_text(size = 18),
    legend.title  = element_text(size = 18),
    legend.text = element_text(size = 18),
    legend.key.height = unit(2, "lines"),   
    legend.spacing.y  = unit(0.5, "cm"),
    legend.box = "horizontal"
  )
)

# ! First, include everyone ! ----------------------------------

## Create samples ----------------------------------------------------

# Notes: here the samples include everyone regardless of migration status

# All non-teleworkers: at least one is employed, no one teleworks 
nonteleworkers_all <-  cleaned_all_4plot |> 
  filter((p1empstat == 1 | p2empstat == 1), any_telework == FALSE)

# teleworkers: at least one teleworks
teleworkers_all <-  cleaned_all_4plot |> filter(any_telework == TRUE) 

# sample 3: retirees
retirees_all <- cleaned_all_4plot |> 
  filter(is_retiree_hh == 1)


# Restrict to all movers ------------------------------------------------------
cleaned_all_movers_4plot <- cleaned_all_4plot |> 
  filter(any_moved == 1)

# Merge in sales tax ------------------------------------------------------
##1. Construct cleaned sales tax dataset -------------------------------------

# read in sales tax data
raw_sales_tax <- read_dta("data/salestax/final/salestaxrates.dta") |> clean_names()

sales_tax <- raw_sales_tax |> 
  select(
    year, state_fips, combined_tax_rate
  ) |> 
  rename(combined_sales_tax_rate = combined_tax_rate) |> 
  filter(year >= 2016, year <= 2024) |> 
  # convert the sales tax rates to pp
  mutate(
    combined_sales_tax_rate = combined_sales_tax_rate * 100
  )



## 2 Create one sales tax dataset for origin and one for destination states --------

cleaned_sales_tax_origin <- sales_tax |> 
  rename_with(
    ~ paste0("origin_", .x), 
    .cols = c(
      state_fips,
      combined_sales_tax_rate
    )
  )

cleaned_sales_tax_dest <- sales_tax |> 
  rename_with(
    ~ paste0("dest_", .x), 
    .cols = c(
      state_fips,
      combined_sales_tax_rate
    )
  )


## 3 Merge with cleaned_all_4plot --------------------------------------------
cleaned_all_movers_4plot_wsales <- cleaned_all_movers_4plot |> 
  left_join(cleaned_sales_tax_origin, by = c("year", "origin_state_fips")) |> 
  left_join(cleaned_sales_tax_dest, by = c("year", "dest_state_fips")) |> 
  mutate(
    # create measure of sales tax differences b/t destination and origin state
    combined_sales_tax_diff = dest_combined_sales_tax_rate - origin_combined_sales_tax_rate
  )


# Merge in property tax ---------------------------------------------------

## 1. Complete property tax dataset (fill in 2020) ------------------------

proptax <- read_dta("data/propertytax/proptax.dta") |> 
  select(year, fips, proptax = tax, proptax_mort = tax_mort)


### 1.1 Fill in 2020 data (interpolate between 2019 and 2021) --------------------------------------------

proptax_2019 <- proptax |> 
  filter(year == 2019) |> 
  pivot_wider(
    names_from = year,
    values_from = c(proptax, proptax_mort)
  )

proptax_2021 <- proptax |> 
  filter(year == 2021) |> 
  pivot_wider(
    names_from = year,
    values_from = c(proptax, proptax_mort)
  )

proptax_2020 <- proptax_2019 |> 
  left_join(proptax_2021) |> 
  mutate(
    year = 2020,
    # property tax rates for 2020 is just the average between 2019 and 2021
    proptax = (proptax_2019 + proptax_2021) / 2,
    proptax_mort = (proptax_mort_2019 + proptax_mort_2021) / 2
  ) |> 
  select(fips, year, proptax, proptax_mort)

# fill in 2020
proptax <- proptax |> 
  bind_rows(proptax_2020) |> 
  # change rates to pp
  mutate(
    proptax = proptax*100,
    proptax_mort = proptax_mort*100
  )


### 1.2 Create one prop tax dataset for origin and one for destination --------------------------------------------

# read in the puma level data on mean household value
puma_d <- read_csv("data/derived_ipums/puma_proptax_hh_vars.csv") 

puma_d_origin <- puma_d |> 
  rename(origin_fips = dest_fips)

# one migpuma can correspond to multiple pumas
proptax_origin <- proptax |> 
  rename_with(
    ~ paste0("origin_", .x), 
    .cols = c(
      fips, proptax, proptax_mort
    )
  ) |> 
  # left_join(migpuma_puma_xwalk, by = c("year","origin_fips")) |> 
  left_join(puma_d_origin, by = c("year", "origin_fips"))


proptax_migpuma_origin <- proptax_origin |> 
  # compute puma-income-age level property tax dollar value
  # two measures, one for proptax, and one for proptax_mort
  mutate(
    origin_mean_proptax_dollar = origin_proptax / 100 * mean_valueh,
    origin_mean_proptax_mort_dollar = origin_proptax_mort / 100 * mean_valueh
  ) |> 
  # For migpumas that correspond to multiple pumas,
  # 1) impute the prop tax rates as the weighted mean by income-age across pumas (two measures)
  # 2) impute the prop tax dollar values as the weighted mean by income-age across pumas (two measures)
  group_by(year, origin_migpuma_fips, valueh_fagi_bins, age_bins) |> 
  summarise(
    #imputed origin prop tax with weights
    imputed_origin_proptax = weighted.mean(origin_proptax, w = n_hh, na.rm = TRUE),
    imputed_origin_proptax_mort = weighted.mean(origin_proptax_mort, w = n_hh, na.rm = TRUE),
    # imputed prop tax dollar amounts, with weights
    imputed_mean_origin_proptax_dollar = weighted.mean(origin_mean_proptax_dollar, w = n_hh, na.rm = TRUE),
    imputed_mean_origin_proptax_mort_dollar = weighted.mean(origin_mean_proptax_mort_dollar, w = n_hh, na.rm = TRUE),
    # mean house value
    mean_origin_valueh = weighted.mean(mean_valueh, w = n_hh, na.rm = TRUE),
    .groups = "drop"
  ) 


# destination state is more straightforward
# don't need to do much
proptax_dest <- proptax |> 
  rename_with(
    ~ paste0("dest_", .x), 
    .cols = c(
      fips, proptax, proptax_mort
    )
  )

#xwalk, merge in mean house value
puma_d_dest <- puma_d |> 
  select(year, dest_fips, valueh_fagi_bins, age_bins, mean_valueh)

# compute prop tax dollar amount
proptax_dest <- proptax_dest |> 
  left_join(puma_d_dest) |> 
  mutate(
    mean_dest_proptax_dollar = dest_proptax / 100 * mean_valueh,
    mean_dest_proptax_dollar_mort = dest_proptax_mort / 100 * mean_valueh
  ) |> 
  rename(mean_dest_valueh = mean_valueh)

# Create expenditure share by income group --------------------------------

# create agi groups by year
fagi_q_by_year <- cleaned_all_4plot |>
  group_by(year) |>
  summarise(
    fagi_q20_ub  = wtd.quantile(fagi, weights = hhwt, probs = 0.20, na.rm = TRUE),
    fagi_q40_ub  = wtd.quantile(fagi, weights = hhwt, probs = 0.40, na.rm = TRUE),
    fagi_q60_ub  = wtd.quantile(fagi, weights = hhwt, probs = 0.60, na.rm = TRUE),
    fagi_q80_ub  = wtd.quantile(fagi, weights = hhwt, probs = 0.80, na.rm = TRUE),
    fagi_q95_ub  = wtd.quantile(fagi, weights = hhwt, probs = 0.95, na.rm = TRUE),
    fagi_q99_ub  = wtd.quantile(fagi, weights = hhwt, probs = 0.99, na.rm = TRUE)
  )

# sales tax as share of family income data
shares_raw <- read_excel("data/salestax/ITEP_WhoPays7_Data_Jan2024rev.xlsx", sheet = "SE_GST", skip = 4) |> 
  clean_names()

shares <- shares_raw |> 
  filter(state == "Average Across All States")

shares_long <- shares |>
  select(-state) |>
  pivot_longer(
    cols = everything(),
    names_to = "income_group",
    values_to = "share"
  ) |> 
  mutate(
    share = share * 100 # convert to pp
  )

# ave combined sales tax rate by year
national_avg_sales_tax <- sales_tax |> 
  group_by(year) |> 
  summarise(
    national_avg_sales_tax_rate = weighted.mean(combined_sales_tax_rate)
  )

##2.  Merge with cleaned_all_movers_4plot_3taxes --------------------------------------------------------
cleaned_all_movers_4plot_3taxes <- cleaned_all_movers_4plot_wsales |> 
  # construct fips+puma geo ids for origin place and destination place
  mutate(
    # make origin_fips into a string with 7 characters (first 2 is the state fips code, then the remaining 5 is the puma code)
    origin_migpuma_fips = origin_state_fips*100000 + migpuma1,
    origin_migpuma_fips = as.character(origin_migpuma_fips),
    # if state code is only 1 character, add a "0" in front of it
    origin_migpuma_fips = ifelse(str_count(origin_migpuma_fips) < 7, paste0("0", origin_migpuma_fips), origin_migpuma_fips),
    # same thing for destination geo id
    dest_fips = dest_state_fips*100000 + puma,
    dest_fips = as.character(dest_fips),
    dest_fips = ifelse(str_count(dest_fips) < 7, paste0("0", dest_fips), dest_fips),
    # clean house value variable
    # missing coded as 9999999
    valueh_nomiss = ifelse(valueh == 9999999, NA_real_, valueh), # replace 9999999 with NAs
    
    # AGI income bins 
    valueh_fagi_bins = cut(
      real_fagi,
      breaks = c(-Inf, 25000, 50000, 75000, 100000, 250000, 500000, Inf),
      right  = FALSE,
      labels = c(
        "[<$25k)",
        "[$25k-$50k)",
        "[$50k-$75k)",
        "[$75k-$100k)",
        "[$100k-$250k)",
        "[$250k-$500k)",
        "[$500k+)"
      )
    ),
    
    # Age bins
    age_bins = cut(
      p1age,
      breaks = c(-Inf, 25, 45, 65, Inf),
      right = FALSE,
      labels = c(
        "<25",
        "[25-45)",
        "[45-65)",
        "[65+)"
      )
    )
  ) |> 
  left_join(proptax_migpuma_origin, by = c("year", "origin_migpuma_fips", "valueh_fagi_bins", "age_bins")) |> 
  left_join(proptax_dest, by = c("year", "dest_fips", "valueh_fagi_bins", "age_bins")) |> 
  mutate(
    # two measures of property tax rate differences
    proptax_diff = dest_proptax - imputed_origin_proptax,
    proptax_mort_diff = dest_proptax_mort - imputed_origin_proptax_mort,
    
    # two measures of property tax dollar differences
    proptax_dollar_diff = mean_dest_proptax_dollar - imputed_mean_origin_proptax_dollar,
    proptax_dollar_mort_diff = mean_dest_proptax_dollar_mort - imputed_mean_origin_proptax_mort_dollar,
                             
  ) |> 
  left_join(fagi_q_by_year, by = "year") |> 
  mutate(
    income_group = case_when(
      fagi <= fagi_q20_ub ~ "lowest_20_percent",
      fagi > fagi_q20_ub & fagi <= fagi_q40_ub ~ "second_20_percent",
      fagi > fagi_q40_ub & fagi <= fagi_q60_ub ~ "middle_20_percent",
      fagi > fagi_q60_ub & fagi <= fagi_q80_ub ~ "fourth_20_percent",
      fagi > fagi_q80_ub & fagi <= fagi_q95_ub ~ "next_15_percent",
      fagi > fagi_q95_ub & fagi <= fagi_q99_ub ~ "next_4_percent",
      fagi > fagi_q99_ub ~ "top_1_percent",
      .default = NA
    )
  ) |> 
  left_join(shares_long, by = "income_group") |> 
  left_join(national_avg_sales_tax, by = "year") |> 
  mutate(
    exp_share = share / national_avg_sales_tax_rate
  ) |> 
  # total tax savings
  mutate(
    astr_tax_dollar_diff = dest_stax - origin_stax,
    sales_tax_dollar_diff = combined_sales_tax_diff/100*exp_share*fagi,
    # sum of state income tax change (in dollars) + property tax change in dollars + (change in sales tax rate)*expenditure share*AGI
    total_tax_diff = astr_tax_dollar_diff + proptax_dollar_diff + sales_tax_dollar_diff,
    
    # convert to rate
    total_tax_rate_diff = total_tax_diff / fagi * 100
  )


# ! Create two samples ! --------------------------------------------------


## 1) three samples of interstate mover -------------------------------------------

# all interstate movers
all_interstate_movers <- cleaned_all_movers_4plot_3taxes |> 
  filter(any_moved_interstate == 1)

# non-teleworkers: at least one is employed, no one teleworks 
nonteleworkers_interstate_movers <- all_interstate_movers |> 
  filter((p1empstat == 1 | p2empstat == 1), any_telework == FALSE)


# teleworkers: at least one teleworks
teleworkers_interstate_movers <- all_interstate_movers |> 
  filter(any_telework == TRUE) 

# retirees
retirees_interstate_movers <- all_interstate_movers |> 
  filter(is_retiree_hh == 1)


### Samples w/o convenience -----------------------
convene_interstate_id <- read.csv("data/derived_ipums/drop_convenience_interstate_commuter_id.csv") |> 
  mutate(
    origin_migpuma_fips = as.character(migpuma_fips),
    origin_migpuma_fips = ifelse(str_count(origin_migpuma_fips) < 7, paste0("0", origin_migpuma_fips), origin_migpuma_fips)
  ) |> 
  select(year, origin_migpuma_fips, frac_interstate_work, convenience_state_itself, convenience_state_work)

convene_interstate_id |> 
  distinct(year)

all_interstate_movers_no_convene <- all_interstate_movers |> 
  left_join(convene_interstate_id, by = c("year", "origin_migpuma_fips")) |> 
  # drop if lived in a convenience state, or in a puma with a high share of 
  # commuters to a convenience state
  mutate(
    drop_convene = (convenience_state_itself == 1 | convenience_state_work == 1)
  ) 

# What fraction of people get dropped?
# 22%
all_interstate_movers_no_convene |> 
  group_by(year) |> 
  summarise(
    weighted.mean(drop_convene, w = hhwt, na.rm = TRUE)
  )

# non-teleworkers: at least one is employed, no one teleworks 
nonteleworkers_interstate_movers_no_convene <- all_interstate_movers_no_convene |> 
  filter(drop_convene==0) |> 
  filter((p1empstat == 1 | p2empstat == 1), any_telework == FALSE)


# teleworkers: at least one teleworks
teleworkers_interstate_movers_no_convene <- all_interstate_movers_no_convene |> 
  filter(drop_convene==0) |>  
  filter(any_telework == TRUE)



## 2) three samples of all movers (for alt proptax graphs in appendix) -------------------------------------------

# non-teleworkers: at least one is employed, no one teleworks 
nonteleworkers_all_movers <- cleaned_all_movers_4plot_3taxes |> 
  filter((p1empstat == 1 | p2empstat == 1), any_telework == FALSE)

# teleworkers: at least one teleworks
teleworkers_all_movers <- cleaned_all_movers_4plot_3taxes |> 
  filter(any_telework == TRUE) 

# retirees
retirees_all_movers <- cleaned_all_movers_4plot_3taxes |> 
  filter(is_retiree_hh == 1)


# Graph Type 1: Move probability-------------------------------------

### Interstate ----------------------------------------------------

####Figure 2: Teleworkers vs non teleworkers----------------------------------------------------

p_nontele_interstate_move_prob <- compute_wtmean_se(nonteleworkers_all, 
                                                    income_bins = real_fagi_bins,
                                                    tax_var     = any_moved_interstate) |> 
  ggplot(aes(x = year, y = mean, alpha = real_fagi_bins, shape = real_fagi_bins, size = real_fagi_bins)) +
  common_layers_combined +
  scale_y_continuous(
    name = "Probability of moving interstate",
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 0.075),
    breaks = seq(0, 0.075, 0.01)
  ) +
  labs(title = "(a) Non-Teleworkers") 


p_tele_interstate_move_prob <- compute_wtmean_se(teleworkers_all, 
                                                 income_bins = real_fagi_bins,
                                                 tax_var     = any_moved_interstate) |> 
  ggplot(aes(x = year, y = mean, alpha = real_fagi_bins, shape = real_fagi_bins, size = real_fagi_bins)) +
  common_layers_combined +
  scale_y_continuous(
    name = "Probability of moving interstate",
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 0.075),
    breaks = seq(0, 0.075, 0.01)
  ) +
  labs(title = "(b) Teleworkers") 



# combine the two for comparisons
combined_interstate_move_prob <- p_nontele_interstate_move_prob + p_tele_interstate_move_prob +
  plot_layout(ncol = 2, guides = "collect") &
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.position = "bottom"
  ) 



ggsave("output/figure_2.png", combined_interstate_move_prob, width = 13.5, height = 7)
ggsave("output/figure_2.tiff", combined_interstate_move_prob, width = 13.5, height = 7, dpi = 300)



#### Retirees ----------------------------------------------------

p_retirees_interstate_move_prob <- compute_wtmean_se(retirees_all, 
                                                     income_bins = real_fagi_bins,
                                                     tax_var     = any_moved_interstate) |> 
  ggplot(aes(x = year, y = mean, alpha = real_fagi_bins, shape = real_fagi_bins, size = real_fagi_bins)) +
  common_layers_combined +
  scale_y_continuous(
    name = "Probability of moving interstate",
    labels = scales::percent_format(accuracy = 1),
    limits = c(-0.01, 0.16),
    breaks = seq(0, 0.16, 0.02)
  ) +
  labs(title = "(a) Interstate") 



### Both inter + within state ----------------------------------------------------

#### Figure A5: Teleworkers vs non teleworkers ----------------------------------------------------
p_nontele_move_prob <- compute_wtmean_se(nonteleworkers_all, 
                                         income_bins = real_fagi_bins,
                                         tax_var     = any_moved) |> 
  ggplot(aes(x = year, y = mean, alpha = real_fagi_bins, shape = real_fagi_bins, size = real_fagi_bins)) +
  common_layers_combined +
  scale_y_continuous(
    name = "Probability of moving",
    labels = scales::percent_format(accuracy = 1),
    limits = c(0.02, 0.3),
    breaks = seq(0.02, 0.3, 0.04)
  ) +
  labs(title = "(a) Non-Teleworkers") 


p_tele_move_prob <- compute_wtmean_se(teleworkers_all, 
                                      income_bins = real_fagi_bins,
                                      tax_var     = any_moved) |> 
  ggplot(aes(x = year, y = mean, alpha = real_fagi_bins, shape = real_fagi_bins, size = real_fagi_bins)) +
  common_layers_combined +
  scale_y_continuous(
    name = "Probability of moving",
    labels = scales::percent_format(accuracy = 1),
    limits = c(0.02, 0.3),
    breaks = seq(0.02, 0.3, 0.04)
  ) +
  labs(title = "(b) Teleworkers") 


# combine the two for comparisons
combined_move_prob <- p_nontele_move_prob + p_tele_move_prob +
  plot_layout(ncol = 2, guides = "collect") &
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.position = "bottom"
  ) 


ggsave("output/figure_A5.png", combined_move_prob, width = 13.5, height = 7, dpi = 300)
ggsave("output/figure_A5.tiff", combined_move_prob, width = 13.5, height = 7, dpi = 300)


#### Retirees ----------------------------------------------------

p_retirees_move_prob <- compute_wtmean_se(retirees_all, 
                                          income_bins = real_fagi_bins,
                                          tax_var     = any_moved) |> 
  ggplot(aes(x = year, y = mean, alpha = real_fagi_bins, shape = real_fagi_bins, size = real_fagi_bins)) +
  common_layers_combined +
  scale_y_continuous(
    name = "Probability of moving",
    labels = scales::percent_format(accuracy = 1),
    limits = c(-0.01, 0.16),
    breaks = seq(0, 0.16, 0.02)
  ) +
  labs(title = "(b) Interstate + Within-State") 


## Figure A6: Combine interstate + all move for retirees ------------------------------

combined_retirees_move_prob <- p_retirees_interstate_move_prob + p_retirees_move_prob +
  plot_layout(ncol = 2, guides = "collect") &
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.position = "bottom"
  )

ggsave("output/figure_A6.png", combined_retirees_move_prob, width = 13.5, height = 7, dpi = 300)
ggsave("output/figure_A6.tiff", combined_retirees_move_prob, width = 13.5, height = 7, dpi = 300)


teleworkers_all_new <- teleworkers_all |> 
  filter(origin_state_fips <=56)

check_exact_new <- teleworkers_all_new |>
  group_by(year, real_fagi_bins) |>
  summarise(
    # overall mean of astr_diff
    mean_all_astr = sum(hhwt * astr_diff, na.rm = TRUE) / sum(hhwt[!is.na(astr_diff)]),
    
    # P(move) using same rows as above (important!)
    p_move = sum(hhwt * any_moved_interstate, na.rm = TRUE) / sum(hhwt[!is.na(any_moved_interstate)]),
    
    # conditional mean among movers
    mean_astr_movers =
      sum(hhwt * astr_diff * (any_moved_interstate == 1), na.rm = TRUE) /
      sum(hhwt * (any_moved_interstate == 1), na.rm = TRUE),
    
    implied = p_move * mean_astr_movers,
    diff = mean_all_astr - implied,
    .groups = "drop"
  )

check_exact_old <- teleworkers_all |>
  group_by(year, real_fagi_bins) |>
  summarise(
    # overall mean of astr_diff
    mean_all_astr = sum(hhwt * astr_diff, na.rm = TRUE) / sum(hhwt[!is.na(astr_diff)]),
    
    # P(move) using same rows as above (important!)
    p_move = sum(hhwt * any_moved_interstate, na.rm = TRUE) / sum(hhwt[!is.na(any_moved_interstate)]),
    
    # conditional mean among movers
    mean_astr_movers =
      sum(hhwt * astr_diff * (any_moved_interstate == 1), na.rm = TRUE) /
      sum(hhwt * (any_moved_interstate == 1), na.rm = TRUE),
    
    implied = p_move * mean_astr_movers,
    diff = mean_all_astr - implied,
    .groups = "drop"
  )


# Graph Type 2: ASTR-------------------------------------

# alpha for interstate movers
d_alpha <- compute_wtmean_se(teleworkers_interstate_movers, 
                  income_bins = real_fagi_bins,
                  tax_var     = astr_diff) |> 
  select(year, real_fagi_bins, alpha = mean)

# interstate move prob
d_p <- compute_wtmean_se(teleworkers_all, 
                         income_bins = real_fagi_bins,
                         tax_var     = any_moved_interstate)|> 
  select(year, real_fagi_bins, p = mean)


# alpha for all movers
d_gamma <- compute_wtmean_se(teleworkers_all, 
                             income_bins = real_fagi_bins,
                             tax_var     = astr_diff) |> 
  select(year, real_fagi_bins, gamma = mean)


d_m <- d_alpha |> 
  left_join(d_p) |> 
  left_join(d_gamma) |> 
  mutate(
    imputed = alpha*p,
    diff = imputed - gamma
  )

### Figure 3: Teleworkers vs non teleworkers --------------------------------------------------------------

# (employed) non-teleworkers
p_nonteleworkers_astr <- compute_wtmean_se(nonteleworkers_interstate_movers, 
                          income_bins = real_fagi_bins,
                          tax_var     = astr_diff) |> 
  ggplot(aes(x = year, y = mean, alpha = real_fagi_bins, shape = real_fagi_bins, size = real_fagi_bins)) +
  common_layers_combined +
  labs(title = "(a) Non-Teleworkers") +
  scale_y_continuous(
    name = "Change in avg. state income tax rate (pp)",
    limits = c(-3.5, 1),
    breaks = seq(-3.5, 1, 0.5)
  ) 


# teleworkers
p_teleworkers_astr <- compute_wtmean_se(teleworkers_interstate_movers, 
                        income_bins = real_fagi_bins,
                        tax_var     = astr_diff) |> 
  ggplot(aes(x = year, y = mean, alpha = real_fagi_bins, shape = real_fagi_bins, size = real_fagi_bins)) +
  common_layers_combined +
  labs(title = "(b) Teleworkers") +
  scale_y_continuous(
    name = "Change in avg. state income tax rate (pp)",
    limits = c(-3.5, 1),
    breaks = seq(-3.5, 1, 0.5)
  )

# combine the two for comparisons
combined_astr <- p_nonteleworkers_astr + p_teleworkers_astr +
  plot_layout(ncol = 2, guides = "collect") &
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.position = "bottom"
    )

ggsave("output/figure_3.png", combined_astr, width = 13.5, height = 7, dpi = 300)
ggsave("output/figure_3.tiff", combined_astr, width = 13.5, height = 7, dpi = 300)


### Retirees --------------------------------------------------------------
p_retirees_astr <- compute_wtmean_se(retirees_interstate_movers, 
                        income_bins = real_fagi_bins,
                        tax_var     = astr_diff) |> 
  ggplot(aes(x = year, y = mean, alpha = real_fagi_bins, shape = real_fagi_bins, size = real_fagi_bins)) +
  common_layers_combined +
  scale_y_continuous(
    name = "Change in avg. state income tax rate (pp)",
    limits = c(-5.5, 5),
    breaks = seq(-5.5, 5, 1.5)
  ) +
  labs(title = "(a) ATR")


# Figure A9: ASTR (dropped convenience interstate commuters)-------------------------------------
# (employed) non-teleworkers
p_nonteleworkers_astr_no_convene <- compute_wtmean_se(nonteleworkers_interstate_movers_no_convene, 
                                           income_bins = real_fagi_bins,
                                           tax_var     = astr_diff) |> 
  ggplot(aes(x = year, y = mean, alpha = real_fagi_bins, shape = real_fagi_bins, size = real_fagi_bins)) +
  common_layers_combined +
  labs(title = "(a) Non-Teleworkers") +
  scale_y_continuous(
    name = "Change in avg. state income tax rate (pp)",
    limits = c(-3.5, 1),
    breaks = seq(-3.5, 1, 0.5)
  ) 


# teleworkers
p_teleworkers_astr_no_convene <- compute_wtmean_se(teleworkers_interstate_movers_no_convene, 
                                        income_bins = real_fagi_bins,
                                        tax_var     = astr_diff) |> 
  ggplot(aes(x = year, y = mean, alpha = real_fagi_bins, shape = real_fagi_bins, size = real_fagi_bins)) +
  common_layers_combined +
  labs(title = "(b) Teleworkers") +
  scale_y_continuous(
    name = "Change in avg. state income tax rate (pp)",
    limits = c(-3.5, 1),
    breaks = seq(-3.5, 1, 0.5)
  )

# combine the two for comparisons
combined_astr_no_convene <- p_nonteleworkers_astr_no_convene + p_teleworkers_astr_no_convene +
  plot_layout(ncol = 2, guides = "collect") &
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.position = "bottom"
  )

ggsave("output/figure_A9.png", combined_astr_no_convene, width = 13.5, height = 7, dpi = 300)
ggsave("output/figure_A9.tiff", combined_astr_no_convene, width = 13.5, height = 7, dpi = 300)

# Graph Type 3: Sales Tax-------------------------------------

## Figure 4: Non-teleworkers vs teleworkers --------------------------------------------------------------

# (employed) non-teleworkers
p_nonteleworkers_sales <- compute_wtmean_se(nonteleworkers_interstate_movers, 
                                      income_bins = real_fagi_bins,
                                      tax_var     = combined_sales_tax_diff) |> 
  ggplot(aes(x = year, y = mean, alpha = real_fagi_bins, shape = real_fagi_bins, size = real_fagi_bins)) +
  common_layers_combined +
  labs(title = "(a) Non-Teleworkers") +
  scale_y_continuous(
    name = "Change in sales tax rate (pp)",
    limits = c(-1.4, 1.2),
    breaks = seq(-1.2, 1.2, 0.3)
  ) 

# teleworkers
p_teleworkers_sales <- compute_wtmean_se(teleworkers_interstate_movers, 
                                   income_bins = real_fagi_bins,
                                   tax_var     = combined_sales_tax_diff) |> 
  ggplot(aes(x = year, y = mean, alpha = real_fagi_bins, shape = real_fagi_bins, size = real_fagi_bins)) +
  common_layers_combined +
  labs(title = "(b) Teleworkers") +
  scale_y_continuous(
    name = "Change in sales tax rate (pp)",
    limits = c(-1.4, 1.2),
    breaks = seq(-1.2, 1.2, 0.3)
  )

# combine the two for comparisons
combined_sales <- p_nonteleworkers_sales + p_teleworkers_sales +
  plot_layout(ncol = 2, guides = "collect") &
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.position = "bottom"
  ) 

ggsave("output/figure_4.png", combined_sales, width = 13.5, height = 7, dpi = 300)
ggsave("output/figure_4.tiff", combined_sales, width = 13.5, height = 7, dpi = 300)


# Graph Type 4: Property Tax-------------------------------------


## 1. Interstate, property tax dollars --------------------------------------------------------------

### Figure 5: Non-teleworkers vs teleworkers --------------------------------------------------------------

p_nonteleworkers_interstate_propdollar <- compute_wtmean_se(nonteleworkers_interstate_movers, 
                                      income_bins = real_fagi_bins,
                                      tax_var     = proptax_dollar_diff) |> 
  ggplot(aes(x = year, y = mean, alpha = real_fagi_bins, shape = real_fagi_bins, size = real_fagi_bins)) +
  common_layers_combined +
  labs(title = "(a) Non-Teleworkers") +
  scale_y_continuous(
    name = "Change in property tax ($)",
    labels = label_number(scale = 1/1000, suffix = "K"),
    limits = c(-4500, 4000),
    breaks = seq(-4500, 4000, 1000)
  )


p_teleworkers_interstate_propdollar <- compute_wtmean_se(teleworkers_interstate_movers, 
                                   income_bins = real_fagi_bins,
                                   tax_var     = proptax_dollar_diff) |> 
  ggplot(aes(x = year, y = mean, alpha = real_fagi_bins, shape = real_fagi_bins, size = real_fagi_bins)) +
  common_layers_combined +
  labs(title = "(b) Teleworkers") +
  scale_y_continuous(
    name = "Change in property tax ($)",
    labels = label_number(scale = 1/1000, suffix = "K"),
    limits = c(-4500, 4000),
    breaks = seq(-4500, 4000, 1000)
  )


# combine the two for comparisons
combined_interstate_propdollar <- p_nonteleworkers_interstate_propdollar + p_teleworkers_interstate_propdollar +
  plot_layout(ncol = 2, guides = "collect") &
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.position = "bottom"
  ) 


ggsave("output/figure_5.png", combined_interstate_propdollar, width = 13.5, height = 7, dpi = 300)
ggsave("output/figure_5.tiff", combined_interstate_propdollar, width = 13.5, height = 7, dpi = 300)



## 2. Interstate + Within State, property tax dollars --------------------------------------------------------------

### Figure A12: Non-teleworkers vs teleworkers --------------------------------------------------------------

p_nonteleworkers_all_movers_propdollar <- compute_wtmean_se(nonteleworkers_all_movers, 
                                                            income_bins = real_fagi_bins,
                                                            tax_var     = proptax_dollar_diff) |> 
  ggplot(aes(x = year, y = mean, alpha = real_fagi_bins, shape = real_fagi_bins, size = real_fagi_bins)) +
  common_layers_combined +
  labs(title = "(a) Non-Teleworkers") +
  scale_y_continuous(
    name = "Change in property tax ($)",
    labels = label_number(scale = 1/1000, suffix = "K"),
    limits = c(-1500, 1700),
    breaks = seq(-1500, 1700, 500)
  )


p_teleworkers_all_movers_propdollar <- compute_wtmean_se(teleworkers_all_movers, 
                                                         income_bins = real_fagi_bins,
                                                         tax_var     = proptax_dollar_diff) |> 
  ggplot(aes(x = year, y = mean, alpha = real_fagi_bins, shape = real_fagi_bins, size = real_fagi_bins)) +
  common_layers_combined +
  labs(title = "(b) Teleworkers") +
  scale_y_continuous(
    name = "Change in property tax ($)",
    labels = label_number(scale = 1/1000, suffix = "K"),
    limits = c(-1500, 1700),
    breaks = seq(-1500, 1700, 500)
  )


# combine the two for comparisons
combined_all_movers_propdollar <- p_nonteleworkers_all_movers_propdollar + p_teleworkers_all_movers_propdollar +
  plot_layout(ncol = 2, guides = "collect") &
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.position = "bottom"
  )


ggsave("output/figure_A12.png", combined_all_movers_propdollar, width = 13.5, height = 7, dpi = 300)
ggsave("output/figure_A12.tiff", combined_all_movers_propdollar, width = 13.5, height = 7, dpi = 300)


## 3. Interstate, property tax rate --------------------------------------------------------------

### Figure A11: Non-teleworkers vs teleworkers --------------------------------------------------------------

p_nonteleworkers_interstate_proprate <- compute_wtmean_se(nonteleworkers_interstate_movers, 
                                                            income_bins = real_fagi_bins,
                                                            tax_var     = proptax_diff) |> 
  ggplot(aes(x = year, y = mean, alpha = real_fagi_bins, shape = real_fagi_bins, size = real_fagi_bins)) +
  common_layers_combined +
  labs(title = "(a) Non-Teleworkers") +
  scale_y_continuous(
    name = "Change in property tax rate (pp)",
    limits = c(-0.4, 0.2),
    breaks = seq(-0.4, 0.2, 0.1)
  )


p_teleworkers_interstate_proprate <- compute_wtmean_se(teleworkers_interstate_movers, 
                                                         income_bins = real_fagi_bins,
                                                         tax_var     = proptax_diff) |> 
  ggplot(aes(x = year, y = mean, alpha = real_fagi_bins, shape = real_fagi_bins, size = real_fagi_bins)) +
  common_layers_combined +
  labs(title = "(b) Teleworkers") +
  scale_y_continuous(
    name = "Change in property tax rate (pp)",
    limits = c(-0.4, 0.2),
    breaks = seq(-0.4, 0.2, 0.1)
  )


# combine the two for comparisons
combined_interstate_proprate <- p_nonteleworkers_interstate_proprate + p_teleworkers_interstate_proprate +
  plot_layout(ncol = 2, guides = "collect") &
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.position = "bottom"
  ) 


ggsave("output/figure_A11.png", combined_interstate_proprate, width = 13.5, height = 7, dpi = 300)
ggsave("output/figure_A11.tiff", combined_interstate_proprate, width = 13.5, height = 7, dpi = 300)


# Graph type 5: Total Tax Savings --------------------------------------------------------------

### Dollars --------------------------------------------------------------

#### Figure 6: Non-teleworkers vs teleworkers --------------------------------------------------------------

p_nonteleworkers_totaltax <- compute_wtmean_se(nonteleworkers_interstate_movers, 
                                               income_bins = real_fagi_bins,
                                               tax_var     = total_tax_diff) |> 
  ggplot(aes(x = year, y = mean, alpha = real_fagi_bins, shape = real_fagi_bins, size = real_fagi_bins)) +
  common_layers_combined +
  labs(title = "(a) Non-Teleworkers") +
  scale_y_continuous(
    name = "Change in total taxes ($)",
    labels = label_number(scale = 1/1000, suffix = "K"),
    limits = c(-20000, 5000),
    breaks = seq(-20000, 5000, 5000)
  )


p_teleworkers_totaltax <- compute_wtmean_se(teleworkers_interstate_movers, 
                                                       income_bins = real_fagi_bins,
                                                       tax_var     = total_tax_diff) |> 
  ggplot(aes(x = year, y = mean, alpha = real_fagi_bins, shape = real_fagi_bins, size = real_fagi_bins)) +
  common_layers_combined +
  labs(title = "(b) Teleworkers") +
  scale_y_continuous(
    name = "Change in total taxes ($)",
    labels = label_number(scale = 1/1000, suffix = "K"),
    limits = c(-20000, 5000),
    breaks = seq(-20000, 5000, 5000)
  )


# combine the two for comparisons
combined_totaltax <- p_nonteleworkers_totaltax + p_teleworkers_totaltax +
  plot_layout(ncol = 2, guides = "collect") &
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.position = "bottom"
  ) 


ggsave("output/figure_6.png", combined_totaltax, width = 13.5, height = 7, dpi = 300)
ggsave("output/figure_6.tiff", combined_totaltax, width = 13.5, height = 7, dpi = 300)



### Rate --------------------------------------------------------------

#### Figure 7: Non-teleworkers vs teleworkers --------------------------------------------------------------

p_nonteleworkers_totaltax_rate <- compute_wtmean_se(nonteleworkers_interstate_movers, 
                                               income_bins = real_fagi_bins,
                                               tax_var     = total_tax_rate_diff) |> 
  ggplot(aes(x = year, y = mean, alpha = real_fagi_bins, shape = real_fagi_bins, size = real_fagi_bins)) +
  common_layers_combined +
  labs(title = "(a) Non-Teleworkers") +
  scale_y_continuous(
    name = "Change in total tax rate (pp)",
    limits = c(-4, 3),
    breaks = seq(-4, 3, 1)
  )


p_teleworkers_totaltax_rate <- compute_wtmean_se(teleworkers_interstate_movers, 
                                            income_bins = real_fagi_bins,
                                            tax_var     = total_tax_rate_diff) |> 
  ggplot(aes(x = year, y = mean, alpha = real_fagi_bins, shape = real_fagi_bins, size = real_fagi_bins)) +
  common_layers_combined +
  labs(title = "(b) Teleworkers") +
  scale_y_continuous(
    name = "Change in total tax rate (pp)",
    limits = c(-4, 3),
    breaks = seq(-4, 3, 1)
  )


# combine the two for comparisons
combined_totaltax_rate <- p_nonteleworkers_totaltax_rate + p_teleworkers_totaltax_rate +
  plot_layout(ncol = 2, guides = "collect") &
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.position = "bottom"
  ) 


ggsave("output/figure_7.png", combined_totaltax_rate, width = 13.5, height = 7, dpi = 300)
ggsave("output/figure_7.tiff", combined_totaltax_rate, width = 13.5, height = 7, dpi = 300)

#### [A] Retirees ----------------------------------------------------------------
p_retirees_totaltax_rate <- compute_wtmean_se(retirees_interstate_movers, 
                                         income_bins = real_fagi_bins,
                                         tax_var     = total_tax_rate_diff) |> 
  ggplot(aes(x = year, y = mean, alpha = real_fagi_bins, shape = real_fagi_bins, size = real_fagi_bins)) +
  common_layers_combined +
  scale_y_continuous(
    name = "Change in total tax rate (pp)",
    breaks = seq(-6, 5, 1)
  ) +
  labs(title = "(b) Total Tax Rate") 



## Figure A8: Combine 2 figures (ASTR & total tax rate) for retirees ------------------------------

combined_retirees_2figs<- p_retirees_astr + p_retirees_totaltax_rate +
  plot_layout(ncol = 2, guides = "collect") &
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.position = "bottom"
  )


ggsave("output/figure_A8.png", combined_retirees_2figs, width = 13.5, height = 7, dpi = 300)
ggsave("output/figure_A8.tiff", combined_retirees_2figs, width = 13.5, height = 7, dpi = 300)


# Statutory top MTR ---------------------------------------------------------------

raw_statu_mtr <- read_dta("data/tax_statutory/final/statutory_rates.dta") 

# clean the statutory top MTR to one row per state*year; make sure the rates are in pp
cleaned_statu_top_mtr <- raw_statu_mtr |> 
  filter(year >= 2016 & year <= 2024) |> 
  select(year, state_fips, top_mtr) |> 
  distinct(year, state_fips, top_mtr) |> 
  arrange(year) |> 
  rename(
    statu_top_mtr = top_mtr
  ) |> 
  mutate(
    statu_top_mtr = statu_top_mtr * 100 # standardize tax rates to pp
  )

origin_statu_top_mtr <- cleaned_statu_top_mtr |> 
  rename(
    origin_state_fips = state_fips,
    origin_statu_top_mtr = statu_top_mtr
  )

dest_statu_top_mtr <- cleaned_statu_top_mtr |> 
  rename(
    dest_state_fips = state_fips,
    dest_statu_top_mtr = statu_top_mtr
  )

# Merge in origin and dest state MTR
# note that non movers always have origin_statu_top_mtr as missing
d_for_bloom <- cleaned_all_4plot |> 
  left_join(origin_statu_top_mtr, by = c("year", "origin_state_fips")) |> 
  left_join(dest_statu_top_mtr, by = c("year", "dest_state_fips")) |> 
  filter(group == "t") |> 
  mutate(
    top_mtr_diff = ifelse(migrate1==1, 0,
                          dest_statu_top_mtr - origin_statu_top_mtr),
    # construct federal AGI income bins
    alt_real_fagi_bins = cut(
      real_fagi,
      breaks = c(25000, 50000, 75000, 100000, 250000, Inf),
      right  = FALSE,
      labels = c(
        "[$25k-$50k)",
        "[$50k-$75k)",
        "[$75k-$100k)",
        "[$100k-$250k)",
        "[$250k+)"
      )
  )
  )

# Common layers for combined plots ------------------------------------------
common_layers_combined_bloom <- list(
  geom_point(
    color = "black", 
    position = position_dodge(width = 0.8),
    stroke = 1.1
  ),
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    color = "black",
    position = position_dodge(width = 0.8),
    width = 0.3,
    linewidth = 0.9,
    show.legend = FALSE
  ),
  geom_hline(yintercept = 0, linetype = "dashed"),
  scale_x_continuous(
    name = NULL, 
    breaks = seq(2016, 2024, by = 1),
    guide = guide_axis(angle = 45)
  ),
  scale_size_manual(
    values = c(
      "[$25k-$50k)" = 3.5,
      "[$50k-$75k)" = 2,
      "[$75k-$100k)" = 3,
      "[$100k-$250k)" = 2,
      "[$250k+)" = 3
    )
  ),
  scale_alpha_manual(
    values = c(
      "[$25k-$50k)" = 0.5,  
      "[$50k-$75k)" = 0.5,  
      "[$75k-$100k)" = 0.8,  
      "[$100k-$250k)" = 0.8,
      "[$250k+)" = 1 
    )
  ),
  scale_shape_manual(
    values = c(
      "[$25k-$50k)" = 18,   # solid diamond
      "[$50k-$75k)" = 5,  #  hollow diamond
      "[$75k-$100k)" = 17,   # solid triangle
      "[$100k-$250k)" = 2, # hollow triangle
      "[$250k+)" = 16 # filled circle
    )
  ),
  labs(
    alpha = "AGI bins",
    shape = "AGI bins",
    size = "AGI bins"
  ),
  guides(
    alpha = guide_legend(nrow = 1, byrow = TRUE),
    shape = guide_legend(nrow = 1, byrow = TRUE),
    size  = guide_legend(nrow = 1, byrow = TRUE)
  ),
  theme_classic(),
  theme(
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.ticks = element_line(color = "black", linewidth = 2),
    axis.title.y  = element_text(size = 18),
    legend.title  = element_text(size = 18),
    legend.text = element_text(size = 18),
    legend.key.height = unit(2, "lines"),   
    legend.spacing.y  = unit(0.5, "cm"),
    legend.box = "horizontal"
  )
)


# teleworkers
bloom_beta_d <- compute_wtmean_se(d_for_bloom, 
                                  income_bins = alt_real_fagi_bins,
                                  tax_var     = top_mtr_diff)  |> 
  select (-ci_low, -ci_high) |> 
  rename(
    bloom_beta_mtr = mean,
    bloom_beta_mtr_se = se
  )


## Figure A7: Replicate Bloom top MTR graph ----------------------

p_bloom_all <- compute_wtmean_se(d_for_bloom, 
                  income_bins = alt_real_fagi_bins,
                  tax_var     = top_mtr_diff) |> 
  ggplot(aes(x = year, y = mean, alpha = alt_real_fagi_bins, shape = alt_real_fagi_bins, size = alt_real_fagi_bins)) +
  common_layers_combined_bloom +
  scale_y_continuous(
    name = "Change in state MTR (pp)"
  ) +
  labs(title = "(a) All Teleworkers")


d_for_bloom_interstate_movers <- d_for_bloom |> filter(any_moved_interstate == 1)


p_bloom_interstate <- compute_wtmean_se(d_for_bloom_interstate_movers, 
                  income_bins = alt_real_fagi_bins,
                  tax_var     = top_mtr_diff) |> 
  ggplot(aes(x = year, y = mean, alpha = alt_real_fagi_bins, shape = alt_real_fagi_bins, size = alt_real_fagi_bins)) +
  common_layers_combined_bloom +
  scale_y_continuous(
    name = "Change in state MTR (pp)"
  ) +
  labs(title = "(b) Interstate Movers")


# combine the two for comparisons
combined_bloom <- p_bloom_all + p_bloom_interstate +
  plot_layout(ncol = 2, guides = "collect") &
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.position = "bottom"
  ) 


ggsave("output/figure_A7.png", combined_bloom, width = 13.5, height = 7, dpi = 300)
ggsave("output/figure_A7.tiff", combined_bloom, width = 13.5, height = 7, dpi = 300)

## Calculations ------------------------------------------------------------

# tax rate reduction for the highest income group from 2020 to 2023
bloom_beta_d |> 
  filter(year > 2019 & year < 2024, alt_real_fagi_bins == "[$250k+)") 

# sum of 4 coeffs
bloom_beta_d |> 
  filter(year > 2019 & year < 2024, alt_real_fagi_bins == "[$250k+)") |> 
  summarise(
    sum(bloom_beta_mtr)
  )

# aggregate AGI for >$250k
adjust_fagi_d <- read_csv("data/intermediate/adjust_agi_factor.csv")

adjust_fagi_d_min <- adjust_fagi_d |> 
  select(year, nom_fagi_bins, adjust_fagi_factor)

final_numbers <- cleaned_all_4plot |> 
  mutate(
    nom_fagi_bins = cut(
      fagi,
      breaks = c(-Inf, 25000, 50000, 75000, 100000, 200000, 500000, Inf),
      right  = FALSE,
      labels = c(
        "[<$25k)",
        "[$25k-$50k)",
        "[$50k-$75k)",
        "[$75k-$100k)",
        "[$100k-$200k)",
        "[$200k-$500k)",
        "[$500k+)"
      )
    ),
    
    alt_real_fagi_bins = cut(
      real_fagi,
      breaks = c(25000, 50000, 75000, 100000, 250000, Inf),
      right  = FALSE,
      labels = c(
        "[$25k-$50k)",
        "[$50k-$75k)",
        "[$75k-$100k)",
        "[$100k-$250k)",
        "[$250k+)"
      )
    )
  ) |> 
  left_join(
    adjust_fagi_d_min, by = c("year", "nom_fagi_bins")
  ) |> 
  mutate(
    adjusted_fagi = fagi*adjust_fagi_factor
  )

# adjusted aggregate AGI for >$250k
final_numbers |> 
  filter(year == 2022) |> 
  group_by(alt_real_fagi_bins) |> 
  summarise(
    sum(adjusted_fagi*hhwt) / 1000000000000
  )

# total adjusted aggregate AGI in 2022
final_numbers |> 
  filter(year == 2022) |> 
  summarise(
    sum(adjusted_fagi*hhwt) / 1000000000000
  )

# Figure B1: Plot differences --------------------------------------------------------

## Create sample for plotting diffs ----------------------------------------

sample_employed <- cleaned_all_4plot |> 
  filter(
    (p1empstat == 1 | p2empstat == 1)
    ) |> 
  mutate(
    post = ifelse(year > 2019, 1, 0)
  )

sample_employed_interstate <- all_interstate_movers |> 
  filter(
    (p1empstat == 1 | p2empstat == 1)
  ) |> 
  mutate(
    post = ifelse(year > 2019, 1, 0)
  )



# function for plotting diffs
compute_diff_tele_nontele <- function(sample, income_bins, tax_var,
                                      tele_var   = tele_dummy,
                                      weight_var = hhwt,
                                      cluster_var = state_pair_no_direct) {
  income_bins <- rlang::enquo(income_bins)
  tax_var     <- rlang::enquo(tax_var)
  tele_var    <- rlang::enquo(tele_var)
  weight_var  <- rlang::enquo(weight_var)
  cluster_var <- rlang::enquo(cluster_var)
  
  sample |>
    filter(!is.na(!!income_bins)) |>
    # one regression per AGI bin (using all years at once)
    dplyr::nest_by(!!income_bins) |>
    mutate(
      model = list(
        feols(
          # y ~ year FE + year×tele interaction
          as.formula(
            paste0(
              rlang::as_name(tax_var),
              " ~ 0 + factor(year) + factor(year):", rlang::as_name(tele_var)
            )
          ),
          data    = data,
          weights = as.formula(paste0("~", rlang::as_name(weight_var))),
          cluster = as.formula(paste0("~", rlang::as_name(cluster_var)))
        )
      ),
      results = list({
        cf <- coef(model)
        se <- sqrt(diag(vcov(model)))
        tibble::tibble(
          term = names(cf),
          coef = as.numeric(cf),
          se   = as.numeric(se)
        )
      })
    ) |>
    dplyr::select(-data, -model) |>
    tidyr::unnest(results) |>
    # keep only the teleworker–non-tele interaction terms
    # e.g. "factor(year)2016:tele_dummy"
    dplyr::filter(grepl("^factor\\(year\\)[0-9]+:", term)) |>
    dplyr::mutate(
      year = as.integer(sub("factor\\(year\\)([0-9]+):.*", "\\1", term)),
      diff   = coef,                  # tele − non-tele difference in mean
      ci_low = diff - 1.96 * se,
      ci_high = diff + 1.96 * se
    ) |>
    dplyr::select(year, !!income_bins, diff, se, ci_low, ci_high)
}



## 1. Move probability -----------------------------------------------------

p_diff_move <- 
  compute_diff_tele_nontele(
  sample      = sample_employed,
  income_bins = real_fagi_bins,
  tax_var     = any_moved_interstate,
  tele_var = any_telework
) |>
  ggplot(aes(x = year, y = diff, alpha = real_fagi_bins, shape = real_fagi_bins, size = real_fagi_bins)) +
  common_layers_combined + 
  scale_y_continuous(
    name = "Diff. in prob. of moving interstate",
    labels = scales::percent_format(accuracy = 1),
    ) +
  labs(
    title = "(a) Prob(Move Interstate)"
  )


## 2. ASTR -----------------------------------------------------

p_diff_astr <- 
  compute_diff_tele_nontele(
    sample      = sample_employed_interstate,
    income_bins = real_fagi_bins,
    tax_var     = astr_diff,
    tele_var = any_telework
  ) |>
  ggplot(aes(x = year, y = diff, alpha = real_fagi_bins, shape = real_fagi_bins, size = real_fagi_bins)) +
  common_layers_combined + 
  scale_y_continuous(
    name = "Diff. in change in ATR (pp)",
    breaks = seq(-2.5, 1, 0.5)
  ) +
  labs(
    title = "(b) Income Tax Rate"
  )


## 3. Sales -----------------------------------------------------

p_diff_sales <- 
  compute_diff_tele_nontele(
    sample      = sample_employed_interstate,
    income_bins = real_fagi_bins,
    tax_var     = combined_sales_tax_diff,
    tele_var = any_telework
  ) |>
  ggplot(aes(x = year, y = diff, alpha = real_fagi_bins, shape = real_fagi_bins, size = real_fagi_bins)) +
  common_layers_combined + 
  scale_y_continuous(
    name = "Diff. in change in sales tax rate (pp)"
  ) +
  labs(
    title = "(c) Sales Tax Rate"
  )


## 4. Property tax -----------------------------------------------------

p_diff_proptax_dollars <- 
  compute_diff_tele_nontele(
    sample      = sample_employed_interstate,
    income_bins = real_fagi_bins,
    tax_var     = proptax_dollar_diff,
    tele_var = any_telework
  ) |>
  ggplot(aes(x = year, y = diff, alpha = real_fagi_bins, shape = real_fagi_bins, size = real_fagi_bins)) +
  common_layers_combined + 
  scale_y_continuous(
    name = "Diff. in change in property tax ($)",
    labels = label_number(scale = 1/1000, suffix = "K"),
    breaks = seq(-6500, 3000, 1500)
  ) +
  labs(
    title = "(d) Property Tax ($)"
  )

## 5. Total tax dollars -----------------------------------------------------

p_diff_tot_tax_dollars <- 
  compute_diff_tele_nontele(
    sample      = sample_employed_interstate,
    income_bins = real_fagi_bins,
    tax_var     = total_tax_diff,
    tele_var = any_telework
  ) |>
  ggplot(aes(x = year, y = diff, alpha = real_fagi_bins, shape = real_fagi_bins, size = real_fagi_bins)) +
  common_layers_combined + 
  scale_y_continuous(
    name = "Diff. in change in total tax ($)",
    labels = label_number(scale = 1/1000, suffix = "K")
  ) +
  labs(
    title = "(e) Total Tax ($)"
  )


## 6. Total tax rates -----------------------------------------------------

p_diff_tot_tax_rate <- 
  compute_diff_tele_nontele(
    sample      = sample_employed_interstate,
    income_bins = real_fagi_bins,
    tax_var     = total_tax_rate_diff,
    tele_var = any_telework
  ) |>
  ggplot(aes(x = year, y = diff, alpha = real_fagi_bins, shape = real_fagi_bins, size = real_fagi_bins)) +
  common_layers_combined + 
  scale_y_continuous(
    name = "Diff. in change in total tax rate (pp)"
  ) +
  labs(
    title = "(f) Total Tax Rate"
  )

## Combine into panel -----------------------------------------------------

combined_diff_panel <- p_diff_move + p_diff_astr + p_diff_sales + 
  p_diff_proptax_dollars + p_diff_tot_tax_dollars + p_diff_tot_tax_rate +
  plot_layout(ncol = 2, nrow = 3, guides = "collect") &
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.position = "bottom"
  ) 


ggsave("output/figure_B1.png", combined_diff_panel, width = 13.5, height = 18, dpi = 300)
ggsave("output/figure_B1.tiff", combined_diff_panel, width = 13.5, height = 18, dpi = 300)


# Coefficient table -------------------------------------------------------
compute_diff_post_coeff <- function(sample, income_bins, tax_var,
                                    tele_var   = tele_dummy,
                                    weight_var = hhwt,
                                    cluster_var = state_pair_no_direct) {
  income_bins <- rlang::enquo(income_bins)
  tax_var     <- rlang::enquo(tax_var)
  tele_var    <- rlang::enquo(tele_var)
  weight_var  <- rlang::enquo(weight_var)
  cluster_var <- rlang::enquo(cluster_var)
  
  sample |>
    filter(!is.na(!!income_bins)) |>
    # one regression per AGI bin (using all years at once)
    dplyr::nest_by(!!income_bins) |>
    mutate(
      model = list(
        feols(
          # y ~ year FE + year×tele interaction
          as.formula(
            paste0(
              rlang::as_name(tax_var),
              " ~ 0 + factor(post) + factor(post):", rlang::as_name(tele_var)
            )
          ),
          data    = data,
          weights = as.formula(paste0("~", rlang::as_name(weight_var))),
          cluster = as.formula(paste0("~", rlang::as_name(cluster_var)))
        )
      ),
      results = list({
        ct <- fixest::coeftable(model)
        tibble::tibble(
          term = rownames(ct),
          coef = as.numeric(ct[, "Estimate"]),
          se   = as.numeric(ct[, "Std. Error"]),
          pval = as.numeric(ct[, "Pr(>|t|)"])
        )
      })
    ) |>
    dplyr::select(-data, -model) |>
    tidyr::unnest(results) |>
    # keep only the teleworker–non-tele interaction terms
    # e.g. "factor(year)2016:tele_dummy"
    dplyr::filter(grepl("^factor\\(post\\)[0-9]+:", term)) |>
    dplyr::mutate(
      post = as.integer(sub("factor\\(post\\)([0-9]+):.*", "\\1", term)),
      diff   = coef,                  # tele − non-tele difference in mean
    ) |>
    dplyr::select(post, !!income_bins, diff, se, pval)
}

# function for adding significance stars
sig_stars <- function(p) {
  dplyr::case_when(
    is.na(p)      ~ "",
    p < 0.01      ~ "***",
    p < 0.05      ~ "**",
    p < 0.10      ~ "*",
    TRUE          ~ ""
  )
}

coeff_moved <- compute_diff_post_coeff(
  sample      = sample_employed,
  income_bins = real_fagi_bins,
  tax_var     = any_moved_interstate,
  tele_var = any_telework
) |> 
  mutate(
    panel = "Prob(Moved Interstate)"
  )

coeff_astr <-compute_diff_post_coeff(
  sample      = sample_employed_interstate,
  income_bins = real_fagi_bins,
  tax_var     = astr_diff,
  tele_var = any_telework
) |> 
  mutate(
    panel = "Income Tax Rate (pp)"
  )

coeff_sales <- compute_diff_post_coeff(
  sample      = sample_employed_interstate,
  income_bins = real_fagi_bins,
  tax_var     = combined_sales_tax_diff,
  tele_var = any_telework
) |> 
  mutate(
    panel = "Sales Tax Rate (pp)"
  )

coeff_proptax <- compute_diff_post_coeff(
  sample      = sample_employed_interstate,
  income_bins = real_fagi_bins,
  tax_var     = proptax_dollar_diff,
  tele_var = any_telework
) |> 
  mutate(
    panel = "Property Tax ($)"
  )

coeff_total_tax <- compute_diff_post_coeff(
  sample      = sample_employed_interstate,
  income_bins = real_fagi_bins,
  tax_var     = total_tax_diff,
  tele_var = any_telework
) |> 
  mutate(
    panel = "Total Tax ($)"
  )

coeff_total_tax_rate <- compute_diff_post_coeff(
  sample      = sample_employed_interstate,
  income_bins = real_fagi_bins,
  tax_var     = total_tax_rate_diff,
  tele_var = any_telework
) |> 
  mutate(
    panel = "Total Tax Rate (pp)"
  )  

all_coef_pre_and_post <- bind_rows(
  coeff_moved,
  coeff_astr,
  coeff_sales,
  coeff_proptax,
  coeff_total_tax,
  coeff_total_tax_rate
)  |> 
  ungroup() |> 
  mutate(
    # rounding
    diff_fmt = paste0(sprintf("%.3f", diff), sig_stars(pval)),
    se_fmt   = sprintf("%.3f", se),
  )


all_coef_pre <- all_coef_pre_and_post |> 
  filter(post==0) |> 
  select(panel, diff_fmt, se_fmt, real_fagi_bins) |> 
  pivot_longer(
    cols = c("diff_fmt", "se_fmt"),
    names_to = "coeffs"
  ) |> 
  pivot_wider(
    names_from = "real_fagi_bins",
    values_from = value
  ) |> 
  mutate(
    panel = ifelse(coeffs == "se_fmt", "", panel)
  ) |> 
  rename(Outcome = panel)

all_coef_post <- all_coef_pre_and_post |> 
  filter(post==1) |> 
  select(panel, diff_fmt, se_fmt, real_fagi_bins) |> 
  pivot_longer(
    cols = c("diff_fmt", "se_fmt"),
    names_to = "coeffs"
  ) |> 
  pivot_wider(
    names_from = "real_fagi_bins",
    values_from = value
  ) |> 
  mutate(
    panel = ifelse(coeffs == "se_fmt", "", panel)
  ) |> 
  rename(Outcome = panel)

make_panel <- function(df, panel_title) {
  header <- df[1, , drop = FALSE]   # keep as 1-row tibble
  header[,] <- ""                  # blank out every cell
  header$Outcome <- panel_title    # put the panel title in first column
  header$coeffs  <- ""             # keep coeffs column empty
  
  dplyr::bind_rows(header, df)
}

all_coef_panel <- dplyr::bind_rows(
  make_panel(all_coef_pre,  "Panel A: Pre (2016--2019)"),
  make_panel(all_coef_post, "Panel B: Post (2020--2024)")
)



names(all_coef_panel) <- str_replace_all(names(all_coef_panel),
                                   fixed("$"),
                                   "\\$")

all_coef_panel <- all_coef_panel |> 
  mutate(Outcome = str_replace_all(Outcome, fixed("$"), "\\$"))


wrap_paren <- function(se) {
  paste0("(", se, ")")
}

all_coef_panel <- all_coef_panel |> 
  mutate(across(`[\\$25k-\\$50k)`:`[\\$500k+)`,
                ~ if_else(coeffs == "se_fmt", wrap_paren(.x), .x))) |> 
  select(-coeffs) 

indent <- "\\hspace{1em}"

all_coef_panel <- all_coef_panel |>
  mutate(
    Outcome = dplyr::case_when(
      stringr::str_detect(Outcome, "^Panel") ~ Outcome,                 # keep header
      Outcome == ""                          ~ Outcome,                 # keep blank SE rows
      TRUE                                   ~ paste0(indent, Outcome)  # indent outcomes
    )
  )


d_for_table <- all_coef_panel |> as.data.frame()

panel_rows <- which(grepl("^Panel", d_for_table$Outcome))

addtorow <- list(
  pos = as.list(panel_rows),
  command = rep("\\cmidrule(lr){1-1}\n", length(panel_rows))
)

table <- xtable(
  d_for_table,
  type = "latex",
  align   = c("l", "l", "l", rep("c", ncol(all_coef_panel) - 2))
)

print(
  table,
  type = "latex",
  size = "scriptsize",
  include.rownames = FALSE,
  booktabs = TRUE,
  sanitize.text.function = identity,
  sanitize.colnames.function = identity,
  floating = FALSE,
  comment = FALSE,
  add.to.row = addtorow,
  file = "output/table_B1.tex"
)


# Precise numbers ---------------------------------------------------------

## Intro ---------------------------------------------------------

# ASTR change for >$500k post-covid
compute_wtmean_se(nonteleworkers_interstate_movers, 
                  income_bins = real_fagi_bins,
                  tax_var     = astr_diff) |> 
  filter(year > 2019, real_fagi_bins == "[$500k+)")

compute_wtmean_se(teleworkers_interstate_movers, 
                  income_bins = real_fagi_bins,
                  tax_var     = astr_diff) |> 
  filter(year > 2019, real_fagi_bins == "[$500k+)")


# ASTR change for $100k to $250k post-covid
compute_wtmean_se(nonteleworkers_interstate_movers, 
                  income_bins = real_fagi_bins,
                  tax_var     = astr_diff) |> 
  filter(year > 2019, real_fagi_bins == "[$100k-$250k)")

compute_wtmean_se(teleworkers_interstate_movers, 
                  income_bins = real_fagi_bins,
                  tax_var     = astr_diff) |> 
  filter(year > 2019, real_fagi_bins == "[$100k-$250k)")

# Max tax savings for non-teleworkers post-covid
compute_wtmean_se(nonteleworkers_interstate_movers, 
                  income_bins = real_fagi_bins,
                  tax_var     = total_tax_diff) |> 
  filter(year > 2019, real_fagi_bins == "[$500k+)") 

# % of AGI = 5566/637158 = 0.87
nonteleworkers_interstate_movers |> 
  filter(year == 2022, real_fagi_bins == "[$500k+)") |> 
  summarise(
    weighted.mean(fagi, w = hhwt)
  )

# Max tax savings for teleworkers post-covid
compute_wtmean_se(teleworkers_interstate_movers, 
                  income_bins = real_fagi_bins,
                  tax_var     = total_tax_diff) |> 
  filter(year > 2019, real_fagi_bins == "[$500k+)") 

# % of AGI = 12970/600303
teleworkers_interstate_movers |> 
  filter(year == 2021, real_fagi_bins == "[$500k+)") |> 
  summarise(
    weighted.mean(fagi, w = hhwt)
  )

compute_wtmean_se(nonteleworkers_interstate_movers, 
                  income_bins = real_fagi_bins,
                  tax_var     = total_tax_diff) |> 
  filter(year > 2019, real_fagi_bins == "[$100k-$250k)")


# % of AGI = 608/127417
nonteleworkers_interstate_movers |> 
  filter(year == 2021, real_fagi_bins == "[$100k-$250k)") |> 
  summarise(
    weighted.mean(fagi, w = hhwt)
  )

compute_wtmean_se(teleworkers_interstate_movers, 
                  income_bins = real_fagi_bins,
                  tax_var     = total_tax_diff) |> 
  filter(year > 2019, real_fagi_bins == "[$100k-$250k)")

# % of AGI = 1225/136412
teleworkers_interstate_movers |> 
  filter(year == 2021, real_fagi_bins == "[$100k-$250k)") |> 
  summarise(
    weighted.mean(fagi, w = hhwt)
  )


# interstate migration rate post-covid, pooling all income bins 
post_interstate_migration_rate <- feols(
  any_moved_interstate ~ 0 + post:any_telework,
  data = sample_employed,
  weights = ~ hhwt,
  cluster = ~ state_pair_no_direct
)

summary(post_interstate_migration_rate)

## Num of movers -----------------------------------------------------------

# non teleworkers, 2023
compute_wtmean_se(nonteleworkers_all, 
                  income_bins = real_fagi_bins,
                  tax_var     = any_moved_interstate) |> 
  filter(year == 2024) 


# teleworkers, 2023
compute_wtmean_se(teleworkers_all, 
                  income_bins = real_fagi_bins,
                  tax_var     = any_moved_interstate) |> 
  filter(year == 2024) 


## Property tax ------------------------------------------------------------

compute_wtmean_se(nonteleworkers_interstate_movers, 
                  income_bins = real_fagi_bins,
                  tax_var     = proptax_dollar_diff) |> 
  filter(year == 2021)

compute_wtmean_se(teleworkers_interstate_movers, 
                  income_bins = real_fagi_bins,
                  tax_var     = proptax_dollar_diff) |> 
  filter(year == 2021)



## Total tax ---------------------------------------------------------------

# expenditure share for income groups in 2021
# the same if include everyone, not just movers, bc the ratio is binned
cleaned_all_movers_4plot_3taxes |> 
  filter(year == 2021) |> 
  group_by(year, real_fagi_bins) |>
  summarise(weighted.mean(exp_share, w = hhwt, na.rm=TRUE))


# check total dollars
compute_wtmean_se(teleworkers_interstate_movers, 
                  income_bins = real_fagi_bins,
                  tax_var     = total_tax_diff) |> 
  filter(year == 2021)

compute_wtmean_se(nonteleworkers_interstate_movers, 
                  income_bins = real_fagi_bins,
                  tax_var     = total_tax_diff) |> 
  filter(year == 2021)

# check total tax rates
compute_wtmean_se(teleworkers_interstate_movers, 
                  income_bins = real_fagi_bins,
                  tax_var     = total_tax_rate_diff) |> 
  filter(year == 2021)

compute_wtmean_se(nonteleworkers_interstate_movers, 
                  income_bins = real_fagi_bins,
                  tax_var     = total_tax_rate_diff) |> 
  filter(year == 2021)
