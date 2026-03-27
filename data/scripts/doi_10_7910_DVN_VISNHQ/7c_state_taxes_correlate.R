##################################################################################
# Agrawal and Chen (2026)
#
# Descriptions: Correlates state sales taxes with average ATR measures.
# Produces Figure 8.
# 
##################################################################################

# Clear environment
rm(list = ls())

library(tidyverse)
library(janitor)
library(purrr)
library(ggrepel)
library(rvest)
library(fixest)
library(haven)
library(rlang)
library(ipumsr)
library(patchwork)
library(readxl)
library(ggthemes)

# data on income taxes ----------------------------------------------------

repre_taxsim_out_16_23 <- read_dta("data/intermediate/taxsim_output/year_state_incomebin_avg_taxsim_out_16_23.dta")

repre_taxsim_out_24 <- read_dta("data/intermediate/taxsim_output/year_state_incomebin_avg_taxsim_out_24.dta") |> 
  mutate(year = 2024)

repre_taxsim_out <- rbind(repre_taxsim_out_16_23, repre_taxsim_out_24)

incbin_xwalk <- read_csv("data/xwalk/year_state_incomebin_atr_taxsimid_xwalk.csv")

repre_atr <- repre_taxsim_out |> 
  # merge on income bin
  left_join(incbin_xwalk) |> 
  select(year, state, real_fagi_bins, fiitax, siitax, v10) |> 
  mutate(
    astr = (siitax) / v10 * 100,
    atr = (fiitax + siitax) / v10 * 100
    )

soi_fips_xwalk <- read_csv("data/xwalk/irs_soi_fips_crosswalk.csv") |> 
  clean_names() |> 
  select(state = irs_soi_code, state_fip = fips_code)

repre_atr <- repre_atr |> 
  # add fips
  left_join(soi_fips_xwalk) |> 
  # remove soi state code
  select(-state)

atr_subset <- repre_atr |> 
  filter(real_fagi_bins == ">$25k" | real_fagi_bins == ">$100k") |> 
  select(year, real_fagi_bins, astr, state_fips = state_fip)
  

# load state name and abbrev/fips xwalk
xwalk <- read_excel("data/xwalk/StateFIPSicsprAB.xls") |>
  clean_names() |> 
  select(state_region = name, state_abbrev = ab, state_fips = fips)

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


# Data on sales taxes -----------------------------------------------------

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


# Merge---------------------------------------------------------------------

m <- atr_subset |> 
  left_join(cleaned_statu_top_mtr) |> 
  left_join(sales_tax) |> 
  left_join(xwalk)


# Figure A10 ---------------------------------------------------------------------

p_atr_2019 <- m |> 
  filter(year==2019, real_fagi_bins == ">$100k") |> 
  ggplot(aes(
    x = astr, 
    y = combined_sales_tax_rate,
  )) +
  geom_point(stroke = 1.1, shape = 1, color = "black", size = 3.5) +
  scale_x_continuous(
    name = "ATR, representative taxpayer, >$100k",
    limits = c(0, 8),
    breaks = seq(0, 8, 2)
  ) +
  scale_y_continuous(
    name = "Combined Sales Tax Rate",
    limits = c(0, 10),
    breaks = seq(0, 10, 2.5)
  ) +
  geom_text_repel(
    aes(label = state_abbrev),
    size = 4.5,
    show.legend = FALSE,
    max.overlaps = Inf,
    color = "black"
  ) +
  theme_bw() +
  theme(
    axis.text.x  = element_text(size = 18),
    axis.text.y  = element_text(size = 18),
    axis.title   = element_text(size = 18),
    axis.ticks   = element_line(color = "black", linewidth = 2),
    legend.title = NULL,
    legend.text  = element_text(size = 18)
  ) +
  labs(
    title = "(a) ATR"
  )


p_top_mtr_2019 <- m |> 
  filter(year==2019, real_fagi_bins == ">$100k") |> 
  ggplot(aes(
    x = statu_top_mtr, 
    y = combined_sales_tax_rate,
  )) +
  geom_point(stroke = 1.1, shape = 1, color = "black", size = 3.5) +
  scale_x_continuous(
    name = "Top MTR",
    limits = c(0, 15),
    breaks = seq(0, 15, 3)
  ) +
  scale_y_continuous(
    name = "Combined Sales Tax Rate",
    limits = c(0, 10),
    breaks = seq(0, 10, 2.5)
  ) +
  geom_text_repel(
    aes(label = state_abbrev),
    size = 4.5,
    show.legend = FALSE,
    max.overlaps = Inf,
    color = "black"
  ) +
  theme_bw() +
  theme(
    axis.text.x  = element_text(size = 18),
    axis.text.y  = element_text(size = 18),
    axis.title   = element_text(size = 18),
    axis.ticks   = element_line(color = "black", linewidth = 2),
    legend.title = NULL,
    legend.text  = element_text(size = 18)
  ) +
  labs(
    title = "(b) Top MTR"
  )


combined <- p_atr_2019 + p_top_mtr_2019 +
  plot_layout(ncol = 2, guides = "collect") &
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.position = "bottom"
  ) 

ggsave("output/figure_A10.png", combined, width = 13.5, height = 6, dpi = 300)
ggsave("output/figure_A10.tiff", combined, width = 13.5, height = 6, dpi = 300)


# Slope coeffs -------------------------------------------------------------
d_for_reg <- m |> 
  filter(year==2019, real_fagi_bins == ">$100k") 

# OLS ATR: -0.063 (0.22)
ols_atr <- feols(combined_sales_tax_rate ~ astr, d_for_reg, vcov = "hetero")
summary(ols_atr)

# OLS MTR: -0.028 (0.13)
ols_mtr <- feols(combined_sales_tax_rate ~ statu_top_mtr, d_for_reg, vcov = "hetero")
summary(ols_mtr)
