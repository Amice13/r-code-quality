##################################################################################
# Agrawal and Chen (2026)
#
# Descriptions: Correlates Job postings with average ATR measures.
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

# load data on representative taxpayer atr --------------------------------

repre_taxsim_out_16_23 <- read_dta("data/intermediate/taxsim_output/year_state_incomebin_avg_taxsim_out_16_23.dta")

repre_taxsim_out_24 <- read_dta("data/intermediate/taxsim_output/year_state_incomebin_avg_taxsim_out_24.dta") |> 
  mutate(year = 2024)

repre_taxsim_out <- rbind(repre_taxsim_out_16_23, repre_taxsim_out_24)

incbin_xwalk <- read_csv("data/xwalk/year_state_incomebin_atr_taxsimid_xwalk.csv")

repre_atr <- repre_taxsim_out |> 
  # merge on income bin
  left_join(incbin_xwalk) |> 
  select(year, state, real_fagi_bins, fiitax, siitax, v10) |> 
  mutate(astr = (siitax) / v10 * 100)

soi_fips_xwalk <- read_csv("data/xwalk/irs_soi_fips_crosswalk.csv") |> 
  clean_names() |> 
  select(state = irs_soi_code, state_fip = fips_code)

repre_atr <- repre_atr |> 
  # add fips
  left_join(soi_fips_xwalk) |> 
  # remove soi state code
  select(-state)

# load data on job posting ---------------------------------
job_posting <- read_excel(
  "data/takeup/remote_work_in_job_ads_signup_data-1.xlsx",
  sheet = "state_by_month"
) |> 
  clean_names()

# restrict to US
job_posting_min <- job_posting |> 
  filter(country == "USA") |> 
  select(year, state_region, percent, n)

# compute annual percent
clean_job_posting <- job_posting_min |> 
  group_by(year, state_region) |> 
  summarise(
    annual_percent = weighted.mean(percent, w = n),
    annual_n = weighted.mean(n),
    .groups = "drop"
  ) |> 
  # change the name for DC for later merging
  mutate(
    state_region = ifelse(
      state_region=="Washington, D.C. (District of Columbia)", 
      "District of Columbia", 
      state_region
      )
  )

# load state name and abbrev/fips xwalk
xwalk <- read_excel("data/xwalk/StateFIPSicsprAB.xls") |>
  clean_names() |> 
  select(state_region = name, state_abbrev = ab, state_fip = fips)

# pull in state abbrev
clean_job_posting <- clean_job_posting |> 
  left_join(xwalk) 


# # load data on convenience state coding -----------------------------------
# conven_rules <- read_csv("data/derived_ipums/flow_convenience_states.csv") |> 
#   select(year, state_fip = pwstate2, convenience_state) |> 
#   distinct(year, state_fip, convenience_state)


# Merge -------------------------------------------------------------------
m <- repre_atr |>
  filter(year>=2019) |> 
  left_join(clean_job_posting, by = c("year", "state_fip")) |> 
  mutate(
  convenience_state = case_when(
    
    # Connecticut has retaliatory rule
    (year %in% c(2019, 2020, 2021, 2022, 2023, 2024) & state_fip == 9) ~ "Retaliatory",
    
    (year == 2019 & state_fip %in% c(10, 31, 36, 42)) ~ "Convenience",
    
    # 2020: Arkansas (5) and Massachusetts (25) adopted temp. convenience rule
    (year == 2020 & state_fip %in% c(10, 31, 36, 42, 5, 25)) ~ "Convenience",
    
    # 2021: 
    # Arkansas repealed convenience rule, effective for tax years beginning on or after January 1, 2021
    # Temp. rule in Massachusetts was challenged and eventually ended as pandemic measures subsided
    (year == 2021 & state_fip %in% c(10, 31, 36, 42)) ~ "Convenience",
  
    # add Oregon (41)
    (year == 2022 & state_fip %in% c(10, 31, 36, 42, 41)) ~ "Convenience",
    
    # 2023 (to present)
    # Alabama (1) adopted convenience rule
    # also Oregon (41)
    (year >= 2023 & state_fip %in% c(10, 31, 36, 42, 1, 41)) ~ "Convenience",
    
    # New Jersey (34) adopted retaliatory rule
    # Connecticut still has retaliatory rule
    (year >= 2023 & state_fip == 34) ~ "Retaliatory",
    
    .default = "No Convenience"
  ),
  
  convenience_state = fct_relevel(convenience_state, "No Convenience", "Convenience", "Retaliatory")
  )



make_two_year_scatter <- function(bin_label) {
  
  base_plot <- function(df, year_label, panel_title) {
    df |>
      filter(year == year_label, real_fagi_bins == bin_label) |>
      ggplot(aes(
        x = astr, 
        y = annual_percent, 
        color = convenience_state, 
        shape = convenience_state,
        size  = convenience_state
      )) +
      # 1) Draw hollow "No Convenience" first (background)
      geom_point(
        data  = df |> 
          filter(year == year_label, real_fagi_bins == bin_label) |> 
          filter(convenience_state == "No Convenience"),
        stroke = 1.1
      ) +
      # 2) Draw solid Convenience + Retaliatory on top
      geom_point(
        data  = df |> 
          filter(year == year_label, real_fagi_bins == bin_label) |> 
          filter(convenience_state != "No Convenience"),
        stroke = 1.1
      ) +
      scale_color_manual(
        values = c(
          # "No Convenience" = "#0072B2",  # blue
          # "Convenience"    = "#D20062",  # red
          # "Retaliatory"    = "#8B0000"   # dark red
          
          "No Convenience" = "black",  # blue
          "Convenience"    = "black",  # red
          "Retaliatory"    = "black"   # dark red
        )
      ) +
      scale_shape_manual(
        values = c(
          "No Convenience" = 1,   # hollow circle
          "Convenience"    = 16,  # filled circle
          "Retaliatory"    = 18   # diamond
        )
      ) +
      scale_size_manual(
        values = c(
          "No Convenience" = 3.5,
          "Convenience"    = 4,
          "Retaliatory"    = 5
        )
      ) +
      scale_y_continuous(
        name   = "Percent Teleworkable Postings",
        limits = c(0, 30),
        breaks = seq(0, 30, 5)
      ) +
      geom_text_repel(
        aes(label = state_abbrev),
        size = 4.5,
        show.legend = FALSE,
        max.overlaps = Inf
      ) +
      labs(
        title = panel_title,
        shape = NULL, linetype = NULL, color = NULL, size = NULL
      ) +
      theme_bw() +
      theme(
        axis.text.x  = element_text(size = 18),
        axis.text.y  = element_text(size = 18),
        axis.title   = element_text(size = 18),
        axis.ticks   = element_line(color = "black", linewidth = 2),
        legend.title = NULL,
        legend.text  = element_text(size = 18)
      )
  }
  
  
  p_2019 <- base_plot(m, 2019, "(a) 2019")
  p_2024 <- base_plot(m, 2024, "(b) 2024")
  
  p_2019 + p_2024 +
    plot_layout(ncol = 2, guides = "collect") &
    theme(
      plot.title      = element_text(hjust = 0.5, size = 18, face = "bold"),
      legend.position = "bottom"
    ) &
    guides(alpha = guide_legend(nrow = 1, byrow = TRUE))
}


# >100k
combined_100k <- make_two_year_scatter(">$100k") &
  scale_x_continuous(
    name = "Average State Tax Rate",
    limits = c(0, 8),
    breaks = seq(0, 8, 2)
  ) 

ggsave("output/figure_8.png", combined_100k, width = 13.5, height = 7)
ggsave("output/figure_8.tiff", combined_100k, width = 13.5, height = 7, dpi = 300)

# Slope coeffs -------------------------------------------------------------
d_for_reg_19 <- m |> 
  filter(year == 2019) |> 
  filter(real_fagi_bins == ">$100k") |> 
  filter(convenience_state != "No Convenience")

d_for_reg_24 <- m |> 
  filter(year == 2024) |> 
  filter(real_fagi_bins == ">$100k") |> 
  filter(convenience_state != "No Convenience")


# OLS 2019: -0.083769 (0.153761)
ols19 <- feols(annual_percent ~ astr, d_for_reg_19, vcov = "hetero")

summary(ols19)


# OLS 2023: 0.650540 (0.410678)
ols24 <- feols(annual_percent ~ astr, d_for_reg_24, vcov = "hetero")
summary(ols24)




