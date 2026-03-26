library(tidyverse)
library(fs)
library(kableExtra)
library(haven)
library(glue)
library(scales)
library(ccesMRPviz)


# Data
est_long <- read_dta("data/cces_by-agg-level.dta") |>
    filter(!st %in% c("PA", "NC")) |>
    mutate(race = as_factor(race),
           division = as_factor(division),
           region = as_factor(region)) |>
    mutate(race = fct_recode(race, "non-White" = "All Non-Whites", "All Races" =  "All"))


# Format ----
rpv_wide <- est_long |>
    filter(race %in% c("White", "non-White")) |>
    pivot_wider(id_cols = c(level, st, cd, region, division, year),
                names_from = race,
                values_from = p_mrp_twway) |>
    mutate(rpv = White - `non-White`) |>
    pivot_wider(id_cols = c(level, st, cd, region, division),
                names_from = year, values_from = rpv, names_prefix = "rpv_")

rvote_wide <- est_long |>
    pivot_wider(id_cols = c(level, st, cd, region, division, race),
                names_from = year, values_from = p_mrp_twway, names_prefix = "trump_")


# Figure ---
rvote_wide |>
    filter(level == "cd", race != "Other") |>
    mutate(race = fct_relevel(race, "All Races")) |>
    scatter_45(trump_2016, trump_2020, by_form = ~race, size.point = 0.1,
               xlab = "Trump Vote among Racial Group (by CD) in 2016",
               ylab = "Trump vote in 2020",
               show_error = "bias")
ggsave("figures/change_2016-2020_by-race.pdf",
       w = 7.5,
       h = 4.5)
