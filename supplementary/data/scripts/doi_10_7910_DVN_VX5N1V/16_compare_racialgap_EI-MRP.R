library(tidyverse)
library(haven)
library(kableExtra)


# Point estimates
df_long <- read_dta("data/ei_by-cd-race_comparsion.dta") |>
    filter(!is.na(ei_mean)) |>
    mutate(race = haven::as_factor(race))

# Summary stats of White/Non-white gap (Table C8) -----
gaps <-
    left_join(
         filter(df_long, race == "White") |>
            rename(p_mrp_twway_white = p_mrp_twway,
                   ei_mean_white = ei_mean) |>
            select(year, cd, matches("_white$")),
        filter(df_long, race != "White") |>
            mutate(race = "Non-White") |>
            group_by(cd) |>
            summarize(
                ei_mean = weighted.mean(ei_mean, N),
                p_mrp_twway = weighted.mean(p_mrp_twway, N))
        ) |>
    mutate(
        gap_mrp = (p_mrp_twway_white - p_mrp_twway),
        gap_ei  = (ei_mean_white - ei_mean))

natl_gap <- df_long |>
    mutate(race = ifelse(race == "White", "white", "nonwhite")) |>
    group_by(race) |>
    summarize(
        p_mrp_twway = weighted.mean(p_mrp_twway, N),
        ei_mean = weighted.mean(ei_mean, N))

# Table
table_gaps <-
    rbind(
        c("Minimum", min(gaps$gap_mrp), min(gaps$gap_ei)),
        c("District in 25th Percentile",
          quantile(gaps$gap_mrp, .25)[[1]],
          quantile(gaps$gap_ei, .25)[[1]]),
        c("Median District",
          quantile(gaps$gap_mrp, .5)[[1]],
          quantile(gaps$gap_ei, .5)[[1]]),
        c("Mean district", mean(gaps$gap_mrp), mean(gaps$gap_ei)),
        c("National Gap",
           t(natl_gap) |> data.frame() |> slice(-1) |>
              mutate(gap = (as.numeric(X2) - as.numeric(X1))) |>
              select(gap) |> t() |> as.vector()),
        c("District in 75th Percentile",
          quantile(gaps$gap_mrp, .75)[[1]],
          quantile(gaps$gap_ei, .75)[[1]]),
        c("Maximum", max(gaps$gap_mrp), max(gaps$gap_ei)),
        c("Standard deviation", sd(gaps$gap_mrp), sd(gaps$gap_ei))) |>
    data.frame() |>
    rename(Statistic = X1, MRP = X2, EI = X3) |>
    mutate(
        MRP = round(as.numeric(MRP), 2),
        EI = round(as.numeric(EI), 2))

# Latex table
kable(table_gaps, booktabs = TRUE, format = "latex",
      align = "lcc", linesep = "",
      col.names = c("Statistic", "MRP", "EI")) |>
    write_lines("tables/EI-MRP_racial_gap_2020.tex")
