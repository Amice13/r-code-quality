library(tidyverse)
library(kableExtra)
library(haven)
library(fs)
library(tictoc)

## Sims ---
df_long <- read_dta("data/ei_by-cd-race_comparsion.dta") |>
    filter(!is.na(ei_mean))

# See script 14_EI-435-districts
ei_sims_raw <- map_dfr(dir_ls("data/intermediate/VAP-EI-CD/", regexp = "\\.csv"),
                       ~read_csv(.x, col_types = cols()))

ei_sims <- left_join(distinct(df_long, st, region, cd), ei_sims_raw)  |>
    mutate(across(where(is.labelled), haven::as_factor),
           race = recode_factor(race, white = "White", black = "Black", hisp = "Hispanic", other = "Other"))

sim_est <- read_csv("data/mrp-full-posterior.csv.gz")

sim_est_20 <- sim_est |>
    filter(year == 2020) |>
    mutate(st = str_sub(cd, 1, 2)) |>
    left_join(ccesMRPprep::states_key, by = "st") |>
    rename(p_mrp = p_mrp_twway)


# ANOVA -----
fmt_var_lm <- function(aov, accr = 0.01) {
    df_pct <- summary(aov)[[1]] |>
        as_tibble(rownames = "grp") |>
        janitor::clean_names() |>
        mutate(frac = scales::percent(sum_sq / sum(sum_sq), accuracy = accr, scale = 1, suffix = "")) |>
        mutate(grp = str_squish(grp))

    df_total <- df_pct |>
        summarize(df = sum(df) + 1, sum_sq = sum(sum_sq)) |>
        mutate(grp = "Total Sums of Squares")
    df_mean <- df_total |>
        mutate(grp = "Total Variance", sum_sq = sum_sq / df)

    bind_rows(df_pct, df_total, df_mean)
}


df_fct <- df_long |>
    mutate(across(where(is.labelled), haven::as_factor),
           cd = as.factor(cd))

mrp_compare_sim <- sim_est_20 |>
    filter(iter %% 10 == 1) |>
    filter(cd %in% unique(c(ei_sims$cd)))

tic()
aov_sims_mrp_aov <- aov(p_mrp ~ race + region + st + cd + race:region + race:st + race:cd,
                        mrp_compare_sim)
toc()
gc()

tic()
aov_sims_ei_aov <- aov(pres2_20_R ~ race + region + st + cd + race:region + race:st + race:cd,
                       filter(ei_sims, iter %in% round(seq(1, 5001, length.out = 200))))
toc()

aov_sims_mrp <- fmt_var_lm(aov_sims_mrp_aov)
aov_sims_ei <- fmt_var_lm(aov_sims_ei_aov)

left_join(aov_sims_mrp, aov_sims_ei, by = c("grp"), suffix = c("_MRP", "_EI")) |>
    select(Component = grp, frac_MRP, frac_EI, df_MRP) |>
    mutate(across(matches("sum_sq"), ~round(.x, 1))) |>
    kbl(col.names = c("Component", "MRP", "EI", "df"),
        booktabs = TRUE,
        linesep = "",
        align = "c",
        format = "latex") |>
    add_header_above(c(" " = 1, "Fraction of Variance" = 2, " "  = 1)) |>
    row_spec(8, hline_after = TRUE)

