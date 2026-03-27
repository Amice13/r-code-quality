library(tidyverse)
library(kableExtra)
library(ccesMRPprep)
library(broom)
library(scales)
library(janitor)
options(knitr.kable.NA = '')

# data
sim_est <- read_csv("data/mrp-full-posterior.csv.gz")

sim_est_16 <- sim_est |>
    filter(year == 2016) |>
    mutate(st = str_sub(cd, 1, 2)) |>
    left_join(ccesMRPprep::states_key, by = "st") |>
    rename(p_mrp = p_mrp_twway)


ff_g <- p_mrp ~ region + st + cd

# may take an hour or more
fit_lm0 <- aov(p_mrp ~ race + cd + (race:cd), filter(sim_est_16, iter %% 10 == 1))
fit_lm  <- aov(p_mrp ~ race + region + st + cd + (race:region) + (race:st) + (race:cd),
              filter(sim_est_16, iter %% 10 == 1))
fitW_lm <- aov(ff_g, filter(sim_est_16, race == "White", iter %% 10 == 1))
fitB_lm <- aov(ff_g, filter(sim_est_16, race == "Black", iter %% 10 == 1))
fitH_lm <- aov(ff_g, filter(sim_est_16, race == "Hispanic", iter %% 10 == 1))


# ANOVA Table ------
#' Format ANOVA table with total degrees of freedom
fmt_var_lm <- function(aov, accr = 0.01) {
    df_pct <- summary(aov)[[1]] |>
        as_tibble(rownames = "grp") |>
        janitor::clean_names() |>
        mutate(frac = scales::percent(sum_sq / sum(sum_sq), accuracy = accr, scale = 1, suffix = "")) |>
        mutate(grp = str_squish(grp))

    df_var <- df_pct |>
        summarize(df = sum(df) + 1, sum_sq = sum(sum_sq)) |>
        mutate(grp = "Total Variance", qty = formatC(sum_sq / df, digits = 2))

    df_n <- tibble(grp = "Samples",
                   qty = formatC(nobs(aov), big.mark = ","))

    bind_rows(df_pct, df_var, df_n)
}


fraclm_long <- list(
    base = fmt_var_lm(fit_lm0),
    overall = fmt_var_lm(fit_lm),
    raceW = fmt_var_lm(fitW_lm),
    raceB = fmt_var_lm(fitB_lm),
    raceH = fmt_var_lm(fitH_lm)) |>
    map_dfr(.id = "model",
            .f = ~ select(.x, grp, qty, frac))

# format to table format
fraclm_wide <- fraclm_long |>
    mutate(frac = str_remove(frac, "^0"),
           frac = str_squish(coalesce(frac, qty))) |>
    pivot_wider(id_cols = grp, values_from = frac, names_from = model) |>
    mutate(grp = fct_relevel(grp, "race", "cd", "st",  "region",
                             "race:cd", "race:st", "race:region", "Residuals",
                             "Total Variance", "Samples")) |>
    arrange(grp) |>
    mutate(grp = str_to_title(grp),
           grp = recode(grp,
                        St = "State",
                        Cd = "Congressional District",
                        Race = "Racial Group",
                        Residuals = "Residuals",
                        `Race:region` = "Race $\\times$ Region",
                        `Race:st` = "Race $\\times$ State",
                        `Race:cd` = "Race $\\times$ Congressional District"))


fraclm_kbl <- kbl(
    fraclm_wide,
    format = "latex",
    col.names = c("", "(1)", "(2)", "(3)", "(4)", "(5)"),
    align = c("r", "c", "c", "c", "c", "c"),
    linesep = "",
    escape = FALSE,
    booktabs = TRUE
) |>
    add_header_above(c(" " = 3, "White" = 1, "Black" = 1, "Hispanic" = 1), line = FALSE) |>
    add_header_above(c(" " = 1, "Across Races" = 2, "Within Race" = 3)) |>
    add_header_above(c(" " = 1, "Fraction of Variation Explained" = 5)) |>
    row_spec(8, hline_after = TRUE) |>
    pack_rows("Race / Ethnicity", 1, 1, latex_gap_space = "1em") |>
    pack_rows("Geography", 2, 4, latex_gap_space = "1em") |>
    pack_rows("Race $\\times$ Geography", 5, 7, latex_gap_space = "1em", escape = FALSE) |>
    pack_rows("Sampling Variation", 8, 8, latex_gap_space = "1em")

fraclm_kbl |>
    write_lines("tables/anova-main.tex")

