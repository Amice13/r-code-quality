# EI and MRP estimate comparisons
#
# This script compares EI and MRP estimates. EI and MRP estimates are merged,
# and comparisons are visualized using scatter-plots.

library(tidyverse)
library(fs)
library(haven)
library(sjlabelled)
library(ccesMRPviz)
library(patchwork)
library(scales)



# EI estimates ----
ei_stacked <- read_csv("data/by-cd-race_EI-ests.csv")

# District-level percent homogeneous ----
dfcd_alarm <- read_csv("data/vtd-cd10-2020_pres-16-20.csv.gz")

ei_use <- ei_stacked |>
    filter(cand == "pres2_20_R")


# MRP estimates ----

mrp_stacked <-
    read_dta("data/cces_by-agg-level.dta") |>
    filter(level == "cd" & year == 2020) |>
    mutate(race = as.character(as_label(race)))

# MRP standard errors
mrp_se <-
    read_csv("data/mrp-full-posterior.csv.gz") |>
    filter(year == 2020) |>
    group_by(year, cd, race) |>
    summarize(
        p_mrp_twway_se = sd(p_mrp_twway),
        p_mrp_twway_nsamples = n(),
        .groups = "drop"
    )

mrp_use <-  mrp_stacked |>
    inner_join(mrp_se, by = c("year", "race", "cd")) |> # loses unused groups e,g. "All non-whites"
    select(
        year, region, division, st, level, cd,
        race, frac_race_in_geo, matches("p_mrp"),
        N, N_geography
    )


# Merged EI and MRP (and percent homogeneous) estimates ----
ei_mrp <-
    inner_join(mrp_use, ei_use, by = c("cd", "race")) |> # loses erroneous CDs like "UT-123"
    mutate(
        race = factor(race, levels = c(
            "White", "Black", "Hispanic",
            "Other", "All Non-White", "All"
        ))
    )



# Figures -----

# Figure `ei-mrp_compare` ----
# EI (x) by MRP (MRP) plot

p_compare <- ei_mrp |>
    filter(race != "Other") |>
    scatter_45(
        xvar = ei_mean, yvar = p_mrp_twway,
        by_form = . ~ race,
        by_nrow = 1,
        size.point = 0.1,
        show_error = c("rmse", "bias"),
        metrics_lbl = c(rmse = "RMSD", bias = "Mean Diff."),
        xlab = "EI Estimate of Trump Vote",
        ylab = "MRP Estimate"
    )


# Figure `ei-mrp_t_homog` ----
# 'MRP and EI Differences by Group Size and Homogeneity'


# Plot 1: MRP-EI difference by group size
p_diff_size <- ei_mrp |>
    filter(race != "Other") |>
    mutate(
        diff = p_mrp_twway - ei_mean,
        diff_abs = abs(diff)
    ) |>
    ggplot(aes(x = frac_race_in_geo, y = diff)) +
    geom_point(size = 0.1) +
    facet_wrap(. ~ race, ncol = 3) +
    theme_bw() +
    scale_x_continuous(labels = percent, limits = c(0, 1)) +
    scale_y_continuous(labels = scales::unit_format(scale = 100, accuracy = 1, unit = "pp")) +
    labs(
        x = "Fraction of Racial Group in District",
        y = "Difference in Trump vote\n(MRP minus EI Estimate)"
    ) +
    theme(
        panel.spacing = unit(15, "pt"),
        plot.title = element_text(
            hjust = 0.5, size = 12,
            margin = margin(0, 0, 12, 0)
        )
    )


# Plot 2: t-test by group homogeneity

# t-test statistics
ei_mrp_t <- ei_mrp |>
    mutate(
        t = (p_mrp_twway - ei_mean) / sqrt(p_mrp_twway_se^2 + ei_se^2),
        t_pval = 2 * pnorm(-abs(t))
    )

# Percent homogeneous (x) by t-test statistic (y)
p_t_homog <-
    ei_mrp_t |>
    filter(race != "Other") |>
    mutate(sig_01 = ifelse(t_pval <= 0.01, "Significant", "Not significant")) |>
    ggplot() +
    geom_point(aes(x = pct_homog, y = t), size = 0.1) +
    facet_wrap(. ~ race, ncol = 3) +
    theme_bw() +
    xlim(0, 1) +
    scale_x_continuous(labels = percent) +
    labs(
        x = "Percent of Homogeneous Precincts in District",
        y = "t-statistic of\nDifference"
    )


# Combined plots
p_t <- p_diff_size / p_t_homog


# Save ------

# Figures in Appendix B4

ggsave("figures/ei-mrp_compare.pdf",
    plot = p_compare, width = 6, height = 2.5
)

ggsave("figures/ei-mrp_t-test.pdf",
    plot = p_t, width = 6, height = 4.5
)


## Combined dataset
write_dta(ei_mrp, "data/ei_by-cd-race_comparsion.dta")
