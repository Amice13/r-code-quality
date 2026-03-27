library(tidyverse)
library(haven)
library(ccesMRPviz)
library(glue)
library(patchwork)

point_est <- read_csv("data/mrp-ests_by-cd-race.csv") |>
    filter(year == 2016, race != "Other") |>
    mutate(race = glue("{race} Voters"),
           race = fct_relevel(race, "White Voters", "Hispanic Voters"))



cc_compare <- function(xvar = p_mrp_nofix, yvar = p_mrp_ggfix) {
    xvar = enquo(xvar)
    yvar = enquo(yvar)

    # automatic axis title
    xlb <- switch(quo_name(xvar),
                  p_mrp_nofix = "Trump vote, (1) MRP without calibration",
                  p_mrp_ggfix = "Trump vote, (2) One-way geography calibration")
    ylb <- switch(quo_name(yvar),
                  p_mrp_nofix = "(1) MRP without calibration",
                  p_mrp_ggfix = "(2) Calibrated one-way",
                  p_mrp_twway = "(3) Calibrated two-way\n(main specification)")

    scatter_45(point_est,
               !!xvar,
               !!yvar,
               xlab = xlb, ylab = ylb,
               size.point = 0.05,
               alpha.point = 0.5,
               by_form = ~ race,
               show_error = c("rmse", "bias"),
               metrics_lbl = c(rmse = "RMSD", bias = "Mean Diff.")) +
        theme_classic() +
        theme(strip.background = element_rect(color = "transparent", fill = "lightgray"))

}


cc_compare(p_mrp_nofix, p_mrp_ggfix) +
    cc_compare(p_mrp_nofix, p_mrp_twway) +
    cc_compare(p_mrp_ggfix, p_mrp_twway) +
    plot_layout(ncol = 1)

ggsave("figures/consequences-calibration.pdf",
       w = 7, h = 6)
