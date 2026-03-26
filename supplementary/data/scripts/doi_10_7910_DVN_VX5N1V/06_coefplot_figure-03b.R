# Get difference between race and their CIs by the full posterior distribution

library(tidyverse)
library(ccesMRPprep)
library(ggtext)
library(glue)


#' Plot diff in race with summary stats. Adjusts to races to show
plot_rpv_coef <- function(tbl, show) {
    tbl <- filter(tbl, race == show, year == 2016)

    race2 <- switch(show, wn = "Non-White Voters", wb = "Black Voters", wh = "Hispanic Voters")

    st_ann <- tbl |>
        group_by(division) |>
        mutate(cd_in_facet = 1:n()) |>
        group_by(division, st, st_color, .add = FALSE) |>
        summarize(st_midpoint = mean(cd_in_facet),
                  diff = quantile(diff_q90, 0.8),
                  n = n(),
                  .groups = "drop")

    tbl |>
        ggplot(aes(x = cd, y = diff_mean, color = st_color)) +
        geom_point(size = 0.5) +
        geom_errorbar(aes(ymin = diff_q10, ymax = diff_q90), width = 0,
                      alpha = 0.3) +
        facet_grid(~ division, drop = TRUE, scales = "free_x", space = "free") +
        geom_text(data = st_ann,
                  aes(label = st,
                      x = st_midpoint,
                      y = diff,
                      size = n),
                  nudge_y = 0.02) +
        geom_hline(yintercept = 0, color = "indianred") +
        scale_color_manual(values = c(`TRUE` = "black", `FALSE` = "gray40")) +
        scale_x_discrete(label = NULL, breaks = NULL) +
        scale_y_continuous(labels = scales::unit_format(scale = 100, unit = "pp")) +
        coord_cartesian(ylim = c(-0.07, 0.83)) +
        theme_bw() +
        guides(color = "none", size = "none") +
        labs(y = glue("White minus **{race2}** Trump Vote"),
             x = NULL) +
        theme(panel.grid.major.x = element_blank(),
              axis.text.y = element_text(color = "black"),
              axis.title.y = element_markdown())
}


# Data ----
sim_est <- read_csv("data/mrp-full-posterior.csv.gz")


# Sim of diffs --------
diff_iter <- sim_est |>
    group_by(year, cd, iter) |>
    summarize(
        wn = sum(p_mrp_twway * (race == "White")) -
            (sum(p_mrp_twway * (race != "White") * N) / sum((race != "White") * N)),
        wb = sum(p_mrp_twway * (race == "White")) - sum(p_mrp_twway * (race == "Black")),
        wh = sum(p_mrp_twway * (race == "White")) - sum(p_mrp_twway * (race == "Hispanic")),
        .groups = "drop"
    ) |>
    pivot_longer(-c(year, cd, iter), names_to = "race", values_to = "diff")


# Quantiles of diff
diff_cd <- diff_iter |>
    group_by(year, cd, race) |>
    summarize(diff_mean = mean(diff),
              diff_se = sd(diff),
              diff_q10 = quantile(diff, 0.10),
              diff_q90 = quantile(diff, 0.90),
              .groups = "drop")


# Plot -----
diff_plot <- diff_cd |>
    mutate(st = str_sub(cd, 1, 2)) |>
    left_join(select(ccesMRPprep::states_key, st, division, region)) |>
    group_by(division, st) |>
    arrange(region, division, st, cd) |>
    mutate(st_order = cur_group_id()) |>
    mutate(division = str_replace_all(division, "\\s", "\n")) |>
    ungroup() |>
    mutate(st_color = st_order %% 2 == 0) |>
    mutate(cd = fct_inorder(cd),
           division = fct_rev(fct_inorder(division)))


# annotate ----
pl_nw <- plot_rpv_coef(diff_plot, "wn")
pl_bl <- plot_rpv_coef(diff_plot, "wb")
pl_hi <- plot_rpv_coef(diff_plot, "wh")


# Save -------
# Saves to Figure 3b, C4
ggsave("figures/rpv-coefplot_nonwhite_2016.pdf", pl_nw, w = 10, h = 3.5)

ggsave("figures/rpv-coefplot_black_2016.pdf", pl_bl, w = 10, h = 3.5)
ggsave("figures/rpv-coefplot_hisp_2016.pdf",  pl_hi, w = 10, h = 3.5)

