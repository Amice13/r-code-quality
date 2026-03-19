library(tidyverse)
library(ccesMRPprep)
library(ccesMRPviz)
library(glue)
library(fs)
library(patchwork)
library(ggtext)


# Data ----
mrp_partyreg <-  read_csv("data/FL-partyreg/FL-val_MRP_partyreg-by-race.csv")
ei_est <- read_rds("data/FL-partyreg/FL-val_ei-by-cd.rds")
prec_frac <- read_csv("data/FL-partyreg/FL-2016_partyreg-race.csv") |>
    filter(total >= 1)

cd_vec <- setNames(unique(prec_frac$cd), unique(prec_frac$cd))

# ground truth from precinct data ---
N_cd_race <- prec_frac |>
    group_by(cd) |>
    summarize(
        N_race_White = sum(total_white),
        N_race_Black = sum(total_black),
        N_race_Hispanic = sum(total_hisp),
        N_race_Other = sum(total_other)
    )  |>
    pivot_longer(-cd, names_prefix = "N_race_", names_to = "race", values_to = "N_race")

prec_truth <- prec_frac |>
    group_by(cd) |>
    summarize(
        frac_rep_White = weighted.mean(frac_rep_white, total_white, na.rm = TRUE),
        frac_rep_Black = weighted.mean(frac_rep_black, total_black, na.rm = TRUE),
        frac_rep_Hispanic  = weighted.mean(frac_rep_hisp, total_hisp, na.rm = TRUE),
        frac_rep_Other = weighted.mean(frac_rep_other, total_other, na.rm = TRUE),
    ) |>
    pivot_longer(-cd, names_prefix = "frac_rep_", names_to = "race", values_to = "val_precinct_R") |>
    left_join(N_cd_race)


# Combine -----
val_cd <- mrp_partyreg |>
    filter(year == 2016) |>
    left_join(ei_est, by = c("cd", "race")) |>
    left_join(prec_truth, by = c("cd", "race")) |>
    filter(str_detect(cd, "FL")) |>
    mutate(race = fct_relevel(race, "White", "Black", "Hispanic")) |>
    rename(vv_party_R = tau_catalist_R,
           p_ei_est_R = p_ei_est) |>
    relocate(year, cd, race, vv_party_R, val_precinct_R,
             p_ei_est_R,
             matches("p_mrp.*_R"), matches("se_.*_R")) |>
    select(-matches("_D$"))


# Graphs --------
# coverage only
val_cd |>
    filter(mrp_type == "nofix") |>
    summarize(
        cvr_ei_80 = mean(p_ei_900 >= val_precinct_R &
                             p_ei_100 <= val_precinct_R),
        cvr_mrp_80 = mean(p_mrp_900 >= vv_party_R &
                              p_mrp_100 <= vv_party_R),
        cvr_ei_95 = mean(p_ei_975 >= val_precinct_R &
                             p_ei_025 <= val_precinct_R),
        cvr_mrp_95 = mean(p_mrp_975 >= vv_party_R &
                              p_mrp_025 <= vv_party_R),
        n = n()
    )

#' Make 2 by 3 plot
make_val_plot <- function(typ = "twway", tbl = val_cd, qval = qnorm(0.9)) {

    val_cd <- tbl |>
        mutate(race = fct_relevel(race, "White", "Hispanic")) |>
        filter(mrp_type == typ)

    ggmain_list <- list()
    for (meth in c("mrp", "ei")) {
        dat <- val_cd
        if (meth == "mrp") {
            dat$est <- val_cd$p_mrp_est
            dat$val <- val_cd$vv_party_R
            dat$se  <- val_cd$p_mrp_se
        }
        if (meth == "ei") {
            dat$est <- val_cd$p_ei_est_R
            dat$val <- val_cd$val_precinct_R
            dat$se  <- val_cd$se_ei_R
        }

        for (g in c("White", "Hispanic", "Black")) {
            i <- glue("{meth}_{g}")

            dat_g <- filter(dat, race == g)
            gg <- scatter_45(
                dat_g,
                val, est,
                ubvar = est + qval*se,
                lbvar = est - qval*se,
                show_error = NULL,
                alpha.CI = 0.3
            )

            # error label
            rmse0 <- error_lbl(dat_g$val, dat_g$est, show_metrics = "rmse")
            bias0 <- error_lbl(dat_g$val, dat_g$est, show_metrics = "bias", metrics_lbl = c(bias = "Mean Error"))
            err_lbl <- glue("{rmse0}\n{bias0}")

            gg <- gg +
                annotate("text", x = Inf, y = -Inf, label = err_lbl, hjust = 1.1, vjust = -0.5,
                         lineheight = 1, size = 2)

            ggmain_list[[i]] <- gg +
                coord_equal(xlim = c(0, 0.7),
                            ylim = c(0, 0.7)) +
                annotate("richtext",
                         x = 0, y = 0.65,
                         label = glue("**{g}** voters"),
                         hjust = 0,
                         size = 3,
                         fill = NA, label.color = NA) +
                labs(
                    x = ifelse(meth == "ei", "True Registered Republican", ""),
                    y = NULL
                ) +
                theme_classic() +
                theme(
                    axis.title.x = element_text(size = 9),
                    axis.title.y = element_markdown(),
                    panel.grid.minor = element_blank(),
                    panel.grid.major = element_blank()
                )

            if (g == "White") {
                ggmain_list[[i]] <- ggmain_list[[i]] +
                    labs(y = switch(meth, ei = "**EI** Estimates", mrp = "**MRP** Estimates"))
            }
        }
    }
    wrap_plots(ggmain_list, ncol = 3)
}

ggsave("figures/FL-val.pdf",
       make_val_plot(typ = "twway"),
       w = 7.2, h = 4.2)

# Save data ----
# write_rds(val_cd, file = "data/FL-partyreg/by-cd_estimates.rds")

