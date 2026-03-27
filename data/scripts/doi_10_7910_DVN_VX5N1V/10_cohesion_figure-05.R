library(tidyverse)
library(fs)

# Data -----
cc_cd <- read_dta("data/cces_by-agg-level.dta") |>
    mutate(race = haven::as_factor(race))

# Small states to exclude
state_demog <- cc_cd |>
    filter(race %in% c("White", "Black", "Hispanic", "All"), level == "state") |>
    group_by(year, st) |>
    transmute(year, race, st, size_frac = N / sum(N *(race == "All"))) |>
    ungroup()

small_state <- state_demog |>
    filter(size_frac < 0.02) |>
    mutate(race = as.character(as_factor(race)))


# Cohesion for non-small states
df_coh <- cc_cd |>
    filter(level == "cd") |>
    filter(year == 2016) |>
    filter(race  %in% c("White", "Hispanic", "Black")) |>
    mutate(race = fct_relevel(race, "White", "Hispanic", "Black")) |>
    group_by(year, cd, race) |>
    summarize(
        coh =  abs(p_mrp_twway - 0.5)
    ) |>
    mutate(st = str_sub(cd, 1, 2)) |>
    anti_join(small_state, by = c("year", "race", "st"))


lbl_diff <- function(x) {
    recode_factor(x, wb = "White - Black", wh = "White - Hispanic", bh = "Black - Hispanic")
}


gg_coh <- ggplot(df_coh, aes(x = coh)) +
    geom_histogram(color = "white",
                   boundary = 0,
                   binwidth = 0.025) +
    facet_wrap(~ race) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
    scale_x_continuous(
        labels = function(x) {
            percent(x, accuracy = 1, suffix = "pp") |>
                recode("0pp" = "0") |>
                recode(`10pp` = "", `30pp` = "")},
        breaks = seq(0, 0.4, by = 0.10),
        limits = c(0, 0.5)) +
    theme_bw() +
    theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
    ) +
    labs(x = "District-level Cohesion in Voting",
         title = NULL,
         y = "Districts")



# Save  -------
# Saves to Figure 5
ggsave("figures/rpv_cohesion_histograms.pdf", gg_coh, w = 5, h = 2)
