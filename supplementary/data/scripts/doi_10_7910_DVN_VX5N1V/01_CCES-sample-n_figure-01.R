library(tidyverse)
library(lemon)
library(glue)
library(ggtext)
library(fs)

# estimates
cc_mrp <- read_csv(path("data/mrp-ests_by-cd-race.csv"))

# data to plot
cc_n_16 <- cc_mrp |>
    select(year, cd, race, n = n_raw) |>
    filter(year == 2016)

# for plotting
cc_plot <- bind_rows(
    cc_n_16 |> mutate(race = recode(race, "Asian" = "Other")),
    count(cc_n_16, cd, wt = n) |> mutate(race = "All Races")) |>
    filter(race != "Other") |>
    mutate(race = fct_relevel(race, "All Races", "White", "Black", "Hispanic"))

plot_stats <-  cc_plot |>
    group_by(race) |>
    summarize(
        med = median(n),
        less20 = sum(n < 20)
    ) |>
    mutate(lbl = glue("Median: n = <b>{med}</b><br><b>{less20}</b> districts with n < 20"))

cc_base <- cc_plot |>
    ggplot(aes(x = n)) +
    facet_rep_wrap(~ race, nrow = 1, repeat.tick.labels = TRUE) +
    scale_y_continuous(expand = expansion(mult= c(0, 0.05))) +
    theme_classic() +
    labs(x = "CCES sample size of racial group in each Congressional District",
         y = "Number of CDs") +
    theme(axis.line.y = element_blank(),
          strip.background = element_rect(color = "transparent", fill = "grey90"))

cc_n <- cc_base +
    geom_histogram(color = "white",
                   binwidth = 10,
                   boundary = 0) +
    geom_richtext(data = plot_stats, aes(label = lbl),
                  x = 30,
                  y = 150,
                  hjust = 0,
                  fill = NA, label.color = NA, # remove background and outline
                  label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                  size = 2.5)

#' Saves Figure 1
ggsave("figures/sample-size_histogram.pdf",
       cc_n,
       w = 7, h = 2)
