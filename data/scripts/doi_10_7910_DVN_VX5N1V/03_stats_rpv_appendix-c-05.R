library(tidyverse)
library(haven)
library(scales)
library(kableExtra)

ests <- read_dta("data/cces_by-agg-level.dta")


# Wide
rpv_wide <- ests |>
    mutate(race = str_remove_all(haven::as_factor(race), " Voters")) |>
    filter(race %in% c("White", "All Non-Whites"),
           level == "cd") |>
    rename(mrp = p_mrp_twway) |>
    pivot_wider(
        id_cols = c(year, cd),
        values_from = c(mrp, N),
        names_from = c(race)) |>
    janitor::clean_names() |>
    mutate(rpv = mrp_white - mrp_all_non_whites)


# Appendix C.5 Racial Gap Estimates
# Distribution
rpv_wide |>
    ggplot(aes(x = rpv)) +
    facet_wrap( ~ year) +
    geom_histogram(
        boundary = 0,
        binwidth = 0.025,
        color = "white") +
    theme_classic() +
    scale_x_continuous(labels = percent_format(suffix = "pp")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(x = "Racial Gap",
         y = "Districts")
ggsave("figures/racialgap_hist.pdf",
       w = 5, h = 2.35)

# Stats
rpv_wide |>
    group_by(year) |>
    summarize(
        `Minimum` = min(rpv),
        `District in 25th Percentile` = quantile(rpv, 0.25),
        `Median District` = quantile(rpv, 0.50),
        `Mean District` = mean(rpv),
        `National Gap` = weighted.mean(mrp_white, n_white) -
            weighted.mean(mrp_all_non_whites, n_all_non_whites),
        `District in 75th Percentile` = quantile(rpv, 0.75),
        `Maximum` = max(rpv),
        `Standard Deviation` = sd(rpv)) |>
    select(-year) |> t() |>
    as_tibble(rownames = "Statistic", .name_repair = make.names) |>
    kbl(digits = 2,
        booktabs = TRUE,
        format = "latex",
        col.names = c("Statistic", "2016", "2020"),
        linesep = c(rep("", 7), "\addlinespace")) |>
    write_lines("tables/racialgap_stats.tex")

# CDs highlighted in main text ------
# check claims in paper

lowest_gap <- rpv_wide |>
    filter(year == 2016) |>
    filter(rpv %in% sort(rpv)[c(1:5)]) |>
    pull(cd)
stopifnot(all(lowest_gap %in% c("CA-02", "CA-12", "CA-13", "WA-07", "NY-12")))

rpv_wide |>
    filter(year == 2016) |>
    filter(rpv %in% sort(rpv)[429:435]) |>
    arrange(year, desc(rpv)) |>
    pull(cd) |>
    as.vector() |>
    str_detect("(AL|MS)") |>
    all()
    stopifnot()

va_sub <- rpv_wide |>
    filter(year == 2016) |>
    mutate(st = str_sub(cd, 1, 2)) |>
    filter(cd %in% c("VA-10", "VA-08", "VA-11")) |>
    pull(rpv)
stopifnot(all(va_sub < 0.21))
