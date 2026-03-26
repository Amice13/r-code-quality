library(tidyverse)
library(eiCompare) # devtools::install_version("eiCompare", version = "3.0.0", repos = "http://cran.us.r-project.org")


# Data ----
prec_frac <- read_csv("data/FL-partyreg/FL-2016_partyreg-race.csv") |>
    filter(total >= 1)

# ei
ei_by_cd <- list()

set.seed(2023)

# CD by CD EI - many minutes
for (j in unique(prec_frac$cd)) {
    mod_ei <- ei_rxc(
        data = filter(prec_frac, cd == j),
        cand_cols = c("frac_rep", "frac_dem", "frac_else"),
        race_cols = c("frac_white", "frac_black", "frac_hisp", "frac_other"),
        totals_col = "total",
        samples = 1e4,
        burnin = 1e3
    )
    ei_by_cd[[j]] <- mod_ei
    cat(j, "\n")
}

# stack summm stats by cd
ei_by_race <- map_dfr(
    .x = ei_by_cd,
    .f = function(x) {

        draws <- select(x$district_samples, matches("frac_rep_"))

        CIs <- draws |> as_tibble() |>
            apply(MARGIN = 2, \(x) quantile(x, c(0.025, 0.10, 0.9, 0.975))) |> t() |>
            as_tibble(rownames = "race") |>
            rename("p_ei_025" = `2.5%`, "p_ei_100" = `10%`, "p_ei_900"= `90%`, "p_ei_975" = `97.5%`)

        SEs <- draws |> as_tibble() |>
            apply(MARGIN = 2, sd) |>
            enframe(name = "race", value = "se_ei_R")

        # Mean across precincts,
        colMeans(draws) |>
            enframe(name = "race", value = "p_ei_est") |>
            # merged with other stats
            left_join(SEs, by = c("race")) |>
            left_join(CIs, by = c("race")) |>
            mutate(race = str_remove_all(race, "frac_rep_frac_"))
    },
    .id = "cd"
)

ei_est <- ei_by_race |>
    mutate(race = recode(race,
                         white = "White",
                         black = "Black",
                         hisp = "Hispanic",
                         other = "Other"))

# Save ----
write_rds(ei_est, file = "data/FL-partyreg/FL-val_ei-by-cd.rds")
