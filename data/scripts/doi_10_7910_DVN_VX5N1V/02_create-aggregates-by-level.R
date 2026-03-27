# Makes main dataset for visualizing point estimates.
# Can be skipped since the output is saved in dataverse as well

library(tidyverse)
library(patchwork)
library(fs)
library(haven)


#' Summarize at race or geography level.
#' @param size the dataset that includes "size_fct", the size of each unit. This will be used to reweight
summ <- function(tbl, level_lab = NULL, race_lab = NULL, size = NULL) {

    if (is.null(size)) {
        tbl <- tbl |>
            mutate(size_fct = 1)
    }

    if (!is.null(size)) {
        tbl <- tbl |>
            left_join(size, by = c("year", "cd"))
    }

    out <- tbl |>
        summarize(
            p_mrp_twway = weighted.mean(p_mrp_twway, N*size_fct),
            p_mrp_ggfix = weighted.mean(p_mrp_ggfix, N*size_fct),
            p_mrp_nofix = weighted.mean(p_mrp_nofix, N*size_fct),
            N = as.integer(sum(N*size_fct)),
            .groups = "drop"
        )

    if (!is.null(level_lab)) {
        out$level <- level_lab
    }
    if (!is.null(race_lab)) {
        out$race <- race_lab
    }

    out |>
        relocate(year, race, level)
}

# Data ------
cc_mrp <- read_csv(path("data/mrp-ests_by-cd-race.csv"))
cd_vfpop <- read_csv(path("data/totalvoters_by-cd.csv"))


# Start data combination ----
cc_df <- cc_mrp |>
    mutate(st = str_sub(cd, 1, 2)) |>
    left_join(select(ccesMRPprep::states_key, st, st_trad, division, region))

## add voterfile calibration update for correcting when adding
cd_fct <- cc_mrp |>
    count(year, cd, wt = N, name = "N") |>
    left_join(cd_vfpop, by = c("cd", "year")) |>
    transmute(year, cd, size_fct = totalvoters / N) |>
    # tsmart issue with UT voterfile
    mutate(size_fct = replace(size_fct, (str_detect(cd, "(UT)") & year == 2020), 1)) |>
    mutate(size_fct = replace_na(size_fct, 1))



# group by (with cd sizes)
rpv_cd <- cc_df |>
    group_by(year, region, division, st, cd, race) |>
    summ(level_lab = "cd", size = cd_fct)

rpv_state <- cc_df |>
    group_by(year, region, division, st, race) |>
    summ(level_lab = "state", size = cd_fct)

rpv_division <- cc_df |>
    group_by(year, region, division, race) |>
    summ(level_lab = "division", size = cd_fct)

rpv_region <- cc_df |>
    group_by(year, region, race) |>
    summ(level_lab = "region", size = cd_fct)

rpv_natl <- cc_df |>
    group_by(year, race) |>
    summ(level_lab = "nation", size = cd_fct)


# combine
stacked_scale <- bind_rows(
    rpv_natl,
    rpv_region,
    rpv_division,
    rpv_state,
    rpv_cd
) |>
    relocate(year, race, level, region, division, st, cd, matches("pr_"))

# sum ACROSS race
all <- stacked_scale |>
    group_by(year, level, region, division, st, cd) |>
    summ(race_lab = "All")

nonwhites <- stacked_scale |>
    filter(race != "White") |> # just this line differs
    group_by(year, level, region, division, st, cd) |>
    summ(race = "All Non-Whites")

# for export
estimates_stacked <- bind_rows(
    stacked_scale,
    nonwhites,
    all) |>
    mutate(race = fct_relevel(race, "White", "All Non-Whites",  "Black", "Hispanic", "Other", "All"))


# Add fraction
out <- estimates_stacked |>
    left_join(select(all, year, level, region, division, st, cd, N_geography = N)) |>
    mutate(frac_race_in_geo = N / N_geography)

# Attributes -----

attr(out$level, "label") <- "Level of Geography"
attr(out$st, "label") <- "State"
attr(out$cd, "label") <- "Congressional District"

attr(out$p_mrp_twway, "label") <- "Trump Vote (Posterior Mean), raked to district vote and national vote by race"
attr(out$p_mrp_ggfix, "label") <- "Trump Vote (Posterior Mean), raked to district vote"
attr(out$p_mrp_nofix, "label") <- "Trump Vote (Posterior Mean), without raking"


attr(out$N, "label") <- "Size of race in the geography (estimated poststratification table)"
attr(out$frac_race_in_geo, "label") <- "Fraction of that racial group in the geography"


# Export ------
# Save to Stata DTA
write_dta(out, "data/cces_by-agg-level.dta")
