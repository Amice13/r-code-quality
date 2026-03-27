# Format estimates and SEs into nice latex tables

library(tidyverse)
library(fs)
library(kableExtra)
library(haven)
library(glue)
library(scales)
library(ccesMRPprep)


#' Turn tibble into kable table latex code and save
#'
#' @param col_names Column names, custom
#' @param dir target directory to save in
#' @param prefix add name to file name
#' @param sort_st sort state by
tabout <- function(tbl,
                   dir = "tables",
                   prefix = "by-race_",
                   level = NULL,
                   year = "",
                   suffix = "",
                   wd = "0.25in",
                   sort_st = "alpha",
                   subset_regex = NULL,
                   cap = NULL,
                   lbl = NULL,
                   col_names = c("", "White", "All", "Black", "Hisp.",  "All"),
                   total = NULL) {

    if (!is.null(subset_regex)) {
        tbl <- tbl |>
            filter(str_detect(str_sub(cd,  start = -5), subset_regex))
    }

    lsp = ""
    if (level == "cd") {
        # space by state
        sts <- str_sub(tbl$cd, start = -5, end = -4) |> table()
        lsp <- map(names(sts),
                   function(x) {
                       stvec <- rep("", as.vector(sts[x]))
                       stvec[length(stvec)] <- "\\addlinespace"
                       stvec
                   }) |>
            unlist()
    }

    col_aligns <- c("r", rep("c", length(col_names) - 1))

    hasyr <- FALSE
    if (level %in% c("national")) {
        col_aligns <- c("c", col_aligns)
        col_names <- c("Year", col_names)
        hasyr <- TRUE
    }

    if (level != "national" & !is.null(total)) {

        tbl <- tbl |>
            mutate(across(.cols = 1, as.character)) |>
            bind_rows(total)

        tbl[nrow(tbl), 1] <- "National"
    }

    tblfmt <- tbl |>
        mutate(across(where(is.numeric), function(x) percent(x, accuracy = 1, suffix = "")))

    tblkbl <- tblfmt |>
        kbl(booktabs = TRUE,
            longtable = ifelse(level == "cd", TRUE, FALSE),
            digits = 0,
            format = "latex",
            linesep = lsp,
            col.names = col_names, # WARNING: handcoded
            caption = cap,
            label = lbl,
            escape = FALSE,
            align = col_aligns) |>
        column_spec(2:6 + hasyr, width = wd) |>
        add_header_above(c(" " = 1 + hasyr, " " = 1, "Non-Whites" = 3, " " = 1))

    if (level != "nation" & !is.null(total)) {
        tblkbl <- tblkbl |>
            row_spec(nrow(tblfmt) - 1, hline_after = TRUE)
    }

    #
    if (level == "division" & is.null(subset_regex)) {
        tblkbl <- tblkbl |>
            pack_rows(index = c("Northeast" = 2, "South" = 3, "North Central" = 2, "West" = 2))
    }

    if (level == "state" & sort_st == "division" & is.null(subset_regex)) {
        tblkbl <- tblkbl |>
            pack_rows(index = c(
                "New England" = 6,
                "Middle Atlantic" = 3,
                "South Atlantic" = 8,
                "East South Central" = 4,
                "West South Central" = 4,
                "East North Central" = 5,
                "West North Central" = 7,
                "Mountain" = 8,
                "Pacific" = 5
            ))
    }

    if (level == "cd") {
        tblkbl <- tblkbl |>
            kable_styling(latex_options = c("repeat_header"))
    }

    if (is.null(subset_regex)) {
        stopifnot(dir_exists(dir))

        tblkbl |>
            write_lines(glue("{dir}/{year}{prefix}{level}{suffix}.tex"))
    }  else {
        return(tblkbl)
    }
}

reshp_wide <- function(data, id_vars, valuevar = p_mrp_twway) {
    id_vars <- enquo(id_vars)
    valuevar <- enquo(valuevar)
    data |>
        pivot_wider(id_cols = {{id_vars}},
                    names_from = race,
                    values_from = {{valuevar}},
                    names_sort = TRUE)
}

#'round
r <- function(x) percent(x, accuracy = 1, suffix = '')




# Data -------
st_ests <- read_csv(path("data/mrp-ests_by-state-race.csv"))
cd_ests <- read_dta("data/cces_by-agg-level.dta")

est_long <- cd_ests |>
    mutate(race = haven::as_factor(race),
           division = haven::as_factor(division),
           region = haven::as_factor(region)) |>
    mutate(race = fct_recode(race, "non-White" = "All Non-Whites")) |>
    filter(race != "Other") |>
    group_by(year, level, region, division, st, cd) |>
    mutate(size = N / sum(N *(race == "All"))) |>
    ungroup()

stopifnot(levels(est_long$race) == c("White", "non-White", "Black", "Hispanic", "Other", "All"))

nation_df <- est_long |>
    filter(level == "nation") |>
    reshp_wide(id_vars = year, valuevar = p_mrp_twway)

nation_df_turn <- est_long |>
    filter(level == "nation") |>
    group_by(year) |>
    mutate(frac = N / sum(N*(race == "All"))) |>
    ungroup() |>
    reshp_wide(id_vars = year, valuevar = frac)

us16 <- nation_df |> filter(year == 2016) |> select(-year)
us20 <- nation_df |> filter(year == 2020) |> select(-year)

us16_turn <- nation_df_turn |> filter(year == 2016) |> select(-year)
us20_turn <- nation_df_turn |> filter(year == 2020) |> select(-year)

# Tables -----
## Nation number -----
est_long |>
    filter(level == "nation") |>
    reshp_wide(id_vars = c(year, level)) |>
    mutate(year = as.character(year)) |>
    tabout(level = "national")


## Region ----
tbl_reg <- est_long |>
    filter(level == "region") |>
    reshp_wide(id_vars = c("year",  "region")) |>
    arrange(year, region)

tbl_reg |> filter(year == 2016) |> select(-year) |> tabout(level = "region", year = "2016-", total = us16)
tbl_reg |> filter(year == 2020) |> select(-year) |> tabout(level = "region", year = "2020-", total = us20)

tbl_div <- est_long |>
    filter(level == "division") |>
    arrange(region, division) |>
    reshp_wide(id_vars = c("year", "division")) |>
    arrange(division)

tbl_div |> filter(year == 2016) |> select(-year) |> tabout(level = "division", year = "2016-", total = us16)
tbl_div |> filter(year == 2020) |> select(-year) |> tabout(level = "division", year = "2020-", total = us20)

# State ----
# Change to character to add parenthesis
us16_chr <- us16 |> mutate(across(where(is.numeric), r))
us20_chr <- us20 |> mutate(across(where(is.numeric), r))

us16t_chr <- us16_turn |> mutate(across(where(is.numeric), r))
us20t_chr <- us20_turn |> mutate(across(where(is.numeric), r))

st_sorted <- est_long |>
    filter(level == "state") |>
    left_join(select(states_key, st, state)) |>
    mutate(trump_chr = as.character(glue("{if_else(size < 0.02, '(', '')}{r(p_mrp_twway)}{if_else(size < 0.02, ')', '')}"))) |>
    arrange(region, division, state)

tbl_st <- st_sorted |>
    reshp_wide(id_vars = c("year", "state"), valuevar = trump_chr)

tbl_st_turn <- st_sorted |>
    group_by(year, state) |>
    mutate(frac = N / sum(N*(race == "All"))) |>
    reshp_wide(id_vars = c("year", "state"), valuevar = frac) |>
    ungroup()

## Saves for Table 1 (and 2020 equivalent)
tbl_st |> filter(year == 2016) |> select(-year) |> tabout(level = "state", year = "2016-", total = us16_chr, sort_st = "division")
tbl_st |> filter(year == 2020) |> select(-year) |> tabout(level = "state", year = "2020-", total = us20_chr, sort_st = "division")
tbl_st_turn |> filter(year == 2016) |> select(-year) |> tabout(level = "state", year = "2016-turnout-", total = us16_turn, sort_st = "division")
tbl_st_turn |> filter(year == 2020) |> select(-year) |> tabout(level = "state", year = "2020-turnout-", total = us20_turn, sort_st = "division")


# CD vote choice ------
cdname_len <- 55
cd_sorted <- est_long |>
    filter(level == "cd") |>
    mutate(trump_chr = as.character(glue("{if_else(size < 0.02, '(', '')}{r(p_mrp_twway)}{if_else(size < 0.02, ')', '')}"))) |>
    left_join(select(states_key, st, state)) |>
    arrange(region, division, state)

tbl_cd <- cd_sorted |>
    reshp_wide(id_vars = c("year", "cd"), valuevar = trump_chr) |>
    arrange(cd)

dk_name <- select(bind_rows(cd_info_2016, cd_info_2020), year, cd, dailykos_name)

tbl_cd_turn <- cd_sorted |>
    group_by(year, cd) |>
    mutate(frac = N / sum(N*(race == "All"))) |>
    reshp_wide(id_vars = c("year", "cd"), valuevar = frac) |>
    arrange(cd) |>
    left_join(dk_name) |>
    mutate(cd_only = cd,
           cd = glue("{str_trunc(dailykos_name, width = cdname_len)}: {cd}")) |>
    select(-dailykos_name) |>
    ungroup()

tbl_cdname <- tbl_cd |>
    left_join(dk_name) |>
    mutate(cd_only = cd,
           cd = glue("{str_trunc(dailykos_name, width = cdname_len)}: {cd}")) |>
    select(-dailykos_name)


## Saves for Tables in Appendix C.1
tbl_cdname |>
    filter(year == 2016) |>
    select(-year, -cd_only) |>
    tabout(level = "cd", year = "2016-",
           cap = "\\textbf{Point Estimates of 2016 Two-Party Vote for the Republican Presidential Candidate, by Race by CD}",
           lbl = "all-cds-2016",
           total = us16_chr)

tbl_cdname |>
    filter(year == 2020) |>
    select(-year, -cd_only) |>
    tabout(level = "cd", year = "2020-",
           cap = "\\textbf{Point Estimates of 2020 Two-Party Vote for the Republican Presidential Candidate, by Race by CD}",
           lbl = "all-cds-2020",
           total = us20_chr)


# turnout -----
tbl_cd_turn |>
    filter(year == 2016) |>
    select(-year, -cd_only) |>
    ## Saves for Tables in Appendix C.1
    tabout(level = "cd",
           year = "2016-turnout-",
           cap = "\\textbf{Composition of 2016 Electorate, by Race by CD}",
           lbl = "all-cds-2016_turnout",
           total = us16_turn)

tbl_cd_turn |>
    filter(year == 2020) |>
    select(-year, -cd_only) |>
    ## Saves for Tables in Appendix C.1
    tabout(level = "cd",
           year = "2020-turnout-",
           cap = "\\textbf{Composition of 2020 Electorate, by Race by CD}",
           lbl = "all-cds-2020_turnout",
           total = us20_turn)


# State-level SEs ----
se_fmt <- st_ests |>
    filter(race != "Other") |>
    mutate(race = fct_relevel(race, "White", "Non-White", "Black", "Hispanic", "All"),
           race = fct_recode(race, "non-White" = "Non-White")) |>
    arrange(race) |>
    left_join(select(states_key, division, st, state)) |>
    mutate(p_mrp_se = as.character(glue("{str_remove(formatC(p_mrp_se, digits = 2), '^0')}"))) |>
    arrange(year, estimator, division, state)


## Saves Tables in Appendix C3
se_fmt |>
    filter(year == 2016, estimator == "p_mrp_nofix") |>
    reshp_wide(id_vars = c("state"), valuevar = p_mrp_se) |>
    tabout(level = "state", year = "2016-SE-nofix-", total = NULL, sort_st = "division")

se_fmt |>
    filter(year == 2020, estimator == "p_mrp_nofix") |>
    reshp_wide(id_vars = c("state"), valuevar = p_mrp_se) |>
    tabout(level = "state", year = "2020-SE-nofix-", total = NULL, sort_st = "division")

se_fmt |>
    filter(year == 2016, estimator == "p_mrp_twway", race != "All") |>
    reshp_wide(id_vars = c("state"), valuevar = p_mrp_se) |>
    tabout(level = "state", year = "2016-SE-twway-", total = NULL,
           sort_st = "division",
           col_names = c("", "White", "All", "Black", "Hisp."))

se_fmt |>
    filter(year == 2020, estimator == "p_mrp_twway", race != "All") |>
    reshp_wide(id_vars = c("state"), valuevar = p_mrp_se) |>
    tabout(level = "state", year = "2020-SE-twway-", total = NULL,
           sort_st = "division",
           col_names = c("", "White", "All", "Black", "Hisp."))
